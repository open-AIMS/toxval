## Session: review-pr42-refactor-plan
Date: 2026-08-19
Model: Claude Opus 5 (claude-opus-5)

### Prompts and Responses

**User:** I need to write a review for this PR https://github.com/open-AIMS/toxval/pull/42 can you make sure my local is up to date with it then then provide your own review?

**Claude:** Fetched `upstream`, confirmed local `main` was already at `aa1e158` (= `upstream/main`),
created local branch `refactor` tracking `upstream/refactor` (single commit `84cfeda`, adds
`REFACTOR-human.md`, 246 lines, no code changes).

Read the full current source (`R/ecx.R`, `R/nsec.R`, `R/nsec_multi.R`, `R/helpers.R`,
`R/predict.R`), the test suite, the existing triage in `notes/implementation/`, and the
open issue list, then reviewed the plan against them.

Review structure and headline findings:

1. **`ecx.bnecfit` and `ecx.brmsfit` implement different estimators** — `ecx.bnecfit`
   builds a *single scalar* reference from `median(control_posterior) - median(dif_valsC) *
   ecx_val/100` and crosses every draw against it; `ecx.brmsfit` dispatches to
   `ecx_x_absolute()` / `ecx_x_relative()` / `ecx_x_direct()`, which recompute the reference
   *per draw* from that draw's own control. Control uncertainty propagates in one and not the
   other. Collapsing to one compute function forces a choice, and Phase 1's "show the numbers
   do not move" promise cannot hold for both paths. This is issue #19 in concrete form, and
   per `02_decisions.md` T7 it is the keystone blocking #1/#8/#12/#14 — so it should be
   resolved *before* the container work, not alongside it.
2. `type = "relative"` also differs between the two (`p_samples[, ncol]` vs `min(y)`), and
   hormesis differs (`modify_posterior()` live in `bnecfit`, commented out in `brmsfit`).
3. `tox_fct()` returns `NA` on no crossing, `nsec_fct()` and `ecx_x_*()` return `max(x_vec)`
   — so ecx already censors two different ways depending on the path. Plan must pick one.
4. `ecnsec` is computed by two different formulas (`bnecfit` vs `brmsfit`).
5. Six different `resolution` defaults across the entry points (1000/100/100/1000/1000/50);
   standardising changes numbers.
6. Plan doesn't mention issue #39 / Tier 0 (the bayesnec dependency untangle). `toxval_pred`
   is in fact exactly the plain-input handle Tier 0 needs — worth making that explicit, and
   noting the plan's "`toxval_predict()` is the only S3 generic" is the one part that must
   stay extensible from *outside* the package.
7. `predict.bayesnecfit` / `predict.bayesmanecfit` have no home in the plan.
8. Internal contradiction: §3.1 says `nsec` adds `reference`, `ecnsec`, `ecnsec.low`,
   `ecnsec.high` as always-present columns, but every `nsec()` example shows 4 columns.
9. `group` column "present only when grouped" contradicts the plan's own NA-not-disappearing
   rule.
10. `nsec_multi` with `type = "both"` (inc + dec + direction per response) is the case that
    most stresses the one-shape contract and has no worked example.
11. Tibble attributes are dropped by dplyr verbs unless `dplyr_reconstruct.toxval` is
    implemented — the "also stored in attributes" claim is fragile.
12. No deprecation path for removing `posterior=` from a 1.0.0 package.
13. Where `xform` applies (draws vs summaries) needs pinning in the contract.
14. Suggested the golden net use the existing `if (FALSE)` pinned-bug tests (4 of them) and
    the ~30 `TODO` markers as the ledger of expected-to-change behaviour.
15. Confirmed the plan's own claimed bug (`ecx.bnecfit` never range-checks `ecx_val`) and the
    "four hand-rolled copies" count (`ecx()`, `nsec()`, `nsec.drc()`, `nsec_multi()`).

Also noted `nsec.brmsfit` reads `x_range` before the `missing(x_var)` guard, `grepl()` used
for column-name matching (substring false positives), and `nsec.drc` hard-coding column 4
(already #34).

Answered Ayla's two direct questions: yes to the tibble structure with caveats on metadata
placement; yes to the `*_draws()` split, with the note that it should be introduced as an
addition before `posterior=` is removed.

---
**User:** Can we add the posterior as a list column? Why do you agree that splitting posterior
into `*_draws()` is the right option? [+ questions on #19 sequencing, the #39 dependency
untangle, `metric` vocabulary, `direction`, and `mode = "interval"` semantics]

**Claude:** Ran empirical checks against the installed packages before answering.

**Key empirical finding — the collision is not theoretical.** Same fit, same arguments,
different answer depending on what is attached:

```r
library(toxval);                 ecx(m1, ecx_val = 10, resolution = 50)
#> 1.49981 1.42070 1.56080     <- toxval::ecx.bnecfit      (scalar reference)
library(toxval); library(bayesnec); ecx(m1, ecx_val = 10, resolution = 50)
#> 1.52884 1.39871 1.59390     <- bayesnec::ecx.bayesnecfit (per-draw reference)
```

R warns about `predict.*` being overwritten and says nothing about `ecx`. Deterministic,
not a sampling artefact.

Narrowed further: `nsec` is **identical** across the two packages; only `ecx` diverges.
And of the three `ecx` implementations in play, two already agree —
`bayesnec::ecx.bayesnecfit` and `toxval::ecx.brmsfit` both use the per-draw `ecx_x_*`
family; `toxval::ecx.bnecfit`'s scalar reference is the sole outlier. So the
disruption-minimising answer to #19 is "adopt per-draw, delete the scalar version",
which leaves CRAN bayesnec's numbers unmoved.

Answers given:

1. **List column** — conceded it has a real case I had undersold (`ecnsec` draws are a
   second per-draw quantity that rides naturally alongside `nsec` draws in a second list
   column). But `toxval_pred` already solves the recompute problem that motivates it, so
   recommended `*_draws()` as primary with an optional `draws` list column later.
   Strongest concrete argument for the split: `nsec.drc` has no `posterior` parameter at
   all today, so `nsec(drc_fit, posterior = TRUE)` is silently inert — a `nsec_draws()`
   that errors is strictly better than an argument that lies.
2. **#19** — yes, do it first, and it is smaller than it looks: three sub-decisions
   (per-draw vs once; what span the percentage applies to; what "control" means), and the
   evidence above already points at the answer. Recommended folding #20 in and shipping it
   as a short reference-and-direction spec, not blocking PR42.
3. **#39 untangle** — gave the full per-function split table, the single import arrow
   (bayesnec → toxval), and the critical hazard: bayesnec's `plot.R:105`, `autoplot.R:312`
   (`bind_ecx` uses `ecx_vals[[1]]`/`[[2]]`/`[[3]]` and `attr(., "ecx_val")`), `summary.R:82`
   and the hurdle methods all consume `ecx()` positionally as a length-3 vector. So the
   tibble change breaks CRAN bayesnec's internals, and the tibble and the untangle must not
   land together. Recommended order: relocate first on the current return type, tibble
   second. Also identified that PR42's `toxval_pred` **is** PHASE1 Step 1's "plain-input
   API" — build it once, don't build both.
4. **`metric` vocabulary** — recommended fixed token (`"ecx"`, `"nsec"`) plus separate
   `ecx_val` / `sig_val` / `type` columns, with `ec10` rendered only in the `tbl_sum()`
   header. The baked-in-string form breaks as soon as #9 (multiple `ecx_val`) lands.
5. **`direction`** — agreed and extended: making it a core column subsumes #20, reframes
   hormesis (#1/#8) as "restrict to the decreasing limb" rather than a separate mechanism,
   and makes `min(zero_crossings())` well-defined. Suggested `direction = "auto"` inferring
   from the curve, and that `nsec_multi`'s `type` argument mostly collapses into a filter
   on `direction`.
6. **`mode = "interval"`** — explained that inverting a pointwise confidence band on y does
   not give a valid CI on x (the calibration/inverse-regression problem). Verified that
   `drc` exposes `fit$fct$fct(x, parm)`, `coef()` and `vcov()`, so a parametric bootstrap
   from `MVN(coef, vcov)` is feasible and would collapse the two-mode design into one mode
   ("realisations"), remove the compute branch, make `*_draws()` work for frequentist fits,
   and give #21 (glm/gam) the same mechanism for free. Also noted `drc::ED(interval="delta")`
   exists for ECx, and `fit$fct$inversion`/`edfct` are relevant to #40.

Also flagged that bayesnec's `dpar` argument and its gaussian/absolute/no-`bot` guard need
homes under the new architecture (predict-side and `meta`-carried respectively).

---
**User:** [Agreed with most recommendations. Four follow-ups: where does `filter()` actually
get used; N(S)EC needs to be in the metric vocabulary and probably its own issue; no `auto`
for direction — look for first increasing and first decreasing, NA if neither; agreed on the
bootstrap, needs a vignette. Still unclear why the posterior can't be a list column. Asked
for a draft PR comment, issues where appropriate rather than duplicating, and edits to
REFACTOR-human.md — all for review before anything is posted or committed.]

**Claude:** Investigated N(S)EC in bayesnec before answering. Two findings that change the
architecture requirements:

1. `expand_classes.R:76` — for a threshold model the no-effect posterior is
   `as_draws_df(fit)[["b_nec_Intercept"]]`, a **model parameter**, not a curve crossing. So
   `toxval_pred` as drafted (curves + x_vec) cannot express NEC or N(S)EC; it needs an
   optional per-realisation `threshold` slot.
2. `helpers.R:89` `w_nec_calc()` builds the model-averaged posterior by
   `sample(mod_fits[[i]]$ne_posterior, round(sample_size * wi))` per model, then `unlist()`.
   So realisation *i* of the averaged posterior corresponds to nothing in `curves`, and the
   count need not equal n_draws. Model-averaged realisation sets are **not row-alignable**
   with curve draws — which constrains the list-column design and breaks the `.draw` index
   assumption. The `sample()` is also unseeded.

Conceded the list column: with the bootstrap decision every fit has realisations, so the
"errors on a frequentist fit" argument evaporates. Settled on **list column as the
primitive, `*_draws()` as sugar** (`ecx_draws()` = `ecx(draws = TRUE) |> unnest()`), default
`draws = FALSE` for size. The case that decides it is `nsec`, which produces two paired
per-realisation quantities (NSEC and ecnsec) from the same draws — list columns keep them
aligned by construction where long form needs a self-join.

Agreed no `auto` for direction; direction becomes a property of the result, which also
subsumes `hormesis_def` and collapses three of `nsec_multi`'s four `type` values into a
filter.

On `filter()`: conceded the phrasing was compressed. The real requirement is that `metric`
be machine-readable because results get *stacked* (#9 multiple ecx_val, nsec_multi
responses/directions, user-built summary tables, CSVs read months later), not that anyone
necessarily calls `filter()`.

Issues #18 and #25 already cover N(S)EC, so recommended commenting rather than filing new.
Only one genuinely new issue needed (frequentist interval semantics).

Deliverables written, nothing posted or committed:
- `REFACTOR-human.md` rewritten in the working tree (365 insertions, 141 deletions).
  Original preserved at `scratchpad/REFACTOR-human.orig.md`.
- `notes/pr42/draft-github-posts.md` — six drafts: comments for #19, #20, #39, #25, one new
  issue, and a short PR comment that points at them rather than duplicating.

Flagged that the PDF could not be opened (no poppler in this WSL env), so the N(S)EC
definition in the draft comes from bayesnec's roxygen and needs checking against the paper;
and that the "nsec identical / ecx divergent" claim was measured on one fit and should be
re-run across the fixture set before being stated that strongly.

---
**User:** yes, run it across the full fixture set

**Claude:** Ran all 13 fixtures x both generics, fetching each implementation
namespace-qualified (`get("ecx.bnecfit", envir = asNamespace("toxval"))` etc.) so the
comparison measures estimators rather than dispatch. Scripts saved to `notes/pr42/`
(`compare_impls.R`, `attach_order.R`, `replicate_manec.R`).

Results, and three corrections to earlier claims:

**1. `ecx` differs on all 6 `bnecfit` fixtures — intervals far worse than point estimates.**
Point estimates agree to 0.3–2%; lower bounds differ by 23% on `ecx4param`
(0.684 vs 0.844) and 50% on `bnec_model_1` (0.748 vs 0.500).

**2. CORRECTION — I had the mechanism wrong.** I had said bayesnec's child-class method
shadows toxval's parent-class one. The dominant effect is **generic masking**, and because
bayesnec has no `ecx.brmsfit`, attaching it second removes toxval's brmsfit support:

```r
library(toxval); library(bayesnec)
environment(ecx)                 #> namespace:bayesnec
ecx(brms_model_1, x_var = "x")   #> Error: no applicable method ... class "brmsfit"
library(bayesnec); library(toxval)
ecx(brms_model_1, x_var = "x")   #> 0.832291 0.819584 1.050000
```

A hard failure of documented functionality, decided by attach order. Arguably the more
urgent half of #39.

**3. `nsec` agrees everywhere it can be tested.** Bit-for-bit identical on 5 single-model
`bayesnecfit`, 3 working `brmsfit`, and `drc`.

**4. CORRECTION — the one `nsec` disagreement (`manec_example`) is noise, not a difference.**
Both packages resample component posteriors unseeded. Over 6 replicate calls the gap
(0.162 at Q2.5) is smaller than the within-package range (0.252), so they are
indistinguishable. Draft updated from "differ" to "indistinguishable".

**5. New finding — model-averaged estimates are not reproducible.** Six identical calls,
range across calls:

| | Q50 | Q2.5 | Q97.5 |
|---|---|---|---|
| `toxval::nsec.bnecfit` | 1.4712–1.4791 | 0.9074–1.0062 | 1.5396–1.5544 |
| `bayesnec::nsec.bayesmanecfit` | 1.4727–1.4782 | 0.7023–0.9543 | 1.5469–1.5544 |

Medians stable to ~0.5%; **lower bound swings 10% (toxval) and 26% (bayesnec)** between
identical calls. Same unseeded `sample()` pattern found earlier in `w_nec_calc()`, but live
in `nsec()` today. Also means #39's "numbers unchanged" verification is not well defined for
`bayesmanecfit` until model averaging is made reproducible.

For `ecx` on `manec_example` the difference **is** real despite the noise — the gap exceeds
the run-to-run range at Q2.5 (0.107 vs 0.065) and Q97.5 (0.040 vs 0.009).

All findings folded into `notes/pr42/draft-github-posts.md`. Nothing posted, nothing
committed.

---
**User:** [Reviewing REFACTOR-human.md. Line 16 still says "split the `posterior` argument
into separate `*_draws()` functions" — didn't we agree we don't want that? Also lines 174,
185, 295+. Do we need `*_draws()` at all if we are using list columns? And line 91 — do we
actually need the `ec10` rendered label in the `tbl_sum()` header?]

**Claude:** Both correct; conceded and edited.

**`*_draws()` dropped entirely.** I had kept them as one-line sugar over the list column,
which was inconsistent. Checking the three original arguments for the split, all now support
the list column and none survives as an argument for a second function:

- *Type stability* — `draws = TRUE` does not change the return type; it is a `toxval` tibble
  either way with one more column. That is the same "column appears when meaningful" rule
  already accepted, so §3.2 is satisfied without a second function.
- *Unit of observation* — the objection to `posterior = TRUE` was that it returned one row
  per realisation. The list column keeps one row per estimate; unnesting is the caller's
  explicit choice at the call site.
- *`nsec.drc` silently-inert `posterior`* — `draws = TRUE` is not inert, since with the
  bootstrap every fit has realisations.

Against them, `*_draws()` would double the exported surface (`ecx_draws`, `nsec_draws`,
`nsec_multi_draws`, later `nec_draws`, `noec_draws`) plus docs and tests, to save one
`unnest()`. §3.5 rewritten around one function per metric with `draws = FALSE` default;
§2, §3.3, §3.4 and the phase table updated to match.

**`ec10` header label dropped.** It restated data already visible on the row (`metric` +
`ecx_val` columns), and with multiple `ecx_val` (#9) it could only ever name one of them —
the exact header/column redundancy I had criticised in the original document and then
reintroduced.

Replaced with the general rule: *the header carries per-call constants that are not columns;
anything that varies by row, or that anyone might compute on, is a column.*

Also synced the corresponding section of the PR-comment draft.

---
**User:** [Queried what "weighted quantiles" meant and whether it is statistically robust,
citing ssdinversionpaper and ssdbootstraprr; preferred the weighted sample method. Also
asked whether the per-model weighted sampling is redundant for nsec/ecx given
predict.bayesmanecfit exists, and where toxval/bayesnec complexity has been disentangled.
Then: make the three edits, agrees with A.]

**Claude:** Checked both local repos. `ssdbootstraprr/analysis/paper/paper.qmd` confirms
`MACL`/`weighted_arithmetic` (weighted mean of component confidence limits) is the legacy,
not-recommended method, and `weighted_samples` (draw from each component in proportion to
weight, pool) is the default and recommended one.

Clarified that the proposal was a weighted ECDF over pooled draws — the deterministic limit
of `weighted_samples`, not MACL — but **conceded the weighted sample is the right call**:
it is the method their simulation study actually validated for coverage, and folding weights
into the sample makes the `draws` list column self-contained rather than carrying a hidden
weight that every downstream consumer must honour. So the fix is not a change of method,
only a shared index plus a seed.

On the `predict.bayesmanecfit` point — correct for `ecx`, and testing revealed it is **not**
correct for `nsec`, for a reason nobody had posed:

- `ecx` computes its reference per draw → row-wise → mixing and computing commute. Measured:
  averaged-curve 1.49171/0.98182/1.56013 vs per-model-mixed 1.49298/1.03096/1.56226, within
  Monte Carlo error.
- `nsec`'s reference is `quantile(p_samples[, 1], sig_val)` — an across-draw quantile — so it
  does not commute. Isolating the reference on `manec_example`:

```
A  averaged curve, mixture reference   : 1.47273, 0.995858, 1.53957
B  per-model, per-model reference      : 1.47372, 0.717214, 1.55436
C  per-model, shared mixture reference : 1.47372, 0.905960, 1.55436
```

B vs C changes only the reference definition (lower bound moves 0.19); A vs C is sampling
noise (0.09). `bayesnec::nsec.bayesmanecfit` implements B; `toxval::nsec.bnecfit` implements
A. Recommended A by analogy with ssdinversionpaper's "average the CDFs, not the hazard
concentrations". **User agreed with A**, so it is recorded as a decision.

Disentangling: the shared-index fix is needed **only** for `nec` and `n(s)ec` (threshold
parameters, not curve functionals). `ecx`, `nsec` and `ecnsec` all come from one averaged
curve matrix, so pairing is automatic and `bayesnec`'s per-metric mixing can be deleted
rather than ported — making #39 smaller.

Three edits made:
1. `REFACTOR-human.md` §3.4 — replaced "realisations are not always row-alignable" with the
   **alignment invariant** (realisation *i* is the same draw in every slot), scoped the
   shared-index requirement to threshold-derived quantities, noted `ecx`/`nsec`/`ecnsec`
   pair automatically. §3.5 caveat changed from "must be documented" to "guaranteed by
   construction". New §3.8 records decision A with the A/B/C evidence. §5 gained the
   non-reproducibility note.
2. #19 draft — new section posing the NSEC-reference question with the numbers and the
   ssdinversionpaper parallel, proposing A.
3. #39 draft — new section showing per-metric mixing should be deleted, not ported.

Also reconciled the #25 draft, which had described the non-alignment as an unfixable
property. Nothing posted, nothing committed.

---
**User:** Ok, I am happy with all this, go ahead and post the new issue(s), then update the
plan and comments with the new issue number(s) (if/where required).

**Claude:** Posted one new issue to `open-AIMS/toxval`:

**#43 — "nsec.drc intervals invert a pointwise confidence band, which is not a CI on x"**
https://github.com/open-AIMS/toxval/issues/43 (label: `bug`; `needs-decision` does not
exist in the repo, so only `bug` applied)

Cross-references updated:
- `REFACTOR-human.md` §2 scope table: "new" → #43; §3.4 heading now "One mode, not two (#43)"
- `notes/pr42/draft-github-posts.md`: routing table marks #43 posted; the draft section
  retitled with the URL; the PR-comment summary row now cites #43. Also fixed a stale
  `*_draws()` reference in that row left over from dropping the split.

Did **not** post: the five comments on existing issues (#19, #20, #25, #39) and the PR #42
comment — those were not part of the instruction. Also did not open a `bayesnec` issue for
the model-averaging non-determinism, which has no home issue and was never drafted;
flagged it for a decision instead.

---
**User:** yes, draft and post that against bayesnec
**User (mid-turn):** wait, but if we implement that plan for toxval, this issue is self
resolve, yes?

**Claude:** Checked rather than assumed. It **half** self-resolves.

- *Alignment* — yes, self-resolves. One averaged curve matrix means one sampling step, so
  `sample_ecx()` and `sample_nsec()` get deleted and everything curve-derived pairs.
- *Reproducibility* — no. The remaining `sample()` inside `w_post_pred_calc()` /
  `posterior_epred.bayesmanecfit` is still unseeded, and `toxval` never touches it.

Verified it is broader than the metrics — plain `bayesnec`, no toxval involved:

```r
p1 <- predict(manec_example, newdata = nd); p2 <- predict(manec_example, newdata = nd)
p1[1,"Estimate"]  #> 2.183761
p2[1,"Estimate"]  #> 2.229868
```

So `predict()`, and hence `plot()`, `autoplot()` and `summary()`, are non-deterministic on a
`bayesmanecfit` today.

Refocused the issue accordingly — filed as reproducibility rather than pairing, since that
half is standalone and true regardless of the refactor:

**bayesnec #216 — "Model-averaged output is not reproducible: predict() on a bayesmanecfit
differs between calls"** https://github.com/open-AIMS/bayesnec/issues/216

Lists the five independent unseeded `sample()` sites, notes that `weighted_samples` itself is
correct and only the placement of the randomness is wrong, and proposes storing the realised
component index on the object at build time so no `seed` argument is needed. Also notes
which sites are expected to be deleted under toxval#39 rather than fixed.

Cross-referenced from `REFACTOR-human.md` §3.4 and §5, and from the #25 draft, with the
"not resolved by the plan" caveat stated explicitly.

---
**User:** Yes, post the rest of the issue comments the the PR 42 comment.

**Claude:** Posted all five. Fixed two things in the #25 body first: a self-reference
("rather than opening a new issue, since #25 and #18" → "since this issue and #18") and a
line that still described realisation sets as "not row-aligned", which contradicts the
alignment invariant agreed since that draft was written.

| where | link |
|---|---|
| #19 reference semantics | issues/19#issuecomment-5337296192 |
| #20 direction | issues/20#issuecomment-5337296332 |
| #39 untangle | issues/39#issuecomment-5337296496 |
| #25 N(S)EC | issues/25#issuecomment-5337296701 |
| PR #42 review | pull/42#issuecomment-5337296919 |

Plus the two new issues posted earlier: toxval #43, bayesnec #216.

`notes/pr42/draft-github-posts.md` updated to record what went where. Nothing committed —
`REFACTOR-human.md` and `notes/` are still uncommitted working-tree changes.

---
**User:** I ran the terminal and authorised. Yes add the scripts required to reproduce the
claims

**Claude:** Rewrote the three measurement scripts for the repo before committing — they had
hardcoded WSL absolute paths (`/mnt/c/Rworking/toxval/tests/testthat/fixtures`) and would not
have reproduced for anyone else. Now repo-relative with a guard, plus a header naming which
claim each backs and where it is cited. Added `notes/pr42/README.md` as an index.

Smoke-tested all three from the repo root; `compare_impls.R` reproduces the posted table
exactly (6 `ecx` DIFFERENT, 9 `nsec` IDENTICAL).

`git add notes/pr42/` swept in `draft-github-posts.md`; removed it from the commit and
amended, since its content is now live on GitHub at stable permalinks and committing it
would duplicate text that will drift. Left untracked locally.

Two commits on `refactor-plan-review`, branched off `refactor` (`84cfeda`):

- `160696d` revise refactor plan from review — `REFACTOR-human.md` (+634/-149),
  `.Rbuildignore`, `prompts/review-pr42-refactor-plan.md`
- `8ff34d7` add scripts reproducing the review measurements — `notes/pr42/` (4 files)

Pushed to `origin` (the fork) and opened **PR #44** against `open-AIMS/toxval:refactor`, so
the rewrite lands inside #42 as a reviewable diff rather than being pushed onto a
collaborator's branch: https://github.com/open-AIMS/toxval/pull/44

PR body tabulates every section changed against its reason, restates the two phasing
constraints, and flags that `air` is not installed here so the `.R` files were hand-formatted
to `air.toml`'s settings but not run through the formatter.

---
**User:** can we install air so we can remove the "Not done"?

**Claude:** Installed `air` 0.11.0 from the official GitHub release rather than piping the
installer script to a shell: downloaded `air-x86_64-unknown-linux-gnu.tar.gz`, verified it
against the published `.sha256` (`b6dd1446...846593`, matched), extracted, and installed to
`~/.local/bin/air` (user-local, no sudo). Already on PATH.

Formatted `notes/pr42/*.R`. Changes were cosmetic only — a brace around a bare `next`, and
one-argument-per-line in two `sprintf()` calls. Verified:

- all three parse
- `attach_order.R` reproduces the masking result unchanged
- `compare_impls.R` output is bit-identical to the pre-format run on every deterministic row;
  the only two rows that moved are the `manec_example` (bayesmanecfit) ones, which is the
  non-determinism documented in bayesnec #216 — an incidental second demonstration of it

Committed as `0d5aa15` and pushed. Removed the "Not done" section from PR #44's body.
PR now 7 files, +1186/-149, 3 commits. `air format --check` is clean on the branch.

Incidental finding, not acted on: `air format --check .` reports `scripts/build.R` would be
reformatted. Pre-existing, unrelated to this branch, and left alone.

---
**User:** I think we are done here finalise the session log to the branch

**Claude:** Session closed. Summary of what the session produced.

### Posted to GitHub

| | |
|---|---|
| toxval #19 | [comment](https://github.com/open-AIMS/toxval/issues/19#issuecomment-5337296192) — reference semantics; the `ecx` scalar-vs-per-draw divergence, plus the NSEC-reference question under model averaging |
| toxval #20 | [comment](https://github.com/open-AIMS/toxval/issues/20#issuecomment-5337296332) — `direction` as a property of the result |
| toxval #25 | [comment](https://github.com/open-AIMS/toxval/issues/25#issuecomment-5337296701) — `n(s)ec` vocabulary, `threshold` slot, shared component index |
| toxval #39 | [comment](https://github.com/open-AIMS/toxval/issues/39#issuecomment-5337296496) — the split, the ordering constraint, the CRAN hazard, per-metric mixing deletable |
| toxval PR #42 | [review comment](https://github.com/open-AIMS/toxval/pull/42#issuecomment-5337296919) |
| toxval #43 | [new issue](https://github.com/open-AIMS/toxval/issues/43) — `nsec.drc` intervals invert a pointwise confidence band |
| bayesnec #216 | [new issue](https://github.com/open-AIMS/bayesnec/issues/216) — model-averaged output is not reproducible |
| toxval PR #44 | [new PR](https://github.com/open-AIMS/toxval/pull/44) — revised plan, into `refactor` |

### Findings that came from measurement rather than reading

1. `ecx` differs between `toxval` and `bayesnec` on **all six** `bnecfit` fixtures. Medians
   agree to 0.3-2%; lower bounds differ by 23% (`ecx4param`) and 50% (`bnec_model_1`).
2. Attach order decides whether `ecx()` works at all. `bayesnec` has no `ecx.brmsfit`, so
   `library(toxval); library(bayesnec)` masks the generic and `ecx()` on a `brmsfit` errors.
3. `nsec` is bit-for-bit identical across the two packages everywhere it can be tested.
4. Model-averaged output is not reproducible in either package — `predict()` on a
   `bayesmanecfit` differs between consecutive calls; lower bounds swing 10%/26%.
5. `nsec` does not commute with model averaging (its reference is an across-draw quantile),
   so the two packages implement different definitions. `ecx` does commute.

### Decisions recorded in the plan

- per-draw reference for `ecx` (#19)
- `direction` as a result property, no argument (#20)
- one realisation mode; frequentist fits get a parametric bootstrap (#43)
- `draws` list column, no `*_draws()` functions
- closed `metric` vocabulary with the value in its own column
- `toxval_pred` alignment invariant; shared index only for threshold-derived quantities
- NSEC reference under model averaging: **A**, the model-averaged control (§3.8)

### Corrections made during the session

- The collision mechanism is **generic masking**, not child-class method shadowing.
- The `nsec` disagreement on `manec_example` is **noise**, not a real difference — caught by
  replicating before posting.
- `*_draws()` was kept as sugar over the list column, then dropped entirely once it was clear
  every argument for the split supported the list column instead.
- The bayesnec issue was refocused from pairing to reproducibility after checking that the
  toxval plan self-resolves the pairing half.

### Environment

Installed `air` 0.11.0 to `~/.local/bin` (release tarball, checksum verified).
`scripts/build.R` is not air-clean — pre-existing, left alone.

### Left untracked by choice

`PHASE1_DEPENDENCY_UNTANGLE.md`, `notes/implementation/`,
`notes/pr42/draft-github-posts.md` (content now live on GitHub),
`prompts/review-pr35-package-cleanup.md`.

---
## Addendum: the `anchor` argument

**User:** [comparing_tox_ests is public. eNSEC is a good name. But given the change is
purely to the reference, can we keep `nsec` and add an argument instead, like `type` for
`ecx`? ... Also, if we go down the anchor road, there is no reason to choose A or B — they
can be different anchors, and the decision is only which to use as the default.]

**Claude:** Agreed on both, and the second point is the better framing.

Read `comparing_tox_ests/compare_nsec_lecx.qmd`. The "NSEC-cv" reformulation anchors the
threshold on the observed control replicates rather than on `SE(b0)`, which makes the A/B
model-averaging question moot for that variant — a data-derived threshold is identical
across component models by construction. The document already names the failure mode
measured here: "a poorly constrained b0 ... pushes the NSEC to artificially low
concentrations". The 17%-weight component dominating the lower tail is an instance of it.

Advised against `NSEC-cv` as a published name ("cv" collides with coefficient of variation)
and against a NOEC root (invites reflexive rejection). User chose **eNSEC**.

Then argued for keeping it as an argument rather than a metric, and the user extended that
to A/B as well. Final design:

```r
nsec(fit, anchor = c("model", "component", "control"))
```

| anchor | threshold from |
|---|---|
| `"model"` (default) | model-averaged control posterior — was "decision A" |
| `"component"` | each component model's own control — was "decision B" |
| `"control"` | observed control replicates — eNSEC; reserved, not implemented |

Key realisations during the discussion:

- Keeping `"component"` is nearly free. It looked expensive when I told #39 that
  `nsec.bayesmanecfit()`'s machinery could be deleted — but once realisations carry
  `meta$draw_model` (needed for `n(s)ec` regardless), a per-component threshold is a grouped
  operation on the aligned set, not a separate sampling path. The resampling machinery still
  goes; the *option* survives cheaply.
- `anchor = "control"` is **not** purely a reference change: eNSEC is defined for a fit
  constrained through the observed control mean. On an unconstrained fit it approximates,
  and degrades worst exactly when `b0` is poorly constrained. Requires a warning comparing
  the model control to the observed control mean — which needs `toxval_pred$control`.
- This softens rather than strengthens the #22 (fitting wrappers) argument: toxval can
  compute it exactly for a constrained fit and approximately otherwise, saying which.
- Only the **default** now needs agreeing, so the `nsec` half of #19 stops blocking the
  refactor. The `ecx` half still blocks — that one is an inconsistency to remove, not a
  choice to expose.
- The code token and the published name need not match: paper says eNSEC, software says
  `nsec(anchor = "control")`, exactly as "absolute EC10" maps to `ecx(type = "absolute")`.

Plan edits (commit `82111f7`): `anchor` added to the descriptor columns; `toxval_pred$control`
slot added; §3.8 rewritten from "Decision: A" to the anchor argument with the measurement as
justification for the default; phasing split so only the `ecx` half of #19 blocks; §5 note on
the softened #22 connection.

Also updated the #19 mechanism comment in place
(issues/19#issuecomment-5337899712) — the mechanism section was still correct, only the
conclusion changed.

Deliberately did **not** mention the unpublished eNSEC work in the public comment; user
subsequently confirmed the repo is public anyway, so that constraint has lapsed.

---
**User:** [Is "supplied" another anchor? Would requiring a `control_fit` argument work, or is
#22 straightforward? This decision is critical — unless we rewrite bayesnec into toxval,
doesn't implementing #22 mean importing bayesnec's fitting methods, re-creating the
dependency problem?]

**Claude:** Corrected an earlier mistake first: I had read the eNSEC threshold as the
closed-form `mean - t * sd / sqrt(n)` on raw replicates, and suggested renaming the anchor to
`"observed"`. Wrong. It is a control-only *fit*, which must share the family and inference
framework of the main model — `bnec()` fits Beta, binomial, Gamma, poisson and negbinomial,
so a Gaussian t-interval would be wrong, and a Bayesian curve compared against a frequentist
threshold is incoherent. `"control"` was the right name after all: the axis is *which model
estimates the control* (the CR model / each component / a control-only fit).

On "supplied" as an anchor: no. That is provenance, not definition — the quantity is the same
whether toxval fits the control model or the user supplies it, so it should not change the
`anchor` label. `anchor = "control"` plus a required `control_fit` argument.

On the circularity, which was the real question: **it does not bite.** toxval Imports `brms`
directly and keeps it after #39 (it owns `toxval_predict.brmsfit`); bayesnec *Depends* on
brms. So brms is the shared substrate, not bayesnec. An intercept-only fit is
`brms::brm(y ~ 1, family = f, data = d)` — no NEC formulas, no priors to choose, no model set,
no averaging, hence no bayesnec. And where the control data live inside a bayesnec object,
`toxval_predict.bayesnecfit()` lives *in bayesnec* and populates `toxval_pred$control`, so the
arrow is unchanged.

Distinguished #22-proper (toxval fits CR models — big scope change, real circularity risk)
from what the anchor needs (a ~10-line helper over `brm(y ~ 1, ...)`). The anchor does not
require resolving #22.

Also proposed, but user declined to pursue: scoping #22 to what bayesnec does not do
(frequentist wrappers — drc, glm, gam), on the grounds that toxval reimplementing Bayesian CR
fitting would rebuild bayesnec inside toxval and make #39 pointless. Not filed.

Edits 1-3 applied (commit pending push): `toxval_pred$control` is now realisations from a
control-only fit subject to the alignment invariant, not raw replicates; §3.8's
`anchor = "control"` subsection rewritten with the family/framework requirement, the
`control_fit` argument, and the no-#39-impact note; §5's #22 note corrected from "not a
blocker" to "cannot be computed from a fitted object alone", with the reasoning that it still
does not threaten the untangle.

---
**User:** [Should we file a bayesnec issue for this? And given the two packages import
different things from each other, is that OK with CRAN?]
**User:** post a #39 comment covering the CRAN prerequisite, the resulting reordering, and
the "component" anchor correction.

**Claude:** Checked CRAN status before answering, which turned up a blocker missing from
PHASE1 and from #39:

```
bayesnec   on CRAN, 2.1.3.1
toxval     not on CRAN (crandb: document not found)
```

A CRAN package's `Imports` must resolve from CRAN; `Remotes:` is ignored by CRAN. So
PHASE1's `Remotes: open-AIMS/toxval` interim works for GitHub installs but **cannot be
released** — `bayesnec` cannot ship the untangle until `toxval` is itself on CRAN. That is a
prerequisite, and CRAN-readiness for toxval is its own body of work.

The mutual dependency itself is fine: circular via Imports/Depends is forbidden, but Imports
one way and Suggests the other is legal and common, provided checks pass without the
suggested package (which the plan already requires).

Also noted the cycle may be avoidable entirely — after the untangle toxval's use of bayesnec
is 4 code references in `R/` (all in the moving code) and 41 test references (almost all
`manec_example` and bnecfit methods, which move too), so `Suggests: bayesnec` could go as
well.

**This reversed earlier advice.** I had said the tibble change and the untangle must not land
together. Right about the risk, wrong about the sequencing: if toxval must reach CRAN first,
it should not be published with an API about to be broken. Revised so the tibble change comes
before CRAN submission and bayesnec relocates + adapts in one PR. Defensible because a shape
change breaks *loudly* (plots error rather than differing), while the dangerous axis — the
numbers — moves at import time under any ordering and is verified in isolation inside toxval
first, using `notes/pr42/compare_impls.R`.

Declined to file a bayesnec issue for the control-fit wrapper: speculative and downstream of
an unimplemented anchor.

Posted https://github.com/open-AIMS/toxval/issues/39#issuecomment-5338476575 covering the
CRAN prerequisite, the possibility of dropping the cycle, the reordering, and the correction
that `nsec.bayesmanecfit()`'s resampling machinery still goes but the per-component reference
survives as `anchor = "component"`.

Plan updated to match: §4 preamble now leads with the CRAN prerequisite; the phase table gains
"toxval to CRAN" as phase 6 and a combined bayesnec phase 7; phase 3 reworded to "toxval sheds
bayesnec".

---
**User:** [inventory the notes across bayesnec and toxval and the comments, make sure
everything aligns] / **User:** For the record, Tier 1 landed already. / **User:** yes, sorry
only bayesnec's Tier 1. Can you update the docs based on your audit, clarifying the cross
referencing. Let's commit the ones essential for Ayla to see for context.

**Claude:** Audited both repos' notes plus the nine GitHub comments; wrote
`notes/implementation/04_alignment_audit.md` (13 conflicts, kept local as a working doc).

On the Tier 1 point I checked rather than accepted: **bayesnec Tier 1 landed** (PRs
#197-#208 merged 14-17 Aug; issues #176/#180/#188/#191/#211/#213/#214 closed), **toxval Tier
1 had not** (zero closed issues; last merged PR #38 on 07 Aug, a week before the notes were
written). User confirmed they meant bayesnec's. Both repos having a
`notes/implementation/01_work_queue.md` with a "Tier 1" is now recorded as a finding in its
own right, and every reference is qualified by repo.

Committed to PR #44, after updating: `PHASE1_DEPENDENCY_UNTANGLE.md`,
`notes/implementation/01_work_queue.md`, `02_decisions.md`. Left local:
`00_protocol.md` (session operating instructions, not reviewer context) and the audit itself.

Substantive doc changes:

- **PHASE1** — superseded-in-part banner pointing at `REFACTOR-human.md` §4; new Step 2b for
  the CRAN prerequisite; Step 1 identified as `toxval_pred`; Step 2 no longer "the breaking
  release"; Step 3 drops the `Remotes:` framing and gains the tibble adaptation; loose-ends
  entry on tracking resolved.
- **02_decisions T4** — softened. It said "do not consult bayesnec's copies as a reference";
  following that literally would have given the wrong answer on #19, since `ecx.bnecfit` is
  the outlier of the three implementations. The narrow T6 claim (do not adopt the `crf()`
  back-transformation) is untouched.
- **02_decisions T7** — split: the `ecx` half of #19 still blocks, the `nsec` half is now the
  `anchor` argument and only its default needs agreeing; #1/#8 absorbed by #20.
- **02_decisions** — #4 marked decided; issues-to-file list updated (#39/#40/#41 filed, #43
  and bayesnec #216 added); dangling `03_uniroot_spike.md` pointed at #40 with the note that
  the code is `bayesnec/ignore/uniroot.R`.
- **01_work_queue** — repo banner; a "sequencing against the refactor" table splitting Tier 1
  into safe-either-way (#33/#31/#32), do-before-the-net (#37) and fold-into-the-refactor (the
  eight behavioural ones); Tier 0 marked as #39 with the CRAN and `toxval_pred` findings.

**bayesnec notes edited but NOT committed** (tracked, on a clean `dev`): `00_protocol.md`
gains #216 as an explicit in-scope exception with the note that the `helpers.R` half stays
and only the call sites migrate; `01_work_queue.md` gains the repo banner and a note that
Tier 2 #120 now overlaps the migration's final step; `03_decisions.md` D8 corrected for the
CRAN/`Remotes:` finding.

PR #44 now 10 files, +2184/-149, 11 commits; body updated to explain the newly tracked notes.

---
**User:** [Don't commit the notes to bayesnec dev — need somewhere else. Where would you
normally put them? Agreed the list; Ayla on leave so parking the refactor, but do 3, 4, 5.
Set up a way to point Claude Code at those from each repo root. bayesnec is working example7
already.]

**Claude:** Parked the bayesnec note edits on a local branch `notes-post-toxval-review` off
`dev` (commit `43580931`), restoring `dev` to clean. Not pushed — bayesnec pushes branches
directly to `open-AIMS`, so that is the user's call.

On where notes belong: for tracked in-repo notes, a branch and PR is the normal route, same
as any other change. The structural point is that the *cross-repo* material is what keeps
drifting, so the shared part should live in one place — toxval now tracks
`PHASE1_DEPENDENCY_UNTANGLE.md` and `notes/implementation/`, and bayesnec's copies should
shrink to pointers rather than being kept in sync.

Wrote three self-contained task briefs, following the existing `00_protocol.md` house style,
each invokable as "Read `notes/tasks/<file>.md` and work it":

- `bayesnec/notes/tasks/216-model-averaging-reproducibility.md` — the bug with its
  reproduction, all five `sample()` sites, the in-scope/out-of-scope split (`helpers.R` stays,
  `ecx.R`/`nsec.R` are migrating and must not be edited), the shared-index fix, and the
  hazard that numbers will change with no reproducible "before" to match. Guardrails: no
  `air`, don't touch `vignettes/example7*` (#193 in another session), branch off `dev` and
  PR `--base dev`.
- `toxval/notes/tasks/tier1-mechanical.md` — #33, #31, #32, #37 only, with an explicit
  explanation of why the other eight Tier 1 items are excluded (they move the golden values
  the refactor's phase 1 locks). Guardrails: fork workflow, never push upstream, `air`
  required, don't delete the `if (FALSE)` tests, don't restructure dispatch in #32.
- `toxval/notes/tasks/cran-readiness-scoping.md` — assessment not fix; the deliverable is an
  issue with `R CMD check` output and a sized checklist. Covers check time, package size,
  `_R_CHECK_DEPENDS_ONLY_`, `\value` completeness, the `1.0.0.9000` version question, and
  sequencing against `REFACTOR-human.md` §4.
- `toxval/notes/tasks/README.md` — index, plus what is parked pending Ayla.

All left untracked, consistent with the earlier decision to keep `00_protocol.md` local:
these are session operating material, not reviewer context.

---
**User:** did we open an issue for the cran readiness?

**Claude:** No — checked and none existed. The brief had it as the session's deliverable,
which left it invisible on the tracker: exactly the failure mode the audit found in
`02_decisions`' "issues to file" list. Item 5 of the agreed list was "file the CRAN-readiness
issue and scope it", so filed **toxval #45** — "Scope CRAN readiness — prerequisite for the
bayesnec dependency reversal (#39)". Written as an explicit placeholder (not assessed),
carrying the rationale, the areas to check, and the decisions it surfaces. Pointed
`notes/tasks/cran-readiness-scoping.md` at filling in #45 rather than opening a new issue,
and linked it from #39.

**User (mid-turn):** if it is relevant I already launched to 2 tasks that were not blocked on
both repos

**Claude:** Highly relevant — it explained a mystery. Two concurrent sessions were sharing
`/mnt/c/Rworking/toxval` as one working tree. The toxval session checked out `dev` (per my
brief), which switched the tree out from under this session, and a subsequent commit intended
for `refactor-plan-review` landed on `dev` instead, carrying a 20-line truncated copy of this
log.

Diagnosed and fixed:

- **bayesnec is fine.** Its #216 session is in its own worktree
  (`scratchpad/wt-216`, branch `issue-216-deterministic-model-averaging`), following the
  worktree pattern in bayesnec's `00_protocol.md`. The main checkout is untouched on `dev`.
- **toxval had only one worktree**, shared. Dropped the stray commit with
  `git reset --mixed origin/dev` (local only, never pushed) and deleted the stray file, so a
  session branching off `dev` no longer inherits it.
- Moved this session's work into its own worktree so it stops competing for the main tree.
- **Root cause was a gap in my own brief:** `notes/tasks/tier1-mechanical.md` told the session
  to branch off `dev` but, unlike bayesnec's protocol, did not tell it to use a worktree.
  Fixed.

---
**User:** [shared the output of the two launched sessions — bayesnec #216 and toxval Tier 1 —
asking for advice] / **User:** Do 2 and 3, I shared 1 with the other claude code session in
bayesnec already.

**Claude:** Reviewed both outputs and verified the checkable claims.

**bayesnec #216.** Inspected the branch: all `helpers.R` sites now take a shared index
(`w_post_pred_calc` folded into `w_grid_pred_calc`), the `.Random.seed` save/restore handles
the no-prior-seed case, and the only unseeded `sample()` calls left are `ecx.R:294` and
`nsec.R:233-234` — exactly the out-of-scope ones.

Its finding #1 is correct and **#216's reproduction was my error**: verified that `predict()`
on a *single* `bayesnecfit` also varies (2.1175 / 2.1900 / 2.1804) while `posterior_epred()`
is bit-stable, so my example conflated likelihood simulation with model averaging. User
relayed this to the bayesnec session directly.

Advice given: (a) `set.seed(seed)` is unpinned, so a stored seed rebuilds a different index
if R's `sample.kind` default ever changes again as it did in 3.6.0 — recommend
`set.seed(seed, sample.kind = "Rejection")` or storing the index; (b) **override my own scope
note** and extend the index into `sample_ecx()`/`sample_nsec()`, because the migration is now
gated on #45 (CRAN) and until then the reported toxicity numbers stay irreproducible — using
one index for both the NSEC and `ecnsec` draws fixes the pairing bug for free and does not
pre-empt the `anchor` decision.

**toxval Tier 1.** PRs #46, #47, #48 open; #32 closed with evidence instead of an empty PR.

Actioned:

1. *(declined — user handled it)* correct #216.
2. **Posted the #37 hormesis finding to #20**
   (issues/20#issuecomment-5340024254). Verified the code first, and it is worse than the
   session reported: `nsec.bnecfit` uses `apply(p_samples, 1, max)` — per-draw peak, coherent
   — while `nsec.brmsfit` uses `apply(p_samples, 2, max)`, the per-x maximum across draws,
   i.e. the upper envelope of the posterior band. A `sig_val` quantile of that is not a
   posterior quantity at all, which is a concrete diagnosis for #1/#8. Plus: uncommenting the
   truncation would *error* (NAs reach an `na.rm`-less `max()`, so the reference becomes NA),
   and `nec_posterior` is assigned and never used in both branches. Argued this supports
   retiring `hormesis_def` via #20 rather than reconciling the two.
3. **Fixed the NEWS instruction.** toxval's `NEWS.md` carries a `fledge` banner, so manual
   entries are overwritten; `00_protocol.md` item 5 and the tier1 brief both mandated one.
   Corrected in both, with an explicit warning that **bayesnec's `NEWS.md` is hand-maintained
   and does want an entry** — checked, no fledge banner there — so nobody "fixes" one repo to
   match the other. The CRAN brief now notes fledge owns the file.

---
