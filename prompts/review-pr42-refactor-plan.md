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
