# Decisions — toxval

Settled 2026-08-14; amended 2026-08-19 after the review of PR #42. A session
implements these rather than re-opening them.

**"Tier" is repo-specific.** `bayesnec` has its own
`notes/implementation/01_work_queue.md` with its own Tier 1 and Tier 2.
**bayesnec Tier 1 is complete**; **toxval Tier 1 has not started.** References
below mean *toxval* tiers unless stated.

---

## T1 — Branching and review

Branch per issue off `main`, push to `origin` (`beckyfisher/toxval`), PR into
`open-AIMS/toxval` **`main`**. Never push to `upstream`. Sessions do not merge.

**Amended 2026-09-04.** This said "off `dev` (fork-only integration branch)".
`dev` was dropped in #50; `main` is the integration branch in both repos.

## T2 — Formatting

`air.toml` is present: format every R file touched with `air` before committing.
This differs from `bayesnec`, which has no `air.toml` and where formatting is
forbidden.

## T3 — Guards error, they do not coerce

Where an argument combination is invalid — two `type` values (#7),
`group_var == x_var` (#15) — **error naming the arguments**. Do not silently use
the first value with a warning. A toxicity estimate produced from a silently
altered request is the kind of number that ends up in a report.

Distinguish this from **#6 and #24**, which are about a *valid* estimate landing
at the edge of the tested range. Those **warn** and return the estimate.

## T4 — Fixes here are canonical, but `bayesnec`'s code is still evidence

`bayesnec` is deleting its own `ecx()`/`nsec()`, not maintaining them. **Do not
port fixes back.**

**Amended 2026-08-19.** This decision originally also said "do not consult
`bayesnec`'s copies as a reference". That is too strong and following it would
have produced the wrong answer on #19. Measured across the full fixture suite,
`toxval::ecx.bnecfit` is the **outlier**: it builds one scalar reference from
medians, while `toxval::ecx.brmsfit` and `bayesnec::ecx.bayesnecfit` both compute
the reference per draw. Two of the three agree and the odd one out is ours.

The narrow claim still holds exactly: `bayesnec` is worse in one specific
respect, the automatic `crf()` back-transformation, and that must not be adopted
(T6). Everywhere else, treat its implementation as a comparison point — the
scripts in `notes/pr42/` exist to make that comparison cheap.

## T5 — Tier 0 is attended

The dependency untangle is not autonomous work. It spans two repositories, the
PRs must land in order, and `bayesnec` needs a `Remotes:` entry in the interim.
Do not attempt it, and do not modify `DESCRIPTION` dependencies.

## T6 — `xform` stays user-supplied

`bayesnec` derives the back-transformation for an inline `crf(log(x + 1))` by
substituting into the parsed call, which silently drops the `+ 1` and returns
`ecx` on a different scale from `nec` (`bayesnec` #196, measured at about 3.3%
error). `toxval` avoids this by taking `xform` as a function from the user.

**Keep it that way.** During Tier 0 there will be a temptation to adopt
`bayesnec`'s automatic back-transformation for convenience. Do not.

## T7 — #19 is the keystone

Several issues filed as bugs are really about the *definition* of "effect":
#1, #8, #12 and #14. Resolving #19 resolves the ambiguity behind them, and doing
them piecemeal first would mean fixing the same thing four times, differently.

**#19 is not in the unattended queue** and neither are its dependants. If a
toxval Tier 1 issue turns out to depend on the definition, stop and say so —
that is a useful finding, not a failure.

**Amended 2026-08-19.** #19 has since split into two halves that behave
differently:

- The **`ecx`** half — per-draw versus scalar reference — was a single decision
  and blocked the refactor. **Settled 2026-09-04: see T9**, which also settles
  the `type` vocabulary and supersedes this half of T7.
- The **`nsec`** half — which control the reference comes from under model
  averaging — is **no longer a blocking decision**. `REFACTOR-claude.md` §3.8
  exposes it as `anchor = c("model", "component", "control")`, so only the
  *default* has to be agreed, and a default can be revisited.

**#1 and #8** (`hormesis_def = "max"`) are largely absorbed by the direction
decision (#20): `direction` becomes a property of the result and replaces
`hormesis_def` rather than fixing it. See `REFACTOR-claude.md` §3.6.

## T8 — `ecnsec` is defined consistently with `ecx` (#49)

Settled 2026-09-04 (RF). `ecnsec` is computed by three different formulas, one
per `nsec` method, which agree only for a monotonic decreasing curve with the
fitted-range denominator selected explicitly — not the default.

**The decision.** `ecnsec` matches `ecx` exactly: it is the inverse of the
`ecx` reference construction under the same `type`, whatever that construction
is, and `nsec()` accepts the arguments `ecx()` accepts, so `type` reaches all
three methods rather than being absorbed by `...` in two of them. There is no
separate `ecnsec` vocabulary to maintain: the per-`type` definitions are §3.10's
and are stated once, in §3.9's table.

Stated in full, with the formulas and the per-method changes, in
`REFACTOR-claude.md` §3.9. Two points from there that affect other work:
`nsec.brmsfit` and `nsec.drc` report a smaller `ecnsec` than they do now at
default settings, so phase 1 golden values must be captured for `ecnsec` under
each `type`; and adding `type` to the `nsec` generic is an S3 signature change,
so it is done with #32.

**Independent of #19.** This decision fixes the *relation* between `ecnsec` and
`ecx`, not the reference construction itself. #19 has since settled that
construction (T9), redefining `relative` and adding `range`; `ecnsec` follows
it without this decision changing. The `hormesis_def == "max"` branches are
retired by #20 rather than realigned here.

## T9 — the `ecx` reference and the `type` vocabulary (#19)

Settled 2026-09-04 (RF), superseding the "still blocks" half of T7. Stated in
full in `REFACTOR-claude.md` §3.10.

**The reference is computed per realisation**, from that realisation's own
control. The single reference built from posterior medians in `ecx.bnecfit`
(`R/ecx.R:146`) is deleted; `ecx.brmsfit` and `bayesnec::ecx.bayesnecfit` already
compute it per realisation.

**`type` is a four-value vocabulary** naming what the percentage is measured
against — for a decreasing curve: `absolute`, control → 0; `relative`, control →
the equation's theoretical asymptote; `range`, control → the minimum predicted
response over the predictor range; `direct`, a supplied response value. Each has
an increasing form (§3.10), reached through `drc` and `brms`; `bnec()` continues
to fit decreasing curves only.

Four points that decide other work:

- **`relative` is a component-level quantity under model averaging.** Components
  have different asymptotes, so a model-averaged `relative` ECx does not
  correspond to a single response level. This is accepted and documented, not
  refused, on the same basis §3.8 records for the NEC. It follows that `ecx`
  does not commute under `relative`, so the reason `ecx` takes no `anchor`
  argument covers `absolute`, `range` and `direct` only.
- **`relative` is refused where the bound is infinite** — an equation with no
  asymptote parameter fitted with a family that has no natural bound. Error for
  a single fit; drop with a warning and renormalise for a model-averaged one.
  The retained equations and weights must be recoverable from the result.
- **`absolute` uses 0 on an unbounded family deliberately**, following OECD TG
  201, which lets percent inhibition exceed 100% for a negative response rather
  than truncating. `ecx_val` is therefore not capped at 100.
- **`relative` changes meaning**, so it is deprecated into `range` via
  `lifecycle::deprecate_warn()`, with a NEWS item. 1.0.0 is released.

**Sequencing.** §3.10 presumes the direction framing (#20), so #19 cannot be
implemented before #20 is settled. T8 (#49) is unaffected — `ecnsec` inverts
whatever `ecx` does — but §3.9's formula table is rewritten to match the new
vocabulary.

---

## T10 — direction is a property of the result (#20)

Settled 2026-09-04 (RF). Stated in full in `REFACTOR-claude.md` §3.6.

An estimate is sought in both directions on every curve and `direction` is a
column of the result, so there is no `direction` argument and no `auto`
override. `hormesis_def` is removed rather than repaired: a hormetic curve has
both an increasing and a decreasing crossing and both are reported. #1 and #8
close as a consequence.

Two points settled in the discussion:

- **A direction with no crossing emits a row with `NA`**, rather than omitting
  the row. The direction was looked for and not found, which is information.
- **Each direction has its own reference.** A single reference would return
  `NA` in both directions for a monotonic increasing response, which is the
  case #20 exists to support. `nsec_multi` already builds `reference_dec` and
  `reference_inc` (`R/helpers.R:44-59`); generalising that is what #20 asks
  for. `type = "direct"` is the exception, taking one supplied value for both
  directions.

With T9 and T10 settled, the three decisions gating the refactor are answered.
See T11 for #43.

## T11 — frequentist realisations come from a parametric bootstrap (#43)

Settled 2026-09-05 (RF). Stated in full in `REFACTOR-claude.md` §3.4.

Realisations are generated the same way for every model class: posterior draws
for a Bayesian fit, a parametric bootstrap for a frequentist one — draw
`n_boot` parameter vectors from `MVN(coef(fit), vcov(fit))` and evaluate the
mean function at each. There is no second mode in which the three columns of
`predict(drc_fit, interval = "confidence")` are treated as three curves.

**Why.** Inverting a pointwise confidence band on `y` does not give a valid
confidence interval on `x`. Coverage is wrong and degrades worst where the
curve is flat, which for a concentration-response curve is where NSEC sits.
Two quantities computed that differently should not share the column names
`conf.low` and `conf.high`.

**Consequence.** `drc` intervals change. They are not a correction of the
current ones but a different quantity, so this needs a NEWS entry and the
reasoning in the documentation, not a silent substitution. Built in phase 2.

This is an approach rather than a definition, so it did not gate phase 0.

## T12 — `anchor` defaults to `"model"` (#19, `nsec` half)

Ratified 2026-09-05 (RF). Stated in full in `REFACTOR-claude.md` §3.8.

For a model-averaged fit the NSEC threshold is the `sig_val` quantile of the
**model-averaged** control posterior, so every draw is measured against one
common response level. `"component"` — each component's own control, the
strict BMA mixture and what `bayesnec` returns today — remains available, as
does the reserved `"control"`. Only the default was in question.

**Two consequences to document rather than gloss.** `"model"` gives a higher
NSEC, which is less protective, and that is the direction needing the most
justification in a guideline context. It also pools control uncertainty across
components into one number, so it understates that uncertainty relative to
`"component"`.

**Attribution.** Agreed on the measurement in §3.8, which was taken while
`bayesnec` #216 left the model-averaged resampling unseeded; the magnitude
varied by about ±0.2 between runs. #216 closed on 2026-08-21 and the
comparison was not re-run before ratification, which was a deliberate call
(RF). The mechanism and the direction do not depend on it. **A precise size
for the change must be measured before it is quoted** in NEWS or the
documentation.

**This completes phase 0.** #19, #20, #49 and the `anchor` default are all
settled, so `REFACTOR-claude.md` §4 phase 1 can begin.

---

## Still to decide

Listed so they are not mistaken for oversights.

- **#3** — `ecx_val` as a proportion rather than a percentage. Breaking; needs a
  deprecation path. Interacts with #9, and now with the closed `metric`
  vocabulary and its separate `ecx_val` column (`REFACTOR-claude.md` §3.1).
- ~~**#4** — always return a tibble.~~ **Decided 2026-08-19**: yes, a `toxval`
  tibble subclass. See `REFACTOR-claude.md` §3.1 and PR #42 / #44.
- **#22** — whether `toxval` takes on model-fitting wrappers at all. Scope
  question for the author; it changes what the package *is*. **Now has a forcing
  case**: `anchor = "control"` (`REFACTOR-claude.md` §3.8) needs a control-only
  fit, which cannot be recovered from an already-fitted object. It needs only
  `brms`, so it does not threaten the untangle.
- **#17, #18, #25** — NOEC, model-averaged N(S)EC, N(S)EC. Each needs its
  definition agreed before any code.
- **bayesnec #39** — root-finding in place of the grid, now **toxval #40**. The
  spike referenced elsewhere as `03_uniroot_spike.md` was never written; the
  code is `bayesnec/ignore/uniroot.R`. The numerical part is tractable, but it
  forces the `type` reference semantics to be made explicit, which is #19 again.

## Issues — status 2026-08-19

- ~~The **Tier 0 untangle** has no issue.~~ Filed: **#39**.
- ~~**bayesnec #39** and **#44** have no counterpart here.~~ Filed: **#40**
  (root-finding) and **#41** (hypothesis method).
- When #29 is fixed, close **bayesnec #166** as a duplicate of it. *(still to
  do)*
- **#49** — `ecnsec` computed from three different definitions across the
  `nsec` methods. **Decided 2026-09-04**: see T8.
- **#19** — the `ecx` reference and the `type` vocabulary. **Decided
  2026-09-04**: see T9.
- **#20** — direction as a property of the result. **Decided 2026-09-04**: see
  T10. #1 and #8 close with it.
- **#43** — `nsec.drc` intervals invert a pointwise confidence band. **Decided
  2026-09-05**: see T11.
- Filed since: **#45** — scope CRAN readiness, which blocks #39.
- **bayesnec #216** — model-averaged output is not reproducible. **Closed
  2026-08-21**, so the `anchor` measurement in `REFACTOR-claude.md` §3.8 can now
  be reproduced. The default was ratified without re-running it (T12); the
  measurement is still owed before any figure is quoted.
