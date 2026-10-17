# Decisions — toxval

Settled 2026-08-14; amended 2026-08-19 after the review of PR #42; extended
2026-09-04/05 with T8-T12, which settle the definitions gating the refactor, and
2026-10-09 with T13-T17, from the review of PR #59, and 2026-10-17 with T18-T20,
from the review of PR #62. A session implements these rather than re-opening
them.

**Scope.** This file records decisions about the **package** — what the code
does and why. Workflow is not recorded here: branching, remotes and review live
in `CLAUDE.md`, and the unattended-session protocol is a local working note.
Duplicating either here is what let the branching entry go stale when the
workflow changed in #50.

**Numbering is stable.** T-numbers are cited from issues, pull requests and the
plan documents, so they are never reused or renumbered. T1 (branching), T2
(formatting) and T5 (Tier 0 is attended) have been removed under the scope rule
above, so the list begins at T3 and skips T5.

**"Tier" is repo-specific.** `bayesnec` has its own
`notes/implementation/01_work_queue.md` with its own Tier 1 and Tier 2.
**bayesnec Tier 1 is complete**; **toxval Tier 1 is two of twelve done** — #33
(PR #46) and #37 (PR #48). References below mean *toxval* tiers unless stated.

---

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

**Settled 2026-09-04 (T9): adopt the per-draw reference and delete the scalar
one.**

The narrow claim still holds exactly: `bayesnec` is worse in one specific
respect, the automatic `crf()` back-transformation, and that must not be adopted
(T6). Everywhere else, treat its implementation as a comparison point — the
scripts in `notes/pr42/` exist to make that comparison cheap.

## T6 — `xform` stays user-supplied

`bayesnec` derives the back-transformation for an inline `crf(log(x + 1))` by
substituting into the parsed call, which silently drops the `+ 1` and returns
`ecx` on a different scale from `nec` (`bayesnec` #196, measured at about 3.3%
error). `toxval` avoids this by taking `xform` as a function from the user.

**Keep it that way.** During Tier 0 there will be a temptation to adopt
`bayesnec`'s automatic back-transformation for convenience. Do not.

## T7 — #19 was the keystone

**Superseded 2026-09-04/05 by T9 and T12, which settle both halves of #19.**
Kept because it records why #1, #8, #12 and #14 were held rather than fixed
individually: each is really about the *definition* of "effect", and doing them
piecemeal would have meant fixing the same thing four times, differently.

The definition now exists, so the standing instruction this decision carried —
stop if a Tier 1 issue turns out to depend on it — no longer applies. Read T9
for the `ecx` reference and the `type` vocabulary, T12 for the `anchor` default.

**Amended 2026-08-19.** #19 has since split into two halves that behave
differently:

- The **`ecx`** half — per-draw versus scalar reference — was a single decision
  and blocked the refactor. **Settled 2026-09-04: see T9**, which also settles
  the `type` vocabulary and supersedes this half of T7.
- The **`nsec`** half — which control the reference comes from under model
  averaging — was exposed as `anchor = c("model", "component", "control")`
  (`REFACTOR-claude.md` §3.8) rather than resolved, so only the *default*
  needed agreeing. **Ratified 2026-09-05 as `"model"`: see T12.**

**#1 and #8** (`hormesis_def = "max"`) are absorbed by the direction decision
(#20), settled as T10: `direction` becomes a property of the result and
`hormesis_def` is removed rather than fixed. Close both when #20 is implemented.

## T8 — `ecnsec` is defined consistently with `ecx` (#49)

Settled 2026-09-04 (RF).

**The problem this fixes.** `ecnsec` is currently computed by three different
formulas, one per `nsec` method. They agree only for a monotonic decreasing
curve, and only when the fitted-range denominator is requested explicitly — the
default in none of them. That is the defect #49 reports; it describes the code
as it stands, not the ruling below.

**The decision.** `ecnsec` matches `ecx` exactly. It is the inverse of the `ecx`
reference construction under the same `type`, whatever that construction is, and
**it takes `ecx`'s default, `type = "absolute"`**. `nsec()` accepts the arguments
`ecx()` accepts, so `type` reaches all three methods rather than being absorbed
by `...` in two of them. There is no separate `ecnsec` vocabulary to maintain:
the per-`type` definitions are §3.10's and are stated once, in §3.9's table.

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

**Sequencing.** §3.10 presumes the direction framing (#20), settled the same day
as T10, so the two are implemented together. T8 (#49) is unaffected — `ecnsec`
inverts whatever `ecx` does — but §3.9's formula table is rewritten to match the
new vocabulary.

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

T9 and T10 answer the two definitions that gated the refactor alongside T8. The
fourth phase 0 item, the `anchor` default, is T12; #43 is T11.

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

**This completes phase 0.** #19, #20, #49 and the `anchor` default are all
settled, so `REFACTOR-claude.md` §4 phase 1 can begin.

## T13 — per-curve `control` (#59)

Settled 2026-09-12 (AP), ratified 2026-10-09 (RF). Stated in full in
`REFACTOR-claude.md` §3.4.

`control` holds one vector of control realisations per curve, aligned to the
rows of `keys` (T14), rather than a single vector. A grouped fit has one control
per group. On three simulated sites with control responses of 10, 6 and 2 and a
common NSEC, per-site controls gave 0.289, 0.301 and 0.309, while one pooled
control gave 2.947, 2.376 and 0.216. Under `anchor = "control"` the control-only
fit must be grouped as the dose-response fit is, and a mismatch is an error.

## T14 — the `keys` table of `toxval_pred` (#59, #20)

Settled 2026-10-09 (RF). Stated in full in `REFACTOR-claude.md` §3.4.

`toxval_pred` identifies each curve by a row of `keys`, a data frame whose
columns are drawn from `group` and `response`. An ungrouped fit has one row and
no columns. `keys` replaces the names of `curves` and `meta$dimension`, so that a
fit that is both grouped and multivariate can be represented.

T15 makes such a call valid. `bayesnec` constructs `toxval_pred` from outside
`toxval` from phase 7, so the structure changes in phase 2, while the change is
confined to one PR. `toxval_predict.brmsfit()` still refuses a multivariate fit
until a later phase adds one curve per response.

The `group` column is a factor whose levels follow the row order, so the order
`group_levels()` gives survives into the result. `response` is a factor for the
same reason (2026-10-11, AP).

## T15 — `nsec_multi()` folded into `nsec()` (#20)

Settled 2026-09-04 (RF) on #20. Recorded 2026-10-09 (RF), after the review of
#59 found the plan still listing `nsec_multi` as a separate metric function.
Stated in full in `REFACTOR-claude.md` §3.3.

`nsec_multi()` becomes a deprecated wrapper around `nsec()`. Its arguments are
redistributed:

- `multi_var` and `trials_var` become arguments of `toxval_predict.brmsfit()`.
- `type = "both"`, `"increasing"` and `"decreasing"` become filters on the
  `direction` column (T10).
- `type = "lower"` with `criterion` stays an argument of `nsec()`, under a new
  name chosen in phase 4, because `type` in `nsec()` selects the `ecnsec`
  definition (T8).

## T16 — marginalised group-level effects in a `brmsfit` (#59)

Settled 2026-10-09 (RF). Stated in full in `REFACTOR-claude.md` §3.7.

Group-level (random) effects are marginalised with `re_formula = NA`, so every
estimate is a population-level estimate. A user who wants one estimate per group
fits the group as a population-level effect. A `group_var` that enters the model
only through a group-level term is an error, because every level would receive
the same curve.

## T17 — the predictor of a `drc` fit (#59)

Settled 2026-10-09 (RF). Stated in full in `REFACTOR-claude.md` §3.7.

`x_var` is optional for a `drc` fit. It is read from
`object$dataList$names$dName` when `NULL`, and a supplied value must equal that
name. Requiring it, and checking only that it named a column of the data,
accepted the response as the predictor without an error.

## T18 — the increasing `absolute` reference (#19, #20)

Settled 2026-10-17 (RF), amending T9. Stated in full in `REFACTOR-claude.md`
§3.10.

Every `type` except `direct` sets the reference as the control minus
(decreasing) or plus (increasing) a percentage of a span. Under `absolute` the
increasing span is 1 minus the control for a response on the proportion scale,
which is extra risk and Abbott's formula, and the control itself for any other
response, which is relative deviation. A 10 per cent increase from a control of
20 is then 22. The decreasing form is unchanged. These are the definitions of US
EPA (2012), EFSA (2022) and OECD (2006) No. 54.

A proportion is recognised from `meta$family`: the `brms` families `bernoulli`,
`beta`, `zero_inflated_beta` and `zero_one_inflated_beta`, and a `drc` fit whose
`type` is `"binomial"`. A `brms` binomial or beta-binomial fit is predicted as a
count of trials, so its increasing reference is `NA` until the number of trials
is recorded (T15).

A span of zero or less gives `NA` for the reference and for `ecnsec`, in either
direction. This replaces the error §3.10 specified for a control at or above 1.
Both directions are computed on every call (T10), so that error would also have
stopped the decreasing estimate: on the `nec4param` component of
`manec_example`, whose control draws lie between 2.03 and 2.41, every call would
have raised it. The same rule returns `NA` under `absolute` for a control at or
below 0.

Daniels, Hommen and Ratte (2025, IEAM 22:734-740) use "absolute" and "relative"
ECx in the senses toxval uses them, so the names stay. Further options, such as
bounds supplied by the user and a type reproducing `drc::ED()`, are left to a
separate issue.

## T19 — `nsec()` refuses `type = "direct"` (#49)

Settled 2026-10-17 (RF), amending T8. Stated in full in `REFACTOR-claude.md`
§3.9.

Under `direct` the `ecnsec` would be the NSEC reference itself, which the
`reference` column already reports. It does not vary between realisations, so it
has no interval either. `nsec()` therefore errors on `type = "direct"` rather
than reporting a duplicate column with `NA` bounds. `ecx()` keeps `direct`.

## T20 — the range of `sig_val`

Settled 2026-10-17 (RF).

`sig_val` lies strictly between 0 and 0.5. At 0.5 or above the decreasing
reference would lie at or above the increasing one. Values above the lower
probability of the summary interval are valid: Fisher and Fox (2023, Table 3)
report NSECs at 0.05, 0.10 and 0.20, with a lower bound at the lowest
concentration (p. 2026). The irregular intervals toxval gives above 0.025 come
from `nsec_fct()` placing the realisations whose control lies below the
reference at `max(x_vec)`, which #60 decides.

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
  code is `bayesnec/ignore/uniroot.R`. The numerical part is tractable, and the
  `type` reference semantics it depends on are now explicit (T9,
  `REFACTOR-claude.md` §3.10), so that blocker is removed.

## Issues — status 2026-09-05

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
