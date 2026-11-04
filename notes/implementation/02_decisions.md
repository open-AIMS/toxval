# Decisions — toxval

Settled 2026-08-14; amended 2026-08-19 after the review of PR #42; extended
2026-09-04/05 with T8-T12, which settle the definitions gating the refactor, and
2026-10-09 with T13-T17, from the review of PR #59, 2026-10-17 with T18-T20,
from the review of PR #62, 2026-10-18/19 with T21-T33, the rulings phases
4 and 5 need, and 2026-10-22 to 2026-10-26 with T34 and T36-T39, from the
review of PR #66, and 2026-10-27 with T40. A session implements these rather
than re-opening them.

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

~~`bayesnec` is deleting its own `ecx()`/`nsec()`, not maintaining them.~~
Amended 2026-10-29 (T35): `bayesnec` keeps its own `ecx()` and `nsec()` for now.
The agreed NSEC no-effect rule is ported to `bayesnec` so the overlapping
functions can give the same answer. Further changes to either implementation
should be compared across packages before release.

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
- **A row with no estimate keeps its `ecnsec`** (RF, 2026-10-24, ruling 1 on
  PR #66). `ecnsec` depends only on the control and the reference, which the
  row already reports, not on a crossing.
- **Each direction has its own reference.** A single reference would return
  `NA` in both directions for a monotonic increasing response, which is the
  case #20 exists to support. `nsec_multi` already builds `reference_dec` and
  `reference_inc` (`R/helpers.R:44-59`); generalising that is what #20 asks
  for. `type = "direct"` is the exception, taking one supplied value for both
  directions.

T9 and T10 answer the two definitions that gated the refactor alongside T8. The
fourth phase 0 item, the `anchor` default, is T12; #43 is T11.

## T11 — frequentist realisations come from a parametric bootstrap (#43)

Settled 2026-09-05 (RF); amended 2026-10-30 (RF, #76) for the log-scale draw.
Stated in full in `REFACTOR-claude.md` §3.4.

Realisations are generated the same way for every model class: posterior draws
for a Bayesian fit, a parametric bootstrap for a frequentist one — draw
`n_boot` parameter vectors from `MVN(coef(fit), vcov(fit))` and evaluate the
mean function at each. A parameter that the mean function requires to be
positive is drawn on the log scale, as set out below. There is no second mode
in which the three columns of `predict(drc_fit, interval = "confidence")` are
treated as three curves.

**Why.** Inverting a pointwise confidence band on `y` does not give a valid
confidence interval on `x`. Coverage is wrong and degrades worst where the
curve is flat, which for a concentration-response curve is where NSEC sits.
Two quantities computed that differently should not share the column names
`conf.low` and `conf.high`.

**Consequence.** `drc` intervals change. They are not a correction of the
current ones but a different quantity, so this needs a NEWS entry and the
reasoning in the documentation, not a silent substitution. Built in phase 2.

**Invalid replicates** (RF, 2026-10-24, ruling 5 on PR #66). A replicate
outside the parameter space gives non-finite predictions. Such a replicate is
excluded from the summary with a warning, in `ecx()` and `nsec()` alike.

RF ruled on 2026-10-30, adopting option 2 of #76, that a parameter the mean
function requires to be positive is drawn on the log scale. `boot_parms()`
draws its logarithm from a normal with mean `log(coef)` and the delta-method
covariance `J V J'`, where `V` is `vcov(fit)` and `J` is diagonal, `1 / coef`
for each such parameter and 1 for the others. It returns `exp()` of the draw,
so every replicate is inside the parameter space. The exclusion of ruling 5
remains for any `drc` function outside this set.

The set was read from the `drc` source. It is `e` wherever the mean function
evaluates `log(dose / e)` or `log(e)`: `LL.*`, `W1.*`, `W2.*`, `W2x.*`,
`LN.*`, `BC.*`, `CRS.*`, `UCRS.*`, `lgaussian()` and `baro5()`, with `e1` and
`e2` in `twophase()`. It also includes `e` in `gammadr()`, the shape of a gamma
distribution, which must be positive for the same reason. `LL2.*` and
`LN.*(loge = TRUE)` estimate `log(e)` already and are drawn unchanged.

On `nsec_drc_2` (`LL.3`, eight points) SE(e) is 2.25 against e of 1.58. Drawn
on the scale of `e`, 24.6% of replicates had `e <= 0` and were excluded
(`n_boot = 1000`, `seed = 1`). Those replicates are the lower tail of `e`, and a
log-logistic ECx is proportional to `e`, so excluding them conditions the
interval on `e` being above 0 without saying so. On the log scale none is
excluded and `drc` no longer warns "NaNs produced". Option 1, dropping invalid
replicates in `toxval_predict.drc()`, would have reported the truncation but
kept it. The evidence is in the pull request that closes #76.

This is an approach rather than a definition, so it did not gate phase 0.

## T12 — the default NSEC reference (#19, `nsec` half)

Ratified 2026-09-05 (RF). Stated in full in `REFACTOR-claude.md` §3.8.

The original default was `"model"`: its NSEC reference is the `sig_val`
quantile of the model-averaged control posterior. The `"component"` reference
uses every control draw of each equation's fit. On 2026-10-29 RF changed the
default for a `bayesmanecfit` to `"component"`, so an ordinary toxval call
agrees with `bayesnec::nsec()` on the same fit. Other classes continue to use
`"model"` by default. Both choices remain explicit arguments and appear in
the result. The reserved `"control"` choice is not implemented.

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

`nsec_multi()` becomes a deprecated wrapper around `nsec()`. Amended
2026-10-24 (RF): `nsec_multi()` is defunct rather than a wrapper (T37). Its
arguments are redistributed:

- `multi_var` and `trials_var` are no longer arguments. `toxval_predict.brmsfit()`
  reads the responses, their families and any trials column from the fit with
  `brms::brmsterms()` (amended 2026-10-23, AP).
- `type = "both"`, `"increasing"` and `"decreasing"` become filters on the
  `direction` column (T10).
- ~~`type = "lower"` with `criterion` stays an argument of `nsec()`, under a new
  name chosen in phase 4, because `type` in `nsec()` selects the `ecnsec`
  definition (T8).~~ Amended by T30: a documented filter on the result.

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
is recorded (T15). Amended 2026-10-23 (AP): such a fit is predicted at one
trial, so it is a proportion with the bound 1 (T26).

A span of zero or less gives `NA` for the reference and for `ecnsec`, in either
direction. This replaces the error §3.10 specified for a control at or above 1.
Both directions are computed on every call (T10), so that error would also have
stopped the decreasing estimate: on the `nec4param` component of
`manec_example`, whose control draws lie between 2.03 and 2.41, every call would
have raised it. The same rule returns `NA` under `absolute` for a control at or
below 0.

Daniels, Hommen and Ratte (2025, IEAM 22:734-740) use "absolute" and "relative"
ECx in the senses toxval uses them, so the names stay. Further options, such as
bounds supplied by the user and a type reproducing `drc::ED()`, are left to
#63.

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

## T21 — realisations with no crossing (#60)

Settled 2026-10-18 (RF), adopting `bayesnec` D17 ruling 1 and the `bayesnec`
#404 rule, as proposed on #60 on 2026-10-17.

A realisation that does not cross a reference is placed where its crossing
lies:

- A realisation whose control already lies beyond the reference in the
  direction sought takes the lowest concentration in the data the model was
  fitted to. That is the control concentration, ~~which `x_vec[1]` is not when
  `x_range` is supplied~~ (amended 2026-10-28, T41: the grid starts at the
  control concentration, which `x_control` can name). In `nsec()`, one that
  shows no effect in the tested range is censored instead (amended 2026-10-27,
  below).
- A realisation with no crossing within the evaluated range keeps its rank
  above `max(x_vec)` and is not given a value. A quantile that falls among such
  realisations is reported as `max(x_vec)`, and the call warns with the
  censored fraction (T25).

Amended 2026-10-29 (RF): when any realisation is censored, the reported
quantiles use observed order statistics (`stats::quantile(type = 1)`). This
matches `bayesnec::summarise_censored()`. Interpolating a finite crossing with
an unknown crossing beyond the grid would report the boundary even when the
order statistic is finite. Uncensored summaries retain type 7.

The increasing direction mirrors the first rule against the `1 - sig_val`
reference. In `ecx()` the second rule applies as in `nsec()`. A realisation
whose `ecx()` reference is `NA`, a span of zero or less (T18), is excluded from
the summary and counted in the warning, not censored. Under `range` it is
censored instead (T38).

`meta` does not record the lowest concentration in the data, so the first rule
adds `meta$x_control`, the concentration assumed to be the control (RF,
2026-10-19). The `x_` prefix, as in `x_var` and `x_range`, separates it from the
`control` slot (T13), which holds response realisations from a control-only fit. Adding it changes the public
`toxval_pred` contract (§3.3).

Amended 2026-10-27 (RF) on #74, for `nsec()` only. For a realisation whose
control is past the NSEC reference in the direction sought:

1. If it crosses the reference in the direction sought, it takes that crossing,
   as before.
2. Otherwise it is censored above `max(x_vec)`, and counted in the censored
   fraction (T25), when both of these hold:
   - its first change in response, as concentration rises from the control,
     is away from the effect: rising for a decreasing reference, falling for an
     increasing one. The first change is the first difference between adjacent
     grid values whose absolute value exceeds `1e-8` times the largest
     absolute response of that realisation. A realisation with no such
     difference counts as away;
   - it is on the no-effect side of the reference at a later grid value. With
     no crossing in the direction sought, it does not return.
3. Every other such realisation takes the control concentration, as before.

A realisation that meets both conditions is past the reference at the control
only because its own control lies in the tail of the control realisations,
which by construction is true of about `sig_val` of them. Its response changes
away from the effect and does not return, so it shows no effect within the
tested range. At the control it was ranked with the most sensitive
realisations, which lowered the lower limit and sometimes the upper limit.

Each condition alone was rejected, because each misplaces realisations in the
test fixtures. Being on the no-effect side at a later grid value, alone,
censors a realisation that first changes further into the effect and leaves
only near the top. In the increasing row of `brms_model_5`, the 10 realisations
above the reference at the control rise, have no increasing crossing, and fall
below the reference at x = 1.98 to 2.10, on a grid from 0.032 to 3.22. Their
effect begins at the control, which is where rule 3 places them. A first change
away from the effect, alone, censors hormetic realisations that rise a little,
stay past the reference throughout, and then fall steeply. The decreasing row
of `bnec_model_1` has 4 (#74).

The tolerance is needed because some curves are flat at the control. `nec`
equations hold a plateau, and `drc` log-logistic curves change by about 1e-14
near 0, a change whose sign is arbitrary. Rule 1 is kept because a small first
change does not locate the crossing. The decreasing row of `brms_model_3` has 10
hormetic realisations that start just below the reference, dip by about
0.0003, rise above it and cross it near 1.2, where the other realisations
cross. They keep that crossing.

`ecx()` keeps the first rule unchanged for every `type` (RF, 2026-10-27).
Under `absolute`, `relative` and `range`, each realisation's reference is built
from its own control, so no control is past its own reference and the rule
could not apply. Under `direct` the reference is a level the user supplies. A
realisation past it at the control is beyond that level, not past it because
of its own control, and the control concentration is the right answer for it.
Applying the rule there would change answers wholesale rather than correct a
few. In `ecx(brms_model_1, type = "direct", reference = 0.5)`, 95% of the
controls exceed 0.5. The rule would censor 858 of 1,000 realisations in the
increasing row, and change its estimate from the control (0.8) to the highest
concentration (1.05) (#74).

Run on `02f543f` with and without the amendment, at 500 grid points unless
stated, the rule censors 7 of the 10 realisations at the control in the
decreasing row of `brms_model_1`, whose lower limit changes from 0.9847278 to
0.991338. It censors all 10 in the decreasing row of `brms_model_6`, from
0.966905 to 0.9716878. The estimates and upper limits of both stay at 1.05. In
`brms_model_2` with `group_var = "z"`, it censors 9 of 10 in the decreasing row
of group 2, whose lower limit changes from 0.9789 to 1.004, and 8 of 10 in the
increasing row, from 0.9205 to 0.9418. In the sp_growth decreasing row of
`nsec_multi_model_2` at 50 points it censors 5 of 10, and the NSEC changes from
0.8591841 (0.3908073 to 1.5) to 0.8671501 (0.410851 to 1.5). These agree with
the values in #74, computed outside toxval with quantile type 7. They precede
the 2026-10-29 type 1 summary amendment above.

The 2026-10-27 amendment departed from `bayesnec` D17 ruling 1 and the
`bayesnec` #404 rule. On 2026-10-29 the same rule was adopted in `bayesnec`.

## T22 — `lifecycle` joins `Imports`

Settled 2026-10-18 (RF).

The deprecations in this plan use `lifecycle::deprecate_warn()`, as T9 and
`REFACTOR-claude.md` §3.5 specify: of `hormesis_def` and `horme` (T28), of
`nsec_multi()` (T15), and of `posterior` (§3.5). `lifecycle` is added to
`Imports` in phase 4. Amended 2026-10-24 (RF): `nsec_multi()` and `posterior`
are made defunct with `lifecycle::deprecate_stop()` instead (T37). The change of meaning of `relative` is not a deprecation,
and has a plain warning (T29).

## T23 — the scope of `anchor` in phase 4 (T12)

Settled 2026-10-18 (RF). The argument is specified in `REFACTOR-claude.md`
§3.8.

`nsec()` accepts `"model"` and `"component"`. Its default resolves to
`"component"` for a `bayesmanecfit` and `"model"` for other classes (T12).
~~`"model"` and `"component"` give the same values for every class toxval
supports, since none is model-averaged.~~ Amended 2026-10-29 (T35): they differ
for a `bayesmanecfit`, where `"component"` takes each equation's reference from
~~the realisations that `meta$draw_model` assigns to that equation~~ the control
of every draw of that equation's fit, recorded in `meta$model_control`, as
`bayesnec::nsec()` does (amended 2026-10-29, RF). Taking it over the
realisations alone gave a lower limit 0.176 below `bayesnec`'s on
`manec_example`, because a 1% quantile of an equation's 17 realisations lies
nearer the centre of its control than one of its 100 draws
(`BAYESNEC-claude.md` §3.2). They give the same values for every other class.
The `anchor` column appears on the result from phase 4, which is done with
phase 5 (T33).

`"control"` and its `control_fit` argument are left out of phase 4 (RF,
2026-10-19, amending this entry as first settled). `"control"` was meant to
align the NSEC with the LECx of de Bruyn and Elphick (2013), and it does not. A
reference from a control-only fit is the NSEC-cv withdrawn from
`comparing_tox_ests` on 2026-09-29 (`compare_nsec_lecx.qmd`, "Changes from the
June 2026 version"). On the fish growth data of that document, with the
exponential decay fitted by `nls()` and a one-sided alpha of 0.05, the NSEC-cv
was 4.84 with an effect of 13.4 per cent. The NSEC under `"model"`, which is
LECx method 2, was 5.37 with 8.6 per cent, and LECx method 1 was 10.9 with 27.0
per cent (2026-10-19). LECx method 1 is `ecx()` at the percent minimum
significant difference (PMSD) of Dunnett's test. `"control"` should probably be
replaced by a route to LECx method 1 rather than implemented later (#63).

## T24 — the source and storage of the asymptote (#63)

Settled 2026-10-18 (RF). `REFACTOR-claude.md` §3.4 and §3.10 refer to it.

- `toxval_predict.brmsfit()` gains `asymptote = NULL`. It takes the names of
  the non-linear parameters, as `c(decreasing = "bot", increasing = "top")`, or
  numeric values.
- `toxval_pred` gains an `asymptote` slot. It is `NULL`, or a list aligned to
  the rows of `keys`, each element a list of two per-realisation vectors named
  `decreasing` and `increasing`, `NA` where the equation has no such asymptote.
- `toxval_predict.drc()` fills it from `fct$lowerAs()` and `fct$upperAs()`,
  applied per bootstrap row and per curve. Given a parameter matrix,
  `lowerAs()` returns a wrong value with a warning.

The `drc` limits are not looked up from `fct$name`, as §3.10 first specified.
`fct$name` is `"::"` for a fit made with `drc::LL.4()`, and custom `names =`
change the parameter letters.

Implemented 2026-10-23 (AP), with two points the ruling did not cover:

- `brms` applies the link to the whole non-linear formula, so a named
  parameter is on the link scale: `bot` of `brms_model_4` (beta, logit link)
  is about -5.0, an asymptote of 0.0067 on the response scale. A named
  parameter is taken per draw with `posterior_linpred(nlpar = )` at the first
  grid value and transformed by `stats::make.link()`'s inverse; a numeric
  `asymptote` is on the response scale. A link `make.link()` does not know
  errors and asks for a number.
- `asymptote` is refused for a multivariate fit until an asymptote per
  response is needed.

`asymptote` is a descriptor column of every `ecx()` and `nsec()` result,
`NA` unless `type = "relative"` (§3.10 Output).

## T25 — warnings for censored and excluded realisations (#6, #24)

Settled 2026-10-18 (RF).

A call warns once when a reported quantile is censored (T21) or a realisation
is excluded. The warning gives the fraction for each curve and direction and
suggests a wider `x_range`. The call returns the estimate rather than failing
(T3).

## T26 — `meta` for a multivariate fit (T15)

Settled 2026-10-18 (RF).

- `meta$family` becomes a character vector named by response, and stays one
  string for a univariate fit. `response_upper_bound()` reads it per curve.
- `meta$multi_var` holds the response names as `posterior_epred()` returns
  them (for example `spsurvival`), matched to the data columns by name rather
  than by position.
- ~~`trials_var` is recorded per curve, and T18's increasing `absolute` reference
  for a binomial response uses it.~~ Amended 2026-10-23 (AP), confirmed
  2026-10-24 (RF, ruling 6 on PR #66): a binomial response is predicted at one trial, so its curve,
  `control` and `reference` are proportions and its upper bound is 1. Trials
  often vary between observations (eggs counted per well, larvae offered), so
  no single count scale exists, and a scale that switched with the data was
  rejected. NSEC, ECx and `ecnsec` do not depend on the number of trials. The
  trials column is read from the fit and recorded as `meta$trials_var`; a
  constant or an expression in `trials()` is an error.
- `meta$multi_var` and the `response` column hold the `brms` response names
  (`spsurvival`, not `sp_survival`), matched by name (2026-10-23, AP).

A fit whose trials are a normalising total rather than a ceiling on the
response then also takes extra risk, which puts its increasing `ecnsec` close to
0 (#63, comment of 2026-10-19). That is accepted for phase 4 as an edge case
(RF, 2026-10-19). It is to be handled when #63 is taken up, by a span the user
names (#63 option 1 in its second form). `REFACTOR-claude.md` §3.10 states it.

## T27 — `ecx()` for `drc` fits

Settled 2026-10-18 (RF).

`ecx()` accepts a `drc` fit from phase 4. Once `ecx()` calls `toxval_predict()`
this needs no new code path. It is tested against `drc::ED()` where the
definitions coincide, within bootstrap error: `direct` against
`ED(type = "absolute")`, and the new `relative` against `ED(type = "relative")`
once `relative` is implemented.

## T28 — retired arguments

Settled 2026-10-18 (RF).

`hormesis_def` and `horme` are deprecated (T22) and ignored, since both
directions are computed (T10), and the tests marked #20 change. `by_group`,
`group_var = NA`, `x_range = NA` and `curveid` are translated to the new
arguments without a warning. They are deprecated with the change to the
`toxval` tibble.

Settled 2026-10-24 (RF), rulings 2 and 4 on PR #66:

- `by_group = FALSE` with a `group_var` is an error (T3), since the pooled
  result it asked for is no longer computed.
- The decline from the peak, `hormesis_def = "max"`, is not reimplemented. It
  defines a different reference, and further reference definitions are the
  scope of #63. It is no longer available to `bayesnec` users after phase 7,
  which the phase 7 plan states.

## T29 — `relative` takes its new meaning in phase 4 (T9)

Settled 2026-10-19 (RF), amending the last point of T9 and adopting `bayesnec`
D15 ruling 8 (2026-09-06).

From phase 4, `type = "relative"` measures from the control to the asymptote
(T24), and the old quantity is `type = "range"`. A call that supplies `relative`
explicitly warns that its meaning has changed and names `range`. Where the bound
is infinite, an equation without an asymptote fitted with a family that has no
bound, the call errors and names `absolute` and `range` (T9).

The warning is a plain `warning()`, not `lifecycle::deprecate_warn()`, because
`relative` is not deprecated and a lifecycle message would say it is. Each
published relative ECx found in the toxval-review literature review
(2026-10-17) is measured to a fitted limit, as in Ritz et al. (2015, p. 9,
Equation 9). Keeping the old quantity under `relative` for one more release
would also make toxval and `bayesnec` disagree for that release.

## T30 — no argument replaces `type = "lower"` (T15)

Settled 2026-10-19 (RF), amending T15.

`nsec()` takes no argument for `nsec_multi()`'s `type = "lower"`. The rule keeps,
for each response, the direction whose NSEC is lower at the `criterion`
quantile of its realisations. That is a filter on the result returned with
`draws = TRUE`:

```r
nsec(fit, x_var = "dose", draws = TRUE) |>
  dplyr::mutate(
    at_criterion = vapply(
      draws,
      \(d) quantile(d$value.draw, 0.8, na.rm = TRUE),
      numeric(1)
    )
  ) |>
  dplyr::filter(!is.na(estimate)) |>
  dplyr::group_by(response) |>
  dplyr::slice_min(at_criterion, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
```

The same filter ran as written on `compute_nsec()` output for `brms_model_4`,
with its seven herbicide groups in place of responses (2026-10-19). A tie keeps
the decreasing row, which comes first, as `nsec_multi()` does today. Censored
realisations rank above `max(x_vec)` (T21), and enter the quantile there.
Excluded realisations are `NA` in `value.draw`, hence `na.rm = TRUE`.

A row whose estimate is `NA` is dropped before choosing (RF, 2026-10-24, ruling
7 on PR #66). Such a direction was searched and has no crossing (T10), so it
cannot be the lower NSEC. Without the filter, `nsec_multi_model_2` at
`criterion = 0.01` kept the spsurvival increasing row, whose estimate is `NA`.

~~`nsec_multi(type = "lower", criterion = )` keeps working through the
deprecated wrapper, which applies the filter itself.~~ Amended 2026-10-24 (RF):
`nsec_multi()` is defunct (T37). The `nsec()` documentation shows the filter
as an example.

## T31 — `resolution` defaults to 500

Settled 2026-10-19 (RF).

Every entry point defaults to `resolution = 500`. `nsec_multi()` uses 50 today,
and the other entry points 100 or 1000. The grid is evenly spaced in
concentration, while dilution series are geometric, so it is coarsest where an
EC10 or NSEC lies. On a simulated `LL.4()` fit to doses from 0 to 100 (1000
bootstrap realisations, against a 10000-point grid), the largest error in a
reported quantile was 21 per cent of the interval width at 50, 8 at 100, 2 at
200, 0.34 at 400 and 0.33 at 1000. On each of the seven fits tested whose
interval has a finite width, it was below 0.35 per cent at 400 or 500 points.

Peak memory grows in proportion to the grid: on `nsec_multi_model_2` (two
responses, 1000 draws) it rose by about 36 bytes per draw, grid point and
response, 1.1 GB between 1000 and 16000 points (2026-10-19). A fit with 8000
draws and four responses would then need about 1.2 GB at 1000 points and half
that at 500, with no gain in accuracy above 500. A grid evenly spaced in log
concentration is not a remedy: on `drc::ryegrass` it left a 28 per cent error
at every resolution, because the span from the zero control to the lowest
tested concentration then has one grid interval.

## T32 — `proportion` replaces `ecx_val` (#3)

Settled 2026-10-19 (RF), for the release that introduces the tibble (phase 5),
so that users meet one breaking release.

`ecx()` takes the effect size as `proportion`, so the EC10 is
`proportion = 0.1`. `ecx_val` is accepted for one release behind
`lifecycle::deprecate_warn()` and divided by 100. Supplying both is an error
(T3). The same argument cannot change scale in place: `chk_ecx_val()` accepts
any positive value, so `ecx_val = 0.5` would mean 0.5 or 50 per cent depending
on the release. `ssdtools` took the same path in 2.0.0, deprecating
`ssd_hc(percent)` in favour of `proportion` and dividing by 100. `msPAF`'s
`hc_fun()` already passes one `proportion` argument to both `ssd_hc()` and
`ecx()`, converting it to `ecx_val = proportion * 100` for the second (`msPAF`
`R/hc_fun.R:31`).

`ecnsec` becomes a proportion in the same release, so that it stays the inverse
of the `ecx` reference (T8). The `ecx_val` column of the tibble
(`REFACTOR-claude.md` §3.1) is renamed `proportion`. `proportion` accepts
several values from the start, one row each, which answers #9.

Under `type = "direct"` the value is a response, not a proportion, and it is
supplied as `reference` (RF, 2026-10-19), the output column that already
reports it: `ecx(fit, type = "direct", reference = 0.5)`. `reference` is
required under `direct` and refused under any other `type`, and `proportion` is
refused under `direct` (T3). The `proportion` column is `NA` on a `direct` row.
In the deprecation release, `ecx_val` under `direct` is taken as `reference`,
not divided by 100. `nsec()` does not take `reference`, since it refuses
`direct` (T19). `response` was not used, because it names the column that
identifies each response of a multivariate fit (`REFACTOR-claude.md` §3.1).

`drc::ED()` offers no alternative. Its `type = "absolute"` is toxval's `direct`,
and it takes the response value through `respLev`, the same argument that holds
the percentage under `type = "relative"`. Its own `reference` argument selects
whether the control or the upper limit is the reference ("control" or "upper",
`drc` 3.0.1), so the name has a different meaning there.

## T33 — phases 4 and 5 are done together

Settled 2026-10-19 (RF), subject to AP's review of the pull request that records
it.

Phase 4 rebuilds the metrics on the spine and phase 5 changes what they return.
One branch does both. Kept apart, phase 4 would return today's shapes. The
increasing direction, `anchor` and `draws` would be computed but not reported
until phase 5. `posterior` would keep working without a warning, and `nsec()` on
a `drc` fit would keep refusing an increasing curve. Done together, that interim
layer is not written, and each changed test is edited once rather than twice.
The `toxval` tibble class is built first and additively, as phase 2 built the
spine. The check that numbers do not change unintentionally compares values
recorded before the change with the decreasing rows of the new output.

## T34 — `control` is summarised over the realisations of the estimate (T21, T25)

Settled 2026-10-22 (RF) on PR #66.

The `control` column of a row is the central quantile of the control
realisations not excluded from that row (T21), the same realisations as the
estimate and the `reference` column. Each row then describes one set of
realisations, and the warning reports the fraction excluded from it (T25).
Summarised over every realisation, the printed reference under `absolute` was
not the stated fraction of the printed control: on `brms_model_1` at a
resolution of 500, 8 of 1000 realisations have a negative control and no
reference, and the EC10 reference was 0.8158 against 0.9 x 0.9045 = 0.8140.

Under `absolute` every reference is an increasing linear function of the
control (0.9 x control decreasing, 1.1 x control increasing, 0.1 + 0.9 x control
increasing on a proportion), and its quantiles commute with such a
function, so the relation holds exactly in the printed table. Under `range` the
reference also depends on each realisation's minimum or maximum, so it does
not.

Each direction excludes its own realisations, so `control` can differ between
the two rows of one curve. Where every realisation of a row is excluded,
`control` is `NA`. Under `relative` that is the row of a direction with no
asymptote, which has no estimate and no reference either. Under `range` a
realisation with no span is censored rather than excluded (T38), so the
increasing row of a monotone decreasing curve keeps its `control`. `nsec()` is unaffected: its reference is one finite value per curve,
so no realisation is excluded.

## T35 — `toxval` contains the `toxval_predict()` methods for `bayesnec` fits (#68)

Settled 2026-10-23 (RF and AP) on #68, replacing the split proposed on #39.
Stated in full in `BAYESNEC-claude.md` §1.

`toxval` contains the `toxval_predict()` methods for fits made with `bayesnec`,
and lists `bayesnec` in `Suggests`. `ecx()` and `nsec()` then accept a `bnec()`
fit again. `bayesnec` does not import `toxval`, so CRAN publication of `toxval`
(#45, phase 6) no longer has to come before support for `bayesnec` fits
(phase 7).

A `toxval_pred` holds facts that `toxval` defines for its compute functions and
that `bayesnec` has no use for: the `keys` table (T14), `meta$x_control` (T21),
the per-realisation asymptotes (T24), the per-curve trials (T26), and, for a
model-averaged fit, the equation behind each realisation. Each was added or
changed during phases 2 to 4. Had the methods been in `bayesnec`, each change
would have needed a `bayesnec` release made after the `toxval` release that
introduced it, and `bayesnec` could not import `toxval` at all until `toxval`
was on CRAN (#45).

The rejected alternative is the split of #39, in which `bayesnec` held the
methods and imported `toxval`. Under it every change to the `toxval_pred`
contract would need a release of both packages, and support for `bayesnec` fits
would wait on CRAN publication of `toxval`.

`toxval_pred` stays exported and documented, and a change to its contract stays
a breaking change, because another package may still supply a
`toxval_predict()` method. What `bayesnec` does with its own `ecx()` and
`nsec()` is for `bayesnec` to decide.

## T36 — a grid value with no finite prediction is an error (T3)

Settled 2026-10-24 (RF), ruling 3 on PR #66. T35 was held for #68 and
recorded on 2026-10-29.

`toxval_predict()` errors when a value of `x_vec` has no finite prediction in
any realisation, naming the affected range of the predictor. An `x_range` below
0 for a formula in `log(x)` is the usual case. Such a grid previously gave
values, then `NA`. A grid value with no finite prediction in any realisation is
a mismatch between `x_range` and the model, so it is an error rather than a
coercion (T3).

## T37 — `nsec_multi()` and `posterior` are defunct

Settled 2026-10-24 (RF) on PR #66, amending T15, T22, T30 and
`REFACTOR-claude.md` §3.5.

`nsec_multi()` and the `posterior` argument of `ecx()` and `nsec()` are made
defunct with `lifecycle::deprecate_stop()`, rather than kept as a deprecated
wrapper and a deprecated argument. A call to `nsec_multi()` stops with an error
that names `nsec()`, and a call that supplies `posterior` stops with one that
names `draws`. The stubs are deleted in a later release.

The wrapper kept a call running, but it returned the new `toxval` tibble, so
code that read the old output, such as the `dec_val`, `inc_val` and `vars`
columns, failed anyway, later and further from the cause. The old function also
predicted a binomial response at the trials in the data, so its `control` and
`reference` were counts where `nsec()` now reports proportions (T26).
`posterior = TRUE` returned a numeric vector of realisations and now returns a
`toxval` tibble, so code that used the vector fails in the same way:
`quantile(ecx(fit, posterior = TRUE), 0.5)` stops with "cannot xtfrm data
frames". An error at the call names the replacement where the code has to
change.

The other retired arguments, `ecx_val`, `hormesis_def`, `horme`, `by_group` and
`curveid`, stay deprecated for one release (T28, T32). Each is ignored or
translated, so a call that supplies one returns the same rows as the call that
replaces it.

No argument replaces `multi_var` or `trials_var`, since `nsec()` reads the
responses of a multivariate `brmsfit` from the fit. `type = "lower"` becomes
the documented filter alone (T30).

## T38 — realisations with no span under `range` are censored (T21)

Settled 2026-10-24 (RF) on PR #66, amending T21.

Under `range`, a realisation whose span is zero or less is censored, not
excluded. Exclusion is kept for a realisation whose control is not finite, and
for a span of zero or less under `absolute` and `relative`.

Under `range`, the span is the distance from the control to the lowest
(decreasing) or highest (increasing) response the curve predicts. A span of
zero means the realisation never falls below, or rises above, its control
within the evaluated range, which is the definition of a censored realisation.
Excluding it instead summarises only the realisations that show an effect in
that direction.

On `brms_model_1` (`y ~ log(x)`, 1000 draws, `resolution = 500`), 951 draws
decrease and 49 increase. With exclusion, the increasing `range` EC10 is 0.822
(0.822, 0.822), computed from the 49 increasing draws with 95.1% excluded, and
its `control` is 0.404 against 0.914 on the decreasing row. With censoring, the
decreasing row is 0.822 (0.822, 1.05) and the increasing row 1.05 (0.822,
1.05).

The censored realisation's reference stays `NA`, so the `reference` summary is
unaffected. `nsec(type = "range")` is unaffected, since its reference does not
depend on the span; its `ecnsec` stays `NA` for such a realisation.

## T39 — `...` follows `object`, and an unused argument is an error

Settled 2026-10-26 (AP) on PR #66.

In `ecx()`, `nsec()`, the `toxval_predict()` generic and its methods, `...` is
the second argument, so every argument after `object` must be named in full.
R matches an argument before `...` by partial name and by position, so
`ecx(fit, x_var = "x", resol = 20)` ran at a resolution of 20 without a
message. After `...`, only an exact name matches.

The `brmsfit` and `drc` methods of `toxval_predict()` error when `...` is not
empty, naming the arguments. They never passed `...` on, so a misspelt,
abbreviated or unnamed argument was dropped without a message. The check is in
the methods rather than in `ecx()` and `nsec()`, which pass `n_boot`, `seed`
and `asymptote` through `...` to the method that takes them. A method defined
in another package decides for itself what to do with `...`.

An argument to the underlying prediction, such as `ndraws` for
`brms::posterior_epred()`, is added as a named argument of the method rather
than passed through `...`.

## T40 — `toxval` tibbles combine only when their settings match

Settled 2026-10-27 (AP) on PR #66.

`vctrs` methods `vec_ptype2.toxval.toxval()` and `vec_cast.toxval.toxval()`
let `dplyr::bind_rows()` combine `toxval` tibbles whose `settings` are
identical, keeping the class and header. Tibbles whose settings differ, such as
`ecx()` on a `brmsfit` and `nsec()` on a `drc` fit, are an error that says to
convert each with `tibble::as_tibble()` first. The header describes every row
(§3.1), and no single header describes rows from different runs.

Before, `bind_rows()` kept the settings of the first tibble and printed
"Model: brmsfit" and "posterior draws" above the `drc` rows. A
`dplyr_reconstruct.toxval()` method, which §3.1 and the phase 5 row planned,
cannot prevent this: `bind_rows()` passes it only the first tibble as the
template, and the generic strips the combined data to a bare data frame, so the
settings of the other tibbles are never seen. No `dplyr_reconstruct()` method is
written. `filter()`, `select()`, `mutate()`, `arrange()`, `slice()`, the joins
and `distinct()` keep the class and the header through the default method,
which copies them from the input they describe.

An error was chosen over dropping to a plain tibble, which `bind_rows()` cannot
be made to do, and over moving the settings into columns, which would change
§3.1. `vctrs` is added to Imports; it is already a dependency of `dplyr` and
`tibble`.

## T41 — the grid starts at the control concentration (#73)

Settled 2026-10-26 (RF) on #73, and for a control outside the data 2026-10-28
(RF).

The control is the predicted response at the control concentration. By default
that is the lowest concentration in the data, and `x_control`, an argument of
`toxval_predict()`, `ecx()` and `nsec()`, names another. The grid starts at the
control concentration and `meta$x_control` records it. The first grid value,
which the compute functions read as the control, and the concentration T21
places realisations at are therefore one value. `x_range` sets the upper end of
the grid only, and a lower bound other than the control concentration is an
error. `new_toxval_pred()` refuses an object whose `x_vec[1]` differs from
`meta$x_control`.

An `x_control` outside the range of the data is accepted with a warning that
gives the range of the data. The request is valid, so it is not refused (T3),
but the control is then predicted at a concentration that was not tested, and
every reference, ECx and NSEC is computed from it. A value at which the model
gives no finite prediction, such as 0 under `log(x)`, is still an error (T36).

A grouped fit keeps one control concentration for every curve, the lowest
across all the data. Giving each curve its own `x_vec` and `x_control` changes
the `toxval_pred` contract and is decided separately.

Before, the control was read at the first grid value, which is `x_range[1]`
when `x_range` is supplied, while T21 placed realisations at the lowest
concentration in the data. On `LL.4()` fitted to `drc::ryegrass`,
`x_range = c(1, 30)` changed the control from 7.79 to 7.50 and the decreasing
EC10 from 1.49 to 1.65, without a message. Starting the grid at the control
concentration was preferred to predicting the control separately, which would
add a slot to `toxval_pred`. A call whose `x_range` starts elsewhere now errors,
which is a breaking change.

## T42 — agreement with `bayesnec` (bayesnec #255)

Settled 2026-10-30 (RF), with `bayesnec` decision D46, which closed bayesnec
#255.

`bayesnec` keeps its own `ecx()`, `nsec()` and `ecnsec()` and does not import
`toxval`, so the two packages compute the same endpoints independently. For a
`bayesnecfit` or `bayesmanecfit`, and for `nsec()` on a `brmsfit` or `drc` fit,
they are to return the same estimates under matching options. Where a default or
the shape of the output differs, the difference is recorded: `ecx()` and `nsec()`
here return the increasing direction as a separate row (T10), and the predictor
scale is open on #82. On 2026-10-31 RF limited the comparison to a change that
edits `ecx()` or `nsec()` in either package, `nec()` or `ecnsec()` in
`bayesnec`, or a function they call, and that changes a value they return for
the same fitted object. Such a change is compared with the other package before
it merges, and no other change is. `CLAUDE.md` §2 gives the check.

On 2026-10-29 paired NSEC draws from the two packages agreed on a model-averaged
fixture and on a heavily censored `brmsfit`. By then `bayesnec` had adopted T21
as amended on #74 and the 500-point grid of T31 (bayesnec #527), and `toxval`
used each equation's control for a model average (T12). The `drc` method was not
compared.

---

## Still to decide

Listed so they are not mistaken for oversights.

- ~~**#4** — always return a tibble.~~ **Decided 2026-08-19**: yes, a `toxval`
  tibble subclass. See `REFACTOR-claude.md` §3.1 and PR #42 / #44.
- **#22** — whether `toxval` takes on model-fitting wrappers at all. Scope
  question for the author; it changes what the package *is*. Not at this stage
  (RF, 2026-10-19). `anchor = "control"`, the case for a helper over
  `brms::brm(y ~ 1, ...)` (`REFACTOR-claude.md` §3.8), is left out of phase 4
  (T23).
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
