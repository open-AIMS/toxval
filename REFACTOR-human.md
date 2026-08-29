# toxval Refactor Plan

This is the plan we are agreeing to. It covers what we are doing, why, what
changes for someone using the package, and in what order the work happens.

The detail lives in a companion document, `REFACTOR-claude.md`: data structures,
function signatures, edge cases, the measurements behind each decision, and the
alternatives that were rejected. Section numbers in brackets below point into
it. Each decision is written out in full there and once only, so the two
documents cannot drift apart.

## 1. What we are doing

Rebuilding the path `toxval` takes from a fitted model to a toxicity estimate,
so that every metric and every model class travels the same path.

The science does not change here. Some estimates will move, but only because of
decisions listed in section 6, each of which is being made separately.

## 2. Why

**Adding support for a new model class is expensive.** Handling `glm` or `gam`
today means writing a new method for every metric. It should mean writing one
method, once.

**The same metric is computed differently in different places.** `ecx.bnecfit`
and `ecx.brmsfit` are different estimators, not two implementations of one
estimator. So are the three versions of `ecnsec` (#49). A user cannot currently
assume that fitting the same curve two ways gives the same answer.

**The return values differ by function and by model class.** Some are named
vectors, some are vectors carrying attributes. None of them compose with
anything else.

## 3. Three terms

**Realisation.** One plausible version of the fitted curve. A Bayesian fit
supplies thousands of these as posterior draws. A frequentist fit gets the same
thing from a parametric bootstrap: repeatedly draw a parameter set from the
fitted model's own uncertainty, and evaluate the curve at each. Every estimate
is computed once per realisation, and the spread across realisations gives the
confidence interval.

**`toxval` tibble.** The single table shape that every metric function returns.

**`toxval_pred`.** The object holding the realisations and the concentrations
they were evaluated at. One function builds it from a fitted model; every metric
is then computed from it.

## 4. What changes for someone using the package

Every metric function returns the same kind of table, whatever the model class:

```r
ecx(manec_example, ecx_val = 10)
#> # A tibble: 1 x 9
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing    0.832    0.818      1.05      10 absolute    1.02     0.918
```

Four practical differences.

The result is a tibble, so it filters, joins and plots like any other data. A
grouped fit returns more rows rather than a different object.

The settings that produced the estimate travel with it. `ecx_val`, `type` and
the reference value are columns, so a saved result is self-describing.

Realisations are available on request, as a list column, by passing
`draws = TRUE`. The return type does not change — it is the same tibble with one
more column.

A `drc` fit returns the same shape as a `brms` fit, including a confidence
interval built the same way.

## 5. The decisions

Each of these is settled. The reasoning is in `REFACTOR-claude.md`.

**Every metric function returns a `toxval` tibble, and model class never changes
the return type.** Core columns are always present; a descriptor column appears
when it is meaningful for that metric, and is `NA` when it is meaningful but
unavailable. [3.1]

**One function, `toxval_predict()`, is the only place that knows about model
classes.** It returns a `toxval_pred`. Every metric is a plain function
computing on that object, so adding a model class means writing one method and
nothing else. [3.3]

**A function must not change its return type based on an argument value.** This
is why the `posterior` argument is retired. [3.2]

**Uncertainty comes from realisations, generated the same way for every fit
type** — posterior draws for a Bayesian fit, a parametric bootstrap for a
frequentist one. The current `drc` intervals invert a confidence band on the
response to get an interval on concentration, which is not a valid interval and
is worst exactly where NSEC sits (#43). [3.4]

**Realisations are an optional list column rather than a second function.**
`ecx(fit, draws = TRUE)` returns the same tibble with a `draws` column, instead
of a separate `ecx_draws()`. This keeps one row per estimate, and keeps paired
quantities such as NSEC and `ecnsec` aligned by construction. [3.5]

**Direction is a property of the result, not an argument.** The estimator looks
for both an increasing and a decreasing crossing and reports what it finds. A
hormetic curve simply has both. This removes `hormesis_def`, which cannot be
repaired in place because the two `nsec` methods compute different quantities
under it, and closes #1 and #8 as a consequence. [3.6]

**Where the `nsec` reference comes from becomes a visible choice**, the `anchor`
argument, rather than a hard-coded rule. The choice is recorded as a column on
the result, so it travels with the estimate. [3.8]

**Argument checking happens once, in one place, using `chk`.** Predict-level
arguments are checked in `toxval_predict()`; metric arguments are checked in the
metric entry point, so every model class gets identical checks. [3.7]

## 6. Decisions that must be made before the work starts

These determine what the numbers are, not what shape they come in. None is
settled by this plan, and each has its own issue.

| decision | what it decides | issue |
|---|---|---|
| The `ecx` reference | how the reference response is computed | #19 |
| Direction | increasing versus decreasing responses, and how hormesis relates | #20 |
| The `ecnsec` denominator | whether `ecnsec` is a percentage of the control or of the fitted range | #49 |

The `nsec` reference does **not** block, because section 3.8 exposes it as an
argument rather than settling it. Only the default has to be agreed, and a
default can be revisited later without invalidating anything.

## 7. The order of work

Two constraints set the order.

**The `ecx` reference has to be decided first, on paper.** Phase 1 locks the
current numbers as a regression net. If #19 is answered afterwards, a genuine
refactor bug cannot be told apart from an intended change of definition.

**`toxval` must reach CRAN before `bayesnec` can depend on it.** `bayesnec` is
on CRAN and `toxval` is not. A CRAN package's dependencies must resolve from
CRAN, so the dependency reversal (#39) cannot ship until `toxval` is published.
That in turn puts the change of output shape *before* the CRAN submission, since
publishing an API that is about to break would force two breaking releases in a
row.

| # | phase | purpose | done when |
|---|---|---|---|
| 0 | Decide #19, #20 and #49; agree the `anchor` default. No code. | Fixes what the numbers should be before anything locks them | The answers are recorded on the issues |
| 1 | Lock a regression net | Makes every later phase checkable | Current estimates are captured as golden values, split into "must not move" and "expected to move, because X" |
| 2 | Build the spine: `toxval_pred`, `toxval_predict()`, the shared validator, the class-agnostic compute functions, the bootstrap | Puts the new machinery in place without disturbing anything | It exists and is tested, and `ecx()` / `nsec()` still return today's values unchanged |
| 3 | `toxval` sheds `bayesnec` | Reverses the dependency, which currently points the wrong way | `bayesnec` is no longer imported, and `ecx` on a `bnecfit` follows the #19 answer |
| 4 | Move the metrics onto the spine, one at a time, each gaining `draws` | Makes every metric class-agnostic | `ecx`, `nsec` and `nsec_multi` all compute from a `toxval_pred`, with `posterior` deprecated but working |
| 5 | Swap the outputs to the `toxval` tibble and remove the old code | Delivers the user-visible change in one step | Every metric returns a tibble, and the tests assert the new shapes |
| 6 | `toxval` to CRAN | Unblocks #39 | Published, carrying the final API |
| 7 | `bayesnec`: import `toxval`, delete its copies, adapt to the tibble | Completes the untangle | One PR, then CRAN |

Phases 2 to 4 change who owns the code and what it computes. Phase 5 changes
what it returns. Keeping those separate means each can be checked against the
regression net inside `toxval`, and `bayesnec` sees one finished API at phase 7.

## 8. What this plan does not do

- **It does not implement `n(s)ec` or `nec`.** The vocabulary reserves them so
  it does not have to be reopened, but the work is #25 and #18.
- **It does not implement `anchor = "control"`.** The argument is specified now
  so that adding it later is a new branch rather than a change to the compute
  path. It needs a separate control-only fit, which is an argument for fitting
  wrappers (#22).
- **It does not fix model-averaged reproducibility.** Repeated calls on an
  averaged fit disagree because the component resampling is unseeded. That is
  `bayesnec` #216 and has to be fixed there.
- **It does not settle `ecx` for `drc`**, which has no method today and becomes
  nearly free under the new architecture. Whether it is in scope needs a
  decision.
