# toxval Refactor Plan

What we are doing, why, what changes for someone using the package, and in what
order the work happens.

The detail lives in `REFACTOR-claude.md`: structures, signatures, edge cases, the
measurements behind each decision, and the rejected alternatives. It uses **the
same section numbers as this document**, so §3.4 here and §3.4 there are the same
subject at two depths. Each decision is written out in full there and once only;
here it is the decision, a short reason, and a pointer.

## 1. Motivation

The refactor has three goals:

1. Make adding a **new model class** easy.
2. Fix the current duplication, inconsistencies, and bugs.
3. Standardise all return structures.

## 2. Scope

**In scope.** Restructure current functionality onto the new architecture:

- standardise outputs to a `toxval` tibble
- replace the `posterior` argument with an optional `draws` list column
- consolidate argument validation on `chk`
- remove duplication, dead code, and fix the known inconsistencies

**Out of scope, and decided first.** Three questions determine what the numbers
are, not what shape they come in. None is settled by this plan, each has its own
issue, and section 4 cannot start until they are answered.

| decision | what it decides | issue |
|---|---|---|
| The `ecx` reference | how the reference response is computed | #19 |
| Direction | increasing versus decreasing responses, and how hormesis relates | #20 |
| The `ecnsec` denominator | whether `ecnsec` is a percentage of the control or of the fitted range | #49 |

The `nsec` reference does **not** block, because §3.8 exposes it as an argument
rather than settling it. Only the default has to be agreed, and a default can be
revisited later.

## 3. Design decisions

### 3.1 Output contract: everything is a `toxval` tibble

Every metric function (`ecx()`, `nsec()`, etc) returns the same tidy container,
a `tbl_df` subclass named `toxval`. Model class never changes the return type.

**The column rule.** Core columns are always present. A descriptor column appears
when it is *meaningful* for that metric, and takes `NA` when it is meaningful but
unavailable. So `group` is absent for an ungrouped fit, while `control` is
present but `NA` for `type = "direct"`.

Core columns: `metric`, `direction`, `estimate`, `conf.low`, `conf.high`.

Descriptor columns, each listed once against every metric it appears for:

- `group` — one row per level, when grouped
- `response` — one row per response, for multivariate fits
- `ecx_val`, `type` — `ecx`
- `sig_val`, `anchor` — `nsec`, `n(s)ec`
- `control` — `ecx`, `nsec`
- `reference` — `ecx`, `nsec`
- `ecnsec`, `ecnsec.low`, `ecnsec.high` — `nsec`

`control` is reported for `nsec` even though the NSEC does not use it, because
`ecnsec` is a percentage change from the control and cannot be read without it.

`metric` is a closed vocabulary — `"ecx"`, `"nsec"`, `"nec"`, `"n(s)ec"`,
`"noec"` — with the value that parameterises it in its own column: `metric =
"ecx"` with `ecx_val = 10`, never `metric = "ec10"`. Run settings are displayed
as tibble header lines via `tbl_sum()`, as `# Groups:` is on a grouped tibble.

Some examples of `ecx()`:

```r
# basic output, all columns have values
ecx(manec_example, ecx_val = 10)
#> # A tibble: 1 x 9
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing    0.832    0.818      1.05      10 absolute    1.02     0.918

# control is NA but still present
ecx(manec_example, type = "direct", ecx_val = 0.5)
#> # A tibble: 1 x 9
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high ecx_val type   control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <dbl>     <dbl>
#> 1 ecx    decreasing     1.13     0.98      1.31     0.5 direct      NA       0.5

# grouped output -- more rows, plus a `group` column
ecx(brms_grouped, x_var = "x", group_var = "site", by_group = TRUE, ecx_val = 10)
#> # A tibble: 3 x 10
#> # Model:    brmsfit
#> # Settings: resolution 1000 | 4000 posterior draws
#> # Groups:   site [3]
#>   metric direction  group estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>      <chr>    <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing A         0.71     0.55      0.88      10 absolute    0.98      0.88
#> 2 ecx    decreasing B         1.02     0.83      1.24      10 absolute    1.10      0.99
#> 3 ecx    decreasing C         0.44     0.31      0.58      10 absolute    0.87      0.78

# frequentist fit -- same shape, realisations from a parametric bootstrap
ecx(drc_fit, x_var = "x", ecx_val = 10)
#> # A tibble: 1 x 9
#> # Model:    drc
#> # Settings: resolution 1000 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing     1.88     1.62      2.19      10 absolute    9.91      8.92
```

And for `nsec()`:

```r
nsec(brms_model_1, x_var = "x")
#> # A tibble: 1 x 12
#> # Model:    brmsfit
#> # Settings: resolution 1000 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high sig_val anchor control reference ecnsec ecnsec.low ecnsec.high
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <dbl>     <dbl>  <dbl>      <dbl>       <dbl>
#> 1 nsec   decreasing     2.31     1.88      2.79    0.01 model     1.02     0.918   11.4       8.91        14.2
```

A grouped `nsec()` gains a `group` column and one row per level, exactly as the
grouped `ecx()` above.

### 3.2 Type stability

A function must not change its return **type** based on an argument value. This
means the `posterior` argument is removed (§3.5).

Arguments that change only values or row counts (`ecx_val`, `type`, `sig_val`,
`by_group`) stay as arguments. A descriptor column appearing or not is governed
by the column rule in §3.1, not by an argument flipping the return type.

### 3.3 Single-generic architecture

`toxval_predict()` is the only S3 generic. It dispatches on model class and
returns a `toxval_pred`. The metric functions (`ecx`, `nsec`, `nsec_multi`, ...)
are plain, non-generic functions that compute on a `toxval_pred`.

Metric functions accept **either a fitted model or a pre-built `toxval_pred`**
(one `inherits()` check at the top), which enables predict-once / compute-many
and makes the compute functions testable in isolation.

Adding a new model class is then one method and nothing else:

```r
toxval_predict.gam <- function(object, x_var, resolution, x_range, ...) {
  # 1. build the x grid
  # 2. generate realisations of the curve (see 3.4)
  # 3. return a toxval_pred
}
```

`toxval_pred` is **public API**, not an internal convenience: under #39 the
`bayesnec` classes are served by methods living in `bayesnec`, which constructs
a `toxval_pred` from outside this package.

### 3.4 The `toxval_pred` intermediate object

`toxval_pred` holds **realisations** of the fitted curve as a list, one element
per group or response, plus metadata.

A *realisation* is one plausible version of the fitted curve. A Bayesian fit
supplies thousands of them as posterior draws. A frequentist fit gets the same
thing from a parametric bootstrap: repeatedly draw a parameter set from the
model's own uncertainty and evaluate the curve at each. Every estimate is
computed once per realisation, and the spread across realisations gives the
confidence interval.

```
toxval_pred:
  curves    : named list of matrices, each [n_realisation x n_x]
  x_vec     : numeric [n_x]
  threshold : per-realisation threshold parameter (a NEC), or NULL
  control   : realisations of a control-only fit, or NULL
  meta      : source_class, x_var, group_var / multi_var, resolution, x_range,
              dimension, family, realisation source, n_realisation, ...
```

One representation covers every case:

| Case | `curves` list | realisation source | `n_realisation` |
|---|---|---|---|
| Ungrouped Bayesian | 1 element | draws | n_draws |
| `by_group` / `group_var` | one per factor level | draws | n_draws |
| Multivariate (`nsec_multi`) | one per response | draws | n_draws |
| `drc` | one per curve (or 1) | bootstrap | n_boot |
| `glm` / `gam` (future, #21) | one per group | bootstrap | n_boot |

**There is no second mode.** An earlier design treated the three columns of
`predict(drc_fit, interval = "confidence")` as three curves, which inverts a
pointwise confidence band on the response to get an interval on concentration.
That is not a valid interval, and it is worst where the curve is flat, which is
exactly where NSEC sits (#43).

The compute functions therefore never inspect the model class, and never branch.
They iterate the `curves` list and interpolate every realisation, then take
quantiles across realisations for the summary.

This changes `drc` numbers, needs a seed, and needs a vignette explaining how it
differs from `drc`'s own `ED()` intervals. `REFACTOR-claude.md` §3.4 has the
reasoning, the alignment invariant, and what model averaging requires.

### 3.5 Draws

There is **one function per metric**, and it always returns a `toxval` tibble.
The realisations are an optional list column, not a second function:

```r
pred <- toxval_predict(fit)
ecx(pred)                  # summary
ecx(pred, draws = TRUE)    # summary + a `draws` list column, one cell per row
```

Long form is one pipe stage rather than a separate API:

```r
ecx(fit, draws = TRUE) |> tidyr::unnest(draws)
#> # A tibble: 4,000 x 6
#>   metric direction  ecx_val estimate .draw  value
#>   <chr>  <chr>        <dbl>    <dbl> <int>  <dbl>
#> 1 ecx    decreasing      10    0.832     1  0.841
#> 2 ecx    decreasing      10    0.832     2  0.795
#> 3 ecx    decreasing      10    0.832     3  0.826
#> # i 3,997 more rows
```

The list column keeps one row per estimate, so unnesting is the caller's explicit
choice; it satisfies §3.2 without a second function; and it keeps paired
quantities such as NSEC and `ecnsec` aligned. `posterior` stays accepted for one
release behind a deprecation warning, since 1.0.0 is released.

### 3.6 Direction

`direction` is a **property of the result, not an argument.** For each curve the
estimator looks for the first decreasing crossing and the first increasing
crossing of the reference, and returns what it finds. If there is no crossing of
a given direction, that estimate is `NA`.

This replaces `hormesis_def`: a hormetic curve simply has both crossings, and
both are reported. `hormesis_def` cannot be repaired in place, because the two
`nsec` methods compute different quantities under it (`REFACTOR-claude.md` §3.6).
#1 and #8 close as a consequence, and #20 is answered directly.

**To pin down:** whether a curve with no crossing in one direction emits a row
with `NA` or omits the row.

### 3.7 Validation

`chk` is the default for all argument checks.

- Predict-level arguments (`x_var`, `group_var`, `x_range`, `resolution`, and
  that the model has a `toxval_predict` method) are validated in
  `toxval_predict()`.
- Metric arguments (`ecx_val`, `type`, `sig_val`, `prob_vals`, `xform`) are
  validated in the metric entry point, so every class gets identical checks.
  - This fixes the current bug where `ecx.bnecfit` never validates the `ecx_val`
    range while `ecx.brmsfit` does.
- Shared checks (`prob_vals` ordering, `xform` is a function, `resolution`) live
  in one internal `chk` helper, replacing the four hand-rolled copies.
- `stop()` calls are replaced.

`REFACTOR-claude.md` §3.7 lists the specific unguarded defects to fold in,
including `nsec.drc`'s positional `curveid` (#34) and the six differing
`resolution` defaults.

### 3.8 The `nsec` reference: the `anchor` argument

Where the `nsec` reference comes from becomes a visible choice rather than a
hard-coded rule:

```r
nsec(fit, anchor = c("model", "component", "control"))
```

`"model"` is the default: the threshold comes from the model-averaged control
posterior, so every pooled value is measured against the same response level.
`"component"` uses each component model's own control, which is the strict
Bayesian model average and reproduces what `bayesnec` returns today. `"control"`
uses a separate control-only fit, specified here but not implemented.

`anchor` is a column on the result, so the choice travels with the estimate. The
measurements behind the default, and the cost of choosing it, are in
`REFACTOR-claude.md` §3.8.

## 4. Phased implementation

Two constraints set the order.

**#19 before the spine.** `ecx.bnecfit` and `ecx.brmsfit` implement different
estimators. Phase 1 locks the current numbers; #19 decides which should move.
Answered afterwards, a refactor bug cannot be told apart from an intended change
of definition.

**`toxval` must reach CRAN before `bayesnec` can depend on it.** A CRAN package's
dependencies must resolve from CRAN, and `toxval` is not published. That puts the
change of output shape *before* the submission, since publishing an API about to
break would force two breaking releases in a row.

0. **Decide #19, #20 and #49, and agree the `anchor` default.** No code. Fixes
   what the numbers should be before anything locks them. Done when the answers
   are on the issues.
1. **Lock a regression net.** Capture current estimates as golden values, split
   into "must not move" and "expected to move, because X", so every later phase
   is checkable. Done when both lists exist and pass.
2. **Build the new spine alongside the old code.** `toxval_pred`,
   `toxval_predict()` and its methods, the shared `chk` validator, the
   class-agnostic compute functions, the parametric bootstrap. Purely additive.
   Done when it is tested and `ecx()` / `nsec()` still return today's values.
3. **`toxval` sheds `bayesnec`.** Reverses a dependency pointing the wrong way.
   Done when `bayesnec` is no longer imported and `ecx` on a `bnecfit` follows
   the #19 answer.
4. **Move metrics onto the spine one at a time** (`ecx`, then `nsec`, then
   `nsec_multi`), each gaining `draws`. Done when all three compute from a
   `toxval_pred`, with `posterior` deprecated but working.
5. **Swap outputs and clean up.** The `toxval` tibble and `tbl_sum()` printing;
   update the tests; remove `posterior` and the dead blocks last. Done when every
   metric returns a tibble and the tests assert the new shapes.
6. **`toxval` to CRAN**, carrying the final API. Unblocks #39. Done when
   published.
7. **`bayesnec`.** One PR: `Imports: toxval`, delete its copies of `ecx` and
   `nsec`, add the `toxval_predict` methods, adapt to the tibble. Then CRAN.

Phases 2 to 4 change who owns the code and what it computes; phase 5 changes what
it returns. Separating them keeps each checkable against the regression net, and
`bayesnec` sees one finished API at phase 7.

## 5. Notes

- **Method gaps to keep in mind:** `ecx` has no `drc` method, which becomes
  nearly free under §3.4 — whether it is in scope needs a decision. `nsec_multi`
  is brms-multivariate only.
- **`n(s)ec` and `nec` are reserved in the vocabulary but not implemented here**
  (#25, #18), so the vocabulary does not have to be reopened later.
- **`anchor = "control"` is specified but not implemented.** It needs a
  control-only fit, which cannot be recovered from a fitted model, so it is
  supplied as an argument — an argument for fitting wrappers (#22).
- **Model-averaged results are not reproducible today** (`bayesnec` #216): the
  component resampling is unseeded. Not fixed by this plan, and it blocks
  regression-testing model-averaged output by equality.
- **`xform` stays user-supplied, and applies to realisations before
  summarising.** Both are current behaviour; the second changes answers for any
  non-linear `xform`, so it belongs in the written contract.
