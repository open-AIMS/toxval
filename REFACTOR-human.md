# toxval Refactor Plan

## 1. Motivation

The refactor has three goals:

1. Make adding a **new model class** easy.
2. Fix the current duplication, inconsistencies, and bugs.
3. Standardise all return structures.

## 2. Scope

**In scope.** Restructure current functionality onto the new architecture; 

- standardise outputs to a `toxval` tibble 
- split the `posterior` argument into separate `*_draws()` functions
- consolidate argument validation on `chk
- remove duplication, dead code, and fix the known inconsistencies

## 3. Design decisions

### 3.1 Output contract: everything is a `toxval` tibble

Every metric function (`ecx()`, `nsec()`, etc) returns the same tidy container, 
a `tbl_df` subclass named `toxval`, with an identical shape across all model 
classes. Model class never changes the return type.

Core columns: 

- `metric`
- `group` (present only when grouped)
- `estimate`
- `conf.low` 
- `conf.high`

When groups are present the `by_group` becomes "more rows".

Secondary derived quantities are added as **extra columns**, if the value isn't 
present for that type or option it gets an NA instead of disappearing:

- `ecx` adds `control`, `reference`.
- `nsec` adds `reference`, `ecnsec`, `ecnsec.low`, `ecnsec.high`.

Run settings (`resolution`, `ecx_val` / `sig_val`, `type`, `mode`, model class)
are displayed as tibble header lines via a `tbl_sum()` method (think
of the `# Groups:` on a grouped tibble). They are also stored in
attributes for programmatic access. 

For some examples of `ecx()`

```r
# basic output and all columns have values
ecx(manec_example, ecx_val = 10)
#> # A tibble: 1 x 6
#> # Metric:   EC10 (absolute)
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | mode draws
#>   metric estimate conf.low conf.high control reference
#>   <chr>     <dbl>    <dbl>     <dbl>   <dbl>     <dbl>
#> 1 ec10      0.832    0.818      1.05    1.02     0.918

# control is NA but still present 
ecx(manec_example, type = "direct", ecx_val = 0.5)
#> # A tibble: 1 x 6
#> # Metric:   EC (direct, y = 0.5)
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | mode draws
#>   metric estimate conf.low conf.high control reference
#>   <chr>     <dbl>    <dbl>     <dbl>   <dbl>     <dbl>
#> 1 ecx        1.13     0.98      1.31      NA      0.5

# grouped output example
ecx(brms_grouped, x_var = "x", group_var = "site", by_group = TRUE, ecx_val = 10)
#> # A tibble: 3 x 7
#> # Metric:   EC10 (absolute)
#> # Model:    brmsfit
#> # Settings: resolution 1000 | mode draws
#> # Groups:   site [3]
#>   metric group estimate conf.low conf.high control reference
#>   <chr>  <chr>    <dbl>    <dbl>     <dbl>   <dbl>     <dbl>
#> 1 ec10   A         0.71     0.55      0.88    0.98     0.88
#> 2 ec10   B         1.02     0.83      1.24    1.10     0.99
#> 3 ec10   C         0.44     0.31      0.58    0.87     0.78
```

Examples for `nsec()` outputs


```r
# basic output will all columns filled out
nsec(brms_model_1, x_var = "x")
#> # A tibble: 1 x 4
#> # Metric:   NSEC (sig_val = 0.01)
#> # Model:    brmsfit
#> # Settings: resolution 1000 | mode draws
#>   metric estimate conf.low conf.high
#>   <chr>     <dbl>    <dbl>     <dbl>
#> 1 nsec       2.31     1.88      2.79

# grouped output
nsec(brms_grouped, x_var = "x", group_var = "site", by_group = TRUE)
#> # A tibble: 3 x 5
#> # Metric:   NSEC (sig_val = 0.01)
#> # Model:    brmsfit
#> # Settings: resolution 1000 | mode draws
#> # Groups:   site [3]
#>   metric group estimate conf.low conf.high
#>   <chr>  <chr>    <dbl>    <dbl>     <dbl>
#> 1 nsec   A         0.71     0.55      0.88
#> 2 nsec   B         1.02     0.83      1.24
#> 3 nsec   C         0.44     0.31      0.58
```

### 3.2 Type stability

A function must not change its return **type** based on an argument value.
This means that the `posterior` argument needs to be removed (section 3.5). 

Arguments that change only values or row counts (`ecx_val`, `type`, `sig_val`,
`hormesis_def`, `by_group`) stay as arguments.

### 3.3 Single-generic architecture

`toxval_predict()` is the only S3 generic. It dispatches on model class and
returns a `toxval_pred`. The metric functions (`ecx`, `nsec`, `ecx_draws`,
`nsec_draws`, ...) are plain, non-generic functions that compute on a
`toxval_pred`. 

Metric functions accept **either a fitted model or a pre-built `toxval_pred`**
(one `inherits()` check at the top), which enables predict-once / compute-many
and makes the compute functions testable in isolation.

Adding a new model class is then one method and nothing else:

```r
toxval_predict.gam <- function(object, x_var, resolution, x_range, ...) {
  # 1. build the x grid
  # 2. call the class's own predictor (e.g. predict(object, se.fit = TRUE))
  # 3. return a toxval_pred (mode = "interval" for a frequentist fit)
}
```

### 3.4 The `toxval_pred` intermediate object

`toxval_pred` holds predictions as a **list**, one element per
group or response (a single element when ungrouped), plus a `mode` flag and
metadata:

```
toxval_pred:
  curves : named list of matrices, each [n_realisation x n_x]
  x_vec  : numeric [n_x]
  mode   : "draws" | "interval"
  meta   : source_class, x_var, group_var / multi_var, resolution,
           x_range, dimension ("none" | "group" | "response"), hormesis, ...
```

One representation covers every case the code handles today:

| Case | `curves` list | `mode` | `n_realisation` |
|---|---|---|---|
| Ungrouped Bayesian | 1 element | `draws` | n_draws |
| `by_group` / `group_var` | one per factor level | `draws` | n_draws |
| Multivariate (`nsec_multi`) | one per response | `draws` | n_draws |
| `drc` | one per curve (or 1) | `interval` | 3 (point / lower / upper) |

The compute functions never inspect the model class. They iterate the `curves`
list and branch exactly once, on `mode`:

- `mode = "draws"`: interpolate every realisation, then quantile across them for
  the summary; `*_draws()` returns them raw.
- `mode = "interval"`: interpolate the three curves, which are the estimate /
  lower / upper directly; `*_draws()` errors (no posterior for a frequentist
  fit).

### 3.5 Posterior / draws split (option a)

The summary and the draws are separate functions, each with one stable return
type:

```r
pred <- toxval_predict(fit)
ecx(pred)          # summary tibble
ecx_draws(pred)    # draws tibble (same predictions, no recompute)
```

`ecx_draws()` / `nsec_draws()` are only meaningful for `mode = "draws"` fits;
calling them on a frequentist fit raises a clear error. 

```
# summary  (what posterior = FALSE used to give)
ecx(manec_example, ecx_val = 10)
#> # A tibble: 1 x 6
#> # Metric:   EC10 (absolute)
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | mode draws
#>   metric estimate conf.low conf.high control reference
#>   <chr>     <dbl>    <dbl>     <dbl>   <dbl>     <dbl>
#> 1 ec10      0.832    0.818      1.05    1.02     0.918

# draws  (what posterior = TRUE used to give)
ecx_draws(manec_example, ecx_val = 10)
#> # A tibble: 400 x 3
#>   metric .draw  value
#>   <chr>  <int>  <dbl>
#> 1 ec10       1  0.841
#> 2 ec10       2  0.795
#> 3 ec10       3  0.826
#> # i 397 more rows
```

### 3.6 Validation

`chk` is the default for all argument checks.

- Predict-level arguments (`x_var`, `group_var`, `x_range`, `resolution`, and
  that the model has a `toxval_predict` method) are validated in
  `toxval_predict()`.
- Metric arguments (`ecx_val`, `type`, `hormesis_def` / `sig_val`, `prob_vals`,
  `xform`) are validated in the metric entry point, so every class gets
  identical checks. 
  - This fixes the current bug where `ecx.bnecfit` never validates the `ecx_val` 
    range while `ecx.brmsfit` does.
- Shared checks (`prob_vals` ordering, `xform` is a function, `resolution`) live
  in one internal `chk` helper, replacing the four hand-rolled copies.
- `stop()` calls are replaced. 

## 4. Phased implementation

1. **Lock a regression net.** Capture the current numeric estimates as golden
   values before touching code, so it can be shown the numbers do not move.
2. **Build the new spine alongside the old code.** `toxval_pred`,
   `toxval_predict()` and its methods, the shared `chk` validator, the
   class-agnostic compute functions, and the summariser / `toxval` tibble.
3. **Move metrics onto the spine one at a time** (`ecx`, then `nsec`, then
   `nsec_multi`), each with its `*_draws()`.
4. **Swap outputs and clean up.** Switch to the `toxval` tibble and `tbl_sum()`
   printing, update the tests to the new shapes, and remove the old code and the
   dead / commented blocks last.

## 5. Notes

- Known method gaps to keep in mind while restructuring: `ecx` has no `drc`
  method; `nsec.drc` has no posterior (correct: frequentist); `nsec_multi` is
  brms-multivariate only.

