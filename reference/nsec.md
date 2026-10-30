# Estimate NSEC values from a fitted model

Estimates the No-Significant-Effect Concentration (NSEC), the
concentration at which the fitted response departs from the control by
more than its uncertainty allows, in each direction, for a fitted
concentration-response model.

## Usage

``` r
nsec(
  object,
  ...,
  sig_val = 0.01,
  type = "absolute",
  anchor = NULL,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  draws = FALSE,
  prob_vals = c(0.5, 0.025, 0.975),
  xform = identity,
  posterior = lifecycle::deprecated(),
  hormesis_def = lifecycle::deprecated(),
  horme = lifecycle::deprecated(),
  by_group = lifecycle::deprecated(),
  curveid = lifecycle::deprecated()
)
```

## Arguments

- object:

  A model fitted with brms, drc or bayesnec, or a
  [toxval_pred](https://open-aims.github.io/toxval/reference/new_toxval_pred.md)
  object.

- ...:

  Additional arguments passed to
  [`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md),
  such as `n_boot` and `seed` for a `drc` fit, or `asymptote` for a
  `brmsfit`.

- sig_val:

  A number strictly between 0 and 0.5, the quantile of the control
  realisations that sets the decreasing reference; `1 - sig_val` sets
  the increasing one.

- type:

  One of `"absolute"`, `"relative"` or `"range"`, naming the span
  `ecnsec` is a proportion of. See
  [`ecx()`](https://open-aims.github.io/toxval/reference/ecx.md).

- anchor:

  For a model-averaged fit, the control the reference is taken from:
  `"model"` uses the control of the averaged model, and `"component"`
  uses the control of each component model over every draw of its fit.
  The default is `"component"` for a `bayesmanecfit`, matching
  [`bayesnec::nsec()`](https://open-aims.github.io/bayesnec/reference/nsec.html),
  and `"model"` otherwise. Both give the same result for a single model.
  Under `"component"` the `reference` column reports the `prob_vals[1]`
  quantile of the component references across realisations, as `control`
  does.

- x_var:

  A string naming the predictor variable in `object`, or `NULL` where
  the class of `object` records which of its variables is the predictor.
  Each method states whether it accepts `NULL`.

- group_var:

  A string naming the grouping variable in `object`, or `NULL` for a
  single curve.

- resolution:

  A whole number giving the number of predictor values to evaluate the
  curve at. Larger values estimate a metric more precisely.

- x_range:

  A numeric vector of length 2 giving the range of predictor values to
  evaluate over, or `NULL` to use the range in the model data. The first
  value must equal the control concentration.

- x_control:

  A number, the control concentration, at which the control response is
  predicted and the grid starts, or `NULL` for the lowest predictor
  value in the data. A value outside the range of the data warns.

- draws:

  A flag. If `TRUE`, a `draws` list column holds the per-realisation
  values of each row.

- prob_vals:

  A numeric vector of the central, lower and upper quantiles to
  summarise the realisations by, in that order.

- xform:

  A function applied to the per-realisation concentrations before they
  are summarised, such as `exp` for a model fitted on log concentration.
  It must be monotone over the evaluated range and may be decreasing.

- posterior:

  **\[defunct\]** Supplying it is an error. Use `draws`.

- hormesis_def, horme:

  **\[deprecated\]** Both directions are now always estimated, so these
  are ignored.

- by_group:

  **\[deprecated\]** Supplying `group_var` gives one estimate per level.

- curveid:

  **\[deprecated\]** Use `group_var`.

## Value

A `toxval` tibble with one row per curve and direction, and the columns:

- `metric`: `"nsec"`.

- `direction`: `"decreasing"` or `"increasing"`.

- `group`: the level of `group_var`, where it is supplied.

- `response`: the response of a multivariate fit.

- `estimate`, `conf.low`, `conf.high`: the NSEC, as the `prob_vals`
  quantiles of the realisations, after `xform`.

- `sig_val`, `type`, `anchor`: the arguments of the call.

- `control`: the predicted response at the control concentration.

- `reference`: the `sig_val` (decreasing) or `1 - sig_val` (increasing)
  quantile of the control realisations, whose crossing is the NSEC.

- `asymptote`: the asymptote `ecnsec` is measured to, or `NA` unless
  `type` is `"relative"`.

- `ecnsec`, `ecnsec.low`, `ecnsec.high`: the effect at the NSEC, as a
  proportion of the span named by `type`, summarised by `prob_vals`.

- `draws`: where `draws = TRUE`, a tibble per row of `.draw`,
  `value.draw`, the NSEC of each realisation after `xform`, and
  `ecnsec.draw`. A realisation that does not reach the reference is
  `Inf` in `value.draw`, or `-Inf` where `xform` is decreasing, and one
  excluded from the summary is `NA`.

`control`, `reference` and `asymptote` are on the response scale, as the
central `prob_vals` quantile of the realisations. The tibble prints the
model class and the number of realisations above the rows.

## Details

The model is evaluated over a grid of concentrations once per
realisation, by
[`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md):
once per posterior draw for a Bayesian fit, and once per parametric
bootstrap replicate for a frequentist one.

The reference for the decreasing direction is the `sig_val` quantile of
the control realisations, the predicted response at the control
concentration (by default the lowest concentration in the data; see
[`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md)),
and for the increasing direction the `1 - sig_val` quantile. The NSEC is
found for each realisation against that reference, and the realisations
are summarised by `prob_vals`. A direction in which no realisation
crosses is returned with `NA` estimates.

By construction, about `sig_val` of the realisations have a control
already past the reference. These are placed at the control
concentration, unless they show no effect in the tested range. A
realisation that does not reach the reference within the evaluated range
ranks above the highest concentration, and an interval limit that falls
among such realisations is reported as the highest concentration, with a
warning. When any realisation is censored, the reported quantiles are
observed order statistics.

`ecnsec` is the effect at the NSEC, as a proportion of the span named by
`type`: the inverse of the
[`ecx()`](https://open-aims.github.io/toxval/reference/ecx.md) reference
under the same `type`.

NSECs are not estimated for `drc` NEC models, whose threshold is a
parameter of the model rather than a feature of a smooth curve.

## Fits from bayesnec

`nsec()` also accepts a `bayesnecfit` or `bayesmanecfit` from
[`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html)
when `bayesnec` is installed. `bayesnec` has its own `nsec()`, and when
both packages are loaded with
[`library()`](https://rdrr.io/r/base/library.html), the one loaded last
is used. Call `toxval::nsec()` to make sure this function runs.

The two functions can give different estimates for the same fit, because
they evaluate the curve at different concentrations and handle
realisations that never reach the reference differently.

## Multivariate fits

A multivariate fit gives rows for each response, named in the `response`
column. This replaces
[`nsec_multi()`](https://open-aims.github.io/toxval/reference/nsec_multi.md),
whose help page shows how to convert existing calls.

## Examples

``` r
# \donttest{
fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
nsec(fit, seed = 42)
#> # A tibble: 2 × 14
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high sig_val type     anchor control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
#> 1 nsec   decreasing     1.18    0.637      1.52    0.01 absolute model     7.80
#> 2 nsec   increasing    NA      NA         NA       0.01 absolute model     7.80
#> # ℹ 5 more variables: reference <dbl>, asymptote <dbl>, ecnsec <dbl>,
#> #   ecnsec.low <dbl>, ecnsec.high <dbl>

nsec(fit, sig_val = 0.05, seed = 42)
#> # A tibble: 2 × 14
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high sig_val type     anchor control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
#> 1 nsec   decreasing     1.03        0      1.41    0.05 absolute model     7.80
#> 2 nsec   increasing    NA          NA     NA       0.05 absolute model     7.80
#> # ℹ 5 more variables: reference <dbl>, asymptote <dbl>, ecnsec <dbl>,
#> #   ecnsec.low <dbl>, ecnsec.high <dbl>

nsec(fit, type = "range", seed = 42)
#> # A tibble: 2 × 14
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high sig_val type  anchor control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr> <chr>    <dbl>
#> 1 nsec   decreasing     1.18    0.637      1.52    0.01 range model     7.80
#> 2 nsec   increasing    NA      NA         NA       0.01 range model     7.80
#> # ℹ 5 more variables: reference <dbl>, asymptote <dbl>, ecnsec <dbl>,
#> #   ecnsec.low <dbl>, ecnsec.high <dbl>

# 80% interval
nsec(fit, prob_vals = c(0.5, 0.1, 0.9), seed = 42)
#> # A tibble: 2 × 14
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high sig_val type     anchor control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>
#> 1 nsec   decreasing     1.18    0.881      1.40    0.01 absolute model     7.80
#> 2 nsec   increasing    NA      NA         NA       0.01 absolute model     7.80
#> # ℹ 5 more variables: reference <dbl>, asymptote <dbl>, ecnsec <dbl>,
#> #   ecnsec.low <dbl>, ecnsec.high <dbl>

res <- nsec(fit, draws = TRUE, seed = 42)
res$draws[[1]]
#> # A tibble: 1,000 × 3
#>    .draw value.draw ecnsec.draw
#>    <int>      <dbl>       <dbl>
#>  1     1      1.42      0.0489 
#>  2     2      1.40      0.0818 
#>  3     3      1.70      0.0635 
#>  4     4      1.03      0.0642 
#>  5     5      0        -0.00139
#>  6     6      1.23      0.0509 
#>  7     7      1.19      0.0308 
#>  8     8      1.36      0.0590 
#>  9     9      1.26      0.0549 
#> 10    10      0.530     0.00787
#> # ℹ 990 more rows

fit_group <- drc::drm(
  DryMatter ~ Dose,
  curveid = Herbicide,
  data = drc::S.alba,
  fct = drc::LL.4()
)
nsec(fit_group, group_var = "Herbicide", seed = 42)
#> # A tibble: 4 × 15
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  group      estimate conf.low conf.high sig_val type   anchor
#>   <chr>  <chr>      <fct>         <dbl>    <dbl>     <dbl>   <dbl> <chr>  <chr> 
#> 1 nsec   decreasing Bentazone      17.8    11.2       21.9    0.01 absol… model 
#> 2 nsec   increasing Bentazone      NA      NA         NA      0.01 absol… model 
#> 3 nsec   decreasing Glyphosate     24.4     7.60      34.6    0.01 absol… model 
#> 4 nsec   increasing Glyphosate     NA      NA         NA      0.01 absol… model 
#> # ℹ 6 more variables: control <dbl>, reference <dbl>, asymptote <dbl>,
#> #   ecnsec <dbl>, ecnsec.low <dbl>, ecnsec.high <dbl>
# }
```
