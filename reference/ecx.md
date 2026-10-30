# Estimate ECx values from a fitted model

Estimates the concentration at which the fitted response changes by a
given proportion from the control, in each direction, for a fitted
concentration-response model.

## Usage

``` r
ecx(
  object,
  ...,
  proportion = 0.1,
  type = "absolute",
  reference = NULL,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  draws = FALSE,
  prob_vals = c(0.5, 0.025, 0.975),
  xform = identity,
  ecx_val = lifecycle::deprecated(),
  posterior = lifecycle::deprecated(),
  hormesis_def = lifecycle::deprecated(),
  horme = lifecycle::deprecated(),
  by_group = lifecycle::deprecated()
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

- proportion:

  A numeric vector of effects, as proportions of the span named by
  `type`; 0.1 is the EC10. Each value gives its own rows. Not supplied
  under `type = "direct"`.

- type:

  One of `"absolute"`, `"relative"`, `"range"` or `"direct"`. See
  Details.

- reference:

  A number, the response value whose crossing is sought under
  `type = "direct"`. Supplied only under `"direct"`.

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

- ecx_val:

  **\[deprecated\]** Use `proportion`, or `reference` under
  `type = "direct"`.

- posterior:

  **\[defunct\]** Supplying it is an error. Use `draws`.

- hormesis_def, horme:

  **\[deprecated\]** Both directions are now always estimated, so these
  are ignored.

- by_group:

  **\[deprecated\]** Supplying `group_var` gives one estimate per level.

## Value

A `toxval` tibble with one row per curve, proportion and direction, and
the columns:

- `metric`: `"ecx"`.

- `direction`: `"decreasing"` or `"increasing"`.

- `group`: the level of `group_var`, where it is supplied.

- `response`: the response of a multivariate fit.

- `estimate`, `conf.low`, `conf.high`: the ECx, as the `prob_vals`
  quantiles of the realisations, after `xform`.

- `proportion`, `type`: the arguments of the call. `proportion` is `NA`
  under `type = "direct"`.

- `control`: the predicted response at the control concentration, or
  `NA` under `type = "direct"`.

- `reference`: the response whose crossing is the ECx.

- `asymptote`: the asymptote the span is measured to, or `NA` unless
  `type` is `"relative"`.

- `draws`: where `draws = TRUE`, a tibble per row of `.draw` and
  `value.draw`, the ECx of each realisation, after `xform`. A
  realisation that does not reach the reference is `Inf`, or `-Inf`
  where `xform` is decreasing, and one excluded from the summary is
  `NA`.

`control`, `reference` and `asymptote` are on the response scale, as the
central `prob_vals` quantile of the realisations. The tibble prints the
model class and the number of realisations above the rows.

## Details

The model is evaluated over a grid of concentrations once per
realisation, by
[`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md):
once per posterior draw for a Bayesian fit, and once per parametric
bootstrap replicate for a frequentist one. The ECx is found for each
realisation and the realisations are summarised by `prob_vals`.

Each realisation is searched for both a decreasing and an increasing
crossing, and each direction is a row of the result. A direction in
which no realisation crosses is returned with `NA` estimates.

`type` names what `proportion` is taken of, from the control, the
predicted response at the control concentration, by default the lowest
concentration in the data (see
[`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md)):

- `"absolute"` (the default): decreasing, the control down to 0;
  increasing, the control up to 1 for a response on the proportion
  scale, and otherwise up to twice the control.

- `"relative"`: the control to the lower (decreasing) or upper
  (increasing) asymptote of the curve, a parameter of the fitted
  equation. It is available where the fit supplies the asymptote: from
  the limits of a `drc` fit, and for a `brmsfit` from the parameters
  named by `asymptote` (see
  [`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md)).
  An asymptote outside the observed responses is an extrapolation, and
  `relative` inherits its uncertainty. Before toxval 2.0.0 `"relative"`
  meant what is now `"range"`, so supplying it warns.

- `"range"`: the control to the lowest (decreasing) or highest
  (increasing) response the curve predicts.

- `"direct"`: no proportion; the ECx is where the curve crosses the
  response value supplied as `reference`.

A realisation that does not reach the reference within the evaluated
range ranks above the highest concentration, and an interval limit that
falls among such realisations is reported as the highest concentration,
with a warning. When any realisation is censored, the reported quantiles
are observed order statistics. Under `"range"` this includes a
realisation that never falls below, or rises above, its control. Under
`"absolute"` and `"relative"`, a realisation with nothing to take the
proportion of, such as a control at or below 0 or a missing asymptote,
is excluded from the summary, with a warning.

## Fits from bayesnec

`ecx()` also accepts a `bayesnecfit` or `bayesmanecfit` from
[`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html)
when `bayesnec` is installed. `bayesnec` has its own `ecx()`, and when
both packages are loaded with
[`library()`](https://rdrr.io/r/base/library.html), the one loaded last
is used. Call `toxval::ecx()` to make sure this function runs. The two
functions can give different estimates for the same fit, because they
evaluate the curve at different concentrations.

## Examples

``` r
# \donttest{
fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())

# EC10 and EC50, as proportions of the span from the control down to 0
ecx(fit, proportion = c(0.1, 0.5), seed = 42)
#> # A tibble: 4 × 10
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high proportion type     control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>      <dbl> <chr>      <dbl>
#> 1 ecx    decreasing     1.48     1.03      1.87        0.1 absolute    7.80
#> 2 ecx    increasing    NA       NA        NA           0.1 absolute    7.80
#> 3 ecx    decreasing     3.19     2.87      3.55        0.5 absolute    7.80
#> 4 ecx    increasing    NA       NA        NA           0.5 absolute    7.80
#> # ℹ 2 more variables: reference <dbl>, asymptote <dbl>

# EC10 of the span from the control to the lower asymptote
ecx(fit, type = "relative", seed = 42)
#> Warning: `type = "relative"` now measures from the control to the asymptote of the curve. Before toxval 2.0.0 it measured to the extent of the curve, which is now `type = "range"`; the two also differ for a non-monotonic curve, because `range` measures from the control rather than from the maximum.
#> Warning: Some realisations have no crossing within the evaluated range, so a reported quantile is at the highest concentration, or have no reference and were excluded. Consider a wider `x_range`.
#> * increasing: 100.0% excluded
#> # A tibble: 2 × 10
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high proportion type     control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>      <dbl> <chr>      <dbl>
#> 1 ecx    decreasing     1.45     1.01      1.82        0.1 relative    7.80
#> 2 ecx    increasing    NA       NA        NA           0.1 relative   NA   
#> # ℹ 2 more variables: reference <dbl>, asymptote <dbl>

# EC10 of the span from the control to the lowest predicted response
ecx(fit, type = "range", seed = 42)
#> # A tibble: 2 × 10
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high proportion type  control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>      <dbl> <chr>   <dbl>
#> 1 ecx    decreasing     1.45     1.00      1.82        0.1 range    7.80
#> 2 ecx    increasing    NA       NA        NA           0.1 range    7.80
#> # ℹ 2 more variables: reference <dbl>, asymptote <dbl>

# concentration at which the predicted root length falls to 4
ecx(fit, type = "direct", reference = 4, seed = 42)
#> # A tibble: 2 × 10
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high proportion type   control
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>      <dbl> <chr>    <dbl>
#> 1 ecx    decreasing     3.13     2.85      3.45         NA direct      NA
#> 2 ecx    increasing    NA       NA        NA            NA direct      NA
#> # ℹ 2 more variables: reference <dbl>, asymptote <dbl>

# per-realisation values of each row
res <- ecx(fit, draws = TRUE, seed = 42)
res$draws[[1]]
#> # A tibble: 1,000 × 2
#>    .draw value.draw
#>    <int>      <dbl>
#>  1     1       1.76
#>  2     2       1.51
#>  3     3       1.93
#>  4     4       1.27
#>  5     5       1.72
#>  6     6       1.59
#>  7     7       1.65
#>  8     8       1.63
#>  9     9       1.53
#> 10    10       1.46
#> # ℹ 990 more rows

# one estimate per herbicide
fit_group <- drc::drm(
  DryMatter ~ Dose,
  curveid = Herbicide,
  data = drc::S.alba,
  fct = drc::LL.4()
)
ecx(fit_group, group_var = "Herbicide", seed = 42)
#> # A tibble: 4 × 11
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  group   estimate conf.low conf.high proportion type  control
#>   <chr>  <chr>      <fct>      <dbl>    <dbl>     <dbl>      <dbl> <chr>   <dbl>
#> 1 ecx    decreasing Bentaz…     19.6     13.9      24.7        0.1 abso…    3.81
#> 2 ecx    increasing Bentaz…     NA       NA        NA          0.1 abso…    3.81
#> 3 ecx    decreasing Glypho…     30.1     12.2      41.1        0.1 abso…    3.88
#> 4 ecx    increasing Glypho…     NA       NA        NA          0.1 abso…    3.88
#> # ℹ 2 more variables: reference <dbl>, asymptote <dbl>
# }
```
