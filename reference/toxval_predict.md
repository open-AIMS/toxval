# Generate realisations of a fitted curve over a predictor grid

Evaluates a fitted concentration-response model over a grid of predictor
values, once per realisation of the fit. The result can be passed to
[`ecx()`](https://open-aims.github.io/toxval/reference/ecx.md) and
[`nsec()`](https://open-aims.github.io/toxval/reference/nsec.md) in
place of the fitted model.

## Usage

``` r
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
)

# Default S3 method
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
)

# S3 method for class 'brmsfit'
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  asymptote = NULL
)

# S3 method for class 'drc'
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  n_boot = 1000,
  seed = NULL
)

# S3 method for class 'bayesnecfit'
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
)

# S3 method for class 'bayesmanecfit'
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
)

# S3 method for class 'bnecfit'
toxval_predict(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
)
```

## Arguments

- object:

  A model fitted with brms, drc or bayesnec.

- ...:

  Additional arguments passed to methods, such as `n_boot` and `seed`
  for a `drc` fit, or `asymptote` for a `brmsfit`.

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

- asymptote:

  The lower and upper asymptotes of a `brmsfit`, for
  `type = "relative"`, as a vector named `decreasing`, `increasing` or
  both. Each is the name of a non-linear parameter, such as
  `c(decreasing = "bot", increasing = "top")`, or a number on the
  response scale.

- n_boot:

  A whole number giving the number of parametric bootstrap replicates to
  draw.

- seed:

  A whole number used to seed the bootstrap draw, or `NULL` to use the
  current random number state.

## Value

A
[toxval_pred](https://open-aims.github.io/toxval/reference/new_toxval_pred.md)
object.

## Details

Realisations are posterior draws for a Bayesian fit and parametric
bootstrap replicates for a frequentist one. A bootstrap replicate is
drawn from a multivariate normal with the coefficients as its mean and
their covariance.

Supply `group_var` to get one curve per level rather than a single
curve. The curves follow the order of the levels of a factor, and
otherwise their sorted order. Each method states what `group_var` must
name.

The grid starts at the control concentration, where the control response
is predicted, and every reference, ECx and NSEC is measured from that
response. By default the control concentration is the lowest predictor
value in the data. Use `x_control` to name another, such as 0 for a
predictor measured as the reduction from an ambient level. A value
outside the range of the data warns. A grouped fit has one control
concentration for every curve.

## Methods (by class)

- `toxval_predict(default)`: Method for any class without one, which
  errors.

- `toxval_predict(brmsfit)`: Method for a
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html).
  Realisations are the posterior draws, with group-level effects
  excluded. `x_var` is required. `group_var` must name a
  population-level effect; a variable that appears only in a group-level
  term, such as `site` in `y ~ x + (1 | site)`, is an error.

  A multivariate fit gives one curve per response, named as `brms` names
  them, for example `spsurvival` for a column `sp_survival`.

  A binomial response, such as `y | trials(n) ~ x`, is predicted at one
  trial, so `control` and `reference` are proportions. The NSEC and ECx
  do not depend on the number of trials.

  `type = "relative"` needs `asymptote`, and is not supported for a
  multivariate fit.

- `toxval_predict(drc)`: Method for a fit from
  [`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html). Realisations are
  parametric bootstrap replicates, so results vary between calls unless
  `seed` is supplied. A parameter that must be positive, such as `e` in
  [`LL.4()`](https://rdrr.io/pkg/drc/man/LL.4.html), is drawn on the log
  scale. `x_var` is read from the fit if not supplied. `group_var` must
  name the column passed as `curveid`. The asymptotes used by
  `type = "relative"` are the lower and upper limits of the fitted
  function.

- `toxval_predict(bayesnecfit)`: Method for a single fit from
  [`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html),
  of class `bayesnecfit`, which requires `bayesnec` to be installed.
  Realisations are the posterior draws, with group-level effects
  excluded. `x_var` is read from the `crf()` term if not supplied.
  `group_var` must be `NULL`. A predictor transformed inside `crf()`,
  such as `crf(log(x), ...)`, is reported in units of `x`, so
  `xform = exp` must not be supplied. A binomial or beta-binomial
  response is predicted at one trial, so its curve is a proportion. The
  asymptotes used by `type = "relative"` are the `bot` parameter, or 0
  for an equation without one, and 1 for a proportion. An asymptote is
  `NA` where the family has no such bound or the link is not identity.

- `toxval_predict(bayesmanecfit)`: Method for a model average from
  [`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html),
  of class `bayesmanecfit`, which requires `bayesnec` to be installed.
  Each model is predicted as for a `bayesnecfit`. Realisations are the
  draws `bayesnec` keeps from each model in proportion to its weight,
  the same draws its own estimates use, and each takes its asymptotes
  from its own model. The random number state of the session is left
  unchanged.

- `toxval_predict(bnecfit)`: Method for any other `bayesnec` fit, which
  errors. Group fits, joint refits and hurdle pairs each hold more than
  one curve, over concentrations that can differ between curves, which a
  [toxval_pred](https://open-aims.github.io/toxval/reference/new_toxval_pred.md)
  does not yet represent.

## Examples

``` r
# \donttest{
fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
toxval_predict(fit, seed = 42)
#> <toxval_pred> drc
#>   realisations: 1000 (bootstrap)
#>   grid:         conc, 500 values from 0 to 30
#>   curves:       1 (ungrouped)
#>   threshold:    none
#>   control:      none
if (requireNamespace("bayesnec", quietly = TRUE)) {
  toxval_predict(bayesnec::manec_example)
}
#> <toxval_pred> bayesmanecfit
#>   realisations: 100 (draws)
#>   grid:         x, 500 values from 0.03234801 to 3.22052
#>   curves:       1 (ungrouped)
#>   threshold:    none
#>   control:      none
# }
```
