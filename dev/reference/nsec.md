# Extracts the predicted NSEC value as desired from a supported class.

Extracts the predicted NSEC value as desired from a supported class.

## Usage

``` r
nsec(
  object,
  sig_val = 0.01,
  resolution = 100,
  x_range = NA,
  hormesis_def = "control",
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  posterior = FALSE,
  ...
)

# S3 method for class 'bnecfit'
nsec(
  object,
  sig_val = 0.01,
  resolution = 100,
  x_range = NA,
  hormesis_def = "control",
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  posterior = FALSE,
  type = "absolute",
  ...
)

# S3 method for class 'brmsfit'
nsec(
  object,
  sig_val = 0.01,
  resolution = 1000,
  x_range = NA,
  hormesis_def = "control",
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  posterior = FALSE,
  x_var,
  group_var = NA,
  by_group = FALSE,
  horme = FALSE,
  ...
)

# S3 method for class 'drc'
nsec(
  object,
  sig_val = 0.01,
  resolution = 1000,
  x_range = NA,
  hormesis_def = "control",
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  ...,
  x_var,
  horme = FALSE,
  curveid = NA
)
```

## Arguments

- object:

  An object of class
  [bayesnec::bayesnecfit](https://open-aims.github.io/bayesnec/reference/bayesnecfit-class.html)
  or
  [bayesnec::bayesmanecfit](https://open-aims.github.io/bayesnec/reference/bayesmanecfit-class.html)
  returned by
  [`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html).

- sig_val:

  Probability value to use as the lower quantile to test significance of
  the predicted posterior values. against the lowest observed
  concentration (assumed to be the control), to estimate NEC as an
  interpolated NOEC value from smooth ECx curves.

- resolution:

  The number of unique x values over which to find the estimate – large
  values will make the estimate more precise.

- x_range:

  A range of x values over which to consider extracting the estimate.

- hormesis_def:

  A character vector, taking values of "max" or "control". See Details.

- xform:

  A function to apply to the returned estimated concentration values.

- prob_vals:

  A vector indicating the probability values over which to return the
  estimated value. Defaults to 0.5 (median) and 0.025 and 0.975 (95
  percent credible intervals).

- posterior:

  A logical value indicating if the full posterior sample of calculated
  values should be returned instead of just the median and 95 credible
  intervals.

- ...:

  Additional arguments passed to class-specific methods.

- type:

  One of "relative" or "absolute" (the default). For "relative" the NSEC
  reference is taken relative to the minimum predicted response; for
  "absolute" it is taken relative to zero.

- x_var:

  A character indicating the name of the predictor (x) data in `object`.

- group_var:

  A character indicating the name of the grouping variable in `object`.

- by_group:

  A logical indicating if values should be returned for each level in
  `group_var`, or marginalised across all groups.

- horme:

  Logical indicating if hormesis is evident.

- curveid:

  A character indicating the name of the grouping variable in `object`.

## Value

A vector containing the estimated NSEC value, including upper and lower
95% credible interval bounds.

## Details

For `hormesis_def`, if "max", then NSEC values are calculated as a
decline from the maximum estimates (i.e. the peak at NEC); if "control",
then NSEC values are calculated relative to the control, which is
assumed to be the lowest observed concentration.

Calls to functions
[`ecx()`](https://open-aims.github.io/toxval/dev/reference/ecx.md) and
`nsec()` and
[`bayesnec::compare_fitted()`](https://open-aims.github.io/bayesnec/reference/compare_fitted.html)
do not require the same level of flexibility in the context of allowing
argument `newdata` (from a
[`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
perspective) to be supplied manually, as this is and should be handled
within the function itself. The argument `resolution` controls how
precisely the
[`ecx()`](https://open-aims.github.io/toxval/dev/reference/ecx.md) or
`nsec()` value is estimated, with argument `x_range` allowing estimation
beyond the existing range of the observed data (otherwise the default
range) which can be useful in a small number of cases. There is also no
reasonable case where estimating these from the raw data would be of
value, because both functions would simply return one of the treatment
concentrations, making NOEC a better metric in that case.

## Methods (by class)

- `nsec(bnecfit)`: Method for a `bayesnec` fit of class
  [bayesnec::bnecfit](https://open-aims.github.io/bayesnec/reference/bnecfit-class.html)
  returned by
  [`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html).

- `nsec(brmsfit)`: Method for a raw `brms` fit of class
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
  returned by
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).
  Requires `x_var`, and supports estimates per group via `group_var` and
  `by_group`.

- `nsec(drc)`: Method for a `drc` fit (class `drc`) returned by
  [`drc::drm()`](https://rdrr.io/pkg/drc/man/drm.html). Supports
  estimates per group via `curveid`. Hormesis is not currently
  implemented.

## Examples

``` r
# \donttest{
library(bayesnec)

data(manec_example)
nsec(manec_example)
#>       Q50      Q2.5     Q97.5 
#> 1.4716527 0.7485697 1.5436978 
#> attr(,"resolution")
#> [1] 1000
#> attr(,"sig_val")
#> [1] 0.01
#> attr(,"toxicity_estimate")
#> [1] "nsec"
#> attr(,"ecnsec_relativeP")
#>       50%      2.5%     97.5% 
#> 2.2511298 0.4977516 4.2787129 
# }
```
