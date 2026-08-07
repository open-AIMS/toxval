# Extracts the predicted ECx value as desired from a supported class.

Extracts the predicted ECx value as desired from a supported class.

## Usage

``` r
ecx(
  object,
  ecx_val = 10,
  resolution = 1000,
  posterior = FALSE,
  type = "absolute",
  hormesis_def = "control",
  x_range = NA,
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  ...
)

# S3 method for class 'bnecfit'
ecx(
  object,
  ecx_val = 10,
  resolution = 100,
  posterior = FALSE,
  type = "absolute",
  hormesis_def = "control",
  x_range = NA,
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  ...
)

# S3 method for class 'brmsfit'
ecx(
  object,
  ecx_val = 10,
  resolution = 1000,
  posterior = FALSE,
  type = "absolute",
  hormesis_def = "control",
  x_range = NA,
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  x_var,
  group_var = NA,
  by_group = FALSE,
  horme = FALSE,
  ...
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

- ecx_val:

  The desired percentage effect value. This must be a value between 1
  and 99 (for type = "relative" and "absolute"), defaults to 10.

- resolution:

  The number of unique x values over which to find the estimate – large
  values will make the estimate more precise.

- posterior:

  A logical value indicating if the full posterior sample of calculated
  values should be returned instead of just the median and 95 credible
  intervals.

- type:

  A character vector, taking values of "relative", "absolute" (the
  default) or "direct". See Details.

- hormesis_def:

  A character vector, taking values of "max" or "control". See Details.

- x_range:

  A range of x values over which to consider extracting the estimate.

- xform:

  A function to apply to the returned estimated concentration values.

- prob_vals:

  A vector indicating the probability values over which to return the
  estimated value. Defaults to 0.5 (median) and 0.025 and 0.975 (95
  percent credible intervals).

- ...:

  Additional arguments passed to class-specific methods.

- x_var:

  A character indicating the name of the predictor (x) data in `object`.

- group_var:

  A character indicating the name of the grouping variable in `object`.

- by_group:

  A logical indicating if values should be returned for each level in
  `group_var`, or marginalised across all groups.

- horme:

  Logical indicating if hormesis is evident.

## Value

A vector containing the estimated ECx value, including upper and lower
95% credible interval bounds.

## Details

`type` "relative" is calculated as the percentage decrease from the
maximum predicted value of the response (top) to the minimum predicted
value of the response. Type "absolute" (the default) is calculated as
the percentage decrease from the maximum value of the response (top) to
0. Type "direct" provides a direct estimate of the x value for a given
y. Note that for the current version, ECx for an "nechorme" (NEC
Hormesis) model is estimated at a percent decline from the control.

For `hormesis_def`, if "max", then ECx values are calculated as a
decline from the maximum estimates (i.e. the peak at NEC); if "control",
then ECx values are calculated relative to the control, which is assumed
to be the lowest observed concentration.

Calls to functions `ecx()` and
[`nsec()`](https://open-aims.github.io/toxval/dev/reference/nsec.md) and
[`bayesnec::compare_fitted()`](https://open-aims.github.io/bayesnec/reference/compare_fitted.html)
do not require the same level of flexibility in the context of allowing
argument `newdata` (from a
[`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
perspective) to be supplied manually, as this is and should be handled
within the function itself. The argument `resolution` controls how
precisely the `ecx()` or
[`nsec()`](https://open-aims.github.io/toxval/dev/reference/nsec.md)
value is estimated, with argument `x_range` allowing estimation beyond
the existing range of the observed data (otherwise the default range)
which can be useful in a small number of cases. There is also no
reasonable case where estimating these from the raw data would be of
value, because both functions would simply return one of the treatment
concentrations, making NOEC a better metric in that case.

## Methods (by class)

- `ecx(bnecfit)`: Method for a `bayesnec` fit of class
  [bayesnec::bnecfit](https://open-aims.github.io/bayesnec/reference/bnecfit-class.html)
  returned by
  [`bayesnec::bnec()`](https://open-aims.github.io/bayesnec/reference/bnec.html).

- `ecx(brmsfit)`: Method for a raw `brms` fit of class
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
  returned by
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).
  Requires `x_var`, and supports estimates per group via `group_var` and
  `by_group`.

## Examples

``` r
# \donttest{
library(bayesnec)
#> Loading required package: brms
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.23.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: ‘brms’
#> The following object is masked from ‘package:stats’:
#> 
#>     ar
#> Loading required package: ggplot2
#> 
#> Attaching package: ‘bayesnec’
#> The following objects are masked from ‘package:toxval’:
#> 
#>     ecx, nsec
#> The following object is masked from ‘package:stats’:
#> 
#>     step

ecx(manec_example, ecx_val = 50)
#>      Q50     Q2.5    Q97.5 
#> 1.671113 1.610238 1.723770 
#> attr(,"resolution")
#> [1] 1000
#> attr(,"ecx_val")
#> [1] 50
#> attr(,"toxicity_estimate")
#> [1] "ecx"
ecx(manec_example)
#>       Q50      Q2.5     Q97.5 
#> 1.4939923 0.9284827 1.5642023 
#> attr(,"resolution")
#> [1] 1000
#> attr(,"ecx_val")
#> [1] 10
#> attr(,"toxicity_estimate")
#> [1] "ecx"
# }
```
