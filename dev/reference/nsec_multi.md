# Extracts the predicted NSEC values from a multivariate brmsfit.

Extracts the predicted NSEC values from a multivariate brmsfit.

## Usage

``` r
nsec_multi(
  object,
  sig_val = 0.01,
  resolution = 50,
  x_range = NA,
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  posterior = FALSE,
  x_var,
  trials_var = NA,
  multi_var = NA,
  type = "both",
  criterion = 0.8,
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

- x_var:

  A character indicating the name of the predictor (x) data in object.

- trials_var:

  NA if no trials, or a character vector indicating the name or prefix
  of any trials column(s).

- multi_var:

  NA if univariate, or a character vector indicating the name or prefix
  of the multivariate column(s).

- type:

  The type of nsec to be returned. See details.

- criterion:

  The criterion to use when type ='lowest'.

- ...:

  Additional arguments passed to class-specific methods.

## Value

A vector or list containing the estimated NSEC value(s).

## Details

nsecID extracts nsec values from response curves of unknown direction or
shape. Both increasing and/or decreasing nsec's can be returned. The
returned output depends on the selected type, which can be one of
'both', 'lower', 'increasing', 'decreasing'.
