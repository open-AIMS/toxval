# Parameters common to multiple toxval functions

This is a documentation-only object whose parameter definitions are
shared across the package via `@inheritParams toxval_params`. It is not
intended to be called directly.

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

- hormesis_def:

  A character vector, taking values of "max" or "control". See Details.

- xform:

  A function to apply to the returned estimated concentration values.

- x_range:

  A range of x values over which to consider extracting the estimate.

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

- group_var:

  A character indicating the name of the grouping variable in object.

- by_group:

  A logical indicating if values should be returned for each level in
  group_var, or marginalised across all groups.

- horme:

  Logical indicating if hormesis is evident.

- ...:

  Additional arguments passed to class-specific methods.
