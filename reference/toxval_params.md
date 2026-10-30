# Parameters common to multiple toxval functions

This is a documentation-only object whose parameter definitions are
shared across the package via `@inheritParams toxval_params`. It is not
intended to be called directly.

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
