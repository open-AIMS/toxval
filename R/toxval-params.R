#' Parameters common to multiple toxval functions
#'
#' This is a documentation-only object whose parameter definitions are shared
#' across the package via `@inheritParams toxval_params`. It is not
#' intended to be called directly.
#'
#' @param object A model fitted with brms, drc or bayesnec, or a [toxval_pred]
#'   object.
#' @param ... Additional arguments passed to [toxval_predict()], such as
#'   `n_boot` and `seed` for a `drc` fit, or `asymptote` for a `brmsfit`.
#' @param x_var A string naming the predictor variable in `object`, or `NULL`
#'   where the class of `object` records which of its variables is the
#'   predictor. Each method states whether it accepts `NULL`.
#' @param group_var A string naming the grouping variable in `object`, or `NULL`
#'   for a single curve.
#' @param resolution A whole number giving the number of predictor values to
#'   evaluate the curve at. Larger values estimate a metric more precisely.
#' @param x_range A numeric vector of length 2 giving the range of predictor
#'   values to evaluate over, or `NULL` to use the range in the model data. The
#'   first value must equal the control concentration.
#' @param x_control A number, the control concentration, at which the control
#'   response is predicted and the grid starts, or `NULL` for the lowest
#'   predictor value in the data. A value outside the range of the data warns.
#' @param draws A flag. If `TRUE`, a `draws` list column holds the
#'   per-realisation values of each row.
#' @param prob_vals A numeric vector of the central, lower and upper quantiles
#'   to summarise the realisations by, in that order.
#' @param xform A function applied to the per-realisation concentrations before
#'   they are summarised, such as `exp` for a model fitted on log
#'   concentration. It must be monotone over the evaluated range and may be
#'   decreasing.
#' @param posterior `r lifecycle::badge("defunct")` Supplying it is an
#'   error. Use `draws`.
#' @param hormesis_def,horme `r lifecycle::badge("deprecated")` Both
#'   directions are now always estimated, so these are ignored.
#' @param by_group `r lifecycle::badge("deprecated")` Supplying `group_var`
#'   gives one estimate per level.
#'
#' @name toxval_params
#' @keywords internal
NULL
