#' Parameters common to multiple toxval functions
#'
#' This is a documentation-only object whose parameter definitions are shared
#' across the package via `@inheritParams toxval_params`. It is not
#' intended to be called directly.
#'
#' @name toxval_params
#' @keywords internal
#'
#' @param object An object of class [bayesnec::bayesnecfit] or
#' [bayesnec::bayesmanecfit] returned by [bayesnec::bnec()].
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values.
#' against the lowest observed concentration (assumed to be the control), to
#' estimate NEC as an interpolated NOEC value from smooth ECx curves.
#' @param resolution The number of unique x values over which to find the
#' estimate -- large values will make the estimate more precise.
#' @param hormesis_def A character vector, taking values
#' of "max" or "control". See Details.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting the
#' estimate.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param posterior A logical value indicating if the full
#' posterior sample of calculated values should be returned instead of
#' just the median and 95 credible intervals.
#' @param x_var A character indicating the name of the predictor (x) data in
#' object.
#' @param group_var A character indicating the name of the grouping variable in
#' object.
#' @param by_group A logical indicating if values should be returned for
#' each level in group_var, or marginalised across all groups.
#' @param horme Logical indicating if hormesis is evident.
#' @param ... Additional arguments passed to class-specific methods.
NULL
