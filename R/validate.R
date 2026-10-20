#' Check the metric arguments shared by the metric functions
#'
#' Metric arguments are validated here, once, so that every model class gets
#' identical checks. Predict-level arguments are checked by `toxval_predict()`.
#'
#' @noRd
chk_type <- function(type) {
  chk::chk_string(type, x_name = "`type`")
  # EXPECTED-CHANGE relative type, once the asymptote is carried on toxval_pred
  chk::chk_subset(type, c("absolute", "range", "direct"), x_name = "`type`")
  invisible(type)
}

#' `ecx_val` is not capped at 100 under `absolute`: a response that goes
#' negative has more than complete inhibition by definition. Under `range` the
#' reference cannot pass the curve's own minimum or maximum, so it is capped.
#'
#' @noRd
chk_ecx_val <- function(ecx_val, type) {
  chk::chk_number(ecx_val, x_name = "`ecx_val`")
  if (type != "direct") {
    chk::chk_gt(ecx_val, 0, x_name = "`ecx_val`")
  }
  if (type == "range") {
    chk::chk_lte(ecx_val, 100, x_name = "`ecx_val`")
  }
  invisible(ecx_val)
}

#' `sig_val` sets the decreasing reference and `1 - sig_val` the increasing
#' one, so it must be below 0.5 for the two to lie either side of the control.
#'
#' @noRd
chk_sig_val <- function(sig_val) {
  chk::chk_number(sig_val, x_name = "`sig_val`")
  chk::chk_gt(sig_val, 0, x_name = "`sig_val`")
  chk::chk_lt(sig_val, 0.5, x_name = "`sig_val`")
  invisible(sig_val)
}

#' @noRd
chk_prob_vals <- function(prob_vals) {
  chk::chk_numeric(prob_vals, x_name = "`prob_vals`")
  chk::chk_length(prob_vals, 3L, x_name = "`prob_vals`")
  chk::chk_not_any_na(prob_vals, x_name = "`prob_vals`")
  chk::chk_range(prob_vals, c(0, 1), x_name = "`prob_vals`")
  if (prob_vals[2] > prob_vals[1] || prob_vals[1] > prob_vals[3]) {
    chk::abort_chk(
      "`prob_vals` must be the central, lower and upper quantiles, in that ",
      "order, not ",
      chk::cc(prob_vals, conj = " and "),
      "."
    )
  }
  invisible(prob_vals)
}

#' @noRd
chk_xform <- function(xform) {
  chk::chk_function(xform, x_name = "`xform`")
  invisible(xform)
}
