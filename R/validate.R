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

#' `proportion` is not capped at 1 under `absolute`: a response that goes
#' negative has more than complete inhibition by definition. Under `range` the
#' reference cannot pass the curve's own minimum or maximum, so it is capped.
#' Several values give one row each (T32).
#'
#' @noRd
chk_proportion <- function(proportion, type) {
  chk::chk_numeric(proportion, x_name = "`proportion`")
  if (!length(proportion)) {
    chk::abort_chk("`proportion` must have at least one value.")
  }
  chk::chk_not_any_na(proportion, x_name = "`proportion`")
  if (!all(is.finite(proportion))) {
    chk::abort_chk("`proportion` must be finite.")
  }
  chk::chk_gt(proportion, 0, x_name = "`proportion`")
  if (type == "range") {
    chk::chk_lte(proportion, 1, x_name = "`proportion`")
  }
  invisible(proportion)
}

#' `sig_val` sets the decreasing reference and `1 - sig_val` the increasing
#' one, so it must be below 0.5 for the two to lie either side of the control.
#' Values above the lower probability of the summary interval are allowed:
#' Fisher and Fox (2023, Table 3) report NSECs at 0.05, 0.10 and 0.20. The
#' fraction `sig_val` of realisations whose control lies at or below the
#' reference then sets the lower bound of the interval, which those authors
#' report as the lowest concentration (p. 2026).
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
