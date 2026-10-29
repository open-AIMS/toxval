#' Check `type`
#'
#' Metric arguments are checked here so every model class gets the same checks;
#' predict-level arguments are checked by `toxval_predict()`.
#'
#' @noRd
chk_type <- function(type) {
  chk::chk_string(type, x_name = "`type`")
  chk::chk_subset(
    type,
    c("absolute", "relative", "range", "direct"),
    x_name = "`type`"
  )
  invisible(type)
}

#' Check `proportion`
#'
#' Capped at 1 under `range` and `relative`, where the reference cannot pass the
#' curve's extent or asymptote, but not under `absolute`. Several values give
#' one row each.
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
  if (type %in% c("range", "relative")) {
    chk::chk_lte(proportion, 1, x_name = "`proportion`")
  }
  invisible(proportion)
}

#' Check `anchor`
#'
#' @noRd
chk_anchor <- function(anchor) {
  chk::chk_string(anchor, x_name = "`anchor`")
  chk::chk_subset(anchor, c("model", "component"), x_name = "`anchor`")
  invisible(anchor)
}

#' Check `sig_val`
#'
#' `sig_val` must lie in (0, 0.5) so that the decreasing (`sig_val`) and
#' increasing (`1 - sig_val`) references lie either side of the median control.
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
