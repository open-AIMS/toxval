# Helpers for the toxval_predict() methods for bayesnec fits, which are in
# toxval-predict.R so that roxygen reads them after the generic they document.

#' The `bayesnec` classes with a `toxval_predict()` method
#'
#' @noRd
bnec_supported_classes <- function() {
  "bayesnecfit"
}

#' Whether `bayesnec` is installed
#'
#' A function of its own so that a test can replace it with
#' `testthat::local_mocked_bindings()`.
#'
#' @noRd
bayesnec_available <- function() {
  requireNamespace("bayesnec", quietly = TRUE)
}

#' Check that `bayesnec` is installed
#'
#' The grid and the predictor are read through `bayesnec`, so a `bnecfit`
#' cannot be predicted without it, although the stored `brmsfit` could be.
#'
#' @param object A `bnecfit`.
#'
#' @return `object`, invisibly.
#'
#' @noRd
check_bayesnec_installed <- function(object) {
  if (!bayesnec_available()) {
    chk::abort_chk(
      "`bayesnec` must be installed to compute on a ",
      chk::cc(class(object)[1]),
      "."
    )
  }
  invisible(object)
}

#' Refuse the `bayesnecfit` objects whose curve is not one expected value
#'
#' @param object A `bayesnecfit`.
#'
#' @return `object`, invisibly.
#'
#' @noRd
chk_bayesnec_supported <- function(object) {
  # One level of a joint refit inherits `bayesnecfit`, so it dispatches to this
  # method, but its brmsfit is fitted to every level's data
  if (inherits(object, "bayesnecjointlevel")) {
    chk::abort_chk(
      "`object` must not be one level of a `bnec_joint()` refit; joint refits ",
      "are not yet supported by `toxval_predict()`."
    )
  }
  family <- object$fit$family$family
  # These families fit the zeros in a second part, and which part, or their
  # combination, a metric should be computed on is not yet decided
  if (family %in% bnec_two_part_families()) {
    chk::abort_chk(
      "`object` must not be fitted with the family ",
      chk::cc(family),
      ", which models the zeros in a separate part; such fits are not yet ",
      "supported by `toxval_predict()`."
    )
  }
  # The count part of a `bnec_hurdle()` fit is truncated at zero, and `brms`
  # gives its expected value as a sum over a finite grid, which `bayesnec`
  # replaces with the closed form
  truncated <- isTRUE(
    attr(object$bayesnecformula, "bayesnec_internal_truncation")
  )
  if (truncated && family %in% c("poisson", "negbinomial")) {
    chk::abort_chk(
      "`object` must not be the zero-truncated count part of a ",
      "`bnec_hurdle()` fit; such fits are not yet supported by ",
      "`toxval_predict()`."
    )
  }
  invisible(object)
}

#' The `bayesnec` families that model the zeros in a second part
#'
#' @noRd
bnec_two_part_families <- function() {
  c(
    "hurdle_gamma",
    "hurdle_poisson",
    "hurdle_negbinomial",
    "zero_inflated_beta"
  )
}

#' Check that no `group_var` was given for a fit with one curve
#'
#' @param group_var The supplied `group_var`.
#' @param object The fitted model, for the message.
#'
#' @return `group_var`, invisibly.
#'
#' @noRd
chk_no_group_var <- function(group_var, object) {
  if (!is.null(group_var)) {
    chk::abort_chk(
      "`group_var` must be NULL for a ",
      chk::cc(class(object)[1]),
      ", which has one curve; its group-level effects are marginalised."
    )
  }
  invisible(group_var)
}

#' The predictor of a `bnec()` fit
#'
#' `bayesnec` records the variable named inside `crf()` on the model frame of
#' its formula, as the `x_var` entry of the `bnec_pop` attribute. For
#' `crf(log(x), ...)` that is `x`, which is also the column the grid is built
#' over.
#'
#' @param formula The `bayesnecformula` of the fit.
#' @param data The data the fit was made to.
#' @param x_var The supplied `x_var`, or `NULL`.
#'
#' @return A string.
#'
#' @noRd
bnec_x_var <- function(formula, data, x_var) {
  # model.frame() dispatches to bayesnec's method for a bayesnecformula
  pop <- attr(stats::model.frame(formula, data = data), "bnec_pop")
  found <- unname(pop[names(pop) == "x_var"])
  if (length(found) != 1) {
    chk::abort_chk(
      "The `crf()` term of `object` must name one predictor variable, not ",
      length(found),
      "."
    )
  }
  if (is.null(x_var)) {
    return(found)
  }
  if (x_var != found) {
    chk::abort_chk(
      "`x_var` must be ",
      chk::cc(found),
      ", the predictor in the `crf()` term of `object`, not ",
      chk::cc(x_var),
      "."
    )
  }
  x_var
}

#' The grid `bayesnec` predicts a fit over
#'
#' `bnec_newdata()` sets the trials of a binomial or beta-binomial response to
#' 1 and the denominator of a `rate()` term to 1. Building the grid here instead
#' would mean copying those rules, which have changed between `bayesnec`
#' releases.
#'
#' @param object A `bnecfit`.
#' @param resolution Number of grid values.
#' @param x_range The requested range, or `NULL` for the range of the data.
#'
#' @return A data frame with one row per grid value.
#'
#' @noRd
bnec_grid <- function(object, resolution, x_range) {
  bayesnec::bnec_newdata(
    object,
    resolution = resolution,
    x_range = if (is.null(x_range)) NA else x_range
  )
}
