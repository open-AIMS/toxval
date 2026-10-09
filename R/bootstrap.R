#' Parametric bootstrap of the parameters of a frequentist fit
#'
#' Draws `n_boot` parameter vectors from `MVN(coef(object), vcov(object))`. The
#' draw is joint over every parameter in the fit, so for a fit with several
#' curves the replicates stay aligned across curves: replicate *i* of one curve
#' and replicate *i* of another come from the same parameter draw, which is the
#' alignment invariant `toxval_pred` carries.
#'
#' @param object A fitted model with `coef()` and `vcov()` methods.
#' @param n_boot Number of bootstrap replicates.
#' @param seed Seed for the draw, or `NULL` to use the current RNG state.
#'
#' @return A numeric matrix, `n_boot` rows by one column per parameter, with
#'   the parameter names as column names.
#'
#' @noRd
boot_parms <- function(object, n_boot, seed = NULL) {
  mu <- stats::coef(object)
  sigma <- stats::vcov(object)

  if (anyNA(mu)) {
    chk::abort_chk(
      "`coef(object)` must not contain missing values; the fit may not have ",
      "converged."
    )
  }
  if (anyNA(sigma)) {
    chk::abort_chk(
      "`vcov(object)` must not contain missing values; the fit may not be ",
      "identified."
    )
  }

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  parms <- MASS::mvrnorm(n = n_boot, mu = mu, Sigma = sigma)

  # mvrnorm() returns a named numeric vector for n = 1 instead of a 1-row matrix
  matrix(parms, nrow = n_boot, dimnames = list(NULL, names(mu)))
}
