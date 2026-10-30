#' Parametric bootstrap of the parameters of a frequentist fit
#'
#' Draws `n_boot` parameter vectors from `MVN(coef(object), vcov(object))`. The
#' draw is joint over every parameter in the fit, so for a fit with several
#' curves the replicates stay aligned across curves: replicate *i* of one curve
#' and replicate *i* of another come from the same parameter draw, which is the
#' alignment invariant `toxval_pred` carries.
#'
#' A parameter named in `log_scale` is drawn as its logarithm and returned as
#' `exp()` of the draw, so every replicate of it is positive. The normal on
#' that scale has mean `log(coef)` and the delta-method covariance `J V J'`,
#' where `V` is `vcov(object)` and `J` is diagonal, `1 / coef` for each such
#' parameter and 1 for the others.
#'
#' The draw uses the Cholesky factor of the covariance, so a seed gives the
#' same replicates on every platform, and the deviates fill by row, so
#' increasing `n_boot` appends replicates rather than redrawing the first ones.
#'
#' @param object A fitted model with `coef()` and `vcov()` methods.
#' @param n_boot Number of bootstrap replicates.
#' @param seed Seed for the draw, or `NULL` to use the current RNG state.
#' @param log_scale Names of coefficients to draw on the log scale, each with a
#'   positive estimate.
#'
#' @return A numeric matrix, `n_boot` rows by one column per parameter, with
#'   the parameter names as column names.
#'
#' @noRd
boot_parms <- function(object, n_boot, seed = NULL, log_scale = character()) {
  mu <- stats::coef(object)
  sigma <- stats::vcov(object)

  if (anyNA(mu)) {
    chk::abort_chk(
      "`coef(object)` must not contain missing values; a parameter may not be ",
      "estimable from the data."
    )
  }
  if (anyNA(sigma)) {
    chk::abort_chk(
      "`vcov(object)` must not contain missing values; the fit may not be ",
      "identified."
    )
  }

  is_log <- names(mu) %in% log_scale
  jacobian <- ifelse(is_log, 1 / mu, 1)
  mu[is_log] <- log(mu[is_log])
  sigma <- sigma * outer(jacobian, jacobian)

  root <- tryCatch(chol(sigma), error = function(e) NULL)
  if (is.null(root)) {
    chk::abort_chk(
      "`vcov(object)` must be positive definite; the fit may not be ",
      "identified."
    )
  }

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  z <- matrix(stats::rnorm(n_boot * length(mu)), nrow = n_boot, byrow = TRUE)
  parms <- sweep(z %*% root, 2, mu, "+")
  parms[, is_log] <- exp(parms[, is_log])
  colnames(parms) <- names(mu)
  parms
}
