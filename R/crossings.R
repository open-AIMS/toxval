#' Find where one realisation of a curve crosses a reference value
#'
#' @param y Numeric, one realisation's fitted values over `x_vec`.
#' @param x_vec Numeric, the predictor grid.
#' @param reference Numeric scalar, the response value to cross.
#'
#' @details
#' A crossing is a change of side of `reference` between two adjacent grid
#' points. A value exactly on the reference is assigned to the left of its
#' interval, so a curve sitting on the reference and then departing crosses
#' where it departs. Only the `n - 1` intervals are scanned, so the last grid
#' point starts no interval and a curve that only reaches the reference there
#' has not crossed.
#'
#' @return A list of two scalars, `decreasing` and `increasing`, each the x
#'   value of the first crossing in that direction, or `NA_real_`.
#'
#' @noRd
find_crossings <- function(y, x_vec, reference) {
  chk::chk_scalar(reference)
  chk::chk_numeric(reference)

  d <- y - reference
  n <- length(d)
  ok <- is.finite(d)
  j <- which(ok[-n] & ok[-1])

  lo <- d[j]
  hi <- d[j + 1]

  list(
    decreasing = interp_crossing(j[lo >= 0 & hi < 0], d, x_vec),
    increasing = interp_crossing(j[lo <= 0 & hi > 0], d, x_vec)
  )
}

#' @noRd
interp_crossing <- function(j, d, x_vec) {
  if (!length(j)) {
    return(NA_real_)
  }
  j <- min(j)
  frac <- d[j] / (d[j] - d[j + 1])
  x_vec[j] + frac * (x_vec[j + 1] - x_vec[j])
}
