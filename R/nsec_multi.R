#' Estimate NSEC values from a multivariate brmsfit
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `nsec_multi()` is deprecated in favour of [nsec()], which gives one row per
#' response and direction for a multivariate fit, reading the responses from
#' the fit. It now calls [nsec()] and returns its `toxval` tibble, keeping the
#' rows `type` selects.
#'
#' @section Converting to `nsec()`:
#' `multi_var` and `trials_var` are not needed: the responses and any trials
#' column are read from the fit. `type` becomes a filter on the `direction`
#' column, and `posterior = TRUE` becomes `draws = TRUE`.
#'
#' ```r
#' # nsec_multi(fit, x_var = "dose", multi_var = "sp")
#' nsec(fit, x_var = "dose")
#'
#' # nsec_multi(fit, x_var = "dose", multi_var = "sp", type = "decreasing")
#' nsec(fit, x_var = "dose") |>
#'   dplyr::filter(direction == "decreasing")
#'
#' # nsec_multi(fit, x_var = "dose", multi_var = "sp", type = "lower",
#' #   criterion = 0.8)
#' nsec(fit, x_var = "dose", draws = TRUE) |>
#'   dplyr::mutate(
#'     at_criterion = vapply(
#'       draws,
#'       \(d) quantile(d$value.draw, 0.8, na.rm = TRUE),
#'       numeric(1)
#'     )
#'   ) |>
#'   dplyr::group_by(response) |>
#'   dplyr::slice_min(at_criterion, n = 1, with_ties = FALSE) |>
#'   dplyr::ungroup()
#' ```
#'
#' `type = "lower"` keeps, for each response, the direction whose NSEC is lower
#' at the `criterion` quantile of its realisations; a tie keeps the decreasing
#' direction.
#'
#' @inheritParams nsec
#' @param x_var A string naming the predictor variable.
#' @param resolution A whole number giving the number of predictor values to
#'   evaluate the curve at.
#' @param x_range A numeric vector of length 2 giving the range of predictor
#'   values to evaluate over, or `NA` to use the range in the model data.
#' @param posterior A flag; `TRUE` adds the `draws` list column, as
#'   `draws = TRUE` does in [nsec()].
#' @param trials_var,multi_var Ignored; the trials and the responses are read
#'   from the fit.
#' @param type One of `"both"`, `"lower"`, `"increasing"` or `"decreasing"`,
#'   the directions to return.
#' @param criterion A number between 0 and 1, the quantile of the realisations
#'   compared when `type = "lower"`.
#' @param ... Additional arguments passed to [nsec()].
#'
#' @return A `toxval` tibble, as from [nsec()], with the rows `type` selects.
#'
#' @keywords internal
#' @export
nsec_multi <- function(
  object,
  sig_val = 0.01,
  resolution = 50,
  x_range = NA,
  xform = identity,
  prob_vals = c(0.5, 0.025, 0.975),
  posterior = FALSE,
  x_var,
  trials_var = NA,
  multi_var = NA,
  type = "both",
  criterion = 0.8,
  ...
) {
  lifecycle::deprecate_warn(
    "2.0.0",
    "nsec_multi()",
    "nsec()",
    details = "The responses of a multivariate fit are read from the fit, and `type` is a filter on the `direction` column of the result."
  )
  chk::chk_flag(posterior)
  chk::chk_string(type)
  chk::chk_subset(type, c("both", "lower", "increasing", "decreasing"))
  chk::chk_number(criterion)
  chk::chk_range(criterion)
  if (missing(x_var)) {
    x_var <- NULL
  }
  if (is_missing_value(x_range)) {
    x_range <- NULL
  }

  result <- nsec(
    object,
    sig_val = sig_val,
    x_var = x_var,
    resolution = resolution,
    x_range = x_range,
    draws = posterior || type == "lower",
    prob_vals = prob_vals,
    xform = xform,
    ...
  )

  if (type %in% c("increasing", "decreasing")) {
    result <- result[result$direction == type, ]
  }
  if (type == "lower") {
    result <- lowest_direction(result, criterion)
    if (!posterior) {
      result$draws <- NULL
    }
  }
  result
}

#' Keep the direction with the lower NSEC for each curve
#'
#' Rows are compared at the `criterion` quantile of their realisations (T30).
#' Censored realisations are `Inf`, so they rank above every crossing, and
#' excluded ones are `NA` and are dropped. A tie keeps the decreasing row,
#' which comes first.
#'
#' @param result A `toxval` tibble from [nsec()] with `draws`.
#' @param criterion The quantile compared.
#'
#' @noRd
lowest_direction <- function(result, criterion) {
  at_criterion <- vapply(
    result$draws,
    function(d) {
      if (all(is.na(d$value.draw))) {
        return(NA_real_)
      }
      stats::quantile(d$value.draw, criterion, names = FALSE, na.rm = TRUE)
    },
    numeric(1)
  )
  curve_cols <- intersect(c("group", "response"), names(result))
  curve <- if (length(curve_cols)) {
    interaction(result[curve_cols], drop = TRUE, lex.order = TRUE)
  } else {
    factor(rep(1L, nrow(result)))
  }
  keep <- vapply(
    split(seq_len(nrow(result)), curve),
    function(rows) {
      values <- at_criterion[rows]
      if (all(is.na(values))) {
        return(rows[1])
      }
      rows[which.min(values)]
    },
    integer(1)
  )
  result[sort(keep), ]
}
