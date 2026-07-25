#' Generates mean posterior predictions for objects fitted by
#' [bayesnec::bnec()]
#'
#' Generates mean posterior predictions for objects fitted by
#' [bayesnec::bnec()]. `object` should be of class
#' [bayesnec::bayesnecfit] or [bayesnec::bayesmanecfit].
#'
#' @name predict
#'
#' @inheritParams toxval_params
#' @param ... Additional arguments to [brms::predict.brmsfit()] if
#' object is of class [bayesnec::bayesnecfit], or to
#' [brms::posterior_predict.brmsfit()] if object is of class
#' [bayesnec::bayesmanecfit].
#'
#' @return See `?brms::predict.brmsfit`.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `resolution` and `x_range` to generate `newdata` internally
#' predict(manec_example)
#' # Provide user-specified `newdata`
#' nd_ <- data.frame(x = seq(0, 3, length.out = 200))
#' predict(manec_example, ecx_val = 50, newdata = nd_, make_newdata = FALSE)
#' # Predictions for raw input data
#' nec4param <- bayesnec::pull_out(manec_example, model = "nec4param")
#' preds <- predict(nec4param, make_newdata = FALSE)
#' x <- bayesnec::pull_brmsfit(nec4param)$data$x
#' plot(x, preds[, 1])
#' }
NULL

#' @describeIn predict Method for a single-model fit of class
#'   [bayesnec::bayesnecfit].
#'
#' @export
predict.bayesnecfit <- function(object, ...) {
  predict(bayesnec::pull_brmsfit(object), ...)
}

#' @describeIn predict Method for a model-averaged fit of class
#'   [bayesnec::bayesmanecfit]; returns summary statistics controlled by
#'   `summary`, `robust` and `probs`.
#'
#' @param summary Should summary statistics be returned
#'  instead of the raw values? Default is `TRUE`.
#' @param robust If `FALSE` (the default) the mean is used as
#'  the measure of central tendency and the standard deviation as
#'  the measure of variability. If `TRUE`, the median and the
#'  median absolute deviation (MAD) are applied instead.
#'  Only used if `summary` is `TRUE`.
#' @param probs  The percentiles to be computed by the `quantile`
#'  function. Only used if `summary` is `TRUE`.
#'
#' @export
predict.bayesmanecfit <- function(object, summary = TRUE,
                                  robust = FALSE,
                                  probs = c(0.025, 0.975), ...) {
  av_post_preds <- posterior_predict(object, ...)
  if (!summary) {
    av_post_preds
  } else {
    out <- apply(av_post_preds, 2, posterior_summary,
                 robust = robust, probs = probs) |>
      t()
    colnames(out) <- c("Estimate", "Est.Error",
                       paste0("Q", probs * 100))
    out
  }
}
