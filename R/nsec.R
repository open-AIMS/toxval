#' Estimate NSEC values from a fitted model
#'
#' Estimates the No-Significant-Effect Concentration (NSEC), the concentration
#' at which the fitted response departs from the control by more than its
#' uncertainty allows, in each direction, for a fitted concentration-response
#' model or for a [toxval_pred] built from one.
#'
#' @details
#' The model is evaluated over a grid of concentrations once per realisation,
#' by [toxval_predict()]: once per posterior draw for a Bayesian fit, and once
#' per parametric bootstrap replicate for a frequentist one.
#'
#' The reference for the decreasing direction is the `sig_val` quantile of the
#' control realisations, the predicted response at the lowest concentration,
#' and for the increasing direction the `1 - sig_val` quantile. The NSEC is
#' found for each realisation against that reference, and the realisations are
#' summarised by `prob_vals`. A direction in which no realisation crosses is
#' returned with `NA` estimates.
#'
#' A fraction `sig_val` of the realisations have a control already beyond the
#' reference. One of these that crosses the reference in the direction sought
#' takes that crossing. One whose response first changes away from the effect
#' as concentration rises from the control (rising, for a decreasing NSEC;
#' falling, for an increasing one), and which passes to the other side of the
#' reference without returning, shows no effect in the tested range. It is
#' treated as a realisation that does not reach the reference. The rest are
#' placed at the lowest concentration in the data. A
#' realisation that does not reach the reference within the evaluated range
#' ranks above the highest concentration, and an interval limit that falls
#' among such realisations is reported as the highest concentration, with a
#' warning.
#'
#' `ecnsec` is the effect at the NSEC, as a proportion of the span named by
#' `type`: the inverse of the [ecx()] reference under the same `type`.
#'
#' NSECs are not estimated for `drc` NEC models, whose threshold is a parameter
#' of the model rather than a feature of a smooth curve.
#'
#' @section Multivariate fits:
#' A multivariate `brmsfit` gives rows for each response, named in the
#' `response` column as `brms` names them; see [toxval_predict()]. This
#' replaces [nsec_multi()]. To keep, for each response, only the direction
#' whose NSEC is lower at the 0.8 quantile of its realisations, as
#' `nsec_multi(type = "lower")` did, filter the result returned with
#' `draws = TRUE`:
#'
#' ```r
#' nsec(fit, x_var = "dose", draws = TRUE) |>
#'   dplyr::mutate(
#'     at_criterion = vapply(
#'       draws,
#'       \(d) quantile(d$value.draw, 0.8, na.rm = TRUE),
#'       numeric(1)
#'     )
#'   ) |>
#'   dplyr::filter(!is.na(estimate)) |>
#'   dplyr::group_by(response) |>
#'   dplyr::slice_min(at_criterion, n = 1, with_ties = FALSE) |>
#'   dplyr::ungroup()
#' ```
#'
#' A censored realisation is `Inf` in `value.draw`, so it ranks above every
#' crossing, and an excluded one is `NA`. A row whose estimate is `NA` has no
#' crossing in that direction, so it is dropped before choosing.
#'
#' @inheritParams toxval_params
#' @param sig_val A number between 0 and 0.5, the quantile of the control
#'   realisations that sets the decreasing reference; `1 - sig_val` sets the
#'   increasing one.
#' @param type One of `"absolute"`, `"relative"` or `"range"`, naming the span
#'   `ecnsec` is a proportion of. See [ecx()].
#' @param anchor One of `"model"` (the default) or `"component"`, where the
#'   reference is taken from for a model-averaged fit: the averaged control,
#'   or each component model's own control. The two coincide for a fit that is
#'   not model-averaged.
#' @param curveid `r lifecycle::badge("deprecated")` Use `group_var`.
#'
#' @return A `toxval` tibble with one row per curve and direction, and the
#'   columns:
#'
#'   - `metric`: `"nsec"`.
#'   - `direction`: `"decreasing"` or `"increasing"`.
#'   - `group`: the level of `group_var`, where it is supplied.
#'   - `response`: the response of a multivariate fit.
#'   - `estimate`, `conf.low`, `conf.high`: the NSEC, as the `prob_vals`
#'     quantiles of the realisations, after `xform`.
#'   - `sig_val`, `type`, `anchor`: the arguments of the call.
#'   - `control`: the predicted response at the lowest concentration.
#'   - `reference`: the `sig_val` (decreasing) or `1 - sig_val` (increasing)
#'     quantile of the control realisations, whose crossing is the NSEC.
#'   - `asymptote`: the asymptote `ecnsec` is measured to, or `NA` unless `type`
#'     is `"relative"`.
#'   - `ecnsec`, `ecnsec.low`, `ecnsec.high`: the effect at the NSEC, as a
#'     proportion of the span named by `type`, summarised by `prob_vals`.
#'   - `draws`: where `draws = TRUE`, a tibble per row of `.draw`,
#'     `value.draw`, the NSEC of each realisation, and `ecnsec.draw`. A
#'     realisation that does not reach the reference is `Inf` in `value.draw`,
#'     and one excluded from the summary is `NA`.
#'
#'   `control`, `reference` and `asymptote` are on the response scale, as the
#'   central `prob_vals` quantile of the realisations. The tibble prints the
#'   model class and the number of realisations above the rows.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("drc", quietly = TRUE)) {
#'   fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
#'   nsec(fit, seed = 42)
#' }
#' }
#'
#' @export
nsec <- function(
  object,
  ...,
  sig_val = 0.01,
  type = "absolute",
  anchor = "model",
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  draws = FALSE,
  prob_vals = c(0.5, 0.025, 0.975),
  xform = identity,
  posterior = lifecycle::deprecated(),
  hormesis_def = lifecycle::deprecated(),
  horme = lifecycle::deprecated(),
  by_group = lifecycle::deprecated(),
  curveid = lifecycle::deprecated()
) {
  # recorded before any argument is reassigned, which `missing()` would miss
  predict_args <- c(
    x_var = !missing(x_var),
    group_var = !missing(group_var),
    resolution = !missing(resolution),
    x_range = !missing(x_range),
    ... = ...length() > 0
  )

  # arguments deprecated in 2.0.0
  if (lifecycle::is_present(posterior)) {
    lifecycle::deprecate_stop(
      "2.0.0",
      "nsec(posterior)",
      "nsec(draws)",
      details = "The result is now a `toxval` tibble with the realisations in a `draws` list column, not a vector of realisations."
    )
  }
  if (lifecycle::is_present(hormesis_def)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "nsec(hormesis_def)",
      details = "Both directions are now estimated; the argument is ignored."
    )
  }
  if (lifecycle::is_present(horme)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "nsec(horme)",
      details = "Both directions are now estimated; the argument is ignored."
    )
  }
  if (lifecycle::is_present(curveid)) {
    lifecycle::deprecate_warn("2.0.0", "nsec(curveid)", "nsec(group_var)")
    if (!is_missing_value(curveid)) {
      if (!is_missing_value(group_var)) {
        chk::abort_chk("`curveid` must not be supplied with `group_var`.")
      }
      group_var <- curveid
      predict_args[["group_var"]] <- TRUE
    }
  }
  if (lifecycle::is_present(by_group)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "nsec(by_group)",
      details = "Every level of `group_var` now gives its own estimate, so `by_group` is not needed."
    )
    chk::chk_flag(by_group)
    if (by_group && is_missing_value(group_var)) {
      chk::abort_chk("`group_var` must be supplied when `by_group` is TRUE.")
    }
    if (!by_group && !is_missing_value(group_var)) {
      chk::abort_chk(
        "`by_group = FALSE` with a `group_var` is no longer supported; every ",
        "level of `group_var` now gives its own estimate."
      )
    }
  }
  if (is_missing_value(group_var)) {
    group_var <- NULL
  }
  if (is_missing_value(x_range)) {
    x_range <- NULL
  }
  warn_relative(type)

  if (inherits(object, "toxval_pred")) {
    if (any(predict_args)) {
      chk::abort_chk(
        "`object` is a `toxval_pred`, whose grid is already built, so ",
        paste0("`", names(predict_args)[predict_args], "`", collapse = ", "),
        " must not be supplied."
      )
    }
    pred <- object
  } else {
    if (inherits(object, "drc") && inherits(object$fct, "NEC")) {
      chk::abort_chk(
        "`object` must not be a `drc` NEC model; NSECs are estimated for ",
        "smooth curves only."
      )
    }
    pred <- toxval_predict(
      object,
      x_var = x_var,
      group_var = group_var,
      resolution = resolution,
      x_range = x_range,
      ...
    )
  }

  rows <- compute_nsec(pred, sig_val = sig_val, type = type, anchor = anchor)
  summarise_metric(
    rows,
    pred,
    metric = "nsec",
    prob_vals = prob_vals,
    xform = xform,
    draws = draws
  )
}
