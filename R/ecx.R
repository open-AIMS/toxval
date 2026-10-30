#' Estimate ECx values from a fitted model
#'
#' Estimates the concentration at which the fitted response changes by a given
#' proportion from the control, in each direction, for a fitted
#' concentration-response model.
#'
#' @details
#' The model is evaluated over a grid of concentrations once per realisation,
#' by [toxval_predict()]: once per posterior draw for a Bayesian fit, and once
#' per parametric bootstrap replicate for a frequentist one. The ECx is found
#' for each realisation and the realisations are summarised by `prob_vals`.
#'
#' Each realisation is searched for both a decreasing and an increasing
#' crossing, and each direction is a row of the result. A direction in which no
#' realisation crosses is returned with `NA` estimates.
#'
#' `type` names what `proportion` is taken of, from the control, the predicted
#' response at the control concentration, by default the lowest concentration
#' in the data (see [toxval_predict()]):
#'
#' - `"absolute"` (the default): decreasing, the control down to 0; increasing,
#'   the control up to 1 for a response on the proportion scale, and otherwise
#'   up to twice the control.
#' - `"relative"`: the control to the lower (decreasing) or upper (increasing)
#'   asymptote of the curve, a parameter of the fitted equation. It is
#'   available where the fit supplies the asymptote: from the limits of a `drc`
#'   fit, and for a `brmsfit` from the parameters named by `asymptote` (see
#'   [toxval_predict()]). An asymptote outside the observed responses is an
#'   extrapolation, and `relative` inherits its uncertainty. Before toxval
#'   2.0.0 `"relative"` meant what is now `"range"`, so supplying it warns.
#' - `"range"`: the control to the lowest (decreasing) or highest (increasing)
#'   response the curve predicts.
#' - `"direct"`: no proportion; the ECx is where the curve crosses the response
#'   value supplied as `reference`.
#'
#' A realisation that does not reach the reference within the evaluated range
#' ranks above the highest concentration, and an interval limit that falls
#' among such realisations is reported as the highest concentration, with a
#' warning. When any realisation is censored, the reported quantiles are
#' observed order statistics. Under `"range"` this includes a realisation that never falls below,
#' or rises above, its control. Under `"absolute"` and `"relative"`, a
#' realisation with nothing to take the proportion of, such as a control at or
#' below 0 or a missing asymptote, is excluded from the summary, with a warning.
#'
#' @section Fits from bayesnec:
#' `ecx()` also accepts a `bayesnecfit` or `bayesmanecfit` from
#' `bayesnec::bnec()` when `bayesnec` is installed. `bayesnec` has its own
#' `ecx()`, and when both packages are loaded with `library()`, the one loaded
#' last is used. Call `toxval::ecx()` to make sure this function runs.
#' The two functions can give different estimates for the same fit, because
#' they evaluate the curve at different concentrations.
#'
#' @inheritParams toxval_params
#' @param proportion A numeric vector of effects, as proportions of the span
#'   named by `type`; 0.1 is the EC10. Each value gives its own rows. Not
#'   supplied under `type = "direct"`.
#' @param type One of `"absolute"`, `"relative"`, `"range"` or `"direct"`. See
#'   Details.
#' @param reference A number, the response value whose crossing is sought under
#'   `type = "direct"`. Supplied only under `"direct"`.
#' @param ecx_val `r lifecycle::badge("deprecated")` Use `proportion`, or
#'   `reference` under `type = "direct"`.
#'
#' @return A `toxval` tibble with one row per curve, proportion and direction,
#'   and the columns:
#'
#'   - `metric`: `"ecx"`.
#'   - `direction`: `"decreasing"` or `"increasing"`.
#'   - `group`: the level of `group_var`, where it is supplied.
#'   - `response`: the response of a multivariate fit.
#'   - `estimate`, `conf.low`, `conf.high`: the ECx, as the `prob_vals`
#'     quantiles of the realisations, after `xform`.
#'   - `proportion`, `type`: the arguments of the call. `proportion` is `NA`
#'     under `type = "direct"`.
#'   - `control`: the predicted response at the control concentration, or
#'     `NA` under `type = "direct"`.
#'   - `reference`: the response whose crossing is the ECx.
#'   - `asymptote`: the asymptote the span is measured to, or `NA` unless `type`
#'     is `"relative"`.
#'   - `draws`: where `draws = TRUE`, a tibble per row of `.draw` and
#'     `value.draw`, the ECx of each realisation, after `xform`. A realisation
#'     that does not reach the reference is `Inf`, or `-Inf` where `xform` is
#'     decreasing, so that it ranks beyond the highest concentration. One
#'     excluded from the summary is `NA`.
#'
#'   `control`, `reference` and `asymptote` are on the response scale, as the
#'   central `prob_vals` quantile of the realisations. The tibble prints the
#'   model class and the number of realisations above the rows.
#'
#' @examplesIf requireNamespace("drc", quietly = TRUE)
#' \donttest{
#' fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
#'
#' # EC10 and EC50, as proportions of the span from the control down to 0
#' ecx(fit, proportion = c(0.1, 0.5), seed = 42)
#'
#' # EC10 of the span from the control to the lower asymptote
#' ecx(fit, type = "relative", seed = 42)
#'
#' # EC10 of the span from the control to the lowest predicted response
#' ecx(fit, type = "range", seed = 42)
#'
#' # concentration at which the predicted root length falls to 4
#' ecx(fit, type = "direct", reference = 4, seed = 42)
#'
#' # per-realisation values of each row
#' res <- ecx(fit, draws = TRUE, seed = 42)
#' res$draws[[1]]
#'
#' # one estimate per herbicide
#' fit_group <- drc::drm(
#'   DryMatter ~ Dose,
#'   curveid = Herbicide,
#'   data = drc::S.alba,
#'   fct = drc::LL.4()
#' )
#' ecx(fit_group, group_var = "Herbicide", seed = 42)
#' }
#'
#' @export
ecx <- function(
  object,
  ...,
  proportion = 0.1,
  type = "absolute",
  reference = NULL,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  draws = FALSE,
  prob_vals = c(0.5, 0.025, 0.975),
  xform = identity,
  ecx_val = lifecycle::deprecated(),
  posterior = lifecycle::deprecated(),
  hormesis_def = lifecycle::deprecated(),
  horme = lifecycle::deprecated(),
  by_group = lifecycle::deprecated()
) {
  # recorded before any argument is reassigned, which `missing()` would miss
  predict_args <- c(
    x_var = !missing(x_var),
    group_var = !missing(group_var),
    resolution = !missing(resolution),
    x_range = !missing(x_range),
    x_control = !missing(x_control),
    ... = ...length() > 0
  )

  # arguments deprecated in 2.0.0
  if (lifecycle::is_present(ecx_val)) {
    if (!missing(proportion) || !is.null(reference)) {
      chk::abort_chk(
        "`ecx_val` must not be supplied with `proportion` or `reference`."
      )
    }
    lifecycle::deprecate_warn(
      "2.0.0",
      "ecx(ecx_val)",
      details = "Use `proportion`, a proportion rather than a percentage, or `reference` under `type = \"direct\"`."
    )
    chk::chk_number(ecx_val)
    if (identical(type, "direct")) {
      reference <- ecx_val
    } else {
      proportion <- ecx_val / 100
    }
  } else if (identical(type, "direct") && !missing(proportion)) {
    chk::abort_chk(
      "`proportion` must not be supplied when `type` is 'direct'; supply the ",
      "response value as `reference`."
    )
  }
  if (lifecycle::is_present(posterior)) {
    lifecycle::deprecate_stop(
      "2.0.0",
      "ecx(posterior)",
      "ecx(draws)",
      details = "The result is now a `toxval` tibble with the realisations in a `draws` list column, not a vector of realisations."
    )
  }
  if (lifecycle::is_present(hormesis_def)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "ecx(hormesis_def)",
      details = "Both directions are now estimated; the argument is ignored."
    )
  }
  if (lifecycle::is_present(horme)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "ecx(horme)",
      details = "Both directions are now estimated; the argument is ignored."
    )
  }
  if (lifecycle::is_present(by_group)) {
    lifecycle::deprecate_warn(
      "2.0.0",
      "ecx(by_group)",
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
    pred <- toxval_predict(
      object,
      x_var = x_var,
      group_var = group_var,
      resolution = resolution,
      x_range = x_range,
      x_control = x_control,
      ...
    )
  }

  rows <- compute_ecx(
    pred,
    proportion = proportion,
    type = type,
    reference = reference
  )
  summarise_metric(
    rows,
    pred,
    metric = "ecx",
    prob_vals = prob_vals,
    xform = xform,
    draws = draws
  )
}

#' Whether an argument is `NULL` or `NA`
#'
#' `group_var = NA` and `x_range = NA`, the defaults before toxval 2.0.0, are
#' treated as `NULL` without a warning.
#'
#' @noRd
is_missing_value <- function(x) {
  is.null(x) || (length(x) == 1 && is.atomic(x) && is.na(x))
}

#' Warn that `relative` has changed meaning
#'
#' A plain warning on every call rather than a lifecycle deprecation, because
#' `relative` is not deprecated.
#'
#' @noRd
warn_relative <- function(type) {
  if (identical(type, "relative")) {
    warning(
      "`type = \"relative\"` now measures from the control to the asymptote ",
      "of the curve. Before toxval 2.0.0 it measured to the extent of the ",
      "curve, which is now `type = \"range\"`; the two also differ for a ",
      "non-monotonic curve, because `range` measures from the control rather ",
      "than from the maximum.",
      call. = FALSE
    )
  }
  invisible(type)
}
