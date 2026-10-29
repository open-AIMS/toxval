#' Estimate ECx values from a fitted model
#'
#' Estimates the concentration at which the fitted response changes by a given
#' proportion from the control, in each direction, for a fitted
#' concentration-response model or for a [toxval_pred] built from one.
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
#' warning. Under `"range"` this includes a realisation that never falls below,
#' or rises above, its control. Under `"absolute"` and `"relative"`, a
#' realisation with nothing to take the proportion of, such as a control at or
#' below 0 or a missing asymptote, is excluded from the summary, with a warning.
#'
#' @section Fits from bayesnec:
#' A `bayesnecfit` or `bayesmanecfit` from `bayesnec::bnec()` is accepted when
#' `bayesnec` is installed; see [toxval_predict()]. `bayesnec` also exports a
#' function named `ecx()`, and whichever of the two packages is attached later
#' masks the other's, without a warning. Call `toxval::ecx()` to be sure of
#' reaching this function.
#'
#' @param object A fitted model with a [toxval_predict()] method, or a
#'   [toxval_pred].
#' @param proportion A numeric vector of effects, as proportions of the span
#'   named by `type`; 0.1 is the EC10. Each value gives its own rows. Not
#'   supplied under `type = "direct"`.
#' @param type One of `"absolute"`, `"relative"`, `"range"` or `"direct"`. See
#'   Details.
#' @param reference A number, the response value whose crossing is sought under
#'   `type = "direct"`. Supplied only under `"direct"`.
#' @inheritParams toxval_predict
#' @param draws A flag. If `TRUE`, a `draws` list column holds the
#'   per-realisation values of each row.
#' @param prob_vals A numeric vector of the central, lower and upper quantiles
#'   to summarise the realisations by, in that order.
#' @param xform A function applied to the per-realisation concentrations before
#'   they are summarised, such as `exp` for a model fitted on log
#'   concentration.
#' @param ... Additional arguments passed to [toxval_predict()], such as
#'   `n_boot` and `seed` for a `drc` fit, or `asymptote` for a `brmsfit`.
#'   These and every argument after `...` must be named in full; an argument
#'   no method uses is an error.
#' @param ecx_val `r lifecycle::badge("deprecated")` Use `proportion`, or
#'   `reference` under `type = "direct"`.
#' @param posterior `r lifecycle::badge("deprecated")` Supplying it is an
#'   error. Use `draws`.
#' @param hormesis_def,horme `r lifecycle::badge("deprecated")` Both
#'   directions are now always estimated, so these are ignored.
#' @param by_group `r lifecycle::badge("deprecated")` Supplying `group_var`
#'   gives one estimate per level.
#'
#' @return A `toxval` tibble with one row per curve, proportion and direction.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("drc", quietly = TRUE)) {
#'   fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
#'   ecx(fit, proportion = c(0.1, 0.5), seed = 42)
#' }
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

  # arguments retired from the pre-refactor interface (T28, T32, §3.5)
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

#' Whether an argument was left at a pre-refactor default of `NA`
#'
#' `group_var = NA` and `x_range = NA` were the defaults before `NULL`, and are
#' translated without a warning (T28).
#'
#' @noRd
is_missing_value <- function(x) {
  is.null(x) || (length(x) == 1 && is.atomic(x) && is.na(x))
}

#' Warn that `relative` has changed meaning
#'
#' A plain warning on every call rather than a lifecycle deprecation, because
#' `relative` is not deprecated (T29).
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
