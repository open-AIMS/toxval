#' Per-realisation ECx values from a toxval_pred
#'
#' For each realisation the reference is built from that realisation's own
#' control, the predicted response at the first value of `x_vec`. Each
#' direction is sought against its own reference, so every curve is searched
#' for both a decreasing and an increasing crossing.
#'
#' | `type` | decreasing | increasing |
#' |---|---|---|
#' | `absolute` | control to 0 | control to 1 for a proportion, otherwise control to twice the control |
#' | `relative` | control to the lower asymptote | control to the upper asymptote |
#' | `range` | control to the minimum of the curve | control to the maximum of the curve |
#' | `direct` | `reference`, a response value | as decreasing |
#'
#' The increasing `absolute` forms are extra risk for a proportion and relative
#' deviation for any other response (T18): a 10 per cent increase from a control
#' of 20 is 22. The reference is `NA` for a realisation whose span, the distance
#' from the control to the bound, is zero or less, such as a control at or below
#' 0 under `absolute`, since the proportion then has nothing to be taken of.
#' Under `range` such a realisation is flagged in `no_span`, so that it is
#' censored rather than excluded when summarised (T38).
#'
#' Values are not summarised. A realisation with no crossing in a direction is
#' `NA`.
#'
#' @param pred A `toxval_pred`.
#' @param proportion The effect as a proportion of the span, one or more
#'   values. Not used under `direct`.
#' @param type One of `"absolute"`, `"relative"`, `"range"` or `"direct"`.
#' @param reference The response value under `direct`, and `NULL` otherwise
#'   (T32).
#'
#' @return A tibble with one row per curve, proportion and direction: the
#'   columns of `keys`, `direction`, `proportion` (`NA` under `direct`), `type`,
#'   and the list columns `control`, `reference`, `asymptote` (`NA` unless
#'   `type` is `relative`), `no_span` (`FALSE` unless `type` is `range`) and
#'   `draws`, each holding one value per realisation.
#'
#' @noRd
compute_ecx <- function(
  pred,
  proportion = 0.1,
  type = "absolute",
  reference = NULL
) {
  chk::chk_s3_class(pred, "toxval_pred")
  chk_type(type)
  if (type == "direct") {
    if (is.null(reference)) {
      chk::abort_chk("`reference` must be supplied when `type` is 'direct'.")
    }
    chk::chk_number(reference)
    proportion <- NA_real_
  } else {
    if (!is.null(reference)) {
      chk::abort_chk(
        "`reference` must be NULL unless `type` is 'direct'; the reference ",
        "is computed from `proportion`."
      )
    }
    chk_proportion(proportion, type)
  }
  chk_asymptote_supplied(pred, type)

  rows <- lapply(seq_along(pred$curves), function(i) {
    upper <- response_upper_bound(pred$meta, pred$keys, i)
    asymptote <- curve_asymptote(pred, type, i)
    # row names would carry through to every per-realisation vector
    curve <- unname(pred$curves[[i]])
    control <- curve[, 1]
    no_span <- if (type == "range") {
      no_span_by_direction(curve, type, upper)
    } else {
      list(
        decreasing = rep(FALSE, nrow(curve)),
        increasing = rep(FALSE, nrow(curve))
      )
    }
    by_proportion <- lapply(proportion, function(p) {
      references <- ecx_reference(curve, p, type, upper, reference, asymptote)
      draws <- lapply(directions(), function(direction) {
        crossings_by_row(curve, pred$x_vec, references[[direction]], direction)
      })

      dplyr::bind_cols(
        keys_by_direction(pred$keys, i),
        dplyr::tibble(
          direction = directions(),
          proportion = p,
          type = type,
          control = list(control, control),
          reference = unname(references[directions()]),
          asymptote = unname(asymptote[directions()]),
          no_span = unname(no_span[directions()]),
          draws = unname(draws)
        )
      )
    })
    dplyr::bind_rows(by_proportion)
  })
  dplyr::bind_rows(rows)
}

#' Per-realisation NSEC values from a toxval_pred
#'
#' The reference is the `sig_val` quantile of the control realisations for the
#' decreasing direction, and the `1 - sig_val` quantile for the increasing one,
#' computed per curve. It is one value across realisations, so the NSEC
#' realisations vary only through the curve. `ecnsec` is the effect at the
#' reference, the inverse of the `compute_ecx()` reference under the same
#' `type`.
#'
#' Values are not summarised. A realisation with no crossing in a direction is
#' `NA`. By construction of the quantile, a fraction `sig_val` of the
#' realisations have a control at or below the decreasing reference, and on a
#' decreasing curve none of them crosses it. How those realisations enter the
#' summary is #60.
#'
#' @param pred A `toxval_pred`.
#' @param sig_val The quantile of the control realisations that sets the
#'   decreasing reference.
#' @param type One of `"absolute"` or `"range"`, selecting the `ecnsec`
#'   definition. `"direct"` is refused (T19): its `ecnsec` would be the
#'   reference itself, which the `reference` column already reports.
#' @param anchor One of `"model"` or `"component"`, where the reference comes
#'   from under model averaging (T23). The two coincide for a fit that is not
#'   model-averaged, which is every class supported, so it is recorded only.
#'
#' @return A tibble with one row per curve and direction: the columns of
#'   `keys`, `direction`, `sig_val`, `type`, `anchor`, `reference`, and the
#'   list columns `control`, `asymptote` (`NA` unless `type` is `relative`),
#'   `draws` and `ecnsec`, each holding one value per realisation. `ecnsec` is
#'   a proportion (T32).
#'
#' @noRd
compute_nsec <- function(
  pred,
  sig_val = 0.01,
  type = "absolute",
  anchor = "model"
) {
  chk::chk_s3_class(pred, "toxval_pred")
  chk_sig_val(sig_val)
  chk_type(type)
  chk_anchor(anchor)
  if (type == "direct") {
    chk::abort_chk(
      "`type` must not be 'direct' for an NSEC; the response value at the ",
      "NSEC is reported as `reference`."
    )
  }
  chk_asymptote_supplied(pred, type)

  rows <- lapply(seq_along(pred$curves), function(i) {
    upper <- response_upper_bound(pred$meta, pred$keys, i)
    asymptote <- curve_asymptote(pred, type, i)
    # row names would carry through to every per-realisation vector
    curve <- unname(pred$curves[[i]])
    control <- curve[, 1]
    # a realisation with no finite control, such as a bootstrap draw outside
    # the parameter space, has no place in the reference; it is excluded in
    # the summary, as in `compute_ecx()`
    finite_control <- control[is.finite(control)]
    reference <- c(
      decreasing = nsec_reference(finite_control, sig_val),
      increasing = nsec_reference(finite_control, 1 - sig_val)
    )
    draws <- lapply(directions(), function(direction) {
      reference_by_row <- rep(reference[[direction]], nrow(curve))
      crossings_by_row(curve, pred$x_vec, reference_by_row, direction)
    })
    ecnsec <- ecnsec_value(curve, reference, type, upper, asymptote)

    dplyr::bind_cols(
      keys_by_direction(pred$keys, i),
      dplyr::tibble(
        direction = directions(),
        sig_val = sig_val,
        type = type,
        anchor = anchor,
        reference = unname(reference[directions()]),
        control = list(control, control),
        asymptote = unname(asymptote[directions()]),
        draws = unname(draws),
        ecnsec = unname(ecnsec[directions()])
      )
    )
  })
  dplyr::bind_rows(rows)
}

#' Check that `relative` has an asymptote to measure to
#'
#' A `toxval_pred` has no asymptote when its method could not supply one, such
#' as a `brmsfit` predicted without `asymptote`, so `relative` is refused rather
#' than measured to an assumed bound (T9, T29).
#'
#' @noRd
chk_asymptote_supplied <- function(pred, type) {
  # a method can supply NA asymptotes rather than NULL, as drc does for a
  # function without lowerAs() or upperAs()
  no_asymptote <- is.null(pred$asymptote) ||
    all(is.na(unlist(pred$asymptote)))
  if (type == "relative" && no_asymptote) {
    chk::abort_chk(
      "`type` must not be 'relative' when `object` supplies no asymptote; ",
      "supply `asymptote` where the `toxval_predict()` method takes it, or ",
      "use `type` 'absolute' or 'range'."
    )
  }
  invisible(pred)
}

#' The asymptotes of curve `i`, or `NA` where `type` does not use them
#'
#' @return A list of two numeric vectors, `decreasing` and `increasing`, one
#'   value per realisation.
#'
#' @noRd
curve_asymptote <- function(pred, type, i) {
  if (type == "relative") {
    return(pred$asymptote[[i]])
  }
  none <- rep(NA_real_, pred$meta$n_realisation)
  list(decreasing = none, increasing = none)
}

#' The NSEC reference, a quantile of the finite control realisations
#'
#' @noRd
nsec_reference <- function(control, prob) {
  if (!length(control)) {
    return(NA_real_)
  }
  unname(stats::quantile(control, prob))
}

#' @noRd
directions <- function() {
  c("decreasing", "increasing")
}

#' The row of `keys` for curve `i`, once per direction
#'
#' @noRd
keys_by_direction <- function(keys, i) {
  rows <- rep(i, length(directions()))
  dplyr::slice(dplyr::as_tibble(keys), rows)
}

#' The first crossing in one direction for every realisation of a curve
#'
#' @param curve A matrix of realisations, one per row.
#' @param x_vec The predictor grid.
#' @param reference The reference value for each realisation.
#' @param direction `"decreasing"` or `"increasing"`.
#'
#' @noRd
crossings_by_row <- function(curve, x_vec, reference, direction) {
  vapply(
    seq_len(nrow(curve)),
    function(j) find_crossings(curve[j, ], x_vec, reference[j])[[direction]],
    numeric(1)
  )
}

#' The upper bound of the response, which sets the increasing `absolute` span
#'
#' A curve predicted on the proportion scale is bounded at 1, and its
#' increasing `absolute` reference is extra risk, which is Abbott's formula. Any
#' other response has no upper bound, and its increasing reference is relative
#' deviation from the control, the mirror of the decreasing form (T18).
#'
#' @param meta The `meta` of a `toxval_pred`.
#' @param keys The `keys` of the `toxval_pred`.
#' @param i The curve, a row of `keys`.
#'
#' @return 1 for a proportion and `Inf` for a response with no upper bound.
#'
#' @noRd
response_upper_bound <- function(meta, keys, i) {
  family <- curve_family(meta$family, keys, i)
  if (is.null(family)) {
    return(Inf)
  }
  # `drc` fits a binomial response on the proportion scale, and names it by
  # `type` rather than by a `brms` family
  if (identical(meta$source_class, "drc")) {
    return(if (family == "binomial") 1 else Inf)
  }
  switch(
    family,
    bernoulli = ,
    beta = ,
    zero_inflated_beta = ,
    zero_one_inflated_beta = ,
    # predicted at one trial by `toxval_predict.brmsfit()`, so a proportion
    # whatever the trials in the data (T26)
    binomial = ,
    beta_binomial = ,
    zero_inflated_binomial = 1,
    Inf
  )
}

#' The family of one curve
#'
#' `meta$family` is one string for every curve, or a vector named by response
#' for a multivariate fit whose responses differ in family (T26).
#'
#' @noRd
curve_family <- function(family, keys, i) {
  if (is.null(family) || is.null(names(family))) {
    return(family)
  }
  family[[as.character(keys$response[i])]]
}

#' The span a percentage effect is taken of, for every realisation of a curve
#'
#' The reference is the control minus (decreasing) or plus (increasing) a
#' percentage of this span. `ecx_reference()` and `ecnsec_value()` both take the
#' span from here, so `ecnsec` stays the inverse of the `ecx` reference (T8). A
#' span of zero or less, up to rounding error relative to the control, gives no
#' percentage, so it is returned as `NA`: under
#' `absolute` that is a control at or below 0, or at or above 1 for a
#' proportion; under `relative` a control at or beyond the asymptote; under
#' `range` a curve with no extent in that direction.
#'
#' @param upper The upper bound from `response_upper_bound()`.
#' @param asymptote The output of `curve_asymptote()`, used under `relative`.
#'
#' @return A list of two numeric vectors, `decreasing` and `increasing`, one
#'   value per realisation.
#'
#' @noRd
reference_span <- function(curve, type, upper, asymptote = NULL) {
  control <- curve[, 1]
  span <- span_extent(curve, type, upper, asymptote)
  lapply(span, function(x) {
    x[no_span(x, control)] <- NA_real_
    x
  })
}

#' Whether each realisation has no span, in each direction
#'
#' Under `range` such a realisation is censored rather than excluded (T38): its
#' curve never falls below, or rises above, its control within the evaluated
#' range.
#'
#' @inheritParams reference_span
#'
#' @return A list of two logical vectors, `decreasing` and `increasing`, one
#'   value per realisation.
#'
#' @noRd
no_span_by_direction <- function(curve, type, upper, asymptote = NULL) {
  control <- curve[, 1]
  span <- span_extent(curve, type, upper, asymptote)
  lapply(span, no_span, control = control)
}

#' @noRd
span_extent <- function(curve, type, upper, asymptote = NULL) {
  control <- curve[, 1]
  increasing_absolute <- if (is.finite(upper)) {
    upper - control
  } else {
    control
  }
  switch(
    type,
    absolute = list(
      decreasing = control,
      increasing = increasing_absolute
    ),
    relative = list(
      decreasing = control - asymptote$decreasing,
      increasing = asymptote$increasing - control
    ),
    range = list(
      decreasing = control - apply(curve, 1, min),
      increasing = apply(curve, 1, max) - control
    )
  )
}

#' @noRd
no_span <- function(span, control) {
  # a span that is zero in exact arithmetic can be a few units of rounding
  # error above zero, so compare against a tolerance on the scale of the control
  tolerance <- sqrt(.Machine$double.eps) * abs(control)
  out <- span <= tolerance
  out[is.na(out)] <- FALSE
  out
}

#' The ECx reference for every realisation of a curve, in each direction
#'
#' @param proportion A single proportion of the span.
#' @param reference The response value under `direct`.
#'
#' @noRd
ecx_reference <- function(
  curve,
  proportion,
  type,
  upper,
  reference = NULL,
  asymptote = NULL
) {
  n <- nrow(curve)
  if (type == "direct") {
    return(list(
      decreasing = rep(reference, n),
      increasing = rep(reference, n)
    ))
  }
  control <- curve[, 1]
  span <- reference_span(curve, type, upper, asymptote)
  list(
    decreasing = control - span$decreasing * proportion,
    increasing = control + span$increasing * proportion
  )
}

#' The effect at the NSEC reference for every realisation of a curve, in each
#' direction
#'
#' The inverse of `ecx_reference()`: the proportion of the span that the
#' reference lies from the control (T32).
#'
#' @param reference The NSEC reference, a numeric vector named `decreasing`
#'   and `increasing`.
#'
#' @noRd
ecnsec_value <- function(curve, reference, type, upper, asymptote = NULL) {
  control <- curve[, 1]
  span <- reference_span(curve, type, upper, asymptote)
  list(
    decreasing = (control - reference[["decreasing"]]) / span$decreasing,
    increasing = (reference[["increasing"]] - control) / span$increasing
  )
}
