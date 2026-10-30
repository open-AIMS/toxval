#' Summarise per-realisation metric values into a toxval tibble
#'
#' Takes the output of `compute_ecx()` or `compute_nsec()`, one row per curve
#' and direction with the realisations in list columns, and reduces each row to
#' an estimate and interval.
#'
#' A realisation with no crossing in a direction is placed where its crossing
#' lies:
#'
#' - one whose control already lies beyond the reference in the direction
#'   sought takes `meta$x_control`, the control concentration. For
#'   an NSEC, one whose first change in response is away from the effect and
#'   which is on the no-effect side of the reference at a later grid value
#'   shows no effect in the tested range and is censored instead. An ECx keeps
#'   the first rule for every `type` (#74);
#' - one whose reference is `NA`, a span of zero or less, or whose
#'   control is not finite, such as a bootstrap draw outside the parameter
#'   space, is excluded, except that under `range` a span of zero or less is
#'   censored, since the curve never leaves its control in that
#'   direction;
#' - any other has no crossing within the evaluated range. It ranks beyond
#'   `max(x_vec)`, so above every other realisation, or below them where
#'   `xform` is decreasing. It is given no value, so a quantile that falls
#'   among such realisations is reported as `xform(max(x_vec))`.
#'
#' Where any realisation is censored, the summary uses an observed order
#' statistic, so it does not interpolate between a known concentration and an
#' unknown one beyond the range.
#'
#' A direction in which no realisation has a crossing returns `NA`. The
#' call warns once when a reported quantile is censored or a realisation is
#' excluded.
#'
#' `xform` is applied to the realisations before they are summarised. The
#' `control` column is the central quantile of the realisations not excluded
#' from that row, the same realisations as the estimate. Each direction
#' excludes its own, so `control` can differ between the two rows of one
#' curve, and it is `NA` where every realisation is excluded or under
#' `type = "direct"`. `asymptote` is summarised over the same realisations,
#' and is `NA` unless `type` is `"relative"`. `reference` is the central
#' quantile of the non-missing references, which for an NSEC is the single
#' reference of the curve.
#'
#' @param rows The output of `compute_ecx()` or `compute_nsec()`.
#' @param pred The `toxval_pred` the rows were computed on.
#' @param metric A string, the value of the `metric` column.
#' @param prob_vals The central, lower and upper quantiles.
#' @param xform A function applied to the realisations, monotone over the
#'   grid.
#' @param draws A flag. If `TRUE` a `draws` list column holds, for each row, a
#'   tibble of the realisations with columns `.draw` and `value.draw`, and
#'   `ecnsec.draw` for an NSEC. A censored realisation is `Inf`, or `-Inf`
#'   where `xform` is decreasing, and an excluded one `NA`.
#'
#' @return An object of class `toxval`.
#'
#' @noRd
summarise_metric <- function(
  rows,
  pred,
  metric,
  prob_vals = c(0.5, 0.025, 0.975),
  xform = identity,
  draws = FALSE
) {
  chk::chk_s3_class(pred, "toxval_pred")
  chk::chk_string(metric)
  chk_prob_vals(prob_vals)
  chk::chk_function(xform)
  chk::chk_flag(draws)

  x_control <- pred$meta$x_control
  x_max <- max(pred$x_vec)
  key_cols <- names(pred$keys)
  # Rank a censored realisation by the direction of xform, not by xform(Inf),
  # which need not be defined.
  censored_at <- if (xform_decreasing(xform, pred$x_vec)) -Inf else Inf

  summaries <- lapply(seq_len(nrow(rows)), function(i) {
    summarise_row(rows[i, ], x_control, x_max, prob_vals, xform, censored_at)
  })

  # the column groups of the result, left to right
  identifiers <- dplyr::tibble(metric = metric, direction = rows$direction)
  keys <- rows[key_cols]
  estimates <- dplyr::bind_rows(lapply(summaries, function(s) s$estimates))
  arguments <- rows[intersect(
    c("proportion", "sig_val", "type", "anchor"),
    names(rows)
  )]
  descriptors <- dplyr::bind_rows(lapply(summaries, function(s) s$descriptors))

  out <- dplyr::bind_cols(identifiers, keys, estimates, arguments, descriptors)

  if (draws) {
    out$draws <- lapply(summaries, function(s) s$draws)
  }

  warn_censored(keys, rows$direction, summaries)
  new_toxval(out, pred$meta)
}

#' Summarise one curve and direction
#'
#' @param row One row of the output of `compute_ecx()` or `compute_nsec()`.
#' @param x_control The concentration given to a realisation whose control is
#'   already past the reference.
#' @param x_max The highest grid value.
#' @param censored_at The value a censored realisation takes after `xform`:
#'   `Inf`, or `-Inf` where `xform` is decreasing, so that it ranks beyond
#'   `xform(x_max)`.
#' @inheritParams summarise_metric
#'
#' @return A list with `estimates`, a one-row tibble of the estimate and
#'   interval; `descriptors`, a one-row tibble of `control`, `reference`,
#'   `asymptote` and, for an NSEC, the `ecnsec` summary; `draws`, a tibble of
#'   the realisations; the counts `n`, `n_censored` and `n_excluded`; and
#'   `censored`, whether a reported quantile is censored.
#'
#' @noRd
summarise_row <- function(
  row,
  x_control,
  x_max,
  prob_vals,
  xform,
  censored_at
) {
  values <- row$draws[[1]]
  control <- row$control[[1]]
  # one value per realisation for an ECx, one for the curve for an NSEC
  reference <- row$reference
  if (is.list(reference)) {
    reference <- reference[[1]]
  }
  reference <- rep_len(reference, length(values))

  no_crossing <- is.na(values)
  # under `range`, a realisation with no span never leaves its control in this
  # direction, so it is censored rather than excluded
  no_span <- if ("no_span" %in% names(row)) {
    row$no_span[[1]] & is.finite(control)
  } else {
    rep(FALSE, length(values))
  }
  excluded <- no_crossing &
    !no_span &
    (is.na(reference) | !is.finite(control))
  if (row$direction == "decreasing") {
    past_reference <- control <= reference
  } else {
    past_reference <- control >= reference
  }
  # An NSEC realisation whose first change is away from the effect and which is
  # on the no-effect side at a later grid value is past the reference only
  # because its own control is in the tail. It shows no effect in the tested
  # range, so it is censored; either condition alone misplaces realisations.
  # An ECx row has no such flags, so a control past the reference takes
  # `x_control`: under `direct` the reference is a level the user supplies, so
  # a control past it is beyond that level (#74).
  no_effect <- if ("first_change_away" %in% names(row)) {
    row$first_change_away[[1]] & row$on_no_effect_side[[1]]
  } else {
    rep(FALSE, length(values))
  }
  beyond <- no_crossing & !excluded & !no_span & past_reference & !no_effect
  censored <- no_crossing & !excluded & !beyond

  placed <- values
  placed[beyond] <- x_control
  placed <- xform(placed)
  placed[censored] <- censored_at

  quantiles <- rep(NA_real_, 3)
  quantile_censored <- FALSE
  if (any(!no_crossing)) {
    quantiles <- stats::quantile(
      placed[!excluded],
      prob_vals,
      names = FALSE,
      type = if (any(censored)) 1 else 7
    )
    # Match the sign as well: xform can map x_control or a crossing to an
    # infinite value, such as log(0), and that quantile is not censored.
    at_censored <- quantiles == censored_at
    quantile_censored <- any(at_censored)
    quantiles[at_censored] <- xform(x_max)
  }

  if (identical(row$type, "direct")) {
    control_central <- NA_real_
  } else {
    control_central <- central(control[!excluded], prob_vals)
  }
  descriptors <- dplyr::tibble(
    control = control_central,
    reference = central(reference, prob_vals),
    asymptote = central(row$asymptote[[1]][!excluded], prob_vals)
  )
  draws <- dplyr::tibble(.draw = seq_along(placed), value.draw = placed)

  if ("ecnsec" %in% names(row)) {
    ecnsec <- row$ecnsec[[1]]
    if (all(is.na(ecnsec))) {
      ecnsec_q <- rep(NA_real_, 3)
    } else {
      ecnsec_q <- stats::quantile(
        ecnsec,
        prob_vals,
        names = FALSE,
        na.rm = TRUE
      )
    }
    descriptors$ecnsec <- ecnsec_q[1]
    descriptors$ecnsec.low <- ecnsec_q[2]
    descriptors$ecnsec.high <- ecnsec_q[3]
    draws$ecnsec.draw <- ecnsec
  }

  list(
    estimates = dplyr::tibble(
      estimate = quantiles[1],
      conf.low = quantiles[2],
      conf.high = quantiles[3]
    ),
    descriptors = descriptors,
    draws = draws,
    n = length(values),
    n_censored = sum(censored),
    n_excluded = sum(excluded),
    censored = quantile_censored
  )
}

#' @noRd
central <- function(x, prob_vals) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  stats::quantile(x, prob_vals[1], names = FALSE, na.rm = TRUE)
}

#' Whether `xform` reverses the order of concentrations over the grid
#'
#' Reads `xform` at two points strictly inside the grid, so a map that is
#' undefined or infinite at an end, such as `log` at 0, still gives an answer.
#' Where those two points give no answer, the images of the two ends decide.
#'
#' @param xform A function, monotone over the grid.
#' @param x_vec The grid of concentrations.
#'
#' @return A flag, `FALSE` unless the inner points or the ends show a
#'   decrease.
#'
#' @noRd
xform_decreasing <- function(xform, x_vec) {
  lo <- min(x_vec)
  hi <- max(x_vec)
  # Muffle the NaN warning of a map undefined at either point; the ends then
  # decide.
  probe <- suppressWarnings(xform(lo + (hi - lo) * c(1, 2) / 3))
  if (length(probe) == 2 && all(is.finite(probe)) && probe[1] != probe[2]) {
    return(probe[2] < probe[1])
  }
  ends <- suppressWarnings(xform(c(lo, hi)))
  isTRUE(length(ends) == 2 && ends[2] < ends[1])
}

#' Warn once for censored quantiles and excluded realisations
#'
#' @param keys The key columns of the rows.
#' @param direction The direction of each row.
#' @param summaries The output of `summarise_row()` for each row.
#'
#' @noRd
warn_censored <- function(keys, direction, summaries) {
  lines <- character(0)
  for (i in seq_along(summaries)) {
    s <- summaries[[i]]
    parts <- character(0)
    if (s$censored) {
      parts <- c(parts, paste0(percent(s$n_censored, s$n), " censored"))
    }
    if (s$n_excluded > 0) {
      parts <- c(parts, paste0(percent(s$n_excluded, s$n), " excluded"))
    }
    if (length(parts)) {
      lines <- c(
        lines,
        paste0(
          row_label(keys[i, ], direction[i]),
          ": ",
          paste(parts, collapse = ", ")
        )
      )
    }
  }
  if (!length(lines)) {
    return(invisible(NULL))
  }
  warning(
    "Some realisations have no crossing within the evaluated range, so a ",
    "reported quantile is at the highest concentration, or have no reference ",
    "and were excluded. Consider a wider `x_range`.\n",
    paste0("* ", lines, collapse = "\n"),
    call. = FALSE
  )
}

#' @noRd
row_label <- function(key, direction) {
  descriptors <- vapply(
    names(key),
    function(col) paste0(col, " ", as.character(key[[col]])),
    character(1)
  )
  paste(c(descriptors, direction), collapse = ", ")
}

#' @noRd
percent <- function(n, total) {
  paste0(format(round(100 * n / total, 1), nsmall = 1), "%")
}
