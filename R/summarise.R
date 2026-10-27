#' Summarise per-realisation metric values into a toxval tibble
#'
#' Takes the output of `compute_ecx()` or `compute_nsec()`, one row per curve
#' and direction with the realisations in list columns, and reduces each row to
#' an estimate and interval.
#'
#' A realisation with no crossing in a direction is placed where its crossing
#' lies (T21):
#'
#' - one whose control already lies beyond the reference in the direction
#'   sought takes `meta$x_control`, the lowest concentration in the data;
#' - one whose reference is `NA`, a span of zero or less (T18), or whose
#'   control is not finite, such as a bootstrap draw outside the parameter
#'   space, is excluded, except that under `range` a span of zero or less is
#'   censored (T38), since the curve never leaves its control in that
#'   direction;
#' - any other has no crossing within the evaluated range. It ranks above
#'   `max(x_vec)` and is given no value, so a quantile that falls among such
#'   realisations is reported as `max(x_vec)`.
#'
#' A direction in which no realisation has a crossing returns `NA` (T10). The
#' call warns once when a reported quantile is censored or a realisation is
#' excluded (T25).
#'
#' `xform` is applied to the realisations before they are summarised. The
#' `control` and `reference` columns are the central quantile of the
#' realisations not excluded from that row, the same realisations as the
#' estimate (T34). Each direction excludes its own, so `control` can differ
#' between the two rows of one curve, and it is `NA` where every realisation is
#' excluded. `control` is also `NA` under `type = "direct"`, which does not use
#' it. `asymptote` is summarised over the same realisations, and is `NA` unless
#' `type` is `"relative"`.
#'
#' @param rows The output of `compute_ecx()` or `compute_nsec()`.
#' @param pred The `toxval_pred` the rows were computed on.
#' @param metric A string, the value of the `metric` column.
#' @param prob_vals The central, lower and upper quantiles.
#' @param xform A function applied to the realisations.
#' @param draws A flag. If `TRUE` a `draws` list column holds, for each row, a
#'   tibble of the realisations with columns `.draw` and `value.draw`, and
#'   `ecnsec.draw` for an NSEC. The `.draw` suffix keeps them distinct from the
#'   summary columns, so `tidyr::unnest(result, draws)` does not clash with
#'   `ecnsec`. A censored realisation is `Inf` and an excluded one `NA`.
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

  summaries <- lapply(seq_len(nrow(rows)), function(i) {
    summarise_row(rows[i, ], x_control, x_max, prob_vals, xform)
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
#'
#' @return A list with `estimates`, a one-row tibble of the estimate and interval;
#'   `descriptors`, a one-row tibble of `control`, `reference`, `asymptote` and,
#'   for an NSEC, the `ecnsec` summary; `draws`, a tibble of the realisations; and the
#'   counts `n`, `n_censored` and `n_excluded`, with `censored`, whether a
#'   reported quantile is censored.
#'
#' @noRd
summarise_row <- function(row, x_control, x_max, prob_vals, xform) {
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
  # direction, so it is censored rather than excluded (T38)
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
  beyond <- no_crossing & !excluded & !no_span & past_reference
  censored <- no_crossing & !excluded & !beyond

  placed <- values
  placed[beyond] <- x_control
  placed <- xform(placed)
  placed[censored] <- Inf

  quantiles <- rep(NA_real_, 3)
  quantile_censored <- FALSE
  if (any(!no_crossing)) {
    quantiles <- stats::quantile(placed[!excluded], prob_vals, names = FALSE)
    quantile_censored <- any(is.infinite(quantiles))
    quantiles[is.infinite(quantiles)] <- xform(x_max)
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
