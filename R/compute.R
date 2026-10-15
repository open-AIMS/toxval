#' Per-realisation ECx values from a toxval_pred
#'
#' For each realisation the reference is built from that realisation's own
#' control, the predicted response at the first value of `x_vec`. Each
#' direction is sought against its own reference, so every curve is searched
#' for both a decreasing and an increasing crossing.
#'
#' | `type` | decreasing | increasing |
#' |---|---|---|
#' | `absolute` | control to 0 | control to 1 |
#' | `range` | control to the minimum of the curve | control to the maximum of the curve |
#' | `direct` | `ecx_val`, a response value | as decreasing |
#'
#' Under `absolute` the increasing reference is `NA` for a realisation whose
#' control is at or above 1, since the percentage has no span to be taken of.
#'
#' Values are not summarised. A realisation with no crossing in a direction is
#' `NA`.
#'
#' @param pred A `toxval_pred`.
#' @param ecx_val The percentage effect, or the response value under `direct`.
#' @param type One of `"absolute"`, `"range"` or `"direct"`.
#'
#' @return A tibble with one row per curve and direction: the columns of
#'   `keys`, `direction`, `ecx_val`, `type`, and the list columns `control`,
#'   `reference` and `draws`, each holding one value per realisation.
#'
#' @noRd
compute_ecx <- function(pred, ecx_val = 10, type = "absolute") {
  chk::chk_s3_class(pred, "toxval_pred")
  chk_type(type)
  chk_ecx_val(ecx_val, type)
  
  rows <- lapply(seq_along(pred$curves), function(i) {
    # row names would carry through to every per-realisation vector
    curve <- unname(pred$curves[[i]])
    control <- curve[, 1]
    reference <- ecx_reference(curve, ecx_val, type)
    draws <- lapply(directions(), function(direction) {
      crossings_by_row(curve, pred$x_vec, reference[[direction]], direction)
    })

    dplyr::bind_cols(
      keys_by_direction(pred$keys, i),
      dplyr::tibble(
        direction = directions(),
        ecx_val = ecx_val,
        type = type,
        control = list(control, control),
        reference = unname(reference[directions()]),
        draws = unname(draws)
      )
    )
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
#' `type`; under `direct` it is the reference itself.
#'
#' Values are not summarised. A realisation with no crossing in a direction is
#' `NA`.
#'
#' @param pred A `toxval_pred`.
#' @param sig_val The quantile of the control realisations that sets the
#'   decreasing reference.
#' @param type One of `"absolute"`, `"range"` or `"direct"`, selecting the
#'   `ecnsec` definition.
#'
#' @return A tibble with one row per curve and direction: the columns of
#'   `keys`, `direction`, `sig_val`, `type`, `reference`, and the list columns
#'   `control`, `draws` and `ecnsec`, each holding one value per realisation.
#'
#' @noRd
compute_nsec <- function(pred, sig_val = 0.01, type = "absolute") {
  chk::chk_s3_class(pred, "toxval_pred")
  chk_sig_val(sig_val)
  chk_type(type)

  rows <- lapply(seq_along(pred$curves), function(i) {
    # row names would carry through to every per-realisation vector
    curve <- unname(pred$curves[[i]])
    control <- curve[, 1]
    reference <- c(
      decreasing = unname(stats::quantile(control, sig_val)),
      increasing = unname(stats::quantile(control, 1 - sig_val))
    )
    draws <- lapply(directions(), function(direction) {
      reference_by_row <- rep(reference[[direction]], nrow(curve))
      crossings_by_row(curve, pred$x_vec, reference_by_row, direction)
    })
    ecnsec <- ecnsec_value(curve, reference, type)

    dplyr::bind_cols(
      keys_by_direction(pred$keys, i),
      dplyr::tibble(
        direction = directions(),
        sig_val = sig_val,
        type = type,
        reference = unname(reference[directions()]),
        control = list(control, control),
        draws = unname(draws),
        ecnsec = unname(ecnsec[directions()])
      )
    )
  })
  dplyr::bind_rows(rows)
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

#' The ECx reference for every realisation of a curve, in each direction
#'
#' @noRd
ecx_reference <- function(curve, ecx_val, type, upper = 1) {
  control <- curve[, 1]
  n <- nrow(curve)
  p <- ecx_val / 100

  reference <- switch(
    type,
    absolute = list(
      decreasing = control - control * p,
      increasing = ifelse(
        control < upper,
        control + (upper - control) * p,
        NA_real_
      )
    ),
    range = list(
      decreasing = control - (control - apply(curve, 1, min)) * p,
      increasing = control + (apply(curve, 1, max) - control) * p
    ),
    direct = list(
      decreasing = rep(ecx_val, n),
      increasing = rep(ecx_val, n)
    )
  )
  reference
}

#' The effect at the NSEC reference for every realisation of a curve, in each
#' direction
#'
#' The inverse of `ecx_reference()`: the percentage of the span between the
#' control and the bound that the reference lies at. Under `direct` it is the
#' reference itself.
#'
#' @param reference The NSEC reference, a numeric vector named `decreasing`
#'   and `increasing`.
#'
#' @noRd
ecnsec_value <- function(curve, reference, type, upper = 1) {
  control <- curve[, 1]
  n <- nrow(curve)
  decreasing <- reference[["decreasing"]]
  increasing <- reference[["increasing"]]

  ecnsec <- switch(
    type,
    absolute = list(
      decreasing = percent_of_span(
        control - decreasing, 
        control
      ),
      increasing = percent_of_span(
        increasing - control, 
        upper - control
      )
    ),
    range = list(
      decreasing = percent_of_span(
        control - decreasing,
        control - apply(curve, 1, min)
      ),
      increasing = percent_of_span(
        increasing - control,
        apply(curve, 1, max) - control
      )
    ),
    direct = list(
      decreasing = rep(decreasing, n),
      increasing = rep(increasing, n)
    )
  )
  ecnsec
}

#' A shift as a percentage of a span, or `NA` where the span is not positive
#'
#' A zero span would divide by zero and a negative one would flip the sign, so
#' neither gives a percentage.
#'
#' @noRd
percent_of_span <- function(shift, span) {
  percent <- shift / span * 100
  percent[!(span > 0)] <- NA_real_
  percent
}
