#' @describeIn toxval_predict Method for a non-empty list of [drc::drm()]
#'   fits to the same response, predictor, data and `curveid`. The fits may
#'   use different equations. Named lists keep their names; unnamed lists use
#'   the equation names when available from the fits. Supply list names when
#'   an equation name cannot be read. Model names must be unique. Akaike
#'   weights from AIC allocate the `n_boot` bootstrap realisations across fits.
#'   Largest-remainder rounding makes the counts sum to `n_boot`; a fit with no
#'   allocated realisations remains in `meta$model_weights`.
#'   `meta$draw_model` identifies the source of each realisation. A single
#'   seeded RNG stream gives reproducible independent draws across fits.
#'   `nsec()` defaults to `anchor = "model"` for this input; explicit
#'   `anchor = "component"` uses `n_boot` bootstrap controls from each fit.
#'   `type = "relative"` requires an asymptote from every fit. Functions
#'   without `lowerAs()` or `upperAs()` are refused, except `W1.*`, `W2.*`,
#'   `EXD.*`, `AR.*` and `W2x.*`, whose `c` and `d` parameters give both limits.
#' @order 9
#'
#' @export
toxval_predict.list <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  n_boot = 1000,
  seed = NULL
) {
  chk_dots_unused(..., source = "list of `drc` fits")
  chk::chk_whole_number(n_boot)
  chk::chk_gt(n_boot, 0)
  chk::chk_null_or(seed, vld = chk::vld_whole_number)
  if (length(object) == 0L) {
    chk::abort_chk("`object` must be a non-empty list of `drc` fits.")
  }
  is_drc <- vapply(object, inherits, logical(1), "drc")
  if (!all(is_drc)) {
    i <- which(!is_drc)[1]
    chk::abort_chk(
      "Every element of `object` must be a `drc` fit; element ",
      i,
      " has class ",
      chk::cc(class(object[[i]])[1]),
      "."
    )
  }

  first <- object[[1]]
  fields <- first$dataList$names
  for (i in seq_along(object)[-1]) {
    fit <- object[[i]]
    other <- fit$dataList$names
    for (field in c("orName", "dName", "cNames")) {
      if (!identical(fields[[field]], other[[field]])) {
        label <- switch(
          field,
          orName = "response",
          dName = "predictor",
          cNames = "curve identifier"
        )
        chk::abort_chk("All `drc` fits must use the same ", label, ".")
      }
    }
    if (
      !identical(
        as.character(group_levels(first$dataList$curveid)),
        as.character(group_levels(fit$dataList$curveid))
      )
    ) {
      chk::abort_chk(
        "All `drc` fits must have the same curve identifier levels."
      )
    }
    if (!identical(first$type, fit$type)) {
      chk::abort_chk("All `drc` fits must use the same response distribution.")
    }
    if (!identical(first$data, fit$data)) {
      chk::abort_chk("All `drc` fits must use the same data.")
    }
  }

  model_set <- names(object)
  if (is.null(model_set)) {
    model_set <- vapply(object, drc_equation_name, character(1))
  } else if (anyNA(model_set) || any(!nzchar(model_set))) {
    chk::abort_chk("`object` must have non-empty names for every fit.")
  }
  if (anyDuplicated(model_set)) {
    chk::abort_chk("The `drc` fits must have unique model names.")
  }
  names(object) <- model_set
  has_asymptote <- all(vapply(
    object,
    function(fit) {
      fct <- fit$fct
      (!is.null(fct$lowerAs) && !is.null(fct$upperAs)) ||
        drc_weibull_without_accessors(fct)
    },
    logical(1)
  ))

  dose_var <- fields$dName
  if (is.null(x_var)) {
    x_var <- dose_var
  } else if (x_var != dose_var) {
    chk::abort_chk(
      "`x_var` must name the predictor of the `drc` fits, ",
      chk::cc(dose_var),
      "."
    )
  }
  if (!is.null(group_var)) {
    chk_var_in_data(group_var, first$data, "group_var")
  }

  aic <- vapply(object, stats::AIC, numeric(1))
  if (any(!is.finite(aic))) {
    chk::abort_chk("Every `drc` fit must have a finite AIC.")
  }
  relative <- exp(-0.5 * (aic - min(aic)))
  weights <- relative / sum(relative)
  counts <- floor(n_boot * weights)
  remainder <- n_boot - sum(counts)
  if (remainder > 0) {
    fractional <- n_boot * weights - counts
    order_fractional <- order(-fractional, seq_along(fractional))
    counts[order_fractional[seq_len(remainder)]] <-
      counts[order_fractional[seq_len(remainder)]] + 1L
  }

  x_vec <- predict_grid(x_range, first$data[[x_var]], resolution, x_control)
  draw_one <- function() {
    lapply(seq_along(object), function(i) {
      fit <- object[[i]]
      parms <- boot_parms(fit, n_boot, log_scale = drc_log_parms(fit))
      control <- drc_curves(fit, x_vec[1], group_var, n_boot, parms = parms)
      component <- if (counts[[i]] > 0) {
        drc_curves(
          fit,
          x_vec,
          group_var,
          counts[[i]],
          parms = parms[seq_len(counts[[i]]), , drop = FALSE]
        )
      }
      list(
        control = lapply(control$curves, function(curve) unname(curve[, 1])),
        component = component
      )
    })
  }
  drawn <- if (is.null(seed)) {
    draw_one()
  } else {
    withr::with_seed(seed, draw_one())
  }
  components <- lapply(drawn[counts > 0], `[[`, "component")
  drawn_names <- model_set[counts > 0]
  keys <- components[[1]]$keys
  if (
    !all(vapply(components, function(x) identical(x$keys, keys), logical(1)))
  ) {
    chk::abort_chk("All `drc` fits must have the same curve identifier levels.")
  }

  curves <- lapply(seq_len(nrow(keys)), function(i) {
    do.call(rbind, lapply(components, function(x) x$curves[[i]]))
  })
  asymptote <- if (has_asymptote) {
    lapply(seq_len(nrow(keys)), function(i) {
      lapply(directions(), function(direction) {
        unlist(
          lapply(components, function(x) x$asymptote[[i]][[direction]]),
          use.names = FALSE
        )
      }) |>
        stats::setNames(directions())
    })
  }
  model_control <- lapply(seq_len(nrow(keys)), function(i) {
    stats::setNames(
      lapply(drawn, function(x) x$control[[i]]),
      model_set
    )
  })
  chk_defined_grid(curves, x_vec)

  meta <- build_meta(
    object = first,
    x_vec = x_vec,
    x_var = x_var,
    curves = curves,
    group_var = group_var,
    realisation = "bootstrap",
    family = first$type,
    seed = seed,
    model_weights = weights,
    draw_model = rep(drawn_names, counts[counts > 0]),
    model_control = model_control
  )
  meta$source_class <- "drc_set"

  new_toxval_pred(
    curves = curves,
    keys = keys,
    x_vec = x_vec,
    asymptote = asymptote,
    meta = meta
  )
}

#' Read the equation name stored in a drc fitting call
#'
#' `drc` may store `"::"` as `fct$name` when the constructor was namespaced.
#'
#' @param fit A `drc` fit.
#'
#' @return The equation name, as a string.
#'
#' @noRd
drc_equation_name <- function(fit) {
  fct_call <- fit$call$fct
  if (is.call(fct_call)) {
    constructor <- fct_call[[1]]
    if (is.call(constructor) && identical(constructor[[1]], as.name("::"))) {
      return(as.character(constructor[[3]]))
    }
    if (is.symbol(constructor)) {
      return(as.character(constructor))
    }
  }
  if (!is.null(fit$fct$name) && !identical(fit$fct$name, "::")) {
    return(fit$fct$name)
  }
  chk::abort_chk(
    "The equation name cannot be read from an unnamed `drc` fit; supply ",
    "names for `object`."
  )
}
