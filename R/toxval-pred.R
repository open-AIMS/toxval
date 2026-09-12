#' Realisations of a fitted curve over a predictor grid
#'
#' `toxval_pred` is the intermediate object that every metric function in this
#' package computes on. It holds realisations of a fitted curve -- posterior
#' draws for a Bayesian fit, parametric bootstrap replicates for a frequentist
#' one -- evaluated over a common predictor grid, together with the metadata
#' needed to describe and interpret them.
#'
#' The class is part of the package interface, not an internal convenience. A
#' model class supported from outside this package supplies a `toxval_predict()`
#' method that builds its `toxval_pred` with `new_toxval_pred()`, so the slot
#' contract documented here is what such a method has to satisfy.
#'
#' @section Slots:
#' \describe{
#'   \item{`curves`}{A list of numeric matrices, each `n_realisation` rows by
#'     `length(x_vec)` columns, holding one realisation of the fitted curve per
#'     row. One element per group or per response; a single element when the fit
#'     is ungrouped.}
#'   \item{`x_vec`}{The predictor grid the curves are evaluated over.}
#'   \item{`threshold`}{Per-realisation values of a genuine threshold parameter
#'     such as a NEC, or `NULL` where the model has none. Not recoverable from
#'     `curves`, which is why it is carried separately.}
#'   \item{`control`}{Per-realisation control response from a control-only fit,
#'     estimated independently of the dose-response shape, or `NULL`. Keyed like
#'     `curves`, because a grouped fit has one control per group.}
#'   \item{`meta`}{Named list of metadata; see below.}
#' }
#'
#' @section Naming of `curves`:
#' `curves` is named when the names mean something and unnamed when they do not.
#' For `meta$dimension` of `"group"` or `"response"` the list has one uniquely
#' named element per level or response, and those names become the `group` or
#' `response` column of the result. For `"none"` the list has exactly one
#' element and no names, rather than a placeholder name that a real group could
#' collide with. `threshold`, when supplied, carries the same names in the same
#' order, and so does `control`.
#'
#' @section One descriptor at a time:
#' `dimension` names a single descriptor, so `curves` is keyed by group or by
#' response and never by both: a fit that is both grouped and multivariate has
#' no representation here. No supported fitting path produces one -- `nsec_multi()`
#' takes no `group_var` -- so the limit sits in the contract rather than in the
#' code. Lifting it means keying `curves` by a table of descriptors, one row per
#' curve, rather than by a single dimension, which is a change to this contract
#' rather than an addition to it.
#'
#' @section Realisation alignment:
#' Realisation *i* is the same underlying draw in every slot: row *i* of every
#' matrix in `curves`, element *i* of every element of `threshold`, and element
#' *i* of `control` all come from the same posterior draw or the same bootstrap
#' replicate. This is what makes per-realisation quantities computed from
#' different slots comparable row by row.
#'
#' The invariant itself cannot be checked from the object -- nothing records
#' which draw a row came from. What `new_toxval_pred()` does check is
#' conformability, which is where a violation shows up: every matrix in `curves`
#' has `meta$n_realisation` rows and `length(x_vec)` columns, and every
#' `threshold` element and `control` has `meta$n_realisation` values. Supplying
#' aligned realisations is the caller's responsibility.
#'
#' @section Metadata:
#' `meta` must contain the following elements. Additional elements are permitted
#' and ignored, so that a `toxval_predict()` method defined in another package
#' can record what it needs to.
#'
#' \describe{
#'   \item{`source_class`}{String, the class of the fitted object the
#'     realisations came from.}
#'   \item{`x_var`}{String, the name of the predictor variable.}
#'   \item{`group_var`}{String naming the grouping variable. Required when
#'     `dimension` is `"group"`, and `NULL` when `dimension` is `"none"`.}
#'   \item{`multi_var`}{String naming the response variable of a multivariate
#'     fit. Required when `dimension` is `"response"`, and `NULL` when
#'     `dimension` is `"none"`.}
#'   \item{`dimension`}{One of `"none"`, `"group"` or `"response"`. Determines
#'     whether the result carries a `group` column, a `response` column or
#'     neither, and which of `group_var` and `multi_var` the object must carry.}
#'   \item{`resolution`}{Whole number, the number of values in `x_vec`.}
#'   \item{`x_range`}{Numeric of length 2, the range of `x_vec`.}
#'   \item{`family`}{String naming the response distribution, or `NULL` where
#'     the fitted object has no family.}
#'   \item{`realisation`}{Either `"draws"` or `"bootstrap"`, the source of the
#'     realisations.}
#'   \item{`n_realisation`}{Whole number, the number of realisations.}
#'   \item{`seed`}{Whole number, the seed used to generate bootstrap
#'     realisations, or `NULL`.}
#' }
#'
#' @aliases toxval_pred
#'
#' @param curves A list of numeric matrices of realisations, one element per
#'   group or response. See Slots and Naming of `curves`.
#' @param x_vec A numeric vector of at least two finite, strictly increasing
#'   predictor values.
#' @param meta A named list of metadata. See Metadata.
#' @param threshold A list of numeric vectors of per-realisation threshold
#'   values, named as `curves` is, or `NULL`.
#' @param control A list of numeric vectors of per-realisation control values,
#'   named as `curves` is, or `NULL`.
#'
#' @return An object of class `toxval_pred`.
#'
#' @examples
#' new_toxval_pred(
#'   curves = list(matrix(c(10, 9, 8, 7, 11, 9, 7, 5), nrow = 2, byrow = TRUE)),
#'   x_vec = c(0, 1, 2, 3),
#'   meta = list(
#'     source_class = "brmsfit",
#'     x_var = "x",
#'     group_var = NULL,
#'     multi_var = NULL,
#'     dimension = "none",
#'     resolution = 4,
#'     x_range = c(0, 3),
#'     family = "gaussian",
#'     realisation = "draws",
#'     n_realisation = 2,
#'     seed = NULL
#'   )
#' )
#'
#' @export
new_toxval_pred <- function(
  curves,
  x_vec,
  meta,
  threshold = NULL,
  control = NULL
) {
  x <- structure(
    list(
      curves = curves,
      x_vec = x_vec,
      threshold = threshold,
      control = control,
      meta = meta
    ),
    class = "toxval_pred"
  )
  validate_toxval_pred(x)
  x
}

#' Check that a toxval_pred satisfies its slot contract
#'
#' Called by `new_toxval_pred()`, which is the only way to build the object, so
#' an unvalidated `toxval_pred` cannot be constructed.
#'
#' @param x An object of class `toxval_pred`.
#'
#' @return `x`, invisibly if valid; an error otherwise.
#'
#' @noRd
validate_toxval_pred <- function(x) {
  chk::chk_s3_class(x, "toxval_pred")

  chk_meta(x$meta)
  chk_x_vec(x$x_vec)
  chk_settings(x$x_vec, x$meta)
  chk_curves(x$curves, x$x_vec, x$meta)
  if (!is.null(x$threshold)) {
    chk_aligned_list(x$threshold, "threshold", x$curves, x$meta)
  }
  if (!is.null(x$control)) {
    chk_aligned_list(x$control, "control", x$curves, x$meta)
  }

  invisible(x)
}

#' @noRd
meta_required <- function() {
  c(
    "source_class",
    "x_var",
    "group_var",
    "multi_var",
    "dimension",
    "resolution",
    "x_range",
    "family",
    "realisation",
    "n_realisation",
    "seed"
  )
}

#' @noRd
chk_meta <- function(meta) {
  chk::chk_list(meta)
  chk::chk_named(meta)
  chk::chk_superset(names(meta), meta_required(), x_name = "names of `meta`")

  chk::chk_string(meta$source_class, x_name = "`meta$source_class`")
  chk::chk_string(meta$x_var, x_name = "`meta$x_var`")
  chk::chk_null_or(
    meta$group_var,
    vld = chk::vld_string,
    x_name = "`meta$group_var`"
  )
  chk::chk_null_or(
    meta$multi_var,
    vld = chk::vld_string,
    x_name = "`meta$multi_var`"
  )

  chk::chk_string(meta$dimension, x_name = "`meta$dimension`")
  chk::chk_subset(
    meta$dimension,
    c("none", "group", "response"),
    x_name = "`meta$dimension`"
  )

  chk::chk_whole_number(meta$resolution, x_name = "`meta$resolution`")
  chk::chk_gt(meta$resolution, 1, x_name = "`meta$resolution`")

  chk::chk_numeric(meta$x_range, x_name = "`meta$x_range`")
  chk::chk_length(meta$x_range, 2L, x_name = "`meta$x_range`")
  chk::chk_not_any_na(meta$x_range, x_name = "`meta$x_range`")
  chk::chk_sorted(meta$x_range, x_name = "`meta$x_range`")

  chk::chk_null_or(meta$family, vld = chk::vld_string, x_name = "`meta$family`")

  chk::chk_string(meta$realisation, x_name = "`meta$realisation`")
  chk::chk_subset(
    meta$realisation,
    c("draws", "bootstrap"),
    x_name = "`meta$realisation`"
  )

  chk::chk_whole_number(meta$n_realisation, x_name = "`meta$n_realisation`")
  chk::chk_gt(meta$n_realisation, 0, x_name = "`meta$n_realisation`")

  chk::chk_null_or(
    meta$seed,
    vld = chk::vld_whole_number,
    x_name = "`meta$seed`"
  )

  if (meta$dimension == "group" && is.null(meta$group_var)) {
    chk::abort_chk(
      "`meta$group_var` must be supplied when `meta$dimension` is \"group\"."
    )
  }
  if (meta$dimension == "response" && is.null(meta$multi_var)) {
    chk::abort_chk(
      "`meta$multi_var` must be supplied when `meta$dimension` is \"response\"."
    )
  }
  if (meta$dimension == "group" && !is.null(meta$multi_var)) {
    chk::abort_chk(
      "`meta$multi_var` must be NULL when `meta$dimension` is \"group\"."
    )
  }
  if (meta$dimension == "response" && !is.null(meta$group_var)) {
    chk::abort_chk(
      "`meta$group_var` must be NULL when `meta$dimension` is \"response\"."
    )
  }
  if (meta$dimension == "none" && !is.null(meta$group_var)) {
    chk::abort_chk(
      "`meta$group_var` must be NULL when `meta$dimension` is \"none\"."
    )
  }
  if (meta$dimension == "none" && !is.null(meta$multi_var)) {
    chk::abort_chk(
      "`meta$multi_var` must be NULL when `meta$dimension` is \"none\"."
    )
  }
  invisible(meta)
}

#' @noRd
chk_x_vec <- function(x_vec) {
  chk::chk_numeric(x_vec, x_name = "`x_vec`")
  chk::chk_not_any_na(x_vec, x_name = "`x_vec`")
  if (!all(is.finite(x_vec))) {
    chk::abort_chk("`x_vec` must be finite.")
  }
  if (length(x_vec) < 2) {
    chk::abort_chk("`x_vec` must have at least 2 values.")
  }
  # crossings are found by scanning adjacent grid points and interpolating
  # between them, so an unordered or duplicated grid gives a wrong answer
  # rather than an error
  chk::chk_sorted(x_vec, x_name = "`x_vec`")
  chk::chk_unique(x_vec, x_name = "`x_vec`")
  invisible(x_vec)
}

#' @noRd
chk_settings <- function(x_vec, meta) {
  # `resolution` and `x_range` are the settings the grid was built from and are
  # reported as such, so an object where they disagree with `x_vec` describes
  # itself wrongly
  if (!isTRUE(all.equal(as.numeric(meta$x_range), range(x_vec)))) {
    chk::abort_chk(
      "`meta$x_range` must be the range of `x_vec` (",
      format(min(x_vec)),
      " to ",
      format(max(x_vec)),
      "), not ",
      format(meta$x_range[1]),
      " to ",
      format(meta$x_range[2]),
      "."
    )
  }
  if (meta$resolution != length(x_vec)) {
    chk::abort_chk(
      "`meta$resolution` must be the number of values in `x_vec` (",
      length(x_vec),
      "), not ",
      meta$resolution,
      "."
    )
  }
  invisible(meta)
}

#' @noRd
chk_curves <- function(curves, x_vec, meta) {
  chk::chk_list(curves, x_name = "`curves`")
  chk::chk_not_empty(curves, x_name = "`curves`")

  if (meta$dimension == "none") {
    if (length(curves) != 1) {
      chk::abort_chk(
        "`curves` must have 1 element when `meta$dimension` is \"none\", not ",
        length(curves),
        "."
      )
    }
    if (!is.null(names(curves))) {
      chk::abort_chk(
        "`curves` must be unnamed when `meta$dimension` is \"none\"."
      )
    }
  } else {
    chk::chk_named(curves, x_name = "`curves`")
    chk::chk_unique(names(curves), x_name = "names of `curves`")
    if (any(is.na(names(curves)) | names(curves) == "")) {
      chk::abort_chk("names of `curves` must not be missing or empty.")
    }
  }

  for (i in seq_along(curves)) {
    nm <- element_name(curves, "curves", i)
    chk::chk_matrix(curves[[i]], x_name = nm)
    chk::chk_numeric(curves[[i]], x_name = nm)
    if (nrow(curves[[i]]) != meta$n_realisation) {
      chk::abort_chk(
        nm,
        " must have ",
        meta$n_realisation,
        if (meta$n_realisation == 1) " row" else " rows",
        " to match `meta$n_realisation`, not ",
        nrow(curves[[i]]),
        "."
      )
    }
    if (ncol(curves[[i]]) != length(x_vec)) {
      chk::abort_chk(
        nm,
        " must have ",
        length(x_vec),
        " columns to match `x_vec`, not ",
        ncol(curves[[i]]),
        "."
      )
    }
  }
  invisible(curves)
}

#' @noRd
chk_aligned_list <- function(x, arg, curves, meta) {
  chk::chk_list(x, x_name = paste0("`", arg, "`"))
  if (!identical(names(x), names(curves))) {
    chk::abort_chk(
      "names of `",
      arg,
      "` must be identical to names of `curves`."
    )
  }
  # only reachable for an ungrouped fit: identical non-NULL names force equal
  # lengths, so both name vectors are NULL here and `curves` has one element
  if (length(x) != length(curves)) {
    chk::abort_chk(
      "`",
      arg,
      "` must have 1 element to match `curves`, not ",
      length(x),
      "."
    )
  }
  for (i in seq_along(x)) {
    nm <- element_name(x, arg, i)
    chk::chk_numeric(x[[i]], x_name = nm)
    chk::chk_vector(x[[i]], x_name = nm)
    chk::chk_length(x[[i]], as.integer(meta$n_realisation), x_name = nm)
  }
  invisible(x)
}

#' @noRd
element_name <- function(x, arg, i) {
  if (is.null(names(x))) {
    return(paste0("`", arg, "[[1]]`"))
  }
  paste0("`", arg, "$", names(x)[i], "`")
}

#' Print a toxval_pred
#'
#' @param x An object of class `toxval_pred`.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' pred <- new_toxval_pred(
#'   curves = list(matrix(c(10, 9, 8, 7, 11, 9, 7, 5), nrow = 2, byrow = TRUE)),
#'   x_vec = c(0, 1, 2, 3),
#'   meta = list(
#'     source_class = "brmsfit",
#'     x_var = "x",
#'     group_var = NULL,
#'     multi_var = NULL,
#'     dimension = "none",
#'     resolution = 4,
#'     x_range = c(0, 3),
#'     family = "gaussian",
#'     realisation = "draws",
#'     n_realisation = 2,
#'     seed = NULL
#'   )
#' )
#' print(pred)
#'
#' @export
print.toxval_pred <- function(x, ...) {
  meta <- x$meta
  cat("<toxval_pred> ", meta$source_class, "\n", sep = "")
  cat(
    "  realisations: ",
    meta$n_realisation,
    " (",
    meta$realisation,
    ")\n",
    sep = ""
  )
  cat(
    "  grid:         ",
    meta$x_var,
    ", ",
    length(x$x_vec),
    " values from ",
    format(min(x$x_vec)),
    " to ",
    format(max(x$x_vec)),
    "\n",
    sep = ""
  )
  cat("  curves:       ", print_curves(x$curves, meta), "\n", sep = "")
  cat(
    "  threshold:    ",
    if (is.null(x$threshold)) "none" else "present",
    "\n",
    sep = ""
  )
  cat(
    "  control:      ",
    if (is.null(x$control)) "none" else "present",
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @noRd
print_curves <- function(curves, meta) {
  if (meta$dimension == "none") {
    return("1 (ungrouped)")
  }
  paste0(
    length(curves),
    " by ",
    if (meta$dimension == "group") meta$group_var else meta$multi_var,
    ": ",
    paste(names(curves), collapse = ", ")
  )
}
