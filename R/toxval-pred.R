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
#'   \item{`curves`}{An unnamed list of numeric matrices, each `n_realisation`
#'     rows by `length(x_vec)` columns, holding one realisation of the fitted
#'     curve per row. One element per row of `keys`.}
#'   \item{`keys`}{A data frame identifying the curves, one row per element of
#'     `curves`; see below.}
#'   \item{`x_vec`}{The predictor grid the curves are evaluated over.}
#'   \item{`threshold`}{An unnamed list of per-realisation values of a genuine
#'     threshold parameter such as a NEC, one element per row of `keys`, or
#'     `NULL` where the model has none. Not recoverable from `curves`, which is
#'     why it is carried separately.}
#'   \item{`control`}{An unnamed list of per-realisation control responses from
#'     a control-only fit, estimated independently of the dose-response shape,
#'     one element per row of `keys`, or `NULL`. One element per curve, because
#'     a grouped fit has one control per group.}
#'   \item{`meta`}{Named list of metadata; see below.}
#' }
#'
#' @section Identifying curves:
#' Row *i* of `keys` identifies element *i* of `curves`, and of `threshold` and
#' `control` when they are supplied. The columns of `keys` are drawn from
#' `group` and `response`, and become the descriptor columns of the result:
#'
#' \describe{
#'   \item{Ungrouped}{One row and no columns, such as
#'     `data.frame(row.names = 1L)`.}
#'   \item{Grouped}{A `group` column, a factor with one row per level.}
#'   \item{Multivariate}{A `response` column, a factor with one row per
#'     response.}
#'   \item{Grouped multivariate}{Both columns, with one row per combination of
#'     group and response.}
#' }
#'
#' Rows of `keys` are unique and contain no missing values. A curve is
#' identified by its row rather than by a list name, because a fit that is both
#' grouped and multivariate needs two descriptors per curve.
#'
#' @section Realisation alignment:
#' Realisation *i* is the same underlying draw in every slot: row *i* of every
#' matrix in `curves`, element *i* of every element of `threshold`, and element
#' *i* of every element of `control` all come from the same posterior draw or
#' the same bootstrap replicate. This is what makes per-realisation quantities
#' computed from different slots comparable row by row.
#'
#' The invariant itself cannot be checked from the object -- nothing records
#' which draw a row came from. What `new_toxval_pred()` does check is
#' conformability, which is where a violation shows up: every matrix in `curves`
#' has `meta$n_realisation` rows and `length(x_vec)` columns, and every element
#' of `threshold` and `control` has `meta$n_realisation` values. Supplying
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
#'     `keys` has a `group` column, and `NULL` otherwise.}
#'   \item{`multi_var`}{String naming the response variable of a multivariate
#'     fit. Required when `keys` has a `response` column, and `NULL`
#'     otherwise.}
#'   \item{`resolution`}{Whole number, the number of values in `x_vec`.}
#'   \item{`x_range`}{Numeric of length 2, the range of `x_vec`.}
#'   \item{`x_control`}{Number, the lowest predictor value in the data the
#'     model was fitted to, taken to be the control. It differs from the first
#'     value of `x_vec` when `x_range` is supplied. A realisation whose control
#'     response already lies beyond the reference is placed here.}
#'   \item{`family`}{String naming the response distribution, such as the
#'     `brms` family or the `type` of a `drc` fit, or `NULL` where the fitted
#'     object has none. It decides whether the response is a proportion,
#'     bounded at 1.}
#'   \item{`realisation`}{Either `"draws"` or `"bootstrap"`, the source of the
#'     realisations.}
#'   \item{`n_realisation`}{Whole number, the number of realisations.}
#'   \item{`seed`}{Whole number, the seed used to generate bootstrap
#'     realisations, or `NULL`.}
#' }
#'
#' @aliases toxval_pred
#'
#' @param curves An unnamed list of numeric matrices of realisations, one
#'   element per row of `keys`. See Slots.
#' @param keys A data frame identifying the curves, one row per element of
#'   `curves`. See Identifying curves.
#' @param x_vec A numeric vector of at least two finite, strictly increasing
#'   predictor values.
#' @param meta A named list of metadata. See Metadata.
#' @param threshold An unnamed list of numeric vectors of per-realisation
#'   threshold values, one element per row of `keys`, or `NULL`.
#' @param control An unnamed list of numeric vectors of per-realisation control
#'   values, one element per row of `keys`, or `NULL`.
#'
#' @return An object of class `toxval_pred`.
#'
#' @examples
#' new_toxval_pred(
#'   curves = list(matrix(c(10, 9, 8, 7, 11, 9, 7, 5), nrow = 2, byrow = TRUE)),
#'   keys = data.frame(row.names = 1L),
#'   x_vec = c(0, 1, 2, 3),
#'   meta = list(
#'     source_class = "brmsfit",
#'     x_var = "x",
#'     group_var = NULL,
#'     multi_var = NULL,
#'     resolution = 4,
#'     x_range = c(0, 3),
#'     x_control = 0,
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
  keys,
  x_vec,
  meta,
  threshold = NULL,
  control = NULL
) {
  x <- structure(
    list(
      curves = curves,
      keys = keys,
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
  chk_keys(x$keys, x$meta)
  chk_x_vec(x$x_vec)
  chk_settings(x$x_vec, x$meta)
  chk_curves(x$curves, x$keys, x$x_vec, x$meta)
  if (!is.null(x$threshold)) {
    chk_aligned_list(x$threshold, "threshold", x$keys, x$meta)
  }
  if (!is.null(x$control)) {
    chk_aligned_list(x$control, "control", x$keys, x$meta)
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
    "resolution",
    "x_range",
    "x_control",
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

  chk::chk_whole_number(meta$resolution, x_name = "`meta$resolution`")
  chk::chk_gt(meta$resolution, 1, x_name = "`meta$resolution`")

  chk::chk_numeric(meta$x_range, x_name = "`meta$x_range`")
  chk::chk_length(meta$x_range, 2L, x_name = "`meta$x_range`")
  chk::chk_not_any_na(meta$x_range, x_name = "`meta$x_range`")
  chk::chk_sorted(meta$x_range, x_name = "`meta$x_range`")

  chk::chk_number(meta$x_control, x_name = "`meta$x_control`")
  if (!is.finite(meta$x_control)) {
    chk::abort_chk("`meta$x_control` must be finite.")
  }

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
  invisible(meta)
}

#' @noRd
chk_keys <- function(keys, meta) {
  chk::chk_data(keys, x_name = "`keys`")
  chk::chk_subset(
    names(keys),
    c("group", "response"),
    x_name = "names of `keys`"
  )
  chk::chk_unique(names(keys), x_name = "names of `keys`")

  if (nrow(keys) == 0) {
    chk::abort_chk("`keys` must have at least 1 row.")
  }
  if (ncol(keys) == 0 && nrow(keys) != 1) {
    chk::abort_chk(
      "`keys` must have 1 row when it has no columns, not ",
      nrow(keys),
      "."
    )
  }
  if (anyNA(keys)) {
    chk::abort_chk("`keys` must not have any missing values.")
  }
  if (anyDuplicated(keys) > 0) {
    chk::abort_chk("Rows of `keys` must be unique.")
  }

  if ("group" %in% names(keys)) {
    chk::chk_factor(keys$group, x_name = "`keys$group`")
  }
  if ("response" %in% names(keys)) {
    chk::chk_factor(keys$response, x_name = "`keys$response`")
  }

  # `arrange()` and plotting order a factor by its levels, so the levels must
  # follow the rows of `keys` for the result to keep the curve order
  for (col in names(keys)) {
    if (!identical(levels(keys[[col]]), unique(as.character(keys[[col]])))) {
      chk::abort_chk(
        "Levels of `keys$",
        col,
        "` must be its values in order of first appearance, ",
        "with no unused levels."
      )
    }
  }

  if ("group" %in% names(keys) && is.null(meta$group_var)) {
    chk::abort_chk(
      "`meta$group_var` must be supplied when `keys` has a 'group' column."
    )
  }
  if (!"group" %in% names(keys) && !is.null(meta$group_var)) {
    chk::abort_chk(
      "`meta$group_var` must be NULL when `keys` has no 'group' column."
    )
  }
  if ("response" %in% names(keys) && is.null(meta$multi_var)) {
    chk::abort_chk(
      "`meta$multi_var` must be supplied when `keys` has a 'response' column."
    )
  }
  if (!"response" %in% names(keys) && !is.null(meta$multi_var)) {
    chk::abort_chk(
      "`meta$multi_var` must be NULL when `keys` has no 'response' column."
    )
  }
  invisible(keys)
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
chk_curves <- function(curves, keys, x_vec, meta) {
  chk_aligned_shape(curves, "curves", keys)

  for (i in seq_along(curves)) {
    nm <- element_name("curves", i)
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
chk_aligned_list <- function(x, arg, keys, meta) {
  chk_aligned_shape(x, arg, keys)

  for (i in seq_along(x)) {
    nm <- element_name(arg, i)
    chk::chk_numeric(x[[i]], x_name = nm)
    chk::chk_vector(x[[i]], x_name = nm)
    chk::chk_length(x[[i]], as.integer(meta$n_realisation), x_name = nm)
  }
  invisible(x)
}

#' Check that a slot is an unnamed list with one element per row of `keys`
#'
#' @noRd
chk_aligned_shape <- function(x, arg, keys) {
  chk::chk_list(x, x_name = paste0("`", arg, "`"))
  if (!is.null(names(x))) {
    chk::abort_chk(
      "`",
      arg,
      "` must be unnamed; each element is identified by its row of `keys`."
    )
  }
  if (length(x) != nrow(keys)) {
    chk::abort_chk(
      "`",
      arg,
      "` must have ",
      nrow(keys),
      if (nrow(keys) == 1) " element" else " elements",
      " to match the rows of `keys`, not ",
      length(x),
      "."
    )
  }
  invisible(x)
}

#' @noRd
element_name <- function(arg, i) {
  paste0("`", arg, "[[", i, "]]`")
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
#'   keys = data.frame(row.names = 1L),
#'   x_vec = c(0, 1, 2, 3),
#'   meta = list(
#'     source_class = "brmsfit",
#'     x_var = "x",
#'     group_var = NULL,
#'     multi_var = NULL,
#'     resolution = 4,
#'     x_range = c(0, 3),
#'     x_control = 0,
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
  cat("  curves:       ", print_curves(x$keys, meta), "\n", sep = "")
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
print_curves <- function(keys, meta) {
  if (ncol(keys) == 0) {
    return("1 (ungrouped)")
  }
  vars <- c(group = meta$group_var, response = meta$multi_var)
  descriptors <- vapply(
    names(keys),
    function(col) {
      values <- unique(as.character(keys[[col]]))
      paste0(vars[[col]], ": ", paste(values, collapse = ", "))
    },
    character(1)
  )
  paste0(nrow(keys), " by ", paste(descriptors, collapse = "; "))
}
