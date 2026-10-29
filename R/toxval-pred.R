#' Realisations of a fitted curve over a predictor grid
#'
#' A `toxval_pred` holds realisations of one or more fitted curves (posterior
#' draws for a Bayesian fit, parametric bootstrap replicates for a frequentist
#' one) evaluated over a common predictor grid. It is returned by
#' [toxval_predict()] and can be passed to [ecx()] and [nsec()] in place of
#' the fitted model.
#'
#' `new_toxval_pred()` builds one, and is for developers writing a
#' `toxval_predict()` method for a new model class.
#'
#' @section Identifying curves:
#' Row *i* of `keys` identifies element *i* of `curves`, and of `threshold`,
#' `control` and `asymptote` when they are supplied. The columns of `keys` are
#' drawn from `group` and `response`, and become the descriptor columns of the
#' result:
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
#' Rows of `keys` are unique and contain no missing values.
#'
#' @section Realisation alignment:
#' Realisation *i* is the same posterior draw or bootstrap replicate in every
#' slot: row *i* of every matrix in `curves`, and element *i* of every element
#' of `threshold`, `control` and `asymptote`. `new_toxval_pred()` checks that
#' the dimensions agree with `meta$n_realisation` and `length(x_vec)`, but
#' cannot check that the realisations themselves are aligned.
#'
#' @section Metadata:
#' `meta` must contain the following elements. Additional elements are
#' permitted and ignored.
#'
#' \describe{
#'   \item{`source_class`}{String, the class of the fitted object the
#'     realisations came from.}
#'   \item{`x_var`}{String, the name of the predictor variable.}
#'   \item{`group_var`}{String naming the grouping variable. Required when
#'     `keys` has a `group` column, and `NULL` otherwise.}
#'   \item{`multi_var`}{Character vector of the response names of a
#'     multivariate fit, including every level of `keys$response`. Required
#'     when `keys` has a `response` column, and `NULL` otherwise.}
#'   \item{`resolution`}{Whole number, the number of values in `x_vec`.}
#'   \item{`x_range`}{Numeric of length 2, the range of `x_vec`.}
#'   \item{`x_control`}{Number, the lowest predictor value in the data the
#'     model was fitted to, taken to be the control.}
#'   \item{`family`}{String naming the response distribution, such as the
#'     `brms` family or the `type` of a `drc` fit, or `NULL`. A multivariate
#'     fit whose responses differ in family gives a character vector named by
#'     response. A binomial family is taken to be on the proportion scale.}
#'   \item{`realisation`}{Either `"draws"` or `"bootstrap"`.}
#'   \item{`n_realisation`}{Whole number, the number of realisations.}
#'   \item{`seed`}{Whole number, the seed used to generate bootstrap
#'     realisations, or `NULL`.}
#' }
#'
#' @aliases toxval_pred
#'
#' @param curves An unnamed list of numeric matrices, one element per row of
#'   `keys`. Each matrix has one row per realisation and one column per value
#'   of `x_vec`.
#' @param keys A data frame identifying the curves, one row per element of
#'   `curves`. See Identifying curves.
#' @param x_vec A numeric vector of at least two finite, strictly increasing
#'   predictor values.
#' @param meta A named list of metadata. See Metadata.
#' @param threshold An unnamed list of numeric vectors of per-realisation
#'   values of a threshold parameter such as an NEC, one element per row of
#'   `keys`, or `NULL` where the model has none.
#' @param control An unnamed list of numeric vectors of per-realisation control
#'   responses from a separate control-only fit, one element per row of
#'   `keys`, or `NULL`.
#' @param asymptote An unnamed list, one element per row of `keys`, of lists
#'   of two numeric vectors of per-realisation asymptotes named `decreasing`
#'   (the lower asymptote) and `increasing` (the upper one), or `NULL` where
#'   the model supplies none. Used by `type = "relative"`.
#'
#' @return An object of class `toxval_pred`, a list with elements `curves`,
#'   `keys`, `x_vec`, `threshold`, `control`, `asymptote` and `meta`.
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
#' @keywords internal
#' @export
new_toxval_pred <- function(
  curves,
  keys,
  x_vec,
  meta,
  threshold = NULL,
  control = NULL,
  asymptote = NULL
) {
  x <- structure(
    list(
      curves = curves,
      keys = keys,
      x_vec = x_vec,
      threshold = threshold,
      control = control,
      asymptote = asymptote,
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
  if (!is.null(x$asymptote)) {
    chk_asymptote(x$asymptote, x$keys, x$meta)
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
  if (!is.null(meta$multi_var)) {
    chk_names_vector(meta$multi_var, "`meta$multi_var`")
    chk::chk_unique(meta$multi_var, x_name = "`meta$multi_var`")
  }

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

  if (!is.null(meta$family)) {
    chk_names_vector(meta$family, "`meta$family`")
    if (length(meta$family) > 1) {
      chk::chk_named(meta$family, x_name = "`meta$family`")
    }
  }

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
  if ("response" %in% names(keys)) {
    chk::chk_subset(
      levels(keys$response),
      meta$multi_var,
      x_name = "levels of `keys$response`"
    )
  }
  chk_family_keys(meta$family, keys)
  invisible(keys)
}

#' Check that `meta$family` gives a family for every curve
#'
#' A family is one string for every curve, or, for a multivariate fit whose
#' responses differ in family, a vector named by response.
#'
#' @noRd
chk_family_keys <- function(family, keys) {
  if (length(family) <= 1 && is.null(names(family))) {
    return(invisible(family))
  }
  if (!"response" %in% names(keys)) {
    chk::abort_chk(
      "`meta$family` must be a single unnamed string when `keys` has no ",
      "'response' column."
    )
  }
  chk::chk_subset(
    levels(keys$response),
    names(family),
    x_name = "levels of `keys$response`"
  )
  invisible(family)
}

#' @noRd
chk_names_vector <- function(x, x_name) {
  chk::chk_character(x, x_name = x_name)
  chk::chk_not_any_na(x, x_name = x_name)
  if (length(x) == 0) {
    chk::abort_chk(x_name, " must not be empty.")
  }
  invisible(x)
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

#' @noRd
chk_asymptote <- function(asymptote, keys, meta) {
  chk_aligned_shape(asymptote, "asymptote", keys)

  for (i in seq_along(asymptote)) {
    nm <- element_name("asymptote", i)
    chk::chk_list(asymptote[[i]], x_name = nm)
    if (!identical(names(asymptote[[i]]), c("decreasing", "increasing"))) {
      chk::abort_chk(
        nm,
        " must be a list named 'decreasing' and 'increasing'."
      )
    }
    for (direction in names(asymptote[[i]])) {
      element <- paste0("`asymptote[[", i, "]]$", direction, "`")
      values <- asymptote[[i]][[direction]]
      chk::chk_numeric(values, x_name = element)
      chk::chk_vector(values, x_name = element)
      chk::chk_length(
        values,
        as.integer(meta$n_realisation),
        x_name = element
      )
    }
  }
  invisible(asymptote)
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
#' @export
#' @noRd
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
  # `multi_var` holds the response names themselves, so the column is labelled
  # by its own name
  vars <- c(group = meta$group_var, response = "response")
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
