# new_toxval_pred -----------------------------------------------------------

# A valid ungrouped meta list, with any element overridden by `...`. Every test
# below starts from a valid object and breaks one thing, so that the assertion
# names the rule being tested rather than whatever fails first.
pred_meta <- function(...) {
  meta <- list(
    source_class = "brmsfit",
    x_var = "x",
    group_var = NULL,
    multi_var = NULL,
    dimension = "none",
    resolution = 4,
    x_range = c(0, 3),
    family = "gaussian",
    realisation = "draws",
    n_realisation = 2,
    seed = NULL
  )
  changes <- list(...)
  meta[names(changes)] <- changes
  meta
}

pred_curve <- function(nrow = 2, ncol = 4) {
  matrix(as.double(seq_len(nrow * ncol)), nrow = nrow)
}

test_that("new_toxval_pred returns the documented slots for an ungrouped fit", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_s3_class(result, "toxval_pred")
  expect_named(result, c("curves", "x_vec", "threshold", "control", "meta"))
  expect_length(result$curves, 1)
  expect_null(names(result$curves))
  expect_identical(result$x_vec, c(0, 1, 2, 3))
  expect_null(result$threshold)
  expect_null(result$control)
  expect_identical(result$meta$dimension, "none")
})

test_that("new_toxval_pred keeps the names of a grouped fit", {
  result <- new_toxval_pred(
    curves = list(A = pred_curve(), B = pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dimension = "group", group_var = "site")
  )

  expect_named(result$curves, c("A", "B"))
  expect_identical(result$meta$group_var, "site")
})

test_that("new_toxval_pred keeps the names of a multivariate fit", {
  result <- new_toxval_pred(
    curves = list(growth = pred_curve(), survival = pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dimension = "response", multi_var = "endpoint")
  )

  expect_named(result$curves, c("growth", "survival"))
  expect_identical(result$meta$multi_var, "endpoint")
})

test_that("new_toxval_pred accepts a threshold and a control", {
  result <- new_toxval_pred(
    curves = list(A = pred_curve(), B = pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dimension = "group", group_var = "site"),
    threshold = list(A = c(1.5, 1.7), B = c(2.1, 2.4)),
    control = c(10.2, 10.6)
  )

  expect_named(result$threshold, c("A", "B"))
  expect_equal(result$threshold$A, c(1.5, 1.7))
  expect_equal(result$control, c(10.2, 10.6))
})

test_that("new_toxval_pred accepts extra meta elements", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dpar = "hu")
  )

  expect_identical(result$meta$dpar, "hu")
})

test_that("new_toxval_pred accepts a bootstrap source with a seed", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(
      source_class = "drc",
      realisation = "bootstrap",
      seed = 42,
      family = NULL
    )
  )

  expect_identical(result$meta$realisation, "bootstrap")
  expect_equal(result$meta$seed, 42)
  expect_null(result$meta$family)
})

test_that("new_toxval_pred accepts NA and Inf in a curve", {
  # a realisation that failed to predict is carried through to find_crossings(),
  # which returns NA for that draw rather than erroring
  curve <- pred_curve()
  curve[1, 2] <- NA
  curve[2, 3] <- Inf

  result <- new_toxval_pred(
    curves = list(curve),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_identical(result$curves[[1]][1, 2], NA_real_)
})

test_that("new_toxval_pred errors when a required meta element is absent", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()[-4]
    ),
    regexp = "Names of `meta` must include 'multi_var'"
  )
})

test_that("new_toxval_pred errors on an unknown dimension", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "site")
    ),
    regexp = "`meta\\$dimension` must match 'group', 'none' or 'response'"
  )
})

test_that("new_toxval_pred errors on an unknown realisation source", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(realisation = "posterior")
    ),
    regexp = "`meta\\$realisation` must match 'bootstrap' or 'draws'"
  )
})

test_that("new_toxval_pred errors when a grouped fit has no group_var", {
  expect_error(
    new_toxval_pred(
      curves = list(A = pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group")
    ),
    regexp = "`meta\\$group_var` must be supplied"
  )
})

test_that("new_toxval_pred errors when a multivariate fit has no multi_var", {
  expect_error(
    new_toxval_pred(
      curves = list(growth = pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "response")
    ),
    regexp = "`meta\\$multi_var` must be supplied"
  )
})

test_that("new_toxval_pred errors when an ungrouped fit has more than one curve", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`curves` must have 1 element"
  )
})

test_that("new_toxval_pred errors when an ungrouped curve is named", {
  # names are carried only where they mean something; a name on an ungrouped
  # fit would reach the result as a `group` value that does not exist
  expect_error(
    new_toxval_pred(
      curves = list(A = pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`curves` must be unnamed"
  )
})

test_that("new_toxval_pred errors when a grouped curve is unnamed", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group", group_var = "site")
    ),
    regexp = "`curves` must be named"
  )
})

test_that("new_toxval_pred errors on duplicate curve names", {
  expect_error(
    new_toxval_pred(
      curves = list(A = pred_curve(), A = pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group", group_var = "site")
    ),
    regexp = "Names of `curves` must be unique"
  )
})

test_that("new_toxval_pred errors when only some curves are named", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), B = pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group", group_var = "site")
    ),
    regexp = "Names of `curves` must not be missing or empty"
  )
})

test_that("new_toxval_pred errors when a curve name is NA", {
  curves <- list(A = pred_curve(), B = pred_curve())
  names(curves) <- c(NA, "B")

  expect_error(
    new_toxval_pred(
      curves = curves,
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group", group_var = "site")
    ),
    regexp = "Names of `curves` must not be missing or empty"
  )
})

test_that("new_toxval_pred errors when a curve is not a matrix", {
  expect_error(
    new_toxval_pred(
      curves = list(c(1, 2, 3, 4)),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(n_realisation = 1)
    ),
    regexp = "`curves\\[\\[1\\]\\]` must be a matrix"
  )
})

test_that("new_toxval_pred errors when a curve has the wrong number of rows", {
  # the alignment invariant is not checkable, but a curve of the wrong height
  # is the conformability failure it shows up as
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(nrow = 3)),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`curves\\[\\[1\\]\\]` must have 2 rows to match `meta\\$n_realisation`, not 3"
  )
})

test_that("new_toxval_pred errors when a single-realisation curve has extra rows", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(nrow = 2)),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(n_realisation = 1)
    ),
    regexp = "`curves\\[\\[1\\]\\]` must have 1 row to match `meta\\$n_realisation`, not 2"
  )
})

test_that("new_toxval_pred errors when curves have different numbers of rows", {
  expect_error(
    new_toxval_pred(
      curves = list(A = pred_curve(), B = pred_curve(nrow = 3)),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group", group_var = "site")
    ),
    regexp = "`curves\\$B` must have 2 rows"
  )
})

test_that("new_toxval_pred errors when a curve does not match the grid", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(ncol = 3)),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`curves\\[\\[1\\]\\]` must have 4 columns to match `x_vec`, not 3"
  )
})

test_that("new_toxval_pred errors when x_vec is not sorted", {
  # crossings are found between adjacent grid points, so an unordered grid
  # returns a wrong value rather than an error
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 2, 1, 3),
      meta = pred_meta()
    ),
    regexp = "`x_vec` must be sorted"
  )
})

test_that("new_toxval_pred errors when x_vec repeats a value", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 1, 3),
      meta = pred_meta()
    ),
    regexp = "`x_vec` must be unique"
  )
})

test_that("new_toxval_pred errors when x_vec is not finite", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, Inf),
      meta = pred_meta()
    ),
    regexp = "`x_vec` must be finite"
  )
})

test_that("new_toxval_pred errors when x_vec has a single value", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(ncol = 1)),
      x_vec = 0,
      meta = pred_meta()
    ),
    regexp = "`x_vec` must have at least 2 values"
  )
})

test_that("new_toxval_pred errors when threshold names do not match curves", {
  expect_error(
    new_toxval_pred(
      curves = list(A = pred_curve(), B = pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(dimension = "group", group_var = "site"),
      threshold = list(A = c(1.5, 1.7), C = c(2.1, 2.4))
    ),
    regexp = "Names of `threshold` must be identical to names of `curves`"
  )
})

test_that("new_toxval_pred errors when threshold has more elements than curves", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      threshold = list(c(1.5, 1.7), c(2.1, 2.4))
    ),
    regexp = "`threshold` must have 1 element to match `curves`, not 2"
  )
})

test_that("new_toxval_pred errors when threshold has the wrong length", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      threshold = list(c(1.5, 1.7, 1.9))
    ),
    regexp = "`threshold\\[\\[1\\]\\]` must be length 2"
  )
})

test_that("new_toxval_pred errors when control has the wrong length", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      control = c(10.2, 10.6, 10.9)
    ),
    regexp = "`control` must be length 2"
  )
})

test_that("new_toxval_pred errors on a fractional seed", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(realisation = "bootstrap", seed = 1.5)
    ),
    regexp = "`meta\\$seed` must be a whole number"
  )
})


# print.toxval_pred ---------------------------------------------------------

test_that("print.toxval_pred describes an ungrouped fit", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_identical(
    capture.output(print(result)),
    c(
      "<toxval_pred> brmsfit",
      "  realisations: 2 (draws)",
      "  grid:         x, 4 values from 0 to 3",
      "  curves:       1 (ungrouped)",
      "  threshold:    none",
      "  control:      none"
    )
  )
})

test_that("print.toxval_pred names the groups of a grouped fit", {
  result <- new_toxval_pred(
    curves = list(A = pred_curve(), B = pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dimension = "group", group_var = "site"),
    threshold = list(A = c(1.5, 1.7), B = c(2.1, 2.4)),
    control = c(10.2, 10.6)
  )

  expect_identical(
    capture.output(print(result)),
    c(
      "<toxval_pred> brmsfit",
      "  realisations: 2 (draws)",
      "  grid:         x, 4 values from 0 to 3",
      "  curves:       2 by site: A, B",
      "  threshold:    present",
      "  control:      present"
    )
  )
})

test_that("print.toxval_pred names the responses of a multivariate fit", {
  result <- new_toxval_pred(
    curves = list(growth = pred_curve(), survival = pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dimension = "response", multi_var = "endpoint")
  )

  expect_identical(
    capture.output(print(result))[4],
    "  curves:       2 by endpoint: growth, survival"
  )
})

test_that("print.toxval_pred names the bootstrap source", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(
      source_class = "drc",
      realisation = "bootstrap",
      seed = 42,
      family = NULL
    )
  )

  expect_identical(
    capture.output(print(result))[1:2],
    c("<toxval_pred> drc", "  realisations: 2 (bootstrap)")
  )
})

test_that("print.toxval_pred returns its input invisibly", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_output(expect_invisible(print(result)))
})
