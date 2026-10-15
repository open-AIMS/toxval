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

# The keys of an ungrouped fit when called with no arguments, otherwise a data
# frame of the descriptor columns supplied
pred_keys <- function(...) {
  if (length(list(...)) == 0) {
    return(data.frame(row.names = 1L))
  }
  data.frame(...)
}

pred_curve <- function(nrow = 2, ncol = 4) {
  matrix(as.double(seq_len(nrow * ncol)), nrow = nrow)
}

test_that("new_toxval_pred returns the documented slots for an ungrouped fit", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    keys = pred_keys(),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_s3_class(result, "toxval_pred")
  expect_named(
    result,
    c("curves", "keys", "x_vec", "threshold", "control", "meta")
  )
  expect_length(result$curves, 1)
  expect_equal(dim(result$keys), c(1L, 0L))
  expect_identical(result$x_vec, c(0, 1, 2, 3))
  expect_null(result$threshold)
  expect_null(result$control)
})

test_that("new_toxval_pred keeps the keys of a grouped fit", {
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve()),
    keys = pred_keys(group = factor(c("A", "B"))),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site")
  )

  expect_equal(result$keys, data.frame(group = factor(c("A", "B"))))
  expect_identical(result$meta$group_var, "site")
})

test_that("new_toxval_pred keeps the keys of a multivariate fit", {
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve()),
    keys = pred_keys(response = factor(c("growth", "survival"))),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(multi_var = "endpoint")
  )

  expect_equal(
    result$keys,
    data.frame(response = factor(c("growth", "survival")))
  )
  expect_identical(result$meta$multi_var, "endpoint")
})

test_that("new_toxval_pred keeps the keys of a grouped multivariate fit", {
  # REVIEW-TEXT remove comment if additional tests to toxval_predict are added
  # no toxval_predict() method produces this yet, so it is built by hand to
  # show the contract can hold one curve per combination of group and response
  keys <- pred_keys(
    group = factor(c("A", "A", "B", "B")),
    response = factor(c("growth", "survival", "growth", "survival"))
  )
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve(), pred_curve(), pred_curve()),
    keys = keys,
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site", multi_var = "endpoint")
  )

  expect_length(result$curves, 4)
  expect_equal(result$keys, keys)
  expect_identical(result$meta$group_var, "site")
  expect_identical(result$meta$multi_var, "endpoint")
})

test_that("new_toxval_pred accepts a threshold and a control", {
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve()),
    keys = pred_keys(group = factor(c("A", "B"))),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site"),
    threshold = list(c(1.5, 1.7), c(2.1, 2.4)),
    control = list(c(10.2, 10.6), c(9.4, 9.9))
  )

  expect_equal(result$threshold[[1]], c(1.5, 1.7))
  expect_equal(result$control[[1]], c(10.2, 10.6))

  expect_equal(result$threshold[[2]], c(2.1, 2.4))
  expect_equal(result$control[[2]], c(9.4, 9.9))
})

test_that("new_toxval_pred accepts extra meta elements", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    keys = pred_keys(),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(dpar = "hu")
  )

  expect_identical(result$meta$dpar, "hu")
})

test_that("new_toxval_pred accepts a bootstrap source with a seed", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    keys = pred_keys(),
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
    keys = pred_keys(),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_identical(result$curves[[1]][1, 2], NA_real_)
})

test_that("new_toxval_pred errors when a required meta element is absent", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()[-4]
    ),
    regexp = "Names of `meta` must include 'multi_var'"
  )
})

test_that("new_toxval_pred errors on an unknown realisation source", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(realisation = "posterior")
    ),
    regexp = "`meta\\$realisation` must match 'bootstrap' or 'draws'"
  )
})

test_that("new_toxval_pred errors when x_range is reversed", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(x_range = c(3, 0))
    ),
    regexp = "`meta\\$x_range` must be sorted"
  )
})

test_that("new_toxval_pred errors when x_range is missing a value", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(x_range = c(0, NA))
    ),
    regexp = "`meta\\$x_range` must not have any missing values"
  )
})

test_that("new_toxval_pred errors when x_range is not the range of x_vec", {
  # `resolution` and `x_range` are reported as the settings the grid was built
  # from, so they cannot be allowed to describe a different grid
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(x_range = c(100, 200))
    ),
    regexp = "`meta\\$x_range` must be the range of `x_vec` \\(0 to 3\\), not 100 to 200"
  )
})

test_that("new_toxval_pred errors when x_range is not finite", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(x_range = c(0, Inf))
    ),
    regexp = "`meta\\$x_range` must be the range of `x_vec`"
  )
})

test_that("new_toxval_pred errors when resolution is not the length of x_vec", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(resolution = 1000)
    ),
    regexp = "`meta\\$resolution` must be the number of values in `x_vec` \\(4\\), not 1000"
  )
})

test_that("new_toxval_pred accepts an integer x_range against a double x_vec", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    keys = pred_keys(),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(x_range = c(0L, 3L))
  )

  expect_identical(result$meta$x_range, c(0L, 3L))
})

test_that("new_toxval_pred errors when keys is not a data frame", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = list(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`keys` must be a data.frame"
  )
})

test_that("new_toxval_pred errors when keys has a column other than group or response", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(site = factor("A")),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "Names of `keys` must match 'group' or 'response'"
  )
})

test_that("new_toxval_pred errors when keys repeats a column", {
  keys <- data.frame(
    group = factor("A"),
    group = factor("B"),
    check.names = FALSE
  )

  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = keys,
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "Names of `keys` must be unique"
  )
})

test_that("new_toxval_pred errors when keys has no rows", {
  expect_error(
    new_toxval_pred(
      curves = list(),
      keys = pred_keys(group = factor(character())),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "`keys` must have at least 1 row"
  )
})

test_that("new_toxval_pred errors when keys has no columns and more than one row", {
  # without a descriptor column there is nothing to tell two curves apart
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = data.frame(row.names = 1:2),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`keys` must have 1 row when it has no columns, not 2"
  )
})

test_that("new_toxval_pred errors when keys has a missing value", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = factor(c("A", NA))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "`keys` must not have any missing values"
  )
})

test_that("new_toxval_pred errors when two rows of keys are the same", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = factor(c("A", "A"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "Rows of `keys` must be unique"
  )
})

test_that("new_toxval_pred accepts a repeated group when the responses differ", {
  # uniqueness is of the row, not of each column
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve()),
    keys = pred_keys(
      group = factor(c("A", "A")),
      response = factor(c("growth", "survival"))
    ),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site", multi_var = "endpoint")
  )

  expect_equal(nrow(result$keys), 2)
})

test_that("new_toxval_pred accepts a group and a response with the same values", {
  # a site called "A" and a response called "A" are told apart by their column
  keys <- pred_keys(
    group = factor(c("A", "B")),
    response = factor(c("A", "B"))
  )
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve()),
    keys = keys,
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site", multi_var = "endpoint")
  )

  expect_equal(result$keys, keys)
})

test_that("new_toxval_pred accepts rows that would collide if the columns were pasted", {
  # both rows paste to "ABC", so a check on pasted identifiers would reject
  # this; the columns are compared separately instead
  keys <- pred_keys(
    group = factor(c("A", "AB")),
    response = factor(c("BC", "C"))
  )
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve()),
    keys = keys,
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site", multi_var = "endpoint")
  )

  expect_equal(result$keys, keys)
})

test_that("new_toxval_pred errors when the group column is not a factor", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = c("A", "B")),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "`keys\\$group` must be factor"
  )
})

test_that("new_toxval_pred errors when the response column is not a factor", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(response = c("growth", "survival")),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(multi_var = "endpoint")
    ),
    regexp = "`keys\\$response` must be factor"
  )
})

test_that("new_toxval_pred errors when the levels of keys do not follow its rows", {
  # `arrange()` and plotting order a factor by its levels, so levels in another
  # order, or a level with no curve, would disagree with the curves
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = factor(c("A", "B"), levels = c("B", "A"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "Levels of `keys\\$group` must be its values in order of first appearance"
  )
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(
        response = factor(
          c("growth", "survival"),
          levels = c("growth", "survival", "mortality")
        )
      ),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(multi_var = "endpoint")
    ),
    regexp = "Levels of `keys\\$response` must be its values in order of first appearance"
  )
})

test_that("new_toxval_pred errors when a grouped fit has no group_var", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(group = factor("A")),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`meta\\$group_var` must be supplied when `keys` has a 'group' column"
  )
})

test_that("new_toxval_pred errors when a multivariate fit has no multi_var", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(response = factor("growth")),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`meta\\$multi_var` must be supplied when `keys` has a 'response' column"
  )
})

test_that("new_toxval_pred errors when an ungrouped fit names a group_var", {
  # `keys` and `group_var` record one fact, so an object cannot call itself
  # ungrouped and name a grouping variable
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "`meta\\$group_var` must be NULL when `keys` has no 'group' column"
  )
})

test_that("new_toxval_pred errors when a univariate fit names a multi_var", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = factor(c("A", "B"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site", multi_var = "endpoint")
    ),
    regexp = "`meta\\$multi_var` must be NULL when `keys` has no 'response' column"
  )
})

test_that("new_toxval_pred errors when an ungrouped fit has more than one curve", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`curves` must have 1 element to match the rows of `keys`, not 2"
  )
})

test_that("new_toxval_pred errors when a grouped fit is missing a curve", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(group = factor(c("A", "B"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "`curves` must have 2 elements to match the rows of `keys`, not 1"
  )
})

test_that("new_toxval_pred errors when curves is named", {
  # a name would be a second record of which curve is which, able to disagree
  # with `keys`
  expect_error(
    new_toxval_pred(
      curves = list(A = pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta()
    ),
    regexp = "`curves` must be unnamed"
  )
})

test_that("new_toxval_pred errors when a curve is not a matrix", {
  expect_error(
    new_toxval_pred(
      curves = list(c(1, 2, 3, 4)),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(n_realisation = 1)
    ),
    regexp = "`curves\\[\\[1\\]\\]` must be a matrix"
  )
})

test_that("new_toxval_pred errors when a curve has the wrong number of rows", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(nrow = 3)),
      keys = pred_keys(),
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
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(n_realisation = 1)
    ),
    regexp = "`curves\\[\\[1\\]\\]` must have 1 row to match `meta\\$n_realisation`, not 2"
  )
})

test_that("new_toxval_pred errors when curves have different numbers of rows", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve(nrow = 3)),
      keys = pred_keys(group = factor(c("A", "B"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site")
    ),
    regexp = "`curves\\[\\[2\\]\\]` must have 2 rows"
  )
})

test_that("new_toxval_pred errors when a curve does not match the grid", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(ncol = 3)),
      keys = pred_keys(),
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
      keys = pred_keys(),
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
      keys = pred_keys(),
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
      keys = pred_keys(),
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
      keys = pred_keys(),
      x_vec = 0,
      meta = pred_meta()
    ),
    regexp = "`x_vec` must have at least 2 values"
  )
})

test_that("new_toxval_pred errors when threshold is named", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = factor(c("A", "B"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site"),
      threshold = list(A = c(1.5, 1.7), B = c(2.1, 2.4))
    ),
    regexp = "`threshold` must be unnamed"
  )
})

test_that("new_toxval_pred errors when threshold has more elements than curves", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      threshold = list(c(1.5, 1.7), c(2.1, 2.4))
    ),
    regexp = "`threshold` must have 1 element to match the rows of `keys`, not 2"
  )
})

test_that("new_toxval_pred errors when threshold has the wrong length", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
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
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      control = list(c(10.2, 10.6, 10.9))
    ),
    regexp = "`control\\[\\[1\\]\\]` must be length 2"
  )
})

test_that("new_toxval_pred errors when control is a bare vector", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      control = c(10.2, 10.6)
    ),
    regexp = "`control` must be a list"
  )
})

test_that("new_toxval_pred errors when control is named", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve(), pred_curve()),
      keys = pred_keys(group = factor(c("A", "B"))),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(group_var = "site"),
      control = list(A = c(10.2, 10.6), B = c(9.4, 9.9))
    ),
    regexp = "`control` must be unnamed"
  )
})

test_that("new_toxval_pred errors when control has more elements than curves", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
      x_vec = c(0, 1, 2, 3),
      meta = pred_meta(),
      control = list(c(10.2, 10.6), c(9.4, 9.9))
    ),
    regexp = "`control` must have 1 element to match the rows of `keys`, not 2"
  )
})

test_that("new_toxval_pred errors on a fractional seed", {
  expect_error(
    new_toxval_pred(
      curves = list(pred_curve()),
      keys = pred_keys(),
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
    keys = pred_keys(),
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
    curves = list(pred_curve(), pred_curve()),
    keys = pred_keys(group = factor(c("A", "B"))),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site"),
    threshold = list(c(1.5, 1.7), c(2.1, 2.4)),
    control = list(c(10.2, 10.6), c(9.4, 9.9))
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
    curves = list(pred_curve(), pred_curve()),
    keys = pred_keys(response = factor(c("growth", "survival"))),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(multi_var = "endpoint")
  )

  expect_identical(
    capture.output(print(result))[4],
    "  curves:       2 by endpoint: growth, survival"
  )
})

test_that("print.toxval_pred names the groups and responses of a grouped multivariate fit", {
  result <- new_toxval_pred(
    curves = list(pred_curve(), pred_curve(), pred_curve(), pred_curve()),
    keys = pred_keys(
      group = factor(c("A", "A", "B", "B")),
      response = factor(c("growth", "survival", "growth", "survival"))
    ),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta(group_var = "site", multi_var = "endpoint")
  )

  expect_identical(
    capture.output(print(result))[4],
    "  curves:       4 by site: A, B; endpoint: growth, survival"
  )
})

test_that("print.toxval_pred names the bootstrap source", {
  result <- new_toxval_pred(
    curves = list(pred_curve()),
    keys = pred_keys(),
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
    keys = pred_keys(),
    x_vec = c(0, 1, 2, 3),
    meta = pred_meta()
  )

  expect_output(expect_invisible(print(result)))
})
