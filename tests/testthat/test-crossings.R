# find_crossings -----------------------------------------------------------

test_that("find_crossings returns a list with two values", {
  x_vec <- 0:2
  y <- 2:0
  reference <- 1

  result <- toxval:::find_crossings(y, x_vec, reference)
  expect_named(result, c("decreasing", "increasing"))
  expect_length(result, 2)
  expect_type(result, "list")
  expect_type(result$increasing, "double")
  expect_type(result$decreasing, "double")
})

test_that("find_crossings interpolates a monotone decreasing crossing", {
  x_vec <- 0:10
  y <- 10:0
  reference <- 5.5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 4.5)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings interpolates a monotone increasing crossing", {
  x_vec <- 0:10
  y <- 0:10
  reference <- 5.5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 5.5)
})

test_that("find_crossings interpolates for unevenly spaced x_vec", {
  x_vec <- c(0, 0.1, 3, 7, 8.2)
  y <- c(1.7, 2.3, 9.2, 12, 17.2)
  reference <- 5.5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 1.444928, tolerance = 0.000001)
})

test_that("find_crossings finds both crossings on a hormetic curve", {
  x_vec <- 0:4
  y <- c(4, 6, 8, 6, 4)
  reference <- 5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 3.5)
  expect_equal(result$increasing, 0.5)
})

test_that("find_crossings finds both crossings on a U-shaped curve", {
  x_vec <- 0:4
  y <- c(4, 3, 2, 3, 4)
  reference <- 3.5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 0.5)
  expect_equal(result$increasing, 3.5)
})

test_that("find_crossings returns NA when the curve stays above the reference", {
  x_vec <- 0:5
  y <- rep(7, 6)
  reference <- 4

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings returns NA when the curve stays below the reference", {
  x_vec <- 0:5
  y <- rep(2, 6)
  reference <- 4

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings returns NA when the curve sits exactly on the reference", {
  x_vec <- 0:5
  y <- rep(2, 6)
  reference <- 2

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings when an increasing crossing lands on a point", {
  x_vec <- 0:5
  y <- 0:5
  reference <- 1

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 1)
})

test_that("find_crossings when a decreasing crossing lands on a point", {
  x_vec <- 0:5
  y <- 5:0
  reference <- 1

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 4)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings interpolates on a two-point grid", {
  x_vec <- 0:1
  y <- 0:1
  reference <- 0.5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 0.5)
})

test_that("find_crossings returns NA when the curve only reaches the reference at the last point", {
  x_vec <- 0:2
  y <- 0:2
  reference <- 2

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

# The test above and the one below are not symmetric, and are not meant to be.
# A value exactly on the reference is assigned to the left of its interval, so
# the last grid point starts no interval and can never be a crossing, while the
# first can. Settled 2026-09-08 (RF) on #59: the convention is kept because the
# alternative returns NA for the two plateau curves below, which are the case
# nsec is defined to estimate.
test_that("find_crossings returns increasing value when the curve only leaves the reference at the first point", {
  x_vec <- 0:2
  y <- 0:2
  reference <- 0

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 0)
})

test_that("find_crossings finds the departure from a plateau on the reference", {
  x_vec <- 0:4
  y <- c(2, 2, 2, 3, 4)
  reference <- 2

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 2)
})

test_that("find_crossings finds the departure from a plateau below the reference", {
  x_vec <- 0:4
  y <- c(2, 2, 2, 1, 0)
  reference <- 2

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 2)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings when reference is NA", {
  x_vec <- 0:2
  y <- 0:2
  reference <- NA_real_

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

# Settled 2026-09-08 (RF) on #59: an NA is not bridged. modify_posterior()
# (R/helpers.R:12-20) only ever writes a leading block of NAs, so once T10
# removes it an interior NA is a prediction failure, and interpolating across it
# invents a value. On y = 10 * exp(-x) over 0:5 with x = 2 dropped and
# reference = 1.5, bridging returns 2.369 against a true crossing of 1.897, an
# error of 24.9 per cent where the same grid with no gap gives 2.0 per cent.
test_that("find_crossings returns NA when an NA is adjacent to the crossing", {
  x_vec <- c(0, 1, 2, 3, 4, 5)
  y <- c(0, 1, 2, NA, 4, 5)
  reference <- 3

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings returns a value when NA not adjacent to the crossing", {
  x_vec <- c(0, 1, 2, 3, 4, 5)
  y <- c(0, NA, 2, 3, 4, 5)
  reference <- 3

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_equal(result$increasing, 3)
})

test_that("find_crossings returns NA when the whole curve is NA", {
  x_vec <- c(0, 1, 2, 3, 4, 5)
  y <- rep(NA_real_, 6)
  reference <- 3

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings returns NA when increasing to an Inf value", {
  x_vec <- c(0, 1)
  y <- c(1, Inf)
  reference <- 3

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings returns NA when decreasing from an Inf value", {
  x_vec <- c(0, 1)
  y <- c(Inf, -1)
  reference <- 3

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_identical(result$decreasing, NA_real_)
  expect_identical(result$increasing, NA_real_)
})

test_that("find_crossings finds closely-spaced crossings (#29)", {
  # modelbased::zero_crossings() re-grids to 100 internal points, so it returns
  # only 40.5 here and loses the spike entirely.
  y <- rep(-5, 41)
  y[20] <- 0.5
  y[41] <- 5
  x_vec <- seq_along(y)
  reference <- 0

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 20.09091, tolerance = 1e-7)
  expect_equal(result$increasing, 19.90909, tolerance = 1e-7)
})

test_that("find_crossings returns the first crossing in each direction when multiple crossings", {
  y <- c(6, 4, 6, 4, 6, 4, 6)
  x_vec <- 0:6
  reference <- 5

  result <- toxval:::find_crossings(y, x_vec, reference)

  expect_equal(result$decreasing, 0.5)
  expect_equal(result$increasing, 1.5)
})

test_that("find_crossings errors when multiple values passed to reference", {
  x_vec <- 0:10
  y <- 10:0
  reference <- c(5.5, 6)

  expect_error(
    toxval:::find_crossings(y, x_vec, reference),
    regexp = "`reference` must be a scalar \\(length 1\\)"
  )
})

test_that("find_crossings misses an excursion narrower than the grid (#40)", {
  # A known limitation, not a defect: crossings are found by scanning for sign
  # changes between adjacent grid points, so a feature that dips below the
  # reference and returns within a single interval is invisible. Increasing
  # resolution reveals it. Closed-form inversion or root-finding (#40) is what
  # would remove the dependence on resolution.
  dip <- function(x) 1 - 4 * exp(-((x - 5.5)^2) / 0.02)
  reference <- 0

  coarse <- 0:11
  result_coarse <- toxval:::find_crossings(dip(coarse), coarse, reference)

  expect_identical(result_coarse$decreasing, NA_real_)
  expect_identical(result_coarse$increasing, NA_real_)

  fine <- seq(0, 11, 0.1)
  result_fine <- toxval:::find_crossings(dip(fine), fine, reference)

  # true crossings are 5.333489 and 5.666511; the residual error is the
  # straight-line interpolation between grid points
  expect_equal(result_fine$decreasing, 5.324335, tolerance = 1e-6)
  expect_equal(result_fine$increasing, 5.675665, tolerance = 1e-6)
})
