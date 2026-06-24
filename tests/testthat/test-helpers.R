# Tests for helper functions in R/helpers.R

# linear_rescale -----------------------------------------------------------

test_that("linear_rescale maps to new range", {
  x <- 1:10
  result <- toxval:::linear_rescale(x, c(0, 1))

  expect_equal(result[1], 0)
  expect_equal(result[10], 1)
  expect_length(result, 10)
})

test_that("linear_rescale preserves relative ordering", {
  x <- c(5, 10, 15, 20)
  result <- toxval:::linear_rescale(x, c(100, 200))

  expect_true(all(diff(result) > 0))
  expect_equal(result[1], 100)
  expect_equal(result[4], 200)
})

test_that("linear_rescale handles negative ranges", {
  x <- 1:5
  result <- toxval:::linear_rescale(x, c(-10, -5))

  expect_equal(result[1], -10)
  expect_equal(result[5], -5)
})

# clean_names --------------------------------------------------------------

test_that("clean_names strips % and adds Q prefix", {
  x <- c("50%" = 1, "2.5%" = 2, "97.5%" = 3)
  result <- toxval:::clean_names(x)

  expect_equal(result, c("Q50", "Q2.5", "Q97.5"))
})

test_that("clean_names handles integer percentiles", {
  x <- c("10%" = 1, "90%" = 2)
  result <- toxval:::clean_names(x)

  expect_equal(result, c("Q10", "Q90"))
})

# nsec_fct -----------------------------------------------------------------

test_that("nsec_fct returns max x_vec when no crossing found", {
  x_vec <- seq(0, 10, length.out = 100)
  # y always above reference

  y <- rep(5, 100)
  reference <- 10

  result <- toxval:::nsec_fct(y, reference, x_vec)
  expect_equal(result, max(x_vec))
})

test_that("nsec_fct interpolates crossing correctly", {
  x_vec <- seq(0, 10, length.out = 100)
  # Simple linear decrease
  y <- seq(10, 0, length.out = 100)
  reference <- 5

  result <- toxval:::nsec_fct(y, reference, x_vec)
  # Should be approximately at midpoint (x=5)
  expect_equal(result, 5, tolerance = 0.2)
})

# tox_fct ------------------------------------------------------------------

test_that("tox_fct returns NA when no crossing found", {
  x_vec <- seq(0, 10, length.out = 100)
  # y always above reference
  y <- rep(5, 100)
  reference <- 10

  result <- toxval:::tox_fct(y, reference, x_vec)
  expect_true(is.na(result))
})

test_that("tox_fct interpolates crossing correctly", {
  x_vec <- seq(0, 10, length.out = 100)
  # Simple linear decrease
  y <- seq(10, 0, length.out = 100)
  reference <- 5

  result <- toxval:::tox_fct(y, reference, x_vec)
  expect_equal(result, 5, tolerance = 0.2)
})

# modify_posterior ---------------------------------------------------------

test_that("modify_posterior with hormesis_def = max sets values before peak to NA", {
  x_vec <- seq(0, 10, length.out = 20)
  # Posterior sample with peak at position 5
  p_sample <- c(1, 2, 3, 4, 5, 4, 3, 2, 1, 0.5, rep(0.3, 10))
  p_samples <- matrix(p_sample, nrow = 1)

  result <- toxval:::modify_posterior(
    1,
    NULL,
    x_vec,
    p_samples,
    hormesis_def = "max"
  )

  # Values before peak (positions 1-4) should be NA
  expect_true(all(is.na(result[1:4])))
  # Peak and after should be unchanged
  expect_equal(result[5], 5)
})

test_that("modify_posterior with hormesis_def = control leaves all values", {
  x_vec <- seq(0, 10, length.out = 20)
  p_sample <- c(1, 2, 3, 4, 5, 4, 3, 2, 1, 0.5, rep(0.3, 10))
  p_samples <- matrix(p_sample, nrow = 1)

  result <- toxval:::modify_posterior(
    1,
    NULL,
    x_vec,
    p_samples,
    hormesis_def = "control"
  )

  # With control, no NAs should be introduced (current implementation)
  expect_equal(result, p_sample)
})

# min_abs ------------------------------------------------------------------

test_that("min_abs finds index of value closest to zero", {
  expect_equal(toxval:::min_abs(c(-5, -1, 0.5, 3, 10)), 3)
  expect_equal(toxval:::min_abs(c(-0.1, 0.2, 5)), 1)
  expect_equal(toxval:::min_abs(c(10, -10, 0)), 3)
})

# contains_zero / contains_one / contains_negative -------------------------

test_that("contains_zero detects zeros", {
  expect_true(toxval:::contains_zero(c(1, 0, 3)))
  expect_false(toxval:::contains_zero(c(1, 2, 3)))
  expect_true(toxval:::contains_zero(c(0, 0, 0)))
})

test_that("contains_one detects ones", {
  expect_true(toxval:::contains_one(c(0, 1, 2)))
  expect_false(toxval:::contains_one(c(0, 0.5, 2)))
})

test_that("contains_negative detects negatives", {
  expect_true(toxval:::contains_negative(c(-1, 0, 1)))
  expect_false(toxval:::contains_negative(c(0, 1, 2)))
  expect_true(toxval:::contains_negative(c(NA, -0.5, 1)))
})

# newdata_eval -------------------------------------------------------------

test_that("newdata_eval returns list with newdata and x_vec", {
  result <- toxval:::newdata_eval(
    bayesnec::manec_example,
    resolution = 50,
    x_range = NA
  )

  expect_type(result, "list")
  expect_true("newdata" %in% names(result))
  expect_true("x_vec" %in% names(result))
  expect_length(result$x_vec, 50)
})

test_that("newdata_eval respects x_range", {
  result <- toxval:::newdata_eval(
    bayesnec::manec_example,
    resolution = 100,
    x_range = c(1, 3)
  )

  expect_equal(min(result$x_vec), 1, tolerance = 0.01)
  expect_equal(max(result$x_vec), 3, tolerance = 0.01)
})

test_that("newdata_eval works with bayesnecfit objects", {
  result <- toxval:::newdata_eval(
    ecx4param,
    resolution = 30,
    x_range = NA
  )

  expect_type(result, "list")
  expect_length(result$x_vec, 30)
})

# estimates_summary --------------------------------------------------------

test_that("estimates_summary returns median and 95% CI", {
  set.seed(42)
  x <- rnorm(1000, mean = 5, sd = 1)
  result <- toxval:::estimates_summary(x)

  expect_length(result, 3)
  expect_equal(names(result), c("Estimate", "Q2.5", "Q97.5"))
  expect_equal(unname(result["Estimate"]), median(x), tolerance = 0.001)
  expect_equal(
    unname(result["Q2.5"]),
    unname(quantile(x, 0.025)),
    tolerance = 0.001
  )
  expect_equal(
    unname(result["Q97.5"]),
    unname(quantile(x, 0.975)),
    tolerance = 0.001
  )
})

# gm_mean ------------------------------------------------------------------

test_that("gm_mean calculates geometric mean", {
  expect_equal(toxval:::gm_mean(c(1, 10, 100)), exp(mean(log(c(1, 10, 100)))))
  expect_equal(toxval:::gm_mean(c(4, 4, 4)), 4)
})

test_that("gm_mean returns NaN for negative values", {
  expect_true(is.nan(toxval:::gm_mean(c(-1, 2, 3))))
})

test_that("gm_mean handles zeros with zero_propagate", {
  expect_equal(toxval:::gm_mean(c(0, 5, 10), zero_propagate = TRUE), 0)
  # Without zero_propagate, zeros are excluded from log calculation
  result <- toxval:::gm_mean(c(0, 5, 10), zero_propagate = FALSE)
  expect_gt(result, 0)
})
