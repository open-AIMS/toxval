# Tests whose values or behaviour are expected to change during the refactor
# have a `# EXPECTED-CHANGE` comment giving the reason and the issue. If a test
# without one changes, the developer should double check the change was
# intentional.
#
# Two changes are deliberately not marked, because each would alter almost every
# test in this file: the phase-5 switch to a toxval tibble (#4), and the
# replacement of the evaluation grid with root-finding (#40), which is not yet
# in the refactor plan.

# Tests for helper functions in R/helpers.R

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
