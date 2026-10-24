# Tests whose values or behaviour are expected to change during the refactor
# have a `# EXPECTED-CHANGE` comment giving the reason and the issue. If a test
# without one changes, the developer should double check the change was
# intentional.
#
# nsec_multi() is a deprecated wrapper around nsec() (T15), so most of what
# it did is tested through nsec() in test-nsec.R. This file keeps the tests of
# the wrapper itself and, at the bottom, tests of nsec() specific to a
# multivariate fit.

# nsec_multi() deprecated wrapper --------------------------------------------

test_that("nsec_multi is deprecated in favour of nsec", {
  lifecycle::expect_deprecated(
    suppressWarnings(
      nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp"),
      classes = "simpleWarning"
    )
  )
})

test_that("nsec_multi returns the nsec result wihtout needed multi_var", {
  output <- suppressWarnings(
    nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp")
  )
  expected <- suppressWarnings(
    nsec(nsec_multi_model_2, x_var = "dose", resolution = 50)
  )

  expect_identical(output, expected)
})

# EXPECTED-CHANGE output is the nsec() toxval tibble (#4, T15); realisations
# with no crossing are placed or censored rather than set to max(x_vec) (T21,
# #60) - done
# Pre-refactor nsec_multi() gave as :
#                          lower,     estimate,  upper      reference:
#   sp_survival decreasing 0.2026466, 0.4624833, 0.6802316, 0.9208626
#   sp_survival increasing 0.001,     1.5,       1.5,       1.0
#   sp_growth   decreasing 0.4181719, 0.8688531, 1.5,       0.8209478
#   sp_growth   increasing 1.5,       1.5,       1.5,       2.350757
# The references are unchanged. sp_survival increasing has no realisation that
# crosses its reference, so it is NA.
test_that("nsec_multi values against the pre-refactor nsec_multi", {
  output <- suppressWarnings(
    nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp")
  )

  expect_equal(
    output$estimate,
    c(0.4615262, NA, 0.8591841, 1.5),
    tolerance = 1e-6
  )
  expect_equal(
    output$conf.low,
    c(0.1559251, NA, 0.3908073, 1.5),
    tolerance = 1e-6
  )
  expect_equal(
    output$conf.high,
    c(0.6570357, NA, 1.5, 1.5),
    tolerance = 1e-6
  )
  expect_equal(
    output$reference,
    c(0.9208626, 1, 0.8209478, 2.350757),
    tolerance = 1e-6
  )
})

test_that("nsec_multi type keeps the rows of one direction", {
  increasing <- suppressWarnings(
    nsec_multi(nsec_multi_model_2, x_var = "dose", type = "increasing")
  )
  decreasing <- suppressWarnings(
    nsec_multi(nsec_multi_model_2, x_var = "dose", type = "decreasing")
  )

  expect_identical(increasing$direction, c("increasing", "increasing"))
  expect_identical(decreasing$direction, c("decreasing", "decreasing"))
  expect_identical(
    as.character(decreasing$response),
    c("spsurvival", "spgrowth")
  )
})

test_that("nsec_multi type = lower keeps the direction lower at the criterion", {
  # the pre-refactor nsec_multi() also kept the decreasing direction of both
  # responses at the default criterion of 0.8
  default <- suppressWarnings(
    nsec_multi(nsec_multi_model_2, x_var = "dose", type = "lower")
  )
  # at 0.01, the realisations of the spsurvival increasing direction placed at
  # the control rank below every decreasing crossing
  low <- suppressWarnings(
    nsec_multi(
      nsec_multi_model_2,
      x_var = "dose",
      type = "lower",
      criterion = 0.01
    )
  )

  expect_identical(default$direction, c("decreasing", "decreasing"))
  expect_identical(low$direction, c("increasing", "decreasing"))
  expect_false("draws" %in% names(default))
})

test_that("nsec_multi posterior = TRUE returns the draws", {
  output <- suppressWarnings(
    nsec_multi(nsec_multi_model_2, x_var = "dose", posterior = TRUE)
  )

  expect_true("draws" %in% names(output))
  expect_equal(nrow(output$draws[[1]]), 1000)
})

test_that("nsec_multi type must be both, lower, increasing or decreasing", {
  expect_error(
    suppressWarnings(
      nsec_multi(nsec_multi_model_2, x_var = "dose", type = "A")
    ),
    regexp = "`type` must match 'both', 'decreasing', 'increasing' or 'lower', not 'A'"
  )
})

# nsec() for a multivariate fit ----------------------------------------------

test_that("nsec gives each response its own realisations", {
  output <- suppressWarnings(
    nsec(nsec_multi_model_2, x_var = "dose", resolution = 50, draws = TRUE)
  )

  expect_equal(vapply(output$draws, nrow, integer(1)), rep(1000L, 4))
  expect_false(identical(
    output$draws[[1]]$value.draw,
    output$draws[[3]]$value.draw
  ))
})

test_that("nsec of a multivariate toxval_pred matches nsec of the fit", {
  pred <- toxval_predict(nsec_multi_model_2, x_var = "dose", resolution = 50)

  expect_identical(
    suppressWarnings(nsec(pred)),
    suppressWarnings(
      nsec(nsec_multi_model_2, x_var = "dose", resolution = 50)
    )
  )
})
