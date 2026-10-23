# chk_type ---------------------------------------------------------------------

test_that("chk_type accepts the supported types", {
  expect_silent(toxval:::chk_type("absolute"))
  expect_silent(toxval:::chk_type("range"))
  expect_silent(toxval:::chk_type("direct"))
})

test_that("chk_type errors on an unsupported type", {
  expect_error(
    toxval:::chk_type("relative"),
    "`type` must match 'absolute', 'direct' or 'range', not 'relative'."
  )
})

test_that("chk_type errors when type is not a string", {
  expect_error(
    toxval:::chk_type(c("absolute", "range")),
    "`type` must be a string"
  )
})

# chk_proportion ------------------------------------------------------------------

test_that("chk_proportion accepts a value above 100 under absolute", {
  expect_silent(toxval:::chk_proportion(1.50, "absolute"))
})

test_that("chk_proportion errors on a value above 100 under range", {
  expect_error(
    toxval:::chk_proportion(1.01, "range"),
    "`proportion` must be less than or equal to 1, not 1.01."
  )
})

test_that("chk_proportion errors on a value of 0 or below under absolute", {
  expect_error(
    toxval:::chk_proportion(0, "absolute"),
    "`proportion` must be greater than 0, not 0."
  )
})

test_that("chk_proportion accepts several values", {
  expect_silent(toxval:::chk_proportion(c(0.10, 0.50), "absolute"))
})

# chk_sig_val ------------------------------------------------------------------

test_that("chk_sig_val accepts a value between 0 and 0.5", {
  expect_silent(toxval:::chk_sig_val(0.01))
})

test_that("chk_sig_val errors on a value of 0.5 or above", {
  expect_error(
    toxval:::chk_sig_val(0.5),
    "`sig_val` must be less than 0.5, not 0.5."
  )
})

test_that("chk_sig_val errors on a value of 0 or below", {
  expect_error(
    toxval:::chk_sig_val(0),
    "`sig_val` must be greater than 0, not 0."
  )
})

# chk_prob_vals ----------------------------------------------------------------

test_that("chk_prob_vals accepts the central, lower and upper quantiles", {
  expect_silent(toxval:::chk_prob_vals(c(0.5, 0.025, 0.975)))
})

test_that("chk_prob_vals errors when the quantiles are out of order", {
  expect_error(
    toxval:::chk_prob_vals(c(0.025, 0.5, 0.975)),
    paste0(
      "`prob_vals` must be the central, lower and upper quantiles, in that ",
      "order, not 0.025, 0.5 and 0.975."
    )
  )
})

test_that("chk_prob_vals errors when there are not three values", {
  expect_error(
    toxval:::chk_prob_vals(c(0.5, 0.025)),
    "`prob_vals` must be length 3"
  )
})

test_that("chk_prob_vals errors on a value outside 0 to 1", {
  expect_error(
    toxval:::chk_prob_vals(c(0.5, -0.1, 0.975)),
    "`prob_vals` must have values between 0 and 1"
  )
})
