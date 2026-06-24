# Tests filling coverage gaps for ecx() across all methods
# Complements test-ecx.R

# ecx.bnecfit — additional parameter tests --------------------------------

test_that("bnecfit xform is applied to output values", {
  output_1 <- ecx(bnec_model_1, ecx_val = 50)
  output_2 <- ecx(bnec_model_1, ecx_val = 50, xform = exp)

  expect_equal(exp(output_1[[1]]), output_2[[1]], tolerance = 0.01)
  expect_equal(exp(output_1[[2]]), output_2[[2]], tolerance = 0.01)
  expect_equal(exp(output_1[[3]]), output_2[[3]], tolerance = 0.01)
})

test_that("bnecfit prob_vals changes quantile levels", {
  output <- ecx(bnec_model_1, prob_vals = c(0.5, 0.1, 0.9))

  expect_equal(rownames(output), c("50%", "10%", "90%"))
  # Narrower interval than default 2.5%/97.5%
  output_default <- ecx(bnec_model_1)
  expect_gte(output[[2]], output_default[[2]])
  expect_lte(output[[3]], output_default[[3]])
})

test_that("bnecfit input validation catches bad type", {
  expect_error(
    ecx(bnec_model_1, type = "nonsense"),
    "type must be one of 'relative', 'absolute'"
  )
})

test_that("bnecfit input validation catches bad hormesis_def", {
  expect_error(
    ecx(bnec_model_1, hormesis_def = "nonsense"),
    "type must be one of 'max' or 'control'"
  )
})

test_that("bnecfit input validation catches non-function xform", {
  expect_error(
    ecx(bnec_model_1, xform = "not_a_function"),
    "xform must be a function"
  )
})

test_that("bnecfit input validation catches bad prob_vals", {
  expect_error(
    ecx(bnec_model_1, prob_vals = c(0.1, 0.5, 0.9)),
    "prob_vals must include central, lower and upper quantiles"
  )
})

test_that("bnecfit input validation catches non-numeric resolution", {
  expect_error(
    ecx(bnec_model_1, resolution = "high"),
    "`resolution` must be numeric"
  )
})

test_that("bnecfit input validation catches non-logical posterior", {
  expect_error(
    ecx(bnec_model_1, posterior = "yes"),
    "`posterior` must be logical"
  )
})

# ecx.bayesmanecfit — additional parameter tests --------------------------

test_that("bayesmanecfit posterior = TRUE returns full posterior", {
  output <- ecx(bayesnec::manec_example, posterior = TRUE)

  expect_type(output, "double")
  # Returns matrix of draws — more than just 3 summary values
  expect_gt(length(output), 3)
  expect_true(!is.null(attr(output, "resolution")))
})

test_that("bayesmanecfit resolution affects output attribute", {
  output_low <- ecx(bayesnec::manec_example, resolution = 10)
  output_high <- ecx(bayesnec::manec_example, resolution = 200)

  expect_length(as.numeric(output_low), 3)
  expect_length(as.numeric(output_high), 3)
  expect_equal(attr(output_low, "resolution"), 10)
  expect_equal(attr(output_high, "resolution"), 200)
})

test_that("bayesmanecfit x_range changes estimation domain", {
  output_default <- ecx(bayesnec::manec_example, ecx_val = 50)
  output_extended <- ecx(
    bayesnec::manec_example,
    ecx_val = 50,
    x_range = c(0.5, 5)
  )

  # Extended range should produce a different estimate
  expect_false(identical(
    as.numeric(output_default),
    as.numeric(output_extended)
  ))
})

# ecx.bayesnecfit (ecx4param) — additional tests --------------------------

test_that("bayesnecfit posterior = TRUE returns many draws", {
  output <- ecx(ecx4param, posterior = TRUE)

  expect_type(output, "double")
  # More than 3 summary values
  expect_gt(length(output), 3)
  expect_true(!is.null(attr(output, "resolution")))
})

test_that("bayesnecfit resolution changes are stored", {
  output <- ecx(ecx4param, resolution = 50)

  expect_equal(attr(output, "resolution"), 50)
})

test_that("bayesnecfit ecx_val ordering is monotonic", {
  e10 <- ecx(ecx4param, ecx_val = 10)
  e50 <- ecx(ecx4param, ecx_val = 50)
  e90 <- ecx(ecx4param, ecx_val = 90)

  expect_lt(e10[[1]], e50[[1]])
  expect_lt(e50[[1]], e90[[1]])
})

# ecx.brmsfit — edge cases ------------------------------------------------

test_that("brmsfit by_group = FALSE with group_var returns marginalised quantiles", {
  output <- ecx(
    brms_model_2,
    x_var = "x",
    by_group = FALSE,
    group_var = "z"
  )

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(names(output), c("Q50", "Q2.5", "Q97.5"))
  expect_equal(attr(output, "ecx_val"), 10)
  expect_equal(attr(output, "toxicity_estimate"), "ecx")
})

test_that("brmsfit ecx_val = 50 is larger than ecx_val = 10", {
  e10 <- ecx(brms_model_1, x_var = "x", ecx_val = 10)
  e50 <- ecx(brms_model_1, x_var = "x", ecx_val = 50)

  expect_gt(e50[[1]], e10[[1]])
})

test_that("brmsfit type = relative gives smaller ECx than type = absolute", {
  e_rel <- ecx(brms_model_1, x_var = "x", type = "relative", ecx_val = 50)
  e_abs <- ecx(brms_model_1, x_var = "x", type = "absolute", ecx_val = 50)

  expect_lt(e_rel[[1]], e_abs[[1]])
})
