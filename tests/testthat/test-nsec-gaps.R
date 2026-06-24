# Tests filling coverage gaps for nsec() across all methods
# Complements test-nsec.R

# nsec.bnecfit — additional tests ------------------------------------------

test_that("bnecfit nsec returns correct structure", {
  output <- nsec(bayesnec_ecx4param, resolution = 50)

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(names(output), c("Q50", "Q2.5", "Q97.5"))
  expect_equal(attr(output, "sig_val"), 0.01)
  expect_equal(attr(output, "toxicity_estimate"), "nsec")
  expect_equal(attr(output, "resolution"), 50)
})

test_that("bnecfit nsec posterior returns full distribution", {
  output <- nsec(bayesnec_ecx4param, posterior = TRUE, resolution = 50)

  expect_type(output, "double")
  # Posterior length = number of MCMC draws (not resolution)
  expect_gt(length(output), 3)
  expect_equal(attr(output, "resolution"), 50)
  expect_equal(attr(output, "sig_val"), 0.01)
  expect_equal(attr(output, "toxicity_estimate"), "nsec")
  # ecnsec_relativeP should also be the full posterior
  expect_length(attr(output, "ecnsec_relativeP"), length(output))
})

test_that("bnecfit nsec sig_val affects reference level and output", {
  output_strict <- nsec(bayesnec_ecx4param, sig_val = 0.01, resolution = 50)
  output_loose <- nsec(bayesnec_ecx4param, sig_val = 0.10, resolution = 50)

  # Stricter sig_val (lower quantile) gives lower reference
  expect_lt(
    attr(output_strict, "reference")[[1]],
    attr(output_loose, "reference")[[1]]
  )
})

test_that("bnecfit nsec xform is applied to posterior before summarising", {
  # xform is applied to nsec_out (the posterior) before quantile summarisation
  # So exp(quantile(x)) != quantile(exp(x)) — we just check it changes
  output_1 <- nsec(bayesnec_ecx4param, resolution = 50)
  output_2 <- nsec(bayesnec_ecx4param, resolution = 50, xform = exp)

  # xform should produce different (larger) values
  expect_gt(as.numeric(output_2[1]), as.numeric(output_1[1]))
})

test_that("bnecfit nsec hormesis_def = max uses maximum as reference", {
  output_control <- nsec(
    bayesnec_ecx4param,
    resolution = 50,
    hormesis_def = "control"
  )
  output_max <- nsec(bayesnec_ecx4param, resolution = 50, hormesis_def = "max")

  # For ecx4param the max should be >= control (first column)
  expect_gte(
    attr(output_max, "reference")[[1]],
    attr(output_control, "reference")[[1]]
  )
})

# nsec.bayesmanecfit — additional tests ------------------------------------

test_that("bayesmanecfit nsec posterior = TRUE returns full draws", {
  output <- nsec(bayesnec::manec_example, posterior = TRUE, resolution = 50)

  expect_type(output, "double")
  # More than just 3 summary values
  expect_gt(length(output), 3)
  expect_equal(attr(output, "resolution"), 50)
  expect_equal(attr(output, "sig_val"), 0.01)
  expect_equal(attr(output, "toxicity_estimate"), "nsec")
})

test_that("bayesmanecfit nsec resolution changes precision", {
  output_low <- nsec(bayesnec::manec_example, resolution = 10)
  output_high <- nsec(bayesnec::manec_example, resolution = 100)

  expect_equal(attr(output_low, "resolution"), 10)
  expect_equal(attr(output_high, "resolution"), 100)
})

test_that("bayesmanecfit nsec xform changes output", {
  output_1 <- nsec(bayesnec::manec_example, resolution = 50)
  output_2 <- nsec(bayesnec::manec_example, resolution = 50, xform = exp)

  # xform applied to posterior before summarising — expect larger values
  expect_gt(as.numeric(output_2[1]), as.numeric(output_1[1]))
})

# nsec.brmsfit — additional tests ------------------------------------------

test_that("brmsfit nsec by_group = TRUE posterior = TRUE returns correct structure", {
  output <- nsec(
    brms_model_2,
    x_var = "x",
    by_group = TRUE,
    posterior = TRUE,
    group_var = "z"
  )

  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("z", "NSEC"))
  expect_equal(dim(output), c(1500, 2))
  # Both groups should be represented
  expect_equal(sort(unique(output$z)), c("1", "2"))
})

test_that("brmsfit nsec by_group = FALSE posterior = TRUE returns numeric vector", {
  output <- nsec(
    brms_model_2,
    x_var = "x",
    by_group = FALSE,
    posterior = TRUE,
    group_var = "z"
  )

  expect_type(output, "double")
  expect_length(output, 1500)
  expect_equal(attr(output, "resolution"), 1000)
  expect_equal(attr(output, "sig_val"), 0.01)
  expect_equal(attr(output, "toxicity_estimate"), "nsec")
})

test_that("brmsfit nsec resolution is stored correctly", {
  output <- nsec(brms_model_1, x_var = "x", resolution = 500)

  expect_equal(attr(output, "resolution"), 500)
})

test_that("brmsfit nsec sig_val affects output", {
  output_strict <- nsec(brms_model_1, x_var = "x", sig_val = 0.001)
  output_loose <- nsec(brms_model_1, x_var = "x", sig_val = 0.10)

  # Loose sig_val should give a higher reference and thus larger nsec
  # (or at least be different from strict)
  expect_false(identical(as.numeric(output_strict), as.numeric(output_loose)))
})

test_that("brmsfit nsec xform changes output values with grouping", {
  output_1 <- nsec(brms_model_2, x_var = "x", group_var = "z", by_group = TRUE)
  output_2 <- nsec(
    brms_model_2,
    x_var = "x",
    group_var = "z",
    by_group = TRUE,
    xform = function(x) x * 10
  )

  # xform is applied to posterior draws, not final quantiles
  # So output_2 values should be larger than output_1 values
  expect_true(all(output_2$Q50 >= output_1$Q50))
})

# nsec.drc — additional tests ---------------------------------------------

test_that("drc nsec sig_val parameter is stored", {
  output <- nsec(nsec_drc_1, x_var = "x", sig_val = 0.05)

  expect_equal(attr(output, "sig_val"), 0.05)
})

test_that("drc nsec resolution parameter is stored", {
  output <- nsec(nsec_drc_1, x_var = "x", resolution = 500)

  expect_equal(attr(output, "resolution"), 500)
})

test_that("drc nsec returns expected attributes", {
  output <- nsec(nsec_drc_1, x_var = "x")

  expect_true(!is.null(attr(output, "ecnsec_relativeP")))
  expect_equal(attr(output, "toxicity_estimate"), "nsec")
})

test_that("drc nsec xform changes output values", {
  output_1 <- nsec(nsec_drc_1, x_var = "x")
  output_2 <- nsec(nsec_drc_1, x_var = "x", xform = function(x) x * 2)

  # For drc, xform is applied to the nsec_out vector which is then assigned
  # The multiplied values should be larger
  expect_true(all(as.numeric(output_2) >= as.numeric(output_1)))
})

# Input validation ---------------------------------------------------------

test_that("nsec input validation catches non-numeric sig_val", {
  expect_error(
    nsec(bayesnec_ecx4param, sig_val = "strict"),
    "`sig_val` must be numeric"
  )
})

test_that("nsec input validation catches non-numeric resolution", {
  expect_error(
    nsec(bayesnec_ecx4param, resolution = "high"),
    "`resolution` must be numeric"
  )
})

test_that("nsec input validation catches non-logical posterior", {
  expect_error(
    nsec(bayesnec_ecx4param, posterior = "yes"),
    "`posterior` must be logical"
  )
})
