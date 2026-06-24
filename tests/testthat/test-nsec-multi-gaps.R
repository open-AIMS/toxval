# Tests filling coverage gaps for nsec_multi()
# Complements test-nsec-multi.R

# Input validation — additional tests --------------------------------------

test_that("nsec_multi errors with non-logical posterior", {
  expect_error(
    nsec_multi(
      nsec_multi_model_2,
      x_var = "dose",
      multi_var = "sp",
      posterior = 1
    ),
    "`posterior` must be logical"
  )
})

test_that("nsec_multi errors with non-numeric sig_val", {
  expect_error(
    nsec_multi(
      nsec_multi_model_2,
      x_var = "dose",
      multi_var = "sp",
      sig_val = "low"
    )
  )
})

# Output structure tests ---------------------------------------------------

test_that("nsec_multi type = both returns all columns", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp")

  expect_s3_class(output, "data.frame")
  expected_cols <- c(
    "vars",
    "inc_lw",
    "inc_val",
    "inc_up",
    "dec_lw",
    "dec_val",
    "dec_up",
    "dec.1%",
    "inc.99%"
  )
  expect_equal(names(output), expected_cols)
  expect_equal(nrow(output), 2)
})

test_that("nsec_multi type = lower returns correct columns", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower"
  )

  expect_s3_class(output, "data.frame")
  expect_equal(
    colnames(output),
    c("lw", "val", "up", "ref", "direction", "var")
  )
  expect_true(all(output$direction %in% c("inc", "dec")))
})

test_that("nsec_multi type = increasing returns only increasing direction", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "increasing"
  )

  expect_s3_class(output, "data.frame")
  expect_true(all(output$direction == "inc"))
})

test_that("nsec_multi type = decreasing returns only decreasing direction", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "decreasing"
  )

  expect_s3_class(output, "data.frame")
  expect_true(all(output$direction == "dec"))
})

# Posterior tests ----------------------------------------------------------

test_that("nsec_multi posterior = TRUE with type = both returns list with both directions", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    posterior = TRUE,
    type = "both"
  )

  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))
  # Each element should have nsec_dec and nsec_inc
  expect_equal(names(output$sp_survival), c("nsec_dec", "nsec_inc"))
  expect_equal(names(output$sp_growth), c("nsec_dec", "nsec_inc"))
  # Posterior length should match number of MCMC draws
  expect_gt(length(output$sp_survival$nsec_dec), 100)
  expect_gt(length(output$sp_survival$nsec_inc), 100)
})

test_that("nsec_multi posterior = TRUE type = lower returns single vector per variable", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    posterior = TRUE,
    type = "lower"
  )

  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))
  # Each should be a numeric vector (not a list of dec/inc)
  expect_type(output$sp_survival, "double")
  expect_type(output$sp_growth, "double")
  # Should have direction attribute
  expect_true(!is.null(attr(output$sp_survival, "direction")))
  expect_true(!is.null(attr(output$sp_growth, "direction")))
})

# Parameter passing --------------------------------------------------------

test_that("nsec_multi sig_val affects reference values", {
  output_strict <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    sig_val = 0.001,
    posterior = TRUE
  )
  output_loose <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    sig_val = 0.10,
    posterior = TRUE
  )

  # Different sig_val should produce different reference values
  ref_strict <- attr(output_strict$sp_survival, "reference_vals")
  ref_loose <- attr(output_loose$sp_survival, "reference_vals")
  expect_false(identical(ref_strict, ref_loose))
})

test_that("nsec_multi resolution changes x_vec length", {
  output_low <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    resolution = 10
  )
  output_high <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    resolution = 100
  )

  # Both should return valid outputs

  expect_s3_class(output_low, "data.frame")
  expect_s3_class(output_high, "data.frame")
  expect_equal(nrow(output_low), 2)
  expect_equal(nrow(output_high), 2)
})

test_that("nsec_multi xform is applied to nsec values", {
  output_1 <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "decreasing"
  )
  output_2 <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "decreasing",
    xform = function(x) x * 10
  )

  # xform should scale the nsec values
  expect_equal(
    as.numeric(output_2$val),
    as.numeric(output_1$val) * 10,
    tolerance = 0.01
  )
})

test_that("nsec_multi prob_vals changes quantile levels in output", {
  output_default <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower"
  )
  output_narrow <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower",
    prob_vals = c(0.5, 0.1, 0.9)
  )

  # Narrower quantiles should give tighter intervals
  expect_gte(
    as.numeric(output_narrow[1, "lw"]),
    as.numeric(output_default[1, "lw"])
  )
  expect_lte(
    as.numeric(output_narrow[1, "up"]),
    as.numeric(output_default[1, "up"])
  )
})
