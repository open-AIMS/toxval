test_that("x_var errors if not supplied", {
  expect_error(
    nsec_multi(nsec_multi_model_1),
    regexp = "x_var must be supplied for a brmsfit object"
  )
})

test_that("x_var errors if not in the data", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "A"),
    regexp = "Your suplied x_var is not contained in the object data.frame"
  )
})

test_that("sig_val errors if more then one value passed", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", sig_val = c(0.1, 0.5)),
    regexp = "You may only pass one sig_val"
  )
})

test_that("resolution errors if not numeric value passed", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", resolution = "A"),
    regexp = "`resolution` must be numeric"
  )
})

test_that("type must be both, lower, increasing, or decreasing", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", type = "A"),
    regexp = "type must be one of both, lower, increasing, or decreasing"
  )
})

test_that("xform errors if it is not a function", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", xform = "A"),
    regexp = "xform must be a function"
  )
})

test_that("posterior errors if it is not a logical", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", posterior = "TRUE"),
    regexp = "`posterior` must be logical"
  )
})

test_that("prob_vals errors if order is not mid, lower, upper", {
  expect_error(
    nsec_multi(
      nsec_multi_model_1,
      x_var = "dose",
      prob_vals = c(0.1, 0.2, 0.3)
    ),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec_multi(
      nsec_multi_model_1,
      x_var = "dose",
      prob_vals = c(0.3, 0.2, 0.1)
    ),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", prob_vals = c(0.3, 0.2)),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", prob_vals = c(0.3)),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("trials_var errors if not in the data", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", trials_var = "A"),
    regexp = "trials_var does not appear to be in your input data"
  )
})

test_that("multi_var errors if not specified", {
  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose"),
    regexp = "nsec_multi currently only supports multivariate data"
  )
})

test_that("multi_var errors if not in the data", {
  expect_error(
    nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "xx"),
    regexp = "multi_var does not appear to be in your input data"
  )
})

test_that("need to supply multi_var and x_var to get output", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp")

  expect_equal(
    names(output),
    c(
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
  )

  expect_equal(output$vars, c("sp_survival", "sp_growth"))

  expect_equal(
    as.numeric(output[1, 2:9]),
    c(
      0.001,
      1.5,
      1.5,
      0.2026466,
      0.4624833,
      0.6802316,
      0.9208626,
      1.0
    ),
    tolerance = 0.01
  )

  expect_equal(
    as.numeric(output[2, 2:9]),
    c(
      1.5,
      1.5,
      1.5,
      0.4181719,
      0.8688531,
      1.5,
      0.8209478,
      2.350757
    ),
    tolerance = 0.01
  )
})

test_that("posterior = TRUE outputs posterior of both models", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    posterior = TRUE
  )

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival$nsec_dec, 1000)
  expect_length(output$sp_survival$nsec_inc, 1000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      names = c("nsec_dec", "nsec_inc"),
      reference_vals = list(
        dec = c("1%" = 0.9208626),
        inc = c("99%" = 1)
      )
    ),
    tolerance = 0.01
  )

  expect_length(output$sp_growth$nsec_dec, 1000)
  expect_length(output$sp_growth$nsec_inc, 1000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      names = c("nsec_dec", "nsec_inc"),
      reference_vals = list(
        dec = c("1%" = 0.8209478),
        inc = c("99%" = 2.350757)
      )
    ),
    tolerance = 0.01
  )
})

test_that("posterior = TRUE outputs posterior of both models and type = lower", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    posterior = TRUE,
    type = "lower"
  )

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival, 1000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      direction = "dec",
      reference_vals = 0.9208626
    ),
    tolerance = 0.01
  )

  expect_length(output$sp_growth, 1000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      direction = "dec",
      reference_vals = 0.8209478
    ),
    tolerance = 0.01
  )
})

test_that("posterior = TRUE outputs posterior of both models and type = increasing", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    posterior = TRUE,
    type = "increasing"
  )

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival, 1000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      reference_vals = 1,
      direction = "inc"
    ),
    tolerance = 0.01
  )

  expect_length(output$sp_growth, 1000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      reference_vals = 2.350757,
      direction = "inc"
    ),
    tolerance = 0.01
  )
})

test_that("posterior = TRUE outputs posterior of both models and type = decreasing", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    posterior = TRUE,
    type = "decreasing"
  )

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival, 1000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      reference_vals = 0.9208626,
      direction = "dec"
    ),
    tolerance = 0.01
  )

  expect_length(output$sp_growth, 1000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      reference_vals = 0.8209478,
      direction = "dec"
    ),
    tolerance = 0.01
  )
})

test_that("table output when type = lower", {
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
  expect_equal(output$direction, c("dec", "dec"))
  expect_equal(output$var, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 1:4]),
    c(0.2026466, 0.4624833, 0.6802316, 0.9208626),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output[2, 1:4]),
    c(0.4181719, 0.8688531, 1.5, 0.8209478),
    tolerance = 0.01
  )
})

test_that("table output when type = increasing", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "increasing"
  )

  expect_s3_class(output, "data.frame")
  expect_equal(
    colnames(output),
    c("lw", "val", "up", "ref", "direction", "var")
  )
  expect_equal(output$direction, c("inc", "inc"))
  expect_equal(output$var, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 1:4]),
    c(0.001, 1.500, 1.500, 1.000),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output[2, 1:4]),
    c(1.5, 1.5, 1.5, 2.350757),
    tolerance = 0.01
  )
})

test_that("table output when type = decreasing", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "decreasing"
  )

  expect_s3_class(output, "data.frame")
  expect_equal(
    colnames(output),
    c("lw", "val", "up", "ref", "direction", "var")
  )
  expect_equal(output$direction, c("dec", "dec"))
  expect_equal(output$var, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 1:4]),
    c(0.2026466, 0.4624833, 0.6802316, 0.9208626),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output[2, 1:4]),
    c(0.4181719, 0.8688531, 1.5, 0.8209478),
    tolerance = 0.01
  )
})

test_that("x_range can be passed a single value", {
  output <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    x_range = c(0.5)
  )

  expect_s3_class(output, "data.frame")
  expect_equal(
    colnames(output),
    c(
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
  )
  expect_equal(output$vars, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 2:9]),
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.030, 1.0),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output[2, 2:9]),
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.644, 1.811),
    tolerance = 0.01
  )
})

# Additional tests ported from test-nsec-multi-gaps.R --------------------

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

# criterion parameter --------------------------------------------------------
# Man page: criterion = 0.8 — "The criterion to use when type = 'lower'."
# (Note: man page has a typo, writing "type='lowest'" — the valid value is
# "lower".) criterion is passed to quantile() to compare the nsec_inc and
# nsec_dec distributions: whichever has the lower criterion-quantile is chosen.

test_that("nsec_multi criterion changes direction selection for type = lower", {
  output_low <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower",
    criterion = 0.01
  )
  output_high <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower",
    criterion = 0.99
  )

  # Both should produce valid data frames with the same columns
  expect_s3_class(output_low, "data.frame")
  expect_s3_class(output_high, "data.frame")
  expect_equal(colnames(output_low), c("lw", "val", "up", "ref", "direction", "var"))
  expect_equal(colnames(output_high), c("lw", "val", "up", "ref", "direction", "var"))
  expect_equal(nrow(output_low), 2)
  expect_equal(nrow(output_high), 2)
})

test_that("nsec_multi explicit criterion = 0.8 matches default", {
  output_default <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower"
  )
  output_explicit <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "lower",
    criterion = 0.8
  )

  expect_equal(output_default$val, output_explicit$val, tolerance = 0.001)
  expect_equal(output_default$direction, output_explicit$direction)
})

test_that("nsec_multi criterion has no effect for type = both", {
  # criterion is only used in extract_nsec_multi when type = "lower"
  output_low_crit <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "both",
    criterion = 0.01
  )
  output_high_crit <- nsec_multi(
    nsec_multi_model_2,
    x_var = "dose",
    multi_var = "sp",
    type = "both",
    criterion = 0.99
  )

  expect_equal(output_low_crit$dec_val, output_high_crit$dec_val, tolerance = 0.001)
  expect_equal(output_low_crit$inc_val, output_high_crit$inc_val, tolerance = 0.001)
})
