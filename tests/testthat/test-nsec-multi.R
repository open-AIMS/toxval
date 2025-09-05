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
    nsec_multi(nsec_multi_model_1, x_var = "dose", prob_vals = c(0.1, 0.2, 0.3)),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec_multi(nsec_multi_model_1, x_var = "dose", prob_vals = c(0.3, 0.2, 0.1)),
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
    c("vars", "inc_lw", "inc_val", "inc_up", "dec_lw", "dec_val", "dec_up", "dec.1%", "inc.99%")
  )

  expect_equal(output$vars, c("sp_survival", "sp_growth"))

  expect_equal(
    as.numeric(output[1, 2:9]),
    c(0.0010000, 1.5000000, 1.5000000, 0.1523446, 0.4415226, 0.7038205, 0.9538846, 1.0000000),
    tolerance = 0.000001
  )

  expect_equal(
    as.numeric(output[2, 2:9]),
    c(1.5000000, 1.5000000, 1.5000000, 0.3089578, 0.7477654, 1.5000000, 0.9115510, 2.4839719),
    tolerance = 0.000001
  )
})

test_that("posterior = TRUE outputs posterior of both models", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", posterior = TRUE)

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival$nsec_dec, 4000)
  expect_length(output$sp_survival$nsec_inc, 4000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      names = c("nsec_dec", "nsec_inc"),
      reference_vals = list(
        dec = c("1%" = 0.9538846),
        inc = c("99%" = 1)
      )
    ),
    tolerance = 0.00001
  )

  expect_length(output$sp_growth$nsec_dec, 4000)
  expect_length(output$sp_growth$nsec_inc, 4000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      names = c("nsec_dec", "nsec_inc"),
      reference_vals = list(
        dec = c("1%" = 0.911551),
        inc = c("99%" = 2.483972)
      )
    ),
    tolerance = 0.00001
  )
})

test_that("posterior = TRUE outputs posterior of both models and type = lower", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", posterior = TRUE, type = "lower")

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival, 4000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      direction = "dec",
      reference_vals = 0.9538846
    ),
    tolerance = 0.00001
  )

  expect_length(output$sp_growth, 4000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      direction = "dec",
      reference_vals = 0.911551
      ),
    tolerance = 0.00001
  )
})

test_that("posterior = TRUE outputs posterior of both models and type = increasing", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", posterior = TRUE, type = "increasing")

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival, 4000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      reference_vals = 1,
      direction = "inc"
    ),
    tolerance = 0.00001
  )

  expect_length(output$sp_growth, 4000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      reference_vals = 2.483972,
      direction = "inc"
    ),
    tolerance = 0.00001
  )
})

test_that("posterior = TRUE outputs posterior of both models and type = decreasing", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", posterior = TRUE, type = "decreasing")

  expect_length(output, 2)
  expect_type(output, "list")
  expect_equal(names(output), c("sp_survival", "sp_growth"))

  expect_length(output$sp_survival, 4000)
  expect_equal(
    attributes(output$sp_survival),
    list(
      reference_vals = 0.9538846,
      direction = "dec"
    ),
    tolerance = 0.00001
  )

  expect_length(output$sp_growth, 4000)
  expect_equal(
    attributes(output$sp_growth),
    list(
      reference_vals = 0.911551,
      direction = "dec"
    ),
    tolerance = 0.00001
  )
})

test_that("table output when type = lower", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", type = "lower")

  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("lw", "val", "up", "ref", "direction", "var"))
  expect_equal(output$direction, c("dec", "dec"))
  expect_equal(output$var, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 1:4]),
    c(0.1523446, 0.4415226, 0.7038205, 0.95388464),
    tolerance = 0.0000001
  )
  expect_equal(
    as.numeric(output[2, 1:4]),
    c(0.3089578, 0.7477654, 1.5000000, 0.9115510),
    tolerance = 0.0000001
  )
})

test_that("table output when type = increasing", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", type = "increasing")

  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("lw", "val", "up", "ref", "direction", "var"))
  expect_equal(output$direction, c("inc", "inc"))
  expect_equal(output$var, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 1:4]),
    c(0.001, 1.500, 1.500, 1.000),
    tolerance = 0.0000001
  )
  expect_equal(
    as.numeric(output[2, 1:4]),
    c(1.500000, 1.500000, 1.500000, 2.483972),
    tolerance = 0.0000001
  )
})

test_that("table output when type = decreasing", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", type = "decreasing")

  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("lw", "val", "up", "ref", "direction", "var"))
  expect_equal(output$direction, c("dec", "dec"))
  expect_equal(output$var, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 1:4]),
    c(0.1523446, 0.4415226, 0.7038205, 0.9538846),
    tolerance = 0.0000001
  )
  expect_equal(
    as.numeric(output[2, 1:4]),
    c(0.3089578, 0.7477654, 1.5000000, 0.9115510),
    tolerance = 0.0000001
  )
})

test_that("x_range can be passed a single value", {
  output <- nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp", x_range = c(0.5))

  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("vars", "inc_lw", "inc_val", "inc_up", "dec_lw", "dec_val", "dec_up", "dec.1%", "inc.99%"))
  expect_equal(output$vars, c("sp_survival", "sp_growth"))
  expect_equal(
    as.numeric(output[1, 2:9]),
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.01225447, 1.0),
    tolerance = 0.0000001
  )
  expect_equal(
    as.numeric(output[2, 2:9]),
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.59370, 1.69408),
    tolerance = 0.00001
  )
})
