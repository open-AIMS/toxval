# bnecfit -----------------------------------------------------------------



# brms --------------------------------------------------------------------

### TODO: this needs to be saved as an object outside the test
data <- data.frame(
  x = c(1.1, 0.1, 0.4),
  y = c(0.1, 0.2, 0.3)
)

model_1 <-
  brms::brm(
  y ~ x,
  data = data,
  chains = 1,
  iter = 500,
  warmup = 250,
  seed = 101
)

test_that("errors if x_var argument not provided", {
  expect_error(
    ecx(model_1),
    "x_var must be supplied for a brmsfit object."
  )
})

test_that("outputs ecx value", {
  output <- ecx(model_1, x_var = "x")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.256, tolerance = 0.01)
  expect_equal(output[[2]], 0.114, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check ecx_val changes", {
  output <- ecx(model_1, x_var = "x", ecx_val = 50)

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], .757, tolerance = 0.01)
  expect_equal(output[[2]], 0.152, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 50,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check ecx_val must be a numeric scalar", {
  expect_length(ecx(model_1, x_var = "x", ecx_val = 50L), 3)
  expect_length(ecx(model_1, x_var = "x", ecx_val = 50), 3)
  expect_error(
    ecx(model_1, x_var = "x", ecx_val = c(50, 10, 2)),
    "You may only pass one ecx_val"
  )
  expect_error(
    ecx(model_1, x_var = "x", ecx_val = "50"),
    "`ecx_val` must be numeric."
  )
  expect_error(
    ecx(model_1, x_var = "x", ecx_val = TRUE),
    "`ecx_val` must be numeric."
  )
})

test_that("outputs ecx value, check resolution changes", {
  output <- ecx(model_1, x_var = "x", resolution = 10)

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], .255, tolerance = 0.01)
  expect_equal(output[[2]], 0.114, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 10,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check posterior = true argument", {
  output <- ecx(model_1, x_var = "x", posterior = TRUE)

  expect_type(output, "double")
  expect_length(output, 250)
  expect_equal(output[[1]], 0.343, tolerance = 0.01)
  expect_equal(output[[100]], 0.391, tolerance = 0.01)
  expect_equal(output[[250]], 0.246, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check type = relative argument", {
  output <- ecx(model_1, x_var = "x", type = "relative")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.2, tolerance = 0.01)
  expect_equal(output[[2]], 0.2, tolerance = 0.01)
  expect_equal(output[[3]], 1.0, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check type = direct argument", {
  output <- ecx(model_1, x_var = "x", type = "direct")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 1.1, tolerance = 0.01)
  expect_equal(output[[2]], 1.1, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check type errors when more then 1 value passed ", {
  output <- ecx(model_1, x_var = "x", type = c("direct", "type"))

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 1.1, tolerance = 0.01)
  expect_equal(output[[2]], 1.1, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})



test_that("outputs ecx value, check hormesis_def = max argument", {
  output <- ecx(model_1, x_var = "x", hormesis_def = "max")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.387, tolerance = 0.01)
  expect_equal(output[[2]], 0.119, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check x_range argument", {
  output <- ecx(model_1, x_var = "x", x_range = 5)

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 5, tolerance = 0.01)
  expect_equal(output[[2]], 5, tolerance = 0.01)
  expect_equal(output[[3]], 5, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check xform argument", {
  output <- ecx(model_1, x_var = "x", xform = function(x) x - 1)

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], -0.744, tolerance = 0.01)
  expect_equal(output[[2]], -0.886, tolerance = 0.01)
  expect_equal(output[[3]], 0.100, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check prob_vals argument", {
  output <- ecx(model_1, x_var = "x", prob_vals = c(0.45, 0.1, 0.9))

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.237, tolerance = 0.01)
  expect_equal(output[[2]], 0.143, tolerance = 0.01)
  expect_equal(output[[3]], 1.10, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q45", "Q10", "Q90"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("outputs ecx value, check prob_vals argument errors with less then 3 values", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.45, 0.1)),
   "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("outputs ecx value, check prob_vals argument errors if the first value is not in the middle", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.1, 0.5, 0.60)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("outputs ecx value, check prob_vals argument errors if the 2nd value is not the lowest", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.5, 0.6, 0.10)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("outputs ecx value, check prob_vals argument errors if the 3nd value is not the highest", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.5, 0.1, 0.05)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("outputs ecx value, check prob_vals can have more then 3 values", {
  output <- ecx(model_1, x_var = "x", prob_vals = c(0.4, 0.1, 0.6, 0.7, 0.05))

  expect_type(output, "double")
  expect_length(output, 5)
  expect_equal(output[[1]], 0.221, tolerance = 0.01)
  expect_equal(output[[2]], 0.143, tolerance = 0.01)
  expect_equal(output[[3]], 0.345, tolerance = 0.01)
  expect_equal(output[[4]], 0.441, tolerance = 0.01)
  expect_equal(output[[5]], 0.133, tolerance = 0.01)
  expect_equal(
    attributes(output),
    list(
      names = c("Q40", "Q10", "Q60", "Q70", "Q5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})
