# bnecfit -----------------------------------------------------------------



# brms --------------------------------------------------------------------

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

test_that("check ecx_val changes when different value provided", {
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

test_that("check ecx_val must be a numeric scalar", {
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

test_that("check resolution changes when different value is provided", {
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

test_that("posterior = true outputs the posterior", {
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

test_that("check type = relative argument", {
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

test_that("check type = direct argument", {
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

test_that("check type = absolute argument", {
  output <- ecx(model_1, x_var = "x", type = "absolute")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.255, tolerance = 0.01)
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

test_that("check type errors when wrong value passed", {
  expect_error(
    ecx(model_1, x_var = "x", type = "something"),
    "type must be one of 'relative', 'absolute' \\(the default\\) or 'direct'"
  )
})

test_that("check type errors when more then 1 value passed ", {
  expect_error(
    ecx(model_1, x_var = "x", type = c("direct", "absolute")),
    "the condition has length > 1"
  )
})

test_that("check hormesis_def = max argument", {
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

test_that("check hormesis_def = control argument", {
  output <- ecx(model_1, x_var = "x", hormesis_def = "control")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.255, tolerance = 0.01)
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

test_that("check hormesis_def errors when more then one value passed", {
  expect_error(
    ecx(model_1, x_var = "x", hormesis_def = c("max", "control")),
    "the condition has length > 1"
  )
})

test_that("check hormesis_def errors when bad value passed", {
  expect_error(
    ecx(model_1, x_var = "x", hormesis_def = "something"),
    "type must be one of 'max' or 'control' \\(the default\\)"
  )
})

test_that("check xform argument", {
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

test_that("check xform fails if function not passed", {
  expect_error(
    ecx(model_1, x_var = "x", xform = 1),
    "xform must be a function."
  )
})

test_that("check prob_vals argument changes when new values provided", {
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

test_that("check prob_vals argument errors with less then 3 values", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.45, 0.1)),
   "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals argument errors if the first value is not in the middle", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.1, 0.5, 0.60)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals argument errors if the 2nd value is not the lowest", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.5, 0.6, 0.10)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals argument errors if the 3nd value is not the highest", {
  expect_error(
    ecx(model_1, x_var = "x", prob_vals = c(0.5, 0.1, 0.05)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals can have more then 3 values", {
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

test_that("when type = relative and hormesis_def = max", {
  output <- ecx(model_1, x_var = "x", type = "relative", hormesis_def = "max")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 1.1, tolerance = 0.01)
  expect_equal(output[[2]], 0.1, tolerance = 0.01)
  expect_equal(output[[3]], 1.1, tolerance = 0.01)
})

test_that("when type = relative and hormesis_def = max", {
  output <- ecx(model_1, x_var = "x", type = "direct", hormesis_def = "max")


})




# BRMS specific tests -----------------------------------------------------

test_that("can only pass a single exc_val argument", {
  expect_error(
    ecx(model_1, x_var = "x", ecx_val = c(10, 50, 100)),
    "You may only pass one ecx_val"
  )
})

test_that("when type is not direc ecx_val has to between 1 and 99", {
  expect_length(ecx(model_1, x_var = "x", type = "absolute", ecx_val = 2), 3)
  expect_length(ecx(model_1, x_var = "x", type = "relative", ecx_val = 2), 3)
  expect_length(ecx(model_1, x_var = "x", type = "direct", ecx_val = 2), 3)

  expect_error(
    ecx(model_1, x_var = "x", type = "absolute", ecx_val = 0),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_error(
    ecx(model_1, x_var = "x", type = "relative", ecx_val = 0),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_length(ecx(model_1, x_var = "x", type = "direct", ecx_val = 0), 3)

  expect_length(ecx(model_1, x_var = "x", type = "absolute", ecx_val = 99), 3)
  expect_length(ecx(model_1, x_var = "x", type = "relative", ecx_val = 99), 3)
  expect_length(ecx(model_1, x_var = "x", type = "direct", ecx_val = 99), 3)

  expect_error(
    ecx(model_1, x_var = "x", type = "absolute", ecx_val = 100),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_error(
    ecx(model_1, x_var = "x", type = "relative", ecx_val = 100),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_length(ecx(model_1, x_var = "x", type = "direct", ecx_val = 1000), 3)
})

test_that("errors if x_var argument not provided", {
  expect_error(
    ecx(model_1),
    "x_var must be supplied for a brmsfit object"
  )
})

test_that("errors if x_var is not in the data set", {
  expect_error(
    ecx(model_1, x_var = "z"),
    "Your suplied x_var is not contained in the object data.frame"
  )
})

test_that("errors if x_var is not in a predictor variable", {
  expect_error(
    ecx(model_1, x_var = "y"),
    "The following variables can neither be found in 'data' nor in 'data2'"
  )
})

test_that("if by_group is TRUE you must supply a grouping variable in group_var that is in the data", {
  expect_error(
    ecx(model_1, x_var = "x", by_group = TRUE),
    "You must specify a group_by variable if you want values returned by groups"
  )

  expect_error(
    ecx(model_1, x_var = "x", by_group = TRUE, group_var = "z"),
    "Your suplied group_var is not contained in the object data.frame"
  )

  expect_error(
    ecx(model_1, x_var = "x", by_group = TRUE, group_var = 1),
    "Your suplied group_var is not contained in the object data.frame"
  )

  expect_error(
    ecx(model_1, x_var = "x", by_group = TRUE, group_var = TRUE),
    "Your suplied group_var is not contained in the object data.frame"
  )

  output <- ecx(model_1, x_var = "x", by_group = TRUE, group_var = "x")
  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(3, 4))
  expect_equal(colnames(output), c("Qx", "Q50", "Q2.5", "Q97.5"))
  expect_equal(
    attributes(output),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(1, 2, 3),
      names = c("Qx", "Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("if by_group is FALSE and you supply a group_var that is in the data", {
  output <- ecx(model_1, x_var = "x", by_group = FALSE, group_var = "x")

  expect_type(output, "double")
  expect_length(output, 3)
})

test_that("check x_range argument", {
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

  expect_error(
    ecx(model_1, x_var = "x", x_range = c(5, 10)),
    "the condition has length > 1"
  )
})

test_that("when using grouping variable the xform function is applied", {
  output <- ecx(model_1, x_var = "x", by_group = TRUE, group_var = "x", xform = function(x) x - 1)

  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(3, 4))
  expect_equal(output$Qx, c("1.1", "0.1", "0.4"))
  expect_equal(output$Q50, c(-0.7444, -0.7444, -0.7444), tolerance = 0.01)
  expect_equal(output$Q2.5, c(-0.886, -0.886, -0.886), tolerance = 0.01)
  expect_equal(output$Q97.5, c(0.100, 0.100, 0.100), tolerance = 0.01)

  output_2 <- ecx(model_1, x_var = "x", by_group = TRUE, group_var = "x")

  expect_equal(output$Q50[1], output_2$Q50[1] - 1, tolerance = 0.01)
  expect_equal(output$Q2.5[1], output_2$Q2.5[1] - 1, tolerance = 0.01)
  expect_equal(output$Q97.5[1], output_2$Q97.5[1] - 1, tolerance = 0.01)
})

test_that("when by_group = TRUE, group_var is provided and posterior = TRUE, you get a long data frame", {
  output <- ecx(model_1, x_var = "x", by_group = TRUE, group_var = "x", posterior = TRUE)

  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(750, 2))
  expect_equal(
    attributes(output),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = 1:750,
      names = c("x", "ECx"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("when by_group = FALSE, group_var is provided and posterior = TRUE", {
  output <- ecx(model_1, x_var = "x", by_group = FALSE, group_var = "x", posterior = TRUE)

  expect_type(output, "double")
  expect_length(output, 750)
  expect_equal(
    attributes(output),
    list(
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

object <- model_1
x_var <- "x"

x_range <- range(object$data[x_var])
x_vec <- seq(min(x_range), max(x_range), length=10)

pred_dat <- data.frame(x_vec)
names(pred_dat) <- x_var

p_samples <- posterior_epred(object, newdata = pred_dat, re_formula = NA)

if (class(p_samples)[1] == "try-error"){
  stop(paste(attributes(p_samples)$condition, "Do you need to specify a group_var variable?", sep=""))
}


