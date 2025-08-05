# brms --------------------------------------------------------------------
test_that("outputs ecx value", {
  output <- ecx(brms_model_1, x_var = "x")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.8320, tolerance = 0.001)
  expect_equal(output[[2]], 0.8177, tolerance = 0.001)
  expect_equal(output[[3]], 1.0500, tolerance = 0.001)
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

test_that("ecx_val changes when different value provided", {
  output <- ecx(brms_model_1, x_var = "x", ecx_val = 50)

  expect_equal(output[[1]], 0.9719, tolerance = 0.01)
  expect_equal(output[[2]], 0.890, tolerance = 0.01)
  expect_equal(output[[3]], 1.05, tolerance = 0.01)
  expect_equal(attributes(output)$ecx_val, 50)
})

test_that("check ecx_val must be a numeric scalar", {
  expect_length(ecx(brms_model_1, x_var = "x", ecx_val = 50L), 3)
  expect_length(ecx(brms_model_1, x_var = "x", ecx_val = 50), 3)
  expect_error(
    ecx(brms_model_1, x_var = "x", ecx_val = c(50, 10, 2)),
    "You may only pass one ecx_val"
  )
  expect_error(
    ecx(brms_model_1, x_var = "x", ecx_val = "50"),
    "`ecx_val` must be numeric."
  )
  expect_error(
    ecx(brms_model_1, x_var = "x", ecx_val = TRUE),
    "`ecx_val` must be numeric."
  )
})

test_that("resolution changes when different value is provided", {
  output <- ecx(brms_model_1, x_var = "x", resolution = 2)

  expect_equal(output[[1]], 0.836, tolerance = 0.01)
  expect_equal(output[[2]], 0.820, tolerance = 0.01)
  expect_equal(output[[3]], 1.050, tolerance = 0.01)
  expect_equal(attributes(output)$resolution, 2)
})

test_that("proper resolution values can be passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = 1),
    regex = "`lower` is not smaller than `upper`"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = -2),
    regexp = "'length\\.out' must be a non-negative number"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = TRUE),
    regexp = "`resolution` must be numeric"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = "3"),
    regexp = "`resolution` must be numeric"
  )

  # TODO: Code in function needs to be updated to silence try error, once that is fixed this will error and need to be udpated
  # since try is not set to silent a message is also being output
  # the message is a special type so expect_message doesn't pick it up
  msg_output <- capture.output({
    expect_error(
      ecx(brms_model_1, x_var = "x", resolution = 0),
      regexp = "group_var"
    )
  }, type = "message")
  # This will fail once we update the try to silent and can then be removed
  expect_match(paste(msg_output, collapse = " "), "replacement has 1 row, data has 0")
})

test_that("posterior = true outputs the posterior", {
  output <- ecx(brms_model_1, x_var = "x", posterior = TRUE)

  expect_type(output, "double")
  expect_length(output, 750)
  expect_equal(output[[1]], 0.8285, tolerance = 0.001)
  expect_equal(output[[100]], 0.8288, tolerance = 0.001)
  expect_equal(output[[750]], 0.824, tolerance = 0.001)
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
  output <- ecx(brms_model_1, x_var = "x", type = "relative")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.8221, tolerance = 0.001)
  expect_equal(output[[2]], 0.8221, tolerance = 0.001)
  expect_equal(output[[3]], 1.021, tolerance = 0.001)
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
  output <- ecx(brms_model_1, x_var = "x", type = "direct")

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 1.05, tolerance = 0.001)
  expect_equal(output[[2]], 1.05, tolerance = 0.001)
  expect_equal(output[[3]], 1.05, tolerance = 0.001)
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
  output <- ecx(brms_model_1, x_var = "x", type = "absolute")

  expect_equal(output[[1]], 0.8320, tolerance = 0.001)
  expect_equal(output[[2]], 0.8176, tolerance = 0.001)
  expect_equal(output[[3]], 1.050, tolerance = 0.001)
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

test_that("type = absolute and value passed to trigger NAN catch", {
  expect_warning(
    output <- ecx(brms_model_1, x_var = "x", type = "absolute", x_range = -1),
    regexp = "NaNs produced"
  )

  expect_equal(output[[1]], -1)
  expect_equal(output[[2]], -1)
  expect_equal(output[[3]], -1)
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

test_that("type = relative and value passed to trigger NAN catch", {
  expect_warning(
    output <- ecx(brms_model_1, x_var = "x", type = "relative", x_range = -1),
    regexp = "NaNs produced"
  )

  expect_equal(output[[1]], -1)
  expect_equal(output[[2]], -1)
  expect_equal(output[[3]], -1)
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

test_that("type = direct and value passed to trigger NAN catch", {
  expect_warning(
    output <- ecx(brms_model_1, x_var = "x", type = "direct", x_range = -1),
    regexp = "NaNs produced"
  )

  expect_equal(output[[1]], -1)
  expect_equal(output[[2]], -1)
  expect_equal(output[[3]], -1)
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

test_that("type errors when wrong value passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", type = "something"),
    "type must be one of 'relative', 'absolute' \\(the default\\) or 'direct'"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", type = c("direct", "absolute")),
    "the condition has length > 1"
  )
})

test_that("hormesis_def = max and type = absolute changes output values", {
  output <- ecx(brms_model_1, x_var = "x", hormesis_def = "max")

  expect_equal(output[[1]], 0.8319, tolerance = 0.001)
  expect_equal(output[[2]], 0.8174, tolerance = 0.001)
  expect_equal(output[[3]], 1.030, tolerance = 0.001)
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

test_that("hormesis_def = control and type = absolute argument", {
  output <- ecx(brms_model_1, x_var = "x", hormesis_def = "control")

  expect_equal(output[[1]], 0.8320, tolerance = 0.001)
  expect_equal(output[[2]], 0.8176, tolerance = 0.001)
  expect_equal(output[[3]], 1.050, tolerance = 0.001)
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

test_that("hormesis_def = max and type = relative changes output values", {
  output <- ecx(brms_model_1, x_var = "x", type = "relative", hormesis_def = "max")

  expect_equal(output[[1]], 1.05, tolerance = 0.001)
  expect_equal(output[[2]], 0.80, tolerance = 0.001)
  expect_equal(output[[3]], 1.05, tolerance = 0.001)
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

test_that("hormesis_def errors wrong values passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", hormesis_def = c("max", "control")),
    "the condition has length > 1"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", hormesis_def = "something"),
    "type must be one of 'max' or 'control' \\(the default\\)"
  )
})

test_that("xform function is applied to the values", {
  output_1 <- ecx(brms_model_1, x_var = "x")
  output_2 <- ecx(brms_model_1, x_var = "x", xform = function(x) x - 1)

  expect_length(output_2, 3)
  expect_equal(output_2[[1]], output_1[[1]] - 1, tolerance = 0.001)
  expect_equal(output_2[[2]], output_1[[2]] - 1, tolerance = 0.001)
  expect_equal(output_2[[3]], output_1[[3]] - 1, tolerance = 0.001)
  expect_equal(
    attributes(output_2),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("xform fails if function not passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", xform = 1),
    "xform must be a function."
  )
})

test_that("prob_vals argument changes when new values provided", {
  output <- ecx(brms_model_1, x_var = "x", prob_vals = c(0.45, 0.1, 0.9))

  expect_length(output, 3)
  expect_equal(output[[1]], 0.8306, tolerance = 0.001)
  expect_equal(output[[2]], 0.8234, tolerance = 0.001)
  expect_equal(output[[3]], 0.8847, tolerance = 0.001)
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
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.45, 0.1)),
   "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals argument errors if the first value is not in the middle", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.1, 0.5, 0.60)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals argument errors if the 2nd value is not the lowest", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.5, 0.6, 0.10)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals argument errors if the 3nd value is not the highest", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.5, 0.1, 0.05)),
    "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("check prob_vals can have more then 3 values", {
  output <- ecx(brms_model_1, x_var = "x", prob_vals = c(0.4, 0.1, 0.6, 0.7, 0.05))

  expect_type(output, "double")
  expect_length(output, 5)
  expect_equal(output[[1]], 0.8299, tolerance = 0.001)
  expect_equal(output[[2]], 0.8234, tolerance = 0.001)
  expect_equal(output[[3]], 0.8351, tolerance = 0.001)
  expect_equal(output[[4]], 0.8386, tolerance = 0.001)
  expect_equal(output[[5]], 0.8202, tolerance = 0.001)
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

# BRMS specific tests -----------------------------------------------------

test_that("can only pass a single exc_val argument", {
  expect_error(
    ecx(brms_model_1, x_var = "x", ecx_val = c(10, 50, 100)),
    "You may only pass one ecx_val"
  )
})

test_that("when type is not direct ecx_val has to between 1 and 99", {
  expect_length(ecx(brms_model_1, x_var = "x", type = "absolute", ecx_val = 2), 3)
  expect_length(ecx(brms_model_1, x_var = "x", type = "relative", ecx_val = 2), 3)
  expect_length(ecx(brms_model_1, x_var = "x", type = "direct", ecx_val = 2), 3)

  expect_error(
    ecx(brms_model_1, x_var = "x", type = "absolute", ecx_val = 0),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_error(
    ecx(brms_model_1, x_var = "x", type = "relative", ecx_val = 0),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_length(ecx(brms_model_1, x_var = "x", type = "direct", ecx_val = 0), 3)

  expect_length(ecx(brms_model_1, x_var = "x", type = "absolute", ecx_val = 99), 3)
  expect_length(ecx(brms_model_1, x_var = "x", type = "relative", ecx_val = 99), 3)
  expect_length(ecx(brms_model_1, x_var = "x", type = "direct", ecx_val = 99), 3)

  expect_error(
    ecx(brms_model_1, x_var = "x", type = "absolute", ecx_val = 100),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_error(
    ecx(brms_model_1, x_var = "x", type = "relative", ecx_val = 100),
    "Supplied ecx_val is not in the required range. Please supply a percentage value between 1 and 99."
  )
  expect_length(ecx(brms_model_1, x_var = "x", type = "direct", ecx_val = 1000), 3)
})

test_that("errors if x_var argument not provided", {
  expect_error(
    ecx(brms_model_1),
    "x_var must be supplied for a brmsfit object"
  )
})

test_that("errors if x_var is not in the data set", {
  expect_error(
    ecx(brms_model_1, x_var = "z"),
    "Your suplied x_var is not contained in the object data.frame"
  )
})

test_that("errors if x_var is not in a predictor variable", {
  expect_error(
    ecx(brms_model_1, x_var = "y"),
    "The following variables can neither be found in 'data' nor in 'data2'"
  )
})

test_that("if by_group is TRUE you must supply a grouping variable in group_var that is in the data", {
  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE),
    "You must specify a group_by variable if you want values returned by groups"
  )

  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "aa"),
    "Your suplied group_var is not contained in the object data.frame"
  )

  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = 1),
    "Your suplied group_var is not contained in the object data.frame"
  )

  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = TRUE),
    "Your suplied group_var is not contained in the object data.frame"
  )
})

test_that("by_group = TRUE and group_var supplied it groups the data based on the group_var", {
  output <- ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "z")
  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(2, 4))
  expect_equal(colnames(output), c("Qz", "Q50", "Q2.5", "Q97.5"))
  expect_equal(output$Q50, c(0.831, 0.830), tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(1, 2),
      names = c("Qz", "Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("by_group = FALSE, group_var is in the data, get vector with length of prob_vals", {
  output <- ecx(brms_model_2, x_var = "z", by_group = FALSE, group_var = "x", prob_vals = c(0.5, 0.025, 0.975))

  expect_type(output, "double")
  expect_length(output, 3)
})

# TODO think this is wrong and you should be able to pass a range
# all that happens is it takes the value and shoves it in as the output
test_that("x_range argument", {
  output <- ecx(brms_model_1, x_var = "x", x_range = 0.5)

  expect_length(output, 3)
  expect_equal(output[[1]], 0.5, tolerance = 0.01)
  expect_equal(output[[2]], 0.5, tolerance = 0.01)
  expect_equal(output[[3]], 0.5, tolerance = 0.01)

  expect_error(
    ecx(brms_model_1, x_var = "x", x_range = c(5, 10)),
    "the condition has length > 1"
  )
})


test_that("when using grouping variable the xform function is applied", {
  output_1 <- ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "z")
  output_2 <- ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "z", xform = function(x) x - 1)

  expect_s3_class(output_2, "data.frame")
  expect_equal(dim(output_2), c(2, 4))
  expect_equal(output_2$Qz, c("1", "2"))
  expect_equal(output_2$Q50, output_1$Q50 - 1, tolerance = 0.001)
  expect_equal(output_2$Q2.5, output_1$Q2.5 - 1, tolerance = 0.001)
  expect_equal(output_2$Q97.5, output_1$Q97.5 - 1, tolerance = 0.001)
})

test_that("when by_group = TRUE, group_var is provided and posterior = TRUE, you get a long data frame", {
  output <- ecx(brms_model_1, x_var = "x", by_group = TRUE, group_var = "x", posterior = TRUE)

  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(3750, 2))
  expect_equal(
    attributes(output),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = 1:3750,
      names = c("x", "ECx"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("when by_group = TRUE, group_var is provided and posterior = TRUE, you get a long data frame by groups", {
  output <- ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "z", posterior = TRUE)

  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(1500, 2))
  expect_equal(output$z, rep(c("1", "2"), length.out = 1500))
  expect_equal(
    attributes(output),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = 1:1500,
      names = c("z", "ECx"),
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("when by_group = FALSE, group_var is provided and posterior = TRUE", {
  output <- ecx(brms_model_1, x_var = "x", by_group = FALSE, group_var = "x", posterior = TRUE)

  expect_type(output, "double")
  expect_length(output, 3750)
  expect_equal(
    attributes(output),
    list(
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

test_that("by_group = FALSE, group_var is provided and posterior = TRUE and there is additional predictors", {
  output <- ecx(brms_model_2, x_var = "x", by_group = FALSE, group_var = "z", posterior = TRUE)

  expect_type(output, "double")
  expect_length(output, 1500)
  expect_equal(
    attributes(output),
    list(
      resolution = 1000,
      ecx_val = 10,
      toxicity_estimate = "ecx"
    )
  )
})

# bnecfit -----------------------------------------------------------------

test_that("bnecfit works with default parameters", {
  output <- ecx(bnec_model_1)

  expect_type(output, "double")
  expect_length(output, 3)
  expect_equal(output[[1]], 0.9356, tolerance = 0.001)
  expect_equal(output[[2]], 0.6980, tolerance = 0.001)
  expect_equal(output[[3]], 1.0337, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(3, 1),
      dimnames = list(
        c("50%", "2.5%", "97.5%"),
        "10"
      ),
      control_value = 0.779768729,
      reference = c("10" = 0.7017919),
      resolution = 100
    ),
    tolerance = 0.001
  )
})

# TODO this test errors because there is a bug in the code, when fixed this test should error as it should start working
test_that("bnecfit checking hormesis_def = max", {
  expect_error(
    ecx(bnec_model_1, hormesis_def = "max"),
    regexp = "need at least two non-NA values to interpolate"
  )
})

test_that("bnecfit checking type = relative", {
  output <- ecx(bnec_model_1, type = "relative")

  expect_equal(output[[1]], 0.9019, tolerance = 0.001)
  expect_equal(output[[2]], 0.6934, tolerance = 0.001)
  expect_equal(output[[3]], 1.0227, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(3, 1),
      dimnames = list(
        c("50%", "2.5%", "97.5%"),
        "10"
      ),
      control_value = 0.7998,
      reference = c("10" = 0.75071),
      resolution = 100
    ),
    tolerance = 0.001
  )
})

test_that("bnecfit checking type = direct", {
  output <- ecx(bnec_model_1, type = "direct")

  expect_equal(output[[1]], 0.9112, tolerance = 0.001)
  expect_equal(output[[2]], 0.7184, tolerance = 0.001)
  expect_equal(output[[3]], 1.0282, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(3, 1),
      dimnames = list(
        c("50%", "2.5%", "97.5%"),
        "10"
      ),
      control_value = 0.7998,
      reference = c("10" = 0.7198),
      resolution = 100
    ),
    tolerance = 0.001
  )
})

test_that("bnecfit checking posterior = TRUE", {
  output <- ecx(bnec_model_1, posterior = TRUE)

  expect_equal(output[[1]], 0.8801, tolerance = 0.001)
  expect_equal(output[[4000]], 0.8928, tolerance = 0.001)
  expect_equal(output[[8000]], 0.6844, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(8000, 1),
      dimnames = list(
        NULL,
        "10"
      ),
      control_value = 0.7998,
      reference = c("10" = 0.7198466),
      resolution = 100
    ),
    tolerance = 0.001
  )
})

test_that("bnecfit checking ecx_val changes", {
  output <- ecx(bnec_model_1, ecx_val = 50)

  expect_equal(output[[1]], 0.9937, tolerance = 0.001)
  expect_equal(output[[2]], 0.9208, tolerance = 0.001)
  expect_equal(output[[3]], 1.0448, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(3, 1),
      dimnames = list(
        c("50%", "2.5%", "97.5%"),
        "50"
      ),
      control_value = 0.7998,
      reference = c("50" = 0.3999),
      resolution = 100
    ),
    tolerance = 0.001
  )
})

test_that("bnecfit checking resolution changes", {
  output <- ecx(bnec_model_1, resolution = 2)

  expect_equal(output[[1]], 0.6021, tolerance = 0.001)
  expect_equal(output[[2]], 0.5114, tolerance = 0.001)
  expect_equal(output[[3]], 0.7405, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(3, 1),
      dimnames = list(
        c("50%", "2.5%", "97.5%"),
        "10"
      ),
      control_value = 0.7998,
      reference = c("10" = 0.7198),
      resolution = 2
    ),
    tolerance = 0.001
  )
})

test_that("bnecfit checking x_range", {
  output <- ecx(bnec_model_1, x_range = c(2, 5))

  expect_equal(output[[1]], 2.5275, tolerance = 0.001)
  expect_equal(output[[2]], 2.0251, tolerance = 0.001)
  expect_equal(output[[3]], 4.2875, tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      dim = c(3, 1),
      dimnames = list(
        c("50%", "2.5%", "97.5%"),
        "10"
      ),
      control_value = 0.2089,
      reference = c("10" = 0.1880),
      resolution = 100
    ),
    tolerance = 0.001
  )
})

# more realistic examples -------------------------------------------------

test_that("brms additional example 1", {
  output <- ecx(brms_model_3, x_var = "x", ecx_val = 50)

  expect_equal(as.numeric(output), c(2.009905, 1.966457, 2.055005), tolerance = 0.001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      resolution = 1000,
      ecx_val = 50,
      toxicity_estimate = "ecx"
    ),
    tolerance = 0.001
  )
})
