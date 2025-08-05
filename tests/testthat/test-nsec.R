# parameters --------------------------------------------------------------
test_that("prob_vals warnings behave as expected", {
  expect_error(
    nsec(bayesnec::manec_example, prob_vals = 0.9, resolution = 10),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec(bayesnec::manec_example, prob_vals = c(0.6, 0.9, 0.1), resolution = 10),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec(bayesnec_nec4param, prob_vals = 0.9, resolution = 10),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec(bayesnec_nec4param, prob_vals = c(0.6, 0.9, 0.1), resolution = 10),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )
})

test_that("prob_vals when passed changes the levels", {
  output <- nsec(bayesnec::manec_example, prob_vals = c(0.6, 0.1, 0.9), resolution = 10)
  expect_length(output, 3)
  expect_equal(names(output), c("Q60", "Q10", "Q90"))

  output <- nsec(bayesnec_nec4param, prob_vals = c(0.4, 0.2, 0.85), resolution = 10)
  expect_length(output, 3)
  expect_equal(names(output), c("Q40", "Q20", "Q85"))
})

test_that("sig_val warning message triggered when more then 1 value passed", {
  expect_error(
    nsec(bayesnec::manec_example, sig_val = c(0.01, 0.05)),
    regexp = "You may only pass one sig_val"
  )
})

# TODO this will fail once fixed up
test_that("hormesis_def errors with wrong input value", {
  expect_error(
    nsec(bayesnec::manec_example, hormesis_def = "other"),
    regexp = 'type must be one of "max" or "control" \\(the default\\). Please see \\?ecx for more details'
  )
})

test_that("xform errors if function not passed ", {
  expect_error(
    nsec(bayesnec::manec_example, xform = 1),
    regexp = "xform must be a function"
  )
})

test_that("xform passes correctly", {
  nsec1 <- nsec(bayesnec_ecx4param, resolution = 10)
  nsec2 <- nsec(bayesnec_ecx4param, xform = exp, resolution = 10)
  expect_gt(nsec2[1], nsec1[2])
})

test_that("when posterior is true you get the posterior as output", {
  output <- nsec(bayesnec_ecx4param, posterior = TRUE, resolution = 10)
  expect_length(output, 100)
})

test_that("sig_val passes correctly", {
  output_1 <- nsec(bayesnec_ecx4param, sig_val = 0.05, resolution = 10)
  expect_equal(attributes(output_1)$reference, c("5%" = 2.156923), tolerance = 0.00001)
  expect_equal(as.numeric(output_1), c(0.8047611, 0.2947924, 3.2205197), tolerance = 0.00001)

  output_2 <- nsec(bayesnec_ecx4param, sig_val = 0.20, resolution = 10)
  expect_equal(attributes(output_2)$reference, c("20%" = 2.212801), tolerance = 0.00001)
  expect_equal(as.numeric(output_2), c(0.7530804, 0.1929446, 3.2205197), tolerance = 0.00001)
})

test_that("nsec returns expected object types and resolution is passing correctly", {

  nsec_summary <- nsec(bayesnec::manec_example, sig_val = 0.01, resolution = 50)
  nsec_summary2 <- nsec(bayesnec::manec_example, sig_val = 0.01, resolution = 50, xform = exp)
  nsec_posterior <- nsec(bayesnec::manec_example, sig_val = 0.01, posterior = TRUE, resolution = 50)

  nsecn_summary <- nsec(bayesnec_nec4param, sig_val = 0.01, resolution = 50)
  nsecn_summary2 <- nsec(bayesnec_nec4param, sig_val = 0.01, resolution = 50, xform = exp)
  nsecn_posterior <- nsec(bayesnec_nec4param, sig_val = 0.01, posterior = TRUE, resolution = 50)

  expect_equal(length(nsec_summary), 3)
  expect_gt(length(nsec_posterior), 3)
  expect_equal(length(nsecn_summary), 3)
  expect_gt(length(nsecn_posterior), 3)
  expect_equal(attributes(nsec_summary)$resolution, 50)
  expect_equal(attributes(nsec_posterior)$resolution, 50)
  expect_equal(attributes(nsecn_summary)$resolution, 50)
  expect_equal(attributes(nsecn_posterior)$resolution, 50)
})

test_that("x_range can be passed various values for bnecfit models", {
  output_1 <- nsec(bayesnec_ecx4param, x_range = c(100))
  expect_equal(as.numeric(output_1), c(100, 100, 100), tolerance = 0.00001)

  output_2 <- nsec(bayesnec_ecx4param, x_range = c(0.5, 100))
  expect_equal(as.numeric(output_2), c(0.7515119, 0.5584780, 0.9257176), tolerance = 0.00001)

  output_3 <- nsec(bayesnec_ecx4param, x_range = c(0.5, 5, 100))
  expect_equal(as.numeric(output_3), c(0.7515119, 0.5584780, 0.9257176), tolerance = 0.00001)
})

# TODO this is wrong, error will fail once fixed
test_that("x_range can be passed various values for drc models", {
  output_1 <- nsec(nsec_drc_1, x_range = c(100), x_var = "x")
  expect_equal(as.numeric(output_1), c(100, 100, 100), tolerance = 0.00001)

  expect_error(
    nsec(nsec_drc_1, x_range = c(0.5, 100), x_var = "x"),
    regexp = "the condition has length > 1"
  )
})

# TODO this is wrong, error will fail once fixed
test_that("x_range can be passed various values for brms models", {
  output_1 <- nsec(brms_model_1, x_range = c(100), x_var = "x")
  expect_equal(as.numeric(output_1), c(100, 100, 100), tolerance = 0.00001)

  expect_error(
    nsec(brms_model_1, x_range = c(0.5, 100), x_var = "x"),
    regexp = "the condition has length > 1"
  )
})

# different model types ---------------------------------------------------

test_that("works for bayesnecfit", {
  nsec1 <- nsec(bayesnec_ecx4param, resolution = 10)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  nsec1 <- nsec(bayesnec::manec_example, resolution = 10)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("additional examples brms 1", {
  output <- nsec(brms_model_3, x_var = "x")
  expect_equal(as.numeric(output), c(0.2830449, 0.1391976, 1.3107842), tolerance = 0.000001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = c("50%" = 4.782721, "2.5%" = 3.066064, "97.5%" = 6.823001),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.000001
  )
})

test_that("additional examples brms 2", {
  output <- nsec(brms_model_4, x_var = "x", by_group = TRUE, group_var = "herbicide")
  expect_equal(names(output), c("Qherbicide", "Q50", "Q2.5", "Q97.5"))
  expect_equal(
    output$Q50,
    c(-2.0726930, -1.8127665, -1.9068558, 1.2526309, -0.9203149, 1.3067134, 0.3225870),
    tolerance = 0.0000001
  )
  expect_equal(
    output$Q2.5,
    c(-2.2466254, -2.1505210, -2.1856878, 0.1746885, -1.7169218, 0.2312798, -0.7213242),
    tolerance = 0.0000001
  )
  expect_equal(
    output$Q97.5,
    c(-1.9186705, -1.5540898, -1.6648760, 1.7585068, -0.4966788, 1.9062046, 0.8107888),
    tolerance = 0.0000001
  )
})

test_that("additional examples brms 2", {
  output <- nsec(brms_model_4, resolution = 10, x_var = "x", group_var = "herbicide")
  expect_equal(as.numeric(output), c(-1.020142, -2.220515, 1.674024), tolerance = 0.0000001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        irgarol = c("50%" = 3.3297024, "2.5%" = 0.5074581, "97.5%" = 6.0617602),
        diuron = c("50%" = 2.9563255, "2.5%" = 0.5288021, "97.5%" = 5.3580609),
        ametryn = c("50%" = 3.014667, "2.5%" = 0.454644, "97.5%" = 5.476075),
        tebuthiuron = c("50%" = 1.8794826, "2.5%" = 0.3358907, "97.5%" = 3.3663824),
        hexazinone = c("50%" = 2.7087672, "2.5%" = 0.3759961, "97.5%" = 4.8952374),
        simazine = c("50%" = 3.1773465, "2.5%" = 0.5612286, "97.5%" = 5.7846814),
        atrazine = c("50%" = 3.3593257, "2.5%" = 0.5548297, "97.5%" = 6.0530515)
      ),
      resolution = 10,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0000001
  )
})






test_that("nsec works for drc using continuous data", {
  model_LL4 <- drc::drm(y ~ x, data = bayesnec::nec_data, fct = drc::LL.4())
  nsec_val <- as.vector(round(nsec(model_LL4, x_var = "x"), 1))
  expect_equal(nsec_val, c(1.2, 1.0, 1.3))
})

test_that("nsec for drc works for curveid and binomial data", {
  daphnids_m2 <- drc::drm((total - no) / total ~ dose, weights = total, curveid = time, data = drc::daphnids, fct = drc::LL.2(), type = "binomial")
  nsec_vals <- nsec(daphnids_m2, x_var = "dose", curveid = "time")
  expect_equal(dim(nsec_vals), c(2, 3))
  expect_equal(rownames(nsec_vals), c("24h", "48h"))
})

test_that("nsec for drc returns an error for an nec model", {
  rye_NEC4 <-  drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::NEC.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
  expect_error(nsec(rye_NEC4, x_var = "conc"), "nsec can currently only be estimated for smooth curves in drc, NEC models are not supported")
})

test_that("nsec for drc throws an error for an increasing function", {
  daphnids_m1 <- drc::drm(no / total ~ dose, weights = total, curveid = time, data = drc::daphnids, fct = drc::LL.2(), type = "binomial")
  expect_error(nsec(daphnids_m1, x_var = "dose"))
})

test_that("nsec drc additional example 1", {
  data <- bayesnec::herbicide[bayesnec::herbicide$herbicide == "ametryn", ]
  data$concentration <- sqrt(data$concentration)
  output_drc <-  drc::drm(fvfm ~ concentration, data = data, fct = drc::LL.3(names = c("Slope", "Upper Limit", "Midpoint")))
  output <- nsec(output_drc, x_var = "concentration")

  expect_equal(as.numeric(output), c(0.3851752, 0.3307462, 0.4231510), tolerance = 0.000001)
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = c("Prediction" = 2.0223202, "Lower" = 0.3312485, "Upper" = 3.6583762),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.000001
  )
})

test_that("nsec drc additional example 2", {
  data <- bayesnec::herbicide[bayesnec::herbicide$herbicide == "ametryn", ]
  data$concentration <- sqrt(data$concentration)
  output_drc <-  drc::drm(fvfm ~ concentration, data = data, fct = drc::LN.4(names = c("Slope", "Lower Limit", "Upper Limit", "Midpoint")))
  output <- nsec(output_drc, x_var = "concentration")

  expect_equal(as.numeric(output), c(0.4021498, 0.3389265, 0.4402086), tolerance = 0.000001)
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = c("Prediction" = 2.1212792, "Lower" = 0.3440486, "Upper" = 3.8753887),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.000001
  )
})

# brms --------------------------------------------------------------------

test_that("brms model errors without x_var", {
  expect_error(
    nsec(brms_model_1),
    regexp = "x_var must be supplied for a brmsfit object"
  )
})

# TODO make better error handling for this
if (FALSE) {
  # turn off for running covr
  test_that("brms model errors if x_var isn't in the dataset", {
    expect_error(
      nsec(brms_model_1, x_var = "z"),
      regexp = "Your suplied x_var is not contained in the object data.frame"
    )
  })
}


test_that("brms model errors if by_group is true but no group_var is supplied", {
  expect_error(
    nsec(brms_model_2, x_var = "x", by_group = TRUE),
    regexp = "You must specify a group_by variable if you want values returned by groups"
  )
})

# TODO make better error handling
test_that("brms model errors if there are additional groups but aren't specified", {
  expect_error(
    nsec(brms_model_2, x_var = "x"),
    regexp = "Do you need to specify a group_var variable?"
  )
})

test_that("brms model errors if group_var is not in the dataset", {
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = "a"),
    regexp = "Your suplied group_var is not contained in the object data.frame"
  )
})

test_that("brms model runs when multiple variables in data and group_var specified", {
  output <- nsec(brms_model_2, x_var = "x", group_var = "z", by_group = FALSE)

  expect_equal(as.numeric(output), c(1.0500000, 0.9326696, 1.0500000), tolerance = 0.0001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        `1` = c("50%" = 444.9909, "2.5%" = 127.4942, "97.5%" = 6537.9810),
        `2` = c("50%" = 216.40707, "2.5%" = 45.80744, "97.5%" = 3059.75473)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0001
  )
})

test_that("brms model runs when multiple variables in data and group_var specified", {
  output <- nsec(brms_model_2, x_var = "x", group_var = "z", by_group = TRUE)

  expect_equal(as.numeric(output[1,]), c(1.00, 1.05, 1.05, 1.05), tolerance = 0.0001)
  expect_equal(as.numeric(output[2,]), c(2.00, 1.05, 0.8914, 1.05), tolerance = 0.0001)
  expect_equal(
    attributes(output),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = c(1, 2),
      names = c("Qz", "Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        `1` = c("50%" = 444.9909, "2.5%" = 127.4942, "97.5%" = 6537.9810),
        `2` = c("50%" = 216.40707, "2.5%" = 45.80744, "97.5%" = 3059.75473)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0001
  )
})

# TODO fix this partial matching error, this should fail once fixed
test_that("brms model errors if only horme supplied as partial matching to other variable", {
  expect_error(
    nsec(brms_model_1, x_var = "x", horme = TRUE),
    regex = 'type must be one of "max" or "control"'
  )
})

test_that("brms model using horme when hormesis_def is control", {
  output <- nsec(brms_model_1, x_var = "x", hormesis_def = "control", horme = TRUE)
  expect_equal(as.numeric(output), c(1.0500000, 0.9760297, 1.0500000), tolerance = 0.0001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = c("50%" = 143.18402, "2.5%" = 75.08872, "97.5%" = 832.02064),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0001
  )
})

test_that("brms model using horme when hormesis_def is max", {
  output <- nsec(brms_model_1, x_var = "x", hormesis_def = "max", horme = TRUE)
  expect_equal(as.numeric(output), c(1.0500000, 0.8175745, 1.0500000), tolerance = 0.0001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = c("50%" = 143.18402, "2.5%" = 75.08872, "97.5%" = 832.02064),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0001
  )
})

test_that("brms model using horme when hormesis_def is max and group is supplied", {
  output <- nsec(brms_model_2, x_var = "x", hormesis_def = "max", horme = TRUE, group_var = "z")
  expect_equal(as.numeric(output), c(1.0500000, 0.8779562, 1.0500000), tolerance = 0.0001)
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        `1` = c("50%" = 444.9909, "2.5%" = 127.4942, "97.5%" = 6537.9810),
        `2` = c("50%" = 216.40707, "2.5%" = 45.80744, "97.5%" = 3059.75473)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0001
  )
})

test_that("brms model by_group is true, group_var is supplied and posterior is true", {
  output <- nsec(brms_model_2, x_var = "x", by_group = TRUE, posterior = TRUE, group_var = "z")
  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("z", "NSEC"))
  expect_equal(dim(output), c(1500, 2))
})

test_that("brms model when posterior is true and group_var is na", {
  output <- nsec(brms_model_1, x_var = "x", by_group = FALSE, posterior = TRUE, group_var = NA)
  output_attr <- attributes(output)

  expect_length(output, 750)
  expect_length(output_attr$ecnsec_relativeP, 750)
  expect_equal(output_attr$resolution, 1000)
  expect_equal(output_attr$sig_val, 0.01)
  expect_equal(output_attr$toxicity_estimate, "nsec")
})

test_that("brms model by_group is false, group_var is supplied and posterior is true", {
  output <- nsec(brms_model_2, x_var = "x", by_group = FALSE, posterior = TRUE, group_var = "z")

  expect_length(output, 1500)
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = list(
        `1` = c("50%" = 444.9909, "2.5%" = 127.4942, "97.5%" = 6537.9810),
        `2` = c("50%" = 216.40707, "2.5%" = 45.80744, "97.5%" = 3059.75473)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.0001
  )
})

# drc ---------------------------------------------------------------------

test_that("drc model errors if no x_var is supplied", {
  expect_error(
    nsec(nsec_drc_1),
    regexp = 'argument "x_var" is missing, with no default'
  )
})

# TODO make better error handling to check right x_var is supplied
test_that("drc model errors if wrong x_var is supplied", {
  expect_error(
    nsec(nsec_drc_1, x_var = "a"),
    regexp = "undefined columns selected"
  )
})

test_that("drc model errors if more then 1 sig val is passed", {
  expect_error(
    nsec(nsec_drc_1, x_var = "x", sig_val = c(0.1, 0.2)),
    regexp = "You may only pass one sig_val"
  )
})

test_that("drc model output attributes", {
  output <- nsec(nsec_drc_1, x_var = "x")
  expect_equal(as.numeric(output), c(1.1788306, 0.9617242, 1.2842022), tolerance = 0.000001)
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = c("Prediction" = 1.6207422, "Lower" = 0.2443134, "Upper" = 3.1976490),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.000001
  )
})

# TODO ask about if there should be a check that the curveid exists in the data
test_that("drc model curveid switches attributes to have a matrix for ecnsec_relativeP", {
  output <- nsec(nsec_drc_1, x_var = "x", curveid = "a")
  expect_equal(as.numeric(output), c(1.1788306, 0.9617242, 1.2842022), tolerance = 0.000001)
  expect_equal(
    attributes(output),
    list(
      dim = c(1, 3),
      dimnames = list(
        "1",
        c("Prediction", "Lower", "Upper")
      ),
      ecnsec_relativeP = matrix(
        data = c(1.6207422, 0.2443134, 3.1976490),
        nrow = 1,
        dimnames = list("1", c("Prediction", "Lower", "Upper"))
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.000001
  )
})

test_that("drc model errors if increasing model supplied", {
  expect_error(
    nsec(nsec_drc_2, x_var = "x"),
    regexp = "nsec can currently only be estimated for curves that represent an overall decreasing function"
  )
})

# TODO the curveid parameter can just be anything which seems wrong
test_that("drc model errors if increasing model supplied", {
  expect_error(
    nsec(nsec_drc_2, x_var = "x", curveid = "a"),
    regexp = "nsec can currently only be estimated for curves that represent an overall decreasing function"
  )
})
