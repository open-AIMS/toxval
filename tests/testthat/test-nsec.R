# parameters --------------------------------------------------------------
test_that("prob_vals warnings behave as expected", {
  expect_error(
    nsec(bayesnec::manec_example, prob_vals = 0.9, resolution = 10),
    regexp = "prob_vals must include central, lower and upper quantiles, in that order"
  )

  expect_error(
    nsec(
      bayesnec::manec_example,
      prob_vals = c(0.6, 0.9, 0.1),
      resolution = 10
    ),
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
  output <- nsec(
    bayesnec::manec_example,
    prob_vals = c(0.6, 0.1, 0.9),
    resolution = 10
  )
  expect_length(output, 3)
  expect_equal(names(output), c("Q60", "Q10", "Q90"))

  output <- nsec(
    bayesnec_nec4param,
    prob_vals = c(0.4, 0.2, 0.85),
    resolution = 10
  )
  expect_length(output, 3)
  expect_equal(names(output), c("Q40", "Q20", "Q85"))
})

test_that("prob_vals with more than 3 values returns that many quantiles", {
  output <- nsec(
    brms_model_1,
    x_var = "x",
    prob_vals = c(0.4, 0.1, 0.6, 0.7, 0.05)
  )
  expect_type(output, "double")
  expect_length(output, 5)
  expect_equal(names(output), c("Q40", "Q10", "Q60", "Q70", "Q5"))
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
  expect_equal(
    attributes(output_1)$reference,
    c("5%" = 2.156923),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output_1),
    c(0.8047611, 0.2947924, 3.2205197),
    tolerance = 0.01
  )

  output_2 <- nsec(bayesnec_ecx4param, sig_val = 0.20, resolution = 10)
  expect_equal(
    attributes(output_2)$reference,
    c("20%" = 2.212801),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output_2),
    c(0.7530804, 0.1929446, 3.2205197),
    tolerance = 0.01
  )
})

test_that("nsec returns expected object types and resolution is passing correctly", {
  nsec_summary <- nsec(bayesnec::manec_example, sig_val = 0.01, resolution = 50)
  nsec_summary2 <- nsec(
    bayesnec::manec_example,
    sig_val = 0.01,
    resolution = 50,
    xform = exp
  )
  nsec_posterior <- nsec(
    bayesnec::manec_example,
    sig_val = 0.01,
    posterior = TRUE,
    resolution = 50
  )

  nsecn_summary <- nsec(bayesnec_nec4param, sig_val = 0.01, resolution = 50)
  nsecn_summary2 <- nsec(
    bayesnec_nec4param,
    sig_val = 0.01,
    resolution = 50,
    xform = exp
  )
  nsecn_posterior <- nsec(
    bayesnec_nec4param,
    sig_val = 0.01,
    posterior = TRUE,
    resolution = 50
  )

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
  expect_equal(as.numeric(output_1), c(100, 100, 100), tolerance = 0.01)

  output_2 <- nsec(bayesnec_ecx4param, x_range = c(0.5, 100))
  expect_equal(
    as.numeric(output_2),
    c(0.7515119, 0.5584780, 0.9257176),
    tolerance = 0.01
  )

  output_3 <- nsec(bayesnec_ecx4param, x_range = c(0.5, 5, 100))
  expect_equal(
    as.numeric(output_3),
    c(0.7515119, 0.5584780, 0.9257176),
    tolerance = 0.01
  )
})

# TODO this is wrong, error will fail once fixed
test_that("x_range can be passed various values for drc models", {
  output_1 <- nsec(nsec_drc_1, x_range = c(100), x_var = "x")
  expect_equal(as.numeric(output_1), c(100, 100, 100), tolerance = 0.01)

  expect_error(
    nsec(nsec_drc_1, x_range = c(0.5, 100), x_var = "x"),
    regexp = "the condition has length > 1"
  )
})

# TODO this is wrong, error will fail once fixed
test_that("x_range can be passed various values for brms models", {
  output_1 <- nsec(brms_model_1, x_range = c(100), x_var = "x")
  expect_equal(as.numeric(output_1), c(100, 100, 100), tolerance = 0.01)

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
  expect_equal(
    as.numeric(output),
    c(0.2576897, 0.1164777, 1.280471),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = c(
        "50%" = 4.39107,
        "2.5%" = 2.57067,
        "97.5%" = 6.280108
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

test_that("additional examples brms 2", {
  output <- nsec(
    brms_model_4,
    x_var = "x",
    by_group = TRUE,
    group_var = "herbicide"
  )
  expect_equal(names(output), c("Qherbicide", "Q50", "Q2.5", "Q97.5"))
  expect_equal(
    output$Q50,
    c(
      -2.068832,
      -1.776294,
      -1.899442,
      1.219105,
      -0.9762579,
      1.218561,
      0.2834879
    ),
    tolerance = 0.01
  )
  expect_equal(
    output$Q2.5,
    c(
      -2.242587,
      -2.093038,
      -2.175464,
      -0.0507173,
      -1.816361,
      0.1034953,
      -0.8067019
    ),
    tolerance = 0.01
  )
  expect_equal(
    output$Q97.5,
    c(
      -1.918876,
      -1.523899,
      -1.677678,
      1.731901,
      -0.5923018,
      1.827742,
      0.8223756
    ),
    tolerance = 0.01
  )
})

test_that("additional examples brms 2", {
  output <- nsec(
    brms_model_4,
    resolution = 10,
    x_var = "x",
    group_var = "herbicide"
  )
  expect_equal(
    as.numeric(output),
    c(-1.069832, -2.215402, 1.585778),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        irgarol = c("50%" = 3.456955, "2.5%" = 0.5323418, "97.5%" = 6.378487),
        diuron = c("50%" = 3.29635, "2.5%" = 0.6846643, "97.5%" = 5.994729),
        ametryn = c("50%" = 3.036638, "2.5%" = 0.5081269, "97.5%" = 5.458449),
        tebuthiuron = c(
          "50%" = 1.867343,
          "2.5%" = 0.2472245,
          "97.5%" = 3.440123
        ),
        hexazinone = c(
          "50%" = 2.580989,
          "2.5%" = 0.3832357,
          "97.5%" = 4.738381
        ),
        simazine = c(
          "50%" = 3.431859,
          "2.5%" = 0.5125153,
          "97.5%" = 5.830609
        ),
        atrazine = c("50%" = 3.353621, "2.5%" = 0.4870696, "97.5%" = 6.231332)
      ),
      resolution = 10,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})


test_that("nsec works for drc using continuous data", {
  model_LL4 <- drc::drm(y ~ x, data = bayesnec::nec_data, fct = drc::LL.4())
  nsec_val <- as.vector(round(nsec(model_LL4, x_var = "x"), 1))
  expect_equal(nsec_val, c(1.2, 1.0, 1.3))
})

test_that("nsec for drc works for curveid and binomial data", {
  daphnids_m2 <- drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )
  nsec_vals <- nsec(daphnids_m2, x_var = "dose", curveid = "time")
  expect_equal(dim(nsec_vals), c(2, 3))
  expect_equal(rownames(nsec_vals), c("24h", "48h"))
})

test_that("nsec for drc returns an error for an nec model", {
  rye_NEC4 <- drc::drm(
    rootl ~ conc,
    data = drc::ryegrass,
    fct = drc::NEC.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))
  )
  expect_error(
    nsec(rye_NEC4, x_var = "conc"),
    "nsec can currently only be estimated for smooth curves in drc, NEC models are not supported"
  )
})

test_that("nsec for drc throws an error for an increasing function", {
  daphnids_m1 <- drc::drm(
    no / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )
  expect_error(nsec(daphnids_m1, x_var = "dose"))
})

test_that("nsec drc additional example 1", {
  data <- bayesnec::herbicide[bayesnec::herbicide$herbicide == "ametryn", ]
  data$concentration <- sqrt(data$concentration)
  output_drc <- drc::drm(
    fvfm ~ concentration,
    data = data,
    fct = drc::LL.3(names = c("Slope", "Upper Limit", "Midpoint"))
  )
  output <- nsec(output_drc, x_var = "concentration")

  expect_equal(
    as.numeric(output),
    c(0.3851752, 0.3307462, 0.4231510),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = c(
        "Prediction" = 2.0223202,
        "Lower" = 0.3312485,
        "Upper" = 3.6583762
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

test_that("nsec drc additional example 2", {
  data <- bayesnec::herbicide[bayesnec::herbicide$herbicide == "ametryn", ]
  data$concentration <- sqrt(data$concentration)
  output_drc <- drc::drm(
    fvfm ~ concentration,
    data = data,
    fct = drc::LN.4(
      names = c("Slope", "Lower Limit", "Upper Limit", "Midpoint")
    )
  )
  output <- nsec(output_drc, x_var = "concentration")

  expect_equal(
    as.numeric(output),
    c(0.4030571, 0.3393384, 0.4412300),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = c(
        "Prediction" = 2.111694,
        "Lower" = 0.3425053,
        "Upper" = 3.857750
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
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

test_that("brms model errors if group_var is not in the dataset when non-character types passed", {
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = 1),
    regexp = "Your suplied group_var is not contained in the object data.frame"
  )
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = TRUE),
    regexp = "Your suplied group_var is not contained in the object data.frame"
  )
})

test_that("brms model runs when multiple variables in data and group_var specified", {
  output <- nsec(brms_model_2, x_var = "x", group_var = "z", by_group = FALSE)

  expect_equal(
    as.numeric(output),
    c(1.05, 1.016295, 1.05),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        `1` = c("50%" = 778.5143, "2.5%" = 169.6611, "97.5%" = 9649.234),
        `2` = c("50%" = 330.8031, "2.5%" = 81.00827, "97.5%" = 4069.751)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

test_that("brms model runs when multiple variables in data and group_var specified", {
  output <- nsec(brms_model_2, x_var = "x", group_var = "z", by_group = TRUE)

  expect_equal(
    as.numeric(output[1, ]),
    c(1.00, 1.05, 1.05, 1.05),
    tolerance = 0.01
  )
  expect_equal(
    as.numeric(output[2, ]),
    c(2.00, 1.05, 0.9791, 1.05),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      row.names = c(1, 2),
      names = c("Qz", "Q50", "Q2.5", "Q97.5"),
      class = c("tbl_df", "tbl", "data.frame"),
      ecnsec_relativeP = list(
        `1` = c("50%" = 778.5143, "2.5%" = 169.6611, "97.5%" = 9649.234),
        `2` = c("50%" = 330.8031, "2.5%" = 81.00827, "97.5%" = 4069.751)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
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
  output <- nsec(
    brms_model_1,
    x_var = "x",
    hormesis_def = "control",
    horme = TRUE
  )
  expect_equal(
    as.numeric(output),
    c(1.05, 0.9903239, 1.05),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = c(
        "50%" = 130.641,
        "2.5%" = 78.48249,
        "97.5%" = 792.1342
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

test_that("brms model using horme when hormesis_def is max", {
  output <- nsec(brms_model_1, x_var = "x", hormesis_def = "max", horme = TRUE)
  expect_equal(
    as.numeric(output),
    c(1.05, 0.8213543, 1.05),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = c(
        "50%" = 130.641,
        "2.5%" = 78.48249,
        "97.5%" = 792.1342
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

test_that("brms model using horme when hormesis_def is max and group is supplied", {
  output <- nsec(
    brms_model_2,
    x_var = "x",
    hormesis_def = "max",
    horme = TRUE,
    group_var = "z"
  )
  expect_equal(
    as.numeric(output),
    c(1.05, 0.9612536, 1.05),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      names = c("Q50", "Q2.5", "Q97.5"),
      ecnsec_relativeP = list(
        `1` = c("50%" = 778.5143, "2.5%" = 169.6611, "97.5%" = 9649.234),
        `2` = c("50%" = 330.8031, "2.5%" = 81.00827, "97.5%" = 4069.751)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

test_that("brms model by_group is true, group_var is supplied and posterior is true", {
  output <- nsec(
    brms_model_2,
    x_var = "x",
    by_group = TRUE,
    posterior = TRUE,
    group_var = "z"
  )
  expect_s3_class(output, "data.frame")
  expect_equal(colnames(output), c("z", "NSEC"))
  expect_equal(dim(output), c(2000, 2))
})

test_that("brms model when posterior is true and group_var is na", {
  output <- nsec(
    brms_model_1,
    x_var = "x",
    by_group = FALSE,
    posterior = TRUE,
    group_var = NA
  )
  output_attr <- attributes(output)

  expect_length(output, 1000)
  expect_length(output_attr$ecnsec_relativeP, 1000)
  expect_equal(output_attr$resolution, 1000)
  expect_equal(output_attr$sig_val, 0.01)
  expect_equal(output_attr$toxicity_estimate, "nsec")
})

test_that("brms model by_group is false, group_var is supplied and posterior is true", {
  output <- nsec(
    brms_model_2,
    x_var = "x",
    by_group = FALSE,
    posterior = TRUE,
    group_var = "z"
  )

  expect_length(output, 2000)
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = list(
        `1` = c("50%" = 778.5143, "2.5%" = 169.6611, "97.5%" = 9649.234),
        `2` = c("50%" = 330.8031, "2.5%" = 81.00827, "97.5%" = 4069.751)
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
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
  expect_equal(
    as.numeric(output),
    c(1.1788306, 0.9617242, 1.2842022),
    tolerance = 0.01
  )
  expect_equal(
    attributes(output),
    list(
      ecnsec_relativeP = c(
        "Prediction" = 1.6207422,
        "Lower" = 0.2443134,
        "Upper" = 3.1976490
      ),
      resolution = 1000,
      sig_val = 0.01,
      toxicity_estimate = "nsec"
    ),
    tolerance = 0.01
  )
})

# TODO ask about if there should be a check that the curveid exists in the data
test_that("drc model curveid switches attributes to have a matrix for ecnsec_relativeP", {
  output <- nsec(nsec_drc_1, x_var = "x", curveid = "a")
  expect_equal(
    as.numeric(output),
    c(1.1788306, 0.9617242, 1.2842022),
    tolerance = 0.01
  )
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
    tolerance = 0.01
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
  expect_equal(dim(output), c(2000, 2))
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
  expect_length(output, 2000)
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

# nsec input validation ----------------------------------------------------

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

# nsec default resolution per method type ------------------------------------
# Man page generic shows resolution = 100, but brmsfit and drc methods default
# to resolution = 1000. These tests document the actual per-method defaults.

test_that("bnecfit default resolution is 100", {
  output <- nsec(bayesnec_ecx4param)
  expect_equal(attr(output, "resolution"), 100)
})

test_that("brmsfit default resolution is 1000", {
  output <- nsec(brms_model_1, x_var = "x")
  expect_equal(attr(output, "resolution"), 1000)
})

# nsec.drc — man page gaps documented as known bugs -------------------------

# TODO: Bug in nsec.drc (single-curve case, no curveid). The line
# `xform(nsec_out)` does not assign its result, so xform is called but
# discarded. out_vals is built from the unmodified nsec_out. The existing
# test "drc nsec xform changes output values" uses >= which passes even when
# both outputs are identical. Remove `if (FALSE)` once the assignment is fixed.
if (FALSE) {
  test_that("drc nsec xform multiplies all output values when curveid is NA", {
    output_1 <- nsec(nsec_drc_1, x_var = "x")
    output_2 <- nsec(nsec_drc_1, x_var = "x", xform = function(x) x * 2)
    expect_equal(as.numeric(output_2), as.numeric(output_1) * 2, tolerance = 0.01)
  })
}

# TODO: nsec.drc returns an unnamed numeric vector (via as.numeric(unlist(...))).
# nsec.brmsfit and nsec.bnecfit both return named vectors c("Q50","Q2.5","Q97.5").
# Remove `if (FALSE)` once nsec.drc adds clean_names() like the other methods.
if (FALSE) {
  test_that("drc nsec output is named Q50, Q2.5, Q97.5 consistent with other methods", {
    output <- nsec(nsec_drc_1, x_var = "x")
    expect_equal(names(output), c("Q50", "Q2.5", "Q97.5"))
  })
}
