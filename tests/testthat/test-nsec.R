# Tests whose values or behaviour are expected to change during the refactor
# have a `# EXPECTED-CHANGE` comment giving the reason and the issue. If a test
# without one changes, the developer should double check the change was
# intentional.
#
# Two changes are deliberately not marked, because each would alter almost every
# test in this file: the phase-5 switch to a toxval tibble (#4), and the
# replacement of the evaluation grid with root-finding (#40), which is not yet
# in the refactor plan.

# parameters --------------------------------------------------------------
# The argument checks in the nsec() generic run before dispatch, so the error
# tests in this section do not depend on the class of the fit. They were written
# against bayesnec fits and rewritten to use brms_model_1 when the bnecfit method
# was removed (#39). - done
test_that("prob_vals warnings behave as expected", {
  expect_error(
    nsec(brms_model_1, x_var = "x", prob_vals = 0.9, resolution = 10),
    regexp = "`prob_vals` must be length 3 not 1"
  )

  expect_error(
    nsec(
      brms_model_1,
      x_var = "x",
      prob_vals = c(0.6, 0.9, 0.1),
      resolution = 10
    ),
    regexp = "`prob_vals` must be the central, lower and upper quantiles, in that order, not 0.6, 0.9 and 0.1"
  )
  
  expect_error(
    nsec(
      brms_model_1,
      x_var = "x",
      prob_vals = c(0.4, 0.1, 0.6, 0.7, 0.05)
    ),
    regexp = "`prob_vals` must be length 3 not 5"
  )
})

# EXPECTED-CHANGE hormesis_def removed, direction becomes a result column #20 - DONE
test_that("hormesis_def is now deprecated and ignored", {
  lifecycle::expect_deprecated(
    nsec(brms_model_3, x_var = "x", hormesis_def = "other")
  )
})

test_that("xform errors if function not passed ", {
  expect_error(
    nsec(brms_model_1, x_var = "x", xform = 1),
    regexp = "`xform` must be a function"
  )
})

# EXPECTED-CHANGE x_range will be accepted as a range #10 - DONE
test_that("x_range can be passed various values", {
  expect_error(
    nsec(nsec_drc_1, x_range = c(100), x_var = "x"),
    regexp = "`x_range` must be length 2 not 1"
  )
  
  output_1 <- nsec(nsec_drc_1, x_range = c(0.5, 100), x_var = "x", seed = 1)
  expect_equal(output_1$estimate, c(1.158826, NA_real_), tolerance = 0.001)
  
  output_2 <- nsec(brms_model_3, x_range = c(0.5, 100), x_var = "x", seed = 1)
  expect_equal(output_2$estimate, c(1.3688852, 0.6452353), tolerance = 0.001)
})

test_that("nsec type = range changes ecnsec but not the NSEC", {
  absolute <- nsec(brms_model_3, x_var = "x")
  range <- nsec(brms_model_3, x_var = "x", type = "range")
  
  expect_equal(range$type, c("range", "range"))
  expect_equal(range$estimate, absolute$estimate)
  expect_equal(range$reference, absolute$reference)
  expect_equal(range$ecnsec[[1]], 0.037344, tolerance = 0.001)
  expect_equal(range$ecnsec.low[[1]], 0.006111, tolerance = 0.001)
  expect_equal(range$ecnsec.high[[1]], 0.062751, tolerance = 0.001)
  
  ecnsec_cols <- c("ecnsec", "ecnsec.low", "ecnsec.high")
  expect_false(isTRUE(all.equal(range[ecnsec_cols], absolute[ecnsec_cols])))
})

# different model types ---------------------------------------------------

# EXPECTED-CHANGE ecnsec reference redefined, ecnsec values change (nsec estimate unchanged) #49 - DONE
# Values the same for est, conf, but not for the ecnsec because they used which were measured from the curves max
test_that("additional examples brms 1", {
  output <- nsec(brms_model_3, x_var = "x", resolution = 1000)
  
  expect_type(output$estimate, "double")
  expect_equal(output$estimate[[1]], 0.2576897, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.1164777, tolerance = 0.001) 
  expect_equal(output$conf.high[[1]], 1.2818, tolerance = 0.001)
  expect_equal(output$metric, c("nsec", "nsec"))
  expect_equal(output$sig_val, c(0.01, 0.01))
  expect_equal(output$direction, c("decreasing", "increasing"))
  expect_equal(output$ecnsec[[1]], 0.03685049, tolerance = 0.001)
  expect_equal(output$ecnsec.low[[1]], 0.006062251, tolerance = 0.001) 
  expect_equal(output$ecnsec.high[[1]], 0.06172548, tolerance = 0.001)
  
  expect_equal(pillar::tbl_sum(output)[["Settings"]], "resolution 1000 | 1000 posterior draws")
})




test_that("additional examples brms 2", {
  output <- nsec(
    brms_model_4,
    x_var = "x",
    by_group = TRUE,
    group_var = "herbicide"
  )
  expect_snapshot({
    print(output, width = Inf)
  })
})

# EXPECTED-CHANGE drc estimate changes (parametric bootstrap) #43; needs a seed - done
test_that("nsec works for drc using continuous data", {
  model_LL4 <- drc::drm(y ~ x, data = bayesnec_nec_data, fct = drc::LL.4())
  output <- nsec(model_LL4, x_var = "x", seed = 1)
  
  expect_snapshot({
    print(output, width = Inf)
  })
  
  expect_equal(
    round(c(output$estimate[[1]], output$conf.low[[1]], output$conf.high[[1]]), 1), 
    c(1.2, 1.0, 1.3)
  )
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
  output <- nsec(daphnids_m2, x_var = "dose", curveid = "time", seed = 1)
  
  expect_snapshot({
    print(output, width = Inf)
  })
})

test_that("nsec for drc returns an error for an nec model", {
  rye_NEC4 <- drc::drm(
    rootl ~ conc,
    data = drc::ryegrass,
    fct = drc::NEC.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))
  )
  expect_error(
    nsec(rye_NEC4, x_var = "conc"),
    "`object` must not be a `drc` NEC model; NSECs are estimated for smooth curves only"
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
  
  output <- nsec(daphnids_m1, group_var = "time", seed = 1)
  
  expect_equal(output$estimate[c(1, 3)], c(NA_real_, NA_real_))
  expect_equal(output$estimate[c(2, 4)], c(513.0672, 274.9758), tolerance = 0.001)
  
  expect_snapshot({
    print(output, width = Inf)
  })
})

# EXPECTED-CHANGE drc bootstrap changes estimate; ecnsec redefined #49; needs a seed - done
test_that("nsec drc additional example 1", {
  data <- bayesnec_herbicide[bayesnec_herbicide$herbicide == "ametryn", ]
  data$concentration <- sqrt(data$concentration)
  output_drc <- drc::drm(
    fvfm ~ concentration,
    data = data,
    fct = drc::LL.3(names = c("Slope", "Upper Limit", "Midpoint"))
  )
  output <- nsec(output_drc, x_var = "concentration", seed = 1)

  expect_equal(
    c(output$estimate[[1]], output$conf.low[[1]], output$conf.high[[1]]),
    c(0.38956, 0.33727, 0.42759),
    tolerance = 0.01
  )
  
  expect_snapshot({
    print(output, width = Inf)
  })
})

# EXPECTED-CHANGE drc bootstrap changes estimate; ecnsec redefined #49; needs a seed - done
test_that("nsec drc additional example 2", {
  data <- bayesnec_herbicide[bayesnec_herbicide$herbicide == "ametryn", ]
  data$concentration <- sqrt(data$concentration)
  output_drc <- drc::drm(
    fvfm ~ concentration,
    data = data,
    fct = drc::LN.4(
      names = c("Slope", "Lower Limit", "Upper Limit", "Midpoint")
    )
  )
  output <- nsec(output_drc, x_var = "concentration", seed = 1)

  expect_equal(
    c(output$estimate[[1]], output$conf.low[[1]], output$conf.high[[1]]),
    c(0.4018911, 0.3380835, 0.4405638),
    tolerance = 0.01
  )
  
  expect_snapshot({
    print(output, width = Inf)
  })
})

test_that("nsec accepts a toxval_pred", {
  output <- toxval_predict(brms_model_3, x_var = "x")
  
  expect_snapshot(print(nsec(output), width = Inf))
})

test_that("nsec gives NA when no realisation has a finite control", {
  x_vec <- 0:10
  y <- 10 - x_vec
  y[1] <- NaN
  pred <- compute_pred(list(rbind(y, y)), x_vec)
  
  expect_warning(output <- nsec(pred), "decreasing: 100.0% excluded")
  expect_equal(output$reference, c(NA_real_, NA_real_))
  expect_equal(output$estimate, c(NA_real_, NA_real_))
})

test_that("a toxval_pred with no family has no upper bound on the response", {
  x_vec <- 0:10
  # This can't occur through the methods created inside the package but if 
  # externals build a toxval_pred this helps to check that behaviour
  pred <- compute_pred(list(rbind(10 + x_vec, 10 + x_vec)), x_vec, family = NULL)
  
  output <- ecx(pred)
  
  # relative deviation: a 10 per cent increase from a control of 10 is 11
  expect_equal(output$reference[[2]], 11)
  expect_equal(output$estimate[[2]], 1)
})

# brms --------------------------------------------------------------------

test_that("brms model errors without x_var", {
  expect_error(
    nsec(brms_model_1),
    regexp = "`x_var` must be supplied for a `brmsfit`"
  )
})

# EXPECTED-CHANGE enable once x_var is validated before x_range #55 - done
  test_that("brms model errors if x_var isn't in the dataset", {
    expect_error(
      nsec(brms_model_1, x_var = "z"),
      regexp = "`x_var` must name a column of the data in `object`, not 'z'"
    )
  })


test_that("brms model errors if by_group is true but no group_var is supplied", {
  expect_error(
    nsec(brms_model_2, x_var = "x", by_group = TRUE),
    regexp = "`group_var` must be supplied when `by_group` is TRUE"
  )
})

# EXPECTED-CHANGE cleaner error when extra predictors require group_var #56 - DONE
test_that("brms model errors if there are additional groups but aren't specified", {
  expect_error(
    nsec(brms_model_2, x_var = "x"),
    regexp = "`object` needs a value of 'z' to predict; name it as `x_var`, or as `group_var` to get one curve per level"
  )
})

test_that("brms model errors if group_var is not in the dataset", {
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = "a"),
    regexp = "group_var` must name a column of the data in `object`, not 'a'"
  )
})

test_that("brms model errors if group_var is not in the dataset when non-character types passed", {
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = 1),
    regexp = "`group_var` must be a string \\(non-missing character scalar\\) or NULL"
  )
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = TRUE),
    regexp = "`group_var` must be a string \\(non-missing character scalar\\) or NULL"
  )
})

test_that("brms model errors if by_group is FALSE and a group_var is supplied", {
  expect_error(
    nsec(brms_model_2, x_var = "x", group_var = "z", by_group = FALSE),
    regexp = "`by_group = FALSE` with a `group_var` is no longer supported"
  )
})

# EXPECTED-CHANGE ecnsec reference redefined, ecnsec values change #49 - done
test_that("brms model runs when multiple variables in data and group_var specified", {
  output <- suppressWarnings(nsec(
    brms_model_2, x_var = "x", group_var = "z", by_group = TRUE
  ))

  expect_equal(
    output$estimate,
    c(NA_real_, 1.05, 1.05, 1.05),
    tolerance = 0.001
  )
  expect_equal(
    output$conf.low,
    c(NA_real_, 1.05, 0.97893, 0.92054),
    tolerance = 0.001
  )
  expect_equal(
    output$conf.high,
    c(NA_real_, 1.05, 1.05, 1.05),
    tolerance = 0.001
  )
  expect_equal(
    output$direction,
    rep(c("decreasing", "increasing"), 2)
  )
  expect_equal(
    output$group,
    factor(c("1", "1", "2", "2"))
  )
})

# EXPECTED-CHANGE horme/hormesis_def removed, direction becomes a result column #20 - done
test_that("brms model throws deprecated message for horme and argument is ignored", {
  lifecycle::expect_deprecated(nsec(brms_model_3, x_var = "x", horme = TRUE))
})

# Golden values updated when the hormetic reference was fixed to max over
# draws (MARGIN = 1) rather than over x values (MARGIN = 2). The old
# expectations were nsec_fct()'s max(x_vec) fallback, not estimates. - DONE
# EXPECTED-CHANGE horme/hormesis_def removed #20; ecnsec redefined #49
test_that("a hormetic fit gives a decreasing and an increasing NSEC", {
  output <- nsec(brms_model_5, x_var = "x")
  
  expect_equal(output$direction, c("decreasing", "increasing"))
  expect_equal(output$estimate[[1]], 2.2916, tolerance = 0.001)
  expect_equal(output$estimate[[2]], 0.1635, tolerance = 0.001)
})

test_that("brms model posterior is true still works but is deprecated, draws replaced it", {
  output_1 <- suppressWarnings(nsec(
    brms_model_2,
    x_var = "x",
    by_group = TRUE,
    posterior = TRUE,
    group_var = "z"
  ))
  expect_s3_class(output_1, "data.frame")
  expect_true("draws" %in% colnames(output_1))
  expect_equal(dim(output_1$draws[[1]]), c(1000, 3))
  
  output_2 <- suppressWarnings(nsec(
    brms_model_2,
    x_var = "x",
    by_group = TRUE,
    draws = TRUE,
    group_var = "z"
  ))
  
  expect_identical(output_1, output_2)
})

test_that("brms model when posterior is true and group_var is na", {
  output <- nsec(
    brms_model_3,
    x_var = "x",
    by_group = FALSE,
    draws = TRUE,
    group_var = NA
  )

  expect_equal(nrow(output$draws[[1]]), 1000)
  expect_equal(output$sig_val, c(0.01, 0.01))
  expect_equal(output$metric, c("nsec", "nsec"))
  expect_named(output$draws[[1]], c(".draw", "value.draw", "ecnsec.draw"))
  expect_false("group" %in% names(output))
})

# drc ---------------------------------------------------------------------

test_that("drc model can read x_var from the fit", {
  expect_no_error(nsec(nsec_drc_1))
})

# EXPECTED-CHANGE clearer error when x_var absent (curveid/columns by name) #34 - done
test_that("drc model errors if wrong x_var is supplied", {
  expect_error(
    nsec(nsec_drc_1, x_var = "a"),
    regexp = "`x_var` must be 'x', the dose variable of `object`, not 'a'"
  )
})

test_that("drc model errors if more then 1 sig val is passed", {
  expect_error(
    nsec(nsec_drc_1, x_var = "x", sig_val = c(0.1, 0.2)),
    regexp = "`sig_val` must be a number \\(non-missing numeric scalar\\)"
  )
})

# EXPECTED-CHANGE drc bootstrap changes estimate #43; ecnsec redefined #49; needs a seed
test_that("drc model output attributes", {
  output <- nsec(nsec_drc_1, x_var = "x", seed = 1)
  output <- output[1,]
  
  expect_equal(
    c(output$estimate, output$conf.low, output$conf.high),
    c(1.183707, 0.99448, 1.2846),
    tolerance = 0.0001
  )
  expect_equal(output$control, 0.897248, tolerance = 0.0001)
  expect_equal(output$reference, 0.8821086, tolerance = 0.0001)
  expect_equal(output$ecnsec, 0.0168731, tolerance = 0.0001)
  expect_equal(output$ecnsec.low, 0.003576807, tolerance = 0.0001)
  expect_equal(output$ecnsec.high, 0.03038984, tolerance = 0.0001)
  expect_equal(output$sig_val, 0.01, tolerance = 0.0001)
})

test_that("drc curveid is translated to group_var", {
  fit <- drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )
  
  expect_identical(
    nsec(fit, curveid = "time", seed = 1),
    nsec(fit, group_var = "time", seed = 1)
  )
})

test_that("drc curveid must name the curveid column of the fit", {
  expect_error(
    nsec(nsec_drc_1, curveid = "a"),
    "`group_var` must name a column of the data in `object`, not 'a'"
  )
})

# EXPECTED-CHANGE drc bootstrap #43; ecnsec redefined #49; curveid selected by name #34; needs a seed
test_that("drc model curveid switches attributes to have a matrix for ecnsec_relativeP", {
  output <- nsec(nsec_drc_1, x_var = "x", seed = 1)
  
  expect_equal(output$estimate[[1]], 1.183707, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.994483, tolerance = 0.001)
  expect_equal(output$conf.high[[1]], 1.284687, tolerance = 0.001)
  expect_equal(output$ecnsec[[1]], 0.016873, tolerance = 0.001)
})

test_that("drc model provides NA estimates when draws are excluded and not enough info to generate", {
  warnings <- capture_warnings(output <- nsec(nsec_drc_2, seed = 1))
  
  expect_match(warnings, "NaNs produced", all = FALSE)
  expect_match(warnings, "increasing: 24.6% excluded", all = FALSE, fixed = TRUE)
  expect_equal(output$direction, c("decreasing", "increasing"))
  expect_equal(output$estimate[[2]], NA_real_)
})

# nsec.brmsfit — additional tests ------------------------------------------

test_that("brms grouped nsec with draws has one row per group and direction", {
  output <- suppressWarnings(
    nsec(brms_model_2, x_var = "x", group_var = "z", draws = TRUE)
  )
  
  expect_snapshot({
    print(output, width = Inf)
    output$draws[[1]]
  })
})

test_that("brmsfit nsec resolution is stored correctly", {
  output <- nsec(brms_model_3, x_var = "x", resolution = 600)

  expect_equal(pillar::tbl_sum(output)[["Settings"]], "resolution 600 | 1000 posterior draws")
})

test_that("a larger sig_val moves the decreasing reference towards the control", {
  strict <- nsec(brms_model_3, x_var = "x", sig_val = 0.001)
  loose <- suppressWarnings(nsec(brms_model_3, x_var = "x", sig_val = 0.1))
  
  expect_gt(loose$reference[[1]], strict$reference[[1]])
  expect_lt(loose$estimate[[1]], strict$estimate[[1]])
})

test_that("brmsfit nsec xform changes output values with grouping", {
  output_1 <- suppressWarnings(
    nsec(brms_model_2, x_var = "x", group_var = "z", by_group = TRUE)
  )
  output_2 <- suppressWarnings(nsec(
    brms_model_2,
    x_var = "x",
    group_var = "z",
    by_group = TRUE,
    xform = function(x) x * 10
  ))

  expect_equal(output_1$estimate*10, output_2$estimate)
  expect_equal(output_1$conf.low*10, output_2$conf.low)
  expect_equal(output_1$conf.high*10, output_2$conf.high)
  
  expect_equal(output_1$reference, output_2$reference)
  expect_equal(output_1$ecnsec, output_2$ecnsec)
  expect_equal(output_1$ecnsec.low, output_2$ecnsec.low)
  expect_equal(output_1$ecnsec.high, output_2$ecnsec.high)
})

# nsec.drc — additional tests ---------------------------------------------

# EXPECTED-CHANGE enable once output is the toxval tibble (uniform naming) #4
test_that("nsec drc model has expected structure and shape", {
  output <- nsec(nsec_drc_1, x_var = "x", sig_val = 0.05, resolution = 600, seed = 1)

  expect_named(
    output,
    c("metric", "direction", "estimate", "conf.low", "conf.high", "sig_val",
      "type", "anchor", "control", "reference", "ecnsec", "ecnsec.low",
      "ecnsec.high")
  )
  
  expect_snapshot({
    print(output, width = Inf)
  })
})

# EXPECTED-CHANGE enable once drc xform assignment is fixed #57 - done
test_that("drc nsec xform changes output values", {
  output_1 <- nsec(nsec_drc_1, x_var = "x", seed = 1)
  output_2 <- nsec(nsec_drc_1, x_var = "x", xform = function(x) x * 2, seed = 1)

  expect_equal(output_1$estimate*2, output_2$estimate)
  expect_equal(output_1$conf.low*2, output_2$conf.low)
  expect_equal(output_1$conf.high*2, output_2$conf.high)
  
  expect_equal(output_1$reference, output_2$reference)
  expect_equal(output_1$ecnsec, output_2$ecnsec)
  expect_equal(output_1$ecnsec.low, output_2$ecnsec.low)
  expect_equal(output_1$ecnsec.high, output_2$ecnsec.high)
})

# nsec input validation ----------------------------------------------------

test_that("nsec input validation catches non-numeric sig_val", {
  expect_error(
    nsec(brms_model_3, x_var = "x", sig_val = "strict"),
    "`sig_val` must be a number \\(non-missing numeric scalar\\)"
  )
})

test_that("nsec input validation catches non-numeric resolution", {
  expect_error(
    nsec(brms_model_1, x_var = "x", resolution = "high"),
    "`resolution` must be a whole number \\(non-missing integer scalar or double equivalent\\)"
  )
})

test_that("nsec input validation catches non-logical draws", {
  expect_error(
    nsec(brms_model_1, x_var = "x", draws = "yes"),
    "`draws` must be a flag \\(TRUE or FALSE\\)"
  )
  
  expect_error(
    lifecycle::expect_deprecated(
      nsec(brms_model_1, x_var = "x", posterior = "yes")
    ),
    "`posterior` must be a flag \\(TRUE or FALSE\\)"
  )
})


test_that("nsec errors when a toxval_pred is given predict-level arguments", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 11 - x_vec)), x_vec)
  
  expect_error(
    nsec(pred, resolution = 100, x_range = c(0, 5)),
    "so `resolution`, `x_range` must not be supplied"
  )
})

test_that("curveid must not be supplied with group_var", {
  expect_error(
    nsec(nsec_drc_1, curveid = "time", group_var = "time"),
    "`curveid` must not be supplied with `group_var`"
  )
})

test_that("anchor is model or component and is recorded in the result", {
  model <- nsec(brms_model_3, x_var = "x")
  component <- nsec(brms_model_3, x_var = "x", anchor = "component")
  
  expect_equal(model$anchor, c("model", "model"))
  expect_equal(component$anchor, c("component", "component"))
  # the two coincide for a fit that is not model-averaged
  expect_equal(component$estimate, model$estimate)
  
  expect_error(
    nsec(brms_model_3, x_var = "x", anchor = "control"),
    "`anchor` must match 'component' or 'model', not 'control'"
  )
})

test_that("nsec gives NA ecnsec when no realisation has a span", {
  x_vec <- 0:10
  # rising curves have no decreasing span under range
  pred <- compute_pred(list(rbind(10 + x_vec, 11 + x_vec)), x_vec)
  
  output <- suppressWarnings(nsec(pred, type = "range"))
  
  expect_equal(
    unlist(output[1, c("ecnsec", "ecnsec.low", "ecnsec.high")], use.names = FALSE),
    c(NA_real_, NA_real_, NA_real_)
  )
})

# Multivariate fits -------------------------------------------------------

test_that("nsec gives rows for each response of a multivariate brmsfit", {
  result <- suppressWarnings(
    nsec(nsec_multi_model_2, x_var = "dose", resolution = 50)
  )

  expect_identical(
    result$response,
    factor(
      c("spsurvival", "spsurvival", "spgrowth", "spgrowth"),
      levels = c("spsurvival", "spgrowth")
    )
  )
  expect_identical(result$direction, rep(c("decreasing", "increasing"), 2))
})

test_that("nsec takes extra risk for a binomial response", {
  # predicted at one trial, a binomial response is a proportion bounded at 1,
  # so its increasing reference and ecnsec are defined (T26)
  result <- suppressWarnings(
    nsec(brms_model_7, x_var = "x", resolution = 50)
  )
  row <- result[
    result$response == "survived" & result$direction == "increasing",
  ]

  expect_true(is.finite(row$reference))
  expect_lte(row$reference, 1)
  expect_true(is.finite(row$ecnsec))
})
