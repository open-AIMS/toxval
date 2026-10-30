# parameters --------------------------------------------------------------
# The argument checks in the nsec() generic run before dispatch, so the error
# tests in this section do not depend on the class of the fit.
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

test_that("hormesis_def is deprecated and ignored", {
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

test_that("x_range can be passed various values", {
  skip_if_not_installed("drc")
  expect_error(
    nsec(nsec_drc_1, x_range = c(100), x_var = "x"),
    regexp = "`x_range` must be length 2 not 1"
  )

  output_1 <- nsec(
    nsec_drc_1,
    x_control = 0.5,
    x_range = c(0.5, 100),
    x_var = "x",
    seed = 1
  )
  expect_equal(output_1$estimate, c(1.158826, NA_real_), tolerance = 0.001)

  output_2 <- nsec(
    brms_model_3,
    x_control = 0.5,
    x_range = c(0.5, 100),
    x_var = "x"
  )
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

  expect_equal(
    pillar::tbl_sum(output)[["Settings"]],
    "resolution 1000 | 1000 posterior draws"
  )
})


test_that("additional examples brms 2", {
  lifecycle::expect_deprecated(
    output <- nsec(
      brms_model_4,
      x_var = "x",
      by_group = TRUE,
      group_var = "herbicide"
    )
  )
  expect_snapshot({
    print(output, width = Inf)
  })
})

test_that("nsec works for drc using continuous data", {
  skip_if_not_installed("drc")
  model_LL4 <- drc::drm(y ~ x, data = bayesnec_nec_data, fct = drc::LL.4())
  output <- nsec(model_LL4, x_var = "x", seed = 1)

  expect_snapshot({
    print(output, width = Inf)
  })

  expect_equal(
    round(
      c(output$estimate[[1]], output$conf.low[[1]], output$conf.high[[1]]),
      1
    ),
    c(1.2, 1.0, 1.3)
  )
})

test_that("nsec for drc works for curveid and binomial data", {
  skip_if_not_installed("drc")
  daphnids_m2 <- drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )
  lifecycle::expect_deprecated(
    output <- nsec(daphnids_m2, x_var = "dose", curveid = "time", seed = 1)
  )

  expect_snapshot({
    print(output, width = Inf)
  })
})

test_that("nsec for drc returns an error for an nec model", {
  skip_if_not_installed("drc")
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
  skip_if_not_installed("drc")
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
  expect_equal(
    output$estimate[c(2, 4)],
    c(523.0816, 270.7910),
    tolerance = 0.001
  )

  expect_snapshot({
    print(output, width = Inf)
  })
})

test_that("nsec drc additional example 1", {
  skip_if_not_installed("drc")
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

test_that("nsec drc additional example 2", {
  skip_if_not_installed("drc")
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

  # the bootstrap quantiles differ between platforms in the third significant
  # figure (conf.high 0.440 on ubuntu-24.04, 0.441 on macOS), so the values are
  # snapshot and compared with a tolerance rather than printed
  expect_snapshot_value(output, style = "json2", tolerance = 0.01)
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
  pred <- compute_pred(
    list(rbind(10 + x_vec, 10 + x_vec)),
    x_vec,
    family = NULL
  )

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

test_that("brms model errors if x_var isn't in the dataset", {
  expect_error(
    nsec(brms_model_1, x_var = "z"),
    regexp = "`x_var` must name a column of the data in `object`, not 'z'"
  )
})


test_that("brms model errors if by_group is true but no group_var is supplied", {
  expect_error(
    suppressWarnings(nsec(brms_model_2, x_var = "x", by_group = TRUE)),
    regexp = "`group_var` must be supplied when `by_group` is TRUE"
  )
})

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
    suppressWarnings(
      nsec(brms_model_2, x_var = "x", group_var = "z", by_group = FALSE)
    ),
    regexp = "`by_group = FALSE` with a `group_var` is no longer supported"
  )
})

test_that("brms model runs when multiple variables in data and group_var specified", {
  output <- suppressWarnings(nsec(
    brms_model_2,
    x_var = "x",
    group_var = "z",
    by_group = TRUE
  ))

  expect_equal(
    output$estimate,
    c(NA_real_, 1.05, 1.05, 1.05),
    tolerance = 0.001
  )
  # In group 2, 9 decreasing and 8 increasing realisations start past the
  # reference, change away from the effect and pass to the other side without
  # returning, so they are censored (#74).
  expect_equal(
    output$conf.low,
    c(NA_real_, 1.05, 1.0008581, 0.9415798),
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

test_that("brms model throws deprecated message for horme and argument is ignored", {
  lifecycle::expect_deprecated(nsec(brms_model_3, x_var = "x", horme = TRUE))
})

test_that("a hormetic fit gives a decreasing and an increasing NSEC", {
  output <- nsec(brms_model_5, x_var = "x")

  expect_equal(output$direction, c("decreasing", "increasing"))
  expect_equal(output$estimate[[1]], 2.2916, tolerance = 0.001)
  expect_equal(output$estimate[[2]], 0.1635, tolerance = 0.001)
})

# The expected values use observed order statistics when draws are censored.
test_that("nsec censors the brms_model_1 realisations that show no effect (#74)", {
  pred <- toxval_predict(brms_model_1, x_var = "x")
  output <- suppressWarnings(nsec(pred, draws = TRUE))

  # 10 realisations start at or below the decreasing reference without
  # crossing it; 7 rise, away from the effect, and pass above it without
  # returning, so they are censored.
  past <- pred$curves[[1]][, 1] <= output$reference[[1]]
  expect_equal(sum(past), 10)
  expect_equal(sum(is.infinite(output$draws[[1]]$value.draw[past])), 7)
  expect_equal(
    unlist(output[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 1.05, conf.low = 0.9911504, conf.high = 1.05),
    tolerance = 1e-6
  )
})

test_that("nsec censors the brms_model_6 realisations that show no effect (#74)", {
  pred <- toxval_predict(brms_model_6, x_var = "x")
  output <- suppressWarnings(nsec(pred, draws = TRUE))

  # all 10 realisations at or below the decreasing reference rise and pass
  # above it without returning, so they are censored.
  past <- pred$curves[[1]][, 1] <= output$reference[[1]]
  expect_equal(sum(past), 10)
  expect_equal(sum(is.infinite(output$draws[[1]]$value.draw[past])), 10)
  expect_equal(
    unlist(output[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 1.05, conf.low = 0.9696808, conf.high = 1.05),
    tolerance = 1e-6
  )
})

test_that("nsec posterior is defunct, draws replaced it", {
  lifecycle::expect_defunct(
    nsec(brms_model_2, x_var = "x", posterior = TRUE, group_var = "z")
  )
})

test_that("brms model when posterior is true and group_var is na", {
  output <- suppressWarnings(nsec(
    brms_model_3,
    x_var = "x",
    by_group = FALSE,
    draws = TRUE,
    group_var = NA
  ))

  expect_equal(nrow(output$draws[[1]]), 1000)
  expect_equal(output$sig_val, c(0.01, 0.01))
  expect_equal(output$metric, c("nsec", "nsec"))
  expect_named(output$draws[[1]], c(".draw", "value.draw", "ecnsec.draw"))
  expect_false("group" %in% names(output))
})

# drc ---------------------------------------------------------------------

test_that("drc model can read x_var from the fit", {
  skip_if_not_installed("drc")
  expect_no_error(nsec(nsec_drc_1))
})

test_that("drc model errors if wrong x_var is supplied", {
  skip_if_not_installed("drc")
  expect_error(
    nsec(nsec_drc_1, x_var = "a"),
    regexp = "`x_var` must be 'x', the dose variable of `object`, not 'a'"
  )
})

test_that("drc model errors if more then 1 sig val is passed", {
  skip_if_not_installed("drc")
  expect_error(
    nsec(nsec_drc_1, x_var = "x", sig_val = c(0.1, 0.2)),
    regexp = "`sig_val` must be a number \\(non-missing numeric scalar\\)"
  )
})

test_that("drc model output attributes", {
  skip_if_not_installed("drc")
  output <- nsec(nsec_drc_1, x_var = "x", seed = 1)
  output <- output[1, ]

  expect_equal(
    c(output$estimate, output$conf.low, output$conf.high),
    c(1.183818, 0.995025, 1.284760),
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
  skip_if_not_installed("drc")
  fit <- drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )

  lifecycle::expect_deprecated(
    output <- nsec(fit, curveid = "time", seed = 1)
  )
  expect_identical(output, nsec(fit, group_var = "time", seed = 1))
})

test_that("drc curveid must name the curveid column of the fit", {
  skip_if_not_installed("drc")
  expect_error(
    suppressWarnings(nsec(nsec_drc_1, curveid = "a")),
    "`group_var` must name a column of the data in `object`, not 'a'"
  )
})

test_that("drc model curveid switches attributes to have a matrix for ecnsec_relativeP", {
  skip_if_not_installed("drc")
  output <- nsec(nsec_drc_1, x_var = "x", seed = 1)

  expect_equal(output$estimate[[1]], 1.183707, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.994483, tolerance = 0.001)
  expect_equal(output$conf.high[[1]], 1.284687, tolerance = 0.001)
  expect_equal(output$ecnsec[[1]], 0.016873, tolerance = 0.001)
})

test_that("drc model provides NA estimates when no realisation crosses the reference", {
  skip_if_not_installed("drc")
  # SE(e) exceeds e in nsec_drc_2; e is drawn on the log scale, so no
  # replicate is excluded, but none crosses the reference in either direction
  expect_no_warning(output <- nsec(nsec_drc_2, seed = 1, draws = TRUE))

  expect_equal(output$direction, c("decreasing", "increasing"))
  expect_equal(output$estimate, c(NA_real_, NA_real_))
  for (row_draws in output$draws) {
    expect_false(anyNA(row_draws$value.draw))
  }
})

# nsec.brmsfit — additional tests ------------------------------------------

# Realisations that start past the reference, change away from the effect and
# pass to the other side are censored (#74). No realisation of group 1 crosses
# in the decreasing direction, so that row is NA.
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

  expect_equal(
    pillar::tbl_sum(output)[["Settings"]],
    "resolution 600 | 1000 posterior draws"
  )
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

  expect_equal(output_1$estimate * 10, output_2$estimate)
  expect_equal(output_1$conf.low * 10, output_2$conf.low)
  expect_equal(output_1$conf.high * 10, output_2$conf.high)

  expect_equal(output_1$reference, output_2$reference)
  expect_equal(output_1$ecnsec, output_2$ecnsec)
  expect_equal(output_1$ecnsec.low, output_2$ecnsec.low)
  expect_equal(output_1$ecnsec.high, output_2$ecnsec.high)
})

test_that("a decreasing xform mirrors a censored NSEC and its draws (#69)", {
  x_vec <- 0:10
  pred <- compute_pred(list(censored_nsec_curves(x_vec)), x_vec)

  ident <- suppressWarnings(nsec(pred, draws = TRUE))
  neg <- suppressWarnings(nsec(pred, draws = TRUE, xform = function(x) -x))

  expect_equal(neg$estimate[[1]], -ident$estimate[[1]])
  expect_equal(neg$conf.low[[1]], -ident$conf.high[[1]])
  expect_equal(neg$conf.high[[1]], -ident$conf.low[[1]])
  expect_identical(
    neg$draws[[1]]$value.draw,
    -ident$draws[[1]]$value.draw
  )
})

# nsec.drc — additional tests ---------------------------------------------

test_that("nsec drc model has expected structure and shape", {
  skip_if_not_installed("drc")
  output <- nsec(
    nsec_drc_1,
    x_var = "x",
    sig_val = 0.05,
    resolution = 600,
    seed = 1
  )

  expect_named(
    output,
    c(
      "metric",
      "direction",
      "estimate",
      "conf.low",
      "conf.high",
      "sig_val",
      "type",
      "anchor",
      "control",
      "reference",
      "asymptote",
      "ecnsec",
      "ecnsec.low",
      "ecnsec.high"
    )
  )

  expect_snapshot({
    print(output, width = Inf)
  })
})

test_that("drc nsec xform changes output values", {
  skip_if_not_installed("drc")
  output_1 <- nsec(nsec_drc_1, x_var = "x", seed = 1)
  output_2 <- nsec(nsec_drc_1, x_var = "x", xform = function(x) x * 2, seed = 1)

  expect_equal(output_1$estimate * 2, output_2$estimate)
  expect_equal(output_1$conf.low * 2, output_2$conf.low)
  expect_equal(output_1$conf.high * 2, output_2$conf.high)

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
})


test_that("nsec errors when a toxval_pred is given predict-level arguments", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 11 - x_vec)), x_vec)

  expect_error(
    nsec(pred, resolution = 100, x_range = c(0, 5)),
    "so `resolution`, `x_range` must not be supplied"
  )
  expect_error(
    nsec(pred, x_control = 1),
    "so `x_control` must not be supplied"
  )
})

test_that("curveid must not be supplied with group_var", {
  skip_if_not_installed("drc")
  expect_error(
    suppressWarnings(nsec(nsec_drc_1, curveid = "time", group_var = "time")),
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
    unlist(
      output[1, c("ecnsec", "ecnsec.low", "ecnsec.high")],
      use.names = FALSE
    ),
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
  # so its increasing reference and ecnsec are defined
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

test_that("nsec relative takes ecnsec as a proportion of the span to the asymptote", {
  # with an asymptote of 0 the decreasing span is the control, as under absolute
  absolute <- suppressWarnings(
    nsec(brms_model_1, x_var = "x", resolution = 50)
  )
  relative <- suppressWarnings(
    nsec(
      brms_model_1,
      x_var = "x",
      type = "relative",
      asymptote = c(decreasing = 0),
      resolution = 50
    )
  )

  expect_equal(relative$ecnsec[[1]], absolute$ecnsec[[1]])
  expect_equal(relative$estimate[[1]], absolute$estimate[[1]])
})

# multivariate fits ------------------------------------------------------------

# The deprecated nsec_multi() gave, for this fit:
#                          lower,     estimate,  upper      reference:
#   sp_survival decreasing 0.2026466, 0.4624833, 0.6802316, 0.9208626
#   sp_survival increasing 0.001,     1.5,       1.5,       1.0
#   sp_growth   decreasing 0.4181719, 0.8688531, 1.5,       0.8209478
#   sp_growth   increasing 1.5,       1.5,       1.5,       2.350757
# The references agree. nsec() places or censors a realisation with no crossing
# rather than setting it to max(x_vec) (#60). sp_survival increasing has no
# realisation that crosses its reference, so it is NA. In sp_growth decreasing,
# 10 realisations start below the reference without crossing it. 5 rise, away
# from the effect, and pass above the reference without returning, so they are
# censored (#74).
test_that("nsec of a multivariate fit against the deprecated nsec_multi", {
  output <- suppressWarnings(
    nsec(nsec_multi_model_2, x_var = "dose", resolution = 50)
  )

  expect_equal(
    output$estimate,
    c(0.4615262, NA, 0.8663563, 1.5),
    tolerance = 1e-6
  )
  expect_equal(
    output$conf.low,
    c(0.1559251, NA, 0.4073518, 1.5),
    tolerance = 1e-6
  )
  expect_equal(
    output$conf.high,
    c(0.6570357, NA, 1.5, 1.5),
    tolerance = 1e-6
  )
  expect_equal(
    output$reference,
    c(0.9208626, 1, 0.8209478, 2.350757),
    tolerance = 1e-6
  )
})

test_that("nsec gives each response its own realisations", {
  output <- suppressWarnings(
    nsec(nsec_multi_model_2, x_var = "dose", resolution = 50, draws = TRUE)
  )

  expect_equal(vapply(output$draws, nrow, integer(1)), rep(1000L, 4))
  expect_false(identical(
    output$draws[[1]]$value.draw,
    output$draws[[3]]$value.draw
  ))
})

test_that("nsec of a multivariate toxval_pred matches nsec of the fit", {
  pred <- toxval_predict(nsec_multi_model_2, x_var = "dose", resolution = 50)

  expect_identical(
    suppressWarnings(nsec(pred)),
    suppressWarnings(
      nsec(nsec_multi_model_2, x_var = "dose", resolution = 50)
    )
  )
})
