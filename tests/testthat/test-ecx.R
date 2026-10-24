# Tests whose values or behaviour are expected to change during the refactor
# have a `# EXPECTED-CHANGE` comment giving the reason and the issue. If a test
# without one changes, the developer should double check the change was
# intentional.
#
# Two changes are deliberately not marked, because each would alter almost every
# test in this file: the phase-5 switch to a toxval tibble (#4), and the
# replacement of the evaluation grid with root-finding (#40), which is not yet
# in the refactor plan.

# brms --------------------------------------------------------------------
test_that("outputs ecx value", {
  output <- suppressWarnings(ecx(brms_model_1, x_var = "x", resolution = 1000))
  settings <- pillar::tbl_sum(output)[["Settings"]]
  
  expect_type(output$estimate, "double")
  expect_equal(output$estimate[[1]], 0.8320, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.820, tolerance = 0.001) # EXPECT-CHANGE delete commment the small change here comes from the tolerance not being strict enough, 
  expect_equal(output$conf.high[[1]], 1.0500, tolerance = 0.001)
  expect_equal(output$metric, c("ecx", "ecx"))
  expect_equal(output$proportion, c(0.1, 0.1))
  expect_equal(settings, "resolution 1000 | 1000 posterior draws")
  expect_equal(output$direction, c("decreasing", "increasing"))
})

test_that("ecx_val changes when different value provided", {
  output_1 <- suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = 50))
  
  expect_equal(output_1$estimate[[1]], 0.975, tolerance = 0.001)
  expect_equal(output_1$conf.low[[1]], 0.904, tolerance = 0.001)
  expect_equal(output_1$conf.high[[1]], 1.05, tolerance = 0.001)
  expect_equal(output_1$proportion, c(0.50, 0.50))
  
  output_2 <- suppressWarnings(ecx(brms_model_1, x_var = "x", proportion = 0.50))
  
  expect_equal(output_2$estimate[[1]], 0.975, tolerance = 0.001)
  expect_equal(output_2$conf.low[[1]], 0.904, tolerance = 0.001)
  expect_equal(output_2$conf.high[[1]], 1.05, tolerance = 0.001)
  expect_equal(output_2$proportion, c(0.50, 0.50))
  
  expect_identical(output_1, output_2)
})

test_that("check ecx_val must be a numeric scalar and gives deprecation warning", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)
  lifecycle::expect_deprecated(ecx(pred, ecx_val = 10))
  
  expect_length(
    suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = 50L)), 
    10
  )
  expect_length(
    suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = 50)), 
    10
  )
  expect_error(
    suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = c(50, 10, 2))),
    "`ecx_val` must be a number"
  )
  expect_error(
    suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = "50")),
    "`ecx_val` must be a number"
  )
  expect_error(
    suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = TRUE)),
    "`ecx_val` must be a number"
  )
})

test_that("resolution changes when different value is provided", {
  output <- suppressWarnings(ecx(brms_model_1, x_var = "x", resolution = 2))

  expect_equal(output$estimate[[1]], 0.836, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.822, tolerance = 0.001)
  expect_equal(output$conf.high[[1]], 1.050, tolerance = 0.001)
  expect_match(pillar::tbl_sum(output)[["Settings"]], "resolution 2")
})

test_that("proper resolution values can be passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = 1),
    regex = "`resolution` must be greater than 1, not 1"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = -2),
    regexp = "`resolution` must be greater than 1, not -2"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = TRUE),
    regexp = "`resolution` must be a whole number \\(non-missing integer scalar or double equivalent\\)"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = "3"),
    regexp = "`resolution` must be a whole number"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", resolution = 0),
    regexp = "`resolution` must be greater than 1, not 0"
  )
})

test_that("posterior = true outputs the posterior and throws deprecation warning now, draws in now the path", {
  lifecycle::expect_deprecated(ecx(nsec_drc_1, posterior = TRUE, seed = 1))
  
  output_pt <- suppressWarnings(ecx(brms_model_1, x_var = "x", posterior = TRUE))
  output_dt <- suppressWarnings(ecx(brms_model_1, x_var = "x", draws = TRUE))
  
  expect_type(output_pt$draws[[1]]$value.draw, "double")
  expect_equal(nrow(output_pt$draws[[1]]), 1000)
  expect_equal(output_pt$estimate[[1]], 0.832, tolerance = 0.001)
  expect_equal(output_pt$draws[[1]]$value.draw[[100]], 0.8189, tolerance = 0.001)
  expect_equal(output_pt$draws[[1]]$value.draw[[1000]], 0.8247, tolerance = 0.001)
  
  expect_equal(output_pt, output_dt)
})

# EXPECTED-CHANGE type "relative" reference, redefined (old behaviour renamed "range") #19 - done
test_that("check type = relative argument, the relative is now range", {
  expect_error(
    suppressWarnings(ecx(brms_model_1, x_var = "x", type = "relative")),
    regexp = "`type` must not be 'relative' when `object` supplies no asymptote"
  )
  
  output <- suppressWarnings(ecx(brms_model_1, x_var = "x", type = "range"))

  expect_equal(output$estimate[[1]], 0.822, tolerance = 0.0001)
  expect_equal(output$conf.low[[1]], 0.822, tolerance = 0.0001)
  expect_equal(output$conf.high[[1]], 0.822, tolerance = 0.0001) # TODO DOUBLE CHECK this one changed from 1.021 to 0.8221
})

# EXPECTED-CHANGE warns when estimate is pinned at a bound #6, #24
# Old code gave back a number it could never reach, this is correct now :)
test_that("check type = direct argument", {
  expect_error(
    ecx(brms_model_1, x_var = "x", type = "direct"),
    regexp = "`reference` must be supplied when `type` is 'direct'"
  )
  
  output <- ecx(brms_model_1, x_var = "x", type = "direct", reference = 10)

  expect_equal(output$estimate[[1]], NA_real_)
  expect_equal(output$conf.low[[1]], NA_real_)
  expect_equal(output$conf.high[[1]], NA_real_)
  expect_equal(output$proportion[[1]], NA_real_)
  
})

# EXPECTED-CHANGE warns when estimate is pinned at a bound #6, #24
# Change has occured as expected 
test_that("check type = direct argument with in range value", {
  expect_warning(
    expect_warning(
      output <- ecx(brms_model_1, x_var = "x", type = "direct", ecx_val = 0.5),
      regexp = 'Use `proportion`, a proportion rather than a percentage, or `reference` under `type = "direct"`'
    )
  )
  
  expect_equal(output$estimate[[1]], 0.954, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.8, tolerance = 0.001)
  expect_equal(output$conf.high[[1]], 1.05, tolerance = 0.001)
  expect_equal(output$proportion[[1]], NA_real_)
  
})

test_that("check type = absolute argument", {
  output <- suppressWarnings(ecx(brms_model_1, x_var = "x", type = "absolute"))

  expect_equal(output$estimate[[1]], 0.8320, tolerance = 0.001)
  expect_equal(output$conf.low[[1]], 0.820, tolerance = 0.01)
  expect_equal(output$conf.high[[1]], 1.050, tolerance = 0.001)
  expect_equal(output$type[[1]], "absolute")

})

test_that("check type = range and absolute arguments behave as expeceted", {
  output1 <- suppressWarnings(ecx(brms_model_1, x_var = "x", type = "range", proportion = 0.50))
  output2 <- suppressWarnings(ecx(brms_model_1, x_var = "x", type = "absolute", proportion = 0.50))
  
  expect_gt(output2$estimate[[1]], output1$estimate[[1]])
  expect_equal(output1$estimate[[1]], 0.9165, tolerance = 0.001)
  expect_equal(output2$estimate[[1]], 0.9747, tolerance = 0.001)
})

# TODO note this now errors instead of producing values, switched it from just giving NAs 
test_that("type = absolute and value passed to trigger NAN catch and throws error", {
  
  expect_error(
    expect_warning(
      ecx(brms_model_1, x_var = "x", type = "absolute", x_range = c(-1, 1.05)),
      regexp = "NaNs produced"
    ),
    regexp = "`object` gives no finite prediction at 244 of 500 grid values, between -1 and -0.001703; `x_range` must lie where the model is defined"
  )
})

test_that("type = range and value passed to trigger NAN catch and throws error", {
  expect_error(
    expect_warning(
      ecx(brms_model_1, x_var = "x", type = "range", x_range = c(-1, 1.05)),
      regexp = "NaNs produced"
    ),
    regexp = "`object` gives no finite prediction at 244 of 500 grid values, between -1 and -0.001703; `x_range` must lie where the model is defined"
  )
})

test_that("type = direct and value passed to trigger NAN catch", {
  expect_error(
    expect_warning(
      ecx(brms_model_1, x_var = "x", type = "direct", x_range = c(-1, 1.05)),
      regexp = "NaNs produced"
    ),
    regexp = "`object` gives no finite prediction at 244 of 500 grid values, between -1 and -0.001703; `x_range` must lie where the model is defined"
  )
})

# EXPECTED-CHANGE type vocabulary gains "range"; passing >1 type will error cleanly #7 - Done :)
test_that("type errors when wrong value passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", type = "something"),
    regex = "`type` must match 'absolute', 'direct', 'range' or 'relative', not 'something'"
  )

  expect_error(
    ecx(brms_model_1, x_var = "x", type = c("direct", "absolute")),
    "type` must be a string \\(non-missing character scalar\\)"
  )
})

# EXPECTED-CHANGE hormesis_def removed, direction becomes a result column #20 - Done
test_that("hormesis_def is deprecated and ignored", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)
  
  lifecycle::expect_deprecated(
    output_max <- ecx(pred, hormesis_def = "max")
  )
  expect_identical(output_max, ecx(pred))

  lifecycle::expect_deprecated(
    output_con <- ecx(pred, hormesis_def = "control")
  )
  
  expect_identical(output_max, output_con)
})

test_that("a hormetic curve gives both an increasing and a decreasing ECx", {
  x_vec <- seq(0, 10, by = 0.01)
  # rises from a control of 10 to a peak of 14 at x = 2, then falls
  y <- 10 + 4 * x_vec - x_vec^2
  pred <- compute_pred(list(rbind(y, y)), x_vec)
  
  output <- ecx(pred)
  
  expect_identical(output$direction, c("decreasing", "increasing"))
  expect_false(anyNA(output$estimate))
})

# EXPECTED-CHANGE hormesis_def removed, direction becomes a result column #20 - done
test_that("a hormetic fit gives a decreasing and an increasing EC50", {
  output <- ecx(brms_model_5, x_var = "x", proportion = 0.5)
  
  expect_equal(output$estimate[[1]], 2.4621, tolerance = 0.001)
  expect_equal(output$estimate[[2]], 0.394, tolerance = 0.001)
  expect_equal(output$direction, c("decreasing", "increasing"))
})

test_that("xform function is applied to the values", {
  output_1 <- suppressWarnings(ecx(brms_model_1, x_var = "x"))
  output_2 <- suppressWarnings(ecx(brms_model_1, x_var = "x", xform = function(x) x - 1))

  expect_equal(output_2$estimate[[1]], output_1$estimate[[1]] - 1, tolerance = 0.01)
  expect_equal(output_2$conf.low[[1]], output_1$conf.low[[1]] - 1, tolerance = 0.01)
  expect_equal(output_2$conf.high[[1]], output_1$conf.high[[1]] - 1, tolerance = 0.01)
})

test_that("xform fails if function not passed", {
  expect_error(
    ecx(brms_model_1, x_var = "x", xform = 1),
    "`xform` must be a function"
  )
})

# Moved from the bnecfit tests when that method was removed (#39). The check is
# in the ecx() generic, so the class of the fit does not matter. - done
test_that("posterior fails if not logical and is deprecated", {
  lifecycle::expect_deprecated(
    expect_error(
      ecx(brms_model_1, x_var = "x", posterior = "yes"),
      regex = "`posterior` must be a flag \\(TRUE or FALSE\\)"
    )
  )
})

test_that("prob_vals argument changes when new values provided", {
  output_1 <- suppressWarnings(ecx(brms_model_1, x_var = "x"))
  output_2 <- suppressWarnings(ecx(brms_model_1, x_var = "x", prob_vals = c(0.35, 0.05, 0.9)))
  columns <- c("estimate", "conf.low", "conf.high")
  
  expect_false(any(output_1[1, columns] == output_2[1, columns]))
})

test_that("check prob_vals argument errors with less then 3 values", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.45, 0.1)),
    regexp = "`prob_vals` must be length 3 not 2"
  )
})

test_that("check prob_vals argument errors if the first value is not in the middle", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.1, 0.5, 0.60)),
    regex = "`prob_vals` must be the central, lower and upper quantiles, in that order, not 0.1, 0.5 and 0.6"
  )
})

test_that("check prob_vals argument errors if the 2nd value is not the lowest", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.5, 0.6, 0.10)),
    regex = "`prob_vals` must be the central, lower and upper quantiles, in that order, not 0.5, 0.6 and 0.1"
  )
})

test_that("check prob_vals argument errors if the 3nd value is not the highest", {
  expect_error(
    ecx(brms_model_1, x_var = "x", prob_vals = c(0.5, 0.1, 0.05)),
    regex = "`prob_vals` must be the central, lower and upper quantiles, in that order, not 0.5, 0.1 and 0.05"
  )
})

test_that("check prob_vals errors with more then 3 values", {
  expect_error(
    ecx(
      brms_model_1,
      x_var = "x",
      prob_vals = c(0.4, 0.1, 0.6, 0.7, 0.05)
    ),
    regexp = "`prob_vals` must be length 3 not 5"
  )
})

# BRMS specific tests -----------------------------------------------------

# EXPECTED-CHANGE multiple ecx_val values will be allowed #9 - DONE
test_that("can only pass a single exc_val argument", {
  expect_error(
    suppressWarnings(ecx(brms_model_1, x_var = "x", ecx_val = c(10, 50, 100))),
    "`ecx_val` must be a number"
  )
  
  output <- suppressWarnings(ecx(brms_model_1, x_var = "x", proportion = c(0.10, 0.50, 1)))
  expect_equal(output$metric, rep("ecx", 6))
  expect_equal(output$direction, rep(c("decreasing", "increasing"), 3))
  expect_equal(output$proportion, c(0.10, 0.10, 0.50, 0.50, 1, 1))
})

# EXPECTED-CHANGE ecx_val may become a proportion not a percentage #3; - done
# ecx_val validation moves to the generic (all methods, not just brms) #55;
# extreme ecx_val not reached within the tested range will warn to widen x_range #24
test_that("when type is not direct ecx_val has to between 1 and 99", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)
  
  expect_no_error(suppressWarnings(ecx(pred, proportion = 0.02, type = "absolute")))
  expect_no_error(suppressWarnings(ecx(pred, proportion = 1.5, type = "absolute")))
  expect_no_error(suppressWarnings(ecx(pred, type = "range", proportion = 1)))
  
  expect_error(ecx(pred, proportion = 0), "`proportion` must be greater than 0")
  expect_error(
    ecx(pred, type = "range", proportion = 1.5),
    "`proportion` must be less than or equal to 1"
  )
})

test_that("reference under direct can be any number", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)
  
  expect_no_error(suppressWarnings(ecx(pred, type = "direct", reference = 0)))
  expect_no_error(suppressWarnings(ecx(pred, type = "direct", reference = 1000)))
  expect_no_error(suppressWarnings(ecx(pred, type = "direct", reference = -0.25)))
})

test_that("errors if x_var argument not provided", {
  expect_error(
    ecx(brms_model_1),
    regex = "`x_var` must be supplied for a `brmsfit`"
  )
})

# EXPECTED-CHANGE error message typo "suplied" fixed #53 - done
test_that("errors if x_var is not in the data set", {
  expect_error(
    ecx(brms_model_1, x_var = "z"),
    "`x_var` must name a column of the data in `object`, not 'z'"
  )
})

test_that("errors if x_var is not in a predictor variable", {
  expect_error(
    ecx(brms_model_1, x_var = "y"),
    "`object` needs a value of 'x' to predict; name it as `x_var`, or as `group_var` to get one curve per level"
  )
})

test_that("errors if additional groups are present but group_var not specified", {
  expect_error(
    ecx(brms_model_2, x_var = "x"),
    regexp = "`object` needs a value of 'z' to predict; name it as `x_var`, or as `group_var` to get one curve per level"
  )
})

test_that("if by_group is TRUE you must supply a grouping variable in group_var that is in the data", {
  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE),
    "`group_var` must be supplied when `by_group` is TRUE"
  )

  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "aa"),
    "`group_var` must name a column of the data in `object`, not 'aa'"
  )

  expect_error(
    ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = 1),
    "`group_var` must be a string \\(non-missing character scalar\\) or NULL"
  )
})

test_that("by_group = TRUE and group_var supplied it groups the data based on the group_var", {
  output <- suppressWarnings(ecx(brms_model_2, x_var = "x", by_group = TRUE, group_var = "z"))
  
  expect_equal(dim(output), c(4, 11))
  expect_true("group" %in% colnames(output))
  expect_equal(output$group, factor(c("1", "1", "2", "2")))
})

test_that("by_group = FALSE, group_var is a value in the data then it throws an", {
  expect_error(
    ecx(
      brms_model_2,
      x_var = "z",
      by_group = FALSE,
      group_var = "x",
      prob_vals = c(0.5, 0.025, 0.975)
    ),
    regexp = "`by_group = FALSE` with a `group_var` is no longer supported; every level of `group_var` now gives its own estimate"
  )
})

# EXPECTED-CHANGE x_range will be treated as a range, not a single value #10
test_that("x_range limits where ecx looks for a crossing", {
  output <- suppressWarnings(ecx(brms_model_1, x_var = "x", x_range = c(0.8, 0.9)))
  expect_equal(output$conf.high, c(0.9, 0.9))
})

# EXPECTED-CHANGE group_var == x_var will error #15 - done
test_that("when group_var == x_var will error it errors", {
  expect_error(
    suppressWarnings(
      ecx(
        brms_model_1,
        x_var = "x",
        by_group = TRUE,
        group_var = "x",
        posterior = TRUE
      ) 
    ),
    regexp = "group_var` and `x_var` must name different columns, not both 'x'"
  )
})

test_that("when by_group = TRUE, group_var is provided and draws = TRUE, you get draws for each group", {
  output <- suppressWarnings(ecx(
    brms_model_2,
    x_var = "x",
    by_group = TRUE,
    group_var = "z",
    posterior = TRUE
  ))
  
  expect_equal(dim(output), c(4, 12))
  expect_true("group" %in% colnames(output))
  expect_true("draws" %in% colnames(output))
})

test_that("ecx passes other brms errors through unchanged", {
  local_mocked_bindings(
    posterior_epred = function(...) {
      stop(structure(
        class = c("brms_error", "error", "condition"),
        list(message = "some other brms problem", call = NULL)
      ))
    }
  )
  
  expect_error(
    ecx(brms_model_1, x_var = "x"),
    "some other brms problem",
    class = "brms_error"
  )
})

# more realistic examples -------------------------------------------------

test_that("brms additional example 1", {
  output <- suppressWarnings(ecx(brms_model_3, x_var = "x", ecx_val = 50))

  expect_equal(
    c(output$estimate[[1]], output$conf.low[[1]], output$conf.high[[1]]),
    c(2.0105, 1.9682, 2.055),
    tolerance = 0.0001
  )
})

# ecx.brmsfit — edge cases ------------------------------------------------

test_that("brmsfit ec50 is larger than ec10", {
  output <- suppressWarnings(
    ecx(brms_model_3, x_var = "x", proportion = c(0.1, 0.5))
  )
  decreasing <- output[output$direction == "decreasing", ]
  
  expect_identical(decreasing$proportion, c(0.1, 0.5))
  expect_gt(decreasing$estimate[[2]], decreasing$estimate[[1]])
})

test_that("brmsfit type = range gives smaller ECx than type = absolute", {
  e_rel <- suppressWarnings(ecx(brms_model_3, x_var = "x", type = "range", ecx_val = 50))
  e_abs <- suppressWarnings(ecx(brms_model_3, x_var = "x", type = "absolute", ecx_val = 50))

  expect_lt(e_rel$estimate[[1]], e_abs$estimate[[1]])
})

# Argument tests non model specific ---------------------------------------

test_that("ecx and nsec error when a toxval_pred is given predict-level arguments", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 11 - x_vec)), x_vec)
  
  expect_error(
    ecx(pred, x_var = "x"),
    "`object` is a `toxval_pred`, whose grid is already built, so `x_var` must not be supplied"
  )
})

test_that("proportion must not be supplied under type = direct", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 11 - x_vec)), x_vec)
  
  expect_error(
    ecx(pred, type = "direct", proportion = 0.1, reference = 5),
    "`proportion` must not be supplied when `type` is 'direct'"
  )
})

test_that("reference must not be supplied unless type is direct", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 11 - x_vec)), x_vec)
  
  expect_error(
    ecx(pred, reference = 5),
    "`reference` must be NULL unless `type` is 'direct'"
  )
})

test_that("ecx_val must not be supplied with proportion or reference", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 11 - x_vec)), x_vec)
  
  expect_error(
    ecx(pred, ecx_val = 10, proportion = 0.1),
    "`ecx_val` must not be supplied with `proportion` or `reference`"
  )
  expect_error(
    ecx(pred, type = "direct", ecx_val = 5, reference = 5),
    "`ecx_val` must not be supplied with `proportion` or `reference`"
  )
})

test_that("horme is deprecated and ignored by ecx", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)
  
  lifecycle::expect_deprecated(output <- ecx(pred, horme = TRUE))
  expect_identical(output, ecx(pred))
})

test_that("proportion must not be empty", {
  expect_error(
    ecx(brms_model_3, x_var = "x", proportion = numeric(0)),
    "`proportion` must have at least one value"
  )
})

test_that("proportion must be finite", {
  expect_error(
    ecx(brms_model_3, x_var = "x", proportion = Inf),
    "`proportion` must be finite"
  )
})

# Comparison to drc::ED() -------------------------------------------------

test_that("ecx direct matches drc::ED absolute", {
  fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
  
  output <- ecx(fit, type = "direct", reference = 5, seed = 1)
  expected <- drc::ED(fit, 5, type = "absolute", display = FALSE)
  
  expect_equal(output$estimate[[1]], expected[[1]], tolerance = 0.01)
})

test_that("ecx absolute matches drc::ED for a curve with a lower limit of 0", {
  fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.3())
  
  output <- ecx(fit, proportion = 0.5, seed = 1)
  expected <- drc::ED(fit, 50, display = FALSE)
  
  expect_equal(output$estimate[[1]], expected[[1]], tolerance = 0.01)
})

test_that("ecx relative matches drc::ED relative", {
  fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())

  output <- suppressWarnings(
    ecx(fit, type = "relative", proportion = c(0.1, 0.5), seed = 1)
  )
  decreasing <- output[output$direction == "decreasing", ]
  expected <- drc::ED(fit, c(10, 50), type = "relative", display = FALSE)

  expect_equal(decreasing$estimate, unname(expected[, 1]), tolerance = 0.01)
})

# type = "relative" ----------------------------------------------------------

test_that("ecx relative warns that its meaning has changed", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(10 - x_vec, 10 - x_vec)),
    x_vec,
    asymptote = list(list(decreasing = c(2, 2), increasing = c(NA_real_, NA_real_)))
  )

  # the increasing direction has no asymptote, so it warns as excluded too
  expect_warning(
    expect_warning(
      ecx(pred, type = "relative"),
      "`type = \"relative\"` now measures from the control to the asymptote"
    ),
    "excluded"
  )
})

test_that("ecx relative measures from the control to the asymptote", {
  # a control of 10 and a lower asymptote of 2: the EC50 reference is 6
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(10 - x_vec, 10 - x_vec)),
    x_vec,
    asymptote = list(list(decreasing = c(2, 2), increasing = c(NA_real_, NA_real_)))
  )

  output <- suppressWarnings(ecx(pred, type = "relative", proportion = 0.5))

  expect_equal(output$estimate, c(4, NA))
  expect_equal(output$reference, c(6, NA))
  expect_equal(output$asymptote, c(2, NA))
})

test_that("ecx relative with an asymptote of 0 matches absolute", {
  absolute <- suppressWarnings(
    ecx(brms_model_1, x_var = "x", resolution = 50)
  )
  relative <- suppressWarnings(
    ecx(
      brms_model_1,
      x_var = "x",
      type = "relative",
      asymptote = c(decreasing = 0),
      resolution = 50
    )
  )

  expect_equal(relative$estimate[[1]], absolute$estimate[[1]])
  expect_equal(relative$asymptote, c(0, NA))
})

test_that("ecx relative takes a named parameter of a brmsfit on the response scale", {
  # brms_model_4 is a beta fit with a logit link, so `bot` is on the logit
  # scale
  output <- suppressWarnings(
    ecx(
      brms_model_4,
      x_var = "x",
      group_var = "herbicide",
      type = "relative",
      asymptote = c(decreasing = "bot"),
      resolution = 50
    )
  )
  bot <- brms::posterior_linpred(
    brms_model_4,
    newdata = data.frame(x = min(brms_model_4$data$x), herbicide = "ametryn"),
    nlpar = "bot",
    re_formula = NA
  )

  expect_equal(output$asymptote[[1]], stats::median(stats::plogis(bot[, 1])))
})

test_that("ecx gives an NA asymptote unless type is relative", {
  output <- ecx(nsec_drc_1, seed = 1, n_boot = 50, resolution = 50)

  expect_equal(output$asymptote, c(NA_real_, NA_real_))
})

test_that("ecx relative caps proportion at 1", {
  expect_error(
    suppressWarnings(
      ecx(nsec_drc_1, type = "relative", proportion = 1.5, n_boot = 10)
    ),
    "`proportion` must be less than or equal to 1, not 1.5"
  )
})

test_that("ecx errors when asymptote names no non-linear parameter", {
  expect_error(
    ecx(
      brms_model_4,
      x_var = "x",
      group_var = "herbicide",
      asymptote = c(decreasing = "slope")
    ),
    "`asymptote` must name non-linear parameters of `object`, one of 'top', 'bot', 'beta' or 'ec50', not 'slope'"
  )
})

test_that("ecx errors when asymptote is supplied for a multivariate brmsfit", {
  expect_error(
    ecx(nsec_multi_model_2, x_var = "dose", asymptote = c(decreasing = 0)),
    "`asymptote` must be NULL for a multivariate `brmsfit`"
  )
})
