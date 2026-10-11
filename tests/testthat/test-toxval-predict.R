# EXPECTED-CHANGE move to a helper file when nsec and ecx also need a grouped
# drc fit; test-nsec.R:347 already builds this same model inline
# A grouped `drc` fit. Neither saved fixture has one: drc fills the curveid
# columns with a constant named `1` when no curveid is given, so both are
# single-curve fits that still have something in that position.
daphnids_fit <- function() {
  drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )
}

# toxval_predict argument checks --------------------------------------------

test_that("toxval_predict errors when a method is given no x_var", {
  expect_error(
    toxval_predict(nsec_drc_1),
    "`x_var` must be supplied for a `drc` fit"
  )
  expect_error(
    toxval_predict(brms_model_1),
    "`x_var` must be supplied for a `brmsfit`"
  )
})

test_that("toxval_predict errors for a class with no method", {
  expect_error(
    toxval_predict(1:10),
    "`object` must be a fitted model with a `toxval_predict\\(\\)` method"
  )

  expect_error(
    toxval_predict(list(x = 1:10), x_var = "x"),
    "`object` must be a fitted model with a `toxval_predict\\(\\)` method"
  )
})

test_that("toxval_predict errors when x_var is not a string", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = 1),
    "`x_var` must be a string"
  )
})

test_that("toxval_predict errors when group_var is not a string", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", group_var = 1),
    "`group_var` must be a string .* or NULL"
  )
})

test_that("toxval_predict errors when resolution is not a whole number", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", resolution = 2.5),
    "`resolution` must be a whole number"
  )
})

test_that("toxval_predict errors when resolution is 1 or less", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", resolution = 1),
    "`resolution` must be greater than 1, not 1"
  )

  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", resolution = 0),
    "`resolution` must be greater than 1, not 0"
  )
})

test_that("toxval_predict errors when x_range is not length 2", {
  # EXPECTED-CHANGE Remove comment after refactoring is done
  # a length-2 `x_range` must reach the comparisons below without being tested
  # element-wise first, which is what `is.na(x_range)` did in the pre-refactor
  # entry points
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", x_range = c(0, 1, 2)),
    "`x_range` must be length 2"
  )

  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", x_range = c(1)),
    "`x_range` must be length 2"
  )
})

test_that("toxval_predict errors when x_range contains a missing value", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", x_range = c(0, NA)),
    "`x_range` must not have any missing values"
  )
})

test_that("toxval_predict errors when x_range is not finite", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", x_range = c(0, Inf)),
    "`x_range` must be finite"
  )
})

test_that("toxval_predict errors when x_range is not increasing", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", x_range = c(3, 0)),
    "`x_range` must be increasing"
  )
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", x_range = c(2, 2)),
    "`x_range` must be increasing"
  )
})

test_that("toxval_predict errors when x_var is not a column of the data", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "dose"),
    "`x_var` must name a column of the data in `object`, not 'dose'"
  )
})

test_that("toxval_predict matches x_var exactly", {
  # `x_var = "x"` must not be satisfied by a column merely containing an "x"
  # brms_model_1 has data with column names y, log(x), x, it won't match "lo"
  expect_error(
    toxval_predict(brms_model_1, x_var = "lo"),
    "`x_var` must name a column of the data in `object`, not 'lo'"
  )
})

test_that("toxval_predict errors when group_var is not a column of the data", {
  expect_error(
    toxval_predict(brms_model_2, x_var = "x", group_var = "site"),
    "`group_var` must name a column of the data in `object`, not 'site'"
  )
})

# toxval_predict.brmsfit ----------------------------------------------------

test_that("toxval_predict returns a toxval_pred for an ungrouped brmsfit", {
  result <- toxval_predict(
    brms_model_1,
    x_var = "x",
    resolution = 6
  )

  expect_s3_class(result, "toxval_pred")
  expect_type(result$curves, "list")
  expect_length(result$curves, 1)
  expect_null(names(result$curves))
  expect_equal(ncol(result$curves[[1]]), 6)
  expect_equal(nrow(result$curves[[1]]), brms::ndraws(brms_model_1))

  expect_equal(result$meta$source_class, "brmsfit")
  expect_equal(result$meta$x_var, "x")
  expect_null(result$meta$group_var)
  expect_null(result$meta$multi_var)
  expect_equal(result$meta$dimension, "none")
  expect_equal(result$meta$resolution, 6)
  expect_equal(result$meta$x_range, c(0.8, 1.05))
  expect_equal(result$meta$family, "gaussian")
  expect_equal(result$meta$realisation, "draws")
  expect_equal(result$meta$n_realisation, brms::ndraws(brms_model_1))
  expect_null(result$meta$seed)
})

test_that("toxval_predict returns one named curve per group for a grouped brmsfit", {
  result <- toxval_predict(
    brms_model_2,
    x_var = "x",
    group_var = "z",
    resolution = 6
  )

  expect_length(result$curves, 2)
  expect_named(result$curves, c("1", "2"))
  expect_equal(result$meta$dimension, "group")
  expect_equal(result$meta$group_var, "z")
  expect_equal(ncol(result$curves[["1"]]), 6)
  expect_equal(ncol(result$curves[["2"]]), 6)
})

test_that("toxval_predict returns different curves for different groups", {
  result <- toxval_predict(
    brms_model_2,
    x_var = "x",
    group_var = "z",
    resolution = 6
  )

  expect_false(identical(result$curves[["1"]], result$curves[["2"]]))
})

test_that("toxval_predict errors for a multivariate brmsfit", {
  # EXPECTED-CHANGE multivariate brmsfit support, one curve per response
  # this test will stop erroring once that change is applied
  expect_error(
    toxval_predict(nsec_multi_model_1, x_var = "dose", resolution = 6),
    "`object` must not be a multivariate `brmsfit`"
  )
})

# toxval_predict.drc --------------------------------------------------------

test_that("toxval_predict returns a toxval_pred for an ungrouped drc fit", {
  result <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    resolution = 5,
    n_boot = 10,
    seed = 42
  )

  expect_s3_class(result, "toxval_pred")
  expect_length(result$curves, 1)
  expect_null(names(result$curves))
  expect_equal(dim(result$curves[[1]]), c(10L, 5L))

  expect_equal(result$meta$source_class, "drc")
  expect_equal(result$meta$x_var, "x")
  expect_null(result$meta$group_var)
  expect_null(result$meta$multi_var)
  expect_equal(result$meta$dimension, "none")
  expect_equal(result$meta$resolution, 5)
  expect_equal(result$meta$x_range, c(0.032, 3.221), tolerance = 0.001)
  expect_null(result$meta$family)
  expect_equal(result$meta$realisation, "bootstrap")
  expect_equal(result$meta$n_realisation, 10)
  expect_equal(result$meta$seed, 42)
})

test_that("toxval_predict returns one named curve per curveid for a grouped drc fit", {
  result <- toxval_predict(
    daphnids_fit(),
    x_var = "dose",
    group_var = "time",
    resolution = 5,
    n_boot = 10,
    seed = 42
  )

  expect_length(result$curves, 2)
  expect_named(result$curves, c("24h", "48h"))
  expect_equal(result$meta$dimension, "group")
  expect_equal(result$meta$group_var, "time")
  expect_equal(dim(result$curves[["24h"]]), c(10L, 5L))
})

test_that("toxval_predict errors when a grouped drc fit is given no group_var", {
  expect_error(
    toxval_predict(
      daphnids_fit(),
      x_var = "dose",
      resolution = 5,
      n_boot = 10
    ),
    "`group_var` must be supplied for a `drc` fit with more than one curve"
  )
})

test_that("toxval_predict errors when an ungrouped drc fit is given a group_var", {
  # EXPECTED-CHANGE remove this comment after re-factoring
  # the curves are read from the coefficient names, so a `group_var` that names
  # a real column of a single-curve fit is still an error unlike issue (#34)
  # in the original code, ie new code didn't code in that bug
  expect_error(
    toxval_predict(
      nsec_drc_1,
      x_var = "x",
      group_var = "y",
      resolution = 5,
      n_boot = 10
    ),
    "`group_var` must be NULL for a `drc` fit with a single curve"
  )
})

test_that("toxval_predict errors when n_boot is not a positive whole number", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", n_boot = 2.5),
    "`n_boot` must be a whole number"
  )
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", n_boot = 0),
    "`n_boot` must be greater than 0"
  )
})

test_that("toxval_predict errors when seed is not a whole number", {
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", seed = "42"),
    "`seed` must be a whole number .* or NULL"
  )
  
  expect_error(
    toxval_predict(nsec_drc_1, x_var = "x", seed = 42.5),
    "`seed` must be a whole number .* or NULL"
  )
})

# the predictor grid --------------------------------------------------------

test_that("toxval_predict builds the grid from x_range and resolution", {
  result <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    x_range = c(0, 3),
    resolution = 4,
    n_boot = 5,
    seed = 42
  )

  expect_equal(result$x_vec, c(0, 1, 2, 3))
})

test_that("toxval_predict takes the grid range from the data when x_range is NULL", {
  result <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    resolution = 4,
    n_boot = 5,
    seed = 42
  )

  expect_equal(range(result$x_vec), range(nsec_drc_1$data$x))
})

test_that("toxval_predict errors when the predictor takes a single value", {
  # every observation at one concentration, so there is no range to build a
  # grid from
  fit <- nsec_drc_1
  fit$data$x <- 1

  expect_error(
    toxval_predict(fit, x_var = "x",  resolution = 4, n_boot = 5),
    "`object` must have more than one value of the predictor, or `x_range` must be supplied"
  )
  expect_no_error(
    toxval_predict(fit, x_var = "x", x_range = c(0, 3), resolution = 4, n_boot = 5)
  )
})

# the parametric bootstrap --------------------------------------------------

test_that("toxval_predict is reproducible for a given seed", {
  args <- list(
    nsec_drc_1,
    x_var = "x",
    resolution = 5,
    n_boot = 10,
    seed = 7
  )

  expect_equal(
    do.call(toxval_predict, args)$curves,
    do.call(toxval_predict, args)$curves
  )
})

test_that("toxval_predict gives different realisations for different seeds", {
  first <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    resolution = 5,
    n_boot = 10,
    seed = 7
  )
  second <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    resolution = 5,
    n_boot = 10,
    seed = 8
  )

  expect_false(identical(first$curves, second$curves))
})

test_that("toxval_predict leaves the random number state alone", {
  set.seed(1)
  before <- .Random.seed
  toxval_predict(
    nsec_drc_1,
    x_var = "x",
    resolution = 5,
    n_boot = 10,
    seed = 7
  )

  expect_equal(.Random.seed, before)
})

test_that("toxval_predict at the fitted coefficients reproduces predict()", {
  # with every replicate set to coef(), the curve must be the fitted curve, so
  # this checks that each parameter reaches the drc mean function in the
  # position it expects; with b and e swapped, the same grid gives 0.897, 0.883,
  # 0.842 and 0.782
  mu <- coef(nsec_drc_1)
  local_mocked_bindings(
    boot_parms = function(object, n_boot, seed = NULL) {
      matrix(
        mu,
        nrow = n_boot,
        ncol = length(mu),
        byrow = TRUE,
        dimnames = list(NULL, names(mu))
      )
    }
  )
  result <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    x_range = c(0, 3),
    resolution = 4,
    n_boot = 2
  )
  fitted <- predict(nsec_drc_1, newdata = data.frame(x = result$x_vec))

  expect_equal(result$curves[[1]][1, ], as.numeric(fitted))
  expect_equal(result$curves[[1]][2, ], as.numeric(fitted))
})

test_that("boot_parms draws from MVN(coef(object), vcov(object))", {
  # tested on the parameter scale rather than against the fitted curve: the
  # curve is nonlinear in its parameters, so the mean bootstrap curve differs
  # from the fitted curve by more than Monte Carlo error (0.0376 against 0.0351
  # at x = 3, 4000 replicates)
  n_boot <- 4000
  parms <- toxval:::boot_parms(nsec_drc_1, n_boot = n_boot, seed = 42)
  mu <- coef(nsec_drc_1)
  sigma <- vcov(nsec_drc_1)
  sample_sigma <- stats::cov(parms)

  expect_equal(colnames(parms), names(mu))

  # each mean within 4 Monte Carlo standard errors of its coefficient
  z <- (colMeans(parms) - mu) / sqrt(diag(sigma) / n_boot)
  expect_lt(max(abs(z)), 4)

  # the standard error of a sample variance relative to its target is about
  # sqrt(2 / n_boot), 0.022 here, so 0.1 is between four and five of them
  expect_lt(max(abs(diag(sample_sigma) / diag(sigma) - 1)), 0.1)

  # the standard error of a sample correlation is at most 1 / sqrt(n_boot),
  # 0.016 here; the fit's b-e and c-e correlations of -0.75 and -0.91 mean a
  # draw that ignored the off-diagonal of `vcov()` would fail by an order of
  # magnitude
  expect_lt(
    max(abs(stats::cov2cor(sample_sigma) - stats::cov2cor(sigma))),
    0.05
  )
})

test_that("the realisations of a grouped drc fit come from one joint parameter draw", {
  # TODO Becky to review next
  # alignment: replicate i of every curve is the same draw of the whole
  # parameter vector, which is what makes per-realisation quantities from
  # different curves comparable row by row
  fit <- daphnids_fit()
  result <- toxval_predict(
    fit,
    x_var = "dose",
    group_var = "time",
    resolution = 4,
    n_boot = 5,
    seed = 11
  )
  parms <- toxval:::boot_parms(fit, n_boot = 5, seed = 11)

  expect_equal(
    result$curves[["24h"]],
    toxval:::drc_eval_curve(
      fit,
      result$x_vec,
      parms[, c("b:24h", "e:24h"), drop = FALSE]
    )
  )
  expect_equal(
    result$curves[["48h"]],
    toxval:::drc_eval_curve(
      fit,
      result$x_vec,
      parms[, c("b:48h", "e:48h"), drop = FALSE]
    )
  )
})

# TODO Becky to review next
test_that("drc_eval_curve reproduces the drc mean function row by row", {
  x_vec <- c(0.5, 1, 2)
  parms <- rbind(coef(nsec_drc_1), coef(nsec_drc_1) * 1.1)
  result <- toxval:::drc_eval_curve(nsec_drc_1, x_vec, parms)

  expect_equal(dim(result), c(2L, 3L))
  # each row is evaluated at its own parameter vector; checking only the first
  # row would pass an implementation that applied it to every row
  for (i in seq_len(nrow(parms))) {
    expect_equal(
      result[i, ],
      nsec_drc_1$fct$fct(
        x_vec,
        matrix(parms[i, ], nrow = 3, ncol = 4, byrow = TRUE)
      )
    )
  }
})

# TODO Becky to review next
test_that("drc_split_parm_names splits drc coefficient names on the first colon", {
  # a curve level comes from the data and may contain a colon; a drc parameter
  # name does not
  result <- toxval:::drc_split_parm_names(c(
    "b:(Intercept)",
    "e:24h",
    "b:t:24h"
  ))

  expect_equal(result$param, c("b", "e", "b"))
  expect_equal(result$curve, c("(Intercept)", "24h", "t:24h"))
})

test_that("toxval_predict keeps a curveid level that contains a colon", {
  data <- drc::daphnids
  data$time <- ifelse(data$time == "24h", "t:24h", "t:48h")
  fit <- drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = data,
    fct = drc::LL.2(),
    type = "binomial"
  )
  result <- toxval_predict(
    fit,
    x_var = "dose",
    group_var = "time",
    resolution = 3,
    n_boot = 2,
    seed = 1
  )

  expect_named(result$curves, c("t:24h", "t:48h"))
})

test_that("drc_split_parm_names splits drc coefficients as expected", {
  # nsec_drc_1 uses LL.4 is the four parameter log-logistic model
  result <- toxval:::drc_split_parm_names(names(coef(nsec_drc_1)))
  
  expect_equal(result$param, c("b", "c", "d", "e"))
  expect_equal(result$curve, c("(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"))
})

# TODO Becky to review next
test_that("drc_split_parm_names errors on a name it cannot split", {
  expect_error(
    toxval:::drc_split_parm_names(c("b", "e")),
    "`coef\\(object\\)` must be named <parameter>:<curve>"
  )
})

# TODO Becky to review next
test_that("toxval_predict builds each curve of a drc fit with shared parameters", {
  # `pmodels` fits one slope across both curves and an ED50 for each, so the
  # coefficients are `b:(Intercept)`, `e:24h` and `e:48h`: a curve is assembled
  # from its own parameters plus the shared one. With every replicate set to
  # coef(), each curve must be the fitted curve for its level.
  fit <- drc::drm(
    (total - no) / total ~ dose,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial",
    pmodels = data.frame(1, time)
  )
  mu <- coef(fit)
  local_mocked_bindings(
    boot_parms = function(object, n_boot, seed = NULL) {
      matrix(
        mu,
        nrow = n_boot,
        ncol = length(mu),
        byrow = TRUE,
        dimnames = list(NULL, names(mu))
      )
    }
  )
  result <- toxval_predict(
    fit,
    x_var = "dose",
    group_var = "time",
    resolution = 4,
    n_boot = 1
  )

  expect_named(result$curves, c("24h", "48h"))
  for (level in c("24h", "48h")) {
    fitted <- predict(
      fit,
      newdata = data.frame(dose = result$x_vec, time = level)
    )
    expect_equal(result$curves[[level]][1, ], as.numeric(fitted))
  }
})

test_that("drc_curve_cols errors when a parameter is missing for a curve", {
  split <- list(param = c("b", "e"), curve = c("24h", "48h"))

  expect_error(
    toxval:::drc_curve_cols(split, is_shared = c(FALSE, FALSE), id = "24h"),
    "`coef\\(object\\)` must name parameter 'e' exactly once for curve '24h', not 0 times"
  )
})

# TODO Becky to review next
test_that("boot_parms errors when a coefficient is missing", {
  # a rank-deficient fit drops a coefficient to NA rather than failing
  data <- data.frame(y = c(1, 2, 3, 5), x = c(1, 2, 3, 4))
  data$x2 <- data$x * 2
  fit <- stats::lm(y ~ x + x2, data = data)

  expect_error(
    toxval:::boot_parms(fit, n_boot = 5),
    "`coef\\(object\\)` must not contain missing values"
  )
})

# TODO Becky to review next
test_that("boot_parms errors when the variance-covariance matrix is missing values", {
  registerS3method("coef", "toxval_stub", function(object, ...) c(a = 1, b = 2))
  registerS3method("vcov", "toxval_stub", function(object, ...) {
    matrix(c(1, NA, NA, 1), nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  })

  expect_error(
    toxval:::boot_parms(structure(list(), class = "toxval_stub"), n_boot = 5),
    "`vcov\\(object\\)` must not contain missing values"
  )
})

# TODO Becky to review next
test_that("toxval_predict orders the curves by the content of the grouping variable", {
  # `herbicide` appears in the data as irgarol, diuron, ametryn, ..., so an
  # order taken from the rows would depend on how the data happened to be
  # sorted, and the curve names become the group descriptor of the result
  result <- toxval_predict(
    brms_model_4,
    x_var = "x",
    group_var = "herbicide",
    resolution = 3
  )

  expect_named(
    result$curves,
    c(
      "ametryn",
      "atrazine",
      "diuron",
      "hexazinone",
      "irgarol",
      "simazine",
      "tebuthiuron"
    )
  )
})

test_that("group_levels orders character levels the same in every locale", {
  # testthat runs with a C collation, under which plain `sort()` would also
  # pass, so the session collation is set to a locale that orders mixed case
  # differently
  withr::local_collate("en_US.UTF-8")
  skip_if_not(
    Sys.getlocale("LC_COLLATE") == "en_US.UTF-8",
    "en_US.UTF-8 collation is not available"
  )

  expect_equal(
    toxval:::group_levels(c("b", "A", "a", "B")),
    c("A", "B", "a", "b")
  )
})

# TODO Becky to review next
test_that("group_levels keeps the declared order of a factor", {
  # the declared order differs from both the alphabetical order (control, high,
  # medium) and the order of first appearance (high, control, medium), so the
  # test fails if either replaces it
  x <- factor(
    c("high", "control", "medium", "high"),
    levels = c("control", "low", "medium", "high")
  )
  result <- toxval:::group_levels(x)

  # the unused level is dropped, the declared order of the rest is kept, and
  # the result stays a factor so the levels reach `newdata`
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), c("control", "medium", "high"))
  expect_equal(levels(result), c("control", "medium", "high"))
})

# TODO Becky to review next
test_that("toxval_predict handles a single bootstrap replicate", {
  # n_boot = 1 is the edge case where a draw is most easily returned as a
  # vector rather than a 1-row matrix. The values also pin the draw for a
  # seed: the Cholesky factor of `vcov()` is unique, so they are the same on
  # every platform, which `MASS::mvrnorm()` did not give (#59)
  result <- toxval_predict(
    nsec_drc_1,
    x_var = "x",
    x_range = c(0, 3),
    resolution = 4,
    n_boot = 1,
    seed = 3
  )

  expect_true(is.matrix(result$curves[[1]]))
  expect_equal(dim(result$curves[[1]]), c(1L, 4L))
  expect_equal(result$meta$n_realisation, 1)
  expect_equal(
    as.numeric(result$curves[[1]]),
    c(0.9002389, 0.8941983, 0.4380216, 0.0091685),
    tolerance = 1e-6
  )
})
