skip_if_not_installed("bayesnec")

# The single-equation bnec() fits among the fixtures: nechorme on a beta
# response, nec4param and ecx4param on a gaussian one (both pulled out of
# manec_example), nec3param on a binomial one, and ecx4param with its
# predictor log-transformed inside crf().
bnec_single_fits <- function() {
  list(
    bnec_model_1 = bnec_model_1,
    bayesnec_nec4param = bayesnec_nec4param,
    bayesnec_ecx4param = bayesnec_ecx4param,
    bayesnec_binomial = bayesnec_binomial,
    bayesnec_logx = bayesnec_logx
  )
}

# the curves --------------------------------------------------------------

test_that("toxval_predict on a bayesnecfit gives the curves bayesnec predicts", {
  fits <- bnec_single_fits()
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    pred <- toxval_predict(fit, resolution = 50)
    # bayesnec's own posterior_epred() method, over bayesnec's own grid
    expected <- brms::posterior_epred(
      fit,
      newdata = bayesnec::bnec_newdata(fit, resolution = 50)
    )
    expect_identical(unname(pred$curves[[1]]), unname(expected), info = nm)
  }
})

test_that("toxval_predict on a bayesnecfit takes x_vec from bayesnec's grid", {
  grid <- bayesnec::bnec_newdata(bnec_model_1, resolution = 50)
  pred <- toxval_predict(bnec_model_1, resolution = 50)

  expect_identical(pred$x_vec, grid$x)
  expect_equal(range(pred$x_vec), range(bnec_model_1$fit$data$x))
})

test_that("toxval_predict on a bayesnecfit ends the grid at a supplied x_range", {
  x_min <- min(bnec_model_1$fit$data$x)
  pred <- toxval_predict(bnec_model_1, resolution = 50, x_range = c(x_min, 1))

  expect_equal(range(pred$x_vec), c(x_min, 1))
  expect_length(pred$x_vec, 50)
  expect_identical(pred$meta$x_control, x_min)
})

test_that("toxval_predict on a bayesnecfit starts the grid at x_control", {
  pred <- toxval_predict(
    bnec_model_1,
    resolution = 50,
    x_control = 0.6,
    x_range = c(0.6, 1)
  )

  expect_identical(pred$x_vec, seq(0.6, 1, length.out = 50))
  expect_identical(pred$meta$x_control, 0.6)
})

test_that("toxval_predict on a bayesnecfit refuses an x_range that does not start at the control concentration", {
  expect_error(
    toxval_predict(bnec_model_1, resolution = 50, x_range = c(0.6, 1)),
    "`x_range` must start at the control concentration, 0.5, not 0.6",
    fixed = TRUE
  )
})

test_that("toxval_predict on a bayesmanecfit starts the grid at x_control", {
  pred <- toxval_predict(
    bayesnec_manec_example,
    resolution = 50,
    x_control = 0.5,
    x_range = c(0.5, 2)
  )

  expect_identical(pred$x_vec, seq(0.5, 2, length.out = 50))
  expect_identical(pred$meta$x_control, 0.5)
})

test_that("ecx() passes x_control to a bayesnecfit", {
  # the increasing ECx of nechorme is censored in most realisations and warns
  result <- suppressWarnings(ecx(
    bnec_model_1,
    proportion = 0.1,
    resolution = 50,
    x_control = 0.6,
    x_range = c(0.6, 1)
  ))

  expect_s3_class(result, "toxval")
  expect_true(all(c(result$conf.low, result$conf.high) >= 0.6))
})

test_that("toxval_predict on a bayesnecfit records the meta contract", {
  pred <- toxval_predict(bayesnec_nec4param, resolution = 50)
  meta <- pred$meta

  expect_identical(meta$source_class, "bayesnecfit")
  expect_identical(meta$x_var, "x")
  expect_null(meta$group_var)
  expect_identical(meta$x_control, min(bayesnec_nec4param$fit$data$x))
  expect_identical(meta$family, "gaussian")
  expect_identical(meta$realisation, "draws")
  expect_equal(meta$n_realisation, brms::ndraws(bayesnec_nec4param$fit))
  expect_null(meta$seed)
  expect_identical(ncol(pred$keys), 0L)
  expect_null(pred$threshold)
})

test_that("toxval_predict on a bayesnecfit reports a crf(log(x)) predictor in x", {
  pred <- toxval_predict(bayesnec_logx, resolution = 50)

  # the grid is over x as recorded, not log(x), which is negative below x = 1
  expect_equal(range(pred$x_vec), range(bayesnec_nec_data$x))
  expect_true(all(pred$x_vec > 0))
  expect_identical(pred$meta$x_var, "x")
})

test_that("toxval_predict on a binomial bayesnecfit gives a proportion", {
  pred <- toxval_predict(bayesnec_binomial, resolution = 50)

  expect_true(all(pred$curves[[1]] >= 0 & pred$curves[[1]] <= 1))
  expect_identical(response_upper_bound(pred$meta, pred$keys, 1), 1)
})

# x_var and group_var -----------------------------------------------------

test_that("toxval_predict on a bayesnecfit reads x_var from the crf() term", {
  pred <- toxval_predict(bayesnec_binomial, resolution = 50)
  expect_identical(pred$meta$x_var, "x")

  expect_identical(
    toxval_predict(bayesnec_binomial, x_var = "x", resolution = 50),
    pred
  )
})

test_that("toxval_predict on a bayesnecfit errors on a different x_var", {
  expect_error(
    toxval_predict(bayesnec_binomial, x_var = "n"),
    "`x_var` must be 'x', the predictor in the `crf()` term of `object`, not 'n'.",
    fixed = TRUE
  )
})

test_that("toxval_predict on a bayesnecfit errors when group_var is supplied", {
  expect_error(
    toxval_predict(bnec_model_1, group_var = "g"),
    "`group_var` must be NULL for a 'bayesnecfit', which has one curve",
    fixed = TRUE
  )
})

# refusals ----------------------------------------------------------------

test_that("toxval_predict refuses one level of a joint refit", {
  fit <- bnec_model_1
  class(fit) <- c("bayesnecjointlevel", class(fit))

  expect_error(
    toxval_predict(fit),
    "`object` must not be one level of a `bnec_joint()` refit",
    fixed = TRUE
  )
})

test_that("toxval_predict refuses a bayesnecfit with a two-part family", {
  for (family in c(
    "hurdle_gamma",
    "hurdle_poisson",
    "hurdle_negbinomial",
    "zero_inflated_beta"
  )) {
    fit <- bnec_model_1
    fit$fit$family$family <- family
    expect_error(
      toxval_predict(fit),
      paste0(
        "`object` must not be fitted with the family '",
        family,
        "', which models the zeros in a separate part"
      ),
      fixed = TRUE
    )
  }
})

test_that("toxval_predict refuses the count part of a bnec_hurdle() fit", {
  fit <- bnec_model_1
  fit$fit$family$family <- "poisson"
  attr(fit$bayesnecformula, "bayesnec_internal_truncation") <- TRUE

  expect_error(
    toxval_predict(fit),
    "`object` must not be the zero-truncated count part of a `bnec_hurdle()` fit",
    fixed = TRUE
  )
})

test_that("toxval_predict refuses the bayesnec classes without a method", {
  for (cls in c("bayesnecgroupfit", "bayesnechurdlefit", "bayesnecjointfit")) {
    fit <- structure(list(), class = c(cls, "bnecfit"))
    expect_error(
      toxval_predict(fit),
      paste0(
        "`object` must be a 'bayesnecfit' or 'bayesmanecfit' to be predicted ",
        "by `toxval_predict()`, not a '",
        cls,
        "'."
      ),
      fixed = TRUE
    )
  }
})

test_that("toxval_predict on a bayesnecfit errors when bayesnec is not installed", {
  local_mocked_bindings(bayesnec_available = function() FALSE)

  expect_error(
    toxval_predict(bnec_model_1),
    "`bayesnec` must be installed to compute on a 'bayesnecfit'.",
    fixed = TRUE
  )
})

test_that("a misspelt or abbreviated argument errors for the bayesnec classes", {
  expect_error(
    ecx(bnec_model_1, proprtion = 0.5),
    "`...` must be unused for a `bayesnecfit`, not 'proprtion'"
  )
  expect_error(
    toxval_predict(bayesnec_manec_example, resol = 20),
    "`...` must be unused for a `bayesmanecfit`, not 'resol'"
  )
})

# the metric functions ----------------------------------------------------

test_that("a bayesnecfit toxval_pred passes through the compute functions", {
  pred <- toxval_predict(bayesnec_ecx4param, resolution = 50)

  ecx_rows <- summarise_metric(compute_ecx(pred), pred, "ecx")
  nsec_rows <- summarise_metric(compute_nsec(pred), pred, "nsec")

  expect_s3_class(ecx_rows, "toxval")
  expect_s3_class(nsec_rows, "toxval")
  expect_identical(ecx_rows$direction, c("decreasing", "increasing"))
  expect_identical(nsec_rows$direction, c("decreasing", "increasing"))
})

test_that("ecx() on a bayesnecfit returns a toxval tibble", {
  fits <- bnec_single_fits()
  for (nm in names(fits)) {
    # nechorme rises before it falls, so the increasing ECx of bnec_model_1 is
    # censored in most realisations and warns (T25), which test-summarise.R
    # tests; only the class of the result is tested here
    result <- suppressWarnings(ecx(fits[[nm]], resolution = 50))
    expect_s3_class(result, "toxval")
    expect_identical(nrow(result), 2L, info = nm)
  }
})

test_that("nsec() on a bayesnecfit returns a toxval tibble", {
  fits <- bnec_single_fits()
  for (nm in names(fits)) {
    result <- suppressWarnings(nsec(fits[[nm]], resolution = 50))
    expect_s3_class(result, "toxval")
    expect_identical(result$direction, c("decreasing", "increasing"))
  }
})

test_that("nsec() on a bayesnecfit gives the same result under either anchor", {
  model <- nsec(bayesnec_ecx4param, resolution = 50)
  component <- nsec(bayesnec_ecx4param, resolution = 50, anchor = "component")

  expect_identical(component$anchor, c("component", "component"))
  expect_identical(component$estimate, model$estimate)
  expect_identical(component$reference, model$reference)
})

# the model average --------------------------------------------------------

# bayesnec's own averaged prediction of a model set, over its own grid
bnec_average <- function(fit, resolution = 50) {
  brms::posterior_epred(
    fit,
    newdata = bayesnec::bnec_newdata(fit, resolution = resolution)
  )
}

test_that("toxval_predict on a bayesmanecfit gives the curves bayesnec averages", {
  # manec_example holds no stored index, so bayesnec rebuilds it from seed 216
  pred <- toxval_predict(bayesnec_manec_example, resolution = 50)

  expect_identical(
    unname(pred$curves[[1]]),
    unname(bnec_average(bayesnec_manec_example))
  )
})

test_that("toxval_predict on a bayesmanecfit records the model of each realisation", {
  fit <- bayesnec_manec_example
  pred <- toxval_predict(fit, resolution = 50)
  model_set <- names(fit$mod_fits)
  weights <- fit$mod_stats[model_set, "wi"]

  # each model keeps round(n * weight) of the n = 100 draws, bound in the
  # order of mod_fits
  expect_identical(
    pred$meta$draw_model,
    rep(model_set, round(100 * weights))
  )
  expect_identical(
    as.vector(table(factor(pred$meta$draw_model, levels = model_set))),
    as.integer(round(100 * weights))
  )
  expect_equal(pred$meta$model_weights, stats::setNames(weights, model_set))
  expect_equal(pred$meta$seed, 216)
  expect_identical(pred$meta$source_class, "bayesmanecfit")
  expect_identical(pred$meta$family, "gaussian")
  expect_identical(pred$meta$n_realisation, 100L)
  expect_identical(
    pred$meta$x_control,
    min(fit$mod_fits[[1]]$fit$data$x)
  )
})

test_that("toxval_predict on a bayesmanecfit leaves the random number state unchanged", {
  # a non-default normal kind shows that the kinds are restored, not only the
  # seed
  withr::local_seed(42, .rng_normal_kind = "Box-Muller")
  seed_before <- get(".Random.seed", envir = globalenv())
  kind_before <- RNGkind()

  first <- toxval_predict(bayesnec_manec_example, resolution = 50)

  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
  expect_identical(RNGkind(), kind_before)
  expect_identical(
    toxval_predict(bayesnec_manec_example, resolution = 50),
    first
  )
})

test_that("toxval_predict on a bayesmanecfit uses a stored index made for the same draws", {
  fit <- bayesnec_manec_example
  fit$w_draw_index <- list(nec4param = 1:80, ecx4param = 81:100)
  fit$w_draw_seed <- 7L
  fit$sample_size <- 100

  pred <- toxval_predict(fit, resolution = 50)

  expect_identical(unname(pred$curves[[1]]), unname(bnec_average(fit)))
  expect_identical(
    pred$meta$draw_model,
    rep(c("nec4param", "ecx4param"), c(80, 20))
  )
  expect_identical(pred$meta$seed, 7L)
})

test_that("toxval_predict on a bayesmanecfit rebuilds an index made for other draws", {
  fit <- bayesnec_manec_example
  fit$w_draw_index <- list(nec4param = 1:80, ecx4param = 81:100)
  fit$w_draw_seed <- 7L
  # the stored index was made for 99 draws, not the 100 each model holds
  fit$sample_size <- 99

  pred <- toxval_predict(fit, resolution = 50)

  expect_identical(unname(pred$curves[[1]]), unname(bnec_average(fit)))
  expect_identical(
    as.vector(table(factor(
      pred$meta$draw_model,
      levels = names(fit$mod_fits)
    ))),
    c(83L, 17L)
  )
  expect_identical(pred$meta$seed, 7L)
})

test_that("toxval_predict on a bayesmanecfit keeps the weight of a model given no draws", {
  fit <- bayesnec_manec_example
  fit$mod_stats[c("nec4param", "ecx4param"), "wi"] <- c(0.999, 0.001)

  pred <- toxval_predict(fit, resolution = 50)

  # round(100 * 0.001) is 0, so ecx4param gives no realisation
  expect_identical(pred$meta$draw_model, rep("nec4param", 100))
  expect_equal(
    pred$meta$model_weights,
    c(nec4param = 0.999, ecx4param = 0.001)
  )
  expect_identical(unname(pred$curves[[1]]), unname(bnec_average(fit)))
})

test_that("toxval_predict refuses a bayesmanecfit whose models differ in family", {
  fit <- bayesnec_manec_example
  fit$mod_fits$ecx4param$fit$family$family <- "beta"

  expect_error(
    toxval_predict(fit),
    "Every model in `object` must have the same family, not 'gaussian' and 'beta'.",
    fixed = TRUE
  )
})

test_that("toxval_predict refuses a bayesmanecfit with a two-part family", {
  fit <- bayesnec_manec_example
  fit$mod_fits$ecx4param$fit$family$family <- "hurdle_gamma"

  expect_error(
    toxval_predict(fit),
    "`object` must not be fitted with the family 'hurdle_gamma'",
    fixed = TRUE
  )
})

test_that("toxval_predict on a bayesmanecfit errors when group_var is supplied", {
  expect_error(
    toxval_predict(bayesnec_manec_example, group_var = "g"),
    "`group_var` must be NULL for a 'bayesmanecfit', which has one curve",
    fixed = TRUE
  )
})

test_that("toxval_predict on a bayesmanecfit errors when bayesnec is not installed", {
  local_mocked_bindings(bayesnec_available = function() FALSE)

  expect_error(
    toxval_predict(bayesnec_manec_example),
    "`bayesnec` must be installed to compute on a 'bayesmanecfit'.",
    fixed = TRUE
  )
})

test_that("ecx() on a bayesmanecfit returns a toxval tibble", {
  result <- ecx(bayesnec_manec_example, resolution = 50)

  expect_s3_class(result, "toxval")
  expect_identical(nrow(result), 2L)
})

test_that("nsec() on a bayesmanecfit returns a toxval tibble", {
  result <- nsec(bayesnec_manec_example, resolution = 50)

  expect_s3_class(result, "toxval")
  expect_identical(result$direction, c("decreasing", "increasing"))
  expect_identical(result$anchor, c("model", "model"))
})

test_that("nsec() refuses the component anchor on a bayesmanecfit", {
  expect_error(
    nsec(bayesnec_manec_example, resolution = 50, anchor = "component"),
    "must be 'model' for a model-averaged fit"
  )
})
