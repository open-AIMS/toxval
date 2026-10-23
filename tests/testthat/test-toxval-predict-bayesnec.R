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

test_that("toxval_predict on a bayesnecfit honours a supplied x_range", {
  pred <- toxval_predict(bnec_model_1, resolution = 50, x_range = c(0.6, 1))

  expect_equal(range(pred$x_vec), c(0.6, 1))
  expect_length(pred$x_vec, 50)
  # the control stays at the lowest concentration in the data (T21)
  expect_identical(pred$meta$x_control, min(bnec_model_1$fit$data$x))
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
  expect_identical(response_upper_bound(pred$meta), 1)
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
        "`object` must be a 'bayesnecfit' to be predicted by ",
        "`toxval_predict()`, not a '",
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
