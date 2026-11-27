glm_test_data <- function() {
  data.frame(
    x = seq(1, 20, length.out = 40),
    y = 0.3 + 0.07 * seq_len(40) + sin(seq_len(40)) / 5,
    g = factor(rep(c("b", "a"), each = 20), levels = c("b", "a")),
    z = rep(c(0, 1), 20)
  )
}

test_that("glm prediction satisfies the toxval_pred contract and seeds", {
  d <- glm_test_data()
  fit <- stats::glm(y ~ x, data = d)
  a <- toxval_predict(
    fit,
    x_var = "x",
    resolution = 30,
    n_boot = 100,
    seed = 42
  )
  b <- toxval_predict(
    fit,
    x_var = "x",
    resolution = 30,
    n_boot = 100,
    seed = 42
  )
  c <- toxval_predict(
    fit,
    x_var = "x",
    resolution = 30,
    n_boot = 100,
    seed = 43
  )

  expect_s3_class(a, "toxval_pred")
  expect_equal(dim(a$curves[[1]]), c(100L, 30L))
  expect_equal(nrow(a$keys), 1L)
  expect_equal(a$meta$realisation, "bootstrap")
  expect_equal(a$meta$seed, 42)
  expect_equal(a$meta$family, "gaussian")
  expect_identical(a$curves, b$curves)
  expect_false(identical(a$curves, c$curves))
})

test_that("glm bootstrap medians agree with response predictions", {
  d <- glm_test_data()
  fits <- list(
    gaussian = stats::glm(y ~ x, data = d),
    quasipoisson = stats::glm(
      round(y * 4) ~ x,
      data = d,
      family = quasipoisson
    ),
    quasibinomial = stats::glm(
      pmin(y / 4, 0.95) ~ x,
      data = d,
      family = quasibinomial
    )
  )
  for (fit in fits) {
    pred <- toxval_predict(
      fit,
      x_var = "x",
      resolution = 20,
      n_boot = 2000,
      seed = 42
    )
    expected <- stats::predict(
      fit,
      newdata = data.frame(x = pred$x_vec),
      type = "response"
    )
    observed <- apply(pred$curves[[1]], 2, stats::median)
    mc_se <- apply(pred$curves[[1]], 2, stats::sd) * 1.253 / sqrt(2000)
    expect_true(all(abs(observed - expected) <= 5 * mc_se + 1e-7))
  }
})

test_that("glm formula transformations are retained on the grid", {
  d <- glm_test_data()
  k <- 2
  x0 <- 1
  for (form in list(
    y ~ log(x),
    y ~ poly(x, 2),
    y ~ poly(x, k),
    y ~ log(x + x0)
  )) {
    fit <- stats::glm(form, data = d)
    pred <- toxval_predict(
      fit,
      x_var = "x",
      resolution = 25,
      n_boot = 2000,
      seed = 42
    )
    expected <- stats::predict(
      fit,
      newdata = data.frame(x = pred$x_vec),
      type = "response"
    )
    observed <- apply(pred$curves[[1]], 2, stats::median)
    mc_se <- apply(pred$curves[[1]], 2, stats::sd) * 1.253 / sqrt(2000)
    expect_true(all(abs(observed - expected) <= 5 * mc_se + 1e-7))
  }
  fit <- stats::glm(y ~ log(x + x0), data = d)
  x0 <- 2
  expect_error(
    toxval_predict(fit, x_var = "x"),
    "data no longer match the fit; refit the glm with `data =`"
  )
})

test_that("transformed responses remain on the fitted scale", {
  d <- glm_test_data()
  fit <- stats::glm(sqrt(y) ~ x, data = d)
  pred <- toxval_predict(
    fit,
    x_var = "x",
    n_boot = 2000,
    resolution = 20,
    seed = 42
  )
  expected <- stats::predict(
    fit,
    newdata = data.frame(x = pred$x_vec),
    type = "response"
  )
  observed <- apply(pred$curves[[1]], 2, stats::median)
  expect_equal(unname(observed), unname(expected), tolerance = 0.02)
})

test_that("a glm fitted without data gives the same curves", {
  d <- glm_test_data()
  x <- d$x
  y <- d$y
  with_data <- stats::glm(y ~ x, data = d)
  without_data <- stats::glm(y ~ x)
  a <- toxval_predict(
    with_data,
    x_var = "x",
    resolution = 20,
    n_boot = 100,
    seed = 42
  )
  b <- toxval_predict(
    without_data,
    x_var = "x",
    resolution = 20,
    n_boot = 100,
    seed = 42
  )
  expect_equal(a$curves, b$curves)

  with_data_log <- stats::glm(y ~ log(x), data = d)
  without_data_log <- stats::glm(y ~ log(x))
  a_log <- toxval_predict(
    with_data_log,
    x_var = "x",
    resolution = 20,
    n_boot = 100,
    seed = 42
  )
  b_log <- toxval_predict(
    without_data_log,
    x_var = "x",
    resolution = 20,
    n_boot = 100,
    seed = 42
  )
  expect_equal(a_log$curves, b_log$curves)
  x <- x + 100
  expect_error(
    toxval_predict(without_data_log, x_var = "x"),
    "data no longer match the fit; refit the glm with `data =`"
  )
})

test_that("grouped glm curves match predict in level order", {
  d <- glm_test_data()
  fit <- stats::glm(y ~ x * g, data = d)
  testthat::local_mocked_bindings(
    boot_parms = function(
      object,
      n_boot,
      seed = NULL,
      log_scale = character()
    ) {
      matrix(
        stats::coef(object),
        nrow = 1,
        dimnames = list(NULL, names(stats::coef(object)))
      )
    },
    .package = "toxval"
  )
  pred <- toxval_predict(
    fit,
    x_var = "x",
    group_var = "g",
    resolution = 12,
    n_boot = 1,
    seed = 42
  )
  expect_equal(as.character(pred$keys$group), c("b", "a"))
  expect_length(pred$curves, 2)
  for (i in seq_along(pred$curves)) {
    newdata <- data.frame(x = pred$x_vec, g = pred$keys$group[i])
    expected <- stats::predict(fit, newdata = newdata, type = "response")
    expect_equal(unname(as.numeric(pred$curves[[i]])), unname(expected))
  }
  expect_error(
    toxval_predict(fit, x_var = "x", group_var = "missing"),
    "`group_var`"
  )
  fit_without_group <- stats::glm(y ~ x, data = d)
  expect_error(
    toxval_predict(fit_without_group, x_var = "x", group_var = "g"),
    "`group_var` names a data column that does not enter the model"
  )
  d$g <- as.character(d$g)
  character_fit <- stats::glm(y ~ x * g, data = d)
  character_pred <- toxval_predict(character_fit, x_var = "x", group_var = "g")
  expect_equal(as.character(character_pred$keys$group), c("a", "b"))
  expect_equal(range(character_pred$x_vec), range(d$x))
})

test_that("glm grids use only the observations in the fit", {
  d <- glm_test_data()
  subset_fit <- stats::glm(y ~ x, data = d, subset = x > 5)
  subset_pred <- toxval_predict(subset_fit, x_var = "x", n_boot = 20, seed = 42)
  expect_equal(range(subset_pred$x_vec), range(d$x[d$x > 5]))

  d$y[1] <- NA_real_
  response_fit <- stats::glm(y ~ x, data = d)
  response_pred <- toxval_predict(
    response_fit,
    x_var = "x",
    n_boot = 20,
    seed = 42
  )
  expect_equal(range(response_pred$x_vec), range(d$x[-1]))

  d$y[1] <- glm_test_data()$y[1]
  d$x[1] <- NA_real_
  predictor_fit <- stats::glm(y ~ x, data = d)
  predictor_pred <- toxval_predict(
    predictor_fit,
    x_var = "x",
    n_boot = 20,
    seed = 42
  )
  expect_equal(range(predictor_pred$x_vec), range(d$x[-1]))

  d <- glm_test_data()
  d$g <- as.character(d$g)
  d$g[1] <- "c"
  group_fit <- stats::glm(y ~ x * g, data = d, subset = x > 1)
  group_pred <- toxval_predict(
    group_fit,
    x_var = "x",
    group_var = "g",
    n_boot = 20,
    seed = 42
  )
  expect_equal(as.character(group_pred$keys$group), c("a", "b"))
  expect_equal(range(group_pred$x_vec), range(d$x[d$x > 1]))
})

test_that("log predictor grids use the fitted range", {
  d <- glm_test_data()
  fit <- stats::glm(y ~ log(x), data = d)
  pred <- toxval_predict(fit, x_var = "x", n_boot = 20, seed = 42)
  expect_equal(range(pred$x_vec), range(d$x))
})

test_that("unsupported glm formulae and subclasses give clear errors", {
  d <- glm_test_data()
  fit <- stats::glm(y ~ x, data = d)
  offset_fit <- stats::glm(y ~ x + offset(z), data = d)
  offset_argument_fit <- stats::glm(y ~ x, data = d, offset = z)
  extra_fit <- stats::glm(y ~ x + z, data = d)
  expect_snapshot(error = TRUE, toxval_predict(fit))
  expect_snapshot(error = TRUE, toxval_predict(offset_fit, x_var = "x"))
  expect_error(toxval_predict(offset_argument_fit, x_var = "x"), "offset")
  expect_snapshot(error = TRUE, toxval_predict(extra_fit, x_var = "x"))
  expect_error(
    toxval_predict(fit, x_var = "y"),
    "`x_var` names a data column that does not enter the model"
  )
  subclass_fit <- fit
  class(subclass_fit) <- c("otherglm", class(fit))
  expect_error(toxval_predict(subclass_fit, x_var = "x"), "plain `glm`")
  aliased_fit <- stats::glm(y ~ x + I(2 * x), data = d)
  expect_error(
    toxval_predict(aliased_fit, x_var = "x"),
    "`coef\\(object\\)` must not contain missing values"
  )
})

test_that("mgcv gam is refused before glm prediction", {
  skip_if_not_installed("mgcv")
  d <- glm_test_data()
  gam_fit <- mgcv::gam(y ~ s(x), data = d)
  expect_snapshot(error = TRUE, toxval_predict(gam_fit, x_var = "x"))
})

test_that("negative-binomial glm fits are accepted with fixed theta", {
  skip_if_not_installed("MASS")
  set.seed(42)
  d <- glm_test_data()
  d$count <- stats::rnbinom(nrow(d), mu = exp(0.2 + 0.1 * d$x), size = 2)
  fit <- MASS::glm.nb(count ~ x, data = d)
  pred <- toxval_predict(
    fit,
    x_var = "x",
    resolution = 10,
    n_boot = 20,
    seed = 42
  )
  expect_s3_class(pred, "toxval_pred")
  expect_equal(pred$meta$source_class, "negbin")
  expect_equal(range(pred$x_vec), range(d$x))
  expect_equal(toxval:::response_upper_bound(pred$meta, pred$keys, 1), Inf)
})

test_that("glm proportion families have an upper bound of one", {
  d <- glm_test_data()
  for (family in list(binomial(), quasibinomial())) {
    fit <- suppressWarnings(stats::glm(
      pmin(y / 4, 0.95) ~ x,
      data = d,
      family = family
    ))
    pred <- toxval_predict(fit, x_var = "x", n_boot = 20, seed = 42)
    expect_equal(toxval:::response_upper_bound(pred$meta, pred$keys, 1), 1)
  }
  for (family in list(gaussian(), quasipoisson())) {
    fit <- stats::glm(y ~ x, data = d, family = family)
    pred <- toxval_predict(fit, x_var = "x", n_boot = 20, seed = 42)
    expect_equal(toxval:::response_upper_bound(pred$meta, pred$keys, 1), Inf)
  }
  fit <- stats::glm(pmin(y / 4, 0.95) ~ x, data = d, family = quasibinomial)
  result <- suppressWarnings(ecx(
    fit,
    x_var = "x",
    proportion = 0.1,
    n_boot = 200,
    seed = 42
  ))
  increasing <- result[result$direction == "increasing", ]
  expect_equal(
    increasing$reference,
    increasing$control + 0.1 * (1 - increasing$control),
    tolerance = 0.02
  )
})

test_that("weighted and two-column binomial fits predict proportions", {
  d <- glm_test_data()
  d$success <- round(pmin(d$y / 4, 0.95) * 10)
  d$failure <- 10 - d$success
  weighted <- stats::glm(
    success / 10 ~ x,
    data = d,
    weights = rep(10, nrow(d)),
    family = binomial
  )
  two_column <- stats::glm(
    cbind(success, failure) ~ x,
    data = d,
    family = binomial
  )
  for (fit in list(weighted, two_column)) {
    pred <- toxval_predict(
      fit,
      x_var = "x",
      n_boot = 100,
      resolution = 20,
      seed = 42
    )
    expect_true(all(pred$curves[[1]] >= 0 & pred$curves[[1]] <= 1))
    expect_equal(toxval:::response_upper_bound(pred$meta, pred$keys, 1), 1)
  }
})

test_that("ecx and nsec accept a glm through toxval_predict", {
  d <- glm_test_data()
  fit <- stats::glm(y ~ x, data = d)
  ec <- suppressWarnings(ecx(
    fit,
    x_var = "x",
    proportion = 0.1,
    n_boot = 100,
    seed = 42
  ))
  ns <- suppressWarnings(nsec(fit, x_var = "x", n_boot = 100, seed = 42))
  expect_s3_class(ec, "toxval")
  expect_s3_class(ns, "toxval")
  expect_equal(ec$metric[1], "ecx")
  expect_equal(ns$metric[1], "nsec")
})

test_that("Acropora NSEC at the seep agrees with the published result", {
  d <- oa_seep_cover
  expect_equal(nrow(d), 37L)
  d$om_deficit <- 3.5 - d$OmAr.mean
  fit <- stats::glm(
    ACROPORA / 100 ~ om_deficit,
    data = d,
    family = quasibinomial
  )
  pred <- toxval_predict(
    fit,
    x_var = "om_deficit",
    x_control = 0,
    seed = 42,
    n_boot = 4000
  )
  expect_equal(pred$x_vec[1], 0)
  result <- NULL
  expect_warning(
    result <- nsec(
      fit,
      x_var = "om_deficit",
      x_control = 0,
      sig_val = 0.005,
      seed = 42,
      n_boot = 4000
    ),
    "Some realisations have no crossing within the evaluated range"
  )
  expect_equal(unique(result$sig_val), 0.005)
  expect_equal(result$control[1], stats::median(pred$curves[[1]][, 1]))
  estimate <- result$estimate[result$direction == "decreasing"]
  expect_equal(estimate, 0.685, tolerance = 0.05)
})
