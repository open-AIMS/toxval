ryegrass_set <- function() {
  list(
    LL.4 = drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4()),
    W1.4 = drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::W1.4()),
    W2.4 = drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::W2.4())
  )
}

test_that("drc set direct ECx and AIC weights agree with maED", {
  skip_if_not_installed("drc")
  fits <- ryegrass_set()
  ma <- drc::maED(
    fits[[1]],
    list(drc::W1.4(), drc::W2.4()),
    respLev = 7,
    type = "absolute",
    display = FALSE,
    extended = TRUE
  )
  ma_fits <- ma[1, "fits"][[1]]
  ma_estimate <- ma[1, "estimates"][[1]][1, 1]
  pred <- toxval_predict(fits, n_boot = 3000, seed = 42)
  result <- ecx(pred, type = "direct", reference = 7)

  expect_equal(
    unname(pred$meta$model_weights),
    unname(ma_fits[, "Weight"]),
    tolerance = 1e-10
  )
  # At this seed, LL.4 alone is 0.054 below maED and equal weights are 0.025
  # below it; a tolerance of 0.02 distinguishes both from the AIC mixture.
  expect_lt(
    abs(result$estimate[result$direction == "decreasing"] - ma_estimate),
    0.02
  )
  expect_equal(length(pred$meta$draw_model), 3000)
  expect_equal(sum(table(pred$meta$draw_model)), 3000)
  expect_lte(
    max(abs(
      as.numeric(table(pred$meta$draw_model)) /
        3000 -
        unname(pred$meta$model_weights)
    )),
    1 / 3000 + 1e-12
  )
})

test_that("relative ECx and NSEC refuse a set with a fit lacking asymptotes", {
  skip_if_not_installed("drc")
  fits <- ryegrass_set()[1:2]
  pred <- toxval_predict(fits, n_boot = 1, seed = 42)
  message <- "relative.*asymptote from every fit"

  expect_null(pred$asymptote)
  expect_identical(unname(pred$meta$draw_model), "LL.4")
  expect_error(suppressWarnings(ecx(pred, type = "relative")), message)
  expect_error(
    suppressWarnings(ecx(fits, type = "relative", n_boot = 10)),
    message
  )
  expect_error(
    suppressWarnings(nsec(fits, type = "relative", n_boot = 10)),
    message
  )
})

test_that("one drc fit as a set has the fit's realisations and estimates", {
  skip_if_not_installed("drc")
  fit <- ryegrass_set()[[1]]
  single <- toxval_predict(fit, n_boot = 80, seed = 17)
  set <- toxval_predict(list(fit), n_boot = 80, seed = 17)

  expect_identical(set$curves, single$curves)
  expect_identical(set$asymptote, single$asymptote)
  expect_equal(ecx(set)$estimate, ecx(single)$estimate)
  expect_equal(nsec(set)$estimate, nsec(single)$estimate)
  expect_identical(set$meta$model_weights, c(LL.4 = 1))
  expect_identical(unique(set$meta$draw_model), "LL.4")

  unnamed <- toxval_predict(unname(ryegrass_set()), n_boot = 6, seed = 17)
  expect_named(unnamed$meta$model_weights, c("LL.4", "W1.4", "W2.4"))

  fct <- drc::LL.4()
  indirect <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = fct)
  expect_error(toxval_predict(list(indirect)), "supply names for `object`")
})

test_that("drc sets refuse different data, response, predictor and curve identifier", {
  skip_if_not_installed("drc")
  fit <- ryegrass_set()[[1]]
  different_data <- drc::drm(
    rootl ~ conc,
    data = drc::ryegrass[-1, ],
    fct = drc::LL.4()
  )
  expect_error(toxval_predict(list(fit, different_data)), "same data")

  data <- drc::ryegrass
  data$rootl2 <- data$rootl
  different_response <- drc::drm(rootl2 ~ conc, data = data, fct = drc::LL.4())
  expect_error(toxval_predict(list(fit, different_response)), "same response")

  data$conc2 <- data$conc
  different_predictor <- drc::drm(rootl ~ conc2, data = data, fct = drc::LL.4())
  expect_error(toxval_predict(list(fit, different_predictor)), "same predictor")

  grouped <- drc::drm(
    DryMatter ~ Dose,
    curveid = Herbicide,
    data = drc::S.alba,
    fct = drc::LL.4()
  )
  ungrouped <- drc::drm(
    DryMatter ~ Dose,
    data = drc::S.alba,
    fct = drc::LL.4()
  )
  expect_error(
    toxval_predict(list(grouped, ungrouped)),
    "same curve identifier"
  )
  expect_error(toxval_predict(list(fit, 1)), "element 2 has class 'numeric'")
})

test_that("drc set checks names, classes, predictor and finite AIC", {
  skip_if_not_installed("drc")
  fit <- ryegrass_set()[[1]]

  expect_error(toxval_predict(list()), "non-empty list")
  expect_error(toxval_predict(list(a = fit, a = fit)), "unique model names")
  partial <- stats::setNames(list(fit, fit), c("a", ""))
  expect_error(toxval_predict(partial), "non-empty names")
  brms_object <- structure(list(), class = "brmsfit")
  expect_error(
    toxval_predict(list(fit, brms_object)),
    "element 2 has class 'brmsfit'"
  )
  expect_error(toxval_predict(list(fit), x_var = "rootl"), "predictor")

  bad_aic <- fit
  bad_aic$estMethod$llfct <- function(object) c(Inf, length(coef(object)))
  expect_false(is.finite(stats::AIC(bad_aic)))
  expect_error(toxval_predict(list(a = fit, b = bad_aic)), "finite AIC")
})

test_that("drc set checks response distribution and curve levels", {
  skip_if_not_installed("drc")
  formula <- (total - no) / total ~ dose
  binomial <- drc::drm(
    formula,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "binomial"
  )
  continuous <- drc::drm(
    formula,
    weights = total,
    curveid = time,
    data = drc::daphnids,
    fct = drc::LL.2(),
    type = "continuous"
  )
  expect_error(
    toxval_predict(list(binomial, continuous)),
    "same response distribution"
  )
  binomial_set <- toxval_predict(
    list(a = binomial, b = binomial),
    group_var = "time",
    n_boot = 20,
    seed = 5
  )
  expect_equal(
    toxval:::response_upper_bound(binomial_set$meta, binomial_set$keys, 1),
    1
  )

  grouped <- drc::drm(
    DryMatter ~ Dose,
    curveid = Herbicide,
    data = drc::S.alba,
    fct = drc::LL.4()
  )
  reordered <- drc::S.alba
  reordered$Herbicide <- factor(
    reordered$Herbicide,
    levels = rev(levels(reordered$Herbicide))
  )
  different_levels <- drc::drm(
    DryMatter ~ Dose,
    curveid = Herbicide,
    data = reordered,
    fct = drc::LL.4()
  )
  expect_error(
    toxval_predict(list(grouped, different_levels)),
    "same curve identifier levels"
  )
})

test_that("drc set has its own header and cannot combine with a single fit", {
  skip_if_not_installed("drc")
  fit <- ryegrass_set()[[1]]
  single <- nsec(fit, n_boot = 30, seed = 9)
  set <- nsec(list(LL.4 = fit), n_boot = 30, seed = 9)

  expect_identical(pillar::tbl_sum(set)[["Model"]], "drc_set")
  expect_error(dplyr::bind_rows(single, set), "Their run settings differ")
})

test_that("NSEC on a drc set uses the model anchor and keeps draw provenance", {
  skip_if_not_installed("drc")
  fits <- ryegrass_set()
  pred <- toxval_predict(fits, n_boot = 1000, seed = 5)
  result <- nsec(fits, n_boot = 1000, seed = 5)
  component <- nsec(pred, anchor = "component")
  standalone <- withr::with_seed(
    5,
    lapply(fits, function(fit) {
      toxval_predict(fit, n_boot = 1000)
    })
  )
  raw <- toxval:::compute_nsec(pred, sig_val = 0.01, anchor = "component")
  decreasing_reference <- raw$reference[[which(raw$direction == "decreasing")]]

  expect_equal(result, nsec(pred))
  expect_equal(
    result$censored,
    vapply(
      nsec(pred, draws = TRUE)$draws,
      function(d) {
        mean(is.infinite(d$value.draw))
      },
      0.0
    )
  )
  expect_equal(result$excluded, rep(0, nrow(result)))
  expect_true(all(result$anchor == "model"))
  expect_true(all(component$anchor == "component"))
  expect_lt(sum(pred$meta$draw_model == "W1.4"), 200)
  for (model in names(fits)) {
    controls <- unname(standalone[[model]]$curves[[1]][, 1])
    expect_identical(pred$meta$model_control[[1]][[model]], controls)
    expected <- unname(stats::quantile(controls, 0.01))
    observed <- unique(decreasing_reference[pred$meta$draw_model == model])
    expect_equal(observed, expected)
  }
  expect_lte(
    max(abs(
      as.numeric(table(pred$meta$draw_model)) /
        1000 -
        unname(pred$meta$model_weights)
    )),
    1 / 1000 + 1e-12
  )
  expect_named(pred$meta$model_control[[1]], names(pred$meta$model_weights))
})

test_that("a grouped drc set gives one decreasing row per curve identifier", {
  skip_if_not_installed("drc")
  fits <- list(
    LL.4 = drc::drm(
      DryMatter ~ Dose,
      curveid = Herbicide,
      data = drc::S.alba,
      fct = drc::LL.4()
    ),
    W1.4 = drc::drm(
      DryMatter ~ Dose,
      curveid = Herbicide,
      data = drc::S.alba,
      fct = drc::W1.4()
    )
  )
  pred <- toxval_predict(fits, group_var = "Herbicide", n_boot = 80, seed = 7)
  result <- nsec(pred)

  expect_equal(nrow(pred$keys), 2)
  expect_equal(
    as.character(result$group[result$direction == "decreasing"]),
    c("Bentazone", "Glyphosate")
  )
  expect_equal(length(pred$curves), 2)
  expect_true(all(vapply(pred$curves, nrow, integer(1)) == 80))
})

test_that("set bootstrap seeds are reproducible and draw independently across fits", {
  skip_if_not_installed("drc")
  fits <- ryegrass_set()
  first <- toxval_predict(fits, n_boot = 60, seed = 9)
  again <- toxval_predict(fits, n_boot = 60, seed = 9)
  other <- toxval_predict(fits, n_boot = 60, seed = 10)

  expect_identical(first, again)
  expect_false(identical(first$curves, other$curves))

  same_fit <- list(a = fits[[1]], b = fits[[1]])
  pred <- toxval_predict(same_fit, n_boot = 20, seed = 9)
  expect_false(identical(pred$curves[[1]][1:10, ], pred$curves[[1]][11:20, ]))
})

test_that("a fit without an allocated draw remains in model weights", {
  skip_if_not_installed("drc")
  pred <- toxval_predict(ryegrass_set(), n_boot = 1, seed = 9)

  expect_length(pred$meta$model_weights, 3)
  expect_length(pred$meta$draw_model, 1)
  expect_equal(sum(pred$meta$model_weights), 1)
})
