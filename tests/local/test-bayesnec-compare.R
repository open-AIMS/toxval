# Comparisons of toxval's estimates with bayesnec's own on bnec() fits
# (BAYESNEC-claude.md §2.2 and task 5). Run by hand from the repository root:
#
#   testthat::test_file(
#     "tests/local/test-bayesnec-compare.R",
#     package = "toxval",
#     load_package = "source"
#   )
#
# The robustness fits are compared too when they have been made with the
# scripts in bayesnec-robustness/; otherwise that test is skipped.

skip_if_not_installed("bayesnec")

resolution <- 200

load_fixture <- function(name) {
  env <- new.env()
  load(
    test_path("..", "testthat", "fixtures", paste0(name, ".rda")),
    envir = env
  )
  env[[name]]
}

# The decreasing absolute EC10 of every realisation from each package, whether
# both found a crossing within the grid, and the grid step
paired_ecx <- function(fit) {
  pred <- toxval_predict(fit, resolution = resolution)
  toxval_draws <- toxval:::compute_ecx(pred, proportion = 0.1)$draws[[1]]
  bnec_draws <- suppressWarnings(suppressMessages(bayesnec::ecx(
    fit,
    ecx_val = 10,
    resolution = resolution,
    posterior = TRUE
  )))
  crossed <- is.finite(toxval_draws) & is.finite(bnec_draws)
  # a model set records no censoring, so the finite values are compared
  censored <- attr(bnec_draws, "censored")
  if (!is.null(censored)) {
    crossed <- crossed & !censored$above & !censored$below
  }
  list(
    toxval = toxval_draws,
    bayesnec = as.numeric(bnec_draws),
    crossed = crossed,
    step = diff(pred$x_vec)[1]
  )
}

# toxval interpolates the crossing exactly. bayesnec finds the root of the same
# interpolant with modelbased's .uniroot.all(), whose tolerance is
# .Machine$double.eps^0.2 in grid steps, so the two agree only to that.
expect_same_crossing <- function(toxval, bayesnec, step, info = NULL) {
  expect_lte(
    max(abs(toxval - bayesnec)),
    .Machine$double.eps^0.2 * step,
    label = paste("largest difference", info)
  )
}

test_that("the decreasing absolute ECx of each realisation equals bayesnec's", {
  fixtures <- c(
    "bnec_model_1",
    "bayesnec_nec4param",
    "bayesnec_ecx4param",
    "bayesnec_binomial",
    "bayesnec_manec_example"
  )
  for (name in fixtures) {
    paired <- paired_ecx(load_fixture(name))
    expect_gt(sum(paired$crossed), 0)
    expect_same_crossing(
      paired$toxval[paired$crossed],
      paired$bayesnec[paired$crossed],
      paired$step,
      info = name
    )
  }
})

test_that("a crf(log(x)) fit reports ECx on the recorded scale", {
  fit <- load_fixture("bayesnec_logx")
  paired <- paired_ecx(fit)

  expect_gt(sum(paired$crossed), 0)
  expect_same_crossing(
    paired$toxval[paired$crossed],
    paired$bayesnec[paired$crossed],
    paired$step
  )

  fitted_scale <- suppressWarnings(suppressMessages(bayesnec::ecx(
    fit,
    ecx_val = 10,
    resolution = resolution,
    posterior = TRUE,
    xform = identity
  )))
  expect_equal(
    exp(as.numeric(fitted_scale)[paired$crossed]),
    paired$bayesnec[paired$crossed],
    tolerance = 1e-12
  )
})

test_that("the component NSEC on manec_example equals bayesnec's", {
  fit <- load_fixture("bayesnec_manec_example")
  pred <- toxval_predict(fit, resolution = resolution)
  toxval_nsec <- nsec(
    fit,
    sig_val = 0.01,
    resolution = resolution,
    draws = TRUE
  )
  decreasing <- toxval_nsec[toxval_nsec$direction == "decreasing", ]
  toxval_draws <- decreasing$draws[[1]]$value.draw
  bnec_posterior <- suppressMessages(bayesnec::nsec(
    fit,
    sig_val = 0.01,
    resolution = resolution,
    posterior = TRUE
  ))
  bnec_draws <- as.numeric(bnec_posterior)
  censored <- attr(bnec_posterior, "censored")
  if (!is.null(censored)) {
    expect_identical(is.infinite(toxval_draws), censored$above)
  }
  crossed <- is.finite(toxval_draws) & is.finite(bnec_draws)
  expect_same_crossing(
    toxval_draws[crossed],
    bnec_draws[crossed],
    diff(pred$x_vec)[1]
  )

  expect_same_crossing(
    c(decreasing$estimate, decreasing$conf.low, decreasing$conf.high),
    unname(as.numeric(suppressMessages(bayesnec::nsec(
      fit,
      sig_val = 0.01,
      resolution = resolution
    )))),
    diff(pred$x_vec)[1]
  )
})

test_that("the robustness fits give bayesnec's ECx in each realisation", {
  fit_dir <- Sys.getenv(
    "TOXVAL_ROBUSTNESS_DIR",
    file.path(tools::R_user_dir("toxval", "cache"), "bayesnec-robustness")
  )
  paths <- list.files(fit_dir, pattern = "[.]rds$", full.names = TRUE)
  paths <- paths[!grepl("[.]error[.]rds$|^compare-", basename(paths))]
  skip_if(length(paths) == 0, "the robustness fits have not been made")

  for (path in paths) {
    fit <- readRDS(path)
    if (!inherits(fit, c("bayesnecfit", "bayesmanecfit"))) {
      next
    }
    paired <- tryCatch(paired_ecx(fit), error = function(e) e)
    if (inherits(paired, "error")) {
      # a refused structure, such as a two-part family, is not compared
      next
    }
    if (any(paired$crossed)) {
      expect_same_crossing(
        paired$toxval[paired$crossed],
        paired$bayesnec[paired$crossed],
        paired$step,
        info = basename(path)
      )
    }
  }
})
