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

test_that("a crf(log(x)) fit reports the exponential of bayesnec's ECx", {
  # bayesnec reports the same crossing in units of log(x)
  paired <- paired_ecx(load_fixture("bayesnec_logx"))

  expect_gt(sum(paired$crossed), 0)
  expect_same_crossing(
    paired$toxval[paired$crossed],
    exp(paired$bayesnec[paired$crossed]),
    paired$step
  )
})

test_that("the component NSEC on manec_example is reported beside bayesnec's", {
  # the difference is reported, not asserted (BAYESNEC-claude.md §3.2):
  # toxval takes each model's reference from that model's realisations in the
  # average, and bayesnec from all of that model's draws
  fit <- load_fixture("bayesnec_manec_example")
  toxval_nsec <- nsec(
    fit,
    anchor = "component",
    sig_val = 0.01,
    resolution = resolution
  )
  bnec_nsec <- suppressMessages(bayesnec::nsec(
    fit,
    sig_val = 0.01,
    resolution = resolution
  ))
  decreasing <- toxval_nsec[toxval_nsec$direction == "decreasing", ]
  compared <- data.frame(
    quantile = c("estimate", "conf.low", "conf.high"),
    toxval = c(decreasing$estimate, decreasing$conf.low, decreasing$conf.high),
    bayesnec = unname(as.numeric(bnec_nsec))
  )
  compared$difference <- compared$toxval - compared$bayesnec
  message(paste(utils::capture.output(print(compared)), collapse = "\n"))

  expect_true(all(is.finite(compared$difference)))
})

# Whether the crf() term of a bnec() formula names its predictor untransformed,
# as in crf(x, ...) but not crf(log(x), ...)
crf_untransformed <- function(formula) {
  find <- function(e) {
    if (!is.call(e)) {
      return(NULL)
    }
    if (identical(e[[1]], as.name("crf"))) {
      return(is.name(e[[2]]))
    }
    for (arg in as.list(e)[-1]) {
      found <- find(arg)
      if (!is.null(found)) {
        return(found)
      }
    }
    NULL
  }
  isTRUE(find(formula[[3]]))
}

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
    formula <- if (inherits(fit, "bayesmanecfit")) {
      fit$mod_fits[[1]]$bayesnecformula
    } else {
      fit$bayesnecformula
    }
    # bayesnec reports a transformed predictor on the transformed scale, which
    # the crf(log(x)) test above compares
    if (!crf_untransformed(formula)) {
      next
    }
    # a model set whose recorded sample_size differs from its draws keeps
    # different draws in each package (bayesnec #477)
    if (
      inherits(fit, "bayesmanecfit") &&
        !isTRUE(fit$sample_size == brms::ndraws(fit$mod_fits[[1]]$fit))
    ) {
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
