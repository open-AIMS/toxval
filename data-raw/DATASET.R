## code to prepare the example fitted-model fixtures used by the test suite
##
## NOTE: Each model is saved individually to tests/testthat/fixtures/ using
## save(..., compress = "xz"). This creates one .rda file per object. These are
## test scaffolding, not user-facing package data, and are loaded by name in
## tests via tests/testthat/helper-fixtures.R.
##
## All Bayesian models use chains = 2, iter = 2000, thin = 2.
## For brms models: warmup = 1000 (brms default = iter/2), yielding
##   (2000 - 1000) / 2 = 500 draws per chain × 2 = 1000 total draws.
## For bayesnec::bnec models: warmup = 1600 (bayesnec default = 0.8 × iter),
##   yielding (2000 - 1600) / 2 = 200 draws per chain × 2 = 400 total draws.
##
## After saving, the compiled Stan C++ DSO is stripped from each object's
## stanmodel slot. This reduces file sizes by ~100x without affecting the
## stored posterior draws or any post-fitting functions (posterior_epred,
## fitted, etc.). Models cannot be re-fitted after stripping; use the source
## data and code above to regenerate.

# Helper: strip compiled Stan binary from a brmsfit to reduce file size
strip_stanmodel <- function(file) {
  varname <- load(file)
  obj <- get(varname)
  cls <- class(obj)

  if ("brmsfit" %in% cls) {
    obj$fit@stanmodel <- methods::new("stanmodel")
    # a cmdstanr fit keeps its compiled model here, which warns on load where
    # cmdstanr is not installed
    attr(obj$fit, "CmdStanModel") <- NULL
  } else if ("bayesnecfit" %in% cls) {
    obj$fit$fit@stanmodel <- methods::new("stanmodel")
  } else if ("bayesmanecfit" %in% cls) {
    obj$mod_fits <- lapply(obj$mod_fits, function(m) {
      m$fit$fit@stanmodel <- methods::new("stanmodel")
      m
    })
  }

  assign(varname, obj)
  save(list = varname, file = file, compress = "xz")
}

# BRMS test models ----------------------------------------------------------

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8)
)

brms_model_1 <-
  brms::brm(
    y ~ log(x),
    data = data,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 101
  )
save(
  brms_model_1,
  file = "tests/testthat/fixtures/brms_model_1.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_1.rda")

# this works for the grouping examples
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8),
  z = c(1, 1, 1, 2, 2)
)

brms_model_2 <-
  brms::brm(
    y ~ log(x) + z,
    data = data,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 101
  )
save(
  brms_model_2,
  file = "tests/testthat/fixtures/brms_model_2.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_2.rda")

brms_model_3 <-
  brms::brm(
    y ~ s(x, bs = "cr", k = 5),
    data = bayesnec::nec_data,
    family = brms::Beta(),
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 123
  )
save(
  brms_model_3,
  file = "tests/testthat/fixtures/brms_model_3.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_3.rda")

data <- bayesnec::herbicide
data$x <- log(data$concentration)
data$y <- (data$fvfm + 0.001) * 0.999

brms_fit_4 <-
  bayesnec::bnec(
    y ~ crf(x, model = "ecx4param"),
    data = data,
    family = brms::Beta(),
    seed = 17,
    chains = 2,
    iter = 2000,
    thin = 2
  )

brms_pull_4 <- bayesnec::pull_brmsfit(brms_fit_4)
brms_prior_4 <- brms::prior_summary(brms_pull_4)

brms_model_4 <-
  brms::brm(
    brms::bf(
      y ~ top + (bot - top) / (1 + exp((ec50 - x) * exp(beta))),
      top + bot + beta + ec50 ~ herbicide,
      nl = TRUE
    ),
    data = data,
    family = brms::Beta(),
    prior = brms_prior_4,
    chains = 2,
    iter = 2000,
    thin = 2,
    save_pars = brms::save_pars(all = TRUE),
    seed = 700,
    init = 0
  )
save(
  brms_model_4,
  file = "tests/testthat/fixtures/brms_model_4.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_4.rda")

set.seed(123)
nec_dat_horme <- bayesnec::nec_data |>
  dplyr::mutate(
    y = (y + exp(-((x - 1.2)^2) / 0.3) + rnorm(length(x), mean = 0, sd = 0.05))
  )
brms_model_5 <-
  brms::brm(
    y ~ s(x, bs = "cr", k = 3),
    data = nec_dat_horme,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 123
  )
save(
  brms_model_5,
  file = "tests/testthat/fixtures/brms_model_5.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_5.rda")

# z enters only through a group-level term
# compared to the population level of brms_model_2
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8),
  z = c(1, 1, 1, 2, 2)
)

brms_model_6 <-
  brms::brm(
    y ~ log(x) + (1 | z),
    data = data,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 101
  )
save(
  brms_model_6,
  file = "tests/testthat/fixtures/brms_model_6.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_6.rda")

# A multivariate fit with a binomial response whose trials vary between
# observations, which toxval_predict() predicts at one trial (T26)
data <- data.frame(
  x = rep(c(0.001, 0.01, 0.1, 0.5, 1, 1.5), each = 2),
  survived = c(19, 17, 18, 19, 15, 17, 10, 9, 4, 5, 1, 0),
  n = c(20, 18, 19, 20, 17, 20, 20, 19, 18, 20, 20, 16),
  growth = c(1.9, 2.1, 2.0, 1.8, 1.7, 1.9, 1.2, 1.3, 0.7, 0.8, 0.5, 0.4)
)

surv_formula <- brms::bf(survived | trials(n) ~ x, family = binomial())
growth_formula <- brms::bf(growth ~ x)

brms_model_7 <-
  brms::brm(
    surv_formula + growth_formula,
    data = data,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 102,
    backend = "cmdstanr"
  )
save(
  brms_model_7,
  file = "tests/testthat/fixtures/brms_model_7.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/brms_model_7.rda")

# bnecfit -----------------------------------------------------------------

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.6, 0.5, 0.5, 0.9, 0.7, 0.6),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8, 0.85, 0.94, 0.9, 0.67, 0.99, 0.95)
)

set.seed(333)
bnec_model_1 <-
  bayesnec::bnec(
    y ~ crf(x, model = "nechorme"),
    data = data,
    open_progress = FALSE,
    chains = 2,
    iter = 2000,
    thin = 2
  )
save(
  bnec_model_1,
  file = "tests/testthat/fixtures/bnec_model_1.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/bnec_model_1.rda")

# nsec bayesnec -------------------------------------------------------------

bayesnec_nec4param <- bayesnec::pull_out(
  bayesnec::manec_example,
  model = "nec4param"
)
save(
  bayesnec_nec4param,
  file = "tests/testthat/fixtures/bayesnec_nec4param.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/bayesnec_nec4param.rda")

bayesnec_ecx4param <- bayesnec::pull_out(
  bayesnec::manec_example,
  model = "ecx4param"
)
save(
  bayesnec_ecx4param,
  file = "tests/testthat/fixtures/bayesnec_ecx4param.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/bayesnec_ecx4param.rda")

# toxval_predict methods for bayesnec fits -----------------------------------

# Fits for the toxval_predict() methods for bayesnecfit and bayesmanecfit
# (BAYESNEC-claude.md §4, task 1). Generated with bayesnec 2.1.3.39.

# The model average of nec4param and ecx4param. Stored whole, rather than read
# from bayesnec in the tests, because the stored object is the one whose draw
# index the tests rebuild; manec_example holds no w_draw_index, so the
# bayesmanecfit method takes the seeded rebuild path on it.
bayesnec_manec_example <- bayesnec::manec_example
save(
  bayesnec_manec_example,
  file = "tests/testthat/fixtures/bayesnec_manec_example.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/bayesnec_manec_example.rda")

# A binomial response predicted as a proportion, from nec3param, which has no
# bot parameter. Simulated from nec3param itself, with top = 0.9, nec = 1 and
# exp(beta) = 1.5: 20 trials at each of three replicates of eight
# concentrations.
set.seed(2160)
binom_data <- data.frame(
  x = rep(c(0, 0.25, 0.5, 1, 1.5, 2, 2.5, 3), each = 3),
  n = 20L
)
binom_p <- 0.9 * exp(-1.5 * pmax(binom_data$x - 1, 0))
binom_data$y <- stats::rbinom(nrow(binom_data), binom_data$n, binom_p)

bayesnec_binomial <-
  bayesnec::bnec(
    y | trials(n) ~ crf(x, model = "nec3param"),
    data = binom_data,
    family = binomial,
    seed = 2161,
    chains = 2,
    iter = 2000,
    thin = 2
  )
save(
  bayesnec_binomial,
  file = "tests/testthat/fixtures/bayesnec_binomial.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/bayesnec_binomial.rda")

# A predictor transformed inside crf(). The brmsfit data and the bayesnec grid
# hold raw x while Stan applies log(), which is what the predictor-scale tests
# (§2.1) need; a log column made before fitting would not exercise it.
# Loaded without bayesnec installed, this fixture warns that the bayesnec
# namespace was replaced by .GlobalEnv: the beta family bayesnec builds for it
# holds a reference to a bayesnec function frame. It is stored as bnec()
# returned it, and the tests that use it are skipped without bayesnec.
bayesnec_logx <-
  bayesnec::bnec(
    y ~ crf(log(x), model = "ecx4param"),
    data = bayesnec::nec_data,
    seed = 2162,
    chains = 2,
    iter = 2000,
    thin = 2
  )
save(
  bayesnec_logx,
  file = "tests/testthat/fixtures/bayesnec_logx.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/bayesnec_logx.rda")

# data from bayesnec -------------------------------------------------------

# Stored so that the drc tests fitted to these data run without bayesnec
# installed, since bayesnec is not a dependency of toxval (#39). Generated with
# bayesnec 2.1.3.39.
# The bayesnec_ prefix keeps them from masking the bayesnec datasets of the
# same name, since devtools::load_all() loads every fixture into the package
# environment through helper-fixtures.R.
bayesnec_nec_data <- bayesnec::nec_data
save(
  bayesnec_nec_data,
  file = "tests/testthat/fixtures/bayesnec_nec_data.rda",
  compress = "xz"
)

bayesnec_herbicide <- bayesnec::herbicide
save(
  bayesnec_herbicide,
  file = "tests/testthat/fixtures/bayesnec_herbicide.rda",
  compress = "xz"
)

# nsec drc ----------------------------------------------------------------

nsec_drc_1 <- drc::drm(y ~ x, data = bayesnec::nec_data, fct = drc::LL.4())
save(
  nsec_drc_1,
  file = "tests/testthat/fixtures/nsec_drc_1.rda",
  compress = "xz"
)

# increasing drc model
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5),
  y = c(1.2, 1.1, 0.8, 0.92, 0.7, 0.4, 0.3, 0.2)
)

nsec_drc_2 <- drc::drm(y ~ x, data = data, fct = drc::LL.3())
save(
  nsec_drc_2,
  file = "tests/testthat/fixtures/nsec_drc_2.rda",
  compress = "xz"
)

# nsec_multi --------------------------------------------------------------

data <- data.frame(
  dose = c(0.001, 0.005, 0.01, 0.05, 0.09, 0.1, 0.5, 0.9, 1.0, 1.5),
  survival = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
  growth = c(1.23, 2.13, 1.05, 1.32, 2.4, 2.1, 0.55, 0.34, 0.56, 0.67)
)

surv_formula <- brms::bf(survival ~ dose, family = brms::bernoulli())
growth_formula <- brms::bf(growth ~ dose)

nsec_multi_model_1 <-
  brms::brm(
    surv_formula + growth_formula,
    data = data,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 123
  )
save(
  nsec_multi_model_1,
  file = "tests/testthat/fixtures/nsec_multi_model_1.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/nsec_multi_model_1.rda")

data <- data.frame(
  dose = c(0.001, 0.005, 0.01, 0.05, 0.09, 0.1, 0.5, 0.9, 1.0, 1.5),
  sp_survival = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
  sp_growth = c(1.23, 2.13, 1.05, 1.32, 2.4, 2.1, 0.55, 0.34, 0.56, 0.67)
)

surv_formula <- brms::bf(sp_survival ~ dose, family = brms::bernoulli())
growth_formula <- brms::bf(sp_growth ~ dose)

nsec_multi_model_2 <-
  brms::brm(
    surv_formula + growth_formula,
    data = data,
    chains = 2,
    iter = 2000,
    thin = 2,
    seed = 123
  )
save(
  nsec_multi_model_2,
  file = "tests/testthat/fixtures/nsec_multi_model_2.rda",
  compress = "xz"
)
strip_stanmodel("tests/testthat/fixtures/nsec_multi_model_2.rda")
