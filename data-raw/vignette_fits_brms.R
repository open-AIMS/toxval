## Stored fits for the article vignettes/articles/case-brms.qmd
##
## Run from the package root:  Rscript data-raw/vignette_fits_brms.R
## Writes vignettes/articles/fits/brms_plate_single.rds and
## vignettes/articles/fits/brms_plate_grouped.rds. Each file is a list with
## `fit`, the brmsfit, and `provenance`: the seed, the time of fitting, the
## sampling time in seconds, the R and package versions, and the toxval commit.
##
## Data
## ----
## data-raw/example_pgl.csv is copied unchanged from the CR modelling training
## repository, https://github.com/open-AIMS/cr_modelling_training, file
## vignettes/example_pgl.csv, added in commit
## c15028702a82c98244f5be687040dee451393b92 and unchanged at
## 33dd784fa9a1508d32f40335950b96254bf61605. The data come from the
## bioluminescent bacterial toxicity assay of Luter et al. (2025, Environmental
## Toxicology, doi:10.1002/tox.70003). The whole concentration series of one
## experiment runs on a single plate, and the plate was repeated in four
## experiments on the same contaminant (`plateRep`, Lum_27C_rep1_qtoxB to
## Lum_27C_rep4_qtoxB). Each plate has four wells at each of 11 concentrations,
## 0.09 to 90, stored as their natural logarithm in `log_x`. `y` is the
## response as a proportion, inside (0, 1). The first, unnamed column is a row
## index and is dropped.
##
## Models
## ------
## The bayesnec `nechorme4` equation, fitted with brm() once with `~ 1` and once
## with `~ plateRep` on every nonlinear parameter. The equation has a hormesis
## term because the response of every plate rises above its value at the lowest
## concentration before it declines. The family is Beta(), with its default
## logit link, because `y` is a proportion inside (0, 1). The formula is written
## out rather than built by bayesnec, so that the article does not depend on
## bayesnec.
##
## The priors are on the logit scale for `top` and `bot`, the
## log-concentration scale for `nec`, and the log scale for the rates `beta`
## and `slope`. Under `~ plateRep` the intercept (the first plate) and the
## differences of the other plates from it have separate priors, the
## differences centred on 0, meaning no difference between plates. A prior
## written without `coef` would apply to the differences as well as the
## intercept.
##
## Sampler settings: 4 chains of 5000 iterations with 2500 of warmup, giving
## 10000 draws per fit, not thinned; init = 0, so that every parameter starts at
## 0 rather than at a random value; save_pars(all = TRUE); seed 700. Warmup
## draws are not saved, and the compiled Stan program is removed from the
## stored object, as data-raw/DATASET.R does for the test fixtures, so a stored
## fit can be predicted from but not updated.

library(brms)

seed <- 700
plates <- read.csv("data-raw/example_pgl.csv")[c("log_x", "plateRep", "y")]
# a factor, so that the plate order and the reference level are fixed by
# their labels rather than by the order of the rows
plates$plateRep <- factor(plates$plateRep)

# The nechorme4 equation of bayesnec: flat at `top` plus a linear rise in
# log_x (the hormesis term, with slope exp(slope)) up to the threshold `nec`,
# then an exponential decay at rate exp(beta) towards the lower asymptote
# `bot`. Under the logit link the whole right-hand side is on the logit scale.
nechorme4 <- y ~ bot +
  ((top + exp(slope) * log_x) - bot) *
    exp(-exp(beta) * (log_x - nec) * step(log_x - nec))

bf_single <- bf(nechorme4, bot + top + beta + nec + slope ~ 1, nl = TRUE)
bf_grouped <- bf(nechorme4, bot + top + beta + nec + slope ~ plateRep, nl = TRUE)

# Centred on the data: for `top`, a response near 0.88 (logit 2) at log_x = 0,
# a concentration of 1, where `top` is defined (not at the control); for
# `bot`, a lower asymptote near 0.12 (logit -2); and for `nec`, a threshold
# inside the observed log_x range of -2.4 to 4.5. The scales are wide relative
# to those centres.
prior_values <- c(
  prior(normal(2, 2), nlpar = "top"),
  prior(normal(-2, 2), nlpar = "bot"),
  prior(normal(1, 2), nlpar = "nec"),
  prior(normal(0, 2), nlpar = "beta"),
  prior(normal(-1, 2), nlpar = "slope")
)
prior_single <- prior_values
# Without `coef`, a prior covers every coefficient of the parameter, so under
# `~ plateRep` the values above are given to the intercept alone and a second
# set, centred on no difference, to the differences between plates.
prior_grouped <- c(
  prior(normal(2, 2), nlpar = "top", coef = "Intercept"),
  prior(normal(-2, 2), nlpar = "bot", coef = "Intercept"),
  prior(normal(1, 2), nlpar = "nec", coef = "Intercept"),
  prior(normal(0, 2), nlpar = "beta", coef = "Intercept"),
  prior(normal(-1, 2), nlpar = "slope", coef = "Intercept"),
  prior(normal(0, 2), nlpar = "top"),
  prior(normal(0, 2), nlpar = "bot"),
  prior(normal(0, 1), nlpar = "nec"),
  prior(normal(0, 1), nlpar = "beta"),
  prior(normal(0, 1), nlpar = "slope")
)

toxval_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  error = function(e) NA_character_
)

fit_and_store <- function(formula, prior, file) {
  start <- Sys.time()
  fit <- brm(
    formula,
    data = plates,
    family = Beta(),
    prior = prior,
    chains = 4,
    cores = 4,
    iter = 5000,
    warmup = 2500,
    init = 0,
    save_pars = save_pars(all = TRUE),
    save_warmup = FALSE,
    seed = seed,
    refresh = 0
  )
  fit_seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  fit$fit@stanmodel <- methods::new("stanmodel")
  provenance <- list(
    seed = seed,
    fitted_on = Sys.time(),
    fit_seconds = fit_seconds,
    r_version = R.version.string,
    packages = vapply(
      c("brms", "rstan", "StanHeaders", "toxval"),
      function(p) as.character(utils::packageVersion(p)),
      character(1)
    ),
    toxval_commit = toxval_commit,
    data_source = paste(
      "open-AIMS/cr_modelling_training vignettes/example_pgl.csv,",
      "c15028702a82c98244f5be687040dee451393b92"
    ),
    model_source = paste(
      "open-AIMS/cr_modelling_training",
      "vignettes/8Factor_covariates_and_groupings.Rmd,",
      "3443691925ffcdc0ebb5c8dba6a3c3ec0043246e"
    )
  )
  saveRDS(list(fit = fit, provenance = provenance), file, compress = "xz")
  message(file, ": ", round(fit_seconds), " s, ", file.size(file), " bytes")
  invisible(fit)
}

dir.create("vignettes/articles/fits", showWarnings = FALSE, recursive = TRUE)
fit_and_store(
  bf_single,
  prior_single,
  "vignettes/articles/fits/brms_plate_single.rds"
)
fit_and_store(
  bf_grouped,
  prior_grouped,
  "vignettes/articles/fits/brms_plate_grouped.rds"
)
