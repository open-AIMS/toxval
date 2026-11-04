## Fits the bayesnec model set used by vignettes/articles/case-bayesnec.qmd and
## stores it in vignettes/articles/fits/, so that the article renders without a
## Stan toolchain and without the time it takes to sample.
##
## Run from the package root:
##
##   Rscript data-raw/vignette_fits_bayesnec.R
##
## The example is the ametryn series of `bayesnec::herbicide`, fitted with the
## `decline` set of equations on `log(concentration)` by the call below, with
## `seed = 17`, `adapt_delta = 0.99` and `max_treedepth = 12`. Two further
## choices are made:
##
## - `thin = 4`. Without thinning each equation retains 8000 draws. Stan's
##   thinning keeps every fourth draw of the same chains, so the 2000 kept per
##   equation are a subset of the posterior the full run samples rather than a
##   shorter run, and the stored file holds a quarter of the draws.
## - the Stan backend is left at `brms`'s default, `rstan`, so that the script
##   needs no package outside toxval's Suggests.
##
## The single-equation fit the article uses is `ecx4param`, the equation with
## the highest weight, pulled out of this set with `bayesnec::pull_out()` at
## render time. It is not stored separately.
##
## The stored file is `list(fit, provenance)`, saved with `compress = "xz"`,
## following `bayesnec/vignettes/precompile.R` and `fit_store.R`.

seed <- 17
out_dir <- "vignettes/articles/fits"
out_file <- file.path(out_dir, "bayesnec_ametryn_decline.rds")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# the four chains of each equation are sampled at once; the equations are
# fitted one after another
options(mc.cores = 4)

ametryn <- bayesnec::herbicide[bayesnec::herbicide$herbicide == "ametryn", ]

set.seed(seed)
started <- proc.time()[["elapsed"]]
fit <- bayesnec::bnec(
  fvfm ~ crf(log(concentration), model = "decline"),
  data = ametryn,
  family = brms::Beta(link = "identity"),
  seed = seed,
  thin = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12)
)
fit_seconds <- proc.time()[["elapsed"]] - started

# bayesnec #477: where an equation holds a different number of draws from the
# recorded `sample_size`, toxval and bayesnec's own estimates are computed on
# different draws, and the article's comparison with bayesnec would not hold
draws_per_model <- vapply(
  fit$mod_fits,
  function(m) brms::ndraws(m$fit),
  numeric(1)
)
stopifnot(all(draws_per_model == fit$sample_size))

# The warmup draws `rstan` stores alongside the posterior draws are removed.
# With `iter = 10000`, `warmup = 8000` and `thin = 4` they are 2000 of the 2500
# stored per chain, and so most of the size of the stored file. No posterior
# summary, prediction or sampler diagnostic reads them: brms, posterior and
# bayesnec all extract the post-warmup draws only. `rstan`'s own `save_warmup`
# setting is not reachable through `bnec()` and `brm()` 2.23.0, so the draws
# are dropped after sampling, and the checks below confirm that what is read
# from the fit is unchanged.
drop_warmup <- function(stanfit) {
  sim <- stanfit@sim
  keep <- lapply(seq_along(sim$samples), function(i) {
    seq.int(sim$warmup2[i] + 1, sim$n_save[i])
  })
  sim$samples <- lapply(seq_along(sim$samples), function(i) {
    chain <- sim$samples[[i]]
    kept <- lapply(chain, function(v) v[keep[[i]]])
    attributes(kept) <- attributes(chain)
    attr(kept, "sampler_params") <- lapply(
      attr(chain, "sampler_params"),
      function(v) v[keep[[i]]]
    )
    kept
  })
  sim$n_save <- sim$n_save - sim$warmup2
  sim$warmup2 <- sim$warmup2 * 0
  stanfit@sim <- sim
  stanfit
}

sampling_before <- bayesnec::check_sampling(fit)
grid <- bayesnec::bnec_newdata(fit, resolution = 50)

# The compiled Stan program is also removed from each equation, as in
# data-raw/DATASET.R. Predictions from the stored draws do not need it, and it
# is the largest part of an `rstan` fit; the fit cannot be resampled without
# refitting.
reduced <- lapply(fit$mod_fits, function(m) {
  m$fit$fit <- drop_warmup(m$fit$fit)
  m$fit$fit@stanmodel <- methods::new("stanmodel")
  m
})
for (m in names(reduced)) {
  before <- fit$mod_fits[[m]]$fit
  after <- reduced[[m]]$fit
  stopifnot(
    identical(
      as.matrix(brms::as_draws_matrix(before)),
      as.matrix(brms::as_draws_matrix(after))
    ),
    identical(
      brms::posterior_epred(before, newdata = grid, re_formula = NA),
      brms::posterior_epred(after, newdata = grid, re_formula = NA)
    ),
    identical(brms::nuts_params(before), brms::nuts_params(after))
  )
}
fit$mod_fits <- reduced
stopifnot(identical(sampling_before, bayesnec::check_sampling(fit)))

toxval_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
  error = function(e) NA_character_,
  warning = function(w) NA_character_
)

package_versions <- vapply(
  c("bayesnec", "brms", "rstan", "StanHeaders", "loo"),
  function(p) as.character(utils::packageVersion(p)),
  character(1)
)

provenance <- list(
  seed = seed,
  fitted_on = Sys.time(),
  fit_seconds = fit_seconds,
  r_version = R.version.string,
  packages = package_versions,
  backend = "rstan",
  toxval_commit = toxval_commit,
  data = "bayesnec::herbicide, herbicide == \"ametryn\"",
  formula = "fvfm ~ crf(log(concentration), model = \"decline\")",
  settings = list(
    model = "decline",
    family = "Beta(link = \"identity\")",
    iter = 10000,
    warmup = 8000,
    chains = 4,
    thin = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
  ),
  draws_per_model = draws_per_model,
  models_fitted = names(draws_per_model),
  sampling = sampling_before,
  source = paste(
    "cr_modelling_training module 7 (vignettes/7Example_case_study.qmd),",
    "object m7_am_fit"
  )
)

saveRDS(list(fit = fit, provenance = provenance), out_file, compress = "xz")

message(sprintf(
  "wrote %s (%.2f MB) after %.0f s of fitting",
  out_file,
  file.size(out_file) / 1024^2,
  fit_seconds
))
