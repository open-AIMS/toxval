## Replace the draws of the six-gene refit with those of the original 78-gene
## fit, so that the stored fit reproduces the original analysis's estimates
## exactly rather than to Monte Carlo error.
##
## The responses of the original model share no parameter and set_rescor(FALSE)
## leaves the residuals uncorrelated, so the original draws of one gene's
## parameters are a sample from that gene's posterior in the six-gene model as
## well. The two fits have the same chains, iterations and warmup, so the draws
## are copied chain by chain and iteration by iteration, warmup included.
##
## Three quantities need care:
## - `Intercept_<gene>`: the intercept of the centred design, which the newer
##   brms stores and brms 2.20.4 did not. It is computed from the original
##   draws as b_Intercept + sum(colMeans(X) * b), with X the polynomial design
##   matrix; the identity is checked on the refit's own draws first.
## - `lprior` and `lp__`: in the original they are summed over 78 genes and do
##   not apply to the six-gene model. They are set to NA. Nothing in the
##   article reads them.
## - the sampler diagnostics (divergences, tree depth, step size) and run
##   times: those of the original run are copied, since they describe the
##   chains the draws came from.

use_original_draws <- function(fit, original) {
  sim <- fit$fit@sim
  osim <- original$fit@sim
  stopifnot(
    sim$chains == osim$chains,
    sim$iter == osim$iter,
    sim$warmup == osim$warmup,
    sim$thin == osim$thin
  )
  pars <- names(sim$samples[[1]])
  copied <- setdiff(pars, c(grep("^Intercept_", pars, value = TRUE), "lprior", "lp__"))
  stopifnot(all(copied %in% names(osim$samples[[1]])))

  # colMeans of the design matrix of each response, from the stored data
  x <- fit$data$log_x
  x_means <- colMeans(cbind(x, x^2, x^3))
  centred <- function(samples, gene) {
    b <- vapply(
      1:3,
      function(k) samples[[paste0("b_", gene, "_polylog_x3rawEQTRUE", k)]],
      numeric(length(samples[[1]]))
    )
    samples[[paste0("b_", gene, "_Intercept")]] + drop(b %*% x_means)
  }
  intercepts <- grep("^Intercept_", pars, value = TRUE)
  genes <- sub("^Intercept_", "", intercepts)
  for (g in genes) {
    stopifnot(isTRUE(all.equal(
      centred(sim$samples[[1]], g),
      sim$samples[[1]][[paste0("Intercept_", g)]]
    )))
  }

  for (ch in seq_len(sim$chains)) {
    s <- sim$samples[[ch]]
    o <- osim$samples[[ch]]
    for (p in copied) s[[p]] <- o[[p]]
    for (g in genes) s[[paste0("Intercept_", g)]] <- centred(s, g)
    s[["lprior"]] <- NA_real_ * s[["lprior"]]
    s[["lp__"]] <- NA_real_ * s[["lp__"]]
    keep_post <- seq.int(sim$warmup2[ch] + 1, sim$n_save[ch])
    attr(s, "sampler_params") <- attr(o, "sampler_params")
    attr(s, "elapsed_time") <- attr(o, "elapsed_time")
    attr(s, "adaptation_info") <- attr(o, "adaptation_info")
    attr(s, "mean_pars") <- vapply(
      s[setdiff(pars, "lp__")],
      function(v) mean(v[keep_post]),
      numeric(1)
    )
    attr(s, "mean_lp__") <- NA_real_
    sim$samples[[ch]] <- s
  }
  fit$fit@sim <- sim
  fit
}
