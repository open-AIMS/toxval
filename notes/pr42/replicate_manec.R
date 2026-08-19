# Quantify the run-to-run variation in model-averaged estimates.
#
# bayesnec resamples each component's draws in proportion to its stacking
# weight using an unseeded sample(), so repeated identical calls disagree.
# Reproduces the tables in open-AIMS/bayesnec#216 and open-AIMS/toxval#25:
# medians stable to ~0.5%, lower bounds swinging 10% (toxval) and 26%
# (bayesnec).
#
# This is also why the "estimates must be identical before and after" test for
# the toxval#39 relocation is not well defined for a bayesmanecfit.
#
# Run from anywhere:  Rscript notes/pr42/replicate_manec.R

suppressPackageStartupMessages({
  library(toxval)
  library(bayesnec)
})

me <- bayesnec::manec_example
fns <- list(
  "nsec toxval" = list(
    f = get("nsec.bnecfit", envir = asNamespace("toxval")),
    a = list()
  ),
  "nsec bayesnec" = list(
    f = get("nsec.bayesmanecfit", envir = asNamespace("bayesnec")),
    a = list()
  ),
  "ecx  toxval" = list(
    f = get("ecx.bnecfit", envir = asNamespace("toxval")),
    a = list(ecx_val = 10)
  ),
  "ecx  bayesnec" = list(
    f = get("ecx.bayesmanecfit", envir = asNamespace("bayesnec")),
    a = list(ecx_val = 10)
  )
)

n_rep <- 6
set.seed(42)
options(width = 200)

res <- list()
for (nm in names(fns)) {
  m <- matrix(NA_real_, n_rep, 3)
  errs <- 0
  for (i in seq_len(n_rep)) {
    v <- tryCatch(
      as.numeric(do.call(
        fns[[nm]]$f,
        c(list(me), fns[[nm]]$a, list(resolution = 50))
      )),
      error = function(e) {
        errs <<- errs + 1
        rep(NA_real_, 3)
      }
    )
    if (length(v) == 3) m[i, ] <- v
  }
  res[[nm]] <- m
  cat(sprintf(
    "%-15s ok=%d/%d  Q50 %.4f-%.4f  Q2.5 %.4f-%.4f  Q97.5 %.4f-%.4f\n",
    nm, n_rep - errs, n_rep,
    min(m[, 1], na.rm = TRUE), max(m[, 1], na.rm = TRUE),
    min(m[, 2], na.rm = TRUE), max(m[, 2], na.rm = TRUE),
    min(m[, 3], na.rm = TRUE), max(m[, 3], na.rm = TRUE)
  ))
}

cat("\n--- between-package gap vs within-package run-to-run range ---\n")
for (g in c("nsec", "ecx ")) {
  a <- res[[paste0(g, " toxval")]]
  b <- res[[paste0(g, " bayesnec")]]
  for (j in seq_len(3)) {
    gap <- abs(mean(a[, j], na.rm = TRUE) - mean(b[, j], na.rm = TRUE))
    noise <- max(
      diff(range(a[, j], na.rm = TRUE)),
      diff(range(b[, j], na.rm = TRUE))
    )
    cat(sprintf(
      "  %s %-5s gap=%.4f  max within-pkg range=%.4f  -> %s\n",
      trimws(g), c("Q50", "Q2.5", "Q97.5")[j], gap, noise,
      if (gap > noise) "GAP EXCEEDS NOISE" else "within noise"
    ))
  }
}
