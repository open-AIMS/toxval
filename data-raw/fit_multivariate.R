## Stored fit for the article vignettes/articles/case-brms-multivariate.qmd
##
## The data are from Luter et al., "Molecular responses to 1-methylnaphthalene
## in Acropora millepora larvae", which is in review. Everything this
## script writes goes to vignettes/articles/.
##
## Run from the package root, with toxval installed from this repository:
##   Rscript data-raw/fit_multivariate.R
## The analysis folder is read from the environment variable POLYNOMIALS_DIR,
## defaulting to /mnt/c/Rworking/Polynomials. Nothing in it is modified.
##
## Writes, in vignettes/articles/:
##
## - data/: copies of the five input files below, unchanged.
## - fits/fit_multivariate.rds: a list with `fit`, the brmsfit, and
##   `provenance`: the genes, the seed, the sampler settings, the time of
##   fitting, the sampling time in seconds, the R and package versions, the
##   toxval commit, and a note on the draws. The brmsfit is the six-gene model
##   with its draws replaced by the original fit's draws for the same
##   parameters (use_original_draws() in original_draws.R), so the article
##   reproduces the original fit's estimates exactly.
## - fits/original.rds: a list with `summary`, the posterior summary of the
##   parameters of the original 78-gene fit for the same genes. The article
##   compares the refit with the original without loading the 77 MB original
##   fit.
##
## Sources
## -------
## The model is that of fit_brms_betaBinomial_poly_final.R in the public
## repository open-AIMS/coral-larvae-1-MN-toxicity: one brm() with a
## beta-binomial response per gene, gene | trials(n) ~ poly(log_x, 3,
## raw = TRUE), set_rescor(FALSE), default priors, 4 chains of 2000
## iterations with 1000 of warmup, init = 0 and seed = 100. That script reads
## BatchCorrectedData.csv. The analysis folder holds no file of that name; the
## batch-corrected counts are in BECdata_wSampleID.csv, which is the file
## posterior_estimates.R in the same folder reads, and the check below confirms
## that the counts and trials built from it equal the data stored in the
## original fit, fit3k_bb_DRomicsGenes_poly_iter1000.RData.
##
## Inputs copied to vignettes/articles/data/:
## - BECdata_wSampleID.csv: batch-corrected counts, one row per gene, one
##   column per sample.
## - DromicsFittedGenesFDR0.05.csv: the 78 genes retained by DRomics.
## - Metadata_1-MN.csv: the sample ID and the measured 1-MN concentration.
## - lower_NSECs_filtered_52genes.csv: the NSECs of the direction with the
##   lower NSEC at the 0.8 quantile, for the 52 genes the original analysis
##   retained because the estimate lies below the highest concentration.
## - toxval_nsec_lower_all78genes.csv: the same NSECs for all 78 genes, which
##   give the direction chosen for each.

library(brms)

article_dir <- file.path("vignettes", "articles")
fits_dir <- file.path(article_dir, "fits")
data_dir <- file.path(article_dir, "data")
source_dir <- Sys.getenv("POLYNOMIALS_DIR", "/mnt/c/Rworking/Polynomials")
stopifnot(dir.exists(article_dir), dir.exists(source_dir))
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

inputs <- c(
  "BECdata_wSampleID.csv",
  "DromicsFittedGenesFDR0.05.csv",
  "Metadata_1-MN.csv",
  "lower_NSECs_filtered_52genes.csv",
  "toxval_nsec_lower_all78genes.csv"
)
stopifnot(all(file.copy(
  file.path(source_dir, inputs),
  data_dir,
  overwrite = TRUE,
  copy.date = TRUE
)))

# Three genes of each direction from the 52 the original analysis retained,
# chosen to cover what the article explains rather than at random:
# - 04448 and 32637 have the lowest NSECs with an upper limit inside the
#   tested range, one in each direction;
# - 32637 and 25399 have the smallest (221) and largest (357,533) trials of
#   the 78 genes, so the trials edge case is shown at both extremes;
# - 00288 and 22478 were chosen for a comparison with the original analysis
#   that the article no longer makes; they are kept so that the stored fit
#   need not be refitted;
# - 19283 is a decreasing gene with a large total count (16,045).
genes <- c(
  decreasing = "Amillepora04448",
  decreasing = "Amillepora19283",
  decreasing = "Amillepora00288",
  increasing = "Amillepora32637",
  increasing = "Amillepora22478",
  increasing = "Amillepora25399"
)

counts <- read.csv(file.path(data_dir, "BECdata_wSampleID.csv"))
fitted_genes <- read.csv(file.path(data_dir, "DromicsFittedGenesFDR0.05.csv"))
stopifnot(all(genes %in% fitted_genes$GeneID))

# The trials of each gene are its total count over all 48 samples, as in the
# original script, so each observation is that sample's share of the gene's
# total: a normalising total, not a ceiling on the response.
gene_counts <- counts |>
  dplyr::filter(GeneID %in% genes) |>
  dplyr::mutate(n = rowSums(dplyr::across(dplyr::starts_with("X")))) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("X"),
    names_to = "SampleID",
    values_to = "y"
  )

# The controls have a measured concentration of 0, set to 0.3 ug/L so that it
# can be log-transformed, as in the original script.
dat_long <- read.csv(file.path(data_dir, "Metadata_1-MN.csv")) |>
  dplyr::left_join(
    gene_counts,
    by = "SampleID",
    relationship = "one-to-many"
  ) |>
  dplyr::mutate(
    x = as.numeric(Measured),
    x = ifelse(x == 0, 0.3, x),
    log_x = log(x)
  )
stopifnot(!anyNA(dat_long$y), nrow(dat_long) == 48 * length(genes))

id_cols <- c(
  "SampleID",
  "Tr",
  "Settlement",
  "Nominal",
  "Measured",
  "x",
  "log_x"
)
dat_use <- dplyr::left_join(
  dat_long |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from = GeneID,
      values_from = y,
      names_prefix = "gene_"
    ),
  dat_long |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from = GeneID,
      values_from = n,
      names_prefix = "n_"
    ),
  by = id_cols
)

# The original fit holds the data it was fitted to. The rows built here must
# equal its rows for these genes, or the comparison in the article would pair
# a refit with different data.
load(file.path(source_dir, "fit3k_bb_DRomicsGenes_poly_iter1000.RData"))
check_cols <- c("log_x", paste0("gene_", genes), paste0("n_", genes))
stopifnot(isTRUE(all.equal(
  as.data.frame(fit3k$data[check_cols]),
  as.data.frame(dat_use[check_cols]),
  check.attributes = FALSE
)))

# The responses share no parameter and set_rescor(FALSE) leaves the residuals
# uncorrelated, so each gene's posterior is the same whether it is fitted
# with the other 77 genes or with the other five. Fitting six genes keeps the
# stored fit small. Its own draws would differ from the original's by Monte
# Carlo error, which for an NSEC can reach a third of the estimate, because the
# reference is a tail quantile of the control draws; the draws are therefore
# replaced with the original's below.
formula <- Reduce(
  `+`,
  lapply(genes, function(g) {
    bf(stats::as.formula(paste0(
      "gene_",
      g,
      " | trials(n_",
      g,
      ") ~ poly(log_x, 3, raw = TRUE)"
    )))
  })
) +
  set_rescor(FALSE)

seed <- 100
chains <- 4
iter <- 2000
warmup <- 1000

toxval_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  error = function(e) NA_character_
)

start <- Sys.time()
fit <- brm(
  formula,
  data = dat_use,
  family = "beta_binomial",
  chains = chains,
  cores = chains,
  iter = iter,
  warmup = warmup,
  init = 0,
  seed = seed,
  refresh = 0
)
fit_seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))

# The default priors must be those of the original fit for these genes, so
# that the six-gene model and the original estimate the same posterior.
prior_cols <- c("prior", "class", "coef", "resp")
priors_of <- function(object) {
  p <- as.data.frame(prior_summary(object))[prior_cols]
  p <- p[p$resp %in% gsub("_", "", paste0("gene_", genes)), ]
  p[do.call(order, p), ]
}
stopifnot(isTRUE(all.equal(
  priors_of(fit),
  priors_of(fit3k),
  check.attributes = FALSE
)))

# The original fit's draws replace the six-gene model's, so the article gives
# the original fit's estimates exactly.
source(file.path("data-raw", "original_draws.R"))
fit <- use_original_draws(fit, fit3k)
summary_new <- posterior_summary(fit)
summary_old <- posterior_summary(fit3k)
shared <- intersect(rownames(summary_new), rownames(summary_old))
stopifnot(
  length(shared) == 5 * length(genes),
  identical(summary_new[shared, ], summary_old[shared, ])
)

# As data-raw/DATASET.R does for the test fixtures, the compiled Stan program
# is removed, so the stored fit can be predicted from but not updated.
fit$fit@stanmodel <- methods::new("stanmodel")

provenance <- list(
  genes = genes,
  seed = seed,
  chains = chains,
  iter = iter,
  warmup = warmup,
  fitted_on = Sys.time(),
  fit_seconds = fit_seconds,
  r_version = R.version.string,
  packages = vapply(
    c("brms", "rstan", "StanHeaders", "toxval"),
    function(p) as.character(utils::packageVersion(p)),
    character(1)
  ),
  toxval_commit = toxval_commit,
  draws = paste(
    "post-processed by data-raw/original_draws.R: every parameter's draws,",
    "warmup included, copied from the original 78-gene fit",
    "fit3k_bb_DRomicsGenes_poly_iter1000.RData; lprior and lp__ set to NA"
  ),
  data_source = paste(
    "Luter et al., Molecular responses to 1-methylnaphthalene in Acropora",
    "millepora larvae (in review); analysis folder files",
    paste(inputs[1:3], collapse = ", ")
  ),
  model_source = paste(
    "open-AIMS/coral-larvae-1-MN-toxicity",
    "fit_brms_betaBinomial_poly_final.R"
  ),
  original_fit = list(
    file = "fit3k_bb_DRomicsGenes_poly_iter1000.RData",
    version = lapply(fit3k$version, as.character)
  )
)

saveRDS(
  list(fit = fit, provenance = provenance),
  file.path(fits_dir, "fit_multivariate.rds"),
  compress = "xz"
)

# The posterior summary of the original fit is kept for the six genes only.
responses <- gsub("_", "", paste0("gene_", genes))
original_summary <- posterior_summary(fit3k)
original_summary <- original_summary[
  grepl(paste0(responses, "(_|$)", collapse = "|"), rownames(original_summary)),
]
saveRDS(
  list(summary = original_summary),
  file.path(fits_dir, "original.rds"),
  compress = "xz"
)

cat(
  "Fitted in",
  round(fit_seconds),
  "s; stored fit",
  round(file.size(file.path(fits_dir, "fit_multivariate.rds")) / 2^20, 2),
  "MB; original summary",
  round(file.size(file.path(fits_dir, "original.rds")) / 2^20, 2),
  "MB\n"
)
