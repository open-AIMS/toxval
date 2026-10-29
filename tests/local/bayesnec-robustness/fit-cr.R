# The five distinct model-set structures of CR_workflows, refitted on its own
# simulated datasets with its formula and identity-link families
# (CR_workflows pkg/R/fit_bayesnec.R:17-63, pkg/R/test_types.R).
#
# One dataset per call, named by the first argument, so that the five can run
# in parallel: Rscript tests/local/bayesnec-robustness/fit-cr.R algal_growth
# CR_WORKFLOWS gives the CR_workflows checkout, by default a sibling of toxval.
source("tests/local/bayesnec-robustness/common.R")
id <- commandArgs(TRUE)[1]
specs <- list(
  algal_growth = list(
    f = growth_rate ~ crf(conc, model = "decline"),
    family = stats::Gamma(link = "identity")
  ),
  fish_larval_survival = list(
    f = alive | trials(total) ~ crf(conc, model = "decline"),
    family = stats::binomial(link = "identity")
  ),
  daphnia_reproduction = list(
    f = offspring ~ crf(conc, model = "decline"),
    family = brms::negbinomial(link = "identity")
  ),
  coral_bleaching = list(
    f = prop_symbiont ~ crf(conc, model = "decline"),
    family = brms::Beta(link = "identity")
  ),
  bioluminescence_inhibition = list(
    f = rlu ~ crf(conc, model = "all"),
    family = stats::Gamma(link = "identity")
  )
)
if (!isTRUE(id %in% names(specs))) {
  stop("The first argument must be one of: ", toString(names(specs)), ".")
}
cr_workflows <- Sys.getenv("CR_WORKFLOWS", file.path("..", "CR_workflows"))
e <- new.env()
load(file.path(cr_workflows, "pkg", "data", paste0(id, ".rda")), envir = e)
d <- as.data.frame(get(ls(e)[1], e))
spec <- specs[[id]]
fit_save(
  paste0("cr_", id),
  bnec_q(formula = spec$f, data = d, family = spec$family, seed = 20260831)
)
