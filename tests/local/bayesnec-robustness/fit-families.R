# Every family and both predictor transforms, one fit each, on nec_data with
# simulated responses for the families that need them.
source("tests/local/bayesnec-robustness/common.R")
set.seed(101)
d <- bayesnec::nec_data
d$y_gamma <- d$y * 10 + 0.01
d$y_gauss <- d$y * 10 - 2 # includes negative values
d$n <- 20L
d$y_binom <- stats::rbinom(nrow(d), d$n, pmin(pmax(d$y, 0.01), 0.99))
d$y_count <- stats::rpois(nrow(d), 40 * d$y)
d$y_nb <- stats::rnbinom(nrow(d), mu = 40 * d$y, size = 5)
d$expo <- sample(c(1, 2, 4), nrow(d), replace = TRUE)
d$y_rate <- stats::rpois(nrow(d), 10 * d$y * d$expo)
saveRDS(d, file.path(fit_dir, "family_data.rds"))
fit_save(
  "fam_beta",
  bnec_q(
    formula = y ~ crf(x, model = "nec4param"),
    data = d,
    family = Beta,
    seed = 1
  )
)
fit_save(
  "fam_gaussian",
  bnec_q(
    formula = y_gauss ~ crf(x, model = "ecx4param"),
    data = d,
    family = gaussian,
    seed = 2
  )
)
fit_save(
  "fam_gamma",
  bnec_q(
    formula = y_gamma ~ crf(x, model = "nec4param"),
    data = d,
    family = Gamma,
    seed = 3
  )
)
fit_save(
  "fam_binomial",
  bnec_q(
    formula = y_binom | trials(n) ~ crf(x, model = "nec3param"),
    data = d,
    family = binomial,
    seed = 4
  )
)
fit_save(
  "fam_beta_binomial",
  bnec_q(
    formula = y_binom | trials(n) ~ crf(x, model = "nec3param"),
    data = d,
    family = beta_binomial,
    seed = 5
  )
)
fit_save(
  "fam_poisson",
  bnec_q(
    formula = y_count ~ crf(x, model = "nec3param"),
    data = d,
    family = poisson,
    seed = 6
  )
)
fit_save(
  "fam_negbinomial",
  bnec_q(
    formula = y_nb ~ crf(x, model = "nec3param"),
    data = d,
    family = negbinomial,
    seed = 7
  )
)
fit_save(
  "fam_poisson_rate",
  bnec_q(
    formula = y_rate | rate(expo) ~ crf(x, model = "nec3param"),
    data = d,
    family = poisson,
    seed = 8
  )
)
fit_save(
  "crf_log",
  bnec_q(
    formula = y ~ crf(log(x), model = "ecx4param"),
    data = d,
    family = Beta,
    seed = 9
  )
)
fit_save(
  "crf_sqrt",
  bnec_q(
    formula = y ~ crf(sqrt(x), model = "nec4param"),
    data = d,
    family = Beta,
    seed = 10
  )
)
