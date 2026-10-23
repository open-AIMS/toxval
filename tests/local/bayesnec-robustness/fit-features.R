# Formula features, the remaining families and the other classes, one fit each
# (structures from the bayesnec predev survey, 116ea91b).
source("tests/local/bayesnec-robustness/common.R")
batch <- commandArgs(TRUE)[1]
set.seed(202)
d <- bayesnec::nec_data
d$g <- factor(rep(c("a", "b", "c", "d"), length.out = nrow(d)))
g_eff <- c(a = -0.05, b = 0, c = 0.03, d = 0.05)[as.character(d$g)]
d$y_g <- pmin(pmax(d$y + g_eff, 0.002), 0.998)
d$y_gamma <- d$y * 10 + 0.01
d$y_gauss <- d$y * 10 - 2
d$w <- sample(c(0.5, 1, 2), nrow(d), replace = TRUE)
d$c <- ifelse(d$y_gauss > 6.5, "right", "none")
d$y_bern <- stats::rbinom(nrow(d), 1, pmin(pmax(d$y, 0.02), 0.98))
d$y_zi <- ifelse(
  stats::runif(nrow(d)) < 0.2,
  0L,
  stats::rpois(nrow(d), 30 * d$y)
)
d$y_hu <- ifelse(
  stats::runif(nrow(d)) < 0.1 + 0.6 * (d$x > 1.5),
  0,
  d$y * 10 + 0.05
)
saveRDS(d, file.path(fit_dir, paste0("feature_data_", batch, ".rds")))
if (batch == "groups") {
  fit_save(
    "feat_pgl_beta",
    bnec_q(
      formula = y_g ~ crf(x, model = "nec4param") + pgl(g),
      data = d,
      family = Beta,
      seed = 11
    )
  )
  fit_save(
    "feat_ogl_beta",
    bnec_q(
      formula = y_g ~ crf(x, model = "ecx4param") + ogl(g),
      data = d,
      family = Beta,
      seed = 12
    )
  )
  fit_save(
    "feat_ogl_gaussian",
    bnec_q(
      formula = y_gauss ~ crf(x, model = "ecx4param") + ogl(g),
      data = d,
      family = gaussian,
      seed = 13
    )
  )
  fit_save(
    "feat_topg_gamma",
    bnec_q(
      formula = y_gamma ~ crf(x, model = "nec3param") + (top | g),
      data = d,
      family = Gamma,
      seed = 14
    )
  )
}
if (batch == "terms") {
  fit_save(
    "feat_weights",
    bnec_q(
      formula = y | weights(w) ~ crf(x, model = "nec4param"),
      data = d,
      family = Beta,
      seed = 21
    )
  )
  fit_save(
    "feat_cens",
    bnec_q(
      formula = y_gauss | cens(c) ~ crf(x, model = "ecx4param"),
      data = d,
      family = gaussian,
      seed = 22
    )
  )
  fit_save(
    "feat_disp_x",
    bnec_q(
      formula = y ~ crf(x, model = "nec4param") + disp(~x),
      data = d,
      family = Beta,
      seed = 23
    )
  )
  fit_save(
    "feat_disp_power",
    bnec_q(
      formula = y_gamma ~ crf(x, model = "nec3param") + disp("power"),
      data = d,
      family = Gamma,
      seed = 24
    )
  )
  fit_save(
    "feat_log_response",
    bnec_q(
      formula = log(y_gamma) ~ crf(x, model = "ecx4param"),
      data = d,
      family = gaussian,
      seed = 25
    )
  )
  fit_save(
    "feat_beta_logit",
    bnec_q(
      formula = y ~ crf(x, model = "ecx4param"),
      data = d,
      family = brms::Beta(link = "logit"),
      seed = 26
    )
  )
}
if (batch == "families") {
  fit_save(
    "fam_bernoulli",
    bnec_q(
      formula = y_bern ~ crf(x, model = "nec3param"),
      data = d,
      family = bernoulli,
      seed = 31
    )
  )
  fit_save(
    "fam_zero_inflated_poisson",
    bnec_q(
      formula = y_zi ~ crf(x, model = "nec3param"),
      data = d,
      family = "zero_inflated_poisson",
      seed = 32
    )
  )
  fit_save(
    "fam_hurdle_gamma",
    bnec_q(
      formula = y_hu ~ crf(x, model = "nec3param"),
      data = d,
      family = "hurdle_gamma",
      seed = 33
    )
  )
}
if (batch == "classes") {
  fit_save(
    "class_group",
    suppressWarnings(suppressMessages(do.call(
      bayesnec::bnec_group,
      c(
        list(
          formula = y_g ~ crf(x, model = "nec3param"),
          data = d,
          group_var = "g",
          family = Beta,
          seed = 41
        ),
        quick
      )
    )))
  )
  fit_save(
    "class_hurdle",
    suppressWarnings(suppressMessages(do.call(
      bayesnec::bnec_hurdle,
      c(
        list(
          formula = y_hu ~ crf(x, model = "nec3param"),
          data = d,
          family_growth = Gamma,
          seed = 42
        ),
        quick
      )
    )))
  )
  grp <- readRDS(file.path(fit_dir, "class_group.rds"))
  if (inherits(grp, "bayesnecgroupfit")) {
    fit_save(
      "class_joint",
      suppressWarnings(suppressMessages(do.call(
        bayesnec::bnec_joint,
        c(list(grp, seed = 43), quick)
      )))
    )
  }
}
if (batch == "thin") {
  # thin does not divide iter - warmup, so the stored sample_size (667) can
  # differ from the draws kept (668), and bayesnec rebuilds its index
  fit_save(
    "set_thin3",
    suppressWarnings(suppressMessages(bayesnec::bnec(
      y ~ crf(x, model = c("nec4param", "ecx4param")),
      data = d,
      family = Beta,
      chains = 4,
      iter = 1000,
      warmup = 500,
      thin = 3,
      refresh = 0,
      silent = 2,
      seed = 51
    )))
  )
}
