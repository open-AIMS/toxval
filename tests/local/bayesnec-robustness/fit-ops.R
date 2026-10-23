# Model sets made by combining or editing fits: `+`, amend() and pull_out().
source("tests/local/bayesnec-robustness/common.R")
d <- readRDS(file.path(fit_dir, "family_data.rds"))
beta_nec4 <- readRDS(file.path(fit_dir, "fam_beta.rds")) # 1000 draws
# a second equation on the same data, with more draws (1400) than fam_beta
more_path <- file.path(fit_dir, "single_ecx4param_1400.rds")
if (!file.exists(more_path)) {
  more <- suppressWarnings(suppressMessages(bayesnec::bnec(
    y ~ crf(x, model = "ecx4param"),
    data = d,
    family = Beta,
    chains = 2,
    iter = 1200,
    warmup = 500,
    refresh = 0,
    silent = 2,
    seed = 61
  )))
}
if (!file.exists(more_path)) {
  saveRDS(more, more_path)
}
# bayesnec refuses `+` on fits with different numbers of draws ("Each object
# in the list must have the same dimensions"), seen on the first run of this
# script, so the combination is made from two fits with 1000 draws each
same_path <- file.path(fit_dir, "single_ecx4param_1000.rds")
if (!file.exists(same_path)) {
  saveRDS(
    bnec_q(
      formula = y ~ crf(x, model = "ecx4param"),
      data = d,
      family = Beta,
      seed = 66
    ),
    same_path
  )
}
same <- readRDS(same_path)
set.seed(62)
fit_save("ops_plus", suppressMessages(beta_nec4 + same))
all_set <- file.path(fit_dir, "set_all_beta.rds")
if (file.exists(all_set)) {
  all_set <- readRDS(all_set)
  set.seed(64)
  keep <- names(all_set$mod_fits)[1:3]
  fit_save(
    "ops_amend_drop",
    suppressMessages(bayesnec::amend(
      all_set,
      drop = setdiff(names(all_set$mod_fits), keep)
    ))
  )
  set.seed(65)
  fit_save(
    "ops_pull_out_two",
    suppressMessages(bayesnec::pull_out(
      all_set,
      model = c("nec4param", "ecx4param")
    ))
  )
} else {
  cat("set_all_beta not yet fitted; amend and pull_out skipped\n")
}
