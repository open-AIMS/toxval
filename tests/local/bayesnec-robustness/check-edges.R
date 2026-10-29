# The checks on the stored fits that compare-fits.R does not make: a refusal
# on an object no fit script saves, a range reaching zero on a log predictor,
# the draw index of a thinned model set (bayesnec #477), and a rebuilt index
# under a second generator. Run after the fit scripts.
source("tests/local/bayesnec-robustness/common.R")
suppressMessages(pkgload::load_all(".", quiet = TRUE))
read_fit <- function(name) readRDS(file.path(fit_dir, paste0(name, ".rds")))
outcome <- function(expr) {
  tryCatch(
    {
      expr
      "no error"
    },
    error = conditionMessage
  )
}

# One level of a joint refit inherits bayesnecfit, so it dispatches to the
# bayesnecfit method, which must refuse it
joint <- read_fit("class_joint")
level <- bayesnec:::joint_level_fit(joint, joint$levels[1])
cat("joint level", class(level)[1], ":", outcome(toxval_predict(level)), "\n")

# Under crf(log(x)) the grid is on x, so a range reaching 0 puts log(0) in the
# curve; ecx4param has a finite limit there, so no error is expected
logx <- read_fit("crf_log")
cat(
  "crf(log(x)), x_range = c(0, 3):",
  outcome(toxval_predict(logx, resolution = 50, x_range = c(0, 3))),
  "\n"
)

# The thin = 3 set holds 668 draws per equation and records a sample_size of
# 667, so posterior_epred() rebuilds the index while the stored summaries and
# bayesnec::ecx() keep the stored one (bayesnec #477)
thin <- read_fit("set_thin3")
model_set <- names(thin$mod_fits)
n <- vapply(thin$mod_fits, function(m) brms::ndraws(m$fit), numeric(1))
stored <- thin$w_draw_index
rebuilt <- bayesnec:::weighted_draw_index(
  model_set,
  min(n),
  thin$mod_stats,
  thin$w_draw_seed
)
cat("sample_size", thin$sample_size, "| draws per model", n, "\n")
cat(
  "stored index sizes",
  lengths(stored),
  "| rebuilt",
  lengths(rebuilt),
  "| rows in both",
  mapply(function(a, b) length(intersect(a, b)), stored, rebuilt),
  "\n"
)
plot_data <- thin$w_pred_vals$data
grid <- bayesnec::bnec_newdata(thin, resolution = nrow(plot_data))
per_model <- lapply(thin$mod_fits, function(m) {
  brms::posterior_epred(m$fit, newdata = grid, re_formula = NA)
})
take <- function(index) {
  do.call(rbind, lapply(model_set, function(m) per_model[[m]][index[[m]], ]))
}
summarise_draws <- function(draws) {
  t(apply(draws, 2, bayesnec:::estimates_summary))
}
epred <- brms::posterior_epred(thin, newdata = grid, re_formula = NA)
cat(
  "posterior_epred() takes the rebuilt index:",
  identical(unname(epred), unname(take(rebuilt))),
  "| the stored index:",
  identical(unname(epred), unname(take(stored))),
  "\n"
)
cat(
  "plot data against the stored index, max abs difference",
  max(abs(as.matrix(plot_data[, -1]) - summarise_draws(take(stored)))),
  "| against posterior_epred()",
  max(abs(as.matrix(plot_data[, -1]) - summarise_draws(epred))),
  "\n"
)
thin_ecx <- suppressWarnings(suppressMessages(
  bayesnec::ecx(thin, posterior = TRUE)
))
cat("bayesnec::ecx() draws", length(thin_ecx), "\n")
pred <- toxval_predict(thin, resolution = 50)
cat(
  "toxval_predict() realisations",
  pred$meta$n_realisation,
  "| identical to bayesnec's posterior_epred():",
  identical(
    unname(pred$curves[[1]]),
    unname(brms::posterior_epred(
      thin,
      newdata = bayesnec::bnec_newdata(thin, resolution = 50),
      re_formula = NA
    ))
  ),
  "\n"
)

# weighted_draw_index() fixes the seed and the sample kind but not the
# generator, so a rebuilt index depends on the session's RNGkind(). Run last,
# because it changes the generator
default_kind <- toxval_predict(thin, resolution = 50)
RNGkind("L'Ecuyer-CMRG")
set.seed(1)
lecuyer <- toxval_predict(thin, resolution = 50)
cat(
  "L'Ecuyer-CMRG: identical to bayesnec:",
  identical(
    unname(lecuyer$curves[[1]]),
    unname(brms::posterior_epred(
      thin,
      newdata = bayesnec::bnec_newdata(thin, resolution = 50),
      re_formula = NA
    ))
  ),
  "| identical to the default generator:",
  identical(lecuyer$curves, default_kind$curves),
  "| generator after the call:",
  RNGkind()[1],
  "\n"
)
