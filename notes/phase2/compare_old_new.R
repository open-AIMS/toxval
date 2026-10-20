# Phase 2 check: old ecx() and nsec() against the new pipeline
#
# Run from the package root. Not a test: it prints what matches and why the
# rest differs.
#
# The new pipeline is not wired into ecx() and nsec() until phase 4, so this
# script puts it together by hand, in the order phase 4 will:
#
#   fit -> toxval_predict() -> compute_ecx() / compute_nsec() -> summarise
#
# Both paths call posterior_epred() on the same grid, so they see the same
# draws and can be compared draw by draw.

devtools::load_all()

load("tests/testthat/fixtures/brms_model_1.rda") # y ~ log(x), ungrouped
load("tests/testthat/fixtures/brms_model_2.rda") # y ~ log(x) + z, grouped

prob_vals <- c(0.5, 0.025, 0.975)

# Draw by draw, label each new value against the old one. A draw with no
# crossing is NA in the new code and max(x_vec) in the old (T10); a draw
# whose old value is a crossing the new code did not report is taken to be a
# crossing in the other direction, which the old code accepted as the first
# crossing of any kind (T10). That label is inferred, not checked per draw.
compare_draws <- function(old, new, x_vec) {
  old <- as.numeric(old)
  same <- !is.na(new) & abs(old - new) < 1e-6
  no_crossing <- is.na(new) & old == max(x_vec)
  other_direction <- is.na(new) & old < max(x_vec)

  data.frame(
    draws = length(new),
    match = sum(same),
    no_crossing = sum(no_crossing),
    other_direction = sum(other_direction),
    unexplained = sum(!(same | no_crossing | other_direction))
  )
}

# The phase 4 summary is not written, and its handling of NA waits on #60.
# Here the old max(x_vec) fallback stands in so the quantiles are comparable.
summarise_old_rule <- function(draws, x_vec) {
  draws[is.na(draws)] <- max(x_vec)
  stats::quantile(draws, prob_vals)
}

# 1. Predict once ---------------------------------------------------------------

pred <- toxval_predict(brms_model_1, x_var = "x")
pred

# 2. Compute ---------------------------------------------------------------------

# One row per direction. Each list column holds one value per draw.
ecx_new <- compute_ecx(pred, ecx_val = 10)
ecx_new

nsec_new <- compute_nsec(pred, sig_val = 0.01)
nsec_new

# 3. Compare with the old functions ---------------------------------------------

# The old functions return a single direction, the first crossing of any kind,
# so they are compared with the decreasing row.
ecx_old <- ecx(brms_model_1, x_var = "x", ecx_val = 10, posterior = TRUE)
compare_draws(ecx_old, ecx_new$draws[[1]], pred$x_vec)

rbind(
  old = ecx(brms_model_1, x_var = "x", ecx_val = 10),
  new = summarise_old_rule(ecx_new$draws[[1]], pred$x_vec)
)

ecx_direct_new <- compute_ecx(pred, ecx_val = 0.5, type = "direct")
ecx_direct_old <- ecx(
  brms_model_1,
  x_var = "x",
  ecx_val = 0.5,
  type = "direct",
  posterior = TRUE
)
compare_draws(ecx_direct_old, ecx_direct_new$draws[[1]], pred$x_vec)

# The NSEC reference is the same quantile of the control draws in both.
nsec_old <- nsec(brms_model_1, x_var = "x", posterior = TRUE)
compare_draws(nsec_old, nsec_new$draws[[1]], pred$x_vec)

rbind(
  old = nsec(brms_model_1, x_var = "x"),
  new = summarise_old_rule(nsec_new$draws[[1]], pred$x_vec)
)

# ecnsec is not compared: the old brmsfit method uses the relative formula
# and the new default is absolute (T8), so the two are different quantities.

# 4. A grouped fit --------------------------------------------------------------

pred_grouped <- toxval_predict(brms_model_2, x_var = "x", group_var = "z")
pred_grouped

ecx_grouped_new <- compute_ecx(pred_grouped, ecx_val = 10)
ecx_grouped_new

# The old function returns long draws with one column naming the group.
ecx_grouped_old <- ecx(
  brms_model_2,
  x_var = "x",
  group_var = "z",
  by_group = TRUE,
  posterior = TRUE
)

decreasing <- ecx_grouped_new[ecx_grouped_new$direction == "decreasing", ]
for (i in seq_len(nrow(decreasing))) {
  group <- as.character(decreasing$group[i])
  old <- ecx_grouped_old$ECx[as.character(ecx_grouped_old$z) == group]
  cat("group", group, "\n")
  print(compare_draws(old, decreasing$draws[[i]], pred_grouped$x_vec))
}
