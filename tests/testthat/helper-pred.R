# A toxval_pred of hand-built curves. Each element of `curves` is a matrix with
# one realisation per row, evaluated over `x_vec`.
compute_pred <- function(
  curves,
  x_vec,
  keys = data.frame(row.names = 1L),
  group_var = NULL,
  multi_var = NULL,
  family = "gaussian",
  source_class = "brmsfit",
  x_control = min(x_vec),
  asymptote = NULL
) {
  new_toxval_pred(
    curves = curves,
    keys = keys,
    x_vec = x_vec,
    asymptote = asymptote,
    meta = list(
      source_class = source_class,
      x_var = "x",
      group_var = group_var,
      multi_var = multi_var,
      resolution = length(x_vec),
      x_range = range(x_vec),
      x_control = x_control,
      family = family,
      realisation = "draws",
      n_realisation = nrow(curves[[1]]),
      seed = NULL
    )
  )
}

# Eleven linear curves from 10 that cross 5 at 1 to 8, and three beyond 10.
# Keep the count odd: with an even count the type 1 median of the negated
# realisations is not the negated median.
censored_ecx_curves <- function(x_vec) {
  10 - outer(5 / c(1:8, 12.5, 50 / 3, 20), x_vec)
}

# Eleven linear curves from controls of 10 to 11, whose 0.01 quantile, 10.01,
# is the decreasing NSEC reference. The first control is below it; the next
# seven cross it at 1 to 7 and the last three beyond 10.
censored_nsec_curves <- function(x_vec) {
  control <- seq(10, 11, by = 0.1)
  slope <- c(1, (control[-1] - 10.01) / c(1:7, 12.5, 50 / 3, 20))
  control - outer(slope, x_vec)
}
