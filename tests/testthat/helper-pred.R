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
