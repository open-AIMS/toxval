# The hand-built curves below are chosen so each crossing is known exactly.
# `prob_vals = c(0.5, 0, 1)` reports the median and the extremes of two
# realisations, so every expected value can be read off the curves.
all_quantiles <- c(0.5, 0, 1)

# columns --------------------------------------------------------------------

test_that("summarise_metric returns a toxval tibble with the ecx columns", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(compute_ecx(pred), pred, "ecx")

  expect_s3_class(result, "toxval")
  expect_named(
    result,
    c(
      "metric",
      "direction",
      "estimate",
      "conf.low",
      "conf.high",
      "proportion",
      "type",
      "control",
      "reference",
      "asymptote"
    )
  )
  expect_identical(result$metric, c("ecx", "ecx"))
  expect_identical(result$direction, c("decreasing", "increasing"))
})

test_that("summarise_metric returns the nsec columns, with the ecnsec summary", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(compute_nsec(pred), pred, "nsec")

  expect_named(
    result,
    c(
      "metric",
      "direction",
      "estimate",
      "conf.low",
      "conf.high",
      "sig_val",
      "type",
      "anchor",
      "control",
      "reference",
      "asymptote",
      "ecnsec",
      "ecnsec.low",
      "ecnsec.high"
    )
  )
  expect_identical(result$metric, c("nsec", "nsec"))
})

test_that("summarise_metric puts the key columns after direction", {
  x_vec <- 0:10
  curve <- rbind(10 - x_vec, 10 - 2 * x_vec)

  pred <- compute_pred(
    list(curve, curve, curve, curve),
    x_vec,
    keys = data.frame(
      group = factor(c("A", "A", "B", "B")),
      response = factor(c("growth", "survival", "growth", "survival"))
    ),
    group_var = "site",
    multi_var = c("growth", "survival")
  )

  result <- summarise_metric(compute_ecx(pred), pred, "ecx")

  expect_identical(
    names(result)[1:4],
    c("metric", "direction", "group", "response")
  )
  expect_identical(result$group, factor(rep(c("A", "B"), each = 4)))
  expect_identical(
    result$response,
    factor(rep(rep(c("growth", "survival"), each = 2), times = 2))
  )
})

# the summary ----------------------------------------------------------------

test_that("summarise_metric takes quantiles of the crossings", {
  x_vec <- 0:10
  # EC10 references 9 for both; crossings at 1 and 0.5
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(
    compute_ecx(pred),
    pred,
    "ecx",
    prob_vals = all_quantiles
  )

  expect_equal(
    unlist(result[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 0.75, conf.low = 0.5, conf.high = 1)
  )
})

test_that("summarise_metric reports the central control and reference", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 8 - x_vec)), x_vec)

  result <- summarise_metric(
    compute_ecx(pred),
    pred,
    "ecx",
    prob_vals = all_quantiles
  )

  expect_equal(result$control, c(9, 9))
  expect_equal(result$reference[1], median(c(9, 7.2)))
})

test_that("summarise_metric returns NA where no realisation crosses", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  expect_no_warning(
    result <- summarise_metric(compute_ecx(pred), pred, "ecx")
  )

  expect_identical(
    unlist(result[2, c("estimate", "conf.low", "conf.high")]),
    c(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  )
})

test_that("summarise_metric returns NA where no realisation crosses, including one past the reference at the control", {
  x_vec <- 0:10
  # The increasing NSEC reference is the 99% quantile of the controls, 9.98.
  # The first control, 10, already lies above it, but that realisation falls,
  # away from the increasing effect, and below the reference, so it is
  # censored (T21, amended on #74). The other two never rise and are censored.
  # None crosses (T10).
  pred <- compute_pred(list(rbind(10 - x_vec, 9 - x_vec, 8 - x_vec)), x_vec)

  expect_no_warning(
    result <- summarise_metric(compute_nsec(pred), pred, "nsec", draws = TRUE)
  )

  expect_identical(result$draws[[2]]$value.draw, c(Inf, Inf, Inf))
  expect_identical(
    unlist(result[2, c("estimate", "conf.low", "conf.high")]),
    c(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  )
})

test_that("summarise_metric places a realisation already past the reference at x_control (T21)", {
  x_vec <- 1:10
  # `direct` reference 5: the first crosses at 5, the second starts at 3.9
  pred <- compute_pred(list(rbind(10 - x_vec, 4 - x_vec / 10)), x_vec)

  result <- summarise_metric(
    compute_ecx(pred, reference = 5, type = "direct"),
    pred,
    "ecx",
    prob_vals = all_quantiles
  )

  expect_equal(
    unlist(result[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 3, conf.low = 1, conf.high = 5)
  )
})

# Ten realisations that start at 4, past a decreasing NSEC reference of 5 at the
# control, and 29 that start at 6 and cross 5 at 2. With `sig_val = 0.25` the
# reference, the 0.25 quantile of the 39 controls, is 0.5 * 4 + 0.5 * 6 = 5.
# Decreasing, towards the effect is falling and the no-effect side is above 5.
# The mirror image about 5 gives the increasing cases, against the 0.75
# quantile, which is also 5. Each row is named for its case; the first two
# cross in the direction sought, at 2.5 and 3.5.
past_at_control <- function(x_vec) {
  rbind(
    crosses = c(4, 6, 6, rep(4, 8)),
    dips_then_crosses = c(4, 3.5, 6, 6, rep(4, 7)),
    towards = 4 - x_vec / 10,
    flat_then_towards = c(4, 4, 4, rep(3, 8)),
    round_off_away_then_towards = c(4, 4 + 1e-12, rep(3, 9)),
    hormetic = c(4, 4.5, 4.8, 4.5, 3, 2, 1, 0, 0, 0, 0),
    away_never_leaves = c(4, rep(4.5, 10)),
    towards_then_leaves = c(4, rep(3, 8), 6, 6),
    round_off_away_then_towards_leaves = c(4, 4 + 1e-12, rep(3, 7), 6, 6),
    away_leaves = c(4, rep(6, 10)),
    matrix(6 - x_vec / 2, nrow = 29, ncol = length(x_vec), byrow = TRUE)
  )
}
past_at_control_expected <- c(2.5, 3.5, rep(0, 7), Inf, rep(2, 29))

test_that("summarise_metric censors a decreasing NSEC realisation past the reference that changes away and leaves (T21, #74)", {
  x_vec <- 0:10
  pred <- compute_pred(list(past_at_control(x_vec)), x_vec)

  expect_warning(
    result <- summarise_metric(
      compute_nsec(pred, sig_val = 0.25),
      pred,
      "nsec",
      prob_vals = all_quantiles,
      draws = TRUE
    ),
    regexp = "decreasing: 2.6% censored"
  )

  expect_equal(result$reference[1], 5)
  expect_equal(result$draws[[1]]$value.draw, past_at_control_expected)
})

test_that("summarise_metric censors an increasing NSEC realisation past the reference that changes away and leaves (T21, #74)", {
  x_vec <- 0:10
  pred <- compute_pred(list(10 - past_at_control(x_vec)), x_vec)

  expect_warning(
    result <- summarise_metric(
      compute_nsec(pred, sig_val = 0.25),
      pred,
      "nsec",
      prob_vals = all_quantiles,
      draws = TRUE
    ),
    regexp = "increasing: 2.6% censored"
  )

  expect_equal(result$reference[2], 5)
  expect_equal(result$draws[[2]]$value.draw, past_at_control_expected)
})

test_that("summarise_metric keeps T21 for an ECx of every type on the same curves (T21, #74)", {
  x_vec <- 0:10
  curves <- past_at_control(x_vec)
  n <- nrow(curves)
  asymptote <- list(list(decreasing = rep(0, n), increasing = rep(20, n)))
  pred <- compute_pred(
    list(curves),
    x_vec,
    asymptote = asymptote
  )
  mirror <- compute_pred(
    list(10 - curves),
    x_vec,
    asymptote = asymptote
  )

  # Under `direct` every realisation past 5 at the control without a crossing
  # takes x_control, including the one the NSEC censors.
  for (case in list(list(pred, 1), list(mirror, 2))) {
    result <- suppressWarnings(summarise_metric(
      compute_ecx(case[[1]], reference = 5, type = "direct"),
      case[[1]],
      "ecx",
      draws = TRUE
    ))
    expect_equal(
      result$draws[[case[[2]]]]$value.draw,
      c(2.5, 3.5, rep(0, 8), rep(2, 29))
    )
  }

  # Under the other types no control is past its own reference, so every
  # realisation takes its crossing or is censored.
  for (type in c("absolute", "relative", "range")) {
    rows <- compute_ecx(pred, type = type)
    result <- suppressWarnings(
      summarise_metric(rows, pred, "ecx", draws = TRUE)
    )
    for (i in 1:2) {
      crossings <- rows$draws[[i]]
      crossings[is.na(crossings)] <- Inf
      expect_identical(result$draws[[i]]$value.draw, crossings)
    }
  }
})

test_that("summarise_metric reports a censored quantile at the highest concentration (T21)", {
  x_vec <- 0:10
  # the second realisation reaches its EC10 reference of 9 only at x = 20
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - x_vec / 20)), x_vec)

  expect_warning(
    result <- summarise_metric(
      compute_ecx(pred),
      pred,
      "ecx",
      prob_vals = all_quantiles
    ),
    regexp = "decreasing: 50.0% censored"
  )

  expect_equal(
    unlist(result[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 10, conf.low = 1, conf.high = 10)
  )
})

test_that("summarise_metric does not warn when no reported quantile is censored", {
  x_vec <- 0:10
  # 1 of 80 realisations is censored, beyond the reported 2.5% to 97.5%
  slopes <- seq(0.2, 2, length.out = 79)
  curves <- rbind(
    t(vapply(slopes, function(a) 10 - a * x_vec, numeric(11))),
    10 - x_vec / 20
  )
  pred <- compute_pred(list(curves), x_vec)
  rows <- compute_ecx(pred)

  expect_no_warning(
    result <- summarise_metric(rows[1, ], pred, "ecx")
  )
  expect_lt(result$conf.high, 10)
})

test_that("summarise_metric excludes a realisation with no reference and warns (T21, T25)", {
  x_vec <- 0:10
  # the second control is negative, so its absolute reference is NA (T18)
  pred <- compute_pred(list(rbind(10 - x_vec, rep(-1, 11))), x_vec)

  expect_warning(
    result <- summarise_metric(
      compute_ecx(pred),
      pred,
      "ecx",
      prob_vals = all_quantiles
    ),
    regexp = "decreasing: 50.0% excluded"
  )

  expect_equal(
    unlist(result[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 1, conf.low = 1, conf.high = 1)
  )
})

test_that("summarise_metric names the group in the warning", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(10 - x_vec, 10 - x_vec), rbind(10 - x_vec, 10 - x_vec / 20)),
    x_vec,
    keys = data.frame(group = factor(c("A", "B"))),
    group_var = "site"
  )

  expect_warning(
    summarise_metric(
      compute_ecx(pred),
      pred,
      "ecx",
      prob_vals = all_quantiles
    ),
    regexp = "group B, decreasing: 50.0% censored"
  )
})

test_that("summarise_metric summarises control over the realisations not excluded", {
  x_vec <- 0:10
  # under `relative` the first realisation has no increasing asymptote and the
  # second no decreasing one, so each row excludes a different realisation
  pred <- compute_pred(
    list(rbind(10 - x_vec, 8 + x_vec)),
    x_vec,
    asymptote = list(list(decreasing = c(0, NA), increasing = c(NA, 20)))
  )

  result <- suppressWarnings(summarise_metric(
    compute_ecx(pred, type = "relative"),
    pred,
    "ecx"
  ))

  expect_identical(result$direction, c("decreasing", "increasing"))
  expect_equal(result$control, c(10, 8))
})

test_that("summarise_metric gives NA control where every realisation is excluded", {
  x_vec <- 0:10
  # under `relative` no realisation has an increasing asymptote
  pred <- compute_pred(
    list(rbind(10 - x_vec, 10 - 2 * x_vec)),
    x_vec,
    asymptote = list(list(
      decreasing = c(0, 0),
      increasing = c(NA_real_, NA_real_)
    ))
  )

  result <- suppressWarnings(summarise_metric(
    compute_ecx(pred, type = "relative"),
    pred,
    "ecx"
  ))

  expect_identical(result$control[2], NA_real_)
  expect_identical(result$reference[2], NA_real_)
  expect_identical(result$estimate[2], NA_real_)
})

test_that("summarise_metric censors a realisation with no span under range", {
  x_vec <- 0:10
  # the second realisation is flat, so it never falls below its control (T38)
  pred <- compute_pred(list(rbind(10 - x_vec, rep(10, 11))), x_vec)

  warnings <- character()
  result <- withCallingHandlers(
    summarise_metric(
      compute_ecx(pred, type = "range"),
      pred,
      "ecx",
      draws = TRUE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(result$draws[[1]]$value.draw[[2]], Inf)
  expect_equal(result$reference[[1]], 9)
  expect_match(warnings, "decreasing: 50.0% censored", all = FALSE)
  expect_no_match(warnings, "% excluded")
})

test_that("summarise_metric keeps excluding a span of zero outside range", {
  x_vec <- 0:10
  # under `absolute` a control of 0 has no decreasing span
  pred <- compute_pred(list(rbind(10 - x_vec, 0 - x_vec)), x_vec)

  result <- suppressWarnings(summarise_metric(
    compute_ecx(pred),
    pred,
    "ecx",
    draws = TRUE
  ))

  expect_identical(result$draws[[1]]$value.draw[[2]], NA_real_)
})

test_that("summarise_metric sets control to NA under direct", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(
    compute_ecx(pred, reference = 5, type = "direct"),
    pred,
    "ecx"
  )

  expect_identical(result$control, c(NA_real_, NA_real_))
  expect_identical(result$reference, c(5, 5))
})

# xform ----------------------------------------------------------------------

test_that("summarise_metric applies xform before summarising", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(
    compute_ecx(pred),
    pred,
    "ecx",
    prob_vals = all_quantiles,
    xform = function(x) x^2
  )

  # the median of 1 and 0.25, not the square of the median of 1 and 0.5
  expect_equal(result$estimate[1], 0.625)
})

test_that("summarise_metric reports a censored quantile as xform of the highest concentration", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - x_vec / 20)), x_vec)

  result <- suppressWarnings(summarise_metric(
    compute_ecx(pred),
    pred,
    "ecx",
    prob_vals = all_quantiles,
    xform = function(x) x * 10
  ))

  expect_equal(result$conf.high[1], 100)
})

# draws ----------------------------------------------------------------------

test_that("summarise_metric adds a draws list column when draws = TRUE", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - x_vec / 20)), x_vec)

  result <- suppressWarnings(
    summarise_metric(compute_ecx(pred), pred, "ecx", draws = TRUE)
  )

  expect_identical(names(result)[ncol(result)], "draws")
  expect_equal(
    result$draws[[1]],
    dplyr::tibble(.draw = 1:2, value.draw = c(1, Inf))
  )
})

test_that("summarise_metric has no draws column by default", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(compute_ecx(pred), pred, "ecx")

  expect_false("draws" %in% names(result))
})

test_that("summarise_metric pairs ecnsec with the nsec realisations in draws", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)
  rows <- compute_nsec(pred)

  result <- summarise_metric(rows, pred, "nsec", draws = TRUE)

  expect_named(result$draws[[1]], c(".draw", "value.draw", "ecnsec.draw"))
  expect_identical(result$draws[[1]]$ecnsec.draw, rows$ecnsec[[1]])
})

test_that("summarise_metric draws unnest alongside the summary columns", {
  skip_if_not_installed("tidyr")
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- summarise_metric(compute_nsec(pred), pred, "nsec", draws = TRUE)
  long <- tidyr::unnest(result, draws)

  expect_equal(nrow(long), 4L)
  expect_true(all(
    c("ecnsec", ".draw", "value.draw", "ecnsec.draw") %in% names(long)
  ))
})

# argument checks ------------------------------------------------------------

test_that("summarise_metric errors on prob_vals out of order", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  expect_error(
    summarise_metric(
      compute_ecx(pred),
      pred,
      "ecx",
      prob_vals = c(0.025, 0.5, 0.975)
    ),
    regexp = "`prob_vals` must be the central, lower and upper quantiles"
  )
})

test_that("summarise_metric errors when xform is not a function", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  expect_error(
    summarise_metric(compute_ecx(pred), pred, "ecx", xform = "log"),
    regexp = "`xform` must be a function"
  )
})

# fitted models --------------------------------------------------------------

test_that("summarise_metric matches today's decreasing EC10 on brms_model_1, less the excluded draws", {
  pred <- toxval_predict(brms_model_1, x_var = "x", resolution = 500)

  # the increasing direction is censored, and 8 of 1000 controls are negative
  result <- suppressWarnings(
    summarise_metric(compute_ecx(pred), pred, "ecx")
  )

  # `ecx(brms_model_1, x_var = "x", resolution = 500)` at a3f682f gives 0.8322880
  # (0.8195822 to 1.05). The per-draw values agree; the difference is the 8
  # draws with a negative control, whose reference is NA and which are excluded
  # (T18, T21) rather than given a crossing of a reference above the control.
  expect_equal(
    unlist(result[1, c("estimate", "conf.low", "conf.high")]),
    c(estimate = 0.832247, conf.low = 0.8197345, conf.high = 1.05),
    tolerance = 1e-6
  )
})

test_that("summarise_metric reports an absolute reference of 0.9 times the control on brms_model_1 (T34)", {
  pred <- toxval_predict(brms_model_1, x_var = "x", resolution = 500)

  # 8 of 1000 controls are negative and excluded from the row
  result <- suppressWarnings(
    summarise_metric(compute_ecx(pred), pred, "ecx")
  )

  expect_equal(result$reference[1], 0.9 * result$control[1])
})

test_that("summarise_metric warns for the censored increasing EC10 on brms_model_1", {
  pred <- toxval_predict(brms_model_1, x_var = "x", resolution = 500)

  expect_warning(
    summarise_metric(compute_ecx(pred), pred, "ecx"),
    regexp = "increasing: [0-9.]+% censored"
  )
})

test_that("summarise_metric summarises a drc NSEC from bootstrap realisations", {
  pred <- toxval_predict(
    nsec_drc_1,
    resolution = 200,
    n_boot = 200,
    seed = 42
  )

  result <- summarise_metric(compute_nsec(pred), pred, "nsec")

  expect_s3_class(result, "toxval")
  expect_true(is.finite(result$estimate[1]))
  expect_true(result$conf.low[1] <= result$estimate[1])
  expect_true(result$estimate[1] <= result$conf.high[1])
})
