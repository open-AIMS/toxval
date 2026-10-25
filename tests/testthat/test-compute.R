# compute_ecx ------------------------------------------------------------------

test_that("compute_ecx returns one row per direction for an ungrouped fit", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- toxval:::compute_ecx(pred)

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "direction",
      "proportion",
      "type",
      "control",
      "reference",
      "asymptote",
      "no_span",
      "draws"
    )
  )
  expect_identical(result$direction, c("decreasing", "increasing"))
  expect_identical(result$proportion, c(0.10, 0.10))
  expect_identical(result$type, c("absolute", "absolute"))
})

test_that("compute_ecx builds the absolute reference from each realisation's control", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 8 - x_vec)), x_vec)

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  expect_equal(result$control[[1]], c(10, 8))
  expect_equal(result$reference[[1]], c(9, 7.2))
  expect_equal(result$draws[[1]], c(1, 0.8))
})

test_that("compute_ecx returns NA for a direction with no crossing", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - 2 * x_vec)), x_vec)

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  expect_equal(result$draws[[1]], c(1, 0.5))
  expect_identical(result$draws[[2]], c(NA_real_, NA_real_))
})

test_that("compute_ecx takes the increasing absolute reference towards 1 for a proportion", {
  x_vec <- 0:10
  y <- 0.1 + 0.08 * x_vec
  pred <- compute_pred(list(rbind(y, y)), x_vec, family = "beta")

  result <- toxval:::compute_ecx(pred, proportion = 0.50)

  expect_equal(result$reference[[2]], c(0.55, 0.55))
  expect_equal(result$draws[[2]], c(5.625, 5.625))
  expect_identical(result$draws[[1]], c(NA_real_, NA_real_))
})

test_that("compute_ecx takes the increasing absolute reference as a percentage of the control for an unbounded response", {
  # relative deviation, the mirror of the decreasing form (T18)
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(20 + x_vec, 10 + x_vec)), x_vec)

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  expect_equal(result$reference[[2]], c(22, 11))
  expect_equal(result$draws[[2]], c(2, 1))
})

test_that("compute_ecx treats a binomial drc fit as a proportion", {
  x_vec <- 0:10
  y <- 0.1 + 0.08 * x_vec
  pred <- compute_pred(
    list(rbind(y, y)),
    x_vec,
    family = "binomial",
    source_class = "drc"
  )

  result <- toxval:::compute_ecx(pred, proportion = 0.50)

  expect_equal(result$reference[[2]], c(0.55, 0.55))
})

test_that("compute_ecx treats a binomial brmsfit as a proportion", {
  # EXPECTED-CHANGE binomial brmsfit predicted at one trial (T26) - done
  # was an NA increasing reference, when the trials were not recorded
  x_vec <- 0:10
  y <- 0.2 + 0.05 * x_vec
  pred <- compute_pred(list(rbind(y, y)), x_vec, family = "binomial")

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  expect_equal(result$reference[[2]], c(0.28, 0.28))
  expect_equal(result$reference[[1]], c(0.18, 0.18))
})

test_that("compute_ecx takes the upper bound from the family of each response", {
  x_vec <- 0:10
  y <- 0.2 + 0.05 * x_vec
  pred <- compute_pred(
    list(rbind(y, y), rbind(y, y)),
    x_vec,
    keys = data.frame(
      response = factor(
        c("survival", "growth"),
        levels = c("survival", "growth")
      )
    ),
    multi_var = c("survival", "growth"),
    family = c(survival = "bernoulli", growth = "gaussian")
  )

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  # the curves are identical, so the references differ only through the family:
  # extra risk for the bernoulli response, 0.2 + (1 - 0.2) * 0.1, and relative
  # deviation for the gaussian one, 0.2 + 0.2 * 0.1
  expect_equal(result$reference[[2]], c(0.28, 0.28))
  expect_equal(result$reference[[4]], c(0.22, 0.22))
})

test_that("compute_ecx returns an NA absolute reference where the control is at or below 0", {
  # a span of zero or less gives no percentage, in either direction
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(-1 - 0.1 * x_vec, 0 - 0.1 * x_vec, 2 - 0.1 * x_vec)),
    x_vec
  )

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  expect_equal(result$reference[[1]], c(NA, NA, 1.8))
  expect_equal(result$reference[[2]], c(NA, NA, 2.2))
  expect_equal(result$draws[[1]], c(NA, NA, 2))
})

test_that("compute_ecx accepts an absolute proportion above 100 for a response that goes negative", {
  x_vec <- 0:10
  y <- 10 - 2 * x_vec
  pred <- compute_pred(list(rbind(y, y)), x_vec)

  result <- toxval:::compute_ecx(pred, proportion = 1.50)

  expect_equal(result$reference[[1]], c(-5, -5))
  expect_equal(result$draws[[1]], c(7.5, 7.5))
})

test_that("compute_ecx measures the range reference down to the minimum of the curve", {
  x_vec <- 0:10
  y <- 10 - 0.5 * x_vec
  pred <- compute_pred(list(rbind(y, y)), x_vec)

  range <- toxval:::compute_ecx(pred, proportion = 0.10, type = "range")
  absolute <- toxval:::compute_ecx(pred, proportion = 0.10, type = "absolute")

  expect_equal(range$reference[[1]], c(9.5, 9.5))
  expect_equal(range$draws[[1]], c(1, 1))
  expect_equal(absolute$draws[[1]], c(2, 2))
})

test_that("compute_ecx measures the range reference up to the maximum of the curve", {
  x_vec <- 0:10
  y <- 2 + 0.5 * x_vec
  pred <- compute_pred(list(rbind(y, y)), x_vec)

  result <- toxval:::compute_ecx(pred, proportion = 0.20, type = "range")

  expect_equal(result$reference[[2]], c(3, 3))
  expect_equal(result$draws[[2]], c(2, 2))
  # the curve never falls below its control, so there is no span downwards
  expect_identical(result$reference[[1]], c(NA_real_, NA_real_))
  expect_identical(result$draws[[1]], c(NA_real_, NA_real_))
})

test_that("compute_ecx uses proportion as the reference in both directions under direct", {
  x_vec <- 0:4
  y <- c(6, 8, 10, 8, 6)
  pred <- compute_pred(list(rbind(y, y)), x_vec)

  result <- toxval:::compute_ecx(pred, reference = 7, type = "direct")

  expect_equal(result$reference[[1]], c(7, 7))
  expect_equal(result$reference[[2]], c(7, 7))
  expect_equal(result$draws[[1]], c(3.5, 3.5))
  expect_equal(result$draws[[2]], c(0.5, 0.5))
})

test_that("compute_ecx returns rows for each group in the order of keys", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(10 - x_vec, 10 - x_vec), rbind(5 - x_vec, 5 - x_vec)),
    x_vec,
    keys = data.frame(group = factor(c("B", "A"), levels = c("B", "A"))),
    group_var = "site"
  )

  result <- toxval:::compute_ecx(pred, proportion = 0.10)

  expect_identical(result$group, factor(c("B", "B", "A", "A"), c("B", "A")))
  expect_identical(result$direction, rep(c("decreasing", "increasing"), 2))
  expect_equal(result$draws[[1]], c(1, 1))
  expect_equal(result$draws[[3]], c(0.5, 0.5))
})

test_that("compute_ecx returns rows for each group and response", {
  x_vec <- 0:10
  y <- rbind(10 - x_vec, 10 - x_vec)
  pred <- compute_pred(
    list(y, y, y, y),
    x_vec,
    keys = data.frame(
      group = factor(c("A", "A", "B", "B")),
      response = factor(c("growth", "survival", "growth", "survival"))
    ),
    group_var = "site",
    multi_var = c("growth", "survival")
  )

  result <- toxval:::compute_ecx(pred)

  expect_identical(nrow(result), 8L)
  expect_identical(
    as.character(result$response),
    rep(c("growth", "growth", "survival", "survival"), 2)
  )
})

test_that("compute_ecx errors when pred is not a toxval_pred", {
  expect_error(
    toxval:::compute_ecx(matrix(1:4, 2)),
    "`pred` must inherit from S3 class 'toxval_pred'"
  )
})

test_that("compute_ecx errors on the relative type without an asymptote", {
  # EXPECTED-CHANGE relative type, measured to the asymptote (T24, T29) - done
  # was refused as an unsupported type
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - x_vec)), x_vec)

  expect_error(
    toxval:::compute_ecx(pred, type = "relative"),
    "`type` must not be 'relative' when `object` supplies no asymptote"
  )
})

test_that("a span of zero up to rounding error is excluded", {
  # the increasing asymptote equals the control up to rounding error, above it
  # in some realisations and below it in others
  x_vec <- 0:10
  curve <- 1 - x_vec / 20
  pred <- compute_pred(
    list(rbind(curve, curve, curve, curve)),
    x_vec,
    asymptote = list(list(
      decreasing = c(0.5, 0.5, 0.5, 0.5),
      increasing = 1 + c(1e-15, -1e-15, 1e-15, 0)
    ))
  )

  output <- suppressWarnings(nsec(pred, type = "relative"))
  expect_identical(output$ecnsec[[2]], NA_real_)

  warnings <- character()
  withCallingHandlers(
    ecx(pred, type = "relative"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warnings, "increasing: 100.0% excluded", all = FALSE)
})

# compute_nsec -----------------------------------------------------------------

test_that("compute_nsec returns one row per direction for an ungrouped fit", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - x_vec)), x_vec)

  result <- toxval:::compute_nsec(pred)

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "direction",
      "sig_val",
      "type",
      "anchor",
      "reference",
      "control",
      "asymptote",
      "first_change_away",
      "on_no_effect_side",
      "draws",
      "ecnsec"
    )
  )
  expect_identical(result$direction, c("decreasing", "increasing"))
  expect_identical(result$sig_val, c(0.01, 0.01))
  expect_identical(result$type, c("absolute", "absolute"))
})

test_that("the first change and the no-effect side are found for each realisation (T21, #74)", {
  # Reference 5. The first changes by round-off and then falls, the second is
  # flat, the third rises above the reference, and the fourth falls across a
  # missing value.
  curve <- rbind(
    c(4, 4 + 1e-12, rep(3, 9)),
    rep(4, 11),
    c(4, rep(6, 10)),
    c(4, NA, rep(3, 9))
  )

  expect_identical(
    toxval:::first_change_away_by_row(curve, "decreasing"),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    toxval:::first_change_away_by_row(curve, "increasing"),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_identical(
    toxval:::on_no_effect_side_by_row(curve, 5, "decreasing"),
    c(FALSE, FALSE, TRUE, FALSE)
  )
  expect_identical(
    toxval:::on_no_effect_side_by_row(curve, 5, "increasing"),
    c(TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("compute_nsec takes the reference from quantiles of the control realisations", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(result$control[[1]], c(9, 10, 11))
  expect_equal(result$reference, c(9.5, 10.5))
})

test_that("compute_nsec refuses the component anchor on a model average", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)), x_vec)
  pred$meta$draw_model <- c("a", "a", "b")

  expect_error(
    toxval:::compute_nsec(pred, anchor = "component"),
    "must be 'model' for a model-averaged fit"
  )
  expect_identical(toxval:::compute_nsec(pred)$anchor, c("model", "model"))
})

test_that("compute_nsec finds the decreasing crossing of each realisation", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  # the first realisation starts below the reference, so never crosses it
  expect_equal(result$draws[[1]], c(NA, 0.5, 1.5))
  expect_identical(result$draws[[2]], c(NA_real_, NA_real_, NA_real_))
})

test_that("compute_nsec finds the increasing crossing of each realisation", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(0.1 + 0.05 * x_vec, 0.2 + 0.05 * x_vec, 0.3 + 0.05 * x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(result$reference, c(0.15, 0.25))
  expect_equal(result$draws[[2]], c(3, 1, NA))
  expect_identical(result$draws[[1]], c(NA_real_, NA_real_, NA_real_))
})

test_that("compute_nsec computes the absolute ecnsec against 0 when decreasing", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(result$ecnsec[[1]], c(-0.0556, 0.05, 0.13636), tolerance = 0.001)
})

test_that("compute_nsec computes the absolute ecnsec against 1 when increasing for a proportion", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(0.1 + 0.05 * x_vec, 0.2 + 0.05 * x_vec, 0.3 + 0.05 * x_vec)),
    x_vec,
    family = "beta"
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(
    result$ecnsec[[2]],
    c(0.16667, 0.0625, -0.07143),
    tolerance = 0.001
  )
})

test_that("compute_nsec computes the absolute ecnsec against the control when increasing for an unbounded response", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(
    result$ecnsec[[2]],
    c(0.16667, 0.05, -0.04545),
    tolerance = 0.001
  )
})

# A span of zero or less gives no percentage, so `ecnsec` is NA rather than
# Inf or NaN (RF, PR #62 review)
test_that("compute_nsec returns an NA ecnsec only for the draws of a proportion whose control is at or above 1 when increasing", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(0.5 + 0.1 * x_vec, 1 + 0.1 * x_vec, 2 + 0.1 * x_vec)),
    x_vec,
    family = "beta"
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(result$reference[2], 1.5)
  expect_equal(result$ecnsec[[2]], c(2, NA, NA))
})

# A span of zero or less gives no percentage (RF, PR #62 review)
test_that("compute_nsec returns an NA ecnsec when the control is at or below 0 when decreasing", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(-1 - 0.1 * x_vec, 0 - 0.1 * x_vec, 2 - 0.1 * x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_equal(result$reference[1], -0.5)
  expect_equal(result$ecnsec[[1]], c(NA, NA, 1.25))
})

test_that("compute_nsec computes the range ecnsec against the minimum of the curve", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25, type = "range")

  expect_equal(result$ecnsec[[1]], c(-0.05, 0.05, 0.15))
})

# A span of zero or less gives no percentage (RF, PR #62 review)
test_that("compute_nsec returns an NA range ecnsec when the curve has no span in that direction", {
  x_vec <- 0:10
  pred <- compute_pred(
    list(rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)),
    x_vec
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25, type = "range")

  # each curve is decreasing, so its maximum is its control
  expect_identical(result$ecnsec[[2]], c(NA_real_, NA_real_, NA_real_))
})

test_that("compute_nsec errors on the direct type", {
  # its ecnsec would be the reference itself, already reported (T19)
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(9 - x_vec, 10 - x_vec)), x_vec)

  expect_error(
    toxval:::compute_nsec(pred, type = "direct"),
    "`type` must not be 'direct' for an NSEC"
  )
})

test_that("compute_nsec takes a separate reference for each group", {
  x_vec <- 0:10
  y <- rbind(9 - x_vec, 10 - x_vec, 11 - x_vec)
  pred <- compute_pred(
    list(y, y / 2),
    x_vec,
    keys = data.frame(group = factor(c("A", "B"))),
    group_var = "site"
  )

  result <- toxval:::compute_nsec(pred, sig_val = 0.25)

  expect_identical(result$group, factor(c("A", "A", "B", "B")))
  expect_equal(result$reference, c(9.5, 10.5, 4.75, 5.25))
  expect_equal(result$draws[[3]], c(NA, 0.5, 1.5))
})

# 0.5 and above would put the decreasing reference at or above the increasing
# one (RF, PR #62 review)
test_that("compute_nsec errors when sig_val is not below 0.5", {
  x_vec <- 0:10
  pred <- compute_pred(list(rbind(10 - x_vec, 10 - x_vec)), x_vec)

  expect_error(
    toxval:::compute_nsec(pred, sig_val = 0.5),
    "`sig_val` must be less than 0.5, not 0.5."
  )
})
