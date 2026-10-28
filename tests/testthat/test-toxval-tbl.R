# new_toxval ----------------------------------------------------------------

# A valid ungrouped result with any column overridden by `...`. Every test below
# starts from a valid object and breaks one thing, so that the assertion names
# the rule being tested rather than whatever fails first.
toxval_rows <- function(...) {
  x <- data.frame(
    metric = c("ecx", "ecx"),
    direction = c("decreasing", "increasing"),
    estimate = c(1.2, NA),
    conf.low = c(0.9, NA),
    conf.high = c(1.5, NA)
  )
  changes <- list(...)
  x[names(changes)] <- changes
  x
}

# The meta of a toxval_pred, with any element overridden by `...`
toxval_meta <- function(...) {
  meta <- list(
    source_class = "brmsfit",
    x_var = "x",
    group_var = NULL,
    multi_var = NULL,
    resolution = 500,
    x_range = c(0, 3),
    family = "gaussian",
    realisation = "draws",
    n_realisation = 1000,
    seed = NULL
  )
  changes <- list(...)
  meta[names(changes)] <- changes
  meta
}

test_that("new_toxval returns a tibble subclass", {
  result <- new_toxval(toxval_rows(), toxval_meta())

  expect_s3_class(result, c("toxval", "tbl_df", "tbl", "data.frame"))
  expect_named(
    result,
    c("metric", "direction", "estimate", "conf.low", "conf.high")
  )
  expect_equal(nrow(result), 2L)
})

test_that("new_toxval keeps additional columns", {
  rows <- toxval_rows(type = "absolute")
  rows$draws <- list(c(1, 2), c(NA_real_, NA_real_))
  result <- new_toxval(rows, toxval_meta())

  expect_identical(result$type, c("absolute", "absolute"))
  expect_identical(result$draws, list(c(1, 2), c(NA_real_, NA_real_)))
})

test_that("new_toxval keeps the model class, resolution and realisations from meta for the header", {
  result <- new_toxval(toxval_rows(), toxval_meta())

  expect_identical(
    attr(result, "settings"),
    list(
      source_class = "brmsfit",
      resolution = 500,
      realisation = "draws",
      n_realisation = 1000
    )
  )
})

test_that("new_toxval errors when a core column is absent", {
  expect_error(
    new_toxval(toxval_rows()[-3], toxval_meta()),
    regexp = "Names of `x` must include 'estimate'"
  )
})

test_that("new_toxval errors on a metric outside the vocabulary", {
  expect_error(
    new_toxval(toxval_rows(metric = c("ec10", "ec10")), toxval_meta()),
    regexp = "`metric` must have values matching"
  )
})

test_that("new_toxval accepts only metrics in the vocabulary", {
  rows <- toxval_rows(metric = c("ecx", "nsec"))
  expect_s3_class(new_toxval(rows, toxval_meta()), "toxval")
})

test_that("new_toxval errors on an unknown direction", {
  expect_error(
    new_toxval(toxval_rows(direction = c("decreasing", "up")), toxval_meta()),
    regexp = "`direction` must have values matching"
  )
})

test_that("new_toxval errors on a non-numeric estimate", {
  expect_error(
    new_toxval(toxval_rows(estimate = c("1.2", NA)), toxval_meta()),
    regexp = "`estimate` must be numeric"
  )
})

test_that("new_toxval errors when a run setting is absent from meta", {
  expect_error(
    new_toxval(toxval_rows(), toxval_meta()[-5]),
    regexp = "Names of `settings` must include 'resolution'"
  )
})

test_that("new_toxval errors on an unknown realisation source", {
  expect_error(
    new_toxval(toxval_rows(), toxval_meta(realisation = "posterior")),
    regexp = "`settings\\$realisation` must match 'bootstrap' or 'draws'"
  )
})

test_that("new_toxval accepts a result with no rows", {
  result <- new_toxval(toxval_rows()[0, ], toxval_meta())
  expect_equal(nrow(result), 0L)
})

# tbl_sum.toxval ------------------------------------------------------------

test_that("tbl_sum.toxval describes a brmsfit result", {
  pred <- toxval_predict(brms_model_1, x_var = "x", resolution = 500)
  result <- suppressWarnings(summarise_metric(compute_ecx(pred), pred, "ecx"))

  expect_identical(
    pillar::tbl_sum(result)[c("Model", "Settings")],
    c(Model = "brmsfit", Settings = "resolution 500 | 1000 posterior draws")
  )
})

test_that("tbl_sum.toxval describes a drc result", {
  pred <- toxval_predict(nsec_drc_1, resolution = 200, n_boot = 100, seed = 42)
  result <- summarise_metric(compute_nsec(pred), pred, "nsec")

  expect_identical(
    pillar::tbl_sum(result)[c("Model", "Settings")],
    c(Model = "drc", Settings = "resolution 200 | 100 bootstrap realisations")
  )
})

test_that("tbl_sum.toxval adds the model and settings of posterior draws", {
  result <- new_toxval(toxval_rows(), toxval_meta())

  expect_identical(
    pillar::tbl_sum(result)[c("Model", "Settings")],
    c(Model = "brmsfit", Settings = "resolution 500 | 1000 posterior draws")
  )
})

test_that("tbl_sum.toxval names bootstrap realisations", {
  result <- new_toxval(
    toxval_rows(),
    toxval_meta(source_class = "drc", realisation = "bootstrap")
  )

  expect_identical(
    pillar::tbl_sum(result)[["Settings"]],
    "resolution 500 | 1000 bootstrap realisations"
  )
})

test_that("tbl_sum.toxval uses the singular for one realisation", {
  result <- new_toxval(toxval_rows(), toxval_meta(n_realisation = 1))

  expect_identical(
    pillar::tbl_sum(result)[["Settings"]],
    "resolution 500 | 1 posterior draw"
  )
})

test_that("print shows the header lines above the columns", {
  result <- new_toxval(toxval_rows(), toxval_meta())
  output <- capture.output(print(result))

  expect_identical(
    output[2:3],
    c(
      "# Model:    brmsfit",
      "# Settings: resolution 500 | 1000 posterior draws"
    )
  )
})

test_that("the header lines survive a dplyr filter", {
  result <- new_toxval(toxval_rows(), toxval_meta())
  filtered <- dplyr::filter(result, direction == "decreasing")

  expect_s3_class(filtered, "toxval")
  expect_identical(attr(filtered, "settings"), attr(result, "settings"))
})

test_that("toxval tibbles with the same settings combine and keep the header", {
  ecx_rows <- ecx(nsec_drc_1, seed = 1)
  nsec_rows <- nsec(nsec_drc_1, seed = 1)

  result <- dplyr::bind_rows(ecx_rows, nsec_rows)

  expect_s3_class(result, "toxval")
  expect_identical(attr(result, "settings"), attr(ecx_rows, "settings"))
  expect_identical(result$metric, c("ecx", "ecx", "nsec", "nsec"))
})

test_that("toxval tibbles with different settings do not combine", {
  brms_rows <- nsec(brms_model_3, x_var = "x")
  drc_rows <- nsec(nsec_drc_1, seed = 1)

  expect_error(
    dplyr::bind_rows(brms_rows, drc_rows),
    "Their run settings differ"
  )
  combined <- dplyr::bind_rows(
    tibble::as_tibble(brms_rows),
    tibble::as_tibble(drc_rows)
  )
  expect_false(inherits(combined, "toxval"))
  expect_identical(nrow(combined), 4L)
})
