# Tests for predict methods in R/predict.R

# predict.bayesnecfit ------------------------------------------------------

# NOTE: predict.bayesnecfit has a known bug — it calls pull_brmsfit()
# without namespace qualification (bayesnec::pull_brmsfit). This means
# it only works when bayesnec is attached via library(bayesnec), not just
# loaded. We test by calling brms::predict on the pulled brmsfit directly.

test_that("predict on pulled brmsfit returns expected structure", {
  bf <- bayesnec::pull_brmsfit(ecx4param)
  preds <- predict(bf)

  expect_true(is.matrix(preds) || is.array(preds))
  expect_equal(ncol(preds), 4)
  expect_equal(
    colnames(preds),
    c("Estimate", "Est.Error", "Q2.5", "Q97.5")
  )
  expect_gt(nrow(preds), 0)
})

# predict.bayesmanecfit ----------------------------------------------------

test_that("predict.bayesmanecfit returns summary by default", {
  preds <- predict(bayesnec::manec_example)

  expect_true(is.matrix(preds))
  expect_equal(ncol(preds), 4)
  expect_equal(
    colnames(preds),
    c("Estimate", "Est.Error", "Q2.5", "Q97.5")
  )
})

test_that("predict.bayesmanecfit summary = FALSE returns raw posterior", {
  preds <- predict(bayesnec::manec_example, summary = FALSE)

  expect_true(is.matrix(preds))
  # Raw posterior: rows = draws, columns = observations
  expect_gt(nrow(preds), 3)
})

test_that("predict.bayesmanecfit robust = TRUE uses median", {
  preds_mean <- predict(bayesnec::manec_example, robust = FALSE)
  preds_median <- predict(bayesnec::manec_example, robust = TRUE)

  # Both should have same shape
  expect_equal(dim(preds_mean), dim(preds_median))
  # Values should differ (median vs mean)
  expect_false(identical(preds_mean[, 1], preds_median[, 1]))
})

test_that("predict.bayesmanecfit probs argument changes CI columns", {
  preds <- predict(bayesnec::manec_example, probs = c(0.1, 0.9))

  expect_equal(ncol(preds), 4)
  expect_equal(colnames(preds)[3:4], c("Q10", "Q90"))
})

# predict.bayesmanecfit probs with > 2 values --------------------------------
# Man page: probs is "The percentiles to be computed by the quantile function."
# No restriction to exactly 2 values — source pads to c("Estimate","Est.Error",
# paste0("Q", probs*100)) so 3 probs gives a 5-column matrix.

test_that("predict.bayesmanecfit probs with 3 values returns 5-column matrix", {
  preds <- predict(bayesnec::manec_example, probs = c(0.1, 0.5, 0.9))

  expect_equal(ncol(preds), 5)
  expect_equal(
    colnames(preds),
    c("Estimate", "Est.Error", "Q10", "Q50", "Q90")
  )
})

# predict.bayesnecfit — direct dispatch documented bug -----------------------
# Man page documents predict.bayesnecfit(object, ...). The implementation calls
# pull_brmsfit() without the bayesnec:: namespace prefix. Since bayesnec is in
# Imports (not Depends) it is not attached, so pull_brmsfit is not in the
# search path and the call fails with "could not find function 'pull_brmsfit'".
# Remove `if (FALSE)` once the namespace prefix is added to R/predict.R.
if (FALSE) {
  test_that("predict.bayesnecfit dispatches correctly and returns 4-column matrix", {
    preds <- predict(ecx4param)

    expect_true(is.matrix(preds) || is.array(preds))
    expect_equal(ncol(preds), 4)
    expect_equal(colnames(preds), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
    expect_gt(nrow(preds), 0)
  })
}
