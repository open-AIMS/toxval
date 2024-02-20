test_that("prob_vals warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_length(
    nsec(manec_example, prob_vals = c(0.6, 0.1, 0.9), resolution = 10), 3
  ) |>
    suppressWarnings()
  expect_error(
    nsec(manec_example, prob_vals = 0.9, resolution = 10)
  ) |>
    suppressWarnings()
  expect_error(
    nsec(manec_example, prob_vals = c(0.6, 0.9, 0.1), resolution = 10)
  ) |>
    suppressWarnings()
  expect_length(
    nsec(nec4param, prob_vals = c(0.6, 0.1, 0.9), resolution = 10), 3
  )
  expect_error(nsec(nec4param, prob_vals = 0.9, resolution = 10))
  expect_error(nsec(nec4param, prob_vals = c(0.6, 0.9, 0.1), resolution = 10))
})

test_that(paste0("nsec returns expected object types and resolution is",
                 " passing correctly"), {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec_summary <- nsec(manec_example, sig_val = 0.01, resolution = 50) |>
    suppressWarnings()
  nsec_summary2 <- nsec(manec_example, sig_val = 0.01, resolution = 50,
                        xform = exp) |>
    suppressWarnings()
  nsec_posterior <- nsec(manec_example, sig_val = 0.01,
                         posterior = TRUE, resolution = 50) |>
    suppressWarnings()
  nsecn_summary <- nsec(nec4param, sig_val = 0.01, resolution = 50) |>
    suppressWarnings()
  nsecn_summary2 <- nsec(nec4param, sig_val = 0.01, resolution = 50,
                         xform = exp) |>
    suppressWarnings()
  nsecn_posterior <- nsec(nec4param, sig_val = 0.01,
                          posterior = TRUE, resolution = 50) |>
    suppressWarnings()
  expect_equal(length(nsec_summary), 3)
  expect_gt(length(nsec_posterior), 3)
  expect_equal(length(nsecn_summary), 3)
  expect_gt(length(nsecn_posterior), 3)
  expect_equal(attributes(nsec_summary)$resolution, 50)
  expect_equal(attributes(nsec_posterior)$resolution, 50)
  expect_equal(attributes(nsecn_summary)$resolution, 50)
  expect_equal(attributes(nsecn_posterior)$resolution, 50)
})

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(ecx4param, resolution = 10)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(manec_example, resolution = 10) |>
    suppressWarnings()
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(ecx4param, resolution = 10)
  nsec2 <- nsec(ecx4param, xform = exp, resolution = 10)
  expect_gt(nsec2[1], nsec1[2])
})

test_that("posterior passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec3 <- nsec(ecx4param, posterior = TRUE, resolution = 10)
  expect_equal(length(nsec3), 100)
})

test_that("prob_vals passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(ecx4param, prob_vals = c(0.5, 0.3, 0.7), resolution = 10)
  expect_equal(names(nsec4), c("Q50", "Q30", "Q70"))
})

test_that("sig_val passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(ecx4param, prob_vals = c(0.5, 0.3, 0.7), sig_val = 0.05,
                resolution = 10)
  expect_equal(names(nsec4), c("Q50", "Q30", "Q70"))
})

test_that("nsec works for drc using continuous data", {
  nec_data <-  bayesnec::nec_data
  model_LL4 <- drm(y~x, data=nec_data, fct=LL.4()) 
  nsec_val <- as.vector(round(nsec(model_LL4, x_var = "x"),1))
  expect_equal(nsec_val, c(1.2, 1.0, 1.3))
})

test_that("nsec for drc throws an error for an increasing function", {
  daphnids_m1 <- drm(no/total~dose, weights = total,curveid = time,
                     data = daphnids, fct = LL.2(), type = "binomial")
  expect_error(nsec(daphnids_m1, x_var = "dose"))
})

test_that("nsec for drc works for curveid and binomial data", {
  daphnids_m2 <- drm((total-no)/total~dose, weights = total,curveid = time,
                     data = daphnids, fct = LL.2(), type = "binomial")
  nsec_vals <- nsec(daphnids_m2, x_var = "dose", curveid = "time")
  expect_equal(dim(nsec_vals), c(2, 3))
  expect_equal(rownames(nsec_vals), c("24h", "48h"))
})

test_that("nsec for drc returns an error for an nec model", {
  data(ryegrass)
  rye_NEC4 <-  drm(rootl~conc, data=ryegrass, fct=NEC.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
  expect_error(nsec(rye_NEC4, x_var = "conc"), "nsec can currently only be estimated for smooth curves in drc, NEC models are not supported")
})






































































