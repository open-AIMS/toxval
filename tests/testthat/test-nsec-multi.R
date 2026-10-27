test_that("nsec_multi is defunct in favour of nsec", {
  lifecycle::expect_defunct(
    nsec_multi(nsec_multi_model_2, x_var = "dose", multi_var = "sp")
  )
})
