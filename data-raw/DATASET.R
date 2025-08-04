## code to prepare `DATASET` dataset goes here

# BRMS test models ----------------------------------------------------------

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8)
)

brms_model_1 <-
  brms::brm(
    y ~ log(x),
    data = data,
    chains = 3,
    iter = 500,
    warmup = 250,
    seed = 101
  )

usethis::use_data(brms_model_1, overwrite = TRUE)

# this works for the grouping examples
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8),
  z = c(1, 1, 1, 2, 2)
)

brms_model_2 <-
  brms::brm(
    y ~ log(x) + z,
    data = data,
    chains = 3,
    iter = 500,
    warmup = 250,
    seed = 101
  )

usethis::use_data(brms_model_2, overwrite = TRUE)

# bnecfit -----------------------------------------------------------------

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.6, 0.5, 0.5, 0.9, 0.7, 0.6),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8, 0.85, .94, 0.9, 0.67, 0.99, 0.95)
)

plot(data$x, data$y)


set.seed(333)
bnec_model_1 <-
  bayesnec::bnec(
    y ~ crf(x, model = "nechorme"), # can simply ecxlin nec3param ecxexp
    data = data,
    open_progress = FALSE
  )

plot(bnec_model_1)

usethis::use_data(bnec_model_1, overwrite = TRUE)


bnec_model_2 <-
  bayesnec::bnec(
    y ~ crf(x, model = "nec3param"), # can simply ecxlin nec3param ecxexp
    data = data,
    open_progress = FALSE
  )


bnec_model_3 <-
  bayesnec::bnec(
    y ~ crf(x, model = "nec4param"), # can simply ecxlin nec3param ecxexp
    data = data,
    open_progress = FALSE
  )


output_1 <- ecx(bnec_model_1, hormesis_def = "max")
output_2 <- ecx(bnec_model_2, hormesis_def = "max")
output_3 <- ecx(bnec_model_3, hormesis_def = "max")

ecx(bnec_model_3, hormesis_def = "max")
output_3 <- ecx(bnec_model_3)


# nsec bayesnec -------------------------------------------------------------

bayesnec_nec4param <- bayesnec::pull_out(bayesnec::manec_example, model = "nec4param")

usethis::use_data(bayesnec_nec4param, overwrite = TRUE)


bayesnec_ecx4param <- bayesnec::pull_out(bayesnec::manec_example, model = "ecx4param")

usethis::use_data(bayesnec_ecx4param, overwrite = TRUE)

# nsec drc ----------------------------------------------------------------

nsec_drc_1 <- drc::drm(y ~ x, data = bayesnec::nec_data, fct = drc::LL.4())
usethis::use_data(nsec_drc_1, overwrite = TRUE)

# increasing drc model
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5),
  y = c(1.2, 1.1, 0.8, 0.92, 0.7, 0.4, 0.3, 0.2)
)

nsec_drc_2 <- drc::drm(y ~ x, data = data, fct = drc::LL.3())
usethis::use_data(nsec_drc_2, overwrite = TRUE)



# TODO ask becky about how grouping works in drc

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5),
  y = c(1.2, 1.1, 0.8, 0.92, 0.7, 0.4, 0.3, 0.2),
  z = c(1, 1, 1, 1, 2, 2, 2, 2)
)

drc::drm(y ~ x, curveid = c(1, 1, 1, 1, 2, 2, 2, 2), data = data, fct = drc::LL.3())
drc::drm(y ~ x, curveid = z, data = data, fct = drc::LL.3())

