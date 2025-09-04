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
usethis::use_data(brms_model_1, overwrite = TRUE, internal = TRUE)

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
usethis::use_data(brms_model_2, overwrite = TRUE, internal = TRUE)

brms_model_3 <-
  brms::brm(
    y ~ s(x, bs = "cr", k = 5),
    data = bayesnec::nec_data,
    family = brms::Beta(),
    seed = 123
  )
usethis::use_data(brms_model_3, overwrite = TRUE, internal = TRUE)


data <- bayesnec::herbicide
data$x <- log(data$concentration)
data$y <- (data$fvfm + 0.001) * 0.999

brms_fit_4 <-
  bayesnec::bnec(
    y ~ crf(x, model = "ecx4param"),
    data = data,
    family = brms::Beta(),
    seed = 17,
    iter = 1000
  )

brms_pull_4 <- bayesnec::pull_brmsfit(brms_fit_4)
brms_prior_4 <- brms::prior_summary(brms_pull_4)

brms_model_4 <-
  brms::brm(
    brms::bf(
      y ~ top + (bot - top)/(1 + exp((ec50 - x) * exp(beta))),
      top + bot + beta + ec50 ~ herbicide,
      nl = TRUE
    ),
    data = data,
    family = brms::Beta(),
    prior = brms_prior_4,
    iter = 1000,
    save_pars = brms::save_pars(all = TRUE),
    seed = 700,
    init = 0
  )

usethis::use_data(brms_model_4, overwrite = TRUE, internal = TRUE)

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

usethis::use_data(bnec_model_1, overwrite = TRUE, internal = TRUE)


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

usethis::use_data(bayesnec_nec4param, overwrite = TRUE, internal = TRUE)


bayesnec_ecx4param <- bayesnec::pull_out(bayesnec::manec_example, model = "ecx4param")

usethis::use_data(bayesnec_ecx4param, overwrite = TRUE, internal = TRUE)

# nsec drc ----------------------------------------------------------------

nsec_drc_1 <- drc::drm(y ~ x, data = bayesnec::nec_data, fct = drc::LL.4())
usethis::use_data(nsec_drc_1, overwrite = TRUE)

# increasing drc model
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5),
  y = c(1.2, 1.1, 0.8, 0.92, 0.7, 0.4, 0.3, 0.2)
)

nsec_drc_2 <- drc::drm(y ~ x, data = data, fct = drc::LL.3())
usethis::use_data(nsec_drc_2, overwrite = TRUE, internal = TRUE)



# TODO ask becky about how grouping works in drc

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5),
  y = c(1.2, 1.1, 0.8, 0.92, 0.7, 0.4, 0.3, 0.2),
  z = c(1, 1, 1, 1, 2, 2, 2, 2)
)

drc::drm(y ~ x, curveid = c(1, 1, 1, 1, 2, 2, 2, 2), data = data, fct = drc::LL.3())
drc::drm(y ~ x, curveid = z, data = data, fct = drc::LL.3())

# nsec_multi --------------------------------------------------------------

data <- data.frame(
  dose = c(0.001, 0.005, 0.01, 0.05, 0.09, 0.1, 0.5, 0.9, 1.0, 1.5),
  survival = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
  growth = c(1.23, 2.13, 1.05, 1.32, 2.4, 2.1, 0.55, 0.34, 0.56, 0.67)
)

surv_formula <- brms::bf(survival ~ dose, family = brms::bernoulli())
growth_formula <- brms::bf(growth ~ dose)

nsec_multi_model_1 <-
  brms::brm(
    surv_formula + growth_formula,
    data = data,
    chains = 4,
    cores = 4,
    iter = 2000,
    seed = 123
  )

usethis::use_data(nsec_multi_model_1, overwrite = TRUE, internal = TRUE)



data <- data.frame(
  dose = c(0.001, 0.005, 0.01, 0.05, 0.09, 0.1, 0.5, 0.9, 1.0, 1.5),
  sp_survival = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
  sp_growth = c(1.23, 2.13, 1.05, 1.32, 2.4, 2.1, 0.55, 0.34, 0.56, 0.67)
)

surv_formula <- brms::bf(sp_survival ~ dose, family = brms::bernoulli())
growth_formula <- brms::bf(sp_growth ~ dose)

nsec_multi_model_2 <-
  brms::brm(
    surv_formula + growth_formula,
    data = data,
    chains = 4,
    cores = 4,
    iter = 2000,
    seed = 123
  )

usethis::use_data(nsec_multi_model_2, overwrite = TRUE, internal = TRUE)





data <- data.frame(
  dose = c(0.001, 0.005, 0.01, 0.05, 0.09, 0.1, 0.5, 0.9, 1.0, 1.5),
  trials = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3),
  sp_survival = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
  sp_growth = c(1.23, 2.13, 1.05, 1.32, 2.4, 2.1, 0.55, 0.34, 0.56, 0.67)
)

surv_formula <- brms::bf(sp_survival ~ dose, family = brms::bernoulli())
growth_formula <- brms::bf(sp_growth ~ dose)

nsec_multi_model_3 <-
  brms::brm(
    surv_formula + growth_formula,
    data = data,
    chains = 4,
    cores = 4,
    iter = 2000,
    seed = 123
  )


