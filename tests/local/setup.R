library(bayesnec)
library(dplyr)
library(testthat)
options(mc.cores = 1)


data(herbicide)
herbicide <- herbicide |> 
  dplyr::mutate(x = log(concentration),
                y = (fvfm+0.001)*0.999)
fit <-  bnec(y ~ crf(x, model = "ecx4param"), data = herbicide, 
             family = Beta(), seed = 17)

fit_brms <- pull_brmsfit(fit)

priors.fit <- prior_summary(fit_brms)
priors.fit
bf_fitInt <- brms::bf(y ~ top + (bot - top)/(1 + exp((ec50 - x) * exp(beta))),
                      top + bot + beta + ec50 ~ herbicide,
                      nl = TRUE)
fit_brmsInt <- brm(bf_fitInt, data = herbicide, family = Beta(),
                   prior = priors.fit, iter = 5000,  save_pars = save_pars(all=TRUE),
                   seed = 700, init = 0)



