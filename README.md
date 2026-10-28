
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![license](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/open-AIMS/toxval/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)
[![DOI](https://zenodo.org/badge/760193848.svg)](https://zenodo.org/doi/10.5281/zenodo.12753573)
[![Codecov test
coverage](https://codecov.io/gh/open-AIMS/toxval/graph/badge.svg)](https://app.codecov.io/gh/open-AIMS/toxval)
[![R-CMD-check](https://github.com/open-AIMS/toxval/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/open-AIMS/toxval/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`toxval` is an R package for estimating ecotoxicological toxicity
metrics from fitted concentration-response models. It extracts effect
concentrations (ECx) with `ecx()` and No-Significant-Effect
Concentrations (NSEC) with `nsec()`.

`toxval` currently has methods for Bayesian models fitted with
[`brms`](https://cran.r-project.org/package=brms) and frequentist models
fitted with [`drc`](https://cran.r-project.org/package=drc). The package
is designed so that more model classes can be easy added, letting users
estimate toxicity metrics from whichever modelling framework they work
in.

For every model class, the fitted curve is evaluated over a grid of
concentrations once per realisation of the fit (once per posterior draw
for a Bayesian fit, and once per parametric bootstrap replicate for a
frequentist one), the metric is found for each realisation, and the
realisations are summarised by an estimate and compatibility limits. The
result is a `toxval` tibble with one row per curve and direction.

For the theoretical basis of the NSEC, see [Fisher & Fox
(2023)](https://doi.org/10.1002/etc.5610). For model-averaged N(S)EC
estimation across smooth and threshold model classes, see [Fisher et al.
(2024)](https://doi.org/10.1002/ieam.4809).

## Installation

The current development version can be installed from GitHub with

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("open-aims/toxval")
```

## Usage

The examples estimate the EC10, EC50 and NSEC of ryegrass root length
from the `ryegrass` data in `drc`.

### A frequentist fit

``` r
library(toxval)
library(drc)

fit_drc <- drm(rootl ~ conc, data = ryegrass, fct = LL.3())

ecx(fit_drc, proportion = c(0.1, 0.5), seed = 42)
#> # A tibble: 4 × 10
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high proportion type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>      <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing     1.33    0.946      1.70        0.1 absolute    7.85      7.06
#> 2 ecx    increasing    NA      NA         NA           0.1 absolute    7.85      8.63
#> 3 ecx    decreasing     3.27    2.89       3.66        0.5 absolute    7.85      3.92
#> 4 ecx    increasing    NA      NA         NA           0.5 absolute    7.85     11.8 
#> # ℹ 1 more variable: asymptote <dbl>

nsec(fit_drc, seed = 42)
#> # A tibble: 2 × 14
#> # Model:    drc
#> # Settings: resolution 500 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high sig_val type     anchor control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>     <dbl>
#> 1 nsec   decreasing     1.06    0.504      1.40    0.01 absolute model     7.85      7.36
#> 2 nsec   increasing    NA      NA         NA       0.01 absolute model     7.85      8.31
#> # ℹ 4 more variables: asymptote <dbl>, ecnsec <dbl>, ecnsec.low <dbl>, ecnsec.high <dbl>
```

Both directions are estimated every time. Root length falls with
concentration, so the increasing rows have no crossing and are `NA`. The
header records the model class and the settings the estimates were
computed with. The realisations of a `drc` fit are parametric bootstrap
replicates, so `seed` makes the results reproducible.

### A Bayesian fit

The same three-parameter log-logistic curve, fitted with `brms`:

``` r
fit_brms <- brms::brm(
  brms::bf(rootl ~ top / (1 + (conc / ec50)^b), top + ec50 + b ~ 1, nl = TRUE),
  data = ryegrass,
  prior = brms::set_prior("normal(0, 10)", nlpar = c("top", "ec50", "b"), lb = 0),
  seed = 42,
  refresh = 0
)
```

``` r
ecx(fit_brms, x_var = "conc", proportion = c(0.1, 0.5))
#> # A tibble: 4 × 10
#> # Model:    brmsfit
#> # Settings: resolution 500 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high proportion type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>      <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing     1.39     1.00      1.87        0.1 absolute    7.82      7.04
#> 2 ecx    increasing    NA       NA        NA           0.1 absolute    7.82      8.60
#> 3 ecx    decreasing     3.29     2.91      3.73        0.5 absolute    7.82      3.91
#> 4 ecx    increasing    NA       NA        NA           0.5 absolute    7.82     11.7 
#> # ℹ 1 more variable: asymptote <dbl>

nsec(fit_brms, x_var = "conc")
#> # A tibble: 2 × 14
#> # Model:    brmsfit
#> # Settings: resolution 500 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high sig_val type     anchor control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <chr>    <dbl>     <dbl>
#> 1 nsec   decreasing     1.14    0.593      1.55    0.01 absolute model     7.82      7.31
#> 2 nsec   increasing    NA      NA         NA       0.01 absolute model     7.82      8.36
#> # ℹ 4 more variables: asymptote <dbl>, ecnsec <dbl>, ecnsec.low <dbl>, ecnsec.high <dbl>
```

The realisations are the posterior draws. `x_var` is required for a
`brmsfit`, since a `brms` model may have several predictors.

`type` sets what the ECx effect is a proportion of, measured from the
control: `"absolute"` (towards 0, or towards 1 or twice the control when
increasing), `"relative"` (towards the asymptote of the fitted curve),
`"range"` (towards the lowest or highest response the curve predicts),
or `"direct"` (a response value supplied as `reference`). Supply
`draws = TRUE` to keep the per-realisation values in a list column, and
`group_var` to get one row per level of a grouping variable. A
multivariate `brms` fit gives one row per response.

Further documentation is on the [project
page](https://open-aims.github.io/toxval/) and the [reference
page](https://open-aims.github.io/toxval/reference/).

## Changes from toxval 1.0.0

toxval 2.0.0 restructures the package so that every model class is
handled the same way and every metric returns the same shape. Code
written for toxval 1.0.0 needs the following changes.

- `ecx()` and `nsec()` return a `toxval` tibble rather than a vector
  with attributes. The per-realisation values that `posterior = TRUE`
  returned are in a `draws` list column with `draws = TRUE`; `posterior`
  is defunct.
- Both directions are estimated every time and reported in the
  `direction` column. `hormesis_def` and `horme` are deprecated and
  ignored.
- `ecx_val` is deprecated in favour of `proportion`, a proportion rather
  than a percentage, so an EC10 is `proportion = 0.1` rather than
  `ecx_val = 10`. Under `type = "direct"` the response value is supplied
  as `reference`.
- `type = "relative"` now measures from the control to the asymptote of
  the fitted curve, and warns when supplied. What it measured before is
  now `type = "range"`.
- `nsec_multi()` is defunct. `nsec()` gives one row per response and
  direction for a multivariate `brms` fit; see `?nsec_multi` for how to
  convert a call.
- `curveid` and `by_group` are deprecated. Supply `group_var` to get one
  row per level.
- `ecx()` supports `drc` fits. The realisations of a `drc` fit are
  parametric bootstrap replicates, so supply `seed` for reproducible
  results.
- `ecx()` and `nsec()` no longer have methods for `bayesnec` fits, and
  `toxval` no longer depends on `bayesnec`. Use `bayesnec`’s own `ecx()`
  and `nsec()` for its fits.

Support for a new model class is added by writing a `toxval_predict()`
method that returns a `toxval_pred`, the object every metric computes
on. See `?toxval_predict` and `?new_toxval_pred`.

## The NSEC

The No-Significant-Effect Concentration (NSEC) is a model-based
alternative to the traditional No-Observed-Effect Concentration (NOEC).
Unlike the NOEC, which is tied to the experimental design and is
estimated via hypothesis testing, the NSEC:

- decouples the no-effect estimate from the set of concentrations
  actually tested;
- enables proper statistical inference with uncertainty quantification;
  and
- retains the intuitive concept of “no observable effect”.

For threshold-based models (NEC models), the NEC is a parameter of the
fitted model. Model-averaged N(S)EC estimates combine NSEC values from
smooth models with NEC values from threshold models, which is a more
statistically defensible approach to no-effect estimation (see [Fisher
et al. 2024](https://doi.org/10.1002/ieam.4809) and the `bayesnec`
package). `toxval` does not extract the NEC parameter or compute
model-averaged N(S)EC estimates.

## Motivation

Contemporary ecotoxicology increasingly relies on model-based inference
to derive toxicity metrics from concentration-response data. However,
the statistical tools used to estimate these metrics remain fragmented,
inconsistently defined, and often tied to specific modelling frameworks.
These inconsistencies hinder transparency, reproducibility, and
comparability in regulatory decision-making.

A core aim of `toxval` is to operationalise NSEC estimation across both
frequentist and Bayesian concentration-response models and to generalise
this capability to a broader suite of toxicity metrics. ECx values,
while widely used, depend on how effect is defined relative to controls,
model structure, and response scale, leading to inconsistencies that are
not always transparent to users (see [issue
\#19](https://github.com/open-AIMS/toxval/issues/19)).

`toxval` addresses these challenges by providing:

- a consistent interface for extracting ECx and NSEC values from fitted
  models;
- support for Bayesian and frequentist inference frameworks;
- explicit definitions of effect and no-effect that are independent of
  model class; and
- reproducible and comparable toxicity estimates across modelling
  approaches.

## Further Information

`toxval` is provided by the [Australian Institute of Marine
Science](https://www.aims.gov.au).

Copyright 2024-2026 Australian Institute of Marine Science. Copyright
2025-2026 Poisson Consulting Ltd.

Released under the [GPL (\>= 2)
License](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html).
