
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![license](https://img.shields.io/badge/license-GPL--2-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/open-AIMS/toxval/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)
[![DOI](https://zenodo.org/badge/760193848.svg)](https://zenodo.org/doi/10.5281/zenodo.12753573)
[![Codecov test
coverage](https://codecov.io/gh/open-AIMS/toxval/graph/badge.svg)](https://app.codecov.io/gh/open-AIMS/toxval)
<!-- badges: end -->

## Overview

`toxval` is an R package providing a unified, flexible framework for
estimating ecotoxicological toxicity metrics from concentration–response
data. It currently supports extraction of effect-based metrics (ECx) and
no-effect metrics — including the No-Significant-Effect Concentration
(NSEC) — from fitted frequentist
([`drc`](https://cran.r-project.org/package=drc)) and Bayesian
([`bayesnec`](https://open-aims.github.io/bayesnec/)) model objects.

For the theoretical basis of the NSEC, see [Fisher & Fox
(2023)](https://doi.org/10.1002/etc.5610). For model-averaged N(S)EC
estimation across smooth and threshold model classes, see [Fisher et al.
(2024)](https://academic.oup.com/ieam/article/20/1/279/7725045).

## Installation

The current development version can be installed from GitHub via

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("open-aims/bayesnec", ref = "dev")
```

## Motivation

Contemporary ecotoxicology increasingly relies on model-based inference
to derive toxicity metrics from concentration–response data. However,
the statistical tools used to estimate these metrics remain fragmented,
inconsistently defined, and often tied to specific modelling frameworks.
These inconsistencies hinder transparency, reproducibility, and
comparability in regulatory decision-making.

A core aim of `toxval` is to operationalise NSEC estimation across both
frequentist and Bayesian concentration–response models and to generalise
this capability to a broader suite of toxicity metrics. In doing so, the
package highlights a critical and often overlooked issue in
ecotoxicology: **ambiguity in the definition of “effect.”** ECx values —
while widely used — depend on how effect is defined relative to
controls, model structure, and response scale, leading to
inconsistencies that are not always transparent to users (see [issue
\#19](https://github.com/open-AIMS/toxval/issues/19)).

`toxval` addresses these challenges by providing:

- a **consistent interface** for extracting ECx, NOEC, and NSEC values
  from fitted models;
- support for **multiple model classes and inference frameworks**;
- **explicit, transparent definitions** of effect and no-effect that are
  independent of model class; and
- improved **reproducibility and comparability** of toxicity estimates
  across modelling approaches.

## The NSEC

The No-Significant-Effect Concentration (NSEC) is a model-based
alternative to the traditional No-Observed-Effect Concentration (NOEC).
Unlike the NOEC, which is tied to the experimental design and is
estimated via hypothesis testing, the NSEC:

- decouples the no-effect estimate from the set of concentrations
  actually tested;
- enables proper statistical inference with uncertainty quantification;
  and
- retains the intuitive concept of “no observable effect.”

For threshold-based models (NEC models), `toxval` also supports
extraction of the NEC parameter directly, and is designed to support
model-averaged N(S)EC estimates that combine NSEC values from smooth
models with NEC values from threshold models — providing a more
statistically defensible approach to no-effect estimation (see [Fisher
et al. 2024](https://academic.oup.com/ieam/article/20/1/279/7725045) and
the `bayesnec` package).

## Installation

The current development version can be downloaded from GitHub via

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("open-aims/toxval", ref = "main")
```

## Usage

Usage and further information about `toxval` can be seen on the [project
page](https://open-aims.github.io/toxval/) and the [reference
page](https://open-aims.github.io/toxval/reference/).

## Planned Development

`toxval` is under active development. The following enhancements are
planned over the coming months, drawing on open issues in the
repository:

### Expanded model class support ([\#21](https://github.com/open-AIMS/toxval/issues/21))

Currently `toxval` supports `drc` and `bayesnec` model fits. Support
will be extended to a wider range of commonly used R model classes,
including `glm`, `glmer`, `gam`, and `gamm`. The refactoring strategy is
to shift the bulk of computation to operate on a prediction matrix, so
that adding support for a new model class requires only a thin wrapper
around that class’s existing prediction method.

### Support for increasing responses ([\#20](https://github.com/open-AIMS/toxval/issues/20))

`nsec()` will be generalised to handle both increasing and decreasing
concentration–response curves. Currently only `nsec_multi()` supports
both directions; this capability will be folded into the main `nsec()`
function, with `nsec_multi()` becoming a deprecated special case for
multivariate responses. `ecx()` may also be extended to support
stimulatory (hormetic) effects where a biologically meaningful upper
bound exists.

### NOEC estimation ([\#17](https://github.com/open-AIMS/toxval/issues/17))

A method for deriving the NOEC will be added for models fitted with a
factor predictor. The control treatment will be inferred from the lowest
concentration level or specified by the user, with support for
one-tailed tests and a user-controlled significance threshold consistent
with the NSEC workflow.

### Model-averaged N(S)EC ([\#18](https://github.com/open-AIMS/toxval/issues/18))

Support will be added for estimating a model-averaged N(S)EC — combining
NSEC estimates from smooth (ECx-type) models with NEC values from
threshold models. Extraction from `bayesnec` multi-model fits will be
straightforward; extension to other fitting frameworks (e.g., `drc` or
custom threshold models) will follow.

### ECx definition clarity and consistency ([\#19](https://github.com/open-AIMS/toxval/issues/19))

Ongoing work will address the implicit ambiguity in ECx definitions
across model classes and implementations. Planned improvements include:

- clearer documentation of the `type` argument (`"relative"`,
  `"absolute"`, `"direct"`) and when each is appropriate;
- explicit warnings when ECx definitions are poorly defined for a given
  model family (e.g., unbounded Gaussian responses);
- transparent exposure of the reference points (e.g., `top`, `bottom`,
  control) used in the calculation.

### Model fitting wrappers ([\#22](https://github.com/open-AIMS/toxval/issues/22))

There is an open question about whether `toxval` should provide a thin
abstraction layer over existing concentration–response fitting
frameworks (e.g., `drc`, `bayesnec`). Such wrappers would standardise
input data structures, model specification, and output objects, while
making assumptions about bounds, link functions, and ECx/NEC definitions
explicit and consistent. Community input is welcome — see [issue
\#22](https://github.com/open-AIMS/toxval/issues/22).

## Further Information

`toxval` is provided by the [Australian Institute of Marine
Science](https://www.aims.gov.au) and Poisson Consulting under the GPL-2
License ([GPL-2](https://opensource.org/license/gpl-2-0/)).
