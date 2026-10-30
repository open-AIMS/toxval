<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# toxval 2.0.0 (2026-10-30)

toxval 2.0.0 restructures the package so that every model class is handled the same way and every metric returns the same shape.
Code written for toxval 1.0.0 needs the changes listed under Breaking changes.

## Breaking changes

- `ecx()` and `nsec()` return a `toxval` tibble rather than a vector with attributes. The per-realisation values that `posterior = TRUE` returned are in a `draws` list column with `draws = TRUE`; `posterior` is defunct.
- Both directions are estimated every time and reported in the `direction` column. `hormesis_def` and `horme` are deprecated and ignored (#20).
- `ecx_val` is deprecated in favour of `proportion`, a proportion rather than a percentage, so an EC10 is `proportion = 0.1` rather than `ecx_val = 10`. Under `type = "direct"` the response value is supplied as `reference` (#3).
- `type = "relative"` now measures from the control to the asymptote of the fitted curve, and warns when supplied. What it measured before is now `type = "range"` (#19).
- The ECx and NSEC references are computed for each realisation from that realisation's own control, so estimates differ from toxval 1.0.0 (#19, #49).
- `nsec_multi()` is defunct. `nsec()` gives one row per response and direction for a multivariate `brms` fit; see `?nsec_multi` for how to convert a call.
- `curveid` and `by_group` are deprecated. Supply `group_var` to get one row per level.
- Arguments after `object` must be named, and a misspelt or unused argument is an error rather than being ignored (#55).
- The default `resolution` is 500 for every model class, so estimates can differ slightly from toxval 1.0.0.
- The intervals of `nsec()` for a `drc` fit come from parametric bootstrap replicates rather than from inverting a confidence band (#43).

## Features

- `ecx()` supports `drc` fits. The realisations of a `drc` fit are parametric bootstrap replicates, so supply `seed` for reproducible results. A parameter that must be positive, such as `e` in `LL.4()`, is drawn on the log scale (#76).
- `bayesnec` is suggested rather than imported. `ecx()` and `nsec()` accept model averages (`bayesmanecfit`) as well as single fits (`bayesnecfit`) when `bayesnec` is installed (#68).
- `nsec()` gains `type`, which names the span that `ecnsec` is a proportion of, and `anchor`, which sets the control of a model-averaged fit.
- New `x_control` argument. The control response is predicted at the lowest concentration in the data by default rather than at the first grid value (#73).
- A warning is given when an interval limit is censored at the highest concentration, or when realisations are excluded from the summary (#24).
- `xform` may be a decreasing function (#69).
- New `toxval_predict()` generic and `new_toxval_pred()` constructor. Support for a new model class is added by writing a `toxval_predict()` method that returns a `toxval_pred`, the object every metric computes on. See `?toxval_predict` and `?new_toxval_pred`.
- New vignettes define the ECx and the NSEC.

# toxval 1.0.0.9000 (2026-07-25)

- Package cleaning

# toxval 1.0.0

* Initial package release.
