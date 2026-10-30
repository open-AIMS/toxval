# Realisations of a fitted curve over a predictor grid

A `toxval_pred` holds realisations of one or more fitted curves
(posterior draws for a Bayesian fit, parametric bootstrap replicates for
a frequentist one) evaluated over a common predictor grid. It is
returned by
[`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md)
and can be passed to
[`ecx()`](https://open-aims.github.io/toxval/reference/ecx.md) and
[`nsec()`](https://open-aims.github.io/toxval/reference/nsec.md) in
place of the fitted model.

## Usage

``` r
new_toxval_pred(
  curves,
  keys,
  x_vec,
  meta,
  threshold = NULL,
  control = NULL,
  asymptote = NULL
)
```

## Arguments

- curves:

  An unnamed list of numeric matrices, one element per row of `keys`.
  Each matrix has one row per realisation and one column per value of
  `x_vec`.

- keys:

  A data frame identifying the curves, one row per element of `curves`.
  See Identifying curves.

- x_vec:

  A numeric vector of at least two finite, strictly increasing predictor
  values.

- meta:

  A named list of metadata. See Metadata.

- threshold:

  An unnamed list of numeric vectors of per-realisation values of a
  threshold parameter such as an NEC, one element per row of `keys`, or
  `NULL` where the model has none.

- control:

  An unnamed list of numeric vectors of per-realisation control
  responses from a separate control-only fit, one element per row of
  `keys`, or `NULL`.

- asymptote:

  An unnamed list, one element per row of `keys`, of lists of two
  numeric vectors of per-realisation asymptotes named `decreasing` (the
  lower asymptote) and `increasing` (the upper one), or `NULL` where the
  model supplies none. Used by `type = "relative"`.

## Value

An object of class `toxval_pred`, a list with elements `curves`, `keys`,
`x_vec`, `threshold`, `control`, `asymptote` and `meta`.

## Details

`new_toxval_pred()` builds one, and is for developers writing a
[`toxval_predict()`](https://open-aims.github.io/toxval/reference/toxval_predict.md)
method for a new model class.

## Identifying curves

Row *i* of `keys` identifies element *i* of `curves`, and of
`threshold`, `control` and `asymptote` when they are supplied. The
columns of `keys` are drawn from `group` and `response`, and become the
descriptor columns of the result:

- Ungrouped:

  One row and no columns, such as `data.frame(row.names = 1L)`.

- Grouped:

  A `group` column, a factor with one row per level.

- Multivariate:

  A `response` column, a factor with one row per response.

- Grouped multivariate:

  Both columns, with one row per combination of group and response.

Rows of `keys` are unique and contain no missing values.

## Realisation alignment

Realisation *i* is the same posterior draw or bootstrap replicate in
every slot: row *i* of every matrix in `curves`, and element *i* of
every element of `threshold`, `control` and `asymptote`.
`new_toxval_pred()` checks that the dimensions agree with
`meta$n_realisation` and `length(x_vec)`, but cannot check that the
realisations themselves are aligned.

## Metadata

`meta` must contain the following elements. Additional elements are
permitted and ignored.

- `source_class`:

  String, the class of the fitted object the realisations came from.

- `x_var`:

  String, the name of the predictor variable.

- `group_var`:

  String naming the grouping variable. Required when `keys` has a
  `group` column, and `NULL` otherwise.

- `multi_var`:

  Character vector of the response names of a multivariate fit,
  including every level of `keys$response`. Required when `keys` has a
  `response` column, and `NULL` otherwise.

- `resolution`:

  Whole number, the number of values in `x_vec`.

- `x_range`:

  Numeric of length 2, the range of `x_vec`.

- `x_control`:

  Number, the control concentration, at which the control response is
  predicted: the lowest predictor value in the data, unless the caller
  names another. It must be the first value of `x_vec`.

- `family`:

  String naming the response distribution, such as the `brms` family or
  the `type` of a `drc` fit, or `NULL`. A multivariate fit whose
  responses differ in family gives a character vector named by response.
  A binomial family is taken to be on the proportion scale.

- `realisation`:

  Either `"draws"` or `"bootstrap"`.

- `n_realisation`:

  Whole number, the number of realisations.

- `seed`:

  Whole number, the seed used to generate bootstrap realisations or to
  choose the realisations of a model average, or `NULL`.

The following elements are optional, and are checked when they are
present. A model average records them.

- `model_weights`:

  Named numeric, the weight of each model in a model average, each
  between 0 and 1 and summing to 1. A model whose weight gives it no
  realisation is still listed.

- `draw_model`:

  Character, one value per realisation, naming the model the realisation
  came from. Each value is a name in `model_weights` when that is
  present.

- `model_control`:

  An unnamed list, one element per row of `keys`, of named lists of
  numeric vectors. Each vector holds the control response of one model
  in every draw of its own fit, including draws left out of the average,
  so its length can differ from `n_realisation`. It requires
  `draw_model`, and every model named there has a vector. Under
  `anchor = "component"`,
  [`nsec()`](https://open-aims.github.io/toxval/reference/nsec.md) takes
  each model's reference from it.

## Examples

``` r
new_toxval_pred(
  curves = list(matrix(c(10, 9, 8, 7, 11, 9, 7, 5), nrow = 2, byrow = TRUE)),
  keys = data.frame(row.names = 1L),
  x_vec = c(0, 1, 2, 3),
  meta = list(
    source_class = "brmsfit",
    x_var = "x",
    group_var = NULL,
    multi_var = NULL,
    resolution = 4,
    x_range = c(0, 3),
    x_control = 0,
    family = "gaussian",
    realisation = "draws",
    n_realisation = 2,
    seed = NULL
  )
)
#> <toxval_pred> brmsfit
#>   realisations: 2 (draws)
#>   grid:         x, 4 values from 0 to 3
#>   curves:       1 (ungrouped)
#>   threshold:    none
#>   control:      none
```
