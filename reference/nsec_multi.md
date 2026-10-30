# Estimate NSEC values from a multivariate brmsfit

**\[defunct\]**

`nsec_multi()` is defunct and calling it is an error. Use
[`nsec()`](https://open-aims.github.io/toxval/reference/nsec.md), which
gives one row per response and direction for a multivariate fit, reading
the responses from the fit.

## Usage

``` r
nsec_multi(...)
```

## Arguments

- ...:

  Unused.

## Converting to [`nsec()`](https://open-aims.github.io/toxval/reference/nsec.md)

`multi_var` and `trials_var` are not needed: the responses and any
trials column are read from the fit. `type` becomes a filter on the
`direction` column, and `posterior = TRUE` becomes `draws = TRUE`.

    # nsec_multi(fit, x_var = "dose", multi_var = "sp")
    nsec(fit, x_var = "dose")

    # nsec_multi(fit, x_var = "dose", multi_var = "sp", type = "decreasing")
    nsec(fit, x_var = "dose") |>
      dplyr::filter(direction == "decreasing")

    # nsec_multi(fit, x_var = "dose", multi_var = "sp", type = "lower",
    #   criterion = 0.8)
    nsec(fit, x_var = "dose", draws = TRUE) |>
      dplyr::mutate(
        at_criterion = vapply(
          draws,
          \(d) quantile(d$value.draw, 0.8, na.rm = TRUE),
          numeric(1)
        )
      ) |>
      dplyr::filter(!is.na(estimate)) |>
      dplyr::group_by(response) |>
      dplyr::slice_min(at_criterion, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
