# Helpers for the toxval_predict() methods for bayesnec fits, which are in
# toxval-predict.R so that roxygen reads them after the generic they document.

#' The `bayesnec` classes with a `toxval_predict()` method
#'
#' @noRd
bnec_supported_classes <- function() {
  c("bayesnecfit", "bayesmanecfit")
}

#' Whether `bayesnec` is installed
#'
#' A function of its own so that a test can replace it with
#' `testthat::local_mocked_bindings()`.
#'
#' @noRd
bayesnec_available <- function() {
  requireNamespace("bayesnec", quietly = TRUE)
}

#' Check that `bayesnec` is installed
#'
#' The grid and the predictor are read through `bayesnec`, so a `bnecfit`
#' cannot be predicted without it, although the stored `brmsfit` could be.
#'
#' @param object A `bnecfit`.
#'
#' @return `object`, invisibly.
#'
#' @noRd
check_bayesnec_installed <- function(object) {
  if (!bayesnec_available()) {
    chk::abort_chk(
      "`bayesnec` must be installed to compute on a ",
      chk::cc(class(object)[1]),
      "."
    )
  }
  invisible(object)
}

#' Refuse the `bayesnecfit` objects whose curve is not one expected value
#'
#' @param object A `bayesnecfit`.
#'
#' @return `object`, invisibly.
#'
#' @noRd
chk_bayesnec_supported <- function(object) {
  # One level of a joint refit inherits `bayesnecfit`, so it dispatches to this
  # method, but its brmsfit is fitted to every level's data
  if (inherits(object, "bayesnecjointlevel")) {
    chk::abort_chk(
      "`object` must not be one level of a `bnec_joint()` refit; joint refits ",
      "are not yet supported by `toxval_predict()`."
    )
  }
  family <- object$fit$family$family
  # These families fit the zeros in a second part, and which part, or their
  # combination, a metric should be computed on is not yet decided
  if (family %in% bnec_two_part_families()) {
    chk::abort_chk(
      "`object` must not be fitted with the family ",
      chk::cc(family),
      ", which models the zeros in a separate part; such fits are not yet ",
      "supported by `toxval_predict()`."
    )
  }
  # The count part of a `bnec_hurdle()` fit is truncated at zero, and `brms`
  # gives its expected value as a sum over a finite grid, which `bayesnec`
  # replaces with the closed form
  truncated <- isTRUE(
    attr(object$bayesnecformula, "bayesnec_internal_truncation")
  )
  if (truncated && family %in% c("poisson", "negbinomial")) {
    chk::abort_chk(
      "`object` must not be the zero-truncated count part of a ",
      "`bnec_hurdle()` fit; such fits are not yet supported by ",
      "`toxval_predict()`."
    )
  }
  invisible(object)
}

#' The `bayesnec` families that model the zeros in a second part
#'
#' @noRd
bnec_two_part_families <- function() {
  c(
    "hurdle_gamma",
    "hurdle_poisson",
    "hurdle_negbinomial",
    "zero_inflated_beta"
  )
}

#' Check that no `group_var` was given for a fit with one curve
#'
#' @param group_var The supplied `group_var`.
#' @param object The fitted model, for the message.
#'
#' @return `group_var`, invisibly.
#'
#' @noRd
chk_no_group_var <- function(group_var, object) {
  if (!is.null(group_var)) {
    chk::abort_chk(
      "`group_var` must be NULL for a ",
      chk::cc(class(object)[1]),
      ", which has one curve; its group-level effects are marginalised."
    )
  }
  invisible(group_var)
}

#' The predictor of a `bnec()` fit
#'
#' `bayesnec` records the variable named inside `crf()` on the model frame of
#' its formula, as the `x_var` entry of the `bnec_pop` attribute. For
#' `crf(log(x), ...)` that is `x`, which is also the column the grid is built
#' over.
#'
#' @param formula The `bayesnecformula` of the fit.
#' @param data The data the fit was made to.
#' @param x_var The supplied `x_var`, or `NULL`.
#'
#' @return A string.
#'
#' @noRd
bnec_x_var <- function(formula, data, x_var) {
  # model.frame() dispatches to bayesnec's method for a bayesnecformula
  pop <- attr(stats::model.frame(formula, data = data), "bnec_pop")
  found <- unname(pop[names(pop) == "x_var"])
  if (length(found) != 1) {
    chk::abort_chk(
      "The `crf()` term of `object` must name one predictor variable, not ",
      length(found),
      "."
    )
  }
  if (is.null(x_var)) {
    return(found)
  }
  if (x_var != found) {
    chk::abort_chk(
      "`x_var` must be ",
      chk::cc(found),
      ", the predictor in the `crf()` term of `object`, not ",
      chk::cc(x_var),
      "."
    )
  }
  x_var
}

#' The data `bayesnec` predicts a fit at, over a grid chosen by `toxval`
#'
#' `bnec_newdata()` sets the trials of a binomial or beta-binomial response to
#' 1 and the denominator of a `rate()` term to 1. Building the data here instead
#' would mean copying those rules, which have changed between `bayesnec`
#' releases. It builds its grid with `seq()` from the ends of `x_range`, as
#' `predict_grid()` does, so the predictor column reproduces `x_vec`.
#'
#' @param object A `bnecfit`.
#' @param x_vec The grid from `predict_grid()`.
#'
#' @return A data frame with one row per grid value.
#'
#' @noRd
bnec_grid <- function(object, x_vec) {
  bayesnec::bnec_newdata(
    object,
    resolution = length(x_vec),
    x_range = range(x_vec)
  )
}

#' Check that every model of a model set has one family
#'
#' `bnec()` fits a model set with one family, so a set that fails this is
#' malformed. The family decides whether the averaged curve is a proportion.
#'
#' @param mod_fits The `mod_fits` of a `bayesmanecfit`.
#'
#' @return `mod_fits`, invisibly.
#'
#' @noRd
chk_one_family <- function(mod_fits) {
  families <- unique(vapply(
    mod_fits,
    function(component) component$fit$family$family,
    character(1)
  ))
  if (length(families) > 1) {
    chk::abort_chk(
      "Every model in `object` must have the same family, not ",
      chk::cc(families, conj = " and "),
      "."
    )
  }
  invisible(mod_fits)
}

#' The asymptotes of a `bnec()` fit at every draw
#'
#' The decreasing asymptote is the `bot` draw where the equation has `bot`, 0
#' where its curve instead tends to zero, and `NA` where the family has no lower
#' bound. On a link other than identity `bot` is on the link scale, so both
#' asymptotes are `NA`. No `bayesnec` equation has an upper-asymptote
#' parameter, so the increasing asymptote is the bound of a proportion, 1, and
#' otherwise `NA`.
#'
#' @param fit The `brmsfit` of one equation.
#'
#' @return A list of two numeric vectors with one value per draw, named
#'   `decreasing` and `increasing`.
#'
#' @noRd
bnec_asymptote <- function(fit) {
  n <- brms::ndraws(fit)
  family <- fit$family$family
  if (!identical(fit$family$link, "identity")) {
    return(list(decreasing = rep(NA_real_, n), increasing = rep(NA_real_, n)))
  }
  decreasing <- if ("b_bot_Intercept" %in% brms::variables(fit)) {
    unname(as.matrix(fit, variable = "b_bot_Intercept")[, 1])
  } else if (family %in% c("gaussian", "student")) {
    rep(NA_real_, n)
  } else {
    rep(0, n)
  }
  upper <- response_upper_bound(list(family = family), NULL, 1)
  increasing <- rep(if (is.finite(upper)) upper else NA_real_, n)
  list(decreasing = decreasing, increasing = increasing)
}

#' The draws a model average keeps from each model
#'
#' Reproduces `bayesnec`'s own choice, so that the realisations are the draws
#' its estimates and plots use: the index stored on the object when it was made
#' for the same number of draws, and otherwise a rebuild from the stored seed,
#' or from 216, `bayesnec`'s default, where none is stored. Each model keeps
#' `round(n * weight)` of the `n` draws, sampled without replacement. A
#' comparison test with `bayesnec::posterior_epred()` fails if `bayesnec`
#' changes the rule.
#'
#' @param object A `bayesmanecfit`.
#' @param model_set The names of the models, in the order of `object$mod_fits`.
#' @param weights The model weights, named by model.
#' @param n The smallest number of draws among the models.
#'
#' @return A list with `rows`, a list of row numbers named by model, and
#'   `seed`, the seed the index was drawn with, or `NULL` where it is not
#'   recorded.
#'
#' @noRd
bnec_draw_index <- function(object, model_set, weights, n) {
  stored <- object$w_draw_index
  sample_size <- object$sample_size
  if (!isTRUE(sample_size == n) && bnec_undercounted_set(object, n)) {
    n <- sample_size
  }
  if (
    !is.null(stored) &&
      isTRUE(sample_size == n) &&
      all(model_set %in% names(stored))
  ) {
    return(list(rows = stored[model_set], seed = object$w_draw_seed))
  }
  seed <- object$w_draw_seed
  if (is.null(seed)) {
    seed <- 216L
  }
  # `with_seed()` restores the caller's seed and generator kinds on exit, as
  # `bayesnec` does around the same draw
  rows <- withr::with_seed(
    seed,
    lapply(model_set, function(m) {
      sample(seq_len(n), as.integer(round(n * weights[[m]])))
    }),
    .rng_sample_kind = "Rejection"
  )
  list(rows = stats::setNames(rows, model_set), seed = seed)
}

#' Whether an older bayesnec fit recorded fewer draws than its models hold
#'
#' @param object A `bayesmanecfit`.
#' @param n The smallest number of draws held by its models.
#'
#' @return A flag.
#' @noRd
bnec_undercounted_set <- function(object, n) {
  fits <- object$mod_fits
  if (!length(fits)) {
    return(FALSE)
  }
  sim <- fits[[1]]$fit$fit@sim
  recorded <- ceiling((sim$iter - sim$warmup) / sim$thin * sim$chains)
  isTRUE(object$sample_size == recorded) &&
    isTRUE(n == min(vapply(fits, function(x) brms::ndraws(x$fit), numeric(1))))
}
