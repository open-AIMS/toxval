#' Generate realisations of a fitted curve over a predictor grid
#'
#' `toxval_predict()` is the single generic in this package. It evaluates a
#' fitted concentration-response model over a grid of predictor values, once
#' per realisation of the fit, and returns the result as a [toxval_pred]. The
#' metric functions compute on that object, so adding support for a new model
#' class means writing one `toxval_predict()` method and nothing else.
#'
#' @details
#' Realisations are generated the same way for every model class: posterior
#' draws for a Bayesian fit, and a parametric bootstrap of
#' `MVN(coef(object), vcov(object))` for a frequentist one. A frequentist fit is
#' therefore not summarised through the three columns of
#' `predict(object, interval = "confidence")`: inverting a pointwise confidence
#' band on the response does not give a valid interval on the predictor, and it
#' degrades worst where the curve is flat, which is where NSEC sits.
#'
#' Because the bootstrap draw is joint over the whole parameter vector, the
#' replicates of one curve stay aligned with those of another in a fit with
#' several curves, as posterior draws are.
#'
#' @section Supplying the predictor:
#' `x_var` is optional in the generic because some classes record which of
#' their variables is the predictor: a `bayesnec` fit carries it on its
#' `bayesnecformula`, and a `drc` fit in `dataList$names$dName`. Requiring it
#' here would force a caller of such a class to repeat what the object already
#' knows, and no method could relax it, because the generic validates before
#' dispatch. A method that cannot identify the predictor -- a `brmsfit` may have
#' several, and a nonlinear formula mixes them with the model parameters --
#' errors on `NULL` instead.
#'
#' @section Grouping:
#' Supply `group_var` to get one curve per level rather than a single curve
#' marginalised across levels. The curves follow the declared order of a
#' factor, and otherwise the sorted order of the levels. Each method states
#' what `group_var` must name.
#'
#' @section The control concentration:
#' The control response is the predicted response at the control
#' concentration, and every reference, ECx and NSEC is measured from it. The
#' grid starts there. By default it is the lowest predictor value in the data.
#' `x_control` names another, such as 0 for a predictor measured as the
#' reduction from an ambient level, where some observations lie below 0. A
#' value outside the range of the data is predicted at a concentration that was
#' not tested, and warns. A grouped fit has one control concentration for every
#' curve.
#'
#' @param object A fitted model object. Methods are provided for
#'   [brms::brmsfit] and `drc` fits, and for the `bayesnecfit` and
#'   `bayesmanecfit` fits of `bayesnec`.
#' @param x_var A string naming the predictor variable in `object`, or `NULL`
#'   where the class of `object` records which of its variables is the
#'   predictor. Each method states whether it accepts `NULL`.
#' @param group_var A string naming the grouping variable in `object`, or `NULL`
#'   for a single curve.
#' @param resolution A whole number giving the number of predictor values to
#'   evaluate the curve at. Larger values estimate a metric more precisely.
#' @param x_range A numeric vector of length 2 whose second value ends the
#'   grid, or `NULL` to end it at the highest predictor value in the data. The
#'   grid starts at the control concentration, so the first value must equal
#'   it; supply `x_control` to name a different one.
#' @param x_control A number, the control concentration, at which the control
#'   response is predicted and the grid starts, or `NULL` for the lowest
#'   predictor value in the data. A value outside the range of the data warns.
#' @param ... Additional arguments passed to methods. These and every argument
#'   after `...` must be named in full; an argument the method does not use is
#'   an error.
#'
#' @return An object of class [toxval_pred].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("drc", quietly = TRUE)) {
#'   fit <- drc::drm(rootl ~ conc, data = drc::ryegrass, fct = drc::LL.4())
#'   toxval_predict(fit, x_var = "conc", resolution = 50, n_boot = 100, seed = 42)
#' }
#' if (requireNamespace("bayesnec", quietly = TRUE)) {
#'   toxval_predict(bayesnec::manec_example, resolution = 50)
#' }
#' }
#'
#' @export
toxval_predict <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
) {
  chk_x_var(x_var)
  chk::chk_null_or(group_var, vld = chk::vld_string)
  chk::chk_whole_number(resolution)
  chk::chk_gt(resolution, 1)
  chk_x_range(x_range)
  chk_x_control(x_control)
  # Grouping by the predictor would set the grid column to one level per curve,
  # giving a constant curve for each (#15). Checked here rather than in each
  # method, since both methods would otherwise return without error.
  if (!is.null(group_var) && identical(group_var, x_var)) {
    chk::abort_chk(
      "`group_var` and `x_var` must name different columns, not both ",
      chk::cc(x_var),
      "."
    )
  }

  UseMethod("toxval_predict")
}

#' @describeIn toxval_predict Method for any class without one, which errors.
#'
#' @export
toxval_predict.default <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
) {
  chk::abort_chk(
    "`object` must be a fitted model with a `toxval_predict()` method, ",
    "not an object of class ",
    chk::cc(class(object), conj = " or "),
    "."
  )
}

#' @describeIn toxval_predict Method for a `brms` fit of class
#'   [brms::brmsfit]. Realisations are the posterior draws, and group-level
#'   effects are marginalised out (`re_formula = NA`). `x_var` is required,
#'   since a `brmsfit` may have several predictors. `group_var` must name a
#'   column of the model data that enters as a population-level effect: a
#'   variable that appears only in a group-level term, such as `site` in
#'   `y ~ x + (1 | site)`, gives every level the same curve and is an error.
#'
#'   A multivariate fit gives one curve per response, and per level of
#'   `group_var` when it is supplied, so the responses are read from the fit
#'   rather than named. They are named as `brms` names them, for example
#'   `spsurvival` for a column `sp_survival`.
#'
#'   A binomial response, such as `y | trials(n) ~ x`, is predicted at one
#'   trial, so its curves, `control` and `reference` are proportions rather
#'   than counts whatever the trials in the data. The trials are read from the
#'   fit and must name a single column of the data. An NSEC or ECx does not
#'   depend on the number of trials, so only the scale of `control` and
#'   `reference` differs from a prediction at the observed trials.
#'
#'   `type = "relative"` needs `asymptote`, since which parameter of a
#'   non-linear formula is an asymptote cannot be read from the fit. It is not
#'   supported for a multivariate fit.
#'
#' @param asymptote `NULL`, or a vector named `decreasing`, `increasing` or
#'   both, giving the lower and upper asymptotes of a `brmsfit` for
#'   `type = "relative"`: as the names of non-linear parameters, such as
#'   `c(decreasing = "bot", increasing = "top")`, which are taken per posterior
#'   draw and transformed by the inverse link, or as numbers on the response
#'   scale. A direction not named has no asymptote.
#'
#' @export
toxval_predict.brmsfit <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  asymptote = NULL
) {
  chk_dots_unused(..., source = "`brmsfit`")
  chk_x_var_supplied(x_var, "`brmsfit`")
  data <- object$data
  chk_var_in_data(x_var, data, "x_var")
  if (!is.null(group_var)) {
    chk_var_in_data(group_var, data, "group_var")
  }
  responses <- brms_responses(object)
  chk_brms_asymptote(asymptote, object, responses)

  x_vec <- predict_grid(x_range, data[[x_var]], resolution, x_control)
  curve_set <- brms_curves(object, x_vec, x_var, group_var, responses)
  chk_defined_grid(curve_set$curves, x_vec)
  chk_population_level(curve_set$curves, max(1L, length(responses$names)))

  new_toxval_pred(
    curves = curve_set$curves,
    keys = curve_set$keys,
    x_vec = x_vec,
    asymptote = brms_asymptote(
      object,
      asymptote,
      x_vec,
      x_var,
      group_var,
      responses$trials_var,
      nrow(curve_set$curves[[1]])
    ),
    meta = c(
      build_meta(
        object = object,
        x_vec = x_vec,
        x_var = x_var,
        curves = curve_set$curves,
        group_var = group_var,
        multi_var = responses$names,
        realisation = "draws",
        family = responses$family
      ),
      list(trials_var = responses$trials_var)
    )
  )
}

#' @describeIn toxval_predict Method for a `drc` fit returned by [drc::drm()].
#'   Realisations are parametric bootstrap replicates, so results are stochastic
#'   unless `seed` is supplied. `x_var` is read from the fit when `NULL`, and a
#'   supplied `x_var` must match the dose variable of the fit, so another
#'   column, such as the response, cannot be passed as the predictor.
#'   `group_var` must name the column passed as `curveid`, and the levels are
#'   read from the coefficient names rather than from a column position. The
#'   asymptotes `type = "relative"` measures to are the lower and upper limits
#'   of the fitted function, from `fct$lowerAs()` and `fct$upperAs()`.
#'
#' @param n_boot A whole number giving the number of parametric bootstrap
#'   replicates to draw.
#' @param seed A whole number used to seed the bootstrap draw, or `NULL` to use
#'   the current random number state. The seed is recorded in the returned
#'   object.
#'
#' @export
toxval_predict.drc <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL,
  n_boot = 1000,
  seed = NULL
) {
  chk_dots_unused(..., source = "`drc` fit")
  chk::chk_whole_number(n_boot)
  chk::chk_gt(n_boot, 0)
  chk::chk_null_or(seed, vld = chk::vld_whole_number)

  dose_var <- object$dataList$names$dName
  if (is.null(x_var)) {
    x_var <- dose_var
  } else if (x_var != dose_var) {
    chk::abort_chk(
      "`x_var` must be ",
      chk::cc(dose_var),
      ", the dose variable of `object`, not ",
      chk::cc(x_var),
      "."
    )
  }
  data <- object$data
  if (!is.null(group_var)) {
    chk_var_in_data(group_var, data, "group_var")
  }

  x_vec <- predict_grid(x_range, data[[x_var]], resolution, x_control)
  curve_set <- drc_curves(object, x_vec, group_var, n_boot, seed)
  chk_defined_grid(curve_set$curves, x_vec)

  new_toxval_pred(
    curves = curve_set$curves,
    keys = curve_set$keys,
    x_vec = x_vec,
    asymptote = curve_set$asymptote,
    meta = build_meta(
      object = object,
      x_vec = x_vec,
      x_var = x_var,
      curves = curve_set$curves,
      group_var = group_var,
      realisation = "bootstrap",
      # `type` names the response distribution of a `drc` fit, and
      # `"binomial"` marks a response on the proportion scale (T18)
      family = object$type,
      seed = seed
    )
  )
}

#' @describeIn toxval_predict Method for a single model fitted by
#'   `bayesnec::bnec()`, of class `bayesnecfit`, which requires `bayesnec` to
#'   be installed. Realisations are the posterior draws of the `brmsfit` held in
#'   `object$fit`, and group-level effects are marginalised out
#'   (`re_formula = NA`). The grid spans the same concentrations as for a
#'   `brmsfit`, and `bayesnec` builds the data to predict at, with a binomial
#'   or beta-binomial response predicted for one trial, so that its curve is a
#'   proportion. `x_var` is read from the `crf()`
#'   term when `NULL`, and a supplied `x_var` must match it. `group_var` must be
#'   `NULL`, since a `bnec()` fit has one curve. A predictor transformed inside
#'   `crf()`, such as `crf(log(x), ...)`, is reported in units of the variable
#'   itself, `x`, so `xform = exp` must not be supplied for it. The lower
#'   asymptote is the `bot` parameter for an equation that has one, and
#'   otherwise 0, the limit of its curve; the upper asymptote is 1 for a
#'   proportion. An asymptote is `NA` where the family has no such bound, or
#'   where the fit uses a link other than identity.
#'
#' @export
toxval_predict.bayesnecfit <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
) {
  chk_dots_unused(..., source = "`bayesnecfit`")
  check_bayesnec_installed(object)
  chk_bayesnec_supported(object)
  chk_no_group_var(group_var, object)

  data <- object$fit$data
  x_var <- bnec_x_var(object$bayesnecformula, data, x_var)
  newdata <- bnec_grid(
    object,
    predict_grid(x_range, data[[x_var]], resolution, x_control)
  )
  x_vec <- newdata[[x_var]]
  # Read from the stored brmsfit rather than through bayesnec's own
  # posterior_epred() methods, which have none for the components of a model
  # set, so that one call serves both bayesnec classes (BAYESNEC-claude.md §3.1)
  curve <- posterior_epred(object$fit, newdata = newdata, re_formula = NA)
  chk_defined_grid(list(curve), x_vec)

  new_toxval_pred(
    curves = list(curve),
    keys = group_keys(),
    x_vec = x_vec,
    asymptote = list(bnec_asymptote(object$fit)),
    meta = build_meta(
      object = object,
      x_vec = x_vec,
      x_var = x_var,
      curves = list(curve),
      realisation = "draws",
      family = object$fit$family$family
    )
  )
}

#' @describeIn toxval_predict Method for a model average fitted by
#'   `bayesnec::bnec()`, of class `bayesmanecfit`, which requires `bayesnec` to
#'   be installed. Each model in the set is predicted as for a `bayesnecfit`.
#'   The realisations are the draws `bayesnec` keeps from each model in
#'   proportion to its weight, the same draws its own estimates and plots use.
#'   The model each realisation came from is recorded in `meta$draw_model`, the
#'   weights in `meta$model_weights`, and the seed the draws were chosen with in
#'   `meta$seed`. The random number state of the session is left unchanged.
#'   Each realisation takes its asymptotes from its own model. The control
#'   response of each model in every draw of its fit, including the draws left
#'   out of the average, is recorded in `meta$model_control`.
#'
#' @export
toxval_predict.bayesmanecfit <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
) {
  chk_dots_unused(..., source = "`bayesmanecfit`")
  check_bayesnec_installed(object)
  mod_fits <- object$mod_fits
  for (component in mod_fits) {
    chk_bayesnec_supported(component)
  }
  chk_one_family(mod_fits)
  chk_no_group_var(group_var, object)

  model_set <- names(mod_fits)
  weights <- stats::setNames(object$mod_stats[model_set, "wi"], model_set)
  # every model is fitted to the same data with the same crf() term
  data <- mod_fits[[1]]$fit$data
  x_var <- bnec_x_var(mod_fits[[1]]$bayesnecformula, data, x_var)
  newdata <- bnec_grid(
    object,
    predict_grid(x_range, data[[x_var]], resolution, x_control)
  )
  x_vec <- newdata[[x_var]]

  per_model <- lapply(mod_fits, function(component) {
    posterior_epred(component$fit, newdata = newdata, re_formula = NA)
  })
  n <- min(vapply(per_model, nrow, integer(1)))
  index <- bnec_draw_index(object, model_set, weights, n)
  curve <- do.call(
    rbind,
    lapply(model_set, function(m) {
      per_model[[m]][index$rows[[m]], , drop = FALSE]
    })
  )
  chk_defined_grid(list(curve), x_vec)
  per_model_asymptote <- lapply(mod_fits, function(component) {
    bnec_asymptote(component$fit)
  })
  asymptote <- lapply(stats::setNames(nm = directions()), function(direction) {
    unlist(
      lapply(model_set, function(m) {
        per_model_asymptote[[m]][[direction]][index$rows[[m]]]
      }),
      use.names = FALSE
    )
  })

  new_toxval_pred(
    curves = list(curve),
    keys = group_keys(),
    x_vec = x_vec,
    asymptote = list(asymptote),
    meta = build_meta(
      object = object,
      x_vec = x_vec,
      x_var = x_var,
      curves = list(curve),
      realisation = "draws",
      family = mod_fits[[1]]$fit$family$family,
      seed = index$seed,
      # a model with no realisation in the average stays in `model_weights`
      draw_model = rep(model_set, lengths(index$rows)),
      model_weights = weights,
      # keep every draw, not only the rows of the index, so that each model's
      # component reference is taken over its whole fit
      model_control = list(lapply(per_model, function(p) unname(p[, 1])))
    )
  )
}

#' @describeIn toxval_predict Method for any other `bayesnec` fit, which
#'   errors. Group fits, joint refits and hurdle pairs each hold more than one
#'   curve, over concentrations that can differ between curves, which a
#'   [toxval_pred] does not yet represent.
#'
#' @export
toxval_predict.bnecfit <- function(
  object,
  ...,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  x_control = NULL
) {
  chk::abort_chk(
    "`object` must be a ",
    chk::cc(bnec_supported_classes(), conj = " or "),
    " to be predicted by `toxval_predict()`, not a ",
    chk::cc(class(object)[1]),
    "."
  )
}

#' Assemble the metadata of a toxval_pred
#'
#' Every method builds its `meta` here, so the contract is satisfied in one
#' place rather than once per method. `resolution` and `x_range` are derived
#' from the grid rather than from the arguments the method was called with,
#' which is what they are documented to describe.
#'
#' @param object The fitted model the realisations came from.
#' @param x_vec The predictor grid.
#' @param x_var Name of the predictor variable.
#' @param curves The list of realisation matrices.
#' @param group_var Name of the grouping variable, or `NULL`.
#' @param multi_var The response names of a multivariate fit, or `NULL`.
#' @param realisation Either `"draws"` or `"bootstrap"`.
#' @param family The response distribution: the `brms` family, named by
#'   response for a multivariate fit, the `type` of a `drc` fit, or `NULL`
#'   where there is none.
#' @param seed The bootstrap seed, or `NULL`.
#' @param ... Optional elements of `meta`, such as `draw_model` and
#'   `model_weights`, appended to the required ones.
#'
#' @return A named list satisfying the `meta` contract of [toxval_pred].
#'
#' @noRd
build_meta <- function(
  object,
  x_vec,
  x_var,
  curves,
  group_var = NULL,
  multi_var = NULL,
  realisation,
  family = NULL,
  seed = NULL,
  ...
) {
  required <- list(
    source_class = class(object)[1],
    x_var = x_var,
    group_var = group_var,
    multi_var = multi_var,
    resolution = length(x_vec),
    x_range = range(x_vec),
    # predict_grid() starts the grid at the control concentration
    x_control = x_vec[1],
    family = family,
    realisation = realisation,
    n_realisation = nrow(curves[[1]]),
    seed = seed
  )
  c(required, list(...))
}

#' The responses of a brmsfit, read from its formula
#'
#' @param object A fitted model of class [brms::brmsfit].
#'
#' @return A list with `names`, the response names of a multivariate fit as
#'   `posterior_epred()` names them, or `NULL` for a univariate fit; `family`,
#'   one string for a univariate fit and a vector named by response for a
#'   multivariate one (T26); and `trials_var`, the columns holding the trials
#'   of any binomial response, named by response for a multivariate fit, or
#'   `NULL`.
#'
#' @noRd
brms_responses <- function(object) {
  terms <- brms::brmsterms(object$formula)
  multivariate <- brms::is.mvbrmsformula(object$formula)
  forms <- if (multivariate) terms$terms else list(terms)

  family <- vapply(forms, function(form) form$family$family, character(1))
  trials_var <- unlist(lapply(forms, trials_column))
  if (!multivariate) {
    return(list(
      names = NULL,
      family = unname(family),
      trials_var = trials_var
    ))
  }
  list(names = names(forms), family = family, trials_var = trials_var)
}

#' The column holding the trials of a binomial response
#'
#' The trials are set to 1 in the prediction data, so that a binomial response
#' is predicted as a proportion (T26). That needs them to be a single column:
#' a constant, as in `trials(10)`, or an expression, as in `trials(n + m)`,
#' cannot be set to 1.
#'
#' @param form The `brmsterms` of one response.
#'
#' @return The name of the trials column, or `NULL` where the response has no
#'   trials.
#'
#' @noRd
trials_column <- function(form) {
  trials <- form$adforms$trials
  if (is.null(trials)) {
    return(NULL)
  }
  # the right-hand side is `resp_trials(<expression>)`
  expr <- trials[[2]][[2]]
  if (!is.symbol(expr)) {
    chk::abort_chk(
      "The trials of a binomial response must be a single column of the ",
      "data, not ",
      chk::cc(deparse(expr)),
      "; it is set to 1 to predict a proportion."
    )
  }
  as.character(expr)
}

#' Posterior draws of every curve in a brmsfit
#'
#' A grouped fit is predicted once per level, and a multivariate fit gives one
#' curve per response from each prediction, so the curves run by response
#' within group.
#'
#' @param object A fitted model of class [brms::brmsfit].
#' @param x_vec The predictor grid.
#' @param x_var Name of the predictor variable.
#' @param group_var Name of the grouping variable, or `NULL`.
#' @param responses The output of `brms_responses()`.
#'
#' @return A list with `curves`, a list of matrices with one per curve, and
#'   `keys`, the [toxval_pred] table identifying them.
#'
#' @noRd
brms_curves <- function(object, x_vec, x_var, group_var, responses) {
  groups <- NULL
  if (!is.null(group_var)) {
    groups <- group_levels(object$data[[group_var]])
  }
  by_group <- lapply(seq_len(max(1L, length(groups))), function(i) {
    epred <- epred_curve(
      object,
      x_vec,
      x_var,
      group_var,
      groups[i],
      responses$trials_var
    )
    if (is.null(responses$names)) {
      return(list(epred))
    }
    # matched by name, not by position (T26)
    lapply(responses$names, function(response) {
      matrix(
        epred[,, response],
        nrow = dim(epred)[1],
        ncol = dim(epred)[2]
      )
    })
  })
  list(
    curves = unlist(by_group, recursive = FALSE),
    keys = brms_keys(groups, responses$names)
  )
}

#' The `keys` table of a brmsfit, with responses within groups
#'
#' @param groups The levels of the grouping variable, or `NULL`.
#' @param responses The response names of a multivariate fit, or `NULL`.
#'
#' @noRd
brms_keys <- function(groups, responses) {
  if (is.null(responses)) {
    return(group_keys(groups))
  }
  n_groups <- max(1L, length(groups))
  keys <- data.frame(
    response = factor(rep(responses, times = n_groups), levels = responses)
  )
  if (is.null(groups)) {
    return(keys)
  }
  groups <- as.character(groups)
  data.frame(
    group = factor(rep(groups, each = length(responses)), levels = groups),
    keys
  )
}

#' Check that the levels of `group_var` give different curves
#'
#' Group-level effects are marginalised, so a `group_var` that enters only a
#' group-level term gives every level the same curve. For a multivariate fit
#' the curves are compared within each response, and it is an error only when
#' `group_var` changes the curve of no response.
#'
#' @param curves The list of realisation matrices, responses within groups.
#' @param n_responses The number of responses, 1 for a univariate fit.
#'
#' @noRd
chk_population_level <- function(curves, n_responses) {
  if (length(curves) == n_responses) {
    return(invisible(curves))
  }
  same <- vapply(
    seq_len(n_responses),
    function(r) {
      by_group <- curves[seq(r, length(curves), by = n_responses)]
      all(vapply(by_group[-1], identical, logical(1), by_group[[1]]))
    },
    logical(1)
  )
  if (all(same)) {
    chk::abort_chk(
      "group-level effects are marginalised; ",
      "`group_var` must be fitted as a population-level effect in `object` to ",
      "give one curve per level."
    )
  }
  invisible(curves)
}

#' The `keys` table of an ungrouped or grouped fit
#'
#' @param groups The levels of the grouping variable, in curve order, or `NULL`
#'   for a single curve.
#'
#' @return A data frame with a factor `group` column whose levels follow the
#'   curve order, or one row and no columns when `groups` is `NULL`.
#'
#' @noRd
group_keys <- function(groups = NULL) {
  if (is.null(groups)) {
    return(data.frame(row.names = 1L))
  }
  groups <- as.character(groups)
  data.frame(group = factor(groups, levels = groups))
}

#' The levels of a grouping variable, in a stable order
#'
#' The levels become the group descriptor of the result, so they are ordered by
#' the content of the variable rather than by the order its rows happen to be
#' in: re-sorting the input data must not re-order the output.
#' Character levels are sorted with `method = "radix"`, which uses the C-locale
#' order in every session.
#'
#' @param x A grouping variable.
#'
#' @return The observed levels, as a factor where `x` is one so that the
#'   levels survive into `newdata`.
#'
#' @noRd
group_levels <- function(x) {
  if (is.factor(x)) {
    x <- droplevels(x)
    return(factor(levels(x), levels = levels(x)))
  }
  sort(unique(x), method = "radix")
}

#' Posterior draws of the mean curve over a grid
#'
#' @param object A fitted model of class [brms::brmsfit].
#' @param x_vec The predictor grid.
#' @param x_var Name of the predictor variable.
#' @param group_var Name of the grouping variable, or `NULL`.
#' @param group A single level of the grouping variable.
#' @param trials_var The columns holding the trials of a binomial response,
#'   which are set to 1, or `NULL`.
#'
#' @details
#' Where the model has a population-level predictor other than `x_var` that is
#' not named as `group_var`, `brms` cannot build the prediction data and errors
#' in its own terms. That error is caught and restated in terms of `x_var`
#' and `group_var` (#56), naming the variables `brms` reported. Which variables
#' need a value is left to `brms` rather than read from the formula, since a
#' variable in a group-level term, such as `site` in `(1 | site)`, needs none
#' under `re_formula = NA`.
#'
#' @return A numeric matrix, one row per draw and one column per grid value,
#'   or for a multivariate fit an array with a third dimension named by
#'   response.
#'
#' @noRd
epred_curve <- function(
  object,
  x_vec,
  x_var,
  group_var = NULL,
  group = NULL,
  trials_var = NULL
) {
  newdata <- brms_newdata(x_vec, x_var, group_var, group, trials_var)
  tryCatch(
    posterior_epred(object, newdata = newdata, re_formula = NA),
    brms_error = function(e) {
      message <- conditionMessage(e)
      if (!grepl("can neither be found in 'data'", message, fixed = TRUE)) {
        stop(e)
      }
      quoted <- regmatches(message, gregexpr("'[^']+'", message))[[1]]
      missing_vars <- setdiff(gsub("'", "", quoted), c("data", "data2"))
      chk::abort_chk(
        "`object` needs a value of ",
        chk::cc(missing_vars, conj = " and "),
        " to predict; name it as `x_var`, or as `group_var` to get one curve ",
        "per level."
      )
    }
  )
}

#' The prediction data of one curve of a brmsfit
#'
#' @noRd
brms_newdata <- function(x_vec, x_var, group_var, group, trials_var) {
  newdata <- stats::setNames(data.frame(x_vec), x_var)
  if (!is.null(group_var)) {
    newdata[[group_var]] <- group
  }
  for (var in trials_var) {
    newdata[[var]] <- 1
  }
  newdata
}

#' Check the `asymptote` of a brmsfit
#'
#' @noRd
chk_brms_asymptote <- function(asymptote, object, responses) {
  if (is.null(asymptote)) {
    return(invisible(asymptote))
  }
  if (!is.null(responses$names)) {
    chk::abort_chk(
      "`asymptote` must be NULL for a multivariate `brmsfit`; an asymptote ",
      "per response is not supported."
    )
  }
  if (!is.character(asymptote) && !is.numeric(asymptote)) {
    chk::abort_chk("`asymptote` must be character or numeric.")
  }
  chk::chk_not_any_na(asymptote, x_name = "`asymptote`")
  chk::chk_named(asymptote, x_name = "`asymptote`")
  chk::chk_subset(
    names(asymptote),
    directions(),
    x_name = "names of `asymptote`"
  )
  chk::chk_unique(names(asymptote), x_name = "names of `asymptote`")
  if (is.numeric(asymptote) && !all(is.finite(asymptote))) {
    chk::abort_chk("`asymptote` must be finite.")
  }
  if (is.character(asymptote)) {
    nlpars <- names(object$formula$pforms)
    if (!all(asymptote %in% nlpars)) {
      chk::abort_chk(
        "`asymptote` must name non-linear parameters of `object`",
        if (length(nlpars)) {
          paste0(", one of ", chk::cc(nlpars, conj = " or "))
        } else {
          ", which has none"
        },
        ", not ",
        chk::cc(setdiff(asymptote, nlpars), conj = " and "),
        "."
      )
    }
  }
  invisible(asymptote)
}

#' The asymptotes of every curve of a brmsfit
#'
#' A named non-linear parameter is predicted per draw at the first grid value,
#' with group-level effects marginalised as for the curves. `brms` applies the
#' link to the whole non-linear formula, so the parameter is on the link scale
#' and is put on the response scale by the inverse link.
#'
#' @param n_realisation The number of posterior draws.
#'
#' @return `NULL` when `asymptote` is `NULL`, otherwise a list with one element
#'   per curve, each a list of two numeric vectors, `decreasing` and
#'   `increasing`.
#'
#' @noRd
brms_asymptote <- function(
  object,
  asymptote,
  x_vec,
  x_var,
  group_var,
  trials_var,
  n_realisation
) {
  if (is.null(asymptote)) {
    return(NULL)
  }
  linkinv <- if (is.character(asymptote)) brms_linkinv(object)
  groups <- list(NULL)
  if (!is.null(group_var)) {
    groups <- as.list(group_levels(object$data[[group_var]]))
  }
  lapply(groups, function(group) {
    newdata <- brms_newdata(x_vec[1], x_var, group_var, group, trials_var)
    values <- lapply(directions(), function(direction) {
      if (!direction %in% names(asymptote)) {
        return(rep(NA_real_, n_realisation))
      }
      value <- asymptote[[direction]]
      if (is.numeric(value)) {
        return(rep(value, n_realisation))
      }
      linpred <- brms::posterior_linpred(
        object,
        newdata = newdata,
        nlpar = value,
        re_formula = NA
      )
      linkinv(unname(linpred[, 1]))
    })
    stats::setNames(values, directions())
  })
}

#' The inverse link of a brmsfit
#'
#' @noRd
brms_linkinv <- function(object) {
  link <- object$family$link
  tryCatch(
    stats::make.link(link)$linkinv,
    error = function(e) {
      chk::abort_chk(
        "`asymptote` must be numeric for a `brmsfit` with link ",
        chk::cc(link),
        "; a named parameter is on the link scale, and the link has no ",
        "inverse here."
      )
    }
  )
}

#' Split `drc` parameter names into parameter and curve
#'
#' `drc` names its coefficients `<parameter>:<curve>`, using `(Intercept)` as
#' the curve for a fit with a single curve. The split is on the first colon:
#' `drc`'s parameter names (`b`, `c`, `d`, `e`, `f`) contain none, while a curve
#' level comes from the data and may.
#'
#' @param nms Character vector of coefficient names.
#'
#' @return A list with character elements `param` and `curve`.
#'
#' @noRd
drc_split_parm_names <- function(nms) {
  pos <- regexpr(":", nms, fixed = TRUE)
  if (any(pos == -1L)) {
    chk::abort_chk(
      "`coef(object)` must be named <parameter>:<curve>, as `drc` names them."
    )
  }
  list(
    param = substr(nms, 1L, pos - 1L),
    curve = substr(nms, pos + 1L, nchar(nms))
  )
}

#' Evaluate a `drc` mean function over a grid at many parameter draws
#'
#' `object$fct$fct(x, parm)` evaluates the mean function at one parameter
#' vector per element of `x`, so every draw is evaluated in a single call by
#' repeating the grid once per draw and each draw once per grid point.
#'
#' @param object A fitted model of class `drc`.
#' @param x_vec The predictor grid.
#' @param parms A matrix of parameter draws, one row per draw, with columns in
#'   the order the mean function expects.
#'
#' @return A numeric matrix, one row per draw and one column per grid value.
#'
#' @noRd
drc_eval_curve <- function(object, x_vec, parms) {
  n <- nrow(parms)
  nx <- length(x_vec)
  x_rep <- rep(x_vec, times = n)
  draw_rows <- rep(seq_len(n), each = nx)
  y <- object$fct$fct(
    x_rep,
    parms[draw_rows, , drop = FALSE]
  )
  matrix(as.numeric(y), nrow = n, ncol = nx, byrow = TRUE)
}

#' Bootstrap realisations of every curve in a `drc` fit
#'
#' @param object A fitted model of class `drc`.
#' @param x_vec The predictor grid.
#' @param group_var Name of the `curveid` column, or `NULL` for a single curve.
#' @param n_boot Number of bootstrap replicates.
#' @param seed Seed for the draw, or `NULL`.
#'
#' @return A list with `curves`, a list of matrices with one per curve,
#'   `keys`, the [toxval_pred] table identifying them, and `asymptote`, the
#'   output of `drc_asymptote()` for each curve.
#'
#' @noRd
drc_curves <- function(object, x_vec, group_var, n_boot, seed = NULL) {
  parms <- boot_parms(object, n_boot, seed)
  split <- drc_split_parm_names(colnames(parms))

  # `drc` suffixes a parameter with `(Intercept)` when it is shared across every
  # curve, and with the curve otherwise, so a fit with `pmodels` carries both
  # kinds at once
  is_shared <- split$curve == "(Intercept)"
  curve_ids <- unique(split$curve[!is_shared])
  grouped <- length(curve_ids) > 0
  if (!grouped) {
    curve_ids <- "(Intercept)"
  }

  # the curves are recorded in the coefficient names, so they are read from
  # there rather than from a column position (#34)
  if (is.null(group_var) && grouped) {
    chk::abort_chk(
      "`group_var` must be supplied for a `drc` fit with more than one curve; ",
      "`object` has ",
      length(curve_ids),
      " (",
      chk::cc(curve_ids, conj = " and "),
      ")."
    )
  }
  if (!is.null(group_var) && !grouped) {
    chk::abort_chk(
      "`group_var` must be NULL for a `drc` fit with a single curve; ",
      "`object` was not fitted with a `curveid`."
    )
  }

  if (grouped) {
    curveid <- object$dataList$names$cNames
    if (group_var != curveid) {
      chk::abort_chk(
        "`group_var` must be the `curveid` of `object`, not ",
        chk::cc(group_var),
        "."
      )
    }
    # `coef()` orders the curves by row when the `curveid` is a character
    # column, so they are put in the order `brms_curves()` gives its groups
    labels <- as.character(group_levels(object$dataList$curveid))
    curve_ids <- curve_ids[order(match(curve_ids, labels))]
  }

  # one draw of the whole parameter vector per replicate, so the curves stay
  # aligned with each other replicate by replicate
  curve_parms <- lapply(curve_ids, function(id) {
    parms[, drc_curve_cols(split, is_shared, id), drop = FALSE]
  })
  curves <- lapply(curve_parms, function(curve_parm) {
    drc_eval_curve(object, x_vec, curve_parm)
  })
  asymptote <- lapply(curve_parms, function(curve_parm) {
    drc_asymptote(object$fct, curve_parm)
  })

  list(
    curves = curves,
    keys = group_keys(if (grouped) curve_ids),
    asymptote = asymptote
  )
}

#' The asymptotes of a `drc` curve at every parameter draw
#'
#' `drc` gives the limits of its mean function with `fct$lowerAs()` and
#' `fct$upperAs()`, which also return a limit that `fct$fixed` fixes, such as
#' the lower limit of 0 in `LL.3()` (T24). They take one parameter vector:
#' given a matrix, `lowerAs()` returns a wrong value with a warning, so they
#' are applied row by row.
#'
#' @param fct The `fct` of a `drc` fit.
#' @param parms A matrix of parameter draws for one curve, one row per draw.
#'
#' @return A list of two numeric vectors, `decreasing` (the lower limit) and
#'   `increasing` (the upper limit), `NA` where `fct` gives no such limit.
#'
#' @noRd
drc_asymptote <- function(fct, parms) {
  limit <- function(as_fun) {
    if (is.null(as_fun)) {
      return(rep(NA_real_, nrow(parms)))
    }
    unname(apply(parms, 1, function(parm) as.numeric(as_fun(parm))))
  }
  list(decreasing = limit(fct$lowerAs), increasing = limit(fct$upperAs))
}

#' Locate the parameter columns of one curve
#'
#' The mean function takes the parameters in the order `drc` names them, so
#' each curve is assembled by taking its own value of every parameter, or the
#' shared value where it has none.
#'
#' @param split The output of `drc_split_parm_names()`.
#' @param is_shared Logical, which parameters are shared across all curves.
#' @param id The curve to assemble.
#'
#' @return An integer vector of column positions, in mean-function order.
#'
#' @noRd
drc_curve_cols <- function(split, is_shared, id) {
  vapply(
    unique(split$param),
    function(param) {
      found <- which(split$param == param & (split$curve == id | is_shared))
      if (length(found) != 1) {
        chk::abort_chk(
          "`coef(object)` must name parameter ",
          chk::cc(param),
          " exactly once for curve ",
          chk::cc(id),
          ", not ",
          length(found),
          " times."
        )
      }
      found
    },
    integer(1)
  )
}

#' Build the predictor grid
#'
#' The grid starts at the control concentration, so the first value of every
#' curve is the control response. `x_range` sets only the upper end.
#'
#' @param x_range The requested range, or `NULL` to end the grid at `max(x)`.
#' @param x The predictor values in the model data.
#' @param resolution Number of grid values.
#' @param x_control The control concentration, or `NULL` for `min(x)`.
#'
#' @return A numeric vector of length `resolution`.
#'
#' @noRd
predict_grid <- function(x_range, x, resolution, x_control = NULL) {
  data_range <- range(x)
  if (is.null(x_range) && is.null(x_control) && diff(data_range) == 0) {
    chk::abort_chk(
      "`object` must have more than one value of the predictor, or ",
      "`x_range` must be supplied."
    )
  }
  if (is.null(x_control)) {
    x_control <- data_range[1]
  }
  if (!is.null(x_range) && !isTRUE(all.equal(x_range[1], x_control))) {
    chk::abort_chk(
      "`x_range` must start at the control concentration, ",
      format(x_control),
      ", not ",
      format(x_range[1]),
      "; supply `x_control` to name a different control concentration."
    )
  }
  upper <- if (is.null(x_range)) data_range[2] else x_range[2]
  if (x_control >= upper) {
    chk::abort_chk(
      "`x_control` must be below the upper end of the grid, ",
      format(upper),
      ", not ",
      format(x_control),
      "."
    )
  }
  if (x_control < data_range[1] || x_control > data_range[2]) {
    warning(
      "`x_control`, ",
      format(x_control),
      ", lies outside the range of the data, ",
      format(data_range[1]),
      " to ",
      format(data_range[2]),
      ", so the control response is predicted at a concentration that was ",
      "not tested.",
      call. = FALSE
    )
  }
  seq(x_control, upper, length.out = resolution)
}

#' Check that the model is defined over the whole grid
#'
#' A grid value at which no realisation of a curve has a finite prediction lies
#' outside where the model is defined, such as `x <= 0` for a formula with
#' `log(x)`. That is a mismatch between the grid and the model, so it errors
#' (T3) rather than returning `NA` estimates. A non-finite value in only some
#' realisations is left for `find_crossings()`, which skips it.
#'
#' @param curves The list of realisation matrices.
#' @param x_vec The predictor grid.
#'
#' @return `curves`, invisibly.
#'
#' @noRd
chk_defined_grid <- function(curves, x_vec) {
  undefined <- rep(FALSE, length(x_vec))
  for (curve in curves) {
    undefined <- undefined | colSums(is.finite(curve)) == 0
  }
  if (any(undefined)) {
    chk::abort_chk(
      "`object` gives no finite prediction at ",
      sum(undefined),
      " of ",
      length(x_vec),
      " grid values, between ",
      format(min(x_vec[undefined]), digits = 4),
      " and ",
      format(max(x_vec[undefined]), digits = 4),
      "; `x_control` and `x_range` must lie where the model is defined."
    )
  }
  invisible(curves)
}

#' @noRd
chk_x_range <- function(x_range) {
  if (is.null(x_range)) {
    return(invisible(x_range))
  }
  chk::chk_numeric(x_range, x_name = "`x_range`")
  chk::chk_length(x_range, 2L, x_name = "`x_range`")
  chk::chk_not_any_na(x_range, x_name = "`x_range`")
  if (!all(is.finite(x_range))) {
    chk::abort_chk("`x_range` must be finite.")
  }
  if (x_range[1] >= x_range[2]) {
    chk::abort_chk("`x_range` must be increasing.")
  }
  invisible(x_range)
}

#' @noRd
chk_x_control <- function(x_control) {
  if (is.null(x_control)) {
    return(invisible(x_control))
  }
  chk::chk_number(x_control, x_name = "`x_control`")
  if (!is.finite(x_control)) {
    chk::abort_chk("`x_control` must be finite.")
  }
  invisible(x_control)
}

#' Check the type of `x_var` in the generic
#'
#' `NULL` is valid here but not for every method, so the message does not offer
#' it: a caller whose class requires `x_var` should not be told `NULL` will do
#' and then refused it.
#'
#' @param x_var The supplied `x_var`.
#'
#' @return `x_var`, invisibly.
#'
#' @noRd
chk_x_var <- function(x_var) {
  if (!is.null(x_var)) {
    chk::chk_string(x_var)
  }
  invisible(x_var)
}

#' Check that no argument was left in `...`
#'
#' `...` follows `object`, so every other argument must be named in full (T39).
#' A misspelt, abbreviated or unnamed argument lands in `...`, and would
#' otherwise be dropped without a message.
#'
#' @param ... The `...` of a `toxval_predict()` method.
#' @param source The class of the method, for the message.
#'
#' @return `NULL`, invisibly.
#'
#' @noRd
chk_dots_unused <- function(..., source) {
  if (...length() == 0) {
    return(invisible(NULL))
  }
  dot_names <- names(list(...))
  if (is.null(dot_names)) {
    dot_names <- rep("", ...length())
  }
  if (any(dot_names == "")) {
    chk::abort_chk(
      "Arguments after `object` must be named, not passed by position."
    )
  }
  chk::abort_chk(
    "`...` must be unused for a ",
    source,
    ", not ",
    chk::cc(dot_names, conj = " and "),
    "; arguments after `object` must be named in full."
  )
}

#' Check that a method was given the `x_var` it requires
#'
#' @param x_var The supplied `x_var`.
#' @param source The class requiring it, for the message.
#'
#' @return `x_var`, invisibly.
#'
#' @noRd
chk_x_var_supplied <- function(x_var, source) {
  if (is.null(x_var)) {
    chk::abort_chk("`x_var` must be supplied for a ", source, ".")
  }
  invisible(x_var)
}

#' @noRd
chk_var_in_data <- function(var, data, arg) {
  if (!var %in% colnames(data)) {
    chk::abort_chk(
      "`",
      arg,
      "` must name a column of the data in `object`, not ",
      chk::cc(var),
      "."
    )
  }
  invisible(var)
}
