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
#' marginalised across levels. For a `brmsfit` it names a column of the model
#' data; for a `drc` fit it names the column passed as `curveid`, and the levels
#' are read from the coefficient names rather than from a column position.
#'
#' @param object A fitted model object. Methods are provided for [brms::brmsfit]
#'   and `drc` fits.
#' @param x_var A string naming the predictor variable in `object`, or `NULL`
#'   where the class of `object` records which of its variables is the
#'   predictor, for a method that reads it from there. Both methods in this
#'   package require a string.
#' @param group_var A string naming the grouping variable in `object`, or `NULL`
#'   for a single curve.
#' @param resolution A whole number giving the number of predictor values to
#'   evaluate the curve at. Larger values estimate a metric more precisely.
#' @param x_range A numeric vector of length 2 giving the range of predictor
#'   values to evaluate over, or `NULL` to use the range in the model data.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class [toxval_pred].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("drc", quietly = TRUE)) {
#'   fit <- drc::drm(y ~ x, data = bayesnec::nec_data, fct = drc::LL.4())
#'   toxval_predict(fit, x_var = "x", resolution = 50, n_boot = 100, seed = 42)
#' }
#' }
#'
#' @export
toxval_predict <- function(
  object,
  x_var = NULL,
  group_var = NULL,
  resolution = 1000,
  x_range = NULL,
  ...
) {
  chk_x_var(x_var)
  chk::chk_null_or(group_var, vld = chk::vld_string)
  chk::chk_whole_number(resolution)
  chk::chk_gt(resolution, 1)
  chk_x_range(x_range)

  UseMethod("toxval_predict")
}

#' @describeIn toxval_predict Method for any class without one, which errors.
#'
#' @export
toxval_predict.default <- function(
  object,
  x_var = NULL,
  group_var = NULL,
  resolution = 1000,
  x_range = NULL,
  ...
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
#'   effects are marginalised out (`re_formula = NA`).
#'
#' @export
toxval_predict.brmsfit <- function(
  object,
  x_var = NULL,
  group_var = NULL,
  resolution = 1000,
  x_range = NULL,
  ...
) {
  # EXPECTED-CHANGE multivariate brmsfit support, one curve per response
  # Removing this guard is not enough on its own: `brms_curves()` returns a
  # single curve, and `meta$family` holds one string where such a fit has a
  # family per response. The test itself stays, as the branch selector.
  if (brms::is.mvbrmsformula(object$formula)) {
    chk::abort_chk(
      "`object` must not be a multivariate `brmsfit`; one curve per response ",
      "is not yet supported by `toxval_predict()`."
    )
  }

  chk_x_var_supplied(x_var, "`brmsfit`")
  data <- object$data
  chk_var_in_data(x_var, data, "x_var")
  if (!is.null(group_var)) {
    chk_var_in_data(group_var, data, "group_var")
  }

  x_vec <- predict_grid(x_range, data[[x_var]], resolution)
  curves <- brms_curves(object, x_vec, x_var, group_var)

  new_toxval_pred(
    curves = curves,
    x_vec = x_vec,
    meta = build_meta(
      object = object,
      x_vec = x_vec,
      x_var = x_var,
      curves = curves,
      group_var = group_var,
      realisation = "draws",
      family = object$family$family
    )
  )
}

#' @describeIn toxval_predict Method for a `drc` fit returned by [drc::drm()].
#'   Realisations are parametric bootstrap replicates, so results are stochastic
#'   unless `seed` is supplied.
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
  x_var = NULL,
  group_var = NULL,
  resolution = 1000,
  x_range = NULL,
  n_boot = 1000,
  seed = NULL,
  ...
) {
  chk::chk_whole_number(n_boot)
  chk::chk_gt(n_boot, 0)
  chk::chk_null_or(seed, vld = chk::vld_whole_number)

  # EXPECTED-CHANGE Delete comment at end of refactoring
  # Comment about choosing to supply x_var instead of deriving it
  # This is one of the model types that can derive it themselves, but this is safer
  # `object$dataList$names$dName` records the predictor, so this method could
  # read it rather than require it
  chk_x_var_supplied(x_var, "`drc` fit")
  data <- object$data
  chk_var_in_data(x_var, data, "x_var")
  if (!is.null(group_var)) {
    chk_var_in_data(group_var, data, "group_var")
  }

  x_vec <- predict_grid(x_range, data[[x_var]], resolution)
  curves <- drc_curves(object, x_vec, group_var, n_boot, seed)

  new_toxval_pred(
    curves = curves,
    x_vec = x_vec,
    meta = build_meta(
      object = object,
      x_vec = x_vec,
      x_var = x_var,
      curves = curves,
      group_var = group_var,
      realisation = "bootstrap",
      seed = seed
    )
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
#' @param realisation Either `"draws"` or `"bootstrap"`.
#' @param family The response distribution, or `NULL` where there is none.
#' @param seed The bootstrap seed, or `NULL`.
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
  realisation,
  family = NULL,
  seed = NULL
) {
  # EXPECTED-CHANGE multivariate brmsfit support, one curve per response
  # The one place that assumes every supported fit is univariate: such a fit
  # keys its curves by response, so it would supply `multi_var` and take
  # `dimension` from that rather than from `group_var`.
  list(
    source_class = class(object)[1],
    x_var = x_var,
    group_var = group_var,
    multi_var = NULL,
    dimension = if (is.null(group_var)) "none" else "group",
    resolution = length(x_vec),
    x_range = range(x_vec),
    family = family,
    realisation = realisation,
    n_realisation = nrow(curves[[1]]),
    seed = seed
  )
}

#' Posterior draws of every curve in a brmsfit
#'
#' Returns the `curves` list, named by group and `NULL`-named when ungrouped,
#' following the [toxval_pred] naming rule.
#'
#' EXPECTED-CHANGE multivariate brmsfit support, one curve per response
#' A multivariate fit adds a third branch here, keyed by the response names
#' `posterior_epred()` returns in `dimnames(.)[[3]]`.
#'
#' @param object A fitted model of class [brms::brmsfit].
#' @param x_vec The predictor grid.
#' @param x_var Name of the predictor variable.
#' @param group_var Name of the grouping variable, or `NULL`.
#'
#' @return A list of matrices, one per curve.
#'
#' @noRd
brms_curves <- function(object, x_vec, x_var, group_var) {
  if (is.null(group_var)) {
    return(list(epred_curve(object, x_vec, x_var)))
  }
  groups <- group_levels(object$data[[group_var]])
  curves <- lapply(seq_along(groups), function(i) {
    epred_curve(object, x_vec, x_var, group_var, groups[i])
  })
  names(curves) <- as.character(groups)
  curves
}

#' The levels of a grouping variable, in a stable order
#'
#' The curve names become the group descriptor of the result, so they are
#' ordered by the content of the variable rather than by the order its rows
#' happen to be in: re-sorting the input data must not re-order the output.
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
  sort(unique(x))
}

#' Posterior draws of the mean curve over a grid
#'
#' @param object A fitted model of class [brms::brmsfit].
#' @param x_vec The predictor grid.
#' @param x_var Name of the predictor variable.
#' @param group_var Name of the grouping variable, or `NULL`.
#' @param group A single level of the grouping variable.
#'
#' @return A numeric matrix, one row per draw and one column per grid value.
#'
#' @noRd
epred_curve <- function(object, x_vec, x_var, group_var = NULL, group = NULL) {
  newdata <- stats::setNames(data.frame(x_vec), x_var)
  if (!is.null(group_var)) {
    newdata[[group_var]] <- group
  }
  posterior_epred(object, newdata = newdata, re_formula = NA)
}

#' Split `drc` parameter names into parameter and curve
#'
#' `drc` names its coefficients `<parameter>:<curve>`, using `(Intercept)` as
#' the curve for a fit with a single curve. The split is on the last colon,
#' because a curve level may itself contain one.
#'
#' @param nms Character vector of coefficient names.
#'
#' @return A list with character elements `param` and `curve`.
#'
#' @noRd
drc_split_parm_names <- function(nms) {
  pos <- regexpr(":[^:]*$", nms)
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
#' @return A list of matrices, named by curve and `NULL`-named for a single
#'   curve, following the `toxval_pred` naming rule.
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

  curves <- lapply(curve_ids, function(id) {
    # one draw of the whole parameter vector per replicate, so the curves stay
    # aligned with each other replicate by replicate
    drc_eval_curve(
      object,
      x_vec,
      parms[, drc_curve_cols(split, is_shared, id), drop = FALSE]
    )
  })

  if (grouped) {
    names(curves) <- curve_ids
  }
  curves
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
#' @param x_range The requested range, or `NULL` to take it from `x`.
#' @param x The predictor values in the model data.
#' @param resolution Number of grid values.
#'
#' @return A numeric vector of length `resolution`.
#'
#' @noRd
predict_grid <- function(x_range, x, resolution) {
  if (is.null(x_range)) {
    x_range <- range(x)
    if (x_range[1] == x_range[2]) {
      chk::abort_chk(
        "`object` must have more than one value of the predictor, or ",
        "`x_range` must be supplied."
      )
    }
  }
  seq(x_range[1], x_range[2], length.out = resolution)
}

#' @noRd
chk_x_range <- function(x_range) {
  if (is.null(x_range)) {
    return(invisible(x_range))
  }
  chk::chk_numeric(x_range, x_name = "`x_range`")
  # EXPECTED-CHANGE Remove this comment after refactor is done
  # length is checked before anything that would be applied element-wise, so a
  # length-2 `x_range` never reaches a scalar test (see `is.na(x_range)` in the
  # pre-refactor entry points)
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
  # EXPECTED-CHANGE delete comment around phase 5
  # Matched with `%in%` rather than the `max(grepl(var, col_names)) == 0` still
  # used by `nsec.brmsfit()`, `nsec_multi()` and `ecx.brmsfit()`, so
  # `x_var = "x"` does not match a column called `max_x`.
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
