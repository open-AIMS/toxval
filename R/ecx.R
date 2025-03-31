#' Extracts the predicted ECx value as desired from a supported class.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param ecx_val The desired percentage effect value. This must be a value
#' between 1 and 99 (for type = "relative" and "absolute"), defaults to 10.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "relative", "absolute" (the default) or "direct". See Details.
#' @param resolution The number of unique x values over which to find ECx --
#' large values will make the ECx estimate more precise.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECx values should be returned instead of
#' just the median and 95 credible intervals.
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values
#' of "max" or "control". See Details.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated ECx value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#'
#' @details \code{type} "relative" is calculated as the percentage decrease
#' from the maximum predicted value of the response (top) to the minimum
#' predicted value of the response. Type "absolute" (the default) is
#' calculated as the percentage decrease from the maximum value of the
#' response (top) to 0. Type "direct"
#' provides a direct estimate of the x value for a given y.
#' Note that for the current version, ECx for an "nechorme" (NEC Hormesis)
#' model is estimated at a percent decline from the control.
#' 
#' For \code{hormesis_def}, if "max", then ECx values are calculated as a
#' decline from the maximum estimates (i.e. the peak at NEC);
#' if "control", then ECx values are calculated relative to the control, which
#' is assumed to be the lowest observed concentration.
#' 
#' Calls to functions \code{\link{ecx}} and \code{\link{nsec}} and
#' \code{\link{compare_fitted}} do not require the same level of flexibility
#' in the context of allowing argument \code{newdata}
#' (from a \code{\link[brms]{posterior_predict}} perspective) to
#' be supplied manually, as this is and should be handled within the function
#' itself. The argument \code{resolution} controls how precisely the
#' \code{\link{ecx}} or \code{\link{nsec}} value is estimated, with 
#' argument \code{x_range} allowing estimation beyond the existing range of
#' the observed data (otherwise the default range) which can be useful in a
#' small number of cases. There is also no reasonable case where estimating
#' these from the raw data would be of value, because both functions would
#' simply return one of the treatment concentrations, making NOEC a better
#' metric in that case.
#'
#' @return A vector containing the estimated ECx value, including upper and
#' lower 95% credible interval bounds.
#'
#' @examples
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' data(manec_example)
#' ecx(manec_example, ecx_val = 50)
#' ecx(manec_example)
#' }
#'
#' @export
ecx <- function(object, ecx_val = 10, resolution = 1000,
                posterior = FALSE, type = "absolute",
                hormesis_def = "control", x_range = NA,
                xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...) {
  
  chk_numeric(resolution)  
  chk_logical(posterior)
  if ((type %in% c("relative", "absolute", "direct")) == FALSE) {
    stop("type must be one of 'relative', 'absolute' (the default) or 'direct'. 
         Please see ?ecx for more details.")
  }
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of 'max' or 'control' (the default). 
         Please see ?ecx for more details.")
  }
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  
  UseMethod("ecx")
}

#' @inheritParams ecx
#'
#' @inherit ecx details return seealso examples
#'
#' @param object An object of class \code{\link{bnecfit}} returned by
#' \code{\link{bnec}}.
#' 
#' @importFrom stats quantile
#' @importFrom brms posterior_epred
#' @importFrom chk chk_logical chk_numeric
#'
#' @noRd
#'
#' @export
ecx.bnecfit <- function(object, ecx_val = 10, resolution = 100,
                         posterior = FALSE, type = "absolute",
                         hormesis_def = "control", x_range = NA,
                         xform = identity,
                         prob_vals = c(0.5, 0.025, 0.975)) {
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  p_samples <- posterior_epred(object, newdata = newdata_list$newdata,
                               re_formula = NA)
  x_vec <- newdata_list$x_vec
  
  if (hormesis_def == "max") {
    control_posterior <- quantile(apply(p_samples, 1, max), probs = 0.5)
  } else {
    control_posterior <- p_samples[, 1]
  }
  
  if(type=="relative"){  
    min_posterior <- p_samples[, ncol(p_samples)]
  } else {
    min_posterior <- 0   
  }
  
  dif_valsC <- control_posterior-min_posterior

  n <- seq_len(nrow(p_samples))
  p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                          p_samples, hormesis_def, fct = "rbind")

  reference <- median(control_posterior) - (median(dif_valsC) * (ecx_val / 100))
  names(reference) <- ecx_val

  tox_out <- do.call("cbind", lapply(reference, FUN = function(r){
    apply(p_samples, 1, tox_fct, r, x_vec)  
  })) 

  tox_out <- xform(tox_out)
    
  tox_estimate <- apply(tox_out, MARGIN = 2, FUN = quantile, 
                        probs = prob_vals, na.rm = TRUE)
    #quantile(unlist(tox_out), probs = prob_vals)
    #names(tox_estimate) <- clean_names(tox_estimate)
  
  attr(tox_estimate, "control_value") <- median(control_posterior)
  attr(tox_out, "control_value") <-  median(control_posterior)
  attr(tox_estimate, "reference") <- reference
  attr(tox_out, "reference") <-  reference
  attr(tox_estimate, "resolution") <- resolution
  attr(tox_out, "resolution") <- resolution
  
  if (!posterior) {
    tox_estimate
  } else {
    tox_out
  }
  
}

#' @noRd
ecx_x_relative <- function(y, ecx_val, x_vec, hormesis_def) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    
    if(hormesis_def=="max") {
      range_y <- c(y[1], min(y, na.rm = TRUE))
      ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)      
    }
    
    if(hormesis_def=="control") {
      range_y <- range(y, na.rm = TRUE)
      ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)      
    }
    
    val <- min(zero_crossings(y - ecx_y))
    if(is.na(val)) {
      outval <- max(x_vec)
      } else {
        floor_x <-  x_vec[floor(val)] 
        ceiling_x <- x_vec[ceiling(val)]
        prop_x <- (val-floor(val))*(ceiling_x-floor_x)
        outval <- floor_x + prop_x
      }
  }
  outval
}

#' @noRd
ecx_x_absolute <- function(y, ecx_val, x_vec, hormesis_def) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    
    if(hormesis_def=="max") {    
      range_y <- c(0, max(y, na.rm = TRUE))
      ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
    }
    
    if(hormesis_def=="control") {  
      range_y <- c(0, y[1])
      ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
    }

    val <- min(zero_crossings(y - ecx_y))
    if(is.na(val)) {
      outval <- max(x_vec)
    } else {
      floor_x <-  x_vec[floor(val)] 
      ceiling_x <- x_vec[ceiling(val)]
      prop_x <- (val-floor(val))*(ceiling_x-floor_x)
      outval <- floor_x + prop_x
    }
  }
  outval
}

#' @noRd
ecx_x_direct <- function(y, ecx_val, x_vec, hormesis_def) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    ecx_y <- ecx_val

    val <- min(zero_crossings(y - ecx_y))
    if(is.na(val)) {
      outval <- max(x_vec)
    } else {
      floor_x <-  x_vec[floor(val)] 
      ceiling_x <- x_vec[ceiling(val)]
      prop_x <- (val-floor(val))*(ceiling_x-floor_x)
      outval <- floor_x + prop_x
    }
  }
  outval
}

#' @inheritParams ecx
#'
#' @param object An object of class \code{\link{brmsfit}} returned by
#' \code{\link{brms}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ecx values should be returned instead of
#' just the median and 95 credible intervals.
#' @param x_var A character indicating the name of the predictor (x) data in object
#' @param group_var A character indicating the name of the grouping variable in object
#' @param by_group A logical indicating if ecx values should be returned for 
#' each level in group_var, or marginalised across all groups.
#' @param horme Logical indicating if hormesis is evident.
#' 
#' @importFrom stats quantile
#' @importFrom chk chk_logical chk_numeric
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom brms as_draws_df posterior_epred
#' @importFrom tidyr pivot_longer everything
#'
#' @noRd
#'
#' @export
ecx.brmsfit <- function(object, ecx_val = 10, resolution = 1000,
                            posterior = FALSE, type = "absolute",
                            hormesis_def = "control", x_range = NA,
                            xform = identity,
                            prob_vals = c(0.5, 0.025, 0.975), 
                            x_var, 
                            group_var = NA, 
                            by_group = FALSE,
                            horme = FALSE) {
  chk_numeric(ecx_val)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  
  if (type != "direct") {
    if (ecx_val < 1 || ecx_val > 99) {
      stop("Supplied ecx_val is not in the required range. ",
           "Please supply a percentage value between 1 and 99.")
    }
  } 

  if (missing(x_var)) {
    stop("x_var must be supplied for a brmsfit object.")    
  }  
  if (by_group & is.na(group_var)){
    stop("You must specify a group_by variable if you want values returned by groups.")
  }
  
  col_names <- colnames(object$data)
  if(max(grepl(x_var, col_names))==0) {
    stop("Your suplied x_var is not contained in the object data.frame")
  }
  if(!is.na(group_var)){
    if(max(grepl(group_var, col_names))==0) {
      stop("Your suplied group_var is not contained in the object data.frame")
    }     
  }
  
  if(is.na(x_range)){
    x_range = range(object$data[x_var])
  }
  x_vec <- seq(min(x_range), max(x_range), length=resolution)
  
  
  if(is.na(group_var)){
    pred_dat <- data.frame(x_vec)
    names(pred_dat) <- x_var

    p_samples <- posterior_epred(object, newdata = pred_dat,
                                 re_formula = NA)
    if (class(p_samples)[1] == "try-error"){
      stop(paste(attributes(p_samples)$condition, "Do you need to specify a group_var variable?", sep=""))
    }

    ecx_fct <- get(paste0("ecx_x_", type))
    ecx_out <- apply(p_samples, 1, ecx_fct, ecx_val, x_vec, hormesis_def)
    
    if (inherits(xform, "function")) {
      ecx_out <- xform(ecx_out)
    } 
    
    
  } else {
    
    groups <-  unlist(unique(object$data[group_var]))
    out_vals <- lapply(groups, FUN = function(g){
      dat_list <- list(x_vec, g) 
      names(dat_list) <- c(x_var, group_var)
      pred_dat <- expand.grid(dat_list)
      
      p_samples <- posterior_epred(object, newdata = pred_dat,
                                   re_formula = NA)
      # if (grepl("horme", object$model)) {
      #   n <- seq_len(nrow(p_samples))
      #   p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
      #                           p_samples, hormesis_def, fct = "rbind")
      # }
      ecx_fct <- get(paste0("ecx_x_", type))
      ecx_out <- apply(p_samples, 1, ecx_fct, ecx_val, x_vec, hormesis_def)      
      if (inherits(xform, "function")) {
        ecx_out <- xform(ecx_out)
      } 
    }) 
    
  }
  
  if(by_group & posterior & !is.na(group_var)){
    names(out_vals) <- groups
    out_vals <- out_vals |> bind_cols() |> 
      pivot_longer(everything(), names_to = group_var, values_to = "ECx")
  }
  
  if(by_group & !posterior & !is.na(group_var)){   
    names(out_vals) <- groups
    out_vals <- lapply(out_vals, quantile, probs = prob_vals) |> 
      bind_rows(.id = group_var)
    names(out_vals) <- clean_names(out_vals)
  }
  
  if(!by_group & posterior & !is.na(group_var)){
    out_vals <- as.numeric((unlist(out_vals)))
  }
  
  if(!by_group & !posterior & !is.na(group_var)){   
    out_vals <- quantile(unlist(out_vals), probs = prob_vals)
    names(out_vals) <- clean_names(out_vals)
  }
  
  if(posterior & is.na(group_var)){ 
    out_vals <- unlist(ecx_out)
  }
  
  if(!posterior & is.na(group_var)){   
    out_vals <- quantile(unlist(ecx_out), probs = prob_vals)
    names(out_vals) <- clean_names(out_vals)
  }
  
  attr(out_vals, "resolution") <- resolution
  attr(out_vals, "ecx_val") <- ecx_val
  attr(out_vals, "toxicity_estimate") <- "ecx"
  
  return(out_vals)

}


