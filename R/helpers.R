#' @noRd
do_wrapper <- function(..., fct = "cbind") {
  do.call(fct, lapply(...))
}

#' @noRd
clean_names <- function(x) {
  paste0("Q", gsub("%", "", names(x), fixed = TRUE))
}

#' @noRd
modify_posterior <- function(n, object, x_vec, p_samples, hormesis_def) {
  posterior_sample <- p_samples[n, ]
  if (hormesis_def == "max") {
    target <- x_vec[which.max(posterior_sample)]
    change <- x_vec < target
  posterior_sample[change] <- NA    
  } 
  posterior_sample
}

#' @noRd
newdata_eval <- function(object, resolution, x_range) {
  # Just need one model to extract and generate data
  # since all models are considered to have the exact same raw data.
  if (inherits(object, "bayesmanecfit")) {
    model_set <- names(object$mod_fits)
    object <- suppressMessages(pull_out(object, model = model_set[1]))
  }
  data <- model.frame(object$bayesnecformula, object$fit$data)
  bnec_pop_vars <- attr(data, "bnec_pop")
  newdata <- bnec_newdata(object, resolution = resolution, x_range = x_range)
  x_vec <- newdata[[bnec_pop_vars[["x_var"]]]]
  list(newdata = newdata, x_vec = x_vec)
}

#' @noRd
get_nsec_multi <- function(a, sig_val, x_vec, xform) {
  reference_dec <- quantile(a[, 1], sig_val)
  nsec_out_dec <- xform(apply(a, 1, nsec_fct, reference=reference_dec, x_vec=x_vec))         
  reference_inc <- quantile(a[, 1], 1-sig_val)
  nsec_out_inc <- xform(apply(a, 1, nsec_fct, reference=reference_inc, x_vec=x_vec))
  nsec_out <- list(nsec_dec = nsec_out_dec, nsec_inc = nsec_out_inc)
  attr(nsec_out, "reference_vals") <- list(dec=reference_dec, inc=reference_inc)
  return(nsec_out)
}

#' @noRd
extract_nsec_multi <- function(all_nsec_out, type, criterion){
  if(type == "both") {
    nsec_out <- all_nsec_out
  }
  
  if(type == "lower") {
    nsec_out <- lapply(all_nsec_out, FUN = function(x){
      up.inc <- quantile(x$nsec_inc, probs = criterion)
      up.dec <- quantile(x$nsec_dec, probs = criterion)
      if(up.inc<up.dec){
        nsec_use <- x$nsec_inc
        attr(nsec_use, "direction") <- "inc" 
        attr(nsec_use, "reference_vals") <- as.numeric(attributes(x)$reference_vals$inc)
      }
      if(up.dec<=up.inc){
        nsec_use <- x$nsec_dec
        attr(nsec_use, "direction") <- "dec" 
        attr(nsec_use, "reference_vals") <- as.numeric(attributes(x)$reference_vals$dec)
      }
      nsec_use
    })      
  }
  
  if(type == "increasing") {   
    nsec_out <- lapply(all_nsec_out, FUN = function(x){
      out <- x$nsec_inc
      attr(out, "reference_vals") <- as.numeric(attributes(x)$reference_vals$inc)
      attr(out, "direction") <- "inc"
    return(out)
    })
  }
  
  if(type == "decreasing") {   
    nsec_out <- lapply(all_nsec_out, FUN = function(x){
      out <- x$nsec_dec
      attr(out, "reference_vals") <- as.numeric(attributes(x)$reference_vals$dec)  
      attr(out, "direction") <- "dec"
    return(out)
    })

  }
  attr(nsec_out, "type") <- type
  return(nsec_out)
}


#' @noRd
nsec_fct <- function(y, reference, x_vec) {
  val <- min(zero_crossings(y - reference))
  if(is.na(val)) {
    return(max(x_vec))} else {
      floor_x <-  x_vec[floor(val)] 
      ceiling_x <- x_vec[ceiling(val)]
      prop_x <- (val-floor(val))*(ceiling_x-floor_x)
      return(floor_x + prop_x)
    }
}

#' @noRd
tox_fct <- function(y, reference, x_vec) {
  val <- min(zero_crossings(y - reference))
  if(is.na(val)) {
    return(NA)} else {
      floor_x <-  x_vec[floor(val)] 
      ceiling_x <- x_vec[ceiling(val)]
      prop_x <- (val-floor(val))*(ceiling_x-floor_x)
      return(floor_x + prop_x)
    }
}
