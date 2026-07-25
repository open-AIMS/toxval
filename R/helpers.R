#' linear_rescale
#' @param x A numeric vector.
#' @param r_out A numeric vector of length 2 containing
#' the new range of values in x.
#' @return A numeric vector.
#' @noRd
linear_rescale <- function(x, r_out) {
  p <- (x - min(x)) / (max(x) - min(x))
  r_out[[1]] + p * (r_out[[2]] - r_out[[1]])
}

#' @noRd
do_wrapper <- function(..., fct = "cbind") {
  do.call(fct, lapply(...))
}

#' @noRd
#' @importFrom stats median quantile
estimates_summary <- function(x) {
  x <- c(median(x), quantile(x, c(0.025, 0.975)))
  names(x) <- c("Estimate", "Q2.5", "Q97.5")
  x
}

#' allot_class
#'
#' Assigns class to an object.
#'
#' @param x An object.
#' @param new_class The new object class.
#'
#' @return An object of class new_class.
#' @noRd
allot_class <- function(x, new_class) {
  class(x) <- new_class
  x
}

#' #' @noRd
#' expand_and_assign_nec <- function(x, ...) {
#'   allot_class(expand_nec(x, ...), c("bayesnecfit", "bnecfit"))
#' }

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
print_mat <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- x
  for (i in seq_len(ncol(x))) {
    out[, i] <- sprintf(fmt, x[, i])
  }
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

#' @noRd
contains_zero <- function(x) {
  sum(x == 0, na.rm = TRUE) >= 1
}

#' @noRd
contains_one <- function(x) {
  sum(x == 1, na.rm = TRUE) >= 1
}

#' Checks if argument is a \code{\link[bayesnec]{bayesnecfit}} object
#'
#' @param x An \R object
#'
#' @noRd
is_bayesnecfit <- function(x) {
  inherits(x, "bayesnecfit")
}

#' Checks if argument is a \code{\link[bayesnec]{bayesmanecfit}} object
#'
#' @param x An \R object
#'
#' @noRd
is_bayesmanecfit <- function(x) {
  inherits(x, "bayesmanecfit")
}

#' @importFrom bayesnec pull_out bnec_newdata
#' @importFrom stats model.frame
#' 
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
crf <- function(x, model, arg_to_retrieve = "x") {
  mf <- match.call(expand.dots = FALSE)
  if (arg_to_retrieve == "x") {
    m <- match("x", names(mf), 0L)
    deparse(substitute(a, list(a = mf[[m]])))
  } else if (arg_to_retrieve == "model") {
    m <- match("model", names(mf), 0L)
    eval(mf[[m]])
  } else {
    stop("arg_to_retrieve must be either \"x\" or \"model\".")
  }
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
#' @importFrom modelbased zero_crossings
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
#' @importFrom modelbased zero_crossings
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

