#' Extracts the predicted NSEC values from a multivariate brmsfit.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values.
#' against the lowest observed concentration (assumed to be the control), to
#' estimate NEC as an interpolated NOEC value from smooth ECx curves.
#' @param resolution The number of unique x values over which to find NSEC -
#' large values will make the NSEC estimate more precise.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting NSEC.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and credible intervals as defined by prob_vals.
#' @param x_var A character indicating the name of the predictor (x) data in object
#' @param trials_var NA if no trials, or a character vector indicating the name or
#' prefix of any trials column(s).
#' @param multi_var NA if univariate, or a character vector indicating the name or
#' prefix of the multivariate column(s).
#' @param type The type of nsec to be returned. See details.
#' @param criterion The criterion to use when type ='lowest'.
#' @param ... Further arguments to pass to class specific methods.
#'
#' @details nsecID extracts nsec values from response curves of unknown
#' direction or shape. Both increasing and/or decreasing nsec's can be returned.
#' The returned output depends on the selected type, which can be one of 'both',
#' 'lower', 'increasing', 'decreasing'.
#'
#' @return A vector or list containing the estimated NSEC value(s).
#' @export
nsec_multi <- function(object, sig_val = 0.01, resolution = 50,
                 x_range = NA,
                 xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                 posterior = FALSE,
                 x_var, trials_var = NA, multi_var = NA,
                 type = "both", criterion = 0.8, ...) {

  #chk_numeric(sig_val)
  chk_numeric(resolution)
  chk_logical(posterior)

  if (is.na(match(type, c('both', 'lower', 'increasing', 'decreasing')))) {
    stop("type must be one of both, lower, increasing, or decreasing")
  }

  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")
  }
  if(!inherits(xform, "function")) {
    stop("xform must be a function.")}
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (missing(x_var)) {
    stop("x_var must be supplied for a brmsfit object.")
  }

  col_names <- colnames(object$data)
  if(max(grepl(x_var, col_names))==0) {
    stop("Your suplied x_var is not contained in the object data.frame")
  }

  if(is.na(x_range)){
    x_range = range(object$data[x_var])
  }
  x_vec <- seq(min(x_range), max(x_range), length=resolution)

  pred_dat <- data.frame(x_vec)
  names(pred_dat) <- x_var

  if(!is.na(trials_var)) {
    trials <- object$data |> dplyr::select(starts_with(trials_var)) |> colnames()
    if(length(trials)==0) stop("trials_var does not appear to be in your input data.")
    trials_data <- object$data |>
      dplyr::select(all_of(trials)) |>
      unique()

    pred_dat <- cbind(pred_dat, trials_data, row.names = NULL)
  }

  p_samples <- try(posterior_epred(object, newdata = pred_dat, re_formula = NA),
                   silent = TRUE)

  if(!is.na(multi_var)) {
    vars <- object$data |> dplyr::select(starts_with(multi_var)) |> colnames()
    if(length(vars)==0) stop("multi_var does not appear to be in your input data.")

    all_nsec_out <- apply(p_samples, MARGIN = 3, FUN = get_nsec_multi,
                          sig_val = sig_val, x_vec = x_vec, xform = xform)
    names(all_nsec_out) <- vars

    nsec_posterior <- extract_nsec_multi(all_nsec_out,
                                         type = type,
                                         criterion = criterion)

    if(posterior == TRUE) {return(nsec_posterior)} else {
      if(type == "both"){
        nsec_out <- lapply(nsec_posterior, FUN = function(x){
          inc_vals <- quantile(x$nsec_inc, probs=c(0.025, 0.5, 0.975))
          names(inc_vals) <- c("inc_lw", "inc_val", "inc_up")
          dec_vals <- quantile(x$nsec_dec, probs=c(0.025, 0.5, 0.975))
          names(dec_vals) <- c("dec_lw", "dec_val", "dec_up")
          ref_vals <- unlist(attributes(x)$reference_vals)
          return(c(inc_vals, dec_vals, ref_vals))
        }) |> dplyr::bind_rows(.id="vars")
      } else {
        nsec_out <- lapply(nsec_posterior, FUN = function(x){
          vals <- quantile(x, probs=c(0.025, 0.5, 0.975))
          names(vals) <- c("lw", "val", "up")
          ref_val <- unlist(attributes(x)$reference_val)
          direction <- unlist(attributes(x)$direction)
          return(c(vals, ref_val, direction))
        })
        nsec_out <- do.call("rbind", nsec_out) |>
          data.frame()
        colnames(nsec_out) <-   c("lw", "val", "up", "ref", "direction")
        nsec_out$var <- rownames(nsec_out)
        rownames(nsec_out) <- 1:nrow(nsec_out)
      }
    return(nsec_out)
    }

  } else {
    stop("nsec_multi currently only supports multivariate data.")
  }

}
