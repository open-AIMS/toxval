#' @noRd
get_nsec_multi <- function(a, sig_val, x_vec, xform) {
  reference_dec <- quantile(a[, 1], sig_val)
  nsec_out_dec <- xform(apply(
    a,
    1,
    nsec_fct,
    reference = reference_dec,
    x_vec = x_vec
  ))
  reference_inc <- quantile(a[, 1], 1 - sig_val)
  nsec_out_inc <- xform(apply(
    a,
    1,
    nsec_fct,
    reference = reference_inc,
    x_vec = x_vec
  ))
  nsec_out <- list(nsec_dec = nsec_out_dec, nsec_inc = nsec_out_inc)
  attr(nsec_out, "reference_vals") <- list(
    dec = reference_dec,
    inc = reference_inc
  )
  return(nsec_out)
}

#' @noRd
extract_nsec_multi <- function(all_nsec_out, type, criterion) {
  if (type == "both") {
    nsec_out <- all_nsec_out
  }

  if (type == "lower") {
    nsec_out <- lapply(all_nsec_out, FUN = function(x) {
      up.inc <- quantile(x$nsec_inc, probs = criterion)
      up.dec <- quantile(x$nsec_dec, probs = criterion)
      if (up.inc < up.dec) {
        nsec_use <- x$nsec_inc
        attr(nsec_use, "direction") <- "inc"
        attr(nsec_use, "reference_vals") <- as.numeric(
          attributes(x)$reference_vals$inc
        )
      }
      if (up.dec <= up.inc) {
        nsec_use <- x$nsec_dec
        attr(nsec_use, "direction") <- "dec"
        attr(nsec_use, "reference_vals") <- as.numeric(
          attributes(x)$reference_vals$dec
        )
      }
      nsec_use
    })
  }

  if (type == "increasing") {
    nsec_out <- lapply(all_nsec_out, FUN = function(x) {
      out <- x$nsec_inc
      attr(out, "reference_vals") <- as.numeric(
        attributes(x)$reference_vals$inc
      )
      attr(out, "direction") <- "inc"
      return(out)
    })
  }

  if (type == "decreasing") {
    nsec_out <- lapply(all_nsec_out, FUN = function(x) {
      out <- x$nsec_dec
      attr(out, "reference_vals") <- as.numeric(
        attributes(x)$reference_vals$dec
      )
      attr(out, "direction") <- "dec"
      return(out)
    })
  }
  attr(nsec_out, "type") <- type
  return(nsec_out)
}


#' @noRd
nsec_fct <- function(y, reference, x_vec) {
  val <- min(modelbased::zero_crossings(y - reference))
  if (is.na(val)) {
    return(max(x_vec))
  } else {
    floor_x <- x_vec[floor(val)]
    ceiling_x <- x_vec[ceiling(val)]
    prop_x <- (val - floor(val)) * (ceiling_x - floor_x)
    return(floor_x + prop_x)
  }
}
