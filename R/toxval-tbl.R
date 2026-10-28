#' Build a toxval tibble
#'
#' Every metric function returns a `toxval` tibble, whatever the class of the
#' fitted model. The run settings come from the `meta` of the `toxval_pred` the
#' metric was computed on, and are printed as header lines by `tbl_sum()`. They
#' are for display only: anything a caller might compute on is a column.
#'
#' @param x A data frame with the core columns `metric`, `direction`,
#'   `estimate`, `conf.low` and `conf.high`, and any descriptor columns.
#' @param meta The `meta` of the `toxval_pred` the metric was computed on.
#'
#' @return An object of class `toxval`.
#'
#' @noRd
new_toxval <- function(x, meta) {
  chk::chk_data(x)
  chk::chk_list(meta)
  x <- tibble::new_tibble(
    x,
    settings = meta[settings_required()],
    class = "toxval"
  )
  validate_toxval(x)
  x
}

#' Check that a toxval tibble satisfies its column contract
#'
#' @param x An object of class `toxval`.
#'
#' @return `x`, invisibly if valid; an error otherwise.
#'
#' @noRd
validate_toxval <- function(x) {
  chk::chk_s3_class(x, "toxval")
  chk::chk_s3_class(x, "tbl_df")
  chk::chk_superset(names(x), core_columns(), x_name = "names of `x`")

  chk::chk_character(x$metric, x_name = "`metric`")
  chk::chk_subset(x$metric, metric_vocabulary(), x_name = "`metric`")
  chk::chk_character(x$direction, x_name = "`direction`")
  chk::chk_subset(x$direction, directions(), x_name = "`direction`")
  for (col in c("estimate", "conf.low", "conf.high")) {
    chk::chk_numeric(x[[col]], x_name = paste0("`", col, "`"))
  }

  chk_toxval_settings(attr(x, "settings"))
  invisible(x)
}

#' @noRd
core_columns <- function() {
  c("metric", "direction", "estimate", "conf.low", "conf.high")
}

#' The values `metric` may take
#'
#' A new metric is added here when it is implemented.
#'
#' @noRd
metric_vocabulary <- function() {
  c("ecx", "nsec")
}

#' @noRd
settings_required <- function() {
  c("source_class", "resolution", "realisation", "n_realisation")
}

#' @noRd
chk_toxval_settings <- function(settings) {
  chk::chk_list(settings, x_name = "`settings`")
  chk::chk_superset(
    names(settings),
    settings_required(),
    x_name = "names of `settings`"
  )
  chk::chk_string(settings$source_class, x_name = "`settings$source_class`")
  chk::chk_whole_number(settings$resolution, x_name = "`settings$resolution`")
  chk::chk_gt(settings$resolution, 1, x_name = "`settings$resolution`")
  chk::chk_string(settings$realisation, x_name = "`settings$realisation`")
  chk::chk_subset(
    settings$realisation,
    c("draws", "bootstrap"),
    x_name = "`settings$realisation`"
  )
  chk::chk_whole_number(
    settings$n_realisation,
    x_name = "`settings$n_realisation`"
  )
  chk::chk_gt(settings$n_realisation, 0, x_name = "`settings$n_realisation`")
  invisible(settings)
}

#' Header lines of a toxval tibble
#'
#' Adds the model class and the run settings below the dimensions line, in the
#' way `# Groups:` appears on a grouped tibble.
#'
#' @param x An object of class `toxval`.
#' @param ... Unused.
#'
#' @return A named character vector, one element per header line.
#'
#' @importFrom pillar tbl_sum
#' @export
#' @noRd
tbl_sum.toxval <- function(x, ...) {
  settings <- attr(x, "settings")
  c(
    NextMethod(),
    Model = settings$source_class,
    Settings = paste0(
      "resolution ",
      settings$resolution,
      " | ",
      format_realisations(settings$n_realisation, settings$realisation)
    )
  )
}

#' Combine toxval tibbles
#'
#' The header describes every row, so toxval tibbles combine, as in
#' `dplyr::bind_rows()`, only when their run settings are identical. Otherwise
#' `bind_rows()` would keep the settings of the first and print them above rows
#' they do not describe; `dplyr_reconstruct()` cannot prevent this, since it
#' sees only the first tibble.
#'
#' @param x,y,to Objects of class `toxval`.
#' @param ...,x_arg,y_arg Passed on by vctrs.
#'
#' @return The common type, or `x` cast to `to`.
#'
#' @importFrom vctrs vec_ptype2
#' @export
#' @noRd
vec_ptype2.toxval.toxval <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!identical(attr(x, "settings"), attr(y, "settings"))) {
    vctrs::stop_incompatible_type(
      x,
      y,
      ...,
      x_arg = x_arg,
      y_arg = y_arg,
      details = paste0(
        "Their run settings differ, so one header cannot describe every row. ",
        "Convert each with `tibble::as_tibble()` before combining."
      )
    )
  }
  tibble::new_tibble(
    vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg),
    settings = attr(x, "settings"),
    class = "toxval"
  )
}

#' @importFrom vctrs vec_cast
#' @export
#' @noRd
vec_cast.toxval.toxval <- function(x, to, ...) {
  tibble::new_tibble(
    vctrs::tib_cast(x, to, ...),
    settings = attr(to, "settings"),
    class = "toxval"
  )
}

#' @noRd
format_realisations <- function(n, realisation) {
  label <- switch(
    realisation,
    draws = c("posterior draw", "posterior draws"),
    bootstrap = c("bootstrap realisation", "bootstrap realisations")
  )
  paste(n, if (n == 1) label[1] else label[2])
}
