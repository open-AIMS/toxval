# Shared set-up for the bayesnec robustness check (BAYESNEC-claude.md §3.7),
# sourced by every script in this folder. Run the scripts with Rscript from the
# toxval repository root; README.md gives the order.
#
# Environment variables:
# - BAYESNEC_SOURCE: a bayesnec source tree, loaded with pkgload::load_all()
#   in place of the installed bayesnec. The check in §3.7 used an export of
#   predev at 116ea91b (2.1.3.42), so that nothing was installed.
# - TOXVAL_ROBUSTNESS_DIR: where the fits and results are written. The default
#   is in the user cache rather than the repository, because the fits take
#   about 3.3 GB and R CMD build would otherwise copy them into the tarball.

if (
  !file.exists("DESCRIPTION") ||
    read.dcf("DESCRIPTION", "Package")[[1]] != "toxval"
) {
  stop("Run the robustness scripts from the toxval repository root.")
}
bayesnec_source <- Sys.getenv("BAYESNEC_SOURCE")
if (nzchar(bayesnec_source)) {
  suppressMessages(pkgload::load_all(
    bayesnec_source,
    quiet = TRUE,
    export_all = FALSE
  ))
} else if (!requireNamespace("bayesnec", quietly = TRUE)) {
  stop("Install bayesnec, or set BAYESNEC_SOURCE to a bayesnec source tree.")
}
suppressPackageStartupMessages(library(brms))
options(mc.cores = 2)
fit_dir <- Sys.getenv(
  "TOXVAL_ROBUSTNESS_DIR",
  file.path(tools::R_user_dir("toxval", "cache"), "bayesnec-robustness")
)
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
cat(
  "bayesnec",
  getNamespaceVersion("bayesnec"),
  "from",
  if (nzchar(bayesnec_source)) bayesnec_source else "the installed library",
  "| fits in",
  fit_dir,
  "\n"
)

# Fits are kept short: the comparison is of predictions, not of inference.
quick <- list(chains = 2, iter = 1000, warmup = 500, refresh = 0, silent = 2)

# Fits and saves one structure, skipping a name already saved, so that an
# interrupted run can be resumed. A fit that errors is saved as the error,
# because the structures bayesnec refuses are part of the record.
fit_save <- function(name, expr) {
  path <- file.path(fit_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    return(invisible(NULL))
  }
  t <- system.time(fit <- tryCatch(expr, error = function(e) e))
  if (inherits(fit, "error")) {
    cat(name, ": FIT ERROR:", conditionMessage(fit), "\n")
    saveRDS(fit, sub("[.]rds$", ".error.rds", path))
  } else {
    saveRDS(fit, path)
    cat(
      name,
      ":",
      paste(class(fit), collapse = "/"),
      "in",
      round(t[["elapsed"]]),
      "s\n"
    )
  }
}

bnec_q <- function(...) {
  suppressWarnings(suppressMessages(do.call(
    bayesnec::bnec,
    c(list(...), quick)
  )))
}
