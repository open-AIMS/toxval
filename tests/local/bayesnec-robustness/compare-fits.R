# Compare toxval_predict() with bayesnec's own posterior_epred() on every
# stored robustness fit, and run the ECx and NSEC computations on the result.
#
# With no arguments every fit is compared and the table is saved as
# compare-all.rds in the fit folder; arguments name the fits to compare.
source("tests/local/bayesnec-robustness/common.R")
suppressMessages(pkgload::load_all(".", quiet = TRUE))
resolution <- 50
first_line <- function(e) strsplit(conditionMessage(e), "\n")[[1]][1]
compare_one <- function(name, fit) {
  row <- list(
    fit = name,
    class = class(fit)[1],
    toxval = NA,
    n = NA,
    vs_bayesnec = NA,
    ecx = NA,
    nsec = NA
  )
  pred <- tryCatch(
    toxval_predict(fit, resolution = resolution),
    error = identity
  )
  if (inherits(pred, "error")) {
    row$toxval <- paste("ERROR:", first_line(pred))
    return(row)
  }
  row$toxval <- "ok"
  row$n <- pred$meta$n_realisation
  bn <- tryCatch(
    # bayesnec's own methods do not set re_formula, while its curve
    # predictions (expand_nec) use re_formula = NA, as toxval does
    brms::posterior_epred(
      fit,
      newdata = bayesnec::bnec_newdata(fit, resolution = resolution),
      re_formula = NA
    ),
    error = identity
  )
  if (inherits(bn, "error")) {
    row$vs_bayesnec <- paste("bayesnec ERROR:", first_line(bn))
  } else {
    a <- unname(pred$curves[[1]])
    b <- unname(as.matrix(bn))
    row$vs_bayesnec <- if (!identical(dim(a), dim(b))) {
      sprintf(
        "dims %s vs %s",
        paste(dim(a), collapse = "x"),
        paste(dim(b), collapse = "x")
      )
    } else if (identical(a, b)) {
      "identical"
    } else {
      sprintf("differs, max abs %.3g", max(abs(a - b), na.rm = TRUE))
    }
  }
  e <- tryCatch(suppressWarnings(ecx(pred)), error = identity)
  row$ecx <- if (inherits(e, "error")) {
    paste("ERROR:", first_line(e))
  } else {
    sprintf("dec %.3g", e$estimate[e$direction == "decreasing"])
  }
  ns <- tryCatch(
    suppressWarnings(summarise_metric(compute_nsec(pred), pred, "nsec")),
    error = identity
  )
  row$nsec <- if (inherits(ns, "error")) {
    paste("ERROR:", first_line(ns))
  } else {
    sprintf("dec %.3g", ns$estimate[ns$direction == "decreasing"])
  }
  row
}
files <- list.files(fit_dir, pattern = "[.]rds$", full.names = TRUE)
files <- files[!grepl("error[.]rds$|_data[.]rds$|^compare-", basename(files))]
args <- commandArgs(TRUE)
if (length(args)) {
  files <- files[sub("[.]rds$", "", basename(files)) %in% args]
}
rows <- lapply(files, function(f) {
  fit <- readRDS(f)
  name <- sub("[.]rds$", "", basename(f))
  out <- list(compare_one(name, fit))
  # every component of a model set, pulled out as a bayesnecfit
  if (inherits(fit, "bayesmanecfit")) {
    for (m in names(fit$mod_fits)) {
      single <- suppressMessages(bayesnec::pull_out(fit, model = m))
      out[[length(out) + 1]] <- compare_one(paste0(name, "/", m), single)
    }
  }
  # the per-level fits of a group fit and the two parts of a hurdle pair are
  # ordinary bnec() fits a user can pass on their own
  if (inherits(fit, "bayesnecgroupfit")) {
    for (lev in names(fit$fits)) {
      out[[length(out) + 1]] <- compare_one(
        paste0(name, "/fits$", lev),
        fit$fits[[lev]]
      )
    }
  }
  if (inherits(fit, "bayesnechurdlefit")) {
    for (part in c("growth", "survival")) {
      out[[length(out) + 1]] <- compare_one(
        paste0(name, "/", part),
        fit[[part]]
      )
    }
  }
  out
})
tab <- do.call(rbind, lapply(unlist(rows, recursive = FALSE), as.data.frame))
saveRDS(
  tab,
  file.path(
    fit_dir,
    paste0(
      "compare-",
      if (length(args)) paste(args, collapse = "_") else "all",
      ".rds"
    )
  )
)
options(width = 200)
print(tab, right = FALSE, row.names = FALSE)
