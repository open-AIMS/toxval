# Compare the toxval and bayesnec implementations of the duplicated ecx/nsec
# methods across the whole toxval fixture suite.
#
# Reproduces the tables in open-AIMS/toxval#19 and #39: ecx differs on every
# bnecfit fixture, while nsec is bit-for-bit identical everywhere except the
# model-averaged case (which is non-deterministic anyway -- see
# replicate_manec.R).
#
# Methods are fetched namespace-qualified so this measures the estimators
# rather than S3 dispatch. For the dispatch/masking behaviour see
# attach_order.R.
#
# Run from the repository root:  Rscript notes/pr42/compare_impls.R

suppressPackageStartupMessages({
  library(toxval)
  library(bayesnec)
})
requireNamespace("drc", quietly = TRUE)

fixtures <- "tests/testthat/fixtures"
if (!dir.exists(fixtures)) {
  stop("run this from the repository root", call. = FALSE)
}
for (f in list.files(fixtures, pattern = "[.]rda$", full.names = TRUE)) {
  load(f)
}
manec_example <- bayesnec::manec_example
nec4param <- suppressMessages(suppressWarnings(
  bayesnec::pull_out(bayesnec::manec_example, model = "nec4param")
))
ecx4param <- suppressMessages(suppressWarnings(
  bayesnec::pull_out(bayesnec::manec_example, model = "ecx4param")
))

tv <- asNamespace("toxval")
bn <- asNamespace("bayesnec")

# The method actually registered for this object in this namespace, walking
# class() so a bnecfit method is found for a bayesnecfit object.
pick <- function(ns, generic, obj) {
  for (cls in class(obj)) {
    nm <- paste0(generic, ".", cls)
    if (exists(nm, envir = ns, inherits = FALSE)) {
      return(list(fun = get(nm, envir = ns), cls = cls))
    }
  }
  NULL
}

specs <- list(
  list(nm = "manec_example", args = list()),
  list(nm = "nec4param", args = list()),
  list(nm = "ecx4param", args = list()),
  list(nm = "bayesnec_nec4param", args = list()),
  list(nm = "bayesnec_ecx4param", args = list()),
  list(nm = "bnec_model_1", args = list()),
  list(nm = "brms_model_1", args = list(x_var = "x")),
  list(nm = "brms_model_2", args = list(x_var = "x")),
  list(nm = "brms_model_3", args = list(x_var = "x")),
  list(nm = "brms_model_4", args = list(x_var = "x")),
  list(nm = "brms_model_5", args = list(x_var = "x")),
  list(nm = "nsec_drc_1", args = list(x_var = "x")),
  list(nm = "nsec_drc_2", args = list(x_var = "x"))
)

run <- function(f, obj, args) {
  tryCatch(
    as.numeric(do.call(f, c(list(obj), args))),
    error = function(e) paste0("ERROR: ", conditionMessage(e))
  )
}

rows <- list()
for (generic in c("ecx", "nsec")) {
  extra <- if (generic == "ecx") {
    list(ecx_val = 10, resolution = 50)
  } else {
    list(sig_val = 0.01, resolution = 50)
  }
  for (s in specs) {
    obj <- get(s$nm)
    a <- c(s$args, extra)
    mt <- pick(tv, generic, obj)
    mb <- pick(bn, generic, obj)
    if (is.null(mt) && is.null(mb)) next
    rt <- if (is.null(mt)) "no method" else run(mt$fun, obj, a)
    rb <- if (is.null(mb)) "no method" else run(mb$fun, obj, a)
    verdict <- if (is.null(mt) || is.null(mb)) {
      "only one pkg has a method"
    } else if (!is.numeric(rt) || !is.numeric(rb)) {
      "error in at least one"
    } else if (isTRUE(all.equal(rt, rb, tolerance = 1e-8))) {
      "IDENTICAL"
    } else {
      "DIFFERENT"
    }
    fmt <- function(v) {
      if (is.numeric(v)) {
        paste(signif(v, 6), collapse = ", ")
      } else {
        substr(v, 1, 45)
      }
    }
    rows[[length(rows) + 1]] <- data.frame(
      generic = generic,
      fixture = s$nm,
      class = class(obj)[1],
      toxval = fmt(rt),
      bayesnec = fmt(rb),
      verdict = verdict,
      stringsAsFactors = FALSE
    )
  }
}

res <- do.call(rbind, rows)
options(width = 250)
cat("\n============ IMPLEMENTATION COMPARISON ============\n")
print(res, row.names = FALSE)
cat("\n============ SUMMARY ============\n")
print(table(res$generic, res$verdict))
