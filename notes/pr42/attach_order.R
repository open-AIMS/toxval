# Show that attach order decides which `ecx` generic is in scope, and that
# bayesnec's has no brmsfit method -- so toxval's documented brmsfit support
# disappears in one of the two orders.
#
# Reproduces the masking demonstration in open-AIMS/toxval#19 and #39.
#
# Run from the repository root, both ways:
#   Rscript notes/pr42/attach_order.R tv_then_bn
#   Rscript notes/pr42/attach_order.R bn_then_tv

order <- commandArgs(trailingOnly = TRUE)[1]
if (!isTRUE(order %in% c("tv_then_bn", "bn_then_tv"))) {
  stop("pass one of 'tv_then_bn' or 'bn_then_tv'", call. = FALSE)
}

if (order == "tv_then_bn") {
  suppressPackageStartupMessages({
    library(toxval)
    library(bayesnec)
  })
} else {
  suppressPackageStartupMessages({
    library(bayesnec)
    library(toxval)
  })
}

fixtures <- "tests/testthat/fixtures"
if (!dir.exists(fixtures)) {
  stop("run this from the repository root", call. = FALSE)
}
for (f in list.files(fixtures, pattern = "[.]rda$", full.names = TRUE)) {
  load(f)
}

cat("attach order:", order, "\n")
cat("  environment(ecx) =", environmentName(environment(ecx)), "\n")
res <- tryCatch(
  paste(
    signif(as.numeric(ecx(brms_model_1, x_var = "x", resolution = 50)), 6),
    collapse = ", "
  ),
  error = function(e) paste0("ERROR: ", conditionMessage(e))
)
cat("  ecx(brms_model_1, x_var = 'x') ->", res, "\n")
