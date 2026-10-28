options(mc.cores = 1)

# drc is only needed so bare predict() on drc objects dispatches to
# predict.drc inside nsec.drc(); load its namespace (no attach, no startup
# banner). No library(brms) is needed because no test makes a bare
# (unqualified) brms call; all brms usage is namespace-qualified. (Imports
# loads brms' namespace but does not attach it, so it would not rescue a bare
# call anyway.)
requireNamespace("drc", quietly = TRUE)

# Data the tests take from bayesnec are stored in fixtures/ and loaded by
# helper-fixtures.R, rather than read from the package, so the suite runs
# without bayesnec installed (#39).
