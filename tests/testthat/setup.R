options(mc.cores = 1)

# drc is loaded, not attached, so that generics such as coef(), vcov() and
# predict() dispatch to its methods for the stored drc fits. brms needs no such
# call, since every brms call in the tests is namespace-qualified.
requireNamespace("drc", quietly = TRUE)

# Data the tests take from bayesnec are stored in fixtures/ and loaded by
# helper-fixtures.R, rather than read from the package, so the suite runs
# without bayesnec installed (#39).
