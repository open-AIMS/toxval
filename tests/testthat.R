# Load quietly. `library(toxval)` emits a packageStartupMessage about
# predict.bayesnecfit and predict.bayesmanecfit overwriting bayesnec's
# registrations, which is noise in the R CMD check test log. Suppressing at
# the point of loading keeps what the package emits unchanged.
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(toxval))

test_check("toxval")
