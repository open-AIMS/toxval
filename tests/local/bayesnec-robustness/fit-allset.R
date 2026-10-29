# Every equation bayesnec offers, as one model set.
source("tests/local/bayesnec-robustness/common.R")
# On a 0-1 response with x > 0. bayesnec kept 17 of the 23 equations on the
# run recorded in BAYESNEC-claude.md §3.7.
fit_save(
  "set_all_beta",
  bnec_q(
    formula = y ~ crf(x, model = "all"),
    data = bayesnec::nec_data,
    family = Beta,
    seed = 216
  )
)
