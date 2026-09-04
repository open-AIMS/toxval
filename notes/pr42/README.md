# Measurements behind the PR #42 review

Scripts that reproduce the numbers quoted in the review of PR #42 and in the
issues it raised. They are diagnostics, not tests — they are not run by
`devtools::test()` and are excluded from the build via `.Rbuildignore`.

Run from the repository root. All three need `toxval`, `bayesnec` and `drc`
installed, and read the fixtures in `tests/testthat/fixtures/`.

| script | what it shows | cited in |
|---|---|---|
| `compare_impls.R` | `toxval` and `bayesnec` implementations of `ecx`/`nsec` compared across all 13 fixtures, fetched namespace-qualified so it measures estimators rather than dispatch. `ecx` differs on every `bnecfit` fixture; `nsec` is identical everywhere it can be tested. | #19, #39 |
| `attach_order.R` | Attach order decides which `ecx` generic is in scope. `bayesnec` has no `ecx.brmsfit`, so one order removes `toxval`'s `brmsfit` support entirely. Run it both ways. | #19, #39 |
| `replicate_manec.R` | Model-averaged estimates are not reproducible — unseeded component resampling makes repeated identical calls disagree, by ~26% on the lower bound. | #25, open-AIMS/bayesnec#216 |

```sh
Rscript notes/pr42/compare_impls.R
Rscript notes/pr42/attach_order.R tv_then_bn
Rscript notes/pr42/attach_order.R bn_then_tv
Rscript notes/pr42/replicate_manec.R
```

`replicate_manec.R` sets a seed for the harness but the behaviour it measures is
itself unseeded inside `bayesnec`, so the exact figures move between runs. The
pattern — stable medians, unstable lower bounds — is the reproducible part.
