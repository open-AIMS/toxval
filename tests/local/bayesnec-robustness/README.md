# bayesnec robustness check

These scripts fit one example of each model structure that `bayesnec` can build, and of the five structures that CR_workflows fits. They then compare `toxval_predict()` on each fit with `bayesnec`'s own `posterior_epred()`, and run the ECx and NSEC computations on every prediction. `BAYESNEC-claude.md` §3.7 records the results of the run on `predev` at `116ea91b`.

The scripts assert nothing, and R CMD check does not run them. They print what they find. They are kept as the source of fits for the comparison tests of task 5 (`BAYESNEC-claude.md` §4).

## Order of the scripts

Each script is run with `Rscript` from the toxval repository root. A fit script skips any fit already saved, so an interrupted run can be resumed.

1. `fit-families.R`, which also saves the data that `fit-ops.R` reads.
2. `fit-allset.R`, the model set that `fit-ops.R` passes to `amend()` and `pull_out()`.
3. `fit-features.R` once for each of the arguments `groups`, `terms`, `families`, `classes` and `thin`.
4. `fit-cr.R` once for each of the arguments `algal_growth`, `fish_larval_survival`, `daphnia_reproduction`, `coral_bleaching` and `bioluminescence_inhibition`.
5. `fit-ops.R`.
6. `compare-fits.R`, which prints the comparison table and saves it as `compare-all.rds` in the fit folder.
7. `check-edges.R`, the checks that the comparison does not make.

The runs in steps 3 and 4 are independent of each other and can be made in parallel. For example:

```bash
Rscript tests/local/bayesnec-robustness/fit-features.R groups
```

## Settings

| variable | default | purpose |
|---|---|---|
| `BAYESNEC_SOURCE` | unset, so the installed `bayesnec` is used | a `bayesnec` source tree, loaded with `pkgload::load_all()` |
| `TOXVAL_ROBUSTNESS_DIR` | `bayesnec-robustness` in `tools::R_user_dir("toxval", "cache")` | where the fits and the comparison table are written |
| `CR_WORKFLOWS` | `../CR_workflows` | the CR_workflows checkout whose datasets `fit-cr.R` refits |

The fits are written outside the repository because they take about 3.3 GB. Inside it, R CMD build would copy them into the package tarball.

## Provenance

The run in §3.7 was made on 2026-10-23 with R 4.6.1, `brms` 2.23.0 and `rstan` 2.32.7, under WSL2. `bayesnec` was `predev` at `116ea91b` (2.1.3.42), exported with `git archive` and loaded through `BAYESNEC_SOURCE`, so nothing was installed. Steps 1 to 5 took about 75 minutes, with the runs of steps 3 and 4 made in parallel.
