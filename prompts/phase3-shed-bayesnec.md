## Session: phase3-shed-bayesnec
Date: 2026-10-17
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** can you implement the prompt at
C:/Rworking/agent-prompts/2026-10-17-phase3-shed-bayesnec.md

The prompt asks for phase 3 of the refactor ("toxval sheds bayesnec") to be
implemented unattended on a new branch `phase-3-shed-bayesnec` from
`origin/phase-2-review`, following `REFACTOR-claude.md` §4 ("Phase 3 without
`bayesnec`"), `PHASE1_DEPENDENCY_UNTANGLE.md` §2, §3 and Step 2, and T4 in
`notes/implementation/02_decisions.md`. The work: delete `ecx.bnecfit()`,
`nsec.bnecfit()`, `predict.bayesnecfit()`, `predict.bayesmanecfit()` and
`newdata_eval()`, keeping any helper a `brmsfit` or `drc` method still calls;
delete the tests of the deleted methods; make the remaining suite independent of
bayesnec, storing what it reads as fixtures; rewrite the `ecx` and `nsec`
examples on a `drc` fit to a dataset shipped with drc; remove bayesnec from
`Imports`; leave `NEWS.md` to fledge; format with air. Verification is
`grep -rn "bayesnec::" R/ tests/`, `devtools::test()` and `devtools::check()`
with bayesnec unavailable and available, and an unchanged result for every
unmarked test. Questions at the listed stop-and-ask points are recorded rather
than decided. The branch is pushed to `origin` only.

**Claude:** Implemented in nine commits on `phase-3-shed-bayesnec`, the last
of which adds this log. The
sections below give the decisions and their reasons, the deleted and rewritten
tests, and the verification. The final report closes the entry.

#### Baseline

The baseline was taken on `phase-2-review` at `cdf878c` with bayesnec
installed: 10 test files, 378 tests, 861 expectations, all passing, no skips
and no warnings. The test run also printed
`Registered S3 methods overwritten by 'toxval': predict.bayesmanecfit,
predict.bayesnecfit`, the S3 collision this phase removes.

A second baseline recorded 25 objects computed on the unchanged code by the
kept methods and the phase 2 spine: `ecx()` on `brms_model_1`, `_2`, `_3` and
`_5` (types absolute, relative and direct, `posterior = TRUE`, grouped,
`hormesis_def = "max"`); `nsec()` on `brms_model_1`, `_4` (grouped) and `_5`
(`horme = TRUE`); `nsec()` on `nsec_drc_1` and on `drc::drm()` fits to
`nec_data` and to the ametryn subset of `herbicide`; `nsec_multi()` on
`nsec_multi_model_2`, summary and posterior; `toxval_predict()` on
`brms_model_1`, `brms_model_2` grouped and `nsec_drc_1` (100 bootstrap
realisations, seed 42); and `compute_ecx()` and `compute_nsec()` on each of
those three `toxval_pred` objects. The script is
`snapshot_values.R` in the session scratchpad and is not committed.

#### Deleted and kept code

| function | outcome | reason |
|---|---|---|
| `ecx.bnecfit()`, `nsec.bnecfit()` | deleted | the task |
| `predict.bayesnecfit()`, `predict.bayesmanecfit()` | deleted, with `R/predict.R` and `man/predict.Rd` | the task |
| `newdata_eval()` | deleted | the task; only the bnecfit methods called it |
| `tox_fct()` | deleted | only `ecx.bnecfit()` called it; the `brmsfit` method uses the `ecx_x_*()` functions |
| `modify_posterior()`, `do_wrapper()` | kept | see below |
| `nsec_fct()`, `clean_names()`, `ecx_x_*()`, `get_nsec_multi()`, `extract_nsec_multi()` | kept | called by the `brmsfit`, `drc` or `nsec_multi` code |

`modify_posterior()` and `do_wrapper()` have no live caller once the bnecfit
methods are gone. They were kept for three reasons. The commented-out hormesis
truncation blocks in `ecx.brmsfit()` and `nsec.brmsfit()` call both functions,
and those blocks are kept deliberately for the hormesis rework (#33, #37, #20).
`PHASE1_DEPENDENCY_UNTANGLE.md` §3 lists `modify_posterior` among the estimator
machinery `toxval` owns after the untangle. And the two `modify_posterior()`
tests are marked `EXPECTED-CHANGE hormesis handling reworked with direction
#20`, which records a change in a later phase rather than removal in this one.
A comment in `R/helpers.R` states this.

The imports used only by the deleted code were dropped: `stats::median`,
`stats::model.frame`, `brms::posterior_predict` and `brms::posterior_summary`.
`brms::as_draws_df` was already unused before this phase and was left alone.
The comments in `ecx.brmsfit()` and `nsec.brmsfit()` that described the bnecfit
methods in the present tense now say they were removed in phase 3. No code line
of a `brmsfit`, `drc` or `nsec_multi` method, or of the phase 2 spine, was
changed. The `toxval_predict()` example was changed only because it read
`bayesnec::nec_data`.

#### Deleted tests

Every test below has a deleted function as its only subject. Fifty-seven ran
in the baseline, holding 148 expectations; three more were inside `if (FALSE)`
blocks or in `tests/local/`, which the baseline does not run.

`tests/testthat/test-ecx.R` (27 run in the baseline, plus 2 disabled):

- bnecfit works with default parameters
- bnecfit checking hormesis_def = max
- bnecfit checking type = relative
- bnecfit checking type = direct
- bnecfit checking type = relative versus type = absolute behaves as expected
- bnecfit checking posterior = TRUE
- bnecfit checking ecx_val changes
- bnecfit checking ecx_val changes as expect
- bnecfit checking resolution changes
- bnecfit checking x_range
- bnecfit checking x_range behaves as expected for extrapolation
- bayesmanecfit works
- bayesnecfit works
- bnecfit xform is applied to output values
- bnecfit prob_vals changes quantile levels
- bnecfit input validation catches bad type
- bnecfit input validation catches bad hormesis_def
- bnecfit input validation catches non-function xform
- bnecfit input validation catches bad prob_vals
- bnecfit input validation catches non-numeric resolution
- bnecfit input validation catches non-logical posterior (replaced, see below)
- bayesmanecfit posterior = TRUE returns full posterior
- bayesmanecfit resolution affects output attribute
- bayesmanecfit x_range changes estimation domain
- bayesnecfit posterior = TRUE returns many draws
- bayesnecfit resolution changes are stored
- bayesnecfit ecx_val ordering is monotonic
- bnecfit ecx_val out-of-range errors for absolute and relative types
  (disabled, `if (FALSE)`, marked #54)
- bnecfit errors if multiple ecx_val values passed (disabled, `if (FALSE)`,
  marked #9)

`tests/testthat/test-nsec.R` (18):

- prob_vals when passed changes the levels
- sig_val warning message triggered when more then 1 value passed
- xform passes correctly
- when posterior is true you get the posterior as output
- sig_val passes correctly
- nsec returns expected object types and resolution is passing correctly
- x_range can be passed various values for bnecfit models
- works for bayesnecfit
- works for bayesmanecfit
- bnecfit nsec returns correct structure
- bnecfit nsec posterior returns full distribution
- bnecfit nsec sig_val affects reference level and output
- bnecfit nsec xform is applied to posterior before summarising
- bnecfit nsec hormesis_def = max uses maximum as reference
- bayesmanecfit nsec posterior = TRUE returns full draws
- bayesmanecfit nsec resolution changes precision
- bayesmanecfit nsec xform changes output
- bnecfit default resolution is 100

`tests/testthat/test-helpers.R` (5):

- tox_fct returns NA when no crossing found
- tox_fct interpolates crossing correctly
- newdata_eval returns list with newdata and x_vec
- newdata_eval respects x_range
- newdata_eval works with bayesnecfit objects

`tests/testthat/test-predict.R`, the whole file (7):

- predict on pulled brmsfit returns expected structure
- predict.bayesmanecfit returns summary by default
- predict.bayesmanecfit summary = FALSE returns raw posterior
- predict.bayesmanecfit robust = TRUE uses median
- predict.bayesmanecfit probs argument changes CI columns
- predict.bayesmanecfit probs with 3 values returns 5-column matrix
- predict.bayesnecfit dispatches correctly and returns 4-column matrix

`tests/local/test-nsec.R` (1, outside the testthat suite):

- works for bayesnecfit

Of the deleted tests, 23 were unmarked. Their subject was a deleted method, so
item 2 of the prompt covers them, and `REFACTOR-claude.md` §4 counts every test
reading `bnec_model_1`, `manec_example`, `nec4param`, `ecx4param` or a
`bayesnec_*` fixture as a test of a deleted method.

#### Rewritten tests

Some bnecfit tests checked an error that the `ecx()` or `nsec()` generic raises
before dispatch, so their subject survives the deletion. A test of that kind was
rewritten to call `brms_model_1` (with `x_var = "x"`) where no test on a
`brmsfit` or `drc` fit already checked the same error, and deleted otherwise. The expected
error messages are unchanged.

- `test-nsec.R`, prob_vals warnings behave as expected: four expectations on
  `manec_example` and `bayesnec_nec4param` became two on `brms_model_1`, since
  the other two repeated the same calls on a second object.
- `test-nsec.R`, hormesis_def errors with wrong input value (keeps its #20
  marker).
- `test-nsec.R`, xform errors if function not passed.
- `test-nsec.R`, nsec input validation catches non-numeric sig_val.
- `test-nsec.R`, nsec input validation catches non-numeric resolution.
- `test-nsec.R`, nsec input validation catches non-logical posterior.
- `test-ecx.R`, bnecfit input validation catches non-logical posterior is
  replaced by "posterior fails if not logical" on `brms_model_1`, placed after
  "xform fails if function not passed".

The other bnecfit argument-check tests were deleted because an equivalent
already exists: in `test-ecx.R`, "type errors when wrong value passed",
"hormesis_def errors wrong values passed", "xform fails if function not
passed", the four `prob_vals` error tests and "proper resolution values can be
passed"; in `test-nsec.R`, "drc model errors if more then 1 sig val is passed".

#### Fixtures and setup.R

`tests/testthat/setup.R` no longer calls bayesnec. It built `nec_data` with three
added columns, `other_data`, `manec_example`, `nec4param` and `ecx4param`; only
tests of the deleted methods read them, so they were removed rather than stored.

Three drc tests in `test-nsec.R` fitted `drc::drm()` to bayesnec data: "nsec
works for drc using continuous data" (`nec_data`, named in the plan) and "nsec
drc additional example 1" and "2" (`herbicide`, which the plan does not list).
`bayesnec::nec_data` and `bayesnec::herbicide` are stored as
`tests/testthat/fixtures/bayesnec_nec_data.rda` and `bayesnec_herbicide.rda`,
generated in `data-raw/DATASET.R` from bayesnec 2.1.3.39, and identical to the
package datasets. The `bayesnec_` prefix was added in a follow-up commit:
`devtools::load_all()` loads every fixture into the package environment through
`helper-fixtures.R`, and the first names, `nec_data` and `herbicide`, masked the
bayesnec datasets in a session with bayesnec attached.

`setup.R` was stored with CRLF line endings, against `.gitattributes`, which
asks for LF. It was normalised in its own commit so the content change stays
readable.

`bnec_model_1.rda`, `bayesnec_ecx4param.rda` and `bayesnec_nec4param.rda` are
no longer read by any test. They were kept: they load without bayesnec
installed (checked in the library without bayesnec, which loaded the objects
without error or warning and without loading the bayesnec namespace), and
`brms::posterior_epred()` on `bayesnec_ecx4param$fit` returned a 100 × 3
matrix there, the route `REFACTOR-claude.md` §4 describes for checking
`toxval_predict()` against a `bnec()` fit.

#### Examples and documentation

The `nsec()` example fits `drc::drm(rootl ~ conc, data = drc::ryegrass, fct =
drc::LL.4())` inside `\donttest{}` and `if (requireNamespace("drc", quietly =
TRUE))`; it returned an NSEC of 1.25 (0.82, 1.64). The `toxval_predict()`
example uses the same fit in place of one to `bayesnec::nec_data`.

`ecx()` has no `drc` method (`REFACTOR-claude.md` §5), so the instruction to
rewrite its example on a `drc` fit cannot be followed: `ecx()` on a `drc` fit
errors. Its example fits `brms::brm(rootl ~ s(conc, k = 5), data =
drc::ryegrass, chains = 2, seed = 1, control = list(adapt_delta = 0.99))` and
is wrapped in `\dontrun{}`, with a comment giving the reason, because the fit
compiles a Stan model. It was run once in this session and returned an EC10 of
0.69 (0.55, 0.90) and an EC50 of 3.60 (2.99, 4.33) with no sampler warnings.
Two earlier attempts, `k = 5` and `k = 4` at the default `adapt_delta`, gave 2
and 4 divergent transitions.

The `[bayesnec::compare_fitted()]` links in the `ecx` and `nsec` details became
"`compare_fitted()` in the bayesnec package". The `object` parameter in
`toxval_params`, which named the bayesnec classes, now reads "A fitted model
object of a class with a method for the function, such as a `brmsfit` returned
by `brms::brm()`".

`DESCRIPTION` drops bayesnec from `Imports`. It is not added to `Suggests`,
because no remaining test compares against bayesnec. `NAMESPACE` has no
`bnecfit`, `bayesnecfit` or `bayesmanecfit` method. `devtools::document()` ran
with roxygen2 8.0.0 against `Config/roxygen2/version: 8.0.0.9000`; it warned of
the mismatch, and the regenerated Rd files differ from the committed ones only
where the source changed.

#### Library without bayesnec

bayesnec is installed twice: 2.1.3.39 in the user library and a second copy in
`/usr/local/lib/R/site-library`. The prompt's method, a symlinked library in
`R_LIBS_USER` with `R_LIBS_SITE` unchanged, left `requireNamespace("bayesnec")`
TRUE through the site library. The checks without bayesnec therefore used a
library of 785 symlinks to every package in the user and site libraries except
bayesnec, with `R_LIBS_USER` set to it and `R_LIBS_SITE` set to an empty
directory. In that R, `.libPaths()` was the symlinked library, the empty
directory and `/usr/lib/R/library`, and `requireNamespace("bayesnec")` was
FALSE while brms, drc and devtools loaded.

#### Verification

The grep in the definition of done, `grep -rn "bayesnec::" R/ tests/`,
returns one line, `tests/local/setup.R:18: fit_brms <-
bayesnec::pull_brmsfit(fit)`, which is left for a decision (see the open
questions). `R/`, `tests/testthat/`,
`DESCRIPTION` and `NAMESPACE` mention bayesnec only in comments and in plain
documentation text.

Test results (`devtools::test()`, R 4.6.1 in WSL, final head `8d43307` on a
fresh clone of the branch):

| run | files | tests | expectations | failed | skipped | warnings |
|---|---|---|---|---|---|---|
| baseline, `cdf878c`, bayesnec available | 10 | 378 | 861 | 0 | 0 | 0 |
| `8d43307`, bayesnec not available | 9 | 322 | 712 | 0 | 0 | 0 |
| `8d43307`, bayesnec available | 9 | 322 | 712 | 0 | 0 | 0 |

The counts reconcile: 378 − 57 deleted + 1 replacement gives 322 tests, and 861
− 148 − 2 + 1 gives 712 expectations. The 321 tests present in both runs all
pass, with the same number of expectations in each, except "prob_vals warnings
behave as expected", which has 2 in place of 4 (see Rewritten tests). The test
code of those 321 tests is otherwise unchanged apart from the object in the six
rewritten `nsec` tests and the fixture names in the three drc tests.

The 25 snapshot objects computed by the kept methods and the spine are
`identical()` before and after, so no number computed on a `brmsfit` or `drc`
fit changed in this phase. Both snapshot runs had bayesnec available and took
the data from it, so the comparison is of the code and not of the fixtures.

`devtools::check()` (which runs `R CMD check --as-cran`, including
`\donttest{}` examples) on the same fresh clone gave Status OK, with 0 errors,
0 warnings and 0 NOTEs, both with bayesnec not available and with it
available. An earlier check run in the working copy, at `fff9540`, gave two
NOTEs, for the `.claude/` and `superceded/` directories, which are untracked and
exist only in that copy. The clean clone was used for the final result for that
reason.

With bayesnec attached and the package loaded by `devtools::load_all()`, the
`Registered S3 methods overwritten` message no longer appears, and the S3
methods table of toxval holds only `ecx.brmsfit`, `nsec.brmsfit`, `nsec.drc` and
the three `toxval_predict` methods. The `ecx` and `nsec` generics are still
exported by both packages, so attaching both still masks one pair; that ends at
phase 7.

#### Final report

Commits on `phase-3-shed-bayesnec`, after `cdf878c`:

1. `6ef71ea` move generic argument-check tests onto brms_model_1
2. `58c57a1` remove the bnecfit and bayesnec predict methods (#39), whose
   message contains the NEWS bullet for fledge
3. `f705048` store setup.R with LF line endings
4. `a409415` read bayesnec data in the tests from fixtures
5. `01c24b9` rewrite the examples without bayesnec
6. `fff9540` drop bayesnec from Imports
7. `98cc08c` prefix the bayesnec data fixtures with bayesnec_
8. `8d43307` reword the comments on the rewritten nsec argument tests
9. the commit adding this log

Deleted: `ecx.bnecfit()`, `nsec.bnecfit()`, `predict.bayesnecfit()`,
`predict.bayesmanecfit()`, `newdata_eval()`, `tox_fct()`, their 57 running
tests and 3 non-running ones, and bayesnec from `Imports`. Kept:
`modify_posterior()` and `do_wrapper()`, for the commented hormesis blocks in the
`brmsfit` methods; every helper the `brmsfit`, `drc` and `nsec_multi` code calls;
the three unread bayesnec fixtures. Tests: 378 tests and 861 expectations
before, 322 and 712 after, all passing with and without bayesnec. Check: Status
OK, 0 errors, 0 warnings, 0 NOTEs with and without bayesnec.

The branch was pushed to `origin`. No pull request was opened and no issue or
pull request was commented on.

Open questions, for Becky:

1. The `ecx()` example. `ecx()` has no `drc` method, so the instruction to use
   a `drc` fit could not be followed. The example uses a `brms` fit inside
   `\dontrun{}`. The choices are to keep that, to switch to `\donttest{}` so
   that R CMD check fits it (a Stan compilation of about a minute on every check
   platform), or to rewrite it on a `drc` fit once phase 4 gives `ecx()` a
   `drc` route.
2. `tests/local/`. This is a stop-and-ask point: a surviving test needs bayesnec
   for a reason other than data. `tests/local/setup.R` fits a model with
   `bayesnec::bnec()`, takes its `brmsfit` with `bayesnec::pull_brmsfit()`, and
   uses its priors for a second `brms::brm()` fit. Its six remaining tests are of
   `nsec.brmsfit()`, so they survive the deletion, and the done grep still
   matches line 18. The suite is not run by `test_check()` and R CMD check does
   not scan it for dependencies, so it does not affect either result. It was left
   unchanged apart from deleting "works for bayesnecfit". The second fit appears
   to be the model stored as `brms_model_4` (same formula, priors taken from the
   same `bnec()` call, seed 700, 1000 rather than 2000 iterations), so the choices
   include pointing these tests at stored fixtures, deleting `tests/local/`, or
   keeping it with `skip_if_not_installed("bayesnec")` and bayesnec in
   `Suggests`.
3. `modify_posterior()` and `do_wrapper()` were kept without a live caller.
   Confirm, or delete them now with the commented blocks that call them.
4. `bnec_model_1`, `bayesnec_ecx4param` and `bayesnec_nec4param` are no longer
   read by any test. They were kept, with their `data-raw/DATASET.R` sections,
   for the comparison of `toxval_predict()` on a `bnec()` fit that §4
   describes. Confirm, or delete them.
5. Seven argument-check tests were rewritten to call `brms_model_1` rather than
   deleted, because the generic they check survives. Confirm this reading of
   item 2 of the prompt.
6. #54. Its disabled test, "bnecfit ecx_val out-of-range errors", was deleted
   with the method. `ecx.brmsfit()` already refuses an `ecx_val` outside 1 to 99,
   so the gap #54 records may now apply only to a check on the generic itself.
   Whether #54 is closed or restated is open.
7. The `DESCRIPTION` Description field and `README.Rmd` / `README.md` still say
   toxval supports bayesnec model objects. They were left because bayesnec fits
   are to be supported again at phase 7, through methods in bayesnec, and the
   wording is a choice for the authors.
8. `PHASE1_DEPENDENCY_UNTANGLE.md` Step 2 asks for a version bump and a tag with
   this change. The prompt did not, so neither was done.
9. Two deviations from the prompt: `R_LIBS_SITE` was pointed at an empty
   directory rather than left unchanged, because bayesnec is also installed in
   the site library; and `setup.R` was normalised to LF in its own commit.

---
