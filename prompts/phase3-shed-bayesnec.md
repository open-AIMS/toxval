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

## Session: phase3-shed-bayesnec, answers to the open questions
Date: 2026-10-18
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:**

> Open questions
>
> 1. ecx() example: switch to \donttest{} so check runs it
>
> 2. tests/local/ (a stop-and-ask point). skip when bayesnec is missing
>
> 3. modify_posterior() / do_wrapper(): I'm unclear why we would consider
>    keeping them?
>
> 4. Unused fixtures: Keep for later comparisons
>
> 5. Rewritten tests: I'm not sure how to easily review these, I can't see the
>    branch on GitHub?
>
> 6. #54: its disabled test went with the bnecfit method. Some of those tests
>    apply to the ecx itself, they were not specific to the bnecfit methods.
>    Can you check they appear where they are actually required? Fix and post
>    on #54 accordingly.
>
> 7. Wording: the DESCRIPTION text and README still say toxval supports
>    bayesnec objects. I left them because bayesnec support is meant to return
>    at phase 7. Let's make this current for the existing state. We can add it
>    back at phase 7.
>
> 8. Version: PHASE1_DEPENDENCY_UNTANGLE.md asks for a version bump and tag
>    with this change. The prompt didn't, so I did neither. Yes do a version
>    bump.

**Claude:** Items 1, 2, 3 and 7 were implemented in four commits, item 6 was
checked and answered on #54, item 5 was answered with links, and item 8 was
not done, for the reasons below.

#### The ecx() example

The example is now in `\donttest{}` rather than `\dontrun{}`, so R CMD check
with `--as-cran` runs it. `refresh = 0` was added to the `brms::brm()` call; it
only stops the sampler printing its progress. The comment above it now reads
"ecx() has no method for drc fits, so this example fits a brms model. Compiling
the Stan model takes about a minute."

In R CMD check at `cccc5b9`, without bayesnec, the example took 76.6 s
elapsed (70.8 s user) of the 79 s for the `--run-donttest` examples. It printed
no sampler warning, and the check gave no timing NOTE. One run was timed, on WSL
R 4.6.1. The same run showed that only the second `ecx()` result was printed,
because inside `if () {}` only the last value is returned. Both calls are now
wrapped in `print()` (`0da5d23`).

#### The local tests

`tests/local/setup.R` fits its two models only when
`requireNamespace("bayesnec", quietly = TRUE)` is true, and
`tests/local/test-nsec.R` opens with `skip_if_not_installed("bayesnec")`, which
skips the whole file. bayesnec was not added to `Suggests`. R CMD check neither
runs `tests/local/` nor scans it for packages, and `skip_if_not_installed()`
does not need a declaration. At phase 7 bayesnec imports toxval, and a
`Suggests` entry would then make each package depend on the other.

The local tests were run with `testthat::test_dir("tests/local", package =
"toxval", load_package = "source")` on a fresh clone at `cccc5b9`. With bayesnec
unavailable, the file was reported as one skip ("{bayesnec} is not installed")
and no test ran. With bayesnec available and `NOT_CRAN=true`, the six tests ran
and their 10 expectations passed.

#### modify_posterior() and do_wrapper()

The two had been kept because the commented-out hormesis blocks in
`ecx.brmsfit()` and `nsec.brmsfit()` call them. That is not a reason to keep
them. Neither block can be uncommented as it stands: the `ecx.brmsfit()` guard
tests `object$model`, which on a `brmsfit` is the Stan code, and restoring the
truncation in `nsec.brmsfit()` makes `nsec_fct()` fail in `approxfun()` (#37).
The design in `REFACTOR-claude.md` §3 has no `modify_posterior()` blanking, and
both functions remain in `R/helpers.R` at the `pre-refactor` tag.

Both were deleted, with their two tests in `test-helpers.R`:

- modify_posterior with hormesis_def = max sets values before peak to NA
- modify_posterior with hormesis_def = control leaves all values

The comments on the commented-out blocks in `R/ecx.R` and `R/nsec.R` now say
that the functions were deleted and where they can be found. The comment in
`test-crossings.R` that cited `R/helpers.R:12-20` was reworded to the past
tense. `brms::as_draws_df`, imported but unused since before phase 3, was left.

#### Fixtures

`bnec_model_1`, `bayesnec_ecx4param` and `bayesnec_nec4param` were kept, as
answered.

#### Review of the rewritten tests

The branch was pushed to the fork on 2026-10-17 and is at
<https://github.com/beckyfisher/toxval/tree/phase-3-shed-bayesnec>; it is not in
`open-AIMS/toxval`. The seven rewritten argument-check tests are the whole of
commit `6ef71ea`
(<https://github.com/beckyfisher/toxval/commit/6ef71ea4c3386445a37f0a4a5fa649474da8f592>),
with comment wording changed in `8d43307`. The whole phase is at
<https://github.com/beckyfisher/toxval/compare/phase-2-review...phase-3-shed-bayesnec>.

#### The deleted tests and the ecx_val issue

Each of the 29 deleted `ecx()` tests and 18 deleted `nsec()` tests was compared
with the remaining `brmsfit` and `drc` tests, at `cdf878c` against the current
files. The two disabled tests of #54 check behaviour that is not specific to
`bnecfit`, and both checks are already tested on `brms_model_1`:

| disabled `bnecfit` test | `brmsfit` test of the same check |
|---|---|
| `ecx_val` of 0 or 100 errors for `type = "absolute"` and `"relative"`, and `type = "direct"` accepts 0 | "when type is not direct ecx_val has to between 1 and 99" (`test-ecx.R:493`), which also tests 1, 99, and `direct` with 1000 |
| more than one `ecx_val` errors | "can only pass a single exc_val argument" (`test-ecx.R:483`) and "check ecx_val must be a numeric scalar" (`test-ecx.R:40`) |

Every other general check in the deleted tests also has a `brmsfit` or `drc`
test, so no test was added. Some are tested with different values; for example
the order of ECx in `ecx_val` is tested at 10 and 50 rather than 10, 40 and 70.
The one exception is `x_range` given as an interval. `ecx.brmsfit()` and
`nsec.brmsfit()` reject an interval, the suite pins that error, and #10 records
the intended behaviour.

The `ecx_val` checks are made in `ecx.brmsfit()`, not in the generic, so the
tests are in the brms-specific section of `test-ecx.R`. #55 puts them in the
generic; `chk_ecx_val()` in `R/validate.R`, written in phase 2, is the shared
check, and the plan has `ecx()` use it in phase 4. The tests go to the general
section then.

The comment posted on #54 gives the table above with links to the lines at
`cccc5b9` and says the issue can be closed when phase 3 merges: <https://github.com/open-AIMS/toxval/issues/54#issuecomment-6247236580>.

#### DESCRIPTION and README

The `Description` field now says "fitted frequentist (drc) and Bayesian (brms)
model objects". In `README.Rmd`, the Overview says ECx and NSEC are extracted
from `brms` fits and the NSEC from `drc` fits, and that bayesnec fits are
handled by bayesnec's own `ecx()` and `nsec()`. The planned-development section
says toxval supports `brms` and `drc` fits, with `ecx()` for `brms` fits only.
`README.md` was rendered with `devtools::build_readme()` (pandoc 3.10). One
unrelated line, the hard line break after the first copyright line, was set back
to two trailing spaces, because pandoc 3.10 writes it as a backslash.
Statements in "Planned Development" about future bayesnec support were left.

The README also says toxval "supports extraction of the NEC parameter directly"
for threshold models. No toxval function does that now; it was not changed, and
is reported as a question.

#### Version bump

fledge was installed in neither the WSL nor the Windows R library. With
approval, fledge 0.1.4 was installed from CRAN into the WSL user library; no
other package was installed or upgraded. In 0.1.4, `bump_version()` stops unless
the current branch is the remote's default branch. A dry run of the steps after
that check, in a scratch clone, found one NEWS-worthy entry in 92 first-parent
commits. It kept only the first line of the multi-line bullet in `58c57a1`. It
also wrote a second header comment above the existing one, whose wording comes
from a later fledge than 0.1.4. Asked how to proceed, Becky chose to bump on
`main` after phase 3 merges. On `main`, fledge reads only first-parent commits,
so the NEWS bullet needs to be in the merge commit or the pull request body.

#### Verification

All runs were on fresh clones, with bayesnec made unavailable as in the
first entry.

| run | bayesnec | files | tests | expectations | failed | skipped | warnings |
|---|---|---|---|---|---|---|---|
| `devtools::test()`, `cccc5b9` | not available | 9 | 320 | 709 | 0 | 0 | 0 |
| `devtools::test()`, `cccc5b9` | available | 9 | 320 | 709 | 0 | 0 | 0 |

The two tests and three expectations fewer than at `173d1f3` (322 and 712) are
the deleted `modify_posterior()` tests.

`devtools::check()` runs `--as-cran`, so the `\donttest{}` examples run. At
`cccc5b9` it gave Status OK, 0 errors, 0 warnings and 0 NOTEs, with and without
bayesnec. At `0da5d23`, which changes only the example, it was run again without
bayesnec: Status OK, 0 errors, 0 warnings, 0 NOTEs, with both results printed (EC10 0.69, 95 per cent interval 0.55 to 0.90; EC50 3.60, 2.99 to 4.33).

The 25 values recorded before phase 3 (`values_before.rds`, outputs of the kept
`brmsfit`, `drc` and `nsec_multi` methods and the phase 2 spine) are all
`identical()` at `cccc5b9`.

#### Commits

1. `719a733` run the ecx example during check
2. `e373325` skip the local tests without bayesnec
3. `58d64b3` remove modify_posterior() and do_wrapper()
4. `cccc5b9` describe brms and drc support in DESCRIPTION and README
5. `0da5d23` print both results in the ecx example
6. the commit adding this entry

The branch was pushed to `origin`. No pull request was opened.

---

## Session: phase3-shed-bayesnec, README and CLAUDE.md
Date: 2026-10-18
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** clean up the claude and readmen

**Claude:** Taken as the two items left open in the previous entry. Two commits.

`a85ea31` corrects two README claims that were not true of the package. The
list of what toxval provides named NOEC extraction, which is not yet available;
it now says ECx and NSEC, with NOEC to follow (#17). The NSEC section said toxval
extracts the NEC parameter of threshold models directly. It now says the NEC is
a parameter of the fitted model, that toxval does not yet extract it or compute
model-averaged N(S)EC estimates, and that both are planned (#18). `README.md` was
rendered with `devtools::build_readme()`, with the copyright line break again set
back to two trailing spaces.

`d6e0695` brings the project `CLAUDE.md` up to date. Section 2 said toxval
supports bayesnec model outputs and extracts NECs. It now says toxval extracts
NSEC and ECx values, from brms fits (`ecx()`, `nsec()`, `nsec_multi()`) and drc
fits (`nsec()` only). It also names `toxval_predict()`, and says that bayesnec
fits are handled by bayesnec until phase 7. Section 4's layout said `data-raw/`
writes to `data/`, which does not exist. It now names
`tests/testthat/fixtures/`, adds `tests/local/`, says `NEWS.md` is generated by
fledge, and refers to section 6 rather than 5 for the prompt log.

The prose check reports nothing in the changed lines. Its remaining hits in both
files are in text this session did not change. No code, test or package
documentation changed, so the suite and R CMD check were not re-run.

---

## Session: phase3-shed-bayesnec, the phase 4 prompt
Date: 2026-10-18
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** if phase 3 is now complete, should we open this as a PR? And can you
write a prompt to implement stage 4? It would branch of this PR

**Claude:** Phase 3 meets its done condition in `REFACTOR-claude.md` §4. The
pull request is a stacked one: `phase-3-shed-bayesnec` starts from
`phase-2-review`, which is draft PR #64 into `open-AIMS:phase-2`, itself on #62.
A pull request into `phase-2` would list the 12 commits of #62 and #64 until
those merge, as #64 lists #62's. The recommendation given was to open it as a
draft into `phase-2` once Becky agrees, with the NEWS bullet from `58c57a1` in
its body, since fledge on `main` reads only first-parent commits.

The phase 4 prompt was written to
`C:/Rworking/agent-prompts/2026-10-18-phase4-metrics-on-spine.md`, after reading
`REFACTOR-claude.md` §3 and §4, `REFACTOR-human.md` §4, issue #60 and the
spine in `R/`, and after a search of the session logs, `notes/implementation/`,
the `EXPECTED-CHANGE` markers and the open issues for anything bearing on
phase 4. The prompt branches `phase-4-metrics-on-spine` from
`origin/phase-3-shed-bayesnec`.

The search found that phase 4 cannot run unattended as the plan stands. The
plan leaves open, or does not address, thirteen questions, and the prompt states
each as a ruling with a recommendation and `Ruling: pending`. A part of the work
whose rulings are pending is not started:

- R1, the mixed case of #60, which the plan says blocks phase 4;
- R2, the return shapes while the two directions exist and the tibble does
  not;
- R3, whether `draws` and the `posterior` deprecation wait for phase 5;
- R4, `lifecycle` in `Imports`, which every planned deprecation needs;
- R5, the name replacing `nsec_multi()`'s `type = "lower"` (T15);
- R6, the meaning of `type = "relative"` during the change (T9);
- R7, the scope of `anchor`;
- R8, the asymptote argument and its `toxval_pred` slot (#63);
- R9, warnings for censored realisations (#6, #24);
- R10, a `resolution` default of 1000 throughout;
- R11, the `meta` contract for a multivariate fit;
- R12, an `ecx()` route for `drc` fits;
- R13, the retired arguments.

The prompt's first part records the rulings in `02_decisions.md` and adds
`EXPECTED-CHANGE` markers to the unmarked tests that phase 4 will change. The
search listed about twenty, in its own commit before any code. Three defects
found by the search are carried into it:

- `nsec.brmsfit()` never applies `xform`, checked on 2026-10-18:
  `nsec(brms_model_1, x_var = "x", xform = function(x) x * 10)` returned 1.05,
  0.990, 1.05, as without it. The test at `test-nsec.R:717` therefore passes
  without testing anything.
- `nsec_multi()` ignores `prob_vals`.
- `chk_prob_vals()` accepts exactly three values where today's `ecx()` and
  `nsec()` accept more.

No file in the repository was changed apart from this log.

---

## Session: phase3-shed-bayesnec, the phase 4 rulings
Date: 2026-10-18
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:**

> Below I rule for some of your recommendations, along with a few questions.
> I've decided not to implement this without disussing with Ayla, so can we
> instead update the plan so it is clear what has been decided (changing as
> little as possible so it can be easily read in the PR).
>
> 1. No-crossing realisations (#60): Yes I agree, the two top points are self
>    evident, for the last one: a) let's use the lowest concentration in the
>    model data. that seems safest in terms of ensuring it is the control. b)
>    and c) seem reasonable.
> 2. Return shapes: Does that mean it would be cleaner to do phase 4 and 5
>    together?
> 3. draws and the posterior deprecation: As above, would it be better to
>    implement these together?
> 4. lifecycle: Yes agree
> 5. The new name for type = "lower": Do you have a recommendation that would
>    be cleanest? I do not have an opinion. Note, can we still get the
>    "decreasing" and "increasing" set?
> 6. What type = "relative" means during the switch: Can we review the recent
>    decisions in Bayesnec on this? Also, can we consider the literature review
>    and what would be most consistent with published convention?
> 7. The scope of anchor. Yes, I agree - and also, does this mean we should do
>    phase 5 with phase 4 (as mentioned above?)
> 8. The asymptote argument and its toxval_pred slot: Yes I agree with your
>    recommendation.
> 9. Warnings for censored realisations: Yes agree
> 10. A resolution default of 1000 everywhere: Why do you recommend 1000? With
>     the zero crossings this is less important than it used to be and 50 or
>     100 would be a lot less to compute, yes?
> 11. How multivariate fits are recorded inside toxval_pred: Yes I agree with
>     your recommendations
> 12. Whether ecx() should work on drc fits: Yes I agree with your
>     recommendation
> 13. What happens to hormesis_def, horme, by_group and curveid: Yes I agree
>     with your recommendation

**Claude:** The eight rulings were recorded as T21 to T28 in
`notes/implementation/02_decisions.md`, and the five open questions under
"Still to decide" with a recommendation each. `REFACTOR-claude.md` and
`REFACTOR-human.md` were changed only where a sentence had become wrong: the
`#60` paragraphs in §3.6, the `asymptote` slot in the §3.4 structure, the `drc`
asymptote lookup in §3.10 (spec only), the phase 4 row and item, and the note
on `ecx()` for `drc` fits in §5.

| prompt item | ruling | recorded as |
|---|---|---|
| R1, #60 | adopted, with the lowest concentration in the model data | T21 |
| R4, `lifecycle` | adopted | T22 |
| R7, `anchor` | adopted | T23 |
| R8, the asymptote | adopted | T24 |
| R9, warnings | adopted | T25 |
| R11, multivariate `meta` | adopted | T26 |
| R12, `ecx()` for `drc` | adopted | T27 |
| R13, retired arguments | adopted | T28 |
| R2, R3, and the timing in R7 | open: whether phases 4 and 5 are done together | Still to decide |
| R5 | open: the name replacing `type = "lower"` | Still to decide |
| R6 | open: what `relative` computes from phase 4 | Still to decide |
| R10 | open: the `resolution` default | Still to decide |

T21 (a) has a consequence that was not in the prompt. `toxval_pred$meta` does
not record the lowest concentration in the data, so an element has to be added
to the public contract, and its name was listed as a fifth open question.
`bayesnec` reads its control the same way (D15 ruling 2, "The control is read
at the lowest observed predictor value", `bayesnec`
`notes/implementation/03_decisions.md:247`).

#### Phases 4 and 5 together

R2, R3 and the last point of R7 exist only to keep today's shapes for one more
phase. Kept apart, phase 4 needs a layer that rebuilds today's vectors and
attributes, computes the increasing direction, `anchor` and `draws` without
reporting them, and keeps `nsec()` on a `drc` fit refusing an increasing
curve. Phase 5 then deletes that layer, and each changed test is edited twice.
What the separation provides is that the unmarked tests check the numbers. A
recorded comparison provides the same check without today's shapes, as in
phase 3 (`values_before.rds`): today's outputs are recorded first and compared
with the decreasing rows of the new output, each difference listed against its
ruling. The recommendation was to do the two together, with the `toxval`
tibble class built first and additively, as phase 2 built the spine, so that no
single pull request holds everything.

#### The name replacing `type = "lower"`

The recommendation was a logical `lower_only = FALSE`, with `criterion`. There
is one selection rule, and a logical states it in the call
(`nsec(fit, lower_only = TRUE)`), where `select = "lower"` does not say lower
than what, and `select` is also the `dplyr` verb for columns. Both directions
are still available: each call reports both, and one is selected by filtering
the `direction` column, as T15 decided. `nsec_multi(type = "increasing")` and
`"decreasing"` keep working through the deprecated wrapper. Kept apart from
phase 5 (R2), `nsec()` would report only the decreasing direction until then.

#### The meaning of `relative`

A research agent read `bayesnec`, the toxval-review literature review and
issues #19 and #63; the key passages were then checked directly.

- `bayesnec` D15 (RF, 2026-09-06, `notes/implementation/03_decisions.md:244-252`
  on `predev`) adopts T9's four types. Ruling 8: "`type = "relative"` warns
  when supplied explicitly, naming `range`, because its meaning changes and
  2.1.3 is released." The code (`R/ecx.R:445-472`) switches to the
  asymptote-based meaning at once and gives the reason: "the new meaning is a
  valid request and the majority of callers will want it." `ecx_asymptote()`
  (`R/ecx.R:492-524`) uses the `bot` draws, or 0 for a family bounded below,
  and otherwise stops, naming `absolute` and `range`. It was merged to `dev` in
  bayesnec PR #281 (2026-09-07) and is not yet released.
- The literature review (`toxval-review/reviews/2026-10-17-ecx-reference-definitions.qmd`)
  makes no recommendation (line 68). Ritz et al. (2015, p. 9, Equation 9)
  define the relative ED against the limits of the curve at 0 and infinity,
  "for most models in drc ... the parameters d and c" (lines 393-400, verified
  against the source). Daniels, Hommen and Ratte (2025) relate the relative
  ECx to "the maximum achievable response" (abstract, verbatim). The one form
  measured to the minimum predicted response, BMDS "Extra", was removed in
  BMDS 3.2, and its reading was not verified.
- #63 and #19 record nothing on the transition since T9.

The recommendation was the new meaning at once, as `bayesnec` did, with a
warning naming `range` whenever `relative` is supplied explicitly, and an error
where the bound is infinite. Computing the old quantity under `relative` for a
release would give the name a meaning no published source found uses, and
would differ from `bayesnec` for that release.

#### The `resolution` default

The linear interpolation between grid points does make the grid error small
where the grid is fine relative to the curve. `predict_grid()` spaces the grid
evenly in concentration, while dilution series are geometric, so the grid is
coarsest where an EC10 or NSEC lies. The phase 2 spine (`toxval_predict()`,
`compute_ecx()` at 10 and 50, `compute_nsec()` at `sig_val = 0.01`) was run at
50, 100, 200, 500 and 1000 points and compared with a 10000-point grid, on the
decreasing direction, one run each. The error is the largest absolute
difference in any of the median, 2.5% and 97.5% quantiles, as a percentage of
the 95% interval width at 10000 points.

| fit | realisations | 50 | 100 | 200 | 1000 | time at 100 (s) | time at 1000 (s) |
|---|---|---|---|---|---|---|---|
| `drc` `LL.4()`, simulated doses 0 to 100 | 1000 bootstrap | 21 | 8.3 | 2.0 | 0.33 | 0.10 | 0.41 |
| `drc` `LL.4()`, `drc::ryegrass` | 1000 bootstrap | 7.0 | 3.2 | 0.72 | 0.02 | 0.19 | 0.76 |
| `nsec_drc_1` | 1000 bootstrap | 1.7 | 0.30 | 0.06 | 0.003 | 0.11 | 0.29 |
| `brms_model_4`, 7 curves | 1000 draws | 2.8 | 0.37 | 0.13 | 0.006 | 2.30 | 3.24 |
| `bayesnec_nec4param$fit` | 100 draws | 3.3 | 0.66 | 0.01 | 0.0006 | 0.17 | 0.19 |
| `bayesnec_ecx4param$fit` | 100 draws | 0.60 | 0.14 | 0.03 | 0.001 | 0.15 | 0.17 |
| `brms_model_3` | 1000 draws | 0.64 | 0.24 | 0.06 | 0.002 | 0.17 | 0.32 |

The simulated fit used doses 0, 0.1, 0.32, 1, 3.2, 10, 32 and 100, four
replicates each, a mean of `0.5 + 9.5 / (1 + exp(1.2 * (log(dose) - log(5))))`
and a normal error with standard deviation 0.6 (`set.seed(7)`). `brms_model_1`
was also run and is not in the table: its 97.5% quantiles do not cross within
the range, so its intervals have no width. On
`brms_model_3` at 50 and 100 points, one realisation's NSEC was displaced by
27% of the predictor range, because the coarse grid missed a crossing where the
curve barely reached the reference; at 200 the largest displacement was 0.11%.
Times include `toxval_predict()` and both compute calls, and are dominated by
fixed overhead below about 1000 points. Memory does scale with the grid: at
1000 points a `toxval_pred` holds 32 MB per curve for 4000 draws. The
recommendation was to keep 1000. A grid spaced on the log scale, or root-finding
in place of the grid (#40), would let a smaller default suffice.

#### Commits

1. the commit adding this entry, with the changes to `02_decisions.md`,
   `REFACTOR-claude.md` and `REFACTOR-human.md`

The phase 4 prompt (`agent-prompts/2026-10-18-phase4-metrics-on-spine.md`) was
not changed. It will need revising after the discussion, substantially if
phases 4 and 5 are combined.

---

## Session: phase3-shed-bayesnec, the phase 4 rulings, second round
Date: 2026-10-19
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:**

> For this one: "The name for type = "lower" (5): I recommend lower_only =
> FALSE with criterion. [...]" Thinking on it, we can easily filter for lower
> just as easily as direction, correct? Show me pseudocode for that? If it is
> trivial, let's not worry about it.
>
> For relative (6): I agree with what you recommend, can we decide that now as
> well?
>
> FOr The resolution default. But on what kind of fit? for a multivariate
> response with a large posterior I was running out of ram.
>
> For The name of the meta element recording the lowest concentration in the
> data (T21). This should be "control" - it is what we are assuming it is.
>
> For #3 What is your recommended depracation path? I agree it would be
> helpful - should be quite simple?
>
> For #22 Not at this stage. Can we link anchor = "control" to an added
> argument which is the control fit model object?
>
> For #17, #18, #25 — NOEC, model-averaged N(S)EC, N(S)EC. What does this
> require exactly?

**Claude:** Recorded in `02_decisions.md`: T29 (`relative`), T30 (no argument
for `type = "lower"`), `meta$control` in T21, `control_fit` in T23, `posterior`
in place of `relative` in T22's list of `lifecycle` deprecations, and #22 "not
at this stage". The `type = "lower"`, `relative` and `meta` name items left
"Still to decide"; the `resolution` item was rewritten with a new
recommendation, and #3 gained a recommended path. `REFACTOR-claude.md` and
`REFACTOR-human.md` were changed where they named a new argument for
`type = "lower"`, called `anchor = "control"` unimplemented, deprecated
`relative` through `lifecycle`, or counted five open questions.

#### The `type = "lower"` filter

The rule in `extract_nsec_multi()` (`R/helpers.R:38-57`) keeps, per response,
the direction whose NSEC realisations have the lower `criterion` quantile, with
a tie going to the decreasing direction. On the planned tibble with
`draws = TRUE` it is:

```r
nsec(fit, multi_var = "sp", draws = TRUE) |>
  dplyr::mutate(
    at_criterion = vapply(draws, \(d) quantile(d$value, 0.8), numeric(1))
  ) |>
  dplyr::group_by(response) |>
  dplyr::slice_min(at_criterion, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
```

It was run on `compute_nsec()` output for `brms_model_4` grouped by
`herbicide`, with `group` in place of `response` and the per-realisation
vectors in place of `d$value`, and returned one row per herbicide. Being five
lines, it was recorded as T30: `nsec()` has no argument for it, and the
deprecated `nsec_multi()` applies it.

#### `relative`

Recorded as T29, as recommended in the first round. The warning is a plain
`warning()`, as in `bayesnec` D15 ruling 8, because `relative` is not
deprecated and `lifecycle::deprecate_warn()` would report that it is. This
amends the mechanism T9 named.

#### The `resolution` default and memory

The fits measured in the first round were univariate or grouped, with at most
seven curves and 1000 draws, and the measurement was of time, not memory.
Memory was then measured as the peak resident set size of a fresh R process
(`VmHWM` in `/proc/self/status`), one call per process, on WSL R 4.6.1. After
`devtools::load_all()` and loading the fixture the process held 775 MB.

| call | grid points | peak above that (MB) |
|---|---|---|
| `nsec_multi()`, `nsec_multi_model_2`, two responses, 1000 draws | 50 | 182 |
| | 1000 | 173 |
| | 4000 | 444 |
| | 16000 | 1265 |
| `toxval_predict()` with `compute_nsec()` and `compute_ecx()`, `brms_model_3`, 1000 draws | 1000 | 56 |
| | 4000 | 210 |
| | 16000 | 498 |

Above about 1000 points the peak grows by about 36 bytes per draw, grid point
and response for `nsec_multi()`, and 30 bytes per draw and grid point for the
spine on one curve: four to five times the 8 bytes of each stored prediction.
Grid points stand in here for draws, since `posterior_epred()` returns draws
times points times responses. A fit with 8000 draws and four responses would
need about 1.2 GB at 1000 points. At 50 points, `nsec_multi()`'s default, the
grid accounts for under 1 MB on this fixture, so a failure at that resolution
has another cause.

Two alternative grids were tried on the three `drc` fits, swapping
`predict_grid()` in the loaded namespace, against the even 10000-point grid:

| fit | grid | 100 | 200 | 400 | 1000 |
|---|---|---|---|---|---|
| `drc::ryegrass` | even | 3.17 | 0.72 | 0.23 | 0.02 |
| | log | 28.4 | 28.4 | | 28.4 |
| | half even, half log | 3.36 | 3.17 | 0.72 | 0.04 |
| simulated 0 to 100 | even | 8.34 | 2.04 | 0.34 | 0.33 |
| | log | 0.17 | 0.03 | | 0.00 |
| | half even, half log | 0.32 | 0.16 | 0.04 | 0.01 |
| `nsec_drc_1` | even | 0.30 | 0.06 | 0.02 | 0.00 |
| | log | 1.84 | 0.55 | | 0.02 |
| | half even, half log | 1.52 | 0.28 | 0.06 | 0.01 |

Values are the largest error in a reported quantile as a percentage of the 95%
interval width. The log grid was 0 followed by points evenly spaced in log(x)
from the lowest positive concentration, and was run at 50, 100, 200 and 1000
points. On `ryegrass` it leaves one grid interval between the zero control and
the lowest tested concentration, 0.94, where the lower NSEC bound (0.717) lies,
so its error does not fall with resolution. The half-and-half grid reduces the
error only on the geometric design.

The recommendation changed from 1000 to 500. On the even grid the simulated fit
reached 0.34 per cent at 400 points and 0.33 at 1000, so points above about 500
reduced the error on no fit tested and doubled memory. Root-finding in
place of the grid (#40) would remove the trade-off.

#### `meta$control`

Recorded in T21 as ruled. `toxval_pred` already has a `control` slot (T13),
holding response realisations from a control-only fit, and the output has a
`control` column, the control response. `meta$control` is a concentration. The
entry states the difference; `x_control` would match `x_var` and `x_range` if
the overlap is to be avoided.

#### #3, `ecx_val` as a proportion

Recommended path, recorded under "Still to decide": a new argument taking a
proportion, with `ecx_val` accepted for one release behind
`lifecycle::deprecate_warn()` and divided by 100, except under `direct`, where
it is a response value. The argument cannot change scale in place, because
`chk_ecx_val()` (`R/validate.R:19`) accepts any positive value under
`absolute`, so `ecx_val = 0.5` would change meaning between releases with no
error. `ecnsec` becomes a proportion in the same release (T8). The new argument
can take several values from the start, which answers #9. It goes with the
tibble, as a breaking release. The code change is small. The work is in the
tests, the documentation, `ecnsec`, and `msPAF`, which calls
`ecx(x, ecx_val = proportion * 100, ...)` (`msPAF` `R/hc_fun.R:31`).

#### #22 and `anchor = "control"`

`REFACTOR-claude.md` §3.8 already specifies `nsec(fit, anchor = "control",
control_fit = ctrl)`, with the control-only fit supplied by the user. The plan
had tied it to #22 in T23 and under "Still to decide"; both now say it does not
depend on #22. T23 puts `control_fit` in phase 4 for a `brmsfit`. `drc` cannot
fit a model of the control alone, so `anchor = "control"` errors on a `drc` fit.
The control fit must use the same family (§3.8) and be grouped as the
dose-response fit (T13). The warning when the two control estimates differ by
more than one standard error (§3.8) comes with it.

#### #17, #18 and #25

#17 (NOEC) needs a fit with concentration as a factor, which `drc` cannot
provide. `brms` can, and `glm` would need #21. `toxval_pred` would hold
realisations at the tested concentrations rather than on a grid. The
definition is to agree first, as #17 says:

- which level is the control;
- the test against it: a posterior probability against `sig_val` for a
  Bayesian fit, or Dunnett's or Williams' test for a frequentist one, which
  may need a new dependency;
- the multiplicity adjustment;
- whether the NOEC is the highest concentration with no significant effect or
  the one below the lowest significant one, which differ on a non-monotone
  response;
- what the result reports, since the estimate is a tested concentration with
  no interval.

#18 and #25 ask for the same estimate, the model-averaged N(S)EC of Fisher et
al. (2024); #25 is the method, and the comment of 2026-08-19 sets out what
`toxval_pred` must hold. Three pieces are needed:

1. `nec` for one threshold fit, from the `threshold` slot. That needs an
   argument naming the NEC parameter of a `brms` non-linear formula, as T24
   does for the asymptote, and the bootstrap of the NEC parameter of a `drc`
   NEC model.
2. A model-averaged `toxval_pred`, built from several fits and their weights,
   with one component index (`meta$draw_model`) drawn once, so that
   realisation *i* is the same component draw in every slot. `bayesnec` builds
   this for a `bayesmanecfit` at phase 7. For `drc` or `brms` fits it is new,
   with the weights (AIC for `drc`, stacking or pseudo-BMA for `brms`) to
   agree.
3. The estimator, which takes the NEC draw from a threshold component and the
   NSEC draw from a smooth one. Its definition is to agree: how `sig_val` and
   `anchor` apply to the NSEC part (§3.8 notes that the mixture combines a
   `"model"`-style and a `"component"`-style quantity), and what `ecnsec`
   reports.

For `bayesnec` fits most of this falls to `bayesnec` at phase 7. One of #18 and
#25 could be closed as a duplicate.

#### Commits

1. the commit adding this entry, with the changes to `02_decisions.md`,
   `REFACTOR-claude.md` and `REFACTOR-human.md`

---

## Session: phase3-shed-bayesnec, the phase 4 rulings, third round
Date: 2026-10-19
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:**

> I am happy with your new resolution recommendation - mark it as decided
>
> FOr meta$control, yes I agree use x_control, mark it as decided
>
> For #3 - ok I agree. Yes I think a new slot "proportion". Unless there is a
> clash, mark that as decided
>
> For anchor = "control", the motivation was alignment with the method in
> Rworking\comparing_tox_ests\refrences\IEAM 2013 - deBruyn and Elphick -
> Proposing a vigorous hybrid of the LOECs and ECx.pdf and the current case
> study work. But I recall there was an error. Using the control as the
> reference, would this still align?

**Claude:** Recorded three rulings and answered the `anchor` question; T23 is
unchanged until it is ruled on.

#### Rulings recorded

- T31: `resolution` defaults to 500 at every entry point. The evidence that
  was under "Still to decide" is now in the entry.
- T21: the `meta` element is `x_control`. The `x_` prefix, as in `x_var` and
  `x_range`, separates it from the `control` slot (T13).
- T32: `proportion` replaces `ecx_val` (#3), by the path recommended in the
  second round, in the phase 5 release. `ssdtools` took the same path in
  2.0.0, deprecating `ssd_hc(percent)` for `proportion` and dividing by 100
  (read from the installed `ssdtools` 2.6.0). `msPAF`'s `hc_fun()` already
  passes one `proportion` argument to `ssd_hc()` and to `ecx()`.

The name clashes in one place. Under `type = "direct"` the value is a
response, not a proportion, so `ecx(fit, type = "direct", proportion = 30)` for
a count response would name it wrongly. T32 excludes `direct`, and "Still to
decide" now asks which argument takes it. Recommended: `reference`, the output
column that already reports that value. `response` is not available, because
it names the column that identifies each response of a multivariate fit. No
other clash was found: no argument or internal function in `R/` is called
`proportion`; the word appears there only in documentation of a response on
the proportion scale.

#### `anchor = "control"` and the LECx

de Bruyn and Elphick (2013) give two calculations of the LECx. Method 1 reads
the fitted curve at an x per cent reduction from the control, with x the
percent minimum significant difference (PMSD) of Dunnett's test. Method 2,
for tests without replication, reads it at the 95 per cent confidence band on
the baseline response. In `comparing_tox_ests` (`compare_nsec_lecx.qmd`) and
in the CETIS sheets of the cerium case study, both are read relative to the
modelled control response, b0. The control observations enter method 1 only
through the PMSD.

The error remembered is the NSEC-cv of the June 2026 version of
`compare_nsec_lecx.qmd`, withdrawn on 2026-09-29 (`comparing_tox_ests`
20987a2). It fixed b0 at the observed control mean and set the reference at
the one-sided bound on that mean, t(n_control - 1) x SD_control /
sqrt(n_control). That document had said its effect size was identical to the
LECx effect size. It is not: its standard error omits the treatment term
1/n_treatment, it uses the control variance in place of the pooled variance,
and it uses t in place of Dunnett's critical value.

`anchor = "control"` as `REFACTOR-claude.md` §3.8 defines it is the Bayesian
form of that NSEC-cv: the `sig_val` quantile of an intercept-only fit to the
controls, defined for a curve constrained through the control estimate. The
frequentist forms were computed on the fish growth data of
`compare_nsec_lecx.qmd` (21 observations, seven concentrations with three
replicates each, the exponential decay fitted by `nls()`, one-sided alpha =
0.05; script `scratchpad/anchor_control_lecx.R`, not kept):

| threshold | reference | effect (%) | concentration |
|---|---|---|---|
| NSEC, `anchor = "model"` (LECx method 2) | 5.67 | 8.6 | 5.37 |
| `anchor = "control"`, unconstrained curve | 5.75 | 7.2 | 4.87 |
| `anchor = "control"`, curve through the control mean (NSEC-cv) | 5.75 | 13.4 | 4.84 |
| LECx method 1 (Dunnett PMSD) | 4.53 | 27.0 | 10.9 |

The NSEC-cv row reproduces the 13.4 per cent the June version reported. A
control-only reference therefore aligns with neither LECx method, and on these
data it lies further from method 1 than the NSEC does. The fitted b0 (6.20) is
1.44 standard errors of the control mean below the observed control mean
(6.64), so the mismatch warning of §3.8 would fire.

LECx method 2 is already `nsec()` with the default `anchor = "model"`. LECx
method 1 is `ecx()` at x = PMSD, which reads each realisation at its own
control less that percentage, as the case study does. It needs the PMSD from a
Dunnett's test on the data, outside `toxval`. Computing the PMSD inside
`toxval` would be a new metric, with the requirements set out for the NOEC
(#17).

Recommended: leave `anchor = "control"` and `control_fit` out of phase 4,
since alignment with the LECx was their motivation and does not hold. To be
ruled on.

#### Commits

1. the commit adding this entry, with the changes to `02_decisions.md`,
   `REFACTOR-claude.md` and `REFACTOR-human.md`

---

## Session: phase3-shed-bayesnec, the phase 4 rulings, fourth round
Date: 2026-10-19
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:**

> I think phase 4 and 5 should be done together, and I think Ayla will agree.
> Let's mark that decided. She can review the PR and indicate if she is happy
> with that, as we don't meet until the end of the week and we really need to
> move this forward at a faster rate than that.
>
> I agree with the problem regarding proportion and type="direct".Can you check
> drc's equivalent? can we use that? If there is something useful there,
> consult with me, otherwise call the argument reference as you suggested and
> change that and update as required.
>
> I agree leave anchor = "control" and control_fit out of phase 4. Can you
> update the issue with a comment detailing that it should probably be
> replaced, given the correction in the other repo.

**Claude:** Recorded the three rulings and posted the comment on #63.

#### Phase sequencing

T33: done together, subject to AP's review of the pull request that records
it. The text that was under "Still to decide" is now the entry. The phase
rows and the paragraphs after them in both plan documents now say phases 2 and
3 stay separate and phases 4 and 5 are done together.

#### `type = "direct"` and `drc`

`drc::ED()` (drc 3.0.1, `args(drc:::ED.drc)` and its help page) was checked.
Its `type = "absolute"` is toxval's `direct`, and it takes the response value
through `respLev`, the same argument that holds the percentage under
`type = "relative"`. That is the conflation T32 removes, so there is nothing to
adopt. `drc::ED()` has its own `reference` argument, `c("control", "upper")`,
which selects where the span starts; the name means something different there.
toxval's `type` names already differ from `drc`'s (`drc`'s `absolute` is
toxval's `direct`), so matching `drc` names was not a goal. The argument is
`reference`, as recommended. T32 records it: required under `direct`, refused
under any other `type`; `proportion` refused under `direct` (T3); the
`proportion` column `NA` on a `direct` row; `ecx_val` under `direct` taken as
`reference`, undivided, in the deprecation release. §3.1 and §3.10 of
`REFACTOR-claude.md`, and §3.1 and phase 5 of `REFACTOR-human.md`, refer to it.

#### `anchor = "control"`

T23 now gives `anchor = c("model", "component")` for phase 4 and records why
`"control"` and `control_fit` are left out, with the fish growth figures of the
third round. §3.8 and §5 of both plan documents, and the #22 entry, say the
same. The §3.8 design is kept as the record of what was proposed.

Comment posted on #63 (2026-10-19,
https://github.com/open-AIMS/toxval/issues/63#issuecomment-6264494441). It
states that `"control"` is left out of phase 4 and should probably be replaced
by a route to LECx method 1, with the table of the third round, and links the
`comparing_tox_ests` correction (20987a2, on its `ce-case-study` branch, not
yet merged to its `main`).

#### Commits

1. the commit adding this entry, with the changes to `02_decisions.md`,
   `REFACTOR-claude.md` and `REFACTOR-human.md`

---

## Session: phase3-shed-bayesnec, the #63 comment on trials fits
Date: 2026-10-19
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** ne final thing, can you asses how this comment here changes the plan?
https://github.com/open-AIMS/toxval/issues/63

**Claude:** Assessed the comment of 2026-10-19 08:32 on #63 (extra risk on a
`brms` beta-binomial fit whose trials are a normalising total). The plan was not
changed; a ruling is needed.

#### Effect on T18 and T26

T18 classes a response as a proportion from `meta$family` alone. For a `brms`
binomial or beta-binomial fit, `response_upper_bound()` (`R/compute.R`) returns
`NA` today, because the trials are not recorded. T26 records `trials_var` per
curve in phase 4 and lets T18's increasing `absolute` reference use it. Phases
4 and 5 are one release (T33), so that release would be the first to report an
increasing `ecnsec` for the 78-gene fit of the comment, and it would report
extra risk: 0.65 to 2.7 per cent for every gene, against 26.8 to 110 per cent
as relative deviation (the comment's table).

The increasing ECx is affected in the same way, since `ecx` and `ecnsec` share
the span (T8). From the comment's control-to-trials ratios (0.0097 to 0.038),
an increasing EC10 under extra risk is reached where the expected count is
1 + 0.1 (n / y0 - 1) times the control: 11.2 times at 0.0097, 5.9 at 0.020 and
3.5 at 0.038, against 1.1 times under relative deviation. Most such crossings
would lie beyond the tested range and be censored (T21). Not affected: the
NSEC, whose reference does not depend on `type`; the decreasing direction,
whose `absolute` span is the control for every family; `range`, `relative` and
`direct`.

#### Options

1. A predict-level argument stating whether the trials bound the response,
   recorded in `meta` beside `trials_var` and read by `response_upper_bound()`.
   `FALSE` makes the fit a count with no upper bound, so `absolute` gives
   relative deviation under T18 as it stands. The default stays an upper bound,
   as the comment proposes. `type` stays a string, and #63 option 1 is not
   needed for this case. Suggested name `trials_bound`, to be chosen.
2. The comment's proposal, #63 option 1 in its second form
   (`type = list(increasing = "control")`). It brings the change of the `type`
   contract, and its naming, into phase 4, which #63 says to stop and ask about,
   and the `type` column of the result would have to represent a list.
3. Keep the increasing `absolute` reference `NA` for trials fits until #63 is
   settled. It also leaves a binomial count of organisms, such as mortality,
   with no increasing reference for that release, which is the case T18 was
   written for.

Recommended: option 1. The comment's diagnosis is that the family does not show
whether the trials are a ceiling, which is a property of the response rather
than a choice of ECx definition, so it is recorded where `trials_var` is. It
amends T26's third bullet and T18's recognition rule, and returns one open item
to phase 4. #63 keeps the general question of user-supplied spans.

---

## Session: phase3-shed-bayesnec, the #63 trials ruling
Date: 2026-10-19
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** can I clarify, what would we get for a properly bounded dataset (ie
that has at least one replicate with 1) if we use the fold change for all
families?

**Claude:** Relative deviation (fold change) for every family fails on a
bounded proportion. Simulated mortality data, `drc::drm(..., fct = LL.3u(),
type = "binomial")`, concentrations 0, 1, 2, 4, 8, 16, 32, four replicates of
20, mortality reaching 1, one seed (20261019) run across true control
mortalities 0.01, 0.05, 0.20 and 0.50. Each increasing ECx was read under extra
risk, `P0 + x (1 - P0)`, under fold change, `P0 (1 + x)`, and on the same data
coded as survival with the decreasing `absolute` reference. Extra risk equalled
`drc::ED()` in every run.

- Control 0.01 (fitted 0.0138): extra risk EC10, EC50, EC90 = 2.18, 5.94,
  16.2; fold change 0.30, 0.62, 0.81, at 1.5, 2.1 and 2.6 per cent mortality,
  all below the lowest tested concentration.
- Control 0.05 (fitted lower limit -0.0009): every fold-change ECx `NA`, since
  the span is at or below zero (T18).
- Control 0.20: extra risk 2.00, 6.57, 21.6; fold change 0.98, 2.51, 3.74.
- With control P0, fold change cannot reach any x above 1/P0 - 1.
- The survival-coded decreasing reading equalled extra risk in all four runs
  (Abbott's formula), so fold change on mortality makes the ECx depend on
  whether deaths or survivors are modelled.

Observed 1s are not a test of a bounded response: a test whose top
concentration kills 60 per cent is still bounded at 1.

**User:** I think we can leave it as it is for phase 4, and deal with it via
option 2, at the time that is tackled down the track. This is likely an edge
case. Can you rule on that?

**Claude:** Recorded in T26: a fit whose trials are a normalising total also
takes extra risk in phase 4, accepted as an edge case, to be handled by a span
the user names (#63 option 1 in its second form) when #63 is taken up.
`REFACTOR-claude.md` §3.10 states it with the #63 figures and the simulation
above; `REFACTOR-human.md` §2 gives one sentence.

#### Commits

1. the commit adding this entry, with the changes to `02_decisions.md`,
   `REFACTOR-claude.md` and `REFACTOR-human.md`

---

## Session: phase3-shed-bayesnec, the phase 4 and 5 prompt
Date: 2026-10-19
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** update the phase 4 prompt. Can you clarify, Ayla can also see our new
plan for the combined phase 4 and 5 migration, yes?

**Claude:** The phase 4 prompt was rewritten for phases 4 and 5 together, as
`/mnt/c/Rworking/agent-prompts/2026-10-19-phase4-5-spine-and-tibble.md`; the
version of 2026-10-18 was moved to `agent-prompts/superceded/`. The pending
rulings R1 to R13 are replaced by a summary of T21 to T33. The parts are: 0,
the regression net, with the baseline values compared against the decreasing
rows of the new output (T33); A, the `toxval` tibble class built first and
additively; B, the summary layer, with `meta$x_control`, T21, T25 and the
`draws` list column; C, `ecx()` with `proportion` and `reference` (T32), the
`drc` route (T27) and the `posterior` deprecation; D, `nsec()` with `anchor`
(T23), both directions for `drc`, and `xform`; E, multivariate fits and the
`nsec_multi()` wrapper with the T30 filter; F, `relative` (T24, T29); G, removal
of the old code. The branch is `phase-4-5-spine-and-tibble` from
`phase-3-shed-bayesnec` at `4e7648a` or later.

Two points no T-entry covers are written into the prompt from the plan's own
rules, for Becky to confirm:

- `nsec_multi()` returns the `toxval` tibble, filtered as its `type` asks,
  since every metric function returns one (`REFACTOR-claude.md` §3.1); its
  deprecation message says the shape has changed.
- `tibble` joins `Imports`, since the output is a `tbl_df` subclass (#4), and
  `pillar` only if `tbl_sum()` needs it. Both are installed with `dplyr`.

Ayla can see the decisions and the plan documents in #65, but not the prompt,
which is outside the repository.

---
