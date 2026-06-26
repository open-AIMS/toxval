## Session: test-symmetry-audit
Date: 2026-06-26
Model: claude-sonnet-4-6

### Prompts and Responses

**User:** Can you go through the current test suite. For any tests that are available for one method (eg nsec, ecx or nsec) can you check if there is an equivalent test for the other. If there isn't determine if that is because it doesn't apply to the other method. If it does apply, please add that test to the tests for that method.

**Claude:** Audited all five test files (test-ecx.R, test-nsec.R, test-nsec-multi.R, test-helpers.R, test-predict.R) and all source functions (ecx.R, nsec.R, helpers.R, data-raw/DATASET.R).

Scope: compared ecx vs nsec (nsec_multi is its own unique method; helpers and predict tests are not method-parallel).

Key structural differences between methods:
- ecx has `ecx_val` and `type` (relative/absolute/direct); nsec has `sig_val` and `horme`
- nsec has a drc method; ecx does not
- nsec.brmsfit wraps `posterior_epred` in `try()`; ecx.brmsfit does not (dead code for the "Do you need to specify a group_var variable?" message)

Gaps identified and resolved:

**Added to test-nsec.R (patterns from ecx not yet in nsec):**

1. `brmsfit: group_var with non-string types (integer, logical) errors` — ecx tested
   group_var=1 and group_var=TRUE; nsec only tested a wrong string. Both methods share
   the same `max(grepl(group_var, col_names))==0` check, so the behaviour is identical.

2. `brmsfit: prob_vals with more than 3 values returns that many quantiles` — ecx had
   a 5-element prob_vals test; nsec had none for any method type. The nsec() generic and
   nsec.brmsfit both pass prob_vals through to quantile(), so this applies.

**Added to test-ecx.R (patterns from nsec not yet in ecx):**

3. `brmsfit: errors if additional groups present but group_var not specified` — nsec had
   this test and uses try() to produce a custom message. ecx.brmsfit has the custom
   message in dead code (posterior_epred is not wrapped in try()), but the call still
   errors (brms throws the error directly). Test added with no regexp so it documents
   the behaviour without depending on the brms error string.

Not added / not applicable:
- NaN catch tests (ecx type=absolute/relative/direct with x_range=-1): nsec has no `type`
  parameter and the computation path through nsec_fct/zero_crossings with NaN input is
  ambiguous without running R. Flagged as a potential future test.
- ecx "errors if additional groups" with custom message: ecx.brmsfit has dead code for the
  custom re-throw; adding a test for the custom message would give a false failure.
- nsec_multi tests: unique to that function, no ecx equivalent needed.
- drc tests: nsec-only method, no ecx.drc exists.
- ecx `group_var=NA` + `posterior=TRUE`: already covered by the existing
  "posterior = true outputs the posterior" test (group_var defaults to NA).
- Resolution range validation (resolution=1, -2, 0): these test seq() edge cases
  rather than method behaviour; not symmetric enough to add across methods.

---

---

## Session: man-page-coverage-audit
Date: 2026-06-26
Model: claude-sonnet-4-6

### Prompts and Responses

**User:** Do one more pass examining the test suite in the context of the stated outputs/expectations in the man pages for each.

**Claude:** Audited all four man pages (ecx.Rd, nsec.Rd, nsec_multi.Rd, predict.Rd) against the five test files and all four source files.

Approach: For each documented parameter and return value, checked whether a test verifies the stated behaviour. Also read source to identify where documented behaviour diverges from implementation.

**Gaps found and tests added:**

### ecx (test-ecx.R — 2 new `if (FALSE)` tests)

**ecx_val validation missing for bnecfit.** Man page: "must be a value between 1 and 99 (for type = 'relative' and 'absolute')." This validation (`ecx_val < 1 || ecx_val > 99`, `length(ecx_val) > 1`) is in `ecx.brmsfit` only — it is NOT in the `ecx` generic or `ecx.bnecfit`. Passing `ecx_val = 0` or `ecx_val = c(10, 50)` to a bnecfit silently succeeds.
→ Added `if (FALSE)` tests with TODO noting the bugs; tests document expected behaviour so they can be activated when the refactor adds generic validation.

### nsec (test-nsec.R — 2 new passing tests, 2 new `if (FALSE)` tests)

**Default resolution not explicitly tested per method.** Man page generic shows `resolution = 100`, but `nsec.brmsfit` and `nsec.drc` default to `resolution = 1000`. No test previously called nsec.bnecfit or nsec.brmsfit without specifying resolution and checked the stored attribute.
→ Added `"bnecfit default resolution is 100"` and `"brmsfit default resolution is 1000"` (both passing).

**nsec.drc xform not applied (single-curve case).** Man page: xform is "A function to apply to the returned estimated concentration values." In `nsec.drc` with no `curveid`, the code reads `xform(nsec_out)` but does NOT assign the result, so `out_vals` uses the untransformed `nsec_out`. The existing test uses `>=` which passes trivially even when outputs are identical.
→ Added `if (FALSE)` test checking `output_2 == output_1 * 2`, with TODO noting the bug.

**nsec.drc output is unnamed.** `nsec.brmsfit` and `nsec.bnecfit` return named vectors (`Q50`, `Q2.5`, `Q97.5`); `nsec.drc` calls `as.numeric(unlist(nsec_out))` which strips all names. Inconsistency with the documented uniform interface.
→ Added `if (FALSE)` test checking `names(output) == c("Q50", "Q2.5", "Q97.5")`, with TODO.

### predict (test-predict.R — 1 new passing test, 1 new `if (FALSE)` test)

**predict.bayesnecfit never directly called.** Man page documents `predict.bayesnecfit(object, ...)`. The test file deliberately avoids calling `predict(ecx4param)` due to a known bug: `predict.bayesnecfit` calls `pull_brmsfit()` without the `bayesnec::` namespace prefix, which fails when bayesnec is in `Imports` (not `Depends`) and therefore not attached.
→ Added `if (FALSE)` test with skip and TODO explaining the bug.

**predict.bayesmanecfit probs with >2 values not tested.** Man page: probs is "The percentiles to be computed by the quantile function." Tests only used 2-element probs. With `probs = c(0.1, 0.5, 0.9)` the source produces a 5-column matrix (`Estimate`, `Est.Error`, `Q10`, `Q50`, `Q90`).
→ Added `"predict.bayesmanecfit probs with 3 values returns 5-column matrix"` (passing).

### nsec_multi (test-nsec-multi.R — 3 new passing tests)

**criterion entirely untested.** Man page documents `criterion = 0.8` as "The criterion to use when type = 'lowest'." (Note: man page has a typo — valid type is `"lower"`, not `"lowest"`.) In `extract_nsec_multi`, criterion is used when `type = "lower"` to compare `quantile(nsec_inc, criterion)` vs `quantile(nsec_dec, criterion)` and select whichever direction has the lower quantile value.
→ Added three tests: (1) different criterion values change direction selection, (2) criterion=0.8 explicit matches default, (3) criterion has no effect for type="both".

**Not added:** Tests for the nsec man page's "credible intervals" language applied to drc (frequentist confidence intervals) — this is a documentation accuracy issue, not a test gap.

