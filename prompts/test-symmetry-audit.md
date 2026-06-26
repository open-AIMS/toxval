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
