## Session: phase2-pr59-review
Date: 2026-10-09
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** Ayla sent through a PR, can you help me work through it?

**Claude:** Reviewed PR #59 (`phase-2`) at 204fdd5: `new_toxval_pred()` and its
validator (12 September), and `toxval_predict()` with `brmsfit` and `drc`
methods plus the parametric bootstrap in `R/bootstrap.R` (9 October). No file
was changed at this stage. Findings, each measured:

- The R-CMD-check failure (`test-toxval-predict.R:633`, one replicate of
  `nsec_drc_1`, `seed = 3`) comes from `MASS::mvrnorm()`, which factorises
  `vcov()` with `eigen()`. Eigenvector signs depend on the LAPACK build:
  reversing the sign of the third eigenvector reproduced the CI values
  (0.9058, 0.8999, 0.4515, 0.0246) from the WSL draw. A seed therefore does not
  reproduce a bootstrap across machines. Proposed a Cholesky draw, which is
  unique for a positive-definite matrix:

  ```r
  z <- matrix(stats::rnorm(n_boot * length(mu)), nrow = n_boot, byrow = TRUE)
  parms <- sweep(z %*% chol(sigma), 2, mu, "+")
  ```

- For a grouped `drc` fit with a character `curveid`, curve order follows the
  row order of the data (daphnids with rows reversed gave `48h`, `24h`), while
  the `brmsfit` method sorts.
- A `drc` `group_var` is only checked to name a column: `group_var = "dose"` was
  accepted and recorded. `object$data[[group_var]]` returns drc's integer codes;
  the labels are in the next column of the same name.
- `re_formula = NA` gives identical curves for a `group_var` that enters only
  through a group-level term. Checked on 30 simulated observations at three
  levels of `z` (intercepts 2 units apart), `brm(y ~ x + (1 | z))`, one chain of
  600 iterations: three identical curve matrices.

---

**User:** For 3, I think for random effects specifically we should drop group
level effects. The point of the random effects is that these are accommodated,
but that the estimates are population level effects. If the user was interested
in the group level effects they would fix them as fixed.

**Claude:** Recorded as a decision: `re_formula = NA` stays, and a `group_var`
with only group-level terms becomes an error (now T16). The guard compares the
curves rather than parsing the formula, so it covers nonlinear formulas.

---

**User:** I thought we already ruled [grouped multivariate] the other way
previously. We may want to fit a grouped multivariate analysis?

**Claude:** Retracted the earlier recommendation, which rested on
`nsec_multi()` having no `group_var`. The record: Becky's #20 decisions comment
(2026-09-04) deprecates `nsec_multi` as the multivariate case of `nsec`, which
makes `nsec(fit, group_var = , multi_var = )` valid; §3.1 already lists `group`
and `response` as independent columns; Ayla's session
(`prompts/phase2-build-spine.md:811-930`) outlined a descriptor table and left
the choice to Becky. Recommended adopting it in phase 2, because `bayesnec`
constructs `toxval_pred` from phase 7 and a later change would break both
packages.

**User:** Yes we want to do it now.

**Claude:** Adopted as T14: a `keys` data frame, one row per curve, columns
drawn from `group` and `response`, replacing the names of `curves` and
`meta$dimension`.

---

**User:** Should we make the changes we are suggesting ourselves? Also she
specifically wanted me to rule on things she flagged; step through them one at a
time.

**Claude:** Recommended that Ayla make the code changes, since she is still
building on the branch, and that the plan edits recording Becky's decisions be
made here. Stepped through sixteen items. Rulings:

1. `control` as one vector per curve: agreed (T13).
2. `meta$resolution` and `meta$x_range` cross-checked against `x_vec`: kept as
   Ayla wrote it. Dropping both fields, for consistency with deleting
   `dimension`, was offered and declined.
3. `group_var` and `multi_var` checked in both directions: agreed, now against
   the columns of `keys`.
4. `nsec_multi()` folded into `nsec()` (T15). `type = "lower"` must be renamed,
   because `type` in `nsec()` selects the `ecnsec` definition; the name is left
   to phase 4.
5. `x_var` for `drc`: optional, read from `object$dataList$names$dName`, and
   checked when supplied (T17). Requiring it and checking only for a column
   accepted `x_var = "y"`, building the grid over the response (0.001 to 0.98)
   instead of the doses (0.032 to 3.221).
6. Bootstrap mean test (line 405): replaced. At 4000 replicates, `seed = 42`,
   the mean curve differed from the fitted curve by 4 to 5 Monte Carlo standard
   errors (0.0376 against 0.0351 at x = 3), because the curve is nonlinear in
   its parameters. Replaced with a test of the full pipeline with
   `boot_parms()` mocked to `coef()`, compared with `predict()`, and a test of
   the draw on the parameter scale (means within 4 MC SE, variances within 10%,
   correlations within 0.05). Both pass under the `MASS` and Cholesky draws;
   reversing the parameter columns, or drawing without correlation, fails them.
   Needs testthat 3.1.7 for `local_mocked_bindings()`.
7. Joint draw test (428): approved.
8. Row-by-row test (461): checked row 1 only, and passed an implementation
   applying the first parameter row to every row. Replaced with a loop over
   rows.
9. `drc_split_parm_names()` (477): split on the last colon, so curve levels
   `t:24h`, `t:48h` were returned as `24h`, `48h`. drc parameter names contain
   no colon. Changed to the first colon, with a test fitting colon levels.
10. Unsplittable names (493): approved.
11. Shared parameters under `pmodels` (501): the hard-coded values were
    bootstrap means, and failed under the Cholesky draw (0.0021 against a
    tolerance of 0.002). Replaced with the mocked comparison against
    `predict()`.
12. Missing coefficient (548): approved; message changed from "may not have
    converged" to "a parameter may not be estimable from the data", since a
    non-converged `drm()` with `errorm = FALSE` returns a plain list, never a
    `drc` object with `NA` coefficients.
13. Missing `vcov()` value (561): approved; a companion test for a
    non-positive-definite `vcov()` to come with the Cholesky change.
14. Herbicide order (574): approved; `group_levels()` changed to
    `sort(method = "radix")`. testthat collates in C, a user session in
    `en_US.UTF-8`, and mixed-case levels sort differently (`a A b B` against
    `A B a b`).
15. Declared factor order (600): the levels `control`, `high` are already
    alphabetical, so the test passed an alphabetical implementation. Added
    `medium` so the declared, alphabetical and appearance orders all differ.
16. Single replicate (615): kept as the `n_boot = 1` edge case, with values
    regenerated under the Cholesky draw (0.9002389, 0.8941983, 0.4380216,
    0.0091685) and tolerance 1e-6.

Proposed tests were run against the current code and against deliberately
broken versions, and formatted with `air`. Drafts are in `notes/drafts/pr59-*`.

---

**User:** Yes, batch the plan edits at the end.

**Claude:** Recorded T13 to T17 in `notes/implementation/02_decisions.md`, with
the full text in `REFACTOR-claude.md` (§3.3 T15; §3.4 the `toxval_pred` block,
the case table, T13 and T14; §3.6 a pointer to T15; §3.7 predict-level
arguments, T16 and T17; phase 4 and §5) and one sentence each in
`REFACTOR-human.md`. Phase 4 now states that folding `nsec_multi()` needs
`toxval_predict.brmsfit()` to return one curve per response first. Left open
in the spec: the type of the `keys$group` column.

---
