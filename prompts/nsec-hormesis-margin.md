## Session: nsec-hormesis-margin
Date: 2026-08-20
Model: Claude Opus 5 (claude-opus-5)

Follow-on to `tier1-mechanical-issues.md`, on the same branch
(`issue-37-nsec-todo-markers`, PR #48).

### Prompts and Responses

**User:** Confused about PR #48 — why are we not fixing the `MARGIN = 2` issue?
It is clearly wrong and was causing the nonsensible test results. Wouldn't
fixing it be a cleaner way to resolve this ToDo?

**Claude:** Confirmed, with measurement on the `brms_model_1` fixture.

`R/nsec.R:246` and `:273` were **live** code in `nsec.brmsfit()`:

```r
reference <- quantile(apply(p_samples, 2, max), probs = sig_val)
```

`nsec.bnecfit()` (`R/nsec.R:118`) uses `MARGIN = 1`. `p_samples` is
draws x x-values, so `MARGIN = 1` is one hormetic peak per draw — a posterior
quantity whose `sig_val` quantile means something — while `MARGIN = 2` is the
pointwise upper envelope across draws, quantiled over x values.

Measured (1000 draws x 1000 x-values, `sig_val = 0.01`):

| reference | value |
|---|---|
| `MARGIN = 2` (live) | 1.297 |
| `MARGIN = 1` (bnecfit) | 0.402 |
| control column | 0.088 |
| range of median curve | 0.283 – 0.904 |

1.297 sits above the whole response surface, so `nsec_fct()` found no zero
crossing and hit its `return(max(x_vec))` fallback (`R/helpers.R:125`). That
reproduced the golden value `c(1.05, 0.8213543, 1.05)` at `test-nsec.R:585`
exactly: the test was locking in a fallback, not an estimate.

PR #48 was worked under a Tier 1 "changes no numbers" brief and folded the
margin into the #20 hormesis/direction question. The scope call was defensible;
treating the margin as undecidable was not — it is live code and one side of it
is simply wrong.

---

**User:** Make the change on the PR branch and add a comment to the PR saying
what was actually fixed.

**Claude:** On `issue-37-nsec-todo-markers`:

- `MARGIN = 2` -> `1` at both hormesis sites in `nsec.brmsfit()`, with a comment
  recording why and what the old margin did.
- Dropped the two dead `nec_posterior <- as_draws_df(object$fit)[[...]]`
  assignments (assigned, never read; `modify_posterior()` does not take a nec
  posterior). The `@importFrom brms as_draws_df` was left in place rather than
  regenerating `NAMESPACE` for a one-line change.
- Replaced the long "two undecided things" comment with the remaining one: the
  `modify_posterior` truncation still cannot be restored. Verified rather than
  assumed — with the margin fixed *and* the bnecfit ordering (reference first,
  then truncate), it still errors, because draws peaking at the last x value are
  left with one non-NA point:
  `Error in stats::approxfun(seq_along(x), x) : need at least two non-NA values to interpolate`.
  So #48's keep decision stands; its stated reason did not.
- Updated the two golden values, with a comment saying why they moved:
  `c(1.05, 0.8213543, 1.05)` -> `c(0.997366, 0.8938963, 1.05)`, and
  `c(1.05, 0.9612536, 1.05)` -> `c(1.05, 0.8596995, 1.05)`.
  `ecnsec_relativeP` is untouched by the fix, as expected.

`devtools::test()` -> `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 531 ]`, same count as
before. `air` run over both changed files.

Flagged for later: the grouped case stays pinned at 1.05 either way —
`brms_model_2` is not hormetic over its 0.8–1.05 x range, so that test would not
catch a regression in this path regardless of the margin.

---
