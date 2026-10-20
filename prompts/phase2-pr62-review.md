## Session: phase2-pr62-review
Date: 2026-10-17
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** Pull whatever Ayla has done recently and help me review and resolve
open issues. We think phase 2 might be nearly complete, and I will be handling
the next step, which is the `bayesnec` migration.

**Claude:** Reviewed PR #62 (`phase-2-compute` into `phase-2`, head `c5b56a7`),
which adds `compute_ecx()`, `compute_nsec()` and the metric validators in
`R/validate.R`. On an exported copy, with the `chk (>= 0.11.0)` requirement
relaxed to the installed 0.10.0, 852 of 853 expectations pass; the one error is
the 0.10.0 message wording "must be an matrix". Phase 2 meets its definition once
#62 merges, apart from the gaps below.

Findings, each measured on test fixtures without fitting a model:

- The spine reproduces today's per-draw `ecx` and `nsec` on the `nec4param`
  component of `manec_example` (100 draws, 200-point grid) to 5e-7. The old path
  uses `modelbased::zero_crossings()`, which calls `uniroot()` on a fixed
  100-point subdivision; `find_crossings()` interpolates exactly. The same
  subdivision is the cause of #29, so #29 closes when the metrics move onto the
  spine in phase 4.
- #15 is not guarded: `toxval_predict(brms_model_1, x_var = "x",
  group_var = "x")` returns five constant curves, because `epred_curve()`
  overwrites the grid column with each level.
- Under `absolute`, `ecx_reference()` gives a decreasing reference above the
  control when the control is negative, while `ecnsec` for the same draw is
  `NA`. T8 makes them inverses, so the reference should be `NA` too.
- `compute_nsec()` returns `NA` for a draw whose control is at or below the NSEC
  reference; `sig_val` of the draws are in this class by construction. Today's
  `nsec_fct()` returns `max(x_vec)` for them. Fisher and Fox (2023, ETC
  42:2019-2028, Table 3 and p. 2026) report 0 for them, and `bayesnec` D17
  (2026-09-12) returns the control concentration. On `bayesnec_ecx4param$fit`
  (100 draws, 200-point grid), today's upper limit is 3.221, the highest
  concentration tested, at every `sig_val` of 0.05 or more, and 2.243 at 0.025;
  with these draws at the control concentration the lower limit is 0.032 and the
  upper limit decreases from 1.141 (0.01) to 0.963 (0.20). This is the source of
  the irregular intervals above `sig_val = 0.025`.

---

**User:** (Replies point by point.) `sig_val` strictly between 0 and 0.5, noting
irregular intervals above 0.025 (Fisher et al. 2024 IEAM). The `absolute`
increasing bound should be an error, and further options (a `custom` type taking
named bounds, a `drc` type, literature on increasing ECx definitions) should be
outlined in an issue. Explain points 3 and 4 and #15. Move `relative` and
`anchor` to phase 4, folded into the issue from point 2, noting that `anchor`
also touches model averaging in `drc`, which the plan does not cover. Close #32.
Update the phase 3 plan, and advise whether skipping tests when `bayesnec` is
not installed is the right approach.

**Claude:** Closed #32 with a comment citing `fce7977` (#35). Published an
explainer with figures from the fixtures for points 3 and 4, #15 and the draws
below the NSEC reference. Cross-checked the last against `bayesnec` D15, D17 and
the #404 censoring summary (`summarise_censored()`, which keeps a censored
draw's rank and reports a quantile among such draws as a bound).

Phase 3 test dependencies, counted on `phase-2` at `cb53250`: every use of
`bnec_model_1`, `manec_example`, `nec4param`, `ecx4param` and the
`bayesnec_*` fixtures is in a test of a method phase 3 deletes, except that
`tests/testthat/setup.R` calls `bayesnec::nec_data`, `bayesnec::manec_example`
and `bayesnec::pull_out()` at top level, and one `drc` test in `test-nsec.R`
fits `bayesnec::nec_data`. A top-level call in `setup.R` fails every test file
when `bayesnec` is absent, so a per-test skip does not cover it. The `brmsfit`
inside the stored `bayesnec_ecx4param` and `bayesnec_nec4param` fixtures was
predicted with `brms::posterior_epred()` in a session with `bayesnec` not
loaded; `brms` provides `step()` for the NEC equations. Recommended storing what
the surviving tests read from `bayesnec` as fixtures, and reserving
`skip_if_not_installed("bayesnec")` for a test that compares against `bayesnec`
itself.

Plan edits on branch `phase3-plan` (from `phase-2` at `cb53250`), uncommitted:

- `REFACTOR-claude.md` §4: the phase 3 row now deletes the `bnecfit` methods
  with their tests, stores what the remaining tests and examples read from
  `bayesnec` as fixtures, and removes `bayesnec` from `Imports`. Its numbers
  column no longer says `ecx` on a `bnecfit` adopts the #19 answer, since phase 3
  deletes that method; `bayesnec` objects receive it at phase 7. The phase 4 row
  adds `type = "relative"` (asymptote stored on `toxval_pred`) and `anchor`,
  deferred from phase 2 (RF, 2026-10-17). New subsection "Phase 3 without
  `bayesnec`" gives the inventory above, the roxygen examples at `R/ecx.R:42`
  and `R/nsec.R:29` that call `library(bayesnec)`, the fixtures-not-skips rule,
  the `object$fit` route for checking a single `bayesnecfit` (not a
  `bayesmanecfit`), the user-visible change (`toxval::ecx()` on a `bnecfit`
  errors) and the done criterion.
- `REFACTOR-claude.md` §5 and `REFACTOR-human.md` §5: model averaging of `drc`
  fits (`drc::maED()`) is in no phase; `anchor = "component"` applies to it, so
  it is scoped with `relative` and the further `type` options.
- `REFACTOR-human.md` §4: phases 3 and 4 restated to match.
- `PHASE1_DEPENDENCY_UNTANGLE.md` §3 and Step 2: replaced "move to `Suggests`
  and guard tests behind `requireNamespace()`" with the fixtures rule and a
  pointer to `REFACTOR-claude.md` §4.

Drafted an issue on the ECx reference for increasing responses, user-supplied
bounds, a `drc` type, and the scheduling of `relative` and `anchor` (not yet
posted; draft in the session scratchpad as `issue-type-options.md`). A
literature and software search, run as a subagent on 2026-10-17, found:

- Every published definition except the hybrid and point forms sets
  y* = y0 ± p·S and differs only in the span S: relative deviation (S = y0;
  EFSA 2022 §2.6.2, BMDS 2.6.0.1 p. 67, OECD 2006 No. 54 ¶306–312), extra risk
  (S = 1 − P0; US EPA 2012), added risk (S = 1), standard deviation (S = SD0).
- toxval `absolute` is relative deviation when decreasing and extra risk
  (Abbott's formula) when increasing. Upward relative deviation, y0(1 + p), is
  the published definition for an increasing response that is not a proportion,
  and toxval has no equivalent.
- `drc::ED(type = "relative", reference = "control")` on an increasing curve
  targets c + p(d − c), checked on a simulated `LL.4()` count curve
  (c = 20.7, d = 59.0, fitted response at ED10 = 24.55); this is toxval
  `relative`. `drc` `type = "absolute"` is toxval `direct`. `lref`/`uref` are
  unused in `drc` 3.0.1.
- Fixed numeric bounds, as in RF's `c(lower = 0, upper = 1)` proposal, cannot
  express relative deviation (upper bound 2·y0 per draw) or the SD form.
- Not verified from a primary source: OECD No. 54 §6.2; OECD TG 201/202/236;
  EFSA 2022 and Daniels et al. 2025 formulas (read through a summary); BMDS 3.x.

---

**User:** Cannot open scratchpad files from Windows. Use the standard literature
definition by default for an increasing ECx; post an issue for the more complex
options; asks whether to align types with Daniels et al. (2025). Asks whether
the published definition resolves the negative-control case, and whether
`nsec()` should refuse `type = "direct"`. Post the review on #62, but implement
the changes on our own branch to build on. Post the two classes of no-crossing
draws on #60. Create a private open-AIMS repository for supporting documents,
with logged prompts, starting with the explainer as reproducible R code. Push
the plan branch and open a PR into `phase-2`. Update `chk`.

**Claude:** The published definition does not resolve the negative-control
case: relative deviation, y0(1 ± p), also assumes y0 > 0. Instead every type
now takes its reference as y0 ± p·S from one span helper, and a span of zero or
less gives `NA` for both the reference and `ecnsec`. Agreed that `nsec()` should
refuse `direct`, since its `ecnsec` would repeat the `reference` column. Daniels,
Hommen and Ratte (2025, IEAM 22:734-740, read from the journal page) define an
absolute ECx relative to the control and a relative ECx relative to the
maximum achievable response, which are toxval's `absolute` and `relative`, so
no renaming.

Branch `phase-2-review` (from `c5b56a7`, the head of #62), draft PR #64 into
`phase-2`:

- `R/toxval-predict.R`: the generic refuses `group_var` equal to `x_var` (#15);
  the `drc` method records `object$type` as `meta$family`.
- `R/compute.R`: `response_upper_bound()` returns 1 for `bernoulli`, `beta`,
  `zero_inflated_beta`, `zero_one_inflated_beta` and a `drc` `"binomial"` fit;
  `NA` for `brms` `binomial`, `beta_binomial` and `zero_inflated_binomial`,
  which `posterior_epred()` returns as counts of trials (bound unknown until
  T15's `trials_var`); `Inf` otherwise. `reference_span()` gives the span per
  realisation and direction (`absolute` decreasing y0; increasing 1 − y0 for a
  proportion, y0 otherwise; `range` the distance to the curve's extreme), with
  spans of zero or less set to `NA`. `ecx_reference()` and `ecnsec_value()` both
  use it, and `percent_of_span()` is removed. `compute_nsec()` errors on
  `direct`.
- `R/validate.R`: `chk_sig_val()` documents values above the lower interval
  probability, citing Fisher and Fox (2023, Table 3, p. 2026).
- Tests: the five `TODO Becky` markers replaced by the rulings; new tests for
  relative deviation (control 20 gives reference 22), a `drc` binomial fit,
  a `brms` binomial fit (`NA` increasing reference), a control at or below 0,
  and the `direct` refusal. The `drc` `curveid` test no longer passes
  `x_var = "dose"`, which the #15 guard now stops first.
- Decisions T18 (increasing `absolute` and the span rule), T19 (`nsec()`
  refuses `direct`) and T20 (`sig_val` in (0, 0.5)) recorded in
  `02_decisions.md`, `REFACTOR-claude.md` §3.9 and §3.10, and `REFACTOR-human.md`
  §2.
- Full suite: 861 expectations, no failures, errors or warnings (R 4.6.1, WSL,
  `chk` 0.11.0).

Posted issue #63 (further reference definitions; `relative` and `anchor` for
phase 4, with `drc` model averaging), the review on #62, and a comment on #60
giving the two classes of no-crossing draws and `bayesnec` D17 and #404 as a
worked answer. Created the private repository `open-AIMS/toxval-review`
(cloned at `C:/Rworking/toxval-review`), with the explainer reproduced as
`reviews/2026-10-17-pr62-edge-cases.qmd`, pinned to toxval `c5b56a7` through
`toxval_at()`, and invited Ayla with write access. The reproduction found the
`direct` `ecnsec` is 2.097, not the 2.098 the first page showed after rounding
twice; the page was corrected.
