# Work queue — toxval

Read `00_protocol.md` first, then `02_decisions.md`.

> **These are *toxval* tiers.** `bayesnec` has its own
> `notes/implementation/01_work_queue.md` with its own Tier 1 and Tier 2, and
> the two cross-reference each other. **bayesnec Tier 1 is complete** (PRs
> #197–#208, merged 14–17 Aug 2026). **toxval Tier 1 is two of twelve done** —
> #33 (PR #46) and #37 (PR #48), both merged; as of 2026-09-05 no other toxval
> issue is closed.

> **Sequencing against the refactor — read before starting Tier 1.**
> `REFACTOR-human.md` (PR #42, revised in PR #44) rebuilds the estimator spine,
> and its phase 1 captures the current estimates as golden values. Tier 1 was
> planned independently, against the current architecture, and the two collide:
> run Tier 1 first and the golden values move; run the refactor first and Tier 1
> lands in code that has been rewritten.
>
> | Tier 1 item | verdict |
> |---|---|
> | #33, #31, #32 | **safe either way** — mechanical, no behaviour change |
> | #37 | safe, but flagged below as behaviour-changing; do it *before* the net is locked |
> | #5, #7, #15, #6/#24, #11, #13/#10, #34, #29 | **fold into the refactor.** All behavioural, and #34, #13/#10 and the `xform` defect are already listed in `REFACTOR-claude.md` §3.7 |
>
> Nothing here is wrong; it just cannot all run before the refactor without
> wasted work.

**Tier 1 is the unattended run.** Twelve issues, ordered: mechanical cleanups
first so the files are tidy before anything substantive touches them, then error
handling, then the genuine bugs.

**Tier 0 and Tier 2 are not autonomous.** They are recorded so the reasoning
survives.

---

# Tier 0 — attended, do first, NOT for an unattended session

## The dependency untangle  *(now issue #39)*

`toxval` has `bayesnec` in its `Imports`, so the dependency runs backwards from
where it is going. Both packages also register the same S3 methods today —
`predict.bayesnecfit`, `predict.bayesmanecfit`, `nsec.brmsfit`, `nsec.drc` —
and both export `ecx` and `nsec` generics. Attaching both masks the generics and
makes the duplicate methods load-order dependent. **That collision is live now,
not something the migration would introduce.**

The shape of the fix: `toxval` keeps the generics, the estimator machinery
(`ecx_x_*`, `nsec_fct`, `nsec_multi`) and the `drc`/`brmsfit` methods, and
exposes the estimator on **plain inputs** — a posterior matrix and an `x_vec` —
rather than on a fitted object. The `bnecfit` methods, the `predict` methods and
`newdata_eval()` move *into* `bayesnec`, which then imports `toxval` and
re-exports the generics. `bayesnec` drops out of `toxval`'s `Imports` to
`Suggests`.

**Why it is not autonomous:** it spans two repositories, and `bayesnec` cannot
declare `Imports: toxval` until a `toxval` carrying the new API is installable —
otherwise `bayesnec`'s R CMD check fails on a function that does not exist yet.
The interim needs a `Remotes:` entry and the two PRs have to land in order. That
wants a person.

~~**No issue exists for this yet. One should be filed.**~~ **Filed: #39.**

**Two things learned since this was written (2026-08-19):**

- **`toxval` must be on CRAN before `bayesnec` can import it.** `bayesnec` is on
  CRAN; `toxval` is not. A CRAN package's `Imports` must resolve from CRAN, and
  `Remotes:` is ignored by CRAN — so the `Remotes:` interim works for GitHub
  installs only. CRAN readiness is a prerequisite, not a follow-up.
- **The "plain-input API" is `toxval_pred`** (`REFACTOR-claude.md` §3.3/§3.4).
  Build it once, there.

`PHASE1_DEPENDENCY_UNTANGLE.md` carries the detail; `REFACTOR-claude.md` §4 is
the authoritative phase order.

---

# Tier 1 — the unattended run

## 1. #33 — remove commented-out code in `ecx.R`  *(done, PR #46)*

Mechanical. Delete the commented blocks. If any block looks like it encodes an
intention rather than dead code, leave it and say which in the PR.

**Done when** `ecx.R` has no commented-out code and the suite still passes.

---

## 2. #37 — resolve the TODO markers in `nsec`  *(done, PR #48)*

The markers were left because it was unclear whether the commented code should
go. **Decide per block on evidence:** if the behaviour is covered by a test or
duplicated by live code, remove it; if it is not, leave it and explain.

**Done when** every TODO marker is either gone or replaced by a comment saying
why the code stays.

**Hazard.** This is the one cleanup that can silently change behaviour. Run the
full suite after each block, not once at the end.

---

## 3. #31 — package loading messages during tests

Test output is polluted by attach messages. Suppress at the point of loading —
`suppressPackageStartupMessages()` in `tests/testthat/setup.R` — rather than by
changing what the package emits at load.

**Done when** `devtools::test()` output is free of them.

---

## 4. #32 — R CMD check warning on S3 generic/method consistency

The generic and one or more methods disagree on their argument list. Align the
methods to the generic, keeping `...` where the generic has it.

**Done when** R CMD check is clean of that warning.

**Hazard.** Related to Tier 0 — some of these methods are moving to `bayesnec`.
Align signatures; do not restructure dispatch.

---

## 5. #34 — `nsec.drc()` assumes `curveid` is column 4

```r
groups <- unlist(unique(object$data[, 4]))
```

Positional indexing into a `drc` object's data. Address it by name, and error
clearly if the expected column is absent rather than silently taking the wrong
one.

**Done when** it works regardless of column order, with a test on a `drc` fit
whose columns are arranged differently.

---

## 6. #5 — silence the `try()` error in `ecx`

`try()` is not `silent = TRUE`, so a message reaches the user and the failure is
not handled. Silence it **and handle the failure** — a silenced `try()` whose
result is never checked is worse than the message.

**Done when** the failure path returns something sensible or errors informatively,
with a test.

---

## 7. #7 — error when more than one `type` is passed to `ecx`

Currently unhandled. Error naming the offending argument. The issue floats
"use the first with a warning"; **error instead** — silently using one of two
requested types is the kind of thing that produces a wrong number in a report.

**Done when** passing two types errors, and the existing test is updated.

---

## 8. #15 — error when `group_var == x_var`

Same class of guard. Error clearly rather than producing a nonsensical grouping.

**Done when** the combination errors with a message naming both arguments.

---

## 9. #6 and #24 — warn when an estimate is at a bound

Take together; they are the same warning from two directions. #6 wants a warning
when the upper limit hits the upper bound, and **definitely** when the lower and
upper bounds are identical. #24 wants the same for potentially censored
estimates, matching the warning `bayesnec`'s `ecx` method already emits.

**Done when** an estimate pinned at a bound warns, an interval collapsed to a
point warns, and both are tested.

**Hazard.** An estimate at the edge of the tested range is a real result, not an
error — warn, do not fail, and word it so a user knows what to do about it.

---

## 10. #11 — "need at least two non-NA values to interpolate"

A genuine bug. The issue has a failing test. Reproduce, find why fewer than two
non-`NA` values reach the interpolation, and fix the cause rather than padding
the input.

**Done when** the test in the issue passes for the right reason.

---

## 11. #13 and #10 — `x_range` handling

Together: #13 reports nonsensical output when `x_range` is passed, #10 says a
*range* should be accepted and is currently taken as a single value and pushed
straight to the output. Same defect from two angles.

**Done when** `ecx(fit, x_range = c(2, 5))` restricts the evaluation to that
range and returns an estimate within it, with tests for a valid range, a
reversed one, and one outside the data.

---

## 12. #29 — `zero_crossings()` misses closely-spaced crossings

Replica of `bayesnec` #166, filed here because `toxval` is becoming the owner.
**Close `bayesnec` #166 as a duplicate of this** when it is fixed.

**Done when** narrow and closely-spaced crossings are detected, with a test
built from a curve whose crossings are known analytically.

---

# Tier 2 — needs a decision first

Not autonomous. Each is a legitimate issue whose *answer* is undetermined.

| | the undetermined part |
|---|---|
| ~~#19~~ | ambiguity in what "effect" means across models and implementations. **Decided 2026-09-04**: the reference is per realisation and `type` is a four-value vocabulary (`absolute`, `relative`, `range`, `direct`). See `02_decisions.md` T9 and `REFACTOR-claude.md` §3.10. The `nsec` half is exposed as `anchor` (§3.8), whose default was ratified as `"model"` on 2026-09-05 (T12). |
| ~~#1, #8~~ | `hormesis_def = "max"` errors, and where it does not error the output is wrong. **Absorbed by #20**, decided 2026-09-04: `direction` is a property of the result and `hormesis_def` is removed. Close both when #20 is implemented. `REFACTOR-claude.md` §3.6. |
| #12, #14 | `type = "direct"` returns something unexpected; output shape differs across methods. Governed by `REFACTOR-claude.md` §3.10 and §3.1; follows #19 and #4. |
| #3 | `ecx_val` as a proportion rather than a percentage — **breaking**, and needs a deprecation decision. |
| ~~#4~~ | ~~always return a tibble~~ — **decided 2026-08-19**: yes, a `toxval` tibble subclass. `REFACTOR-claude.md` §3.1. #14 follows from it. |
| #9 | multiple `ecx_val` values, to match revised `bayesnec` methods. Shape follows #4. |
| ~~#20~~ | increasing versus decreasing responses. **Decided 2026-09-04**: `direction` is a column of the result, each direction has its own reference, and a direction with no crossing returns `NA`. See `02_decisions.md` T10 and `REFACTOR-claude.md` §3.6. |
| #17, #18, #25 | NOEC, model-averaged N(S)EC, N(S)EC. New estimators, each needing a definition agreed before code. |
| #21 | `glm` and other model classes. Naturally follows Tier 0, since that is what makes the estimator class-agnostic. |
| #22 | whether `toxval` should include model-fitting wrappers at all. A scope question for the author. |

## Inherited from bayesnec

These were triaged in `bayesnec` and belong here. ~~**#39 and #44 have no toxval
issue yet and should be filed.**~~ **Filed as #40 and #41.**

| | |
|---|---|
| bayesnec #195 | inert `hormesis_def`, divergent `type` implementations, wrong documented default. Substantially **already better here** — `toxval` threads `hormesis_def` through `ecx_x_*` with a live `modify_posterior()`, where `bayesnec`'s equivalent is commented out. Overlaps #1, #8, #12, #14. |
| bayesnec #196 | inline `crf()` transformations back-transformed incorrectly. **Does not apply here** — `toxval` takes `xform` as a user-supplied function instead of deriving it from the formula, so the failure mode is designed out. Worth recording as a reason not to adopt `bayesnec`'s approach during Tier 0. |
| bayesnec #39 | replace the evaluation grid with root-finding. **Now toxval #40.** The spike referenced here as `03_uniroot_spike.md` was never written; the code is `bayesnec/ignore/uniroot.R`. |
| bayesnec #44 | hypothesis method for *NEC*/*NSEC*/*ECx* exceedance. New API; belongs with the estimators. **Now toxval #41.** |
