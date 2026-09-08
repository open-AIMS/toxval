## Session: phase2-build-spine
Date: 2026-09-07
Model: Claude Opus 5 (1M context)

Planning session for `REFACTOR-claude.md` §4 phase 2. Claude plans, checks and
works through problems; the user writes all repo code.

### Prompts and Responses

**User:** Ok, lets start working on phase 2. Can you start by giving a short
outline of what needs to be done in phase two. Keep it to bullet points, it
should be a high level summary.

**Claude:** High-level outline of phase 2, the additive spine build:

- `toxval_pred` — the intermediate object (`curves`, `x_vec`, `threshold`,
  `control`, `meta`), with constructor, validator and print method, exported
  because `bayesnec` constructs one from outside the package at phase 7.
- `toxval_predict()` — the single S3 generic, with `brmsfit` (ungrouped, grouped,
  multivariate) and `drc` methods.
- Two realisation sources, one representation: posterior draws for Bayesian
  fits, parametric bootstrap from `coef()`/`vcov()` for frequentist ones, so the
  compute layer never branches on class.
- Compute core: direction-aware crossing finder, grid interpolation and
  per-`type` reference construction, replacing the five duplicated copies across
  `nsec_fct`, `tox_fct` and the three `ecx_x_*` functions, and fixing #29.
- Shared `chk` validators, called by new code only; the four hand-rolled copies
  stay until phase 4.
- Tests: slot contract and alignment invariant, crossings against analytic
  curves, the `drc` mean function against `predict()`, and equivalence with
  current output only where definitions coincide.

Not in phase 2: rewiring `ecx()`/`nsec()` (phase 4), tibble output (phase 5),
`bnecfit` methods (they move to `bayesnec`); `threshold` and `control` are
defined in the contract but populated `NULL`.

Carried forward as open: which classes get methods (recommended `brmsfit` and
`drc` only) and the `resolution` default (recommended 1000).

---

**User:** Agreed on not creating things for bayesnec that get removed in the next
step — `brmsfit` and `drc` only. Start with resolution 1000 and see how that
goes. So we are writing new functions here and not touching the pre-existing code
at this phase?

**Claude:** Both decisions settled: methods for `brmsfit` and `drc` only,
`resolution` defaults to 1000.

Confirmed the additive reading, with three qualifications:

- **Duplication is deliberate and temporary.** The new crossing/interpolation
  code duplicates `nsec_fct`, `tox_fct` and the three `ecx_x_*` functions, and
  both copies live in the package through phases 2-4. Worth a comment in the new
  file, or it reads as not having noticed the existing helpers.
- **Files that change, none behavioural:** `DESCRIPTION` (`MASS`, `withr` into
  Imports); `NAMESPACE` and `man/` (regenerated); `R/toxval-params.R`, the one
  pre-existing file with real edits, documentation-only, to add shared
  `@inheritParams` entries for `n_boot`, `seed`, `multi_var`. Flagged that its
  `object` entry still reads "An object of class bayesnecfit or bayesmanecfit",
  already wrong for the `brmsfit` and `drc` methods — small fix, could ride along
  here or go with the #56 documentation work. `NEWS.md` left alone, fledge
  maintains it from commit messages.
- **Equivalence tests are scaffolding with a known end date.** With nothing
  wired, the only way to show the spine reproduces today's numbers is to call
  both paths in one test and compare; those tests are meaningless once the old
  path goes at phase 5, so they should say so in a comment.

Noted that `brmsfit` + `drc` only means the §3.4 alignment invariant cannot be
exercised through a method, so it is tested by hand-building a `toxval_pred` from
a `bayesmanecfit`'s draws — the same path `bayesnec` uses at phase 7, so the test
doubles as a check that the public constructor works from outside the package.

---

**User:** For the testing, are we just going to have the exact same tests but
they point to a different file or do all the tests need to be duplicated with the
new function names? [and, mid-turn] Are we duplicating the files so there will be
an ecx and ecx_refactor? Or am I adding an ecx_refactor function to the current
ecx file?

**Claude:** Neither — there is no `ecx_refactor` and no parallel copy of `ecx()`.
Phase 2 builds a lower layer whose functions are not ECx-shaped:
`R/toxval-pred.R`, `R/toxval-predict.R`, `R/crossings.R`, `R/compute.R`
(`compute_ecx()`, `compute_nsec()` on a `toxval_pred`), `R/validate.R`,
`R/bootstrap.R`. At phase 4 `ecx()` is rewritten *in place* — body becomes
validate -> `toxval_predict()` -> `compute_ecx()` -> summarise — and
`ecx_x_relative/absolute/direct` are deleted then. `ecx()` never has a twin.

Corrected the earlier "deliberate duplication" wording, which prompted the
question: what is duplicated is the crossing-and-interpolation *logic*, present
in `crossings.R` and the five old copies simultaneously through phases 2-4. No
public function is duplicated. Noted that `compute_ecx()`/`compute_nsec()` should
take their permanent names now, since phase 4's public wrappers call them and
they outlive phase 2.

Tests are therefore not duplicated either, because the argument surface splits:
predict-level (`x_var`, `group_var`, `x_range`, `resolution`) migrates to
`toxval_predict()` tests, metric-level (`ecx_val`, `type`, `sig_val`,
`prob_vals`, `xform`, `posterior`) stays with `ecx()`/`nsec()` until phase 4/5.

Grounded in the current suite (199 tests: ecx 76, nsec 70, nsec-multi 35,
helpers 11, predict 7): of the 76 `ecx` tests only about ten have a phase-2
counterpart, and those assert grid contents and `curves` names rather than ECx
values; ~25 more are `bnecfit` tests heading to `bayesnec`. The 199 existing
tests are phase 4's acceptance criteria and stay untouched. Only
`test-helpers.R` is genuinely superseded, since it tests the internals being
replaced, and 5 of its 11 are already marked. New suite estimated at 40-60
tests.

**Fixture gap found:** there is no `drc` fixture with a real `curveid`.
`nsec_drc_1$data` has columns `x, y, 1, 1, weights` — column 4 is literally
named `1` and holds a constant, which is why `object$data[, 4]` appears to work
and why `nsec(nsec_drc_1, curveid = "a")` succeeds with a non-existent
`curveid` (#34 in one line). The only genuine multi-curve test builds
`daphnids_m2` inline from `drc::daphnids` at `test-nsec.R:347`. Phase 2's
grouped-`drc` tests either follow that pattern or need a new fixture — decide
before writing `toxval_predict.drc`.

**Bootstrap verified feasible:** `fct$fct(x, parm)` reproduces
`predict(fit, newdata)` to a maximum absolute difference of exactly 0 on
`nsec_drc_1`, and `vcov()` returns a full 4x4. T11's mechanism is sound.

**Defect in the §3.10 asymptote plan (phase 4, flagged early):** the spec keys
the asymptote lookup on `fit$fct$name`, but on these fixtures `fct$name` is
`"::"` rather than `"LL.4"` — drc captures the call and `drc::LL.4()` leaves the
namespace operator as the name. `fct$text` gives
`"Log-logistic (ED50 as parameter)"`, and the `fct` object carries `lowerAs`,
`upperAs` and `monoton` slots the spec did not account for, which look like a
better source than a hand-maintained name table.

---

**User:** Ok, lets start then. Which function should I start with? Can you
provide a clear but simple outline of the first steps to take

**Claude:** Recommended starting with `R/crossings.R` — the only piece with no
dependencies (no brms, drc, class or fixtures), testable against hand-computed
curves immediately, where T10's direction semantics become concrete, and the
foundation everything else sits on. It replaces `nsec_fct`, `tox_fct` and the
shared tail of all three `ecx_x_*` functions.

Supplied `find_crossings(y, x_vec, reference)` returning both directions in one
pass, with an `interp_crossing()` helper:

```r
find_crossings <- function(y, x_vec, reference) {
  d <- y - reference
  n <- length(d)
  ok <- !is.na(d)
  j <- which(ok[-n] & ok[-1])

  lo <- d[j]
  hi <- d[j + 1]

  list(
    decreasing = interp_crossing(j[lo >= 0 & hi < 0], d, x_vec),
    increasing = interp_crossing(j[lo <= 0 & hi > 0], d, x_vec)
  )
}

interp_crossing <- function(j, d, x_vec) {
  if (!length(j)) {
    return(NA_real_)
  }
  j <- min(j)
  frac <- d[j] / (d[j] - d[j + 1])
  x_vec[j] + frac * (x_vec[j + 1] - x_vec[j])
}
```

Interpolates in response space rather than via `zero_crossings()`'s fractional
index — same answer on a linear segment, one less indirection, no `modelbased`
call.

Six rules to settle while writing it:

1. No crossing returns `NA_real_`, not `max(x_vec)` (§3.6) — the rule that makes
   `test-helpers.R:31` change.
2. First means lowest x (`min(j)`).
3. Exact zeros count as on the reference, hence `>=`/`<=` on the low side, so a
   curve starting at the reference and falling registers a crossing at
   `x_vec[1]` (`frac` = 0). Flip to strict if a touch should never count.
4. NA gaps break the scan — consecutive non-NA pairs only, so an all-`NA` `y`
   returns `NA` both ways with no special case.
5. A curve starting below the reference has no decreasing crossing; reachable
   because `nsec`'s reference is an across-draw quantile, so a single draw can
   begin below its own threshold.
6. This is the #29 fix — a sign-change scan catches every crossing the grid can
   resolve. A down-and-back-up excursion inside one interval remains out of
   reach for any grid method, which is #40's territory.

Tests, all hand-computable with no fixtures: monotone decreasing; monotone
increasing; hormetic with both crossings; flat above; flat below; flat exactly
equal; crossing on a grid point; two crossings one interval apart (#29); an `NA`
in the middle; all `NA`; a two-point grid.

Advised against vectorising over realisations for now — the metrics will
`apply()` row-wise as the current code does, and the scan generalises to a matrix
later if profiling calls for it. `R/toxval-pred.R` next, mostly transcription
from §3.4.

---

**User:** I think I have most of the tests covered, except for the two crossings
one interval apart. Can you help explain what is needed for that one and see if
there are any tests that I missed?

**Claude:** Read the user's `R/crossings.R` and `tests/testthat/test-crossings.R`
(15 tests) and traced every expectation against the implementation; all pass as
written.

**The #29 mechanism.** `modelbased::zero_crossings()` does not scan the input
vector — it builds a linear interpolant and re-samples onto its own fixed
100-point grid, so any feature narrower than that spacing is invisible and a
crossing-and-return loses both crossings. The sign-change scan examines every
consecutive input pair, so nothing wider than one interval can hide. Supplied the
test using the issue's reprex shape with `x_vec = seq_along(y)`, asserting
`19 + 10/11` and `20 + 1/11`; verified `find_crossings()` returns those and
`zero_crossings()` returns `40.5` alone. Advised keeping the contrast as a
comment, not an assertion, so the test survives an upstream fix.

**Resolved the user's top/bottom asymmetry TODO.** Only three conventions are
self-consistent: inclusive-low (current), inclusive-high (the mirror), and
strict-both — which is broken, since a crossing landing exactly on a grid point
is missed entirely (the "on a point" test would return `NA` instead of `1`). The
first two agree wherever the zero is interior and differ only at the two
endpoints. Keep inclusive-low: the reachable exact zero is at `d[1]`, because
`nsec`'s reference is a quantile of the control posterior and a draw can land on
it, and for that draw "NSEC at the lowest concentration" is right rather than
dropping it from the quantile. A zero at the last point is a touch, not a
crossing, and #6/#24 will flag range-edge results anyway.

**Resolved the NA TODOs.** The "NA in the curve" test already returns `3`, so its
comment is stale. The open case is an NA sitting exactly at the crossing, which
returns `NA` because the scan uses consecutive non-NA pairs only. Recommended not
bridging: once T10 removes `modify_posterior()`, an `NA` in a fitted curve means
the model failed to predict there, and interpolating invents a number; better for
the metric layer to count `NA` realisations and warn (#6/#24). Noted the test
puts `NA` in `x_vec` as well as `y`, which cannot happen since the grid is
`seq()` — test `NA` in `y` only.

**Missing tests, ranked:** (1) unequally spaced `x_vec` — every existing test
uses consecutive integers so `x_vec[j+1] - x_vec[j]` is always 1 and the scaling
term is never exercised; `x_vec = c(0, 2, 10)`, `y = c(10, 0, -10)`,
`reference = 5` gives 1, a dropped multiplication gives 0.5. (2) Two crossings in
the same direction, to test `min(j)`: `y = c(6, 4, 6, 4, 6, 4, 6)`, `x_vec = 0:6`,
`reference = 5` gives dec 0.5, inc 1.5. (3) Non-finite values. (4) A sub-interval
excursion, asserting `NA`, commented as #40's limit. (5) The return contract —
a list of exactly `decreasing` and `increasing`, numeric scalars.

**One code change: `ok <- is.finite(d)` instead of `!is.na(d)`.** Verified
`y = c(Inf, -1)` with `reference = 0` currently returns `NaN` for the decreasing
crossing (`frac = Inf / Inf`), leaking a value where a missing one belongs;
`is.finite()` subsumes the existing NA and NaN handling, returns `NA`, and leaves
monotone results unchanged. `Inf` is reachable from a bootstrap draw through an
exponential link.

**Also flagged:** the scratch ggplot block at the end of the test file runs
outside any `test_that()` and `ggplot2` is in neither Imports nor Suggests, so it
fails R CMD check on CI. `notes/` is already `.Rbuildignore`d if the visual aid
is worth keeping.

---

**User:** Does this set of numbers capture 29? `y <- rep(0, 40); y[11] <- 2;
y[39] <- 5; x_vec <- seq(from = 0.5, to = 20, 0.5); reference <- 1`

**Claude:** No — verified, `zero_crossings()` finds all four crossings
(`10.5 11.5 38.2 39.8`), so the old implementation handles it. A valid
narrow-spike test, but not a #29 regression test.

What decides it is not the spike's width in indices (always two intervals here)
but the width of the window where the curve is above the reference, since that is
what must contain one of `zero_crossings()`'s internal grid points. For a
background `b` below the reference and a spike `s` above it, that window is
`2 * s / (s + b)` index units against a spacing of `(n - 1) / 100`:

- proposed: `s = 1`, `b = 1` -> window 1.0 vs spacing 0.39, so grid points land
  inside and both crossings are seen;
- issue reprex: `s = 0.5`, `b = 5` -> window 0.18, nothing lands inside, both
  lost.

A symmetric spike is always caught. Smallest fix: `y[11] <- 1.1`, verified to
make `zero_crossings()` return only `38.2 39.8` while `find_crossings()` finds
both. Expected values in exact form: `increasing = 60 / 11` (5.454545),
`decreasing = 61 / 11` (5.545455).

Two features of the user's setup worth keeping: the `0.5` grid spacing also
exercises the interpolation scaling (though a dedicated uneven-spacing test is
still wanted, since 0.5 is uniform); and because `y[40]` returns to 0 the
index-39 feature is a spike rather than an end crossing, giving four crossings,
so the assertion doubles as a check that the first of each direction is returned
— worth a comment or the second spike looks decorative.

---

**User:** Are there any additional tests to crossings you would add now or is the
suite complete?

**Claude:** Reviewed the updated files (`is.finite()` applied, unequal-spacing
and multiple-crossing tests added, 20 tests). Four gaps, plus fixes.

Gaps, ranked:

1. **Rule 3 on the decreasing branch.** The "on a point" test exercises
   inclusive-low for `lo <= 0 & hi > 0` but nothing tests it for
   `lo >= 0 & hi < 0`, and decreasing is the primary path for both ECx and NSEC.
   Verified: `y = 5:0`, `x_vec = 0:5`, `reference = 3` gives
   `decreasing = 2`, the grid point where the curve sits on the reference.
2. **A multi-element `reference` recycles silently** — `c(5.5, 2.5)` on `10:0`
   returns 5.625 / 6.75 with only R's recycling warning. Guard with
   `chk::chk_scalar()` or pin the behaviour.
3. **The return contract** — a list of exactly `decreasing` and `increasing`,
   each a numeric scalar, since `compute_*` will index it by name.
4. **The resolution limitation (#40), optional.** Same function on two grids,
   verified: `f(x) = 1 - 4 * exp(-((x - 5.5)^2) / 0.02)` on `0:11` gives `NA`
   both ways, on `seq(0, 11, 0.1)` gives 5.324335 / 5.675665 (true 5.333489 /
   5.666511). Documents why an estimate moves with `resolution`.

Fixes: delete the commented-out `#ok <- !is.na(d)` (issue #33's exact smell);
the all-NA test uses `y <- NA` against a six-point `x_vec`, so it tests a
length-1 input rather than its stated case — `rep(NA_real_, 6)` gives the same
answer and tests what the name says, and length mismatches are silent generally
(`y` length 5 against `x_vec` length 10 returns a value); the
`expect_equal(modelbased::zero_crossings(y), 40.5)` assertion pins another
package's bug and goes red if it is fixed upstream; the #29 tolerances are
unnecessary given `20 + 1/11` and `19 + 10/11` are exact.

Advised stating the same-length and increasing-`x_vec` invariants in the roxygen
and enforcing them in `compute_*`, rather than putting `chk` in a primitive that
runs once per realisation. Verified a named `reference` from `quantile()` does not
leak its name into the result, so no `clean_names()`-style handling is needed.
Left the two Becky TODOs (asymmetry, NA bridging) as genuine co-author questions.

---

**User:** This doesn't error: `expect_equal(result$decreasing, NaN)`

**Claude:** Correct, and the reason is broader than the one test. `expect_equal()`
in testthat 3e calls `waldo::compare(..., tolerance = testthat_tolerance())`, and
setting a tolerance switches waldo to `all.equal()` numeric semantics, where
`all.equal(NA_real_, NaN)` is `TRUE`. Verified:

```
expect_equal(NA_real_, NaN)          PASS
expect_identical(NA_real_, NaN)      FAIL
all.equal(NA_real_, NaN)             TRUE
waldo::compare(NA_real_, NaN)                        -> `old`: NA / `new`: NaN
waldo::compare(NA_real_, NaN, tolerance = default)   -> No differences
```

So waldo can distinguish them; the tolerance is what makes it stop caring.

**Consequence:** every `expect_equal(..., NA_real_)` in the file — about twenty
assertions — is blind to `NaN`. Before `is.finite()` was added, when the
decreasing branch really did return `NaN`, those tests would have passed anyway,
so they never protected against the leak they appear to.

Fix: `expect_identical()` for missing-value expectations, `expect_equal()` for
interpolated numbers where floating point matters. `expect_identical()` also pins
the type, so a logical or integer `NA` would fail, which covers most of the
return-contract test for free. Recommended the swap across the whole file rather
than just the `Inf` test, and supplied the rewritten `Inf` test asserting
`expect_identical(result$decreasing, NA_real_)`.

---

**User:** What about now? I want the tolerances as it is easier for a human to
eyeball the number on a plot when it is a number and not a fraction.

**Claude:** Ran the file against a loaded package: 48 assertions, all passing.

Accepted the tolerance preference and noted why it is now free: the NaN blindness
came from tolerance being applied to the *missing-value* comparisons, and since
those moved to `expect_identical()`, tolerance only touches the interpolated
numbers where it belongs. Verified both tolerances sit inside the true values.
Recorded the preference to memory.

Four remaining items, none structural:

1. **Duplicate test name.** Two tests are both called
   `"find_crossings when multiple crossings"`; the second is the
   scalar-`reference` guard, so a failure report would not say which broke.
2. **`chk_scalar()` lets a character through** — a character scalar is a scalar,
   so `find_crossings(10:0, 0:10, "a")` fails from the arithmetic with
   "non-numeric argument to binary operator". Adding `chk::chk_numeric(reference)`
   gives `` `reference` must be numeric. `` and still accepts `NA_real_`, so the
   NA-reference test keeps working — verified both, along with the existing
   regexp matching chk's real message exactly.
3. **The `chk` call costs 27% of the primitive's runtime**, measured — but ~30 ms
   per 4000 realisations at resolution 1000, which is noise next to
   `posterior_epred()`. Recommended keeping it: the guard makes the function safe
   to call from anywhere, and vectorising over the realisation matrix would
   collapse it to one call. Flagged only so the cost is known rather than a
   surprise under later profiling.
4. **Two known leftovers.** The commented `ggplot2` block no longer runs or
   breaks check but is commented-out code heading into a PR (#33's subject); and
   the resolution-limitation test (#40) is still absent — optional, but it is the
   test that would answer "why did my NSEC change when I changed `resolution`".

Minor: `1e-7` reads more easily than `0.0000001`.

---

**User:** Can you give me a full example for this 40 issue. Otherwise I believe
those things are fixed. The choice to check is intentional, ensuring the right
values is critical over that small of a speed diff.

**Claude:** Agreed on keeping `chk_scalar()`/`chk_numeric()` — correctness over
30 ms matches the standard applied elsewhere in the package. Supplied and
verified the #40 test (4 successes):

```r
test_that("find_crossings misses an excursion narrower than the grid (#40)", {
  # A known limitation, not a defect: crossings are found by scanning for sign
  # changes between adjacent grid points, so a feature that dips below the
  # reference and returns within a single interval is invisible. Increasing
  # resolution reveals it. Closed-form inversion or root-finding (#40) is what
  # would remove the dependence on resolution.
  dip <- function(x) 1 - 4 * exp(-((x - 5.5)^2) / 0.02)

  coarse <- 0:11
  result_coarse <- toxval:::find_crossings(dip(coarse), coarse, 0)

  expect_identical(result_coarse$decreasing, NA_real_)
  expect_identical(result_coarse$increasing, NA_real_)

  fine <- seq(0, 11, 0.1)
  result_fine <- toxval:::find_crossings(dip(fine), fine, 0)

  # true crossings are 5.333489 and 5.666511; the residual error is the
  # straight-line interpolation between grid points
  expect_equal(result_fine$decreasing, 5.324335, tolerance = 1e-6)
  expect_equal(result_fine$increasing, 5.675665, tolerance = 1e-6)
})
```

What it encodes: a narrow Gaussian dip centred at 5.5 from a baseline of 1 down
to -3, against a reference of 0, crossing at 5.333489 and 5.666511 — an
excursion 0.33 wide. On the coarse grid the bracketing points x = 5 and x = 6
both evaluate to 0.999985, so a plot of that grid is a flat line at y = 1 and
`NA` is the honest answer. On the fine grid both crossings appear at 5.324335 and
5.675665, about 0.009 from truth, which is the second lesson: finer resolution
finds the crossing and still only approximates it.

Stressed that the comment carries the weight here — without it a reader sees two
`NA`s on a curve that visibly crosses the reference and treats it as a bug, where
naming #40 says the fix is a different algorithm rather than a patch. Noted that
plotting `dip()` over `seq(0, 11, 0.01)` with the two grids overlaid makes it
obvious at a glance.

---
