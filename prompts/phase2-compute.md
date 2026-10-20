## Session: phase2-compute
Date: 2026-10-12
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** Ok start logging froms this prompt onward. Also can we start a branch
off this branch so we can keep the changes Becky requested from these next steps
of work?

**Claude:** Created `phase-2-compute` off `phase-2-keys` (HEAD `f835bf6`), so
the PR #59 review changes stay on `phase-2-keys` and the remaining phase 2 work
(the class-agnostic compute functions and the shared `chk` validator for metric
arguments) goes on the new branch. Started this log.

---

**User:** Ok can you create a very short summary of things that we need beyond
this list? [pasted list of six `phase-2-keys` changes: the T14 `keys` table, a
seed-reproducible bootstrap draw, stable `drc` curve order and the `group_var`
check, the T16 group-level error, T17 `x_var` from a `drc` fit, and the
first-colon split] I am just writing up the phase-2-keys branch PR comments.
Write them here so I can edit and copy over myself

**Claude:** Compared `phase-2..phase-2-keys` and listed what the pasted list
does not cover: `group`/`response` as factors in curve order; `meta$dimension`
removed; `MASS` dropped from Imports; the `brms_model_6` fixture for T16;
per-method docs in `@describeIn`; T13 to T17 and the factor decision recorded in
the plan documents and `02_decisions.md`; review flags replaced with
`REVIEW-TEXT`; smaller fixes (error message typo, test name, `air`). Noted that
the version bounds and the T13 to T17 record sit on `phase-2`, not
`phase-2-keys`, and that local `phase-2` is one commit ahead of origin.

---

**User:** Can you start the compute functions next

**Claude:** Read `R/toxval-pred.R`, `R/ecx.R`, `R/nsec.R` and `R/helpers.R`, and
proposed a shape before writing: `R/compute.R` with `compute_ecx()` and
`compute_nsec()` returning per-realisation values only (no summary, no
`prob_vals`, no pooling across groups, since #60 is open), one row per curve and
direction, with `keys` columns and list columns of draws; and `R/validate.R` for
the shared metric checks. Six decisions raised: leave `relative` out (needs the
asymptote, which `toxval_pred` does not carry); leave `anchor` out; `xform` in
the phase-4 wrapper; no `NA` warning until #60; `ecx_val` uncapped under
`absolute` and capped at 100 under `range`; who writes the code.

---

**User:** 1: leave relative out for now but we may do it after we resolve a few
of these other items. 2: I am not exactly sure what you are referencing. 3: I
think that xform goes in the wrapper. 4: ok this sounds good for now. 5. Yes
this sounds good. 6: You can write the code when directed like this time.

**Claude:** Explained item 2: `anchor` (§3.8) selects where the NSEC reference
comes from; for a single fit `"model"` and `"component"` coincide, and
`"control"` is not implemented, so `compute_nsec()` computes the `"model"`
reference with no argument. Then wrote:

- `R/validate.R`: `chk_type()` (`absolute`, `range`, `direct`, with an
  EXPECTED-CHANGE marker for `relative`), `chk_ecx_val()` (greater than 0 except
  under `direct`, at most 100 under `range`), `chk_sig_val()` (between 0 and
  0.5, exclusive, so the two references lie either side of the control),
  `chk_prob_vals()` and `chk_xform()`.
- `R/compute.R`: `compute_ecx(pred, ecx_val, type)` builds the reference per
  realisation from its own control (T9), separately for each direction, and
  keeps `$decreasing` from the decreasing reference and `$increasing` from the
  increasing one. `compute_nsec(pred, sig_val, type)` takes the `sig_val` and
  `1 - sig_val` quantiles of each curve's control realisations (T13) and returns
  `ecnsec` per realisation as the inverse of the `ecx` reference (T8); under
  `direct` it is the reference repeated. Helpers `directions()`,
  `keys_by_direction()`, `crossings_by_row()`, `ecx_reference()`,
  `ecnsec_value()`.
- `tests/testthat/test-compute.R` (25 tests) and
  `tests/testthat/test-validate.R` (16 tests), on hand-built curves with
  decimal expected values.

Choices made that were not discussed:

- Under `absolute`, the increasing upper bound is 1 for every family, and a
  realisation whose control is at or above 1 gets an `NA` increasing reference
  rather than an error. Since both directions are always computed, the §3.10
  error would fire on every decreasing curve with a control above 1.
- An `ecnsec` whose span is not positive (such as `range` increasing on a
  decreasing curve) is `NA` rather than `Inf` or `NaN`.
- Under `direct`, `ecnsec` is the reference repeated per realisation, so the
  list columns stay the same length; phase 4 must report `NA` bounds for it (T8).

Checked with `load_all()` on hand-built curves and on a `drc` `LL.4` fit of
`nec_data` (500 bootstrap draws): 5 of 500 draws had no decreasing NSEC
crossing at the default `sig_val`, so the #60 mixed case occurs on the example
data. Formatted the four new files with `air`. Test suite not run.

---

**User:** Can you clarify where this compute function is plugging into? What
comes after this, I am struggling at seeing how we are going to be able to put
the whole thing together to check the new code gives the same output as the old
code to confirm the outputs are what we wanted and refactoring didn't change
correct thigns

**Claude:** Laid out the pipeline: fit -> `toxval_predict()` -> `toxval_pred`
-> `compute_ecx()`/`compute_nsec()` -> summarise (phase 4: `xform`, pooling
when `by_group = FALSE`, `prob_vals` quantiles, the #60 rule) -> `toxval`
tibble (phase 5). Nothing calls `compute_*` until phase 4 rewrites the bodies
of `ecx()`/`nsec()`. The phase 1 regression net is the old-versus-new check at
phase 4: unmarked tests must pass, `EXPECTED-CHANGE` tests may move for their
stated reason.

Showed it can also be checked now, since both paths call `posterior_epred()` on
the same grid. On `brms_model_1`, ECx absolute 10: 995 of 1000 draws matched
to 1e-6 once `NA` was replaced by the old `max(x_vec)` fallback. The other 5
have a negative control (gaussian fit), so the absolute reference sits above
the control and the curve rises to meet it; the old code took the first
crossing of any kind, the new code treats it as increasing (T10). Found by
inspecting one draw by hand. `modelbased::zero_crossings()` and the new linear
interpolation agree on every other draw. Listed the intended differences (T8,
T9, T10, T11) and proposed a small comparison script.

---

**User:** Sure yes add the script but not for every fixture, just one or two so
it is small and a quick check. I would like to see how things go together as
that would help me to keep things straight

**Claude:** Wrote `notes/phase2/compare_old_new.R` (already excluded from the
build by `^notes$`). It loads `brms_model_1` and `brms_model_2`, runs the new
pipeline step by step (predict, compute, compare), and labels each draw as
`match`, `no_crossing` (new `NA`, old `max(x_vec)`), `other_direction` (new
`NA`, old a value below `max(x_vec)`; inferred, not checked per draw) or
`unexplained`. A stand-in summary applies the old `max(x_vec)` fallback so the
quantiles are comparable. `ecnsec` is not compared, since T8 changes its
definition.

Results, all with 0 unexplained:

| check | match | no_crossing | other_direction |
|---|---|---|---|
| ECx absolute 10 | 932 | 63 | 5 |
| ECx direct 0.5 | 858 | 120 | 22 |
| NSEC | 124 | 869 | 7 |
| grouped ECx, z = 1 | 791 | 112 | 97 |
| grouped ECx, z = 2 | 776 | 197 | 27 |

Formatted with `air`.

---

**User:** I am curious about the choice of having both compute functions in the
same script? Will these be moved around later into their respective full files
ie ecx() and ecx_compute() together in one file

**Claude:** One file because the phase 2 plan named `R/compute.R` for both
(`phase2-build-spine.md`), and because they share more than they separate:
`directions()`, `keys_by_direction()` and `crossings_by_row()` are used by
both, and `ecnsec_value()` is defined as the inverse of `ecx_reference()` (T8),
so the two formulas should sit side by side. No move is planned. At phase 4
`ecx()` is rewritten in place in `R/ecx.R` and calls `compute_ecx()`. Offered
both layouts (keep `R/compute.R` as the shared layer, or move each
`compute_*` beside its wrapper with the shared helpers in `R/compute.R`),
recommended keeping it as is, and left the choice to the user. No change made.

---

**User:** For esnec is NA when there is no gap item. Can you explain why and
tell me which test shows this behaviour clearly

**Claude:** `ecnsec_value()` computes `shift / span * 100`, where `span` is the
distance from the control to the bound in that direction. A zero span divides
by zero (`Inf`, or `NaN` for `0 / 0`) and a negative span flips the sign, so
neither is a percentage; both are set to `NA`. Three cases reach it: `range`
in the direction a curve never moves (zero span); `absolute` increasing with a
control at or above 1 (negative span); `absolute` decreasing with a control at
or below 0 (negative or zero span). Tests: `test-compute.R:311` (zero span,
`range` increasing on decreasing curves) and `:287` (negative span, `absolute`
increasing). Both use the same three curves and assert all three draws, so
neither shows a mix of `NA` and values. The control at or below 0 case has no
test. Flagged an inconsistency: `ecx_reference()` sets `NA` for `absolute`
increasing with a control at or above 1, but not for `absolute` decreasing
with a control at or below 0, while `ecnsec_value()` sets `NA` for both. No
change made.

---

**User:** Yes add the additional tests you mentioned and add the same TODO flag
I have to the other two in this group

**Claude:** Added two tests to `test-compute.R`, each under the user's
`# TODO Becky to review decision of setting to NA instead of Inf or NaN` flag:

- "compute_nsec returns an NA ecnsec only for the draws whose control is at or
  above 1 when increasing": controls 0.5, 1 and 2, increasing reference 1.5,
  `ecnsec` `c(200, NA, NA)` (a positive span, a zero span, a negative span in
  one curve).
- "compute_nsec returns an NA ecnsec when the control is at or below 0 when
  decreasing": controls -1, 0 and 2, decreasing reference -0.5, `ecnsec`
  `c(NA, NA, 125)`.

Expected values checked with `load_all()`. Formatted with `air`. Suite not run.

---

**User:** Lets go over the sig_val part next. sig_val must be strictly between
0 and 0.5.

**Claude:** Explained the bounds. `sig_val` sets the decreasing reference
(`sig_val` quantile of the control draws) and `1 - sig_val` the increasing one
(§3.6, as `get_nsec_multi()`). Neither the plans nor the old code set a range:
`nsec()` checks only that it is numeric and single, and `quantile()` errors
outside 0 to 1. At 0.5 both references are the control median; above 0.5 they
swap sides, so the decreasing reference sits above the increasing one. At 0 the
references are the smallest and largest control draws, which are not posterior
quantities and grow more extreme with more draws. Called the upper bound a
consequence of T10 and the lower bound a judgement call; recommended keeping
both exclusive and flagging them for Becky. Tests: `test-validate.R`
`chk_sig_val` block (0.01 accepted, 0.5 and 0 rejected); `test-compute.R`
"compute_nsec errors when sig_val is not below 0.5". No change made.

---

**User:** Ok cause the old sig_val definition makes it sound like it can be any
value between 0 and 1 since it is a probability. You are saying it is still a
probablity it is just that value beyond 0.5 don't make sense

**Claude:** Confirmed: still a probability, a tail probability of the control
draws applied to each tail. The old `@param` already called it "the lower
quantile", which implies below 0.5, but nothing enforced it, and with one
direction a value above 0.5 ran and gave an NSEC against a reference above the
control median. With both directions searched it also sets the upper reference
(`1 - sig_val`), so above 0.5 the references swap. Noted the `@param` wording
for the end-of-project docs pass. No change made.

---

**User:** Ok, next lets chat about the abolute item you flagged

**Claude:** Laid out §3.10's increasing upper bound (1 where the family has
none, an error when the control is at or above it) and why the code departs
from it: §3.10 was written for a curve known to be increasing, but under T10
both directions are computed every call, so the error would stop every
decreasing fit with a control above 1 under the default `type`. The code
instead sets the increasing reference to `NA` per draw. Three further points:
the bound is 1 for every family, so "or the family's" is not implemented (a
binomial `brmsfit` predicts counts, so its increasing direction is always `NA`
with more than one trial); `NA` per draw adds a source of mixed draws to #60
when the control posterior straddles 1; and the decreasing side has no matching
rule (a control at or below 0 still gets a reference in `ecx_reference()`, but
`NA` in `ecnsec_value()`). Options: keep per draw; decide per curve from the
median control; family-aware bounds. Recommended per draw for phase 2 and
taking the per-curve question and family bounds to Becky with #60. No change
made.

---

**User:** I think I need some visualizations to understand what you are talking
about. Can you make some plotting code or create a diagram to show me waht you
are talking about with the ecx_reference() setting things to NA when the
control is at or above 1

**Claude:** Wrote `notes/phase2/plot_absolute_bound.R` (ggplot2 and patchwork,
not package dependencies), which calls the package's own `ecx_reference()` and
`find_crossings()` so the plots show what the code does. Four panels at
`ecx_val = 10`:

- A. Increasing proportion, control 0.2: increasing reference 0.28 is crossed;
  decreasing reference 0.18 is never reached.
- B. Decreasing gaussian-scale curve, control 20: without the guard the
  increasing reference would be 18.1, a 10% fall towards 1 sitting on top of the
  decreasing reference (18.0); it is set to `NA` instead of erroring.
- C. Six draws of one increasing curve, controls 0.90 to 1.08: three get a
  reference and three get `NA`, decided per draw, which feeds #60.
- D. Control -0.13, decreasing: the reference (-0.117) sits above the control
  and the curve rises through it, an increasing crossing, so the decreasing ECx
  is `NA` anyway.

Point from D: with a control at or below 0, the decreasing ECx is already `NA`
for any curve that only rises or only falls, so the missing decreasing guard
changes only the `reference` column, plus a hormetic curve that rises and then
falls back through it. Rendered to a PNG in the scratchpad and checked by eye.

---

**User:** Ok lets park this one for Becky as well. Ok onto the last thing you
had flagged which is the under direct ecnsec is the reference repated once per
draw

**Claude:** Parked the absolute bound for Becky. Explained the `direct` choice:
T8 (§3.9, "`type = "direct"` has no interval") makes `ecnsec` the NSEC
reference on the response scale, with `ecnsec.low` and `ecnsec.high` `NA`,
because the reference is one value across draws. `compute_nsec()` stores
`ecnsec` per draw, so under `direct` it repeats `R`; this keeps `ecnsec` the
same length as `draws`, so the two stay paired when unnested (§3.5). The cost
is in phase 4: quantiles of a repeated value give bounds equal to the
estimate, a zero-width interval T8 rejects, so the summary must set the bounds
to `NA` when `type == "direct"`. Alternatives (store one value, or `NA` per
draw) need the same special case and break the pairing. Recommended keeping it
and marking the test with EXPECTED-CHANGE or a note for phase 4. Test:
"compute_nsec returns the reference as the ecnsec under direct". No change
made.

---

**User:** I noticed that covr doesn't seem to pick up when things are tested
with the ::: so it looks like validate has no coverage when I run this
covr::file_report(covr::file_coverage("R/validate.R",
"tests/testthat/test-validate.R"))

**Claude:** Explained the cause from how `covr::file_coverage()` works (not
verified by running it): it sources the R files into a fresh environment,
instruments those copies, and runs the tests there, so a bare call reaches the
instrumented copy while `toxval:::chk_type()` reaches the installed or loaded
namespace, which is not traced. Any namespaced call is missed, not only `:::`.
`test-validate.R` calls everything through `toxval:::`, so it reports 0%; the
same affects `test-crossings.R`, `test-compute.R`, `test-helpers.R` and the 14
`:::` calls in `test-toxval-predict.R`. Options: `covr::package_coverage()` or
`devtools::test_coverage()`, which instrument the namespace (whole suite, slow
with the brms fixtures); `devtools::test_coverage_active_file()` in RStudio,
to be checked; or drop `toxval:::`, since testthat runs tests in an environment
that sees internal functions. Recommended the first two, keeping `:::`. No
change made.

---

**User:** This test errors: [the "compute_ecx finds an increasing crossing
against the absolute upper bound of 1" test] It is because the result is a
named vector of > result$reference[[2]] y y 0.55 0.55

**Claude:** A bug in `compute_*`, not the test: `rbind(y, y)` names the rows
"y", `curve[, 1]` keeps them, and the names carry through `control`, the
references and `ecnsec` (the crossings, built by `vapply()` over indices, were
unnamed). `posterior_epred()` matrices have no row names, but a hand-built
`toxval_pred` can. Fixed in `R/compute.R`: both `compute_ecx()` and
`compute_nsec()` now take `curve <- unname(pred$curves[[i]])`, with a one-line
comment. Confirmed with `load_all()` that `reference`, `control` and `ecnsec`
come back unnamed. The tests that build curves with `rbind(y, y)` now cover
named rows. Suite not run.

---

**User:** I noticed with xform we are only checking the error. there is no test
that passes when using xform

**Claude:** Agreed: `test-validate.R` has only the failing case for
`chk_xform()`, while every other check has an accepted case. Proposed one test
accepting a named function (`log`) and an anonymous one, placed above the
error test. Noted that `xform` itself is applied in the phase 4 wrapper, so its
effect on estimates is tested then, not here. No change made, waiting on the
user.

---

**User:** Yes, might as well

**Claude:** Added "chk_xform accepts a function" to `test-validate.R` (`log`
and `function(x) 10^x`, both `expect_silent()`), above the error test.
Formatted with `air`.

---

**User:** Why is directions a function? Just because it is repeated in places
and a function makes the most sense to reduce repetition?

**Claude:** Mostly yes, but the reason is order as well as repetition: the
`direction` column, the order of the `draws`, `reference` and `ecnsec` lists,
and the number of key rows in `keys_by_direction()` must all agree, and one
definition guarantees it. A function rather than a top-level constant follows
the package's existing pattern (`meta_required()` in `R/toxval-pred.R`). A
constant would work equally well; inlining the vector would risk a mismatch.
No change made.

---

**User:** Ok for the ecx_reference code I don't get the if logic. First if is
if it is direct, then we have a second if statement for absolute what is the
else catching here? Then we do another thing and change absolute again. I find
this not super clear

**Claude:** Explained the current flow: `direct` returns early; the `else` of
the second `if` is `range`, the only type left once `relative` was excluded
(and silently what `relative` would fall into if added without a new branch);
the third block is the absolute-only `NA` guard, applied after the formula
because it masks values. The code shares one bound-based formula across
`absolute` and `range`, which hides the per-type definitions. Proposed a
`switch()` with one arm per type, each giving both directions as in the
§3.10 table, and the same shape for `ecnsec_value()` so the inverse pairs read
side by side. Not applied, waiting on the user.

---

**User:** Yes I like this structure better, it is much more clear, can we have
switch save to an output and return the output instead of all of switch. Also
what do you mean apply() runs on range?

**Claude:** Rewrote both functions in `R/compute.R` with one `switch()` arm per
type, assigned to `reference` / `ecnsec` and returned:

- `ecx_reference()`: `absolute` (`control - control * p`; increasing
  `ifelse(control < upper, ..., NA_real_)`), `range` (row `min`/`max`),
  `direct` (`ecx_val` repeated).
- `ecnsec_value()` now takes the named pair of NSEC references and returns
  both directions, matching `ecx_reference()`; its `direction` argument is
  gone and `compute_nsec()` calls it once instead of per direction.
- New `percent_of_span()` holds the `NA` for a span that is not positive,
  shared by `absolute` and `range`.

Explained `apply(curve, 1, min)`: the minimum of each row, so one per draw,
needed only by `range`. `switch()` evaluates only the arm it selects, so it
is not computed for the other types; the old `else` did the same, so this was
not a change, and the earlier wording overstated it. Values checked against the
test expectations with `load_all()`, all unchanged. Formatted with `air`. Suite
not run.

---

**User:** I am not a fan of this type of code with the , , keys <- keys[rep(i,
length(directions())), , drop = FALSE] I have noted it before. Is there another
way this code can be structured? I find the line a lot and is not very readible
at first until i start to break it apart and run the peices

**Claude:** Two alternatives, both checked on a zero-column `keys` and a grouped
one with dplyr 1.2.1:

- `dplyr::slice(dplyr::as_tibble(keys), rep(i, length(directions())))` inside
  `keys_by_direction()`: same structure, no commas or `drop`.
- `dplyr::cross_join(dplyr::slice(keys, i), <metric tibble>)`: pairs the one key
  row with every row of the results, so the row count comes from the results and
  `keys_by_direction()` is deleted. Needs `dplyr (>= 1.1.0)` in `DESCRIPTION`,
  and `keys` converted to a tibble once at the top.

Recommended `cross_join()`. Not applied, waiting on the user.

---

**User:** I went with option 1, that is easier to read. Next lets look at the
compute_nsec code. [pasted the `rows <- lapply(...)` block] This is a lot,
where we are doing a nested lapply. Can you give a brief explanation of what is
happening

**Claude:** Walked through the block: the outer `lapply()` runs once per curve
(one per row of `keys`) and returns a two-row tibble; inside it, the curve is
unnamed, the control taken, the two NSEC references set from the control
quantiles, the inner `lapply()` runs once per direction to find each draw's
crossing against that direction's reference, `ecnsec_value()` gives both
directions' effects, and the pieces are put side by side with the key row
repeated. `bind_rows()` stacks the per-curve tibbles. Noted that
`unname(draws)` does nothing (an `lapply()` over an unnamed character vector
returns an unnamed list), while the other two `unname()` calls are needed.
No change made.

---

**User:** I have created the PR, can you summarize the things I need becky to
look at and understand about this set of changes

**Claude:** Drafted a PR description for #62 (`phase-2-compute` into
`phase-2`, commit `6ecbd5e`): what the compute layer is and where it plugs in;
scope left out (`relative`, `anchor`, `xform`, summary and #60, wiring); and
the decisions for Becky: absolute increasing bound of 1 with `NA` per draw
instead of the §3.10 error, the missing decreasing guard, `NA` for a span that
is not positive, `sig_val` in (0, 0.5), `ecx_val` uncapped under `absolute`
and capped at 100 under `range`, `direct` `ecnsec` repeated per draw, and when
multivariate `brmsfit` support is built. Noted that `notes/phase2/` and this log
are not committed, so the comparison and plot scripts are not in the PR, and
that the absolute bound and the `direct` `ecnsec` have no `TODO` in the code.

---

**User:** You think xform should be dealt with in phase 4?

**Claude:** Yes, following the user's earlier choice to put `xform` in the
wrapper, and the wrapper (`ecx()`/`nsec()` calling `compute_*`) is written in
phase 4. Nothing in phase 2 applies `xform`, so there is nothing to test yet.
Phase 4 must apply it to the `draws` column only (concentrations), before
summarising (§5), and to the `draws` returned when `draws = TRUE`; `control`,
`reference` and `ecnsec` are on the response or percentage scale and are left
alone. Offered to note this for phase 4.

---
