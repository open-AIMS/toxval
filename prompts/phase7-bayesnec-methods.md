## Session: phase7-bayesnec-methods
Date: 2026-10-23
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** I think Ayla has completed most of the phase 4/5. Can you make sure my
local has the most up to day code. After that, I would like to make a branch that
will form the basis of a PR that will allow toxval to be used with bayesnec
again. I have decided it will be simpler to make toxval predict methods for
bayesnec inside toxval, because toxval requires elements that bayesnec doesn't.
Can you make a plan to implement that

**Claude:** The sync is housekeeping and is not logged here. One finding from it
bears on the plan: the latest code is the draft PR #66
(`phase-4-5-spine-and-tibble`, head `9a3b846`, base `phase-2`). It adds
`summarise_metric()`, the `toxval` tibble class and `x_control`. It does not yet
rebuild `ecx()` or `nsec()` on the spine. Nor does it add `type = "relative"`,
the `asymptote` slot, `anchor`, the trials per curve or the `resolution` default
of 500. Branch `phase-7-bayesnec-methods` was created from `9a3b846`.

The plan was written as `BAYESNEC-human.md` and `BAYESNEC-claude.md` at the
repository root, following CLAUDE.md §13, and both were added to
`.Rbuildignore`. An Explore agent surveyed the `bayesnec` object structure on
`origin/predev` (`116ea91b`, 2.1.3.42) and the CRAN release (`origin/master`,
2.1.3.1). The key claims were then checked directly.

Measurements (R 4.6.1, WSL, `bayesnec` 2.1.3.39):

- `brms::posterior_epred(<component>$fit, newdata, re_formula = NA)` on the
  `nec4param` component of `manec_example`, over a 50-point `bnec_newdata()`
  grid, was identical to `bayesnec`'s own prediction (100 x 50).
- `brms::posterior_epred()` called on the `bnecfit` object itself fails with "no
  applicable method" unless the `bayesnec` namespace is loaded.
- `bayesnec` now builds its model average from a seeded draw index (#216). On
  `manec_example`, which holds no stored index, rebuilding it with seed 216 and
  `sample.kind = "Rejection"` gave a matrix identical to
  `bayesnec::posterior_epred(manec_example, newdata)`: 100 realisations, 83 from
  `nec4param` and 17 from `ecx4param`.
- `bnec_newdata()` took 1.12 s on `manec_example` (500 points), because it calls
  `pull_out()`. On a `bayesnecfit` it took 0.007 s.
- The existing fixtures total 5.4 MB.

Decisions proposed in the plan, with the alternatives rejected:

- T35 (RF's decision, provisional number): the methods live in `toxval`. The
  rejected alternative is the #39 split, which put them in `bayesnec`. Under it,
  every change to the `toxval_pred` contract would need releases of both
  packages, and support would wait on CRAN publication of `toxval`.
- The grid comes from `bayesnec::bnec_newdata()`, and the predictor from
  `model.frame.bayesnecformula`'s `bnec_pop` attribute. Curves come from
  `brms::posterior_epred()` on the stored `brmsfit`. `bayesnec` goes in
  `Suggests` (for RF's approval). Two alternatives were rejected. The first,
  using `bayesnec`'s `posterior_epred()` methods, was rejected because they do
  not say which rows of a model average came from which equation, and have no
  method for a `prebayesnecfit` component. The second, using no `bayesnec` at
  all, was rejected because it needs the formula parsed and the trials and rate
  grid rules copied.
- For a `bayesmanecfit`, `toxval` reproduces `bayesnec`'s draw index: the stored
  `w_draw_index` where valid, and otherwise the rebuild from `w_draw_seed` or
  216. It records `meta$draw_model` and `meta$model_weights`. A `toxval`-seeded
  index was rejected, because it would give a second averaged posterior of the
  same object.
- `anchor = "component"` takes each equation's reference from its realisations
  in the average. `bayesnec` uses each equation's full posterior, so the two
  differ by sampling error. This is accepted and is to be measured in task 5.
  Storing per-equation control posteriors was rejected.
- The first PR covers `bayesnecfit` and `bayesmanecfit`. Two-part families,
  `bayesnechurdlefit`, `bayesnecgroupfit` and `bayesnecjointfit` are refused.
  The last two need a ruling on a per-curve grid and per-curve `x_control` in
  `toxval_pred`.
- `threshold` stays `NULL` until #25.
- The gaussian absolute guard is not reproduced, since `bayesnec` removed it
  (`ecx.R:234-237`).
- Estimates for an inline `crf(log(x))` are reported on the recorded `x` scale.
  `bayesnec` reports them on the `log(x)` scale until its D41.
- The name conflict between the two packages' `ecx()` and `nsec()` is
  documented. An `.onAttach()` message was rejected.

The plan's six tasks: task 0 records T35 and amends the passages listed in
`BAYESNEC-claude.md` §1.2. Task 1 adds fixtures for `manec_example`, a binomial
fit and a `crf(log(x))` fit. Tasks 2 and 3 add the two methods. Task 4 adds
asymptotes and the component anchor, after phase 4. Task 5 adds end-to-end
tests, comparison with `bayesnec`, and the documentation, after phases 4 and 5.

---

**User:** I am happy with your decisions, with respect to 3 - I'm not too
worried about that, we are a long way form getting this on cran, w can sort that
out later (make some local only, as for bayesnec) and the tests are very
valuable for development. In terms of adding the baysenec predict methods, can
we safely proceed from the current state?

**Claude:** Rulings recorded in both plan documents (§6 and task 1):
`bayesnec` goes in `Suggests`; the sampling difference under
`anchor = "component"` is accepted; fixture size does not limit this work, and
when phase 6 prepares for CRAN a fit needed only by `tests/local/` can be kept
out of the built package, as `bayesnec` does with `^tests/local$` in its
`.Rbuildignore`. The stop-and-ask at 1 MB was removed.

State checked before answering. AP pushed three commits to PR #66 on
2026-10-22 (head `6bdd99c`). `ecx()` is no longer an S3 generic and calls
`toxval_predict()`, with `proportion` (T32) and deprecated `ecx_val`. The
generic's `resolution` default is 500 (T31). A new `chk_defined_grid()` follows
curve prediction in the `brmsfit` and `drc` methods. `build_meta()`,
`new_toxval_pred()`, `chk_meta()` and `group_keys()` are unchanged apart from
line offsets. `nsec()` is still the old generic; AP's comment on #66 says it is
next. `asymptote`, `anchor` and `type = "relative"` are not yet present.

`devtools::test()` at `6bdd99c` (R 4.6.1, WSL): 684 expectations pass and 21
tests fail, 13 in `test-compute.R`, 5 in `test-validate.R` and 3 in
`test-summarise.R`. All of them call the `ecx_val` argument that T32 renamed;
AP's comment says the code works and these tests are being updated.
`test-ecx.R` and `test-toxval-predict.R` pass.

Answer: tasks 1 to 3 can proceed now. Their code is in new files and depends
only on internals that #66 has not changed. Plan changes made to fit the new
state:

- The method signature takes `resolution = 500`, and both methods call
  `chk_defined_grid()` on their curve matrices, as the `brmsfit` and `drc`
  methods now do.
- Task 2 gains a test that `ecx()` on each fixture returns a `toxval` tibble,
  since `ecx()` already dispatches through `toxval_predict()`.
- Task 2's done criterion compares the suite against the 21-test baseline at
  `6bdd99c` until AP's test updates land.
- Task 0 moves to last, after #66 merges, because #66 edits the same refactor
  documents and adds T34, so the next decision number is not yet fixed.
- After #66 is squash-merged into `phase-2` (as #65 was), the branch is rebased
  with `git rebase --onto upstream/phase-2 <last #66 commit>`, so that #66's
  commits are not replayed over their squashed copy.
- Task 5 now waits for `nsec()` on the spine and for task 4 only.

The branch was rebased from `9a3b846` onto `6bdd99c`; it had only the two plan
commits, so the rebase was clean.

---

**User:** yes go ahead and start with task 1

**Claude:** Task 1 (fixtures) done. Three blocks added to `data-raw/DATASET.R`
under "toxval_predict methods for bayesnec fits", each saved with
`compress = "xz"` and passed through `strip_stanmodel()`. Only those blocks
were run, from a runner holding `strip_stanmodel()` and the new section
(4 min 42 s, R 4.6.1, WSL, `bayesnec` 2.1.3.39, `rstan` backend).

```r
bayesnec_manec_example <- bayesnec::manec_example

set.seed(2160)
binom_data <- data.frame(
  x = rep(c(0, 0.25, 0.5, 1, 1.5, 2, 2.5, 3), each = 3),
  n = 20L
)
binom_p <- 0.9 * exp(-1.5 * pmax(binom_data$x - 1, 0))
binom_data$y <- stats::rbinom(nrow(binom_data), binom_data$n, binom_p)
bayesnec_binomial <- bayesnec::bnec(
  y | trials(n) ~ crf(x, model = "nec3param"),
  data = binom_data, family = binomial, seed = 2161,
  chains = 2, iter = 2000, thin = 2
)

bayesnec_logx <- bayesnec::bnec(
  y ~ crf(log(x), model = "ecx4param"),
  data = bayesnec::nec_data, seed = 2162,
  chains = 2, iter = 2000, thin = 2
)
```

`manec_example` is stored whole rather than read from `bayesnec` in the tests,
because it holds no `w_draw_index` and so exercises the seeded rebuild. The
binomial data are simulated from `nec3param` itself (top 0.9, nec 1,
exp(beta) 1.5), which has no `bot` parameter. The `crf(log(x))` fit keeps raw
`x` in the `brmsfit` data, which the predictor-scale tests need; a log column
made before fitting would not exercise that.

Checks on the stored, stripped objects:

- All three load through `helper-fixtures.R`.
- `bayesnec_binomial`: binomial, identity link, 400 draws. Rhat at most 1.02.
  Bulk ESS was 249 for `top`, 152 for `beta` and 83 for `nec`, which raised
  brms's low-ESS warning. Estimates: top 0.886 (0.834–0.929), beta 0.601
  (0.250–0.934; true log(1.5) = 0.405), nec 1.126 (0.881–1.324). The
  `bnec_newdata()` grid holds `x` and `n`. Curves lie in 0.006–0.938.
- `bayesnec_logx`: beta, identity link, 400 draws, Rhat at most 1.01, bulk ESS
  at least 313. The grid runs over raw `x`, 0.032–3.22, the data range.
- On both single fits, `brms::posterior_epred(fit$fit, newdata, re_formula = NA)`
  over a 50-point `bnec_newdata()` grid was identical to `bayesnec`'s
  `posterior_epred()`.
- `bayesnec_manec_example`: weights 0.8274 and 0.1726 for `nec4param` and
  `ecx4param`; `sample_size` 100; no `w_draw_index`. The seed-216 rebuild was
  identical to `bayesnec::posterior_epred()`, with a split of 83 and 17.
- Sizes: 567, 220 and 299 KB, 1.06 MB in all. The fixtures total 6.39 MB.
  RF's ruling puts no limit on this during development.
- `devtools::test()` gave the same baseline as before the fixtures: 684
  expectations pass and 21 tests fail, all in `test-compute.R`,
  `test-validate.R` and `test-summarise.R` (the stale `ecx_val` calls).

Coordination with session toxval-58, which opened open-AIMS/toxval#68 for T35.
It reported that RF and AP agreed the decision on 2026-10-23. The T35 lines of
both plan documents now say so and cite #68. The three sub-rulings stay
attributed to RF alone. Task 0's #39 comment is now one sentence and a link to
#68.

---

**User:** push it and then go to task 2

**Claude:** Pushed `phase-7-bayesnec-methods` to origin (`1b1f54e`) without
changing its tracking of `upstream/phase-4-5-spine-and-tibble`. Then task 2,
the single-fit method.

Code:

- `toxval_predict.bayesnecfit()` and `toxval_predict.bnecfit()` in
  `R/toxval-predict.R`, after the `drc` method. The helpers are in the new
  `R/toxval-predict-bayesnec.R`: `bnec_supported_classes()`,
  `bayesnec_available()`, `check_bayesnec_installed()`,
  `chk_bayesnec_supported()`, `bnec_two_part_families()`,
  `chk_no_group_var()`, `bnec_x_var()` and `bnec_grid()`.
- `build_meta()` gains `data = object$data`, used for `x_control`. The
  optional elements of BAYESNEC-claude.md §3.1 are left to task 3, where they
  are first used.
- `response_upper_bound()` returns 1 for a `bayesnecfit` or `bayesmanecfit`
  with a binomial or beta-binomial family, because `bnec_newdata()` predicts
  one trial. This is a branch on `source_class` until T26.
- `bayesnec` added to `Suggests` (RF approved, 2026-10-23).

```r
toxval_predict.bayesnecfit <- function(object, x_var = NULL, group_var = NULL,
                                       resolution = 500, x_range = NULL, ...) {
  check_bayesnec_installed(object)
  chk_bayesnec_supported(object)
  chk_no_group_var(group_var, object)
  data <- object$fit$data
  x_var <- bnec_x_var(object$bayesnecformula, data, x_var)
  newdata <- bnec_grid(object, resolution, x_range)
  x_vec <- newdata[[x_var]]
  curve <- posterior_epred(object$fit, newdata = newdata, re_formula = NA)
  chk_defined_grid(list(curve), x_vec)
  new_toxval_pred(curves = list(curve), keys = group_keys(), x_vec = x_vec,
    meta = build_meta(object = object, x_vec = x_vec, x_var = x_var,
      curves = list(curve), realisation = "draws",
      family = object$fit$family$family, data = data))
}
```

`bnec_x_var()` reads the `x_var` entries of the `bnec_pop` attribute of
`stats::model.frame(<bayesnecformula>, data)`. It counts them with
`names(pop) == "x_var"` rather than `[["x_var"]]`, so that a `crf()` naming
two variables is refused rather than reduced to its first.

Departure from the spec. With the documented methods in
`R/toxval-predict-bayesnec.R`, `devtools::document()` named
`man/toxval_predict.Rd` as `\name{toxval_predict.bayesnecfit}`. roxygen takes
the topic's name from the first block it reads, and reads files in C-locale
order, where `-` (0x2D) precedes `.` (0x2E). The two exported methods were
therefore placed beside the `brmsfit` and `drc` methods. `@include` was
rejected because it adds a `Collate` field to `DESCRIPTION` that every new
file must then join. BAYESNEC-claude.md §4 is amended. roxygen 8.0.0 is
installed against the RoxygenNote of 8.0.0.9000; only `toxval_predict.Rd` and
`NAMESPACE` changed.

Tests in `tests/testthat/test-toxval-predict-bayesnec.R` (17 tests, 53
expectations, all passing), on `bnec_model_1`, `bayesnec_nec4param`,
`bayesnec_ecx4param`, `bayesnec_binomial` and `bayesnec_logx` at
`resolution = 50`:

- curves identical to `brms::posterior_epred(<bayesnecfit>, newdata =
  bayesnec::bnec_newdata(fit, resolution = 50))`, which is `bayesnec`'s own
  method, for all five fits;
- `x_vec` equal to the grid column, a supplied `x_range` honoured, and
  `x_control` kept at the data minimum;
- the `meta` contract; the `crf(log(x))` grid over raw `x`; the binomial
  curve within [0, 1] with an upper bound of 1;
- `x_var` read, matched or refused; `group_var` refused;
- the refusals of a joint level, of the four two-part families, of the
  truncated count part, and of `bayesnecgroupfit`, `bayesnechurdlefit` and
  `bayesnecjointfit`;
- the error with `bayesnec_available()` mocked to `FALSE`;
- `compute_ecx()`, `compute_nsec()` and `summarise_metric()` on
  `bayesnec_ecx4param`, and `ecx()` on all five fits. `bnec_model_1`
  (`nechorme`) warns that the increasing ECx is censored (T25), so that call
  is wrapped in `suppressWarnings()` with a comment.

Checks:

- Full `devtools::test()`: 737 expectations pass. The same 21 tests fail as at
  `6bdd99c`, the stale `ecx_val` calls.
- With `bayesnec` removed from the library path, using a scratch library of
  symlinks to every package but `bayesnec`, assigned to `.lib.loc` (it is
  installed both in the user library and in `/usr/local/lib/R/site-library`),
  the new file is skipped at its first line.
- `devtools::check(args = "--no-tests")`: 0 errors and 0 warnings. Two notes
  concern the untracked local `.claude` and `superceded` directories.

Finding on the task 1 fixtures. Loaded without `bayesnec`, `bayesnec_logx`
warns that the `bayesnec` namespace was replaced by `.GlobalEnv`. Bisecting
its components located the cause in the brms family object. The objects that
`bayesnec:::validate_family()` returns for `"beta"`,
`Beta(link = "identity")` and `binomial` all warn when saved and reloaded
without `bayesnec`; `brms::Beta(link = "identity")` called directly does not.
`bnec_model_1` and `bayesnec_binomial` load cleanly. The fixture was left as
`bnec()` returned it, with a comment in `data-raw/DATASET.R`. Rebuilding its
family was rejected, because the fixture would then differ from what a user
holds.

---

**User:** push that commit and start task 3, the model-averaged method

**Claude:** Pushed `9402879` to origin. PR #66 was unchanged at `6bdd99c`.
Then task 3.

Code:

- `toxval_predict.bayesmanecfit()` in `R/toxval-predict.R`. It checks each
  component with `chk_bayesnec_supported()` and requires one family across the
  set (`chk_one_family()`). It reads `x_var` and the data from the first
  component and builds the grid with `bnec_grid(object, ...)`. Each equation
  is predicted with `posterior_epred(component$fit, newdata, re_formula = NA)`.
  `n` is the smallest number of draws, and the rows at the index are bound in
  the order of `mod_fits`.
- `bnec_draw_index()` in `R/toxval-predict-bayesnec.R` reproduces
  `bayesnec:::pull_draw_index()` and `weighted_draw_index()` as installed
  (2.1.3.39). The stored `w_draw_index` is used when
  `sample_size == n` and every model is named in it. Otherwise the index is
  rebuilt from `w_draw_seed`, or from 216:

  ```r
  rows <- withr::with_seed(
    seed,
    lapply(model_set, function(m) {
      sample(seq_len(n), as.integer(round(n * weights[[m]])))
    }),
    .rng_sample_kind = "Rejection"
  )
  ```

  `withr::with_seed(.rng_sample_kind = "Rejection")` sets the same kinds as
  `bayesnec`'s `set.seed(seed, sample.kind = "Rejection")`, and restores the
  caller's seed and kinds on exit. A hand-written restore was rejected, because
  assigning `.Random.seed` in the global environment from package code is
  flagged by R CMD check.
- `meta$draw_model` is `rep(model_set, lengths(index))`, and
  `meta$model_weights` holds the `mod_stats$wi` values named by model.
  `meta$seed` is the seed of the index (216 on `manec_example`, which stores
  none). `build_meta()` gains `...` for these optional elements.
- `chk_meta()` checks them when present, in the new `chk_model_weights()` and
  `chk_draw_model()`. `draw_model` must be character, have no `NA`, have
  length `n_realisation`, and name only models in `model_weights`.
  `model_weights` must be numeric with no `NA`, have unique non-empty names,
  lie in [0, 1] and sum to 1 by `all.equal()`. They are documented as optional
  in the Metadata section of `?new_toxval_pred`. The `seed` entry now also
  covers the index of a model average.
- `bnec_supported_classes()` now includes `bayesmanecfit`.

Tests: 12 new in `test-toxval-predict-bayesnec.R` and 7 new in
`test-toxval-pred.R`, all passing.

- On `bayesnec_manec_example`, the matrix is identical to
  `brms::posterior_epred(<bayesmanecfit>, newdata = bnec_newdata(fit, 50))`,
  which is `bayesnec`'s own method. `draw_model` gives 83 `nec4param` then 17
  `ecx4param`, equal to `round(100 * wi)`; `model_weights` equals `wi`; and
  `seed` is 216.
- Under `withr::local_seed(42, .rng_normal_kind = "Box-Muller")`,
  `.Random.seed` and `RNGkind()` are identical before and after the call, and
  two calls give identical objects.
- A copy given `w_draw_index = list(nec4param = 1:80, ecx4param = 81:100)`,
  `w_draw_seed = 7L` and `sample_size = 100` uses that index (80/20, seed 7).
  With `sample_size = 99` the index is rebuilt from seed 7 (83/17). Both are
  identical to `bayesnec`'s matrix for the same copy.
- Weights of 0.999 and 0.001 give 100 `nec4param` realisations, with
  `ecx4param` kept in `model_weights`; this too is identical to `bayesnec`.
- Refused: components of differing families, a two-part family in one
  component, and a `group_var`. The error with `bayesnec` mocked as absent,
  and `ecx()` returning a `toxval` tibble, are also tested.
- The `meta` checks: valid elements are accepted; errors are tested for the
  wrong length, an `NA`, an unknown model, weights summing to 0.9, unnamed
  weights, and a weight outside [0, 1].

Checks: full `devtools::test()` passes 772 expectations, with the same 21
stale `ecx_val` failures as at `6bdd99c`. `devtools::check(args =
"--no-tests")` gives 0 errors and 0 warnings, and the same two notes about the
local `.claude` and `superceded` directories.

Measurement (BAYESNEC-claude.md §3.2). A set of `nec4param` and `ecx4param`
was fitted to `nec_data` with `chains = 4, iter = 2000, warmup = 1000,
seed = 4000`, giving 4000 draws per equation, in 217 s. Two equations suffice,
because `bnec_newdata()` pulls out only the first. Timings:

| resolution | `bnec_newdata()` | `posterior_epred()`, both equations |
|---|---|---|
| 200 | 3.0 s | 0.7 s |
| 500 | 3.2 s | 0.9 s |
| 1000 | 4.9 s | 1.6 s |

`toxval_predict()` at 500 took 5.8 s, and `ecx()` 3.7 s on a later call. The
30 s stopping point was not approached. The fit stored a `w_draw_index` (seed
815105072, weights 0.846 and 0.154), so this run also exercised the stored
path on a real object: 3385 and 615 realisations, identical to
`bayesnec::posterior_epred()`. The fit is in the session scratchpad only and
is not a fixture.

---

**User:** There are a bunch of different models in the bayesnec dev/predev
branch, and used as examples in the CR workflows repo. Can you confirm if the
predict method is robust to all the possible baysenc model structures

**Claude:** The check was run rather than argued. Two read-only Explore surveys
listed the structures. The first covered `bayesnec` `predev`: formula terms,
families, equations, classes, model-set operations, and where `bayesnec`'s
`posterior_epred()` differs from `brms`'s. The second covered every `bnec()`
fit in CR_workflows; the repository has one call site,
`pkg/R/fit_bayesnec.R:85`, and no saved fits.

`bayesnec` `predev` at `116ea91b` (2.1.3.42, against 2.1.3.39 installed) was
exported with `git archive` to the session scratchpad and loaded with
`pkgload::load_all()`. Nothing was installed, and the bayesnec checkout was
not touched. About 40 fits were made, most with `chains = 2, iter = 1000,
warmup = 500`. A comparison script ran `toxval_predict(resolution = 50)` on
each fit, on every equation pulled out of each model set, on the per-level
fits of a group fit, and on the parts of a hurdle pair. It compared each
curve matrix with `identical()` against
`brms::posterior_epred(<fit>, newdata = bayesnec::bnec_newdata(fit, 50),
re_formula = NA)`, which dispatches to `bayesnec`'s own method, and then ran
`ecx()` and `summarise_metric(compute_nsec())`. `re_formula = NA` is needed
because `bayesnec`'s methods set neither `newdata` nor `re_formula`, while its
internal curve predictions use `re_formula = NA`.

Results:

- 128 predicted objects (33 fits and 95 components) were all identical to
  `bayesnec`. They cover:
  - every family except the two-part ones;
  - `trials()`, `rate()`, `weights()` and `cens()`;
  - `crf(log(x))` and `crf(sqrt(x))`;
  - `pgl()`, `ogl()` and `(top | g)`;
  - `disp(~x)` and `disp("power")`;
  - a `log(y)` response and a logit link;
  - the every-equation set (17 kept) and the CR_workflows `decline` and `all`
    sets on gamma, binomial, negbinomial and beta with a zero control;
  - `amend(drop = )` and `+`.
- No `ecx()` or NSEC computation errored.
- The refusals worked on real objects: `hurdle_gamma`, `bayesnecgroupfit`,
  `bayesnechurdlefit`, `bayesnecjointfit`, and a `bayesnecjointlevel` from
  `bayesnec:::joint_level_fit()`.
- `thin = 3` with four chains gave `sample_size` 667 against 668 draws, so the
  stored index was bypassed by both packages. This confirms the survey's
  reading of `extract_simdat()`. `bayesnec`'s stored summaries were computed
  on the 667-draw index, while its `posterior_epred()` uses the rebuilt one,
  so the two describe different draws. This is a `bayesnec` issue.
- Under `RNGkind("L'Ecuyer-CMRG")` the rebuilt index matched `bayesnec`, but
  differed from the default-generator index: the generator kind is not
  pinned in `weighted_draw_index()`.
- `zero_inflated_poisson` and `zero_inflated_negbinomial` are accepted, since
  `bayesnec`'s two-part registry excludes them on purpose (constant `zi`;
  `data-raw/sysdata.R:60-74`).
- Not covered, because the structure could not be built:
  - the truncated count part of `bnec_hurdle()`, which needs brms 2.23.2;
  - `disp(~z)`, for which `bnec()` fails (#443);
  - `+` with unequal draws, which `bayesnec` refuses;
  - `pull_out()` of two equations, which 2.1.3.42 refuses.
- From the code: a `trials(n * 2)` expression would be predicted for two trials
  while `response_upper_bound()` gives 1. Not fixed.

Recorded as BAYESNEC-claude.md §3.7. The fits and scripts are in the session
scratchpad only (`fit-common.R`, `fit-families.R`, `fit-allset.R`,
`fit-cr.R`, `fit-features.R`, `fit-ops.R`, `compare-fits.R`).

---

**User:** push both commits, open the bayesnec issue about the draw count, and
move the scripts into the repo

**Claude:** `49fe974` and `65e2d69` were pushed to `origin`.

Before writing the issue, the `thin = 3` set was measured again on the predev
export. Its two equations, `nec4param` (weight 0.83) and `ecx4param` (0.17),
hold 668 draws each, while `sample_size` is 667. The stored index keeps 553
and 114 rows drawn from rows 1 to 667. `posterior_epred()` rebuilds 554 and
114 rows from rows 1 to 668. The two indices share 518 and 106 rows.

- `bayesnec`'s `posterior_epred()` is identical to the rows taken under the
  rebuilt index.
- The stored plot data, `w_pred_vals$data` at 1000 grid values, are identical
  to the summary under the stored index. They differ from the summary of
  `posterior_epred()` on the same grid by up to 0.0038.
- `bayesnec::ecx(posterior = TRUE)` returns 667 draws.

So `bayesnec`'s stored summaries, `ecx()`, `nsec()` and
`extrapolated_manec_ne()` take `object$sample_size` and keep the stored index.
`posterior_epred()`, `posterior_predict()` and the two asymptote helpers count
the draws themselves and rebuild a different index.

The count comes from `extract_simdat()`:
`ceiling((iter - warmup) / thin * chains)`. Stan keeps
`ceiling((iter - warmup) / thin)` draws per chain. The two counts differ when
the fractional part of `(iter - warmup) / thin` is greater than 0 and at most
`(chains - 1) / chains`. At the `bnec()` defaults, the counts were computed
rather than fitted: 2668 kept against 2667 recorded for `thin = 3`, and 728
both ways for `thin = 11`. The code involved is unchanged on the current
`predev`, `a710fc5a`.

No existing issue covered this; #327 and #216 are related but different. The
fault was opened as open-AIMS/bayesnec#477, written to CLAUDE.md §14. The fold
holds the files and lines, two possible corrections, and the open question
about objects already saved. Counting the draws from the fit is preferred,
because the stored count and the counted one cannot then disagree.
`bayesnec_manec_example` records 100 of its 100 draws, so it is unaffected.

The harness went to `tests/local/bayesnec-robustness/`:

- `common.R`, the shared set-up, with three changes from the scratchpad
  copy:
  - the scripts run from the repository root and check that they do;
  - `BAYESNEC_SOURCE` names a source tree to `load_all()`, falling back to
    the installed `bayesnec`;
  - the fits go to `TOXVAL_ROBUSTNESS_DIR`, by default
    `tools::R_user_dir("toxval", "cache")`. Inside the repository, R CMD
    build would copy their 3.3 GB into the tarball.
- `fit-families.R`, `fit-allset.R`, `fit-features.R`, `fit-cr.R` and
  `fit-ops.R`, the fits. `fit-cr.R` reads `CR_WORKFLOWS`, by default
  `../CR_workflows`.
- `compare-fits.R`, the comparison, which now writes its table to the fit
  folder.
- `check-edges.R`, new. It gathers the checks that had been run inline:
  - the refusal of a `bayesnecjointlevel`;
  - `crf(log(x))` with `x_range = c(0, 3)`;
  - the #477 measurement;
  - the L'Ecuyer-CMRG generator, run last because it changes the generator.
- `README.md`: the order of the scripts, their settings, and provenance (R
  4.6.1, brms 2.23.0, rstan 2.32.7, predev `116ea91b`).

The scripts were formatted with air. Every script was rerun from the
repository against the saved fits, and each fit script skipped all of them.
The comparison table was `identical()` to the recorded one: 137 rows, 128
identical to `bayesnec` and 9 refusals, with no ECx or NSEC error. The edge
checks reproduced every recorded result.

`BAYESNEC-claude.md` §3.7 now names the script folder. Its `thin` bullet adds
that `bayesnec`'s `ecx()` and `nsec()` keep the stored index, cites #477, and
records that the fixture is unaffected. Task 5 lists the harness fits as a
further source, and leaves a set whose `sample_size` differs from its draws
out of the per-realisation comparison.

---

## Session: phase7-bayesnec-methods (continued)
Date: 2026-10-26
Model: Claude Opus 5.5 (claude-opus-5-5)

### Prompts and Responses

**User:** Ayla has push up a lot more to the 4/5 branch, including nsec. Can
bayesnec fits be run now?

**Claude:** Yes. `upstream/phase-4-5-spine-and-tibble` had eight new commits,
`a043730` to `2436858`:

- `nsec()` on the spine, calling `toxval_predict()` for any class;
- `nsec_multi()` folded into `nsec()`;
- the `asymptote` slot with `type = "relative"`;
- an `anchor` argument that is recorded but does not group;
- T26 amended so that a `brms` binomial fit is predicted at one trial and
  bounded at 1.

The branch was rebased onto `2436858` in a scratch worktree on a new branch,
`phase-7-rebase`. The main checkout, which holds other sessions' uncommitted
prompt files, was left alone. The only conflict was in `build_meta()`: Ayla
had added `multi_var` and removed a stale EXPECTED-CHANGE comment, while this
branch had added `data` and `...`. The resolution keeps both and drops the
comment. roxygen produced no changes.

Full suite on the rebased branch: 812 expectations, 1 failure. The failure is
the snapshot of `test-nsec.R:225` ("nsec drc additional example 2"), a drc
bootstrap NSEC that differs in the third figure (0.402 against 0.403). It
fails identically on `upstream/phase-4-5-spine-and-tibble` alone, in a
detached worktree, so it is not from this branch. It is for AP.

`ecx(proportion = c(0.1, 0.5))` and `nsec()` were run directly on every
bayesnec fixture with bayesnec 2.1.3.39 installed: `bnec_model_1`,
`bayesnec_nec4param`, `bayesnec_ecx4param`, `bayesnec_binomial`,
`bayesnec_logx` and `bayesnec_manec_example`. Each returned a `toxval`
tibble with `# Model:` naming the class. The binomial control was 0.886, a
proportion. The only warnings were the expected censoring warnings on
`bnec_model_1` (`nechorme`). `type = "range"` ran on the model set, and
`type = "relative"` stopped with phase 4's "supplies no asymptote" message.

`nsec(bayesnec_manec_example, anchor = "component")` returned the `"model"`
numbers labelled `"component"`: the same estimate, 1.47, and the same
reference, 2.04. Phase 4 records `anchor` only, on the stated ground that no
supported class is model-averaged, which this branch makes untrue. The plan's
task 4 had said to stop and ask how phase 4 groups. It does not group, so the
grouping belongs to task 4. As the minimal guard until then, `compute_nsec()`
refuses `"component"` when `meta$draw_model` is present:

```r
  # the reference below is pooled over every realisation, so a model average
  # would otherwise return the "model" reference labelled "component"
  if (anchor == "component" && !is.null(pred$meta$draw_model)) {
    chk::abort_chk(
      "`anchor` must be 'model' for a model-averaged fit; 'component' is not ",
      "yet supported."
    )
  }
```

The `@param anchor` of `compute_nsec()` and `nsec()` say so. The `bayesnecfit`
/ `bayesmanecfit` branch of `response_upper_bound()` was removed, because the
amended T26 bounds every binomial family at 1. Tests were added:

- the guard on a hand-built model average in `test-compute.R`;
- `nsec()` on every single fixture and on the model set;
- identical results under either anchor on a single fit;
- the refusal on the model set.

The binomial bound test now calls the three-argument `response_upper_bound()`.
Committed as `c3c0dd8`. Full suite: 831 expectations, the same single drc
failure.

`BAYESNEC-claude.md` (§3.4 response bound, task 4, §5 dependency table and
rebase history) and `BAYESNEC-human.md` §4 were updated: tasks 1 to 3 done,
task 4 can start, task 5 waits only for task 4, and the interim refusal is
noted. Committed as `67809d1`.

The robustness harness was not rerun. The session scratchpad had been cleared
since 2026-10-23 (`/tmp` is a tmpfs under WSL), taking the 3.3 GB of fits, the
predev export and the recorded comparison table with it. The scripts are in the
repository, and their default output is the user cache, which persists.
Refitting takes about 75 minutes. The prediction path of the bayesnec methods
is unchanged by the rebase apart from `build_meta()`, so it was offered rather
than run.

`phase-7-rebase` holds the result. The local and fork copies of
`phase-7-bayesnec-methods` are still at `f727958`. Moving the branch and
force-pushing the fork were left for RF to confirm.

---

**User:** Part of the issue text is out of date - can you update it? And
formally confirm the amendment where ever that needs to happen? The vignette
format will be quarto. The vignette doesn't need nec_data as package data? A
vignette can call a dataset from another package? we might use drc ones as
well. Yes go ahead and push the rebased branch, and I'll be doing the first
draft of vignettes in the toxval-review repo, so it can install from that
branch. Yes go ahead and move phase-7-bayesnec-methods to the rebased commits
and force-push the fork.

**Claude:** The branch was moved with `git reset --keep`. No prompt file
differed between the two heads, so the other sessions' uncommitted prompt
files were kept. The log was committed, and the fork was force-pushed with a
lease on its previous head, `65e2d69`. It is now at `eb0a68e`. The scratch
worktree and `phase-7-rebase` were removed.

RF had already confirmed the T26 amendment, as ruling 6 of the PR #66
rulings comment of 2026-10-24 (issuecomment-6382626146). Ayla's `2436858`,
committed a few hours before that comment, still read "for Becky to confirm"
in `notes/implementation/02_decisions.md`. That line now reads "Amended
2026-10-23 (AP), and confirmed 2026-10-24 (RF, ruling 6 on PR #66)",
committed as `6ef7c56` and pushed. The same comment asks AP to amend T26 in
place on #66, so a later rebase may meet her wording on that line. Her
wording takes precedence there.

Issue #71 was edited; it had no comments, so no marker comment was posted:

- "Proposed decisions" became "Decisions". Decision 3 is settled (RF,
  2026-10-26): Quarto for vignettes and articles. The R Markdown alternative
  was removed.
- The dependency table gained a state column for 2026-10-26:
  - ECx and NSEC definitions, brms single and grouped, and drc are on PR #66;
  - multivariate support is on PR #66 (`d220ef3`), but the paper and data
    are unpublished;
  - bayesnec single fits and model sets are on `phase-7-bayesnec-methods`,
    while `anchor = "component"` on a model set and `type = "relative"`
    need #68 task 4;
  - there is no `glm` method on either branch.

  It names `beckyfisher/toxval@phase-7-bayesnec-methods` as the branch drafts
  install from. toxval-review is private, so it is not named in the issue.
- A Build bullet says that a vignette can use another package's dataset
  (`bayesnec::nec_data`, `drc::daphnids`) without toxval storing it. A
  shipped vignette may use only Suggests packages and guards their chunks with
  `requireNamespace()`.
- The multivariate bullet now follows T26 as confirmed. A beta-binomial is
  predicted at one trial, so `type = "absolute"` gives an increasing extra-risk
  reference, and the increasing `ecnsec` sits near 0 for normalising-total
  trials (the #63 edge case). It replaces "`response_upper_bound()` returns
  `NA` ... use `type = "range"`, or wait for #63. Ask which."
- The drc bullet takes `nec_data` from `bayesnec`; it is not package data.
- The bayesnec bullet notes the interim refusal of `"component"` on a model
  set.
- The definition of done names the vignettes in commit messages for `fledge`,
  instead of `NEWS.md` listing them, because `NEWS.md` is not edited by hand.
- The ask list dropped the format, the `nec_data` question and the
  multivariate `type`. It keeps any package other than `quarto` added to
  DESCRIPTION, one article or one per class, and the publication timing.

The answer to the question on datasets is yes. A vignette can call
`bayesnec::nec_data` or `drc::daphnids` directly. A shipped vignette, built
by R CMD check, may use only packages in Imports or Suggests, and guards their
chunks because a Suggests package can be absent. Articles excluded from the
build need only the packages installed.

toxval-review loads toxval with `toxval_at(commit)`, which archives a commit
of the local toxval checkout and loads it with `pkgload`. Either
`toxval_at("phase-7-bayesnec-methods")` or a pinned SHA works. A SHA keeps a
draft reproducible across the rebases #66 will force, and the archive in
`_cache/` keeps the code after a rebase.

---
