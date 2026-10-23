# Specification for bayesnec support in toxval

The implementation specification for `toxval_predict()` methods for fits made
with `bayesnec`: function signatures, the calls each method makes, invariants,
edge cases, the measurements behind each decision, and the alternatives
rejected.

`BAYESNEC-human.md` is the plan. Read it first. Section numbers are the same in
both documents. Each decision is stated in full here and once only.

Facts about `bayesnec` are cited as `file:line` on `origin/predev` at
`116ea91b` (2026-10-20, version 2.1.3.42) unless marked `[master]`, which is the
CRAN release 2.1.3.1 at `6f6bd25d`. `toxval` facts are cited on
`phase-7-bayesnec-methods` at `6bdd99c`, the head of PR #66. Measurements were
run on 2026-10-23 in R 4.6.1 under WSL with `bayesnec` 2.1.3.39 installed.

Decision numbers T35 onwards are provisional until they are written into
`notes/implementation/02_decisions.md` (task 0). T34 is reserved for the control
ruling AP is recording on PR #66.

## 1. The decision (T35)

`toxval` contains the `toxval_predict()` methods for `bayesnec` fits. Agreed
by RF and AP on 2026-10-23, and proposed in #68.

### 1.1 Reason

A `toxval_pred` holds facts that `toxval` defines for its own compute functions
and that `bayesnec` has no use for: the `keys` table (T14), `meta$x_control`
(T21), the per-realisation asymptotes (T24), the per-curve trials (T26), and,
for a model-averaged fit, the equation behind each realisation (§3.2). Each of
these was added or changed during phases 2 to 4. Had the methods been in
`bayesnec`, each change would have needed a `bayesnec` release made after the
`toxval` release that introduced it, and `bayesnec` could not import `toxval`
at all until `toxval` was on CRAN (#45).

### 1.2 Passages the decision overrides

Each passage below states or depends on the earlier split, in which `bayesnec`
held the methods and imported `toxval`. Task 0 edits them.

`toxval`:

- `REFACTOR-human.md` §3.3, last paragraph: `toxval_pred` is public "under #39"
  because `bayesnec` constructs one.
- `REFACTOR-human.md` §4: the constraint "`toxval` must reach CRAN before
  `bayesnec` can depend on it", phases 6 and 7, and the closing paragraph
  ("`bayesnec` sees one finished API at phase 7").
- `REFACTOR-claude.md` §2, the sequencing constraint paragraph.
- `REFACTOR-claude.md` §3.3, the paragraph "`toxval_pred` is public API".
- `REFACTOR-claude.md` §3.4, realisation alignment: "Both of these are
  `bayesnec` fixes, implemented in `toxval_predict.bayesmanecfit()` and in
  `bayesnec`'s averaging helpers. `toxval` only states the invariant and tests
  it."
- `REFACTOR-claude.md` §3.7, "Where class-specific guards go": the `dpar` home
  is unchanged in name but is now in `toxval`; the gaussian guard is deleted
  (§3.5).
- `REFACTOR-claude.md` §3.8, the first consequence bullet (`bayesnec`'s
  resampling machinery goes "(#39)").
- `REFACTOR-claude.md` §4: the paragraphs from "`toxval` must reach CRAN before
  #39 can ship" to "before the `bayesnec` PR"; table rows 6 and 7; the closing
  paragraph; and the last sentence of the `bayesmanecfit` paragraph in "Phase 3
  without `bayesnec`" ("so it is checked at phase 7").
- `REFACTOR-claude.md` §5: the bullet on `bayesnec` #216, which `bayesnec` has
  since fixed (§3.2), and the sub-paragraph stating that
  `toxval_predict.bayesnecfit()` "lives in `bayesnec`".
- `notes/implementation/02_decisions.md` T4, first sentence ("`bayesnec` is
  deleting its own `ecx()`/`nsec()`").
- `notes/implementation/01_work_queue.md`, Tier 0 "The dependency untangle".
- Issue #39: the split table in the comment of 2026-08-19, and the ordering in
  the follow-up of the same day. #45 no longer blocks #39.

`bayesnec`, for its maintainers to revise; this work does not edit them:

- `notes/implementation/02_deferred.md:10-21`: `ecx()`, `nsec()`, `ecnsec()`
  and `zero_crossings()` are to be relocated to `toxval`, "which will become
  a dependency of bayesnec".
- `notes/implementation/03_decisions.md` D10 (:82-109), D8 (:111-120), D11
  (:137-172).
- `bayesnec` issue #255.

### 1.3 Parts of the earlier plan that stand

`toxval_pred` stays exported and documented, and changes to its contract stay
breaking changes. Another package may still supply a `toxval_predict()` method,
and #21 (`glm`, `gam`) uses the same seam. Only the reason given for it in
§3.3 of the refactor plan changes.

### 1.4 Rejected alternative

The methods in `bayesnec`, as #39 proposed. Rejected for the reason in §1.1:
every `toxval_pred` contract change becomes a two-package release, and support
for `bayesnec` fits waits on CRAN publication of `toxval`.

## 2. User-visible behaviour

### 2.1 The predictor scale

`toxval` reports estimates on the scale of the variable named inside `crf()`.
For `crf(log(x), ...)` that is `x`. `bayesnec::ecx()` and `bayesnec::nsec()`
report the same fit on the `log(x)` scale.

The mechanism:

- `bayesnec` takes the predictor as `all.vars()` of the first argument of
  `crf()` (`bayesnecformula.R:821-826`), so `bnec_pop["x_var"]` is `"x"`.
- The transformed term is substituted into the `brms` nonlinear formula
  (`bayesnecformula.R:843-845`, `:868-872`), so Stan applies `log()` and the
  `brmsfit` data hold raw `x`.
- `prediction_grid()` builds the grid over raw `x` (`expand_classes.R:319-326`;
  the comment at `helpers.R:2380-2386`).
- `bayesnec` finds the crossing on that grid, then applies the `crf()`
  transformation to the estimate with `sub_x_transformation()`
  (`helpers.R:2409-2430`, called at `ecx.R:267` and `nsec.R:310`), and prints
  a once-per-call message (`ecx.R:84-88`). Reporting on the recorded scale by
  default is decided for `bayesnec` 3.0 as D41 (`03_decisions.md:568`) and not
  implemented.

`toxval` takes `x_vec` from the same grid column and applies no transformation,
so its estimates are on the raw scale. `xform` is applied afterwards (T6), so a
user who supplies `xform = exp` for an inline `log()` gets `exp(x)`. The method
documentation says so. A predictor transformed into its own column before
fitting, such as `x = log(concentration)` in the data, is unaffected: the grid
is then over that column.

Test (task 5, `tests/local/`): on the `crf(log(x))` fixture, each `toxval` ECx
realisation equals `exp()` of the matching `bayesnec::ecx(posterior = TRUE)`
realisation.

### 2.2 Differences from bayesnec's numbers

| quantity | `toxval` | `bayesnec` | source |
|---|---|---|---|
| `nsec` reference, model-averaged fit | pooled control (`anchor = "model"`, T12) | each equation's own control | `nsec.R:422-456` |
| realisation with no crossing | placed by T21 | censored at a bound, `bound_*` reported | `helpers.R:2959` |
| directions | both, as rows (T10) | decreasing | — |
| default `resolution` | 500 (T31) | 200 | `ecx.R:178-183` |
| control | first grid value, the lowest observed `x` when `x_range` is `NULL` | prediction at the lowest observed `x` | `helpers.R:2697-2742` |
| interpolation | linear between grid points | linear | `helpers.R:2483-2507` |

The last two rows agree, so under `type = "absolute"`, at equal `resolution` and
with `x_range` unset, the decreasing ECx of every realisation that crosses
within the grid should equal `bayesnec`'s. Task 5 tests this to `1e-8`. A
discrepancy there means that one of the packages locates the crossing
incorrectly.

## 3. Design

### 3.1 Sources of the grid and the curves

#### Signature

```r
toxval_predict.bayesnecfit <- function(
  object,
  x_var = NULL,
  group_var = NULL,
  resolution = 500,
  x_range = NULL,
  ...
)
```

`resolution` follows the generic's default of 500 (T31, `6bdd99c`).

#### Body

```r
check_bayesnec_installed()
chk_bayesnec_supported(object)
chk_no_group_var(group_var, "`bayesnecfit`")
x_var <- bnec_x_var(object$bayesnecformula, object$fit$data, x_var)
newdata <- bnec_grid(object, resolution, x_range)
x_vec <- newdata[[x_var]]
curve <- posterior_epred(object$fit, newdata = newdata, re_formula = NA)
chk_defined_grid(list(curve), x_vec)

new_toxval_pred(
  curves = list(curve),
  keys = group_keys(),
  x_vec = x_vec,
  meta = build_meta(
    object = object,
    x_vec = x_vec,
    x_var = x_var,
    curves = list(curve),
    realisation = "draws",
    family = object$fit$family$family,
    data = object$fit$data
  )
)
```

#### Helpers

`check_bayesnec_installed()` stops with
"`bayesnec` must be installed to compute on a `<class>`." when
`requireNamespace("bayesnec", quietly = TRUE)` is `FALSE`. The
`requireNamespace()` call is in its own one-line function, `bayesnec_available()`,
so that a test can replace it with `testthat::local_mocked_bindings()`.

`bnec_x_var(formula, data, x_var)`:

```r
found <- attr(stats::model.frame(formula, data = data), "bnec_pop")[["x_var"]]
```

`stats::model.frame()` dispatches to `bayesnec`'s registered method
`model.frame.bayesnecformula` (`bayesnecformula.R:748-759`), which is also in
the CRAN release `[master NAMESPACE]`. This is how `newdata_eval()` found the
predictor before phase 3 (`58c57a1^:R/helpers.R:23`). Rules:

- `length(found) != 1` stops: the `crf()` term names more than one variable.
  `bayesnec` refuses the same case when it back-transforms (`sub_x_transformation()`,
  `helpers.R:2409-2430`).
- `x_var = NULL` takes `found`.
- A supplied `x_var` that differs from `found` stops, naming both, as T17 does
  for a `drc` fit.

`bnec_grid(object, resolution, x_range)`:

```r
bayesnec::bnec_newdata(
  object,
  resolution = resolution,
  x_range = if (is.null(x_range)) NA else x_range
)
```

`bnec_newdata()` is exported in both the CRAN and the development release. It
sets the trials column to 1 for `binomial` and `beta_binomial`
(`expand_classes.R:327-330`) and a `rate()` denominator to 1
(`expand_classes.R:331-345`), and adds level columns only for a joint refit,
which is out of scope (§3.3). It builds no response column and no group-level
variables, so group-level effects must be marginalised: `re_formula = NA`, as
every `bayesnec` curve prediction does (`expand_classes.R:281-291`) and as T16
specifies for `brmsfit`.

`build_meta()` (`R/toxval-predict.R:263-291`) reads `min(object$data[[x_var]])`
for `x_control`. A `bnecfit` has no `$data`; its data are `object$fit$data`.
`build_meta()` therefore gains `data = object$data`, used for `x_control`, and
`...`, a set of optional elements appended to the list (§3.4). The `brmsfit`
and `drc` methods pass neither and are unchanged.

`x_control` is then the lowest observed predictor value, which is what
`bayesnec` reads its control at (`control_x()`, `helpers.R:2554-2572`).

#### Refusals

`chk_bayesnec_supported(object)` stops, naming the class or family, when:

- `inherits(object, "bayesnecjointlevel")`. The class is internal to `bayesnec`
  (`bayesnecjointfit-methods.R:24-44`) and inherits `bayesnecfit`, so dispatch
  would reach this method with a grid over every level's data.
- `object$fit$family$family` is one of `hurdle_gamma`, `hurdle_poisson`,
  `hurdle_negbinomial` or `zero_inflated_beta`. `bayesnec` fits these with a
  second block for the zeros, named `hu` or `zi` (`data-raw/sysdata.R:73-74`,
  `hurdle_family.R:21-36`). Their default expected value is the combined mean,
  and `bayesnec` reports either block through `dpar` (`hurdle_family.R:221-244`).
  Which to report is the decision deferred to the hurdle pull request (§3.3).
- `isTRUE(attr(object$bayesnecformula, "bayesnec_internal_truncation"))` with a
  `poisson` or `negbinomial` family (`hurdle_family.R:154-157`). For this formula
  `brms` returns a truncated expectation summed over a finite grid, and
  `bayesnec` replaces it with the closed form (`hurdle_family.R:171-207`). It
  arises only as the growth component of `bnec_hurdle()`
  (`bnec_hurdle.R:275-288`), which a user can extract as `hurdle_fit$growth`.
- `group_var` is not `NULL`. A `bnec()` fit has one curve: its group-level
  terms (`ogl()`, `pgl()`, `(par | g)`) are marginalised, as under T16.

`toxval_predict.bnecfit()` is the method for every other `bnecfit` subclass.
It stops naming `class(object)[1]` and the two supported classes. The
`bayesnecfit` and `bayesmanecfit` methods are more specific and dispatch first.

#### Measurement

On the `nec4param` component of `manec_example`, over a 50-point
`bnec_newdata()` grid, `brms::posterior_epred(<component>$fit, newdata, re_formula = NA)`
and `bayesnec`'s `posterior_epred()` on the pulled-out `bayesnecfit` gave
identical 100 × 50 matrices (`all.equal(tolerance = 0)`).

Calling `brms::posterior_epred()` on a `bnecfit` object itself, rather than on
`$fit`, fails with "no applicable method" unless the `bayesnec` namespace is
loaded, and dispatches to `bayesnec`'s method once it is.

#### Rejected alternatives

- Curves from `bayesnec`'s own `posterior_epred()` methods. They give the same
  matrices for the classes in scope, but there is no method for the
  `prebayesnecfit` components of a model set, and
  `posterior_epred.bayesmanecfit()` binds the equations' rows without saying
  which rows came from which equation (`posterior_epred.R:66-77`). One call on
  the `brmsfit` serves both classes.
- No use of `bayesnec` at all. The predictor would have to be found by parsing
  the formula, which `bayesnec` does by evaluating the term with its internal
  `crf()` (`bayesnecformula.R:786-788`, `:1709-1726`), and the trials and rate
  rules of the grid would have to be copied. Both have changed recently in
  `bayesnec` (#136 for the rate, #382 for level terms).
- `bayesnec::pull_brmsfit()` in place of `object$fit`. It returns `object$fit`
  for a `bayesnecfit` (`pull_brmsfit.R:30-32`) and has no method for a
  `prebayesnecfit`, so the field is read directly in both methods.

### 3.2 Model-averaged fits

#### Structure

A `bayesmanecfit` holds `mod_fits`, a named list of `prebayesnecfit` objects,
each `list(fit, model, init, bayesnecformula)` (`fit_bayesnec.R:183-185`), and
`mod_stats`, a data frame whose `wi` column holds the model weights with rows
named by equation (`expand_classes.R:517-522`; row names confirmed on
`manec_example`). Weights are pseudo-BMA by default (`helpers.R:1302-1304`).
Since 2.1.3.7 the object also holds `w_draw_seed` and `w_draw_index`
(`expand_classes.R:537-539`, `:579`); earlier objects do not
(`bayesmanecfit-class.R:41-44`), and the CRAN release has neither.

#### The average in bayesnec

`posterior_epred.bayesmanecfit()` (`posterior_epred.R:66-77`):

1. predicts each equation;
2. takes `n`, the smallest number of draws among the equations;
3. keeps, for each equation `m`, the rows `w_draw_index[[m]]`;
4. binds them in the order of `names(mod_fits)`.

`pull_draw_index()` (`helpers.R:215-227`) uses the stored index when
`object$sample_size == n` and every equation is named in it. Otherwise
`weighted_draw_index()` (`helpers.R:180-196`) rebuilds it:

```r
set.seed(seed, sample.kind = "Rejection")  # seed is w_draw_seed, or 216
lapply(model_set, function(m) sample(seq_len(n), as.integer(round(n * wi[[m]]))))
```

inside a function that restores the caller's random number state. Each
equation contributes `round(n * wi)` draws, sampled without replacement. The
total can differ from `n` by rounding. The CRAN release used an unseeded
`sample()` on every call `[master helpers.R:84-87, 102-106]`.

#### Specification

`toxval_predict.bayesmanecfit()` has the signature of §3.1. Its body:

1. The checks of §3.1, applied to each component's family and formula.
2. `model_set <- names(object$mod_fits)` and
   `wi <- object$mod_stats[model_set, "wi"]`.
3. One family across the set, or stop. `bnec()` fits a set with one family, so
   a failure here means a malformed object.
4. `x_var` from the first component, as `bnec_x_var(mod_fits[[1]]$bayesnecformula, mod_fits[[1]]$fit$data, x_var)`.
5. The grid from `bnec_grid(object, ...)`.
6. For each equation, `posterior_epred(mod_fits[[m]]$fit, newdata, re_formula = NA)`.
7. `n <- min(nrow(...))` over the equations.
8. The index from `bnec_draw_index(object, model_set, wi, n)`, which reproduces
   `pull_draw_index()`: the stored `w_draw_index` under the same three
   conditions, and otherwise the rebuild above, with the seed taken from
   `object$w_draw_seed` or 216. The rebuild runs inside
   `withr::with_preserve_seed()`, so the caller's `.Random.seed` and
   `RNGkind()` are unchanged.
9. The curve matrix is the equations' rows at the index, bound in `model_set`
   order, and is passed to `chk_defined_grid()` as in §3.1.
   `meta$draw_model` is `rep(model_set, lengths(index))`.
10. `meta$model_weights` is `stats::setNames(wi, model_set)`, and `meta$seed` is
    the seed the index was built from.

An equation whose `round(n * wi)` is 0 contributes no realisation. It is absent
from `draw_model` and listed in `model_weights`.

#### Measurement

On `manec_example` (`bayesnec` 2.1.3.39, which holds no `w_draw_index` or
`w_draw_seed`), a 50-point grid, and the rebuild with seed 216, the matrix
built by steps 2 to 9 was identical to `bayesnec::posterior_epred(manec_example, newdata)`
(`all.equal(tolerance = 0)`): 100 realisations, 83 from `nec4param` and 17 from
`ecx4param`.

`bnec_newdata()` on a `bayesmanecfit` calls `pull_out()` on the first equation
(`bnec_newdata.R:63-68`), which recomputes that equation's summaries. With a
500-point grid it took 1.12 s on `manec_example`, against 0.007 s on a
`bayesnecfit`. Since only the first equation is pulled out, its run time is set
by that equation's draws rather than by the number of equations, so a set of
two equations measures it.

Task 3 measured it on a set of `nec4param` and `ecx4param` fitted to
`nec_data` with four chains of 1000 retained draws, 4000 per equation
(`seed = 4000`, `bayesnec` 2.1.3.39). The 30 s at which the task was to stop
was not approached:

| resolution | `bnec_newdata()` | `posterior_epred()`, both equations |
|---|---|---|
| 200 | 3.0 s | 0.7 s |
| 500 | 3.2 s | 0.9 s |
| 1000 | 4.9 s | 1.6 s |

`toxval_predict()` at the default 500 took 5.8 s, and `ecx()` 3.7 s on a second
call. The fit held a `w_draw_index` (seed 815105072, weights 0.846 and 0.154),
so the stored index was used, giving 3385 and 615 realisations. The matrix was
identical to `bayesnec::posterior_epred()` over the same grid.

#### The choice of index

An index drawn by `toxval` from a `seed` argument, as `toxval_predict.drc()`
draws its bootstrap, was rejected. It would give a second model-averaged
posterior of the same object, so a `bayesnec` plot and a `toxval` estimate would
describe different draws. `bayesnec` has already made its index reproducible
without a user seed (#216); reproducing it takes about ten lines and one
comparison test. The risk is that `bayesnec` changes its rule. The comparison
test in task 3 fails if it does.

#### The component anchor

`anchor = "component"` (T23, `REFACTOR-claude.md` §3.8) takes each equation's
reference from that equation's realisations:

```r
reference[m] <- quantile(control[draw_model == m], sig_val)
```

applied to those realisations, with `ecnsec` paired to it. Without
`meta$draw_model`, every realisation belongs to one equation and
`"component"` equals `"model"`, which is T23's statement for every class
supported so far.

`bayesnec::nsec()` on a model set computes each equation's NSEC from that
equation's full posterior and then keeps the rows of the index
(`nsec.R:425-456`; the per-equation reference at `:422-424`). Its reference for
`ecx4param` on `manec_example` is a quantile of 100 control draws; `toxval`'s is
a quantile of 17. Both estimate the same quantile. The `toxval` estimate is
based on fewer draws, and for `sig_val = 0.01` and 17 draws the type-7 quantile
interpolates between the lowest two. Task 5 measures the difference.

Storing each equation's full control posterior in the `toxval_pred` was
rejected. It would add a slot whose only use is to reproduce `bayesnec` to within Monte Carlo
error. Recorded here so it can be reopened if the measured difference is large.

### 3.3 Classes

| class | `[master]` | structure | first PR | needs |
|---|---|---|---|---|
| `bayesnecfit` | yes | `fit`, `model`, `bayesnecformula`, summaries (`expand_classes.R:243-252`) | yes | — |
| `bayesmanecfit` | yes | `mod_fits`, `mod_stats`, `sample_size`, index (`expand_classes.R:577-584`) | yes | — |
| `bayesnecfit`, two-block family | — | `hurdle` element (`expand_classes.R:172-221`) | no | `dpar` |
| `bayesnechurdlefit` | no | `growth`, `survival` fits (`bnec_hurdle.R:342-344`) | no | `which` |
| `bayesnecgroupfit` | no | one fit per level in `fits` (`bnec_group.R:478-485`) | no | per-curve grid |
| `bayesnecjointfit` | no | one `brmsfit` over levels (`bnec_joint.R:436-444`) | no | per-curve grid |

Class vectors: `bnec.R:1096`, `:1102`, `:1123`; `bnec_hurdle.R:345`;
`bnec_group.R:486`; `bnec_joint.R:445`.

The combined curve of a hurdle pair is the product of the growth and survival
curves, paired by row after both are cut to the smaller number of draws
(`bayesnechurdlefit-class.R:63-101`), over the survival fit's range. `bayesnec`
selects with `which = "combined" | "growth" | "survival"` and refuses
`type = "relative"` (`:309-318`). A `toxval` method needs `which` and a ruling
on `relative`.

In a group fit each level is an independent `bnec()` fit to that level's rows
(`bnec_group.R:379-382`, `:424-426`), with its own concentrations and possibly
its own number of draws. Rows of different levels are different, unrelated
draws.

A joint refit is one `brmsfit` with the level in the population-level part of
every curve parameter, so its draws are aligned across levels. Since D40
(`ce670a39`) each level is read over its own concentrations
(`bayesnecjointfit-methods.R:66-81`, `:291-302`). `bayesnec`'s
`control_posterior()` has no branch for a level and reads the control at the
lowest concentration across all levels (`helpers.R:2723-2742`), which disagrees
with the level's own search start (`nsec.R:298-300`) when levels do not share
it.

Both of the last two conflict with the `toxval_pred` contract: one `x_vec`
checked against every curve (`chk_curves()`, `R/toxval-pred.R:373-403`), one
`meta$x_control`, and one `meta$n_realisation`. Supporting them needs a ruling
on whether `x_vec` and `x_control` become per curve. That is a breaking change
to a public contract, so it is decided on its own pull request, not here.

### 3.4 Additions to `toxval_pred`

#### Optional metadata

| element | type | rule |
|---|---|---|
| `meta$draw_model` | character, length `n_realisation` | no `NA`; every value in `names(meta$model_weights)` when that is present |
| `meta$model_weights` | named numeric | names unique and non-empty; values in `[0, 1]`; sum equal to 1 by `all.equal()` |

Both are checked in `chk_meta()` (`R/toxval-pred.R:203-252`) when not `NULL`, and
documented in the Metadata section of `?new_toxval_pred` as optional. The change
is additive: every existing object stays valid. `meta$seed` is documented as the
seed of a bootstrap or of a model-averaging index.

How the result reports `model_weights`, which `REFACTOR-claude.md` §3.10
requires for a `relative` estimate that drops equations, is decided with the
phase 4 implementation of `relative`. The `toxval` tibble keeps
`meta[settings_required()]` only (`R/toxval-tbl.R:18-22`).

#### The asymptote slot (after T24)

Per realisation, from the equation that produced it:

- decreasing: the `b_bot_Intercept` draw where `"b_bot_Intercept" %in% brms::variables(fit)`
  (parameter names `curve_params.R:536-543`, used as such at `ecx.R:510`); 0
  where the equation has no `bot` and the family is bounded below; `NA` where it
  has none and the family is `gaussian` or `student`, the two with no lower
  bound in `bayesnec`'s own test (`helpers.R:2694`);
- increasing: `response_upper_bound()`, which is 1 for a proportion and `NA`
  otherwise. No `bayesnec` equation has an upper-asymptote parameter: `top` is
  the response at zero concentration of a decreasing curve.

The equations with `bot` are found from the draws rather than from a list. The
result matches the nine named in `REFACTOR-claude.md` §3.10 and
`mod_groups$bot_free` (`data-raw/sysdata.R:20-23`). Under a link other than
identity `bot` is on the link scale (`hurdle_family.R:45-49`). `bnec()` uses the
identity link unless the user wrote one (`bayesnecformula.R:253-257`), so a
non-identity link sets the asymptote to `NA` and `relative` then refuses the
fit, as §3.10 specifies for an undefined asymptote.

On a model set the slot is filled from each equation's rows at the index, so
alignment holds by construction. Dropping the equations whose asymptote is `NA`
and renormalising, as §3.10 specifies, is equivalent to excluding their
realisations; the warning of T25 then gives the excluded fraction. §3.10 also
asks for the equations to be named, which needs the compute layer to read
`meta$draw_model` (task 4).

#### The response bound

`response_upper_bound()` (`R/compute.R`) returns `NA` for a `brms` `binomial` or
`beta_binomial` fit because `posterior_epred()` predicts a count of trials. A
`bnec()` fit is predicted with one trial (`expand_classes.R:327-330`), so its
curve is a proportion and the bound is 1. Implemented as a branch on
`source_class`, as the `drc` branch is, unless T26 has by then recorded trials
per curve, in which case the method records one trial.

#### The threshold slot

Left `NULL`. A NEC is `b_nec_Intercept` (`expand_classes.R:147`) and exists only
for the ten NEC equations (`data-raw/sysdata.R:6-8`). In a model set it would be
`NA` for the realisations of the others, and what such a realisation
contributes to an N(S)EC is the question #25 is open for. Filling the slot
before #25 rules would fix that meaning by default.

### 3.5 bayesnec behaviour not reproduced

- The back-transformation by the `crf()` term (§2.1; T6).
- The refusal of an absolute ECx for a `gaussian` fit of an equation without
  `bot`. `bayesnec` removed it, following OECD TG 201, with a comment at
  `ecx.R:234-237`, which agrees with `REFACTOR-claude.md` §3.10. The guard in
  §3.7 of the refactor plan, and the second of the "two guards needing homes" on
  #39, are deleted.
- Per-equation computation then mixing (`ecx.R:398-428`, `nsec.R:425-456`).
  Every metric is computed on the averaged matrix (§3.2).

### 3.6 The shared function names

`bayesnec` exports `ecx()`, `nsec()`, `nec()` and `ecnsec()`. Its methods
(`NAMESPACE`, checked 2026-10-23):

- `origin/predev`: `ecx` for `bayesnecfit`, `bayesmanecfit`,
  `bayesnecgroupfit`, `bayesnechurdlefit`, `bayesnecjointfit`; `nsec` for the
  same five plus `brmsfit` and `drc`;
- `[master]`: `ecx` and `nsec` for `bayesnecfit` and `bayesmanecfit`, and `nsec`
  for `brmsfit` and `drc`.

After phase 4 `toxval::ecx()` and `toxval::nsec()` are plain functions calling
`toxval_predict()` (`REFACTOR-claude.md` §3.3). With both packages attached the
later one masks the earlier. With `bayesnec` attached last, `ecx(<brmsfit>)`
stops with "no applicable method" and `nsec(<brmsfit>)` runs `bayesnec`'s copy,
the hazard #39 measured on 2026-08-19.

This work adds a paragraph to the README and to `?ecx` and `?nsec` advising
`toxval::ecx()`. An `.onAttach()` message was rejected. It would print for every
user who attaches both packages and would not change which function runs.

The remedy is in `bayesnec`: keep, rename or retire its functions, or call
`toxval`'s. That is not decided here.

### 3.7 Robustness across bayesnec structures

After task 3, both methods were run on one fit of every structure a user can
build on `bayesnec` `predev` at `116ea91b` (2.1.3.42), and on the five
structures that CR_workflows fits. Two read-only surveys listed the structures:
one of the `bayesnec` source and one of CR_workflows, whose only `bnec()` call
is `pkg/R/fit_bayesnec.R:85`. `bayesnec` was loaded from an export of
`116ea91b` with `pkgload::load_all()`, without installing it.

Each fit was predicted at `resolution = 50`. The curve matrix was compared with
`identical()` against `bayesnec`'s own method:
`brms::posterior_epred(<fit>, newdata = bnec_newdata(fit, 50), re_formula = NA)`.
`re_formula = NA` is passed because `bayesnec`'s `posterior_epred()` methods
set neither `newdata` nor `re_formula`, while its own curve predictions in
`expand_nec()` use `re_formula = NA`. Every equation of a model set was also
pulled out with `pull_out()` and compared as a `bayesnecfit`, as were the
per-level fits of a group fit and the two parts of a hurdle pair. `ecx()`, and
`summarise_metric(compute_nsec())`, were then run on each prediction. Fits used
two chains of 500 retained draws unless stated otherwise. The CR_workflows sets were
refitted with that repository's formulas, identity-link families and simulated
datasets (`pkg/data/*.rda`), with the same short sampler.

| structure | fits |
|---|---|
| families | `gaussian`, `beta`, `gamma`, `binomial` and `beta_binomial` with `trials()`, `poisson`, `poisson` with `rate()`, `negbinomial`, `bernoulli`, `zero_inflated_poisson` |
| formula terms | `crf(log(x))`, `crf(sqrt(x))`, `weights()`, `cens()`, `pgl(g)` on beta, `ogl(g)` on beta and on gaussian, `(top \| g)` on gamma, `disp(~x)`, `disp("power")`, `log(y)` as response, beta with a logit link |
| model sets | every equation on `nec_data` (17 kept); `amend(drop = )` to three; `+` of two fits; four chains with `thin = 3` |
| CR_workflows | `decline` sets on gamma, binomial, negbinomial and beta, and an `all` set on gamma, each with a control at concentration 0 |
| classes | the per-level fits of a `bnec_group()` fit; the gamma growth and Bernoulli survival parts of a `bnec_hurdle()` fit |

The results:

- All 128 predicted objects, 33 fits and 95 components, were identical to
  `bayesnec`'s matrix. The predicted objects comprise the 24 single fits,
  the 9 model sets, the 89 equations pulled out of those sets, the 4 group
  levels and the 2 hurdle parts.
- `ecx()` and the NSEC computation ran without error on every one.
- The refusals gave their messages on real objects. The objects were a
  `hurdle_gamma` fit, a `bayesnecgroupfit`, a `bayesnechurdlefit`, a
  `bayesnecjointfit`, and a `bayesnecjointlevel` built with
  `bayesnec:::joint_level_fit()`.
- With `thin = 3`, four chains and 500 post-warmup iterations, each equation
  held 668 draws while `sample_size` recorded 667. The stored index was
  therefore bypassed by both packages and rebuilt from `w_draw_seed`, giving
  668 realisations. `bayesnec`'s stored summaries, its `ecx()` and its `nsec()`
  pass the stored `sample_size` and so keep the stored index of 667 draws. On
  such a set `toxval`'s metrics and `bayesnec::ecx()` are therefore computed on
  different draws. The fault is `bayesnec` #477. `bayesnec_manec_example` holds
  100 draws per equation and records 100, so the task 5 comparison on it is
  unaffected. The `decline` sets on binomial and beta gave 1001 and 999
  realisations, from rounding each equation's share. Both packages do the same.
- In a session using `RNGkind("L'Ecuyer-CMRG")`, the rebuilt index again
  matched `bayesnec`. It differed from the index rebuilt under the default
  generator, because `weighted_draw_index()` fixes the seed and the sample
  kind but not the generator.

`zero_inflated_poisson` and `zero_inflated_negbinomial` are accepted. `bayesnec`
leaves them out of its two-part registry (`data-raw/sysdata.R:60-74`): `brms`
fits them as a mixture with a constant zero-inflation probability, so their
expected value is one curve.

Four structures could not be fitted, and so are not covered:

- the zero-truncated count part of `bnec_hurdle()`, which needs `brms` 2.23.2
  (2.23.0 is installed), so its refusal is tested on an altered copy only;
- `disp(~z)` with a covariate other than `x`, for which `bnec()` itself fails
  after sampling (`bayesnec` #443);
- `+` on fits with different numbers of draws, which `bayesnec` refuses ("Each
  object in the list must have the same dimensions");
- `pull_out()` with two equations, which 2.1.3.42 refuses ("Argument model can
  only take one value").

One case gives a wrong upper bound, found from the code rather than from a
fit. With a `trials()` expression rather than a column, such as `trials(n * 2)`,
`bnec_newdata()` sets `n` to 1 and the curve is predicted for two trials.
`response_upper_bound()` then gives 1 for a curve that reaches 2. A bare
`trials()` column, the form in every example found, is unaffected.

The scripts are in `tests/local/bayesnec-robustness/`, and its `README.md`
gives their order and settings. `compare-fits.R` makes the comparison and
`check-edges.R` the checks on the joint level, the log predictor, the thinned
set and the generator. The fits are written to the user cache rather than the
repository, because they take about 3.3 GB.

## 4. Tasks

The exported methods go in `R/toxval-predict.R` after the `drc` method,
documented with `@describeIn toxval_predict`. Their helpers go in
`R/toxval-predict-bayesnec.R`. roxygen takes the `\name` of a merged topic from
the first block it reads, and reads files in C-locale order, in which
`toxval-predict-bayesnec.R` precedes `toxval-predict.R`. With the methods there,
`man/toxval_predict.Rd` was named `toxval_predict.bayesnecfit` (found in task 2).
An `@include` would fix the order only by adding a `Collate` field to
`DESCRIPTION`. Tests go in `tests/testthat/test-toxval-predict-bayesnec.R`,
which calls `skip_if_not_installed("bayesnec")` at the top. `air.toml` is present, so R code
is formatted with `air` before each commit. `NEWS.md` is written by `fledge`
from commit messages and is not edited by hand.

### The decision record (task 0)

- Add T35 to `notes/implementation/02_decisions.md` after T34, stating §1.1 and
  §1.4.
- Amend every `toxval` passage in §1.2.
- Draft a decision comment for #39 in
  `notes/drafts/issue-39-toxval-methods-comment.md`, following CLAUDE.md §14.
  #68 holds the decision, its reason and the superseded split, so the comment
  is the decision in one sentence and a link to #68. RF posts it.
- Cite #68 in T35.

The refactor documents are also edited on PR #66 (AP, T34; `REFACTOR-claude.md`
in `4b90146`). Task 0 is therefore done after #66 merges into `phase-2`, as the
last task before the pull request opens. By then the next free decision number
is known, and the passages in §1.2 no longer change under the edit. Edit only
the passages listed.

Done when no passage in §1.2 still places the methods in `bayesnec`, except as
history.

### Fixtures (task 1)

Add to `data-raw/DATASET.R`, each saved with `compress = "xz"` and passed
through `strip_stanmodel()`:

| fixture | fit | tests |
|---|---|---|
| `bayesnec_manec_example` | `bayesnec::manec_example` | model average, §3.2 |
| `bayesnec_binomial` | `bnec(y \| trials(n) ~ crf(x, model = "nec3param"), family = binomial)` on simulated data, `set.seed()` recorded | proportion bound, §3.4; `bot`-free asymptote |
| `bayesnec_logx` | `bnec(y ~ crf(log(x), model = "ecx4param"))` on `nec_data` | predictor scale, §2.1 |

`bnec()` fits use `chains = 2, iter = 2000, thin = 2` like `bnec_model_1`. Record
the `bayesnec` version in a comment, as the existing `bayesnec_` data fixtures
do.

The fixtures already total 5.4 MB, against the 5 MB CRAN suggests for a
package tarball. `manec_example` was 1.09 MB as an unstripped `xz` file. RF
ruled on 2026-10-23 that fixture size does not limit this work, because the
tests are needed during development and CRAN submission is not near. Size is
settled when phase 6 prepares the package for CRAN. A fit needed only by
`tests/local/` can then be kept out of the built package, as `bayesnec` does
with `^tests/local$` in its `.Rbuildignore`. Record the size of each new fixture
in the pull request.

Done when the fixtures load through `helper-fixtures.R`.

Found in task 2: loaded without `bayesnec` installed, `bayesnec_logx` warns that
the `bayesnec` namespace was replaced by `.GlobalEnv`. The family objects that
`bayesnec`'s `validate_family()` returns hold a reference to a `bayesnec`
function frame. This was shown for `"beta"`, for `Beta(link = "identity")` and
for `binomial`, each saved and then loaded in a session with `bayesnec`
removed from the library path. `brms::Beta(link = "identity")` called directly
loads cleanly, as do `bnec_model_1` and `bayesnec_binomial`. The fit is stored
as `bnec()` returned it. The tests that use it are skipped without `bayesnec`,
so the warning is the only effect.

### The single-fit method (task 2)

- `toxval_predict.bayesnecfit()`, `toxval_predict.bnecfit()`,
  `check_bayesnec_installed()`, `bayesnec_available()`, `bnec_x_var()`,
  `bnec_grid()`, `chk_bayesnec_supported()`, as §3.1.
- `build_meta()` gains `data` (§3.1). The optional elements are added in task
  3, where they are first used and tested.
- `bayesnec` in `Suggests`, approved by RF on 2026-10-23.

Tests, on `bnec_model_1` (`nechorme`, beta), `bayesnec_nec4param`,
`bayesnec_ecx4param` (both gaussian), `bayesnec_binomial` and `bayesnec_logx`:

1. `curves[[1]]` is identical to `bayesnec`'s
   `posterior_epred(fit, newdata = bnec_newdata(fit, resolution = r))`.
2. `x_vec` equals the grid column; a supplied `x_range` is honoured.
3. `meta`: `source_class`, `x_var`, `x_control == min(fit$fit$data[[x_var]])`,
   `family`, `realisation == "draws"`, `n_realisation == brms::ndraws(fit$fit)`.
4. `x_var = NULL` is read from the formula; a matching `x_var` is accepted; a
   different one stops; a non-`NULL` `group_var` stops.
5. On `bayesnec_logx`, `range(x_vec)` equals the range of raw `x`.
6. On `bayesnec_binomial`, every curve value lies in `[0, 1]` and
   `response_upper_bound()` is 1.
7. Each refusal in §3.1 stops with its message, on a copy of a fixture altered
   for the purpose (a family name, a formula attribute, a class vector). The
   method for other subclasses is tested on objects of class
   `c("bayesnecgroupfit", "bnecfit")` and the other unsupported classes.
8. With `bayesnec_available()` mocked to `FALSE`, the method stops naming
   `bayesnec`.
9. The result passes through `compute_ecx()`, `compute_nsec()` and
   `summarise_metric()` and returns a `toxval` tibble.
10. `ecx()` on each fixture returns a `toxval` tibble. `ecx()` calls
    `toxval_predict()` from `6bdd99c`, so this needs no change to `ecx()`.

Done when the tests pass, the file is skipped cleanly with `bayesnec` absent,
and no other test fails that passes at the head of PR #66. At `6bdd99c`, 21
tests fail in `test-compute.R`, `test-validate.R` and `test-summarise.R`,
because they still call the `ecx_val` argument that T32 renamed `proportion`.
AP is updating them on #66. R CMD check is required to pass once they are.

### The model-averaged method (task 3)

- `toxval_predict.bayesmanecfit()` and `bnec_draw_index()`, as §3.2.
- The two `meta` elements and their checks, as §3.4.

Tests, on `bayesnec_manec_example`:

1. The curve matrix is identical to `bayesnec`'s
   `posterior_epred(fit, newdata = bnec_newdata(fit, resolution = r))`.
2. `table(meta$draw_model)` equals `round(n * wi)` per equation.
3. `meta$model_weights` equals `mod_stats$wi`, named.
4. `.Random.seed` and `RNGkind()` are the same before and after the call, and
   two calls give identical objects.
5. A copy given a `w_draw_index`, `w_draw_seed` and matching `sample_size` uses
   that index; with `sample_size` changed, the index is rebuilt.
6. A copy with weights `c(0.999, 0.001)` drops the second equation from
   `draw_model` and keeps it in `model_weights`.
7. A copy with one component's family changed stops.

Tests of the `meta` checks go in `tests/testthat/test-toxval-pred.R`: wrong
length, `NA`, a name not in `model_weights`, weights not summing to 1.

Measure the run time of `bnec_newdata()` on a model set of the default
equations at 4000 draws (§3.2).

Done when the tests pass and the measurement is recorded in the pull request.

### Asymptotes and the component anchor (task 4)

Waits for phase 4 to add the `asymptote` slot (T24), `type = "relative"` and
`anchor` (T23).

- Fill `asymptote` as §3.4.
- Make `anchor = "component"` group the realisations by `meta$draw_model` (§3.2).

Stop and ask AP how phase 4 implements `"component"`. If it already groups by
`meta$draw_model`, treating `NULL` as one equation, this task only fills the
element; if not, the grouping is added here.

Tests: on `bayesnec_nec4param` the decreasing asymptote equals the
`b_bot_Intercept` draws; on `bnec_model_1` (`nechorme`, beta, no `bot`) it is 0;
on a copy with the family set to `gaussian` it is `NA` and `relative` stops; on
`bayesnec_manec_example` the `"component"` reference of each equation equals
the `sig_val` quantile of that equation's control realisations, and `"model"`
equals the pooled quantile.

Done when `type = "relative"` and both anchors are tested on the model set.

### End-to-end tests and documentation (task 5)

Waits for `nsec()` to call `toxval_predict()`, which AP is doing next on #66,
and for task 4. `ecx()` has called it since `6bdd99c`.

- `tests/testthat/`: `ecx()` and `nsec()` on every fixture return a `toxval`
  tibble with `# Model:` naming the class.
- `tests/local/test-bayesnec-compare.R`, run by hand:
  - the decreasing absolute ECx, per realisation, against
    `bayesnec::ecx(posterior = TRUE)` at the same `resolution`, for
    realisations crossing within the grid, to `1e-8` (§2.2);
  - on `bayesnec_logx`, `toxval` equals `exp()` of `bayesnec` (§2.1);
  - `nsec(anchor = "component")` against `bayesnec::nsec()` on
    `manec_example` at `sig_val = 0.01`: the difference in each quantile is
    reported in the pull request, not asserted (§3.2).
  - the fits of `tests/local/bayesnec-robustness/` (§3.7), where structures
    beyond the fixtures are wanted. A model set whose `sample_size` differs
    from its draws (`bayesnec` #477) is left out of the per-realisation
    comparison with `bayesnec::ecx()`.
- The README section on `bayesnec` fits and the masking paragraph (§3.6).
- The `Description` field of `DESCRIPTION` names `bayesnec`.
- An example on `?toxval_predict` using `manec_example`, inside `\donttest{}`
  and guarded by `requireNamespace("bayesnec", quietly = TRUE)`.

Done when R CMD check passes with `bayesnec` installed, and with it absent and
`_R_CHECK_FORCE_SUGGESTS_=false`, and `tests/local/` passes.

## 5. Branch and dependencies

`phase-7-bayesnec-methods` was branched from `upstream/phase-4-5-spine-and-tibble`
at `9a3b846`, rebased onto `6bdd99c` on 2026-10-23, and tracks it. AP adds
commits to #66 rather than rewriting them, so while #66 is open the branch is
rebased with `git rebase upstream/phase-4-5-spine-and-tibble`.

PR #65 was squash-merged into `phase-2`, and #66 is expected to be as well. A
plain rebase onto `upstream/phase-2` would then replay #66's commits over their
own squashed copy. Rebase with
`git rebase --onto upstream/phase-2 <last #66 commit>` instead, which replays
only this branch's commits. Then do task 0 and open the pull request from
`beckyfisher:phase-7-bayesnec-methods` into `open-AIMS:phase-2`, ready for
review rather than as a draft.

This branch's code is in new files, so a rebase can conflict only in
`DESCRIPTION`, `NAMESPACE` and `man/`, which `devtools::document()` regenerates,
and in the refactor documents that task 0 edits.

| needed | by task | from | state on 2026-10-23 |
|---|---|---|---|
| `toxval_predict()`, `new_toxval_pred()`, `group_keys()`, `build_meta()` | 2, 3 | phase 2 | merged into `phase-2` |
| `compute_ecx()`, `compute_nsec()`, `summarise_metric()`, `new_toxval()` | 2, 3 | phases 2, 4 | on PR #66 |
| `resolution` default of 500 (T31), `chk_defined_grid()` | 2, 3 | phase 4 | on PR #66 (`6bdd99c`) |
| trials per curve (T26) | 2 | phase 4 | not started |
| `asymptote` slot (T24), `type = "relative"` | 4 | phase 4 | not started |
| `anchor` (T23) | 4 | phase 4 | not started |
| `ecx()` on the spine; `proportion` (T32) | 2, 5 | phase 4 | on PR #66 (`6bdd99c`) |
| `nsec()` on the spine | 5 | phase 5 | AP's next step on #66 |

Tasks 1 to 3 need only code that is merged or on PR #66. Task 2's response
bound uses `source_class` until T26 lands (§3.4). Task 4 needs phase 4 work that
has not started, and task 5 needs task 4 and `nsec()`. Phase 6, CRAN
publication, is not a prerequisite of any task.

## 6. Decisions and points for review

RF approved three of these on 2026-10-23: `bayesnec` in `Suggests` (§3.1), the
difference under `anchor = "component"` (§3.2), and fixture size (task 1, §4).
The rest are listed in `BAYESNEC-human.md` §6. The arguments are in §3.2 (the
draw index), §3.3 (scope), §3.4 (`threshold`) and §2.1 (the predictor scale).
