# toxval Refactor Plan

## 1. Motivation

The refactor has three goals:

1. Make adding a **new model class** easy.
2. Fix the current duplication, inconsistencies, and bugs.
3. Standardise all return structures.

## 2. Scope

**In scope.** Restructure current functionality onto the new architecture:

- standardise outputs to a `toxval` tibble
- replace the `posterior` argument with an optional `draws` list column
- consolidate argument validation on `chk`
- remove duplication, dead code, and fix the known inconsistencies

**Out of scope but adjacent.** Three decisions have to be made *before* or
*alongside* this work because they determine what the numbers are, not just what
shape they come in. Each has its own issue; none is settled here:

| | what it decides | issue |
|---|---|---|
| Reference semantics | how the reference y-value is computed | #19 |
| Direction | increasing vs decreasing responses, and how hormesis relates | #20 |
| Frequentist intervals | how uncertainty is generated for a non-Bayesian fit | #43 |

**Sequencing constraint.** The `bayesnec` dependency untangle (#39) shares its
central abstraction with this plan — see [3.3](#33-single-generic-architecture)
— and has a hard ordering requirement against it. See
[4. Phased implementation](#4-phased-implementation).

## 3. Design decisions

### 3.1 Output contract: everything is a `toxval` tibble

Every metric function (`ecx()`, `nsec()`, etc) returns the same tidy container,
a `tbl_df` subclass named `toxval`. Model class never changes the return type.

**The column rule.** Core columns are always present. A descriptor column
appears when it is *meaningful* for that metric and model class, and takes `NA`
when it is meaningful but unavailable. So `group` is absent for an ungrouped fit
(not meaningful), while `control` is present but `NA` for `type = "direct"`
(meaningful for `ecx`, undefined for that type).

Core columns:

- `metric`
- `direction`
- `estimate`
- `conf.low`
- `conf.high`

Descriptor columns, present when meaningful:

- `group` — one row per level, when grouped
- `response` — one row per response, for multivariate fits
- `ecx_val`, `type` — `ecx` only
- `sig_val` — `nsec` / `n(s)ec` only
- `control`, `reference` — `ecx`
- `reference`, `ecnsec`, `ecnsec.low`, `ecnsec.high` — `nsec`

Optional column:

- `draws` — a list column of realisations, attached when `draws = TRUE`.
  See [3.5](#35-draws).

#### `metric` is a closed vocabulary

`metric` is machine-readable, validated against a fixed set, and never has a
value interpolated into it:

```
metric ∈ { "ecx", "nsec", "nec", "n(s)ec", "noec" }
```

The value that parameterises the metric lives in its own column (`ecx_val`,
`sig_val`), never baked into the string. So it is `metric = "ecx"`,
`ecx_val = 10` — **not** `metric = "ec10"`.

This matters because results get **stacked**: multiple `ecx_val` values in one
call (#9), several responses and directions from `nsec_multi`, a user binding
EC10 / EC50 / NSEC into one table, or a saved CSV read by someone else months
later. A parsed string (`"ec10"` → `as.numeric(sub("ec", "", metric))`) breaks
on `type = "direct"`, on fractional values (`"ec12.5"`), and is inconsistent
between `ecx` (bakes its value in) and `nsec` (does not). Putting the value in a
column makes multiple `ecx_val` "just more rows".

`n(s)ec` is listed for completeness — see [5. Notes](#5-notes); it is not
implemented by this refactor.

#### Run settings

Run settings (`resolution`, number and source of realisations, model class) are
displayed as tibble header lines via a `tbl_sum()` method, in the way
`# Groups:` appears on a grouped tibble.

**The dividing line:** the header carries per-call constants that are not
columns; anything that varies by row, or that anyone might compute on, is a
column. `ecx_val` varies by row as soon as #9 lands, so it is a column and is
*not* restated in the header — a `# Metric: EC10` line would duplicate data
already visible on the row, and could only ever name one of several `ecx_val`
values.

Header lines are **display only**. Anything a caller needs programmatically is a
column. Bare attributes on a tibble subclass are dropped by most dplyr verbs
unless `dplyr_reconstruct.toxval` and friends are implemented, so a user doing
`result |> filter(group == "A")` would silently lose them or keep ones that
no longer describe the data.

#### Examples

```r
# basic output
ecx(manec_example, ecx_val = 10)
#> # A tibble: 1 x 9
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing    0.832    0.818      1.05      10 absolute    1.02     0.918

# control is NA but still present
ecx(manec_example, type = "direct", ecx_val = 0.5)
#> # A tibble: 1 x 9
#> # Model:    bayesmanecfit
#> # Settings: resolution 100 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high ecx_val type   control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>    <dbl>     <dbl>
#> 1 ecx    decreasing     1.13     0.98      1.31     0.5 direct      NA       0.5

# grouped output — more rows, plus a `group` column
ecx(brms_grouped, x_var = "x", group_var = "site", by_group = TRUE, ecx_val = 10)
#> # A tibble: 3 x 10
#> # Model:    brmsfit
#> # Settings: resolution 1000 | 4000 posterior draws
#> # Groups:   site [3]
#>   metric direction  group estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>      <chr>    <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing A         0.71     0.55      0.88      10 absolute    0.98      0.88
#> 2 ecx    decreasing B         1.02     0.83      1.24      10 absolute    1.10      0.99
#> 3 ecx    decreasing C         0.44     0.31      0.58      10 absolute    0.87      0.78

# frequentist fit — same shape, realisations from a parametric bootstrap
ecx(drc_fit, x_var = "x", ecx_val = 10)
#> # A tibble: 1 x 9
#> # Model:    drc
#> # Settings: resolution 1000 | 1000 bootstrap realisations
#>   metric direction  estimate conf.low conf.high ecx_val type     control reference
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl> <chr>      <dbl>     <dbl>
#> 1 ecx    decreasing     1.88     1.62      2.19      10 absolute    9.91      8.92
```

```r
nsec(brms_model_1, x_var = "x")
#> # A tibble: 1 x 10
#> # Model:    brmsfit
#> # Settings: resolution 1000 | 4000 posterior draws
#>   metric direction  estimate conf.low conf.high sig_val reference ecnsec ecnsec.low ecnsec.high
#>   <chr>  <chr>         <dbl>    <dbl>     <dbl>   <dbl>     <dbl>  <dbl>      <dbl>       <dbl>
#> 1 nsec   decreasing     2.31     1.88      2.79    0.01     0.918   11.4       8.91        14.2
```

### 3.2 Type stability

A function must not change its return **type** based on an argument value. This
is why the `posterior` argument goes ([3.5](#35-draws)).

Arguments that change only values or row counts (`ecx_val`, `type`, `sig_val`,
`by_group`) stay as arguments. A descriptor column appearing or not is governed
by the column rule in [3.1](#31-output-contract-everything-is-a-toxval-tibble),
not by an argument flipping the return type.

### 3.3 Single-generic architecture

`toxval_predict()` is the only S3 generic. It dispatches on model class and
returns a `toxval_pred`. The metric functions (`ecx`, `nsec`, `nsec_multi`,
...) are plain, non-generic functions that compute on a `toxval_pred`.

Metric functions accept **either a fitted model or a pre-built `toxval_pred`**
(one `inherits()` check at the top), which enables predict-once / compute-many
and makes the compute functions testable in isolation.

Adding a new model class is one method and nothing else:

```r
toxval_predict.gam <- function(object, x_var, resolution, x_range, ...) {
  # 1. build the x grid
  # 2. generate realisations of the curve (posterior draws, or a
  #    parametric bootstrap from coef() / vcov() -- see 3.4)
  # 3. return a toxval_pred
}
```

**`toxval_pred` is public API.** It is not an internal convenience. Under #39
the `bayesnec` classes are served by methods that live *in `bayesnec`*, which
imports `toxval` and constructs a `toxval_pred` from outside this package. So
the constructor (`new_toxval_pred()`), the slot contract, and the validator are
all exported and documented, and changes to them are breaking changes.

This is also what makes #39 tractable: `toxval_pred` **is** the "plain-input
API" that untangle needs. It should be built once, here, and not prototyped
separately.

### 3.4 The `toxval_pred` intermediate object

`toxval_pred` holds realisations of the fitted curve as a **list**, one element
per group or response (a single element when ungrouped), plus metadata:

```
toxval_pred:
  curves    : named list of matrices, each [n_realisation x n_x]
  x_vec     : numeric [n_x]
  threshold : named list of numeric [n_realisation], or NULL
              per-realisation threshold parameter where the model has a
              genuine one (a NEC); NULL otherwise. Needed for `nec` and
              `n(s)ec`, which are not recoverable from `curves`.
  meta      : source_class, x_var, group_var / multi_var, resolution, x_range,
              dimension ("none" | "group" | "response"), family, realisation
              source ("draws" | "bootstrap"), n_realisation, ...
```

#### One mode, not two  (#43)

An earlier version of this plan carried two modes — `draws` for Bayesian fits
and `interval` for frequentist ones, where the three columns of
`predict(drc_fit, interval = "confidence")` were treated as three curves. **That
is dropped.**

Inverting a pointwise confidence band on `y` does not give a valid confidence
interval on `x`. This is the inverse-regression / calibration problem: coverage
is wrong, and it degrades worst where the curve is flat — which for a
concentration–response curve is exactly where NSEC sits. It is the same reason
an LD50 confidence interval cannot be read off a fitted-line confidence band.
Two quantities computed that differently should not share the column names
`conf.low` / `conf.high`.

Instead, frequentist fits generate realisations by **parametric bootstrap**:
draw `n_boot` parameter vectors from `MVN(coef(fit), vcov(fit))`, evaluate the
mean function at each, and treat the result as realisations exactly as posterior
draws are treated. For `drc` this is available directly — `fit$fct$fct(x, parm)`
evaluates the mean curve at an arbitrary parameter matrix, and `coef()` and
`vcov()` are both defined.

| Case | `curves` list | realisation source | `n_realisation` |
|---|---|---|---|
| Ungrouped Bayesian | 1 element | draws | n_draws |
| `by_group` / `group_var` | one per factor level | draws | n_draws |
| Multivariate (`nsec_multi`) | one per response | draws | n_draws |
| `drc` | one per curve (or 1) | bootstrap | n_boot |
| `glm` / `gam` (future, #21) | one per group | bootstrap | n_boot |

What this buys:

- **The compute functions have no branch at all.** They iterate `curves` and
  interpolate. There is no `mode`.
- **`draws = TRUE` works for frequentist fits**, so [3.5](#35-draws) needs no
  "no realisations for a frequentist fit" carve-out.
- **#21 is answered by the same mechanism.** Anything with `coef()` and
  `vcov()` comes in through one door.
- **`prob_vals` means one thing.** Under the two-mode design it set quantile
  positions for Bayesian fits and the `level` argument of `predict.drc` for
  frequentist ones.

Costs, stated plainly: `drc` numbers change (the current ones are inverted-band
values, which are not what they claim to be); bootstrapping is slower than one
`predict()` call; and it needs a seed, so `n_boot` and `seed` become documented
arguments. **A vignette is required** setting out how this differs from
`drc`'s own `ED()` intervals, which use the delta method. The goal is a single
defensible definition across fitting platforms, not replication of what each
platform does natively.

#### Realisation alignment

**Invariant: realisation *i* is the same underlying draw in every slot of a
`toxval_pred`.** Row *i* of every matrix in `curves`, and element *i* of
`threshold`, come from the same posterior draw (or the same bootstrap
replicate). This is what makes the `draws` list columns in
[3.5](#35-draws) honest, and it is cheap to assert in the validator.

For a single fit it holds trivially. For a **model-averaged** fit it has to be
constructed, and the amount of work depends on the quantity:

- **Curve-derived metrics (`ecx`, `nsec`, `ecnsec`) need nothing.**
  `toxval_predict.bayesmanecfit()` returns *one* model-averaged curve matrix,
  produced by a single weighted resample, and every metric is computed from it.
  Alignment is automatic because there is only one sampling step.

  This also means `bayesnec`'s per-metric mixing is redundant. `ecx` is a
  per-draw row-wise function of the curve, so mixing and computing commute —
  measured on `manec_example`, computing on the averaged curve and computing
  per model then mixing agree to within Monte Carlo error. `nsec` is not
  row-wise (its reference is an across-draw quantile), which is a definitional
  question settled in [3.8](#38-references-under-model-averaging), not an
  alignment one.

- **Threshold-derived metrics (`nec`, `n(s)ec`) need a shared index.** A NEC is
  a model *parameter*, not a functional of the curve, so it cannot be read off
  the averaged curve. The component index must be drawn **once** and reused for
  every quantity, so that realisation *i* means "component model `m[i]`,
  iteration `j[i]`" throughout:

  ```r
  idx <- sample(seq_along(models), n, replace = TRUE, prob = weights)  # once
  ```

  This keeps the `weighted_samples` approach already used in `bayesnec` and in
  `ssdtools` — it is not a change of method, only of where the randomness is
  drawn. `meta$draw_model` records the component per realisation, which is what
  lets `n(s)ec` label each draw a NEC or an NSEC. Tracked as `bayesnec` #216.

**Both of these are `bayesnec` fixes**, implemented in
`toxval_predict.bayesmanecfit()` and in `bayesnec`'s averaging helpers.
`toxval` only states the invariant and tests it.

Note that moving to a single averaged curve matrix fixes *alignment* but not
*reproducibility*: the one remaining resample in `bayesnec`'s averaging is still
unseeded, and `toxval` never touches it. That half is `bayesnec` #216 and has to
be fixed there — see [5. Notes](#5-notes).

### 3.5 Draws

There is **one function per metric**, and it always returns a `toxval` tibble.
The realisations are an optional list column, not a second function and not a
second return type:

```r
pred <- toxval_predict(fit)
ecx(pred)                  # summary
ecx(pred, draws = TRUE)    # summary + a `draws` list column, one cell per row
```

One compute path produces the per-realisation values; `draws` only controls
whether they are kept. `draws = FALSE` is the default, because a 1000-point grid
with 4000 draws across several groups is tens of MB and most callers want three
numbers.

Long form is one pipe stage, not an API:

```r
ecx(fit, draws = TRUE) |> tidyr::unnest(draws)
#> # A tibble: 4,000 x 6
#>   metric direction  ecx_val estimate .draw  value
#>   <chr>  <chr>        <dbl>    <dbl> <int>  <dbl>
#> 1 ecx    decreasing      10    0.832     1  0.841
#> 2 ecx    decreasing      10    0.832     2  0.795
#> 3 ecx    decreasing      10    0.832     3  0.826
#> # i 3,997 more rows
```

`.draw` follows the `posterior` / `tidybayes` convention deliberately.

#### Why a list column rather than `*_draws()` functions

An earlier draft of this plan split each metric into `ecx()` / `ecx_draws()`.
That is dropped. Every argument for the split turns out to be an argument for
the list column instead:

- **Type stability.** `draws = TRUE` does not change the return type — it is a
  `toxval` tibble either way, with one more column. That is the same
  "column appears when it is meaningful" rule used everywhere else in
  [3.1](#31-output-contract-everything-is-a-toxval-tibble), so §3.2 is satisfied
  without a second function.
- **Unit of observation.** The objection to `posterior = TRUE` was that it
  returned one row per realisation where the summary returns one row per
  estimate. The list column *keeps* one row per estimate. Unnesting changes the
  unit of observation, and that is then the caller's explicit choice at the call
  site rather than something a flag does invisibly.
- **Paired quantities.** `nsec` produces two per-realisation quantities — the
  NSEC realisations and the `ecnsec` realisations — from the same draws. As two
  list columns on the same row they are aligned by construction. In long form
  they need a `quantity` column and a self-join to re-pair.

Against that, `*_draws()` would double the exported surface (`ecx_draws`,
`nsec_draws`, `nsec_multi_draws`, later `nec_draws`, `noec_draws`) and duplicate
the documentation and tests, to save one `unnest()`.

**Pairing is guaranteed, not assumed.** Two list columns on the same row imply
realisation *i* of one corresponds to realisation *i* of the other, and the
`toxval_pred` alignment invariant in
[3.4](#34-the-toxval_pred-intermediate-object) makes that true by construction —
including for model-averaged fits, which is a `bayesnec`-side requirement rather
than something the output has to hedge about.

#### Retiring `posterior`

`posterior = TRUE` today *replaces* the return value with a bare vector of
draws. `draws = TRUE` *adds* a column. The shape changes either way, so the
deprecation message has to say so.

Since 1.0.0 has been released, keep `posterior` accepted for one release behind
`lifecycle::deprecate_warn()`, mapping it to `draws` and warning that the return
shape has changed. Two current defects make the retirement easy to justify:

- `nsec.drc()` has no `posterior` parameter at all — it is absorbed by `...`, so
  `nsec(drc_fit, posterior = TRUE)` passes `chk_logical()` in the generic and is
  then **silently ignored**.
- `posterior` was doing two jobs: "give me the realisations" and "change the
  return type". Only the first is wanted.

### 3.6 Direction

`direction` is a **property of the result, not an argument.** For each curve the
estimator looks for the first *decreasing* crossing and the first *increasing*
crossing of the reference over the fitted range, and returns what it finds. If
there is no crossing of a given direction within range, that estimate is `NA`.

This replaces `hormesis_def` as the mechanism for non-monotone curves. A
hormetic curve simply has both an increasing and a decreasing crossing, and both
are reported; there is no separate argument and no `modify_posterior()` blanking
out part of the grid.

It also:

- answers #20 directly — `%inhibition` and other increasing responses come in
  with no new function;
- makes `min(zero_crossings(...))` well defined. "First crossing" is ambiguous
  on a non-monotone curve; "first downward crossing" is not;
- lets univariate `gam` fits in on the same terms as everything else;
- collapses most of `nsec_multi`'s `type` argument. `type = "increasing"` is
  `filter(direction == "increasing")` on the result, and `type = "both"` is no
  filter. Only `type = "lower"` survives as an argument, because it is a
  *selection rule* using `criterion` rather than a filter.

**To pin down:** whether a curve with no crossing in one direction emits a row
with `NA` or omits the row. Under the column rule the direction was looked for
and not found, which argues for a row with `NA`.

### 3.7 Validation

`chk` is the default for all argument checks.

- Predict-level arguments (`x_var`, `group_var`, `x_range`, `resolution`, and
  that the model has a `toxval_predict` method) are validated in
  `toxval_predict()`.
- Metric arguments (`ecx_val`, `type`, `sig_val`, `prob_vals`, `xform`) are
  validated in the metric entry point, so every class gets identical checks.
  - This fixes the current bug where `ecx.bnecfit` never validates the `ecx_val`
    range while `ecx.brmsfit` does.
- Shared checks (`prob_vals` ordering, `xform` is a function, `resolution`) live
  in one internal `chk` helper, replacing the four hand-rolled copies in
  `ecx()`, `nsec()`, `nsec.drc()` and `nsec_multi()`. `nsec.drc()`'s copy is
  pure duplication — the generic has already run those checks before dispatch.
- Use `chk::chk_subset()` (or `rlang::arg_match()`) for closed sets like `type`,
  so the error names the valid values.
- `stop()` calls are replaced.

Specific defects to fold into this pass, each currently unguarded:

- `is.na(x_range)` is called on a length-2 vector in `ecx.brmsfit` and
  `nsec.brmsfit`, which errors on R >= 4.2. Pinned in the suite as
  `expect_error(..., "the condition has length > 1")`. `nsec_multi` gets it
  right (`length(x_range) == 1 && is.na(x_range)`) — copy that.
- `nsec.brmsfit` reads `x_range` at `R/nsec.R:191`, *before* the
  `missing(x_var)` guard at line 195, so a missing `x_var` errors obscurely.
- Column names are matched with `max(grepl(x_var, col_names)) == 0`, so
  `x_var = "x"` matches a column called `max_x`. Use `%in%`.
- `nsec.drc()` takes `object$data[, 4]` positionally for `curveid` (#34).
- `nsec.drc()` calls `xform(nsec_out)` and discards the result, so `xform` is
  never applied in the single-curve case. Pinned as an `if (FALSE)` test.
- `resolution` defaults differ across every entry point (1000, 100, 100, 1000,
  1000, 50). Standardising them changes numbers.

#### Where class-specific guards go

Two `bayesnec` checks have no obvious home under a class-agnostic estimator, and
need one:

- **`dpar`** (hurdle families: choose `mu` vs `hu`/`zi`, invert the zero block).
  This determines *which curves are generated*, so it belongs on the predict
  side: `toxval_predict.bayesnecfit(..., dpar = NULL)`.
- **The gaussian + `type = "absolute"` + no-`bot` guard.** This is a metric-level
  check that depends on a class-level fact. The metric function can only apply it
  if `toxval_pred$meta` carries `family` — which is why `family` is in the meta
  list in [3.4](#34-the-toxval_pred-intermediate-object).

### 3.8 References under model averaging

`ecx` computes its reference **per draw**, from that draw's own control (#19),
so it is a row-wise function of the curve. `nsec` does not: its reference is
`quantile(p_samples[, 1], sig_val)`, a quantile taken *across* draws. That
distinction only matters for a model-averaged fit, where it becomes a real
choice about what the estimate means:

| | reference | reading |
|---|---|---|
| **A** | the `sig_val` quantile of the **model-averaged** control | where the averaged curve departs from the averaged control |
| **B** | the `sig_val` quantile of **each model's** control, then mix | pick a model, compute its NSEC, repeat |

Measured on `manec_example` (`resolution = 50`, `sig_val = 0.01`), isolating the
reference by holding the curves and the sampled draws fixed:

```
A  averaged curve, mixture reference   : 1.47273, 0.995858, 1.53957
B  per-model, per-model reference      : 1.47372, 0.717214, 1.55436
C  per-model, shared mixture reference : 1.47372, 0.905960, 1.55436
```

B against C changes only the reference definition and moves the lower bound by
0.19; A against C is sampling noise (0.09). So the definitional effect is about
twice the Monte Carlo noise, and it lands almost entirely on the lower bound —
the end used to set a protective concentration.

**Decision: A.** The model-averaged NSEC is a property of the model-averaged
curve, so its control should be the model-averaged control. This is the same
fork as "average the CDFs, or average the hazard concentrations" in the
`ssdtools` inversion work, resolved the same way: average the distributions
first, then derive the endpoint.

Consequences:

- `nsec` is computed from the single averaged curve matrix, exactly like `ecx`.
  `bayesnec`'s `nsec.bayesmanecfit()` per-model sampling is not just redundant
  but implements B, and should be deleted rather than ported (#39).
- `ecnsec` follows the same reference, so it stays paired with `nsec`.
- `nec` and `n(s)ec` are unaffected — a NEC is a parameter, so it is inherently
  a B-style mixture over models. `n(s)ec` therefore combines an A-style and a
  B-style quantity by construction, which is a property of the definition rather
  than an inconsistency, but should be stated wherever `n(s)ec` is documented
  (#25).

## 4. Phased implementation

Two constraints drive the order.

**#19 first, on paper.** `ecx.bnecfit` and `ecx.brmsfit` implement *different
estimators* — a single scalar reference versus a per-realisation one. Phase 1
promises that the numbers do not move; #19 determines which ones do. Answering
it afterwards means being unable to distinguish a refactor bug from a definition
change. It does not need code, only a decision, and #20 (direction) should be
decided with it.

**The tibble and the #39 untangle must not land together.** `bayesnec` calls
`ecx()` internally in `plot.R`, `autoplot.R`, `summary.R` and the hurdle methods,
and consumes the result positionally as a length-3 vector
(`bind_ecx()` uses `ecx_vals[[1]]`, `[[2]]`, `[[3]]` and
`attr(ecx_vals, "ecx_val")`). Changing the return type in the same step that
reverses the dependency breaks a CRAN package's plots and summaries.

| # | phase | numbers |
|---|---|---|
| 0 | Decide #19 and #20. No code. | — |
| 1 | **Lock a regression net.** Capture current estimates as golden values, split into "must not move" and "expected to move, because #19/#20/#34/xform". The existing `if (FALSE)` tests and `TODO` markers are the starting ledger for the second list. | — |
| 2 | **Build the spine.** `toxval_pred`, `toxval_predict()` and its methods, the shared `chk` validator, the class-agnostic compute functions, the parametric bootstrap. `ecx()` / `nsec()` keep returning **today's named vectors**. Purely additive. | unchanged |
| 3 | **#39 untangle.** toxval drops the `bnecfit` and `predict` methods; `bayesnec` imports toxval, adds `toxval_predict` methods, deletes its `R/ecx.R` and `R/nsec.R`. | unchanged in `bayesnec`; `ecx` on `bnecfit` adopts the #19 answer in toxval |
| 4 | **Move metrics onto the spine** (`ecx`, then `nsec`, then `nsec_multi`), each gaining `draws`, with `posterior` deprecated but working. | per #19/#20 |
| 5 | **Swap outputs and clean up.** The `toxval` tibble, `tbl_sum()`, `dplyr_reconstruct`. Update tests to the new shapes. Remove `posterior`, the old code and the dead blocks last. | shape changes |
| 6 | **`bayesnec` follow-up.** Update `bind_ecx()`, `plot`, `summary` to the tibble. | — |

Phases 2–3 change *who owns the code*. Phases 5–6 change *what the code
returns*. Keeping them apart means each step has one reason to fail.

## 5. Notes

- **Method gaps to keep in mind:** `ecx` has no `drc` method — under
  [3.4](#34-the-toxval_pred-intermediate-object) it becomes nearly free, so it
  should be added and the plan should say whether that is in scope.
  `nsec_multi` is brms-multivariate only.
- **`n(s)ec` is listed in the `metric` vocabulary but not implemented here.**
  It needs `toxval_pred$threshold` and the shared component index described in
  [3.4](#34-the-toxval_pred-intermediate-object); see #25 and #18. It is flagged now only so the vocabulary does not have to be
  reopened later. `drc` supports both model averaging and threshold models, so
  this is not permanently a Bayesian-only concern.
- **Model-averaged results are not reproducible today** (`bayesnec` #216). The
  component resampling is unseeded, so repeated calls disagree: over 6
  replicates on `manec_example` the `nsec` lower bound spanned 0.907–1.006 in
  `toxval` and 0.702–0.954 in `bayesnec`, against medians stable to ~0.5%.
  `predict()` on a `bayesmanecfit` is affected too, so this is not specific to
  the metrics. Note this is **not** resolved by the plan: moving to a single
  averaged curve matrix fixes alignment, but the one remaining `sample()` in
  `bayesnec`'s averaging is still unseeded and `toxval` never touches it. It has
  to be fixed before model-averaged output can be regression-tested by equality,
  which the #39 relocation depends on.
- **Do not adopt `bayesnec`'s automatic back-transformation.** `toxval` takes
  `xform` as a user-supplied function; `bayesnec` derives it from the parsed
  `crf()` call and drops offsets (`bayesnec` #196). Keep `xform` user-supplied.
- **`xform` applies to realisations before summarising.** This is the current
  behaviour in the `bnecfit` methods and there are tests asserting it. It
  changes answers for any non-linear `xform`, so it belongs in the written
  contract, not just in the code.
