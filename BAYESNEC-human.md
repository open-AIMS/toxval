# bayesnec support in toxval

`toxval` stopped accepting models fitted with `bayesnec` in phase 3, when its
methods for them were deleted. The work set out here restores that support. It
covers what will be built, why, what a user will see, and the order of the work.

The detail is in `BAYESNEC-claude.md`, which uses the same section numbers: the
function calls, the measurements, the edge cases and the rejected alternatives.
Each decision is stated in full there. Here each is given with a short reason
and a pointer.

`REFACTOR-human.md` is the plan this one amends. It is assumed to have been
read.

## 1. The decision

`toxval` will contain the `toxval_predict()` methods for fits made with
`bayesnec`. `ecx()` and `nsec()` will then accept a `bnec()` fit again, as they
did before phase 3. RF and AP agreed this on 2026-10-23. It is proposed in
#68 and will be recorded as decision T35.

The reason is that a `toxval_pred` needs facts that `toxval` defines and
`bayesnec` has no use for. Examples are the `keys` table that names each curve,
`x_control`, the lowest concentration in the data, and the equation that
produced each realisation of a model-averaged fit. If `bayesnec` wrote these, any
change to the `toxval_pred` contract would need a release of both packages, in
the right order.

The decision replaces phase 7 of `REFACTOR-human.md` and the split recorded on
#39. That split put the methods in `bayesnec`, with `bayesnec` importing
`toxval`. It required `toxval` to be published on CRAN before `bayesnec` could
use it. That requirement no longer applies, because `bayesnec` no longer needs to
import `toxval`. Publication on CRAN (phase 6) therefore no longer has to come
before `bayesnec` support.

The decision does not change the compute functions or the `toxval` tibble. The
`toxval_pred` contract gains two optional metadata elements (section 3.4).
`BAYESNEC-claude.md` §1 lists every passage in the plan documents and on the
issues that the decision overrides.

## 2. Changes for a user

A user can pass a `bnec()` fit to `ecx()`, `nsec()` or `toxval_predict()`, and
gets a `toxval` tibble, as for a `brms` or `drc` fit:

```r
library(bayesnec)
fit <- bnec(y ~ crf(x, model = "nec4param"), data = nec_data)

toxval::ecx(fit, proportion = 0.1)
toxval::nsec(manec_example)
toxval::nsec(manec_example, anchor = "component")
```

The header of the result names the class, for example `# Model: bayesmanecfit`.
The columns are those of `REFACTOR-human.md` §3.1.

`bayesnec` must be installed, because it builds the grid of concentrations the
curve is predicted on (section 3.1). A user with a `bnec()` fit will normally
have it installed.

The estimates are on the scale of the predictor named in `crf()`. For
`crf(log(x), ...)` the result is in units of `x`. `bayesnec::ecx()` currently
reports the same fit in units of `log(x)`, with a message. So `xform = exp` must
not be supplied for a transformation written inside `crf()`. A predictor that
was transformed into its own column before fitting is reported on that column's
scale, as it is now. `bayesnec` decision D41 makes its own functions report on
the recorded scale from version 3.0. `BAYESNEC-claude.md` §2.1 gives the
mechanism.

The numbers will differ from `bayesnec::ecx()` and `bayesnec::nsec()` wherever
the two packages define the estimate differently. Four differences are known:

- the default `nsec()` reference is taken from the model-averaged control
  (`anchor = "model"`, decision T12), while `bayesnec` uses each equation's own
  control;
- a realisation with no crossing is placed by decision T21;
- both directions are reported (decision T10);
- the default `resolution` is 500 (decision T31), against 200 in `bayesnec`.

Under `type = "absolute"`, the decreasing ECx should agree with `bayesnec`
realisation by realisation. Task 5 tests that.

A call to `ecx()` or `nsec()` without a package prefix can reach `bayesnec`'s
function of the same name instead (section 3.6).

## 3. Design decisions

### 3.1 Sources of the grid and the curves

`bayesnec` supplies the prediction grid. `toxval` computes the curves itself
from the `brmsfit` objects stored inside the fit (`BAYESNEC-claude.md` §3.1).

The grid comes from `bayesnec::bnec_newdata()`, which is exported and is in the
CRAN release. It sets the number of trials to 1 for a binomial fit, and an
exposure denominator to 1 for a rate, so that the curve is a proportion or a
rate. Copying those rules into `toxval` would mean keeping two copies in step.

The curves come from `brms::posterior_epred()` called on the `brmsfit` stored in
the object, with group-level effects marginalised. On the `nec4param` equation
of `manec_example`, over a 50-point grid, this gave a matrix identical to
`bayesnec`'s own prediction.

`bayesnec` is added to `Suggests` (section 6). The methods stop with a message
when it is not installed, and their tests are skipped.

### 3.2 Model-averaged fits

For a `bayesmanecfit`, `toxval` builds one model-averaged curve matrix from the
same draws that `bayesnec` uses. It also records which equation produced each
realisation (`BAYESNEC-claude.md` §3.2).

`bayesnec` keeps a number of draws from each equation in proportion to its
model weight. The list of draws kept from each equation is what `bayesnec`
calls the draw index. It is stored on the object. An object saved before
`bayesnec` 2.1.3.7 has none, and `bayesnec` rebuilds it from a fixed seed.
`toxval` uses the stored index, or rebuilds it by the same rule.

This was checked on `manec_example`, with `bayesnec` 2.1.3.39 and a 50-point
grid. The matrix built by `toxval` was identical to
`bayesnec::posterior_epred()`. It held 100 realisations: 83 from `nec4param`
and 17 from `ecx4param`. Realisation *i* is therefore the same draw in both
packages. A `bayesnec` plot and a `toxval` estimate then describe the same
posterior.

Recording the equation of each realisation is what `anchor = "component"`
requires (`REFACTOR-human.md` §3.8). A `bayesmanecfit` is the first class for
which `"component"` and `"model"` give different answers.

Under `"component"` there is one difference from `bayesnec` to document.
`toxval` takes each equation's reference from that equation's draws in the
average, which on `manec_example` is 17 draws for `ecx4param`. `bayesnec` takes
it from all 100 draws of the equation. Both estimate the same quantile of the
equation's control posterior. The `toxval` estimate is based on fewer draws, and
the shortfall is largest for an equation with a small weight.

Task 5 measured the difference at `sig_val = 0.01`. The decreasing NSEC was
1.4741 (0.5900, 1.5508) from `toxval` and 1.4740 (0.7656, 1.5507) from
`bayesnec`. The estimate and the upper limit agree to within 0.0002, and the
lower limit is 0.18 lower in `toxval`. With so few draws, the draw with the lowest control is always
past its equation's reference, so `toxval` places one draw from each equation at
the control concentration, where `bayesnec` places one in all. The lower limit
falls among the lowest four of 100 draws, so that extra draw changes it.

### 3.3 Classes in the first pull request

The first pull request covers `bayesnecfit` and `bayesmanecfit`. These are the
two classes in the CRAN release of `bayesnec`. Every other `bayesnec` class
stops with a message naming the class (`BAYESNEC-claude.md` §3.3).

Two terms are used in the table. A two-part family, such as `hurdle_gamma`,
models the zeros separately from the positive values, so one fit describes two
curves. A `bayesnechurdlefit` holds two separate fits, one for survival and one
for growth among survivors.

| class | on CRAN | in the first PR | what support needs |
|---|---|---|---|
| `bayesnecfit` | yes | yes | — |
| `bayesmanecfit` | yes | yes | — |
| `bayesnecfit` with a two-part family such as `hurdle_gamma` | no | no | an argument choosing which part of the response to report |
| `bayesnechurdlefit` | no | no | an argument choosing growth, survival or their product |
| `bayesnecgroupfit` | no | no | a change to `toxval_pred` (below) |
| `bayesnecjointfit` | no | no | a change to `toxval_pred` (below) |

A `bayesnecgroupfit` is a separate fit for each level of a factor, such as one
fit per site. Each level is predicted over its own concentrations. A
`bayesnecjointfit` is one fit across levels, and since `bayesnec` decision D40
each level is also read over its own concentrations. A `toxval_pred` has one
grid and one control concentration for every curve. Supporting these classes
therefore needs a decision on whether a `toxval_pred` can hold a grid per curve.

The four classes not in the first pull request exist only on `bayesnec`'s
development branches, and `bayesnecjointfit` changed this month. Each will get
its own pull request once it is released.

### 3.4 Additions to `toxval_pred`

`meta` gains two optional elements, and no new slot is added
(`BAYESNEC-claude.md` §3.4).

- `meta$draw_model` names the equation that produced each realisation.
- `meta$model_weights` gives the model weight of each equation in the set.

The second makes the equations behind a model-averaged estimate recoverable from
the result, as `REFACTOR-claude.md` §3.10 requires. Both are checked when
present. `toxval_pred` is public interface, so both are documented with the rest
of the metadata.

Two existing slots are filled from the fit:

- The `asymptote` slot, which phase 4 adds (decision T24), takes the `bot`
  parameter for the nine equations that have one. It takes 0 for an equation
  without `bot`, whose curve tends to zero. It is `NA` for a `gaussian` fit of
  such an equation, since that family has no lower bound.
- The `threshold` slot stays empty. The NEC, the `nec` parameter, exists only
  in the ten equations whose names begin `nec`. In a model-averaged set it is
  therefore missing for some realisations. Deciding what that means is the
  question #25 is open for.

A binomial or beta-binomial `bnec()` fit is predicted with one trial. Its curve
is therefore a proportion, bounded at 1, and the increasing `absolute`
reference uses that bound (decision T18).

### 3.5 bayesnec behaviour not reproduced

Four behaviours of `bayesnec` are not reproduced (`BAYESNEC-claude.md` §3.5).

- The back-transformation of a predictor transformed inside `crf()` (decision
  T6, section 2).
- The refusal of an absolute ECx for a `gaussian` fit of an equation without
  `bot`. `bayesnec` has itself removed it, following OECD TG 201. The guard
  listed in `REFACTOR-claude.md` §3.7 is deleted.
- The computation of each metric per equation followed by mixing of the
  results. `toxval` computes every metric on the one model-averaged matrix
  (section 3.2).
- The decline from the peak, `hormesis_def = "max"`, which defines a different
  reference (decision T28).

### 3.6 The shared function names

This work documents the name conflict between the two packages and changes
nothing in `bayesnec` (`BAYESNEC-claude.md` §3.6).

`bayesnec` exports `ecx()` and `nsec()`, with methods for its own classes and
copies of `nsec()` for `brmsfit` and `drc` fits. When `bayesnec` is attached
after `toxval`, `ecx()` on a `brmsfit` stops with "no applicable method". With
the other attach order, `ecx()` on a `bnec()` fit runs `toxval`'s definition.
Neither order gives a warning.

What `bayesnec` does about its own `ecx()` and `nsec()` is for `bayesnec` to
decide. Its plan currently relocates them to `toxval` and imports `toxval`
(`bayesnec` decision D10 and issue #255). That plan is no longer needed for
`toxval` to support `bayesnec` fits.

## 4. Order of work

There are six tasks, and all six are done. Task 0 was done last, after PR #66
merged. `BAYESNEC-claude.md` §4 gives files, tests and stopping points for each.

0. Task 0 records the decision. It adds T35 to
   `notes/implementation/02_decisions.md`, rewrites phases 6 and 7 of the
   refactor plan, and drafts a decision comment for #39. It is needed so that
   the plan of record and the issue agree with this document. It waits for
   PR #66 to merge, because #66 edits the same refactor documents and adds T34.
   It is done when no passage listed in `BAYESNEC-claude.md` §1 still places the
   methods in `bayesnec`.
1. Task 1 stores the test fits. It adds `manec_example`, a binomial fit and a
   fit with `crf(log(x))` to `tests/testthat/fixtures/`, with the code that made
   them in `data-raw/`. The tests need a model-averaged fit, a binomial response
   predicted as a proportion, and a predictor transformed inside `crf()`, and no
   stored fit has these. Their size is not limited during development. It is
   done when the fixtures load and `data-raw/DATASET.R` records the `bayesnec`
   version.
2. Task 2 adds the single-fit method, `toxval_predict.bayesnecfit()`, with the
   check that `bayesnec` is installed and the refusal of the other classes. It
   is done when its curves are identical to `bayesnec`'s on every fixture and
   each refusal is tested.
3. Task 3 adds the model-averaged method, `toxval_predict.bayesmanecfit()`, and
   the two `meta` elements. It is done when its matrix is identical to
   `bayesnec::posterior_epred()` on `manec_example` and the call leaves the
   caller's random number state unchanged.
4. Task 4 fills the `asymptote` slot and makes `anchor = "component"` read
   `meta$draw_model`. It is done when `type = "relative"` and both anchors are
   tested on `manec_example`.
5. Task 5 tests `ecx()` and `nsec()` on every fixture and compares them with
   `bayesnec` in `tests/local/`. It also updates the examples, the README and
   `DESCRIPTION`. It waits for task 4. It is done when R CMD check passes with
   and without `bayesnec` installed.

The four classes of section 3.3 follow in later pull requests.

## 5. Branch

The work is on `phase-7-bayesnec-methods`. PR #66 has merged into `phase-2`.
The branch is built on PR #78, which starts every grid at the control
concentration (#73) and has merged into `phase-2`. The pull request goes into
`phase-2`, and is merged before #77, the documentation review
(`BAYESNEC-claude.md` §5).

## 6. Decisions and points for review

RF decided three of these on 2026-10-23.

- `bayesnec` is added to `Suggests` (section 3.1).
- The difference from `bayesnec` under `anchor = "component"` is accepted
  (section 3.2). Its measured size, 0.18 in the lower limit on `manec_example`,
  is for RF to confirm, since `BAYESNEC-claude.md` §3.2 keeps the rejected
  alternative open for a large difference.
- Fixture size does not limit this work, because the tests are needed during
  development and CRAN submission is not near. When the package is prepared for
  CRAN, a fit needed only by the tests in `tests/local/` can be kept out of the
  built package, as `bayesnec` does (`BAYESNEC-claude.md` §4, task 1).

The rest are proposed rather than decided, and each is argued in the section
named.

- Using `bayesnec`'s own draw index for a model-averaged fit (section 3.2).
- Limiting the first pull request to two classes (section 3.3).
- Leaving `threshold` empty until #25 (section 3.4).
- Reporting a predictor transformed inside `crf()` on its recorded scale
  (section 2).
