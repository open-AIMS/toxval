# The backlog run into `predev`

Written 2026-11-27. This file sets which open issues of `open-AIMS/toxval` are
worked on from that date, in what order, and how each fix is merged into
`main`. That work is called the run below. The rulings it relies on are T43 to
T46 in `02_decisions.md`. `01_work_queue.md`, the queue of tiers written before
the refactor, is kept as a record and no longer sets the order of work.

## Summary

The run works on 21 of the 29 issues open on 2026-11-27. #101 is closed,
because it records a comparison that needed no change in `toxval`, and seven
issues are left for later. None of the open issues is a phase of the refactor
set out in `REFACTOR-human.md`: phases 2 to 5 and 7 merged in #59 as toxval
2.0.0, #97 added the four case studies as pkgdown articles, and only phase 6,
the CRAN release, remains.

## Procedure for each item

Each fix is merged first into `predev`, a branch of `open-AIMS/toxval` that
collects the fixes until RF has reviewed them together. Each item is fixed on
its own branch, `issue-<n>-<slug>`, cut from the tip of `predev`. A Codex
session implements the fix and its tests in a separate checkout of the
repository (a git worktree). A second session, which has not seen the
instructions given to the first, then reviews the changes, and its findings are
addressed before the pull request opens. The pull request runs from
`beckyfisher/toxval` into `predev`, and R CMD check and the pkgdown build run on
it. It is merged once RF has approved it, unless RF waives that approval for the
item, and the item is then done.

An issue is closed when the pull request that fixes it merges into `predev`,
with a comment naming the pull request (RF, 2026-11-27). GitHub closes an issue
automatically only on a merge into `main`, so this is done by hand. The issues
of the run are in the milestone "Backlog run into predev". An open issue there
is still to be done, and a closed one is fixed in `predev` and awaits RF's full
review of `predev` before `predev` is merged into `main`. The pull request that
merges `predev` into `main` lists them.

An item is stopped, with the reason recorded on its issue or pull request, where
the fix needs a ruling this file does not give, a dependency RF has not
approved, or a change to `bayesnec`.

## Agreement with `bayesnec`

`toxval` on `predev` is compared with `bayesnec` on its own `predev` branch,
not with its CRAN release (T46). On 2026-11-27 that is `bayesnec` 3.0.0 at
`f633e89b`. The comparison is required only for a change to a value that
`ecx()` or `nsec()` returns, as `CLAUDE.md` §2 sets out, and its result is
recorded on a `bayesnec` issue. Items 16 to 18 match `bayesnec` pull requests
that have not yet been merged into `bayesnec` `predev`, and they wait until
those are merged.

## The queue

The items are done one after another, in the order below, because most of them
edit `R/ecx.R`, `R/nsec.R`, `R/summarise.R` or `R/toxval-predict.R`. Two run at
once only where they edit different parts of the code. For example, item 1
edits the warning in `R/ecx.R` and item 2 a check in `R/nsec.R`. The "After"
column names the items that must be merged first.

| Item | Issue | What changes for a user | After |
|---|---|---|---|
| 0 | none | `predev` receives #88 and #89, which merged into `phase-2` after `phase-2` had merged into `main`; this file and T43 to T46 are added | |
| 1 | #93 | `type = "relative"` warns of its change of meaning once per session, not on every call (T43) | |
| 2 | #72 | `nsec()` accepts a `drc` NEC fit such as `NEC.3()` and finds its NSEC on the curve, as it does for a `bnec()` NEC fit | |
| 3 | #90 | `ecx(fit, type = c("absolute", "relative"))` returns the rows of both types, computed from one set of realisations | 1 |
| 4 | #94 | each result gains `censored` and `excluded` columns, and a declining curve under `relative` no longer warns about its increasing direction (T44) | |
| 5 | #99 | a `bnec()` fit that recorded `x_control` when it was fitted uses that concentration as its default control | |
| 6 | #100 | `ecx()` and `nsec()` take `extrapolate`, which reads an estimate beyond the tested range without changing the grid inside it, as in `bayesnec` | 5 |
| 7 | #96 | `pool` combines the groups of a grouped fit into one estimate, by one of three methods | 4 |
| 8 | #98, #80 | a group-level term such as `(1 \| plate)`, on plates tested across the whole concentration series, is read for each plate against that plate's own control, and the plates are pooled; a group with no data at a shared control concentration warns | 5, 7 |
| 9 | #91 | a new function gives the ratio and the difference of an ECx between two curves of a grouped fit, as `drc::EDcomp()` does | 4 |
| 10 | #70 | `ecx()` and `nsec()` accept a `glm()` fit; `MASS` and `mgcv` join Suggests for its tests | |
| 11 | #71 | the glm case study, which reproduces an NSEC from Noonan et al. (2025), is added as a pkgdown article | 10 |
| 12 | #95 | `plot()` and `autoplot()` draw a fitted curve with its estimates marked (T45), and the four case studies use them | |
| 13 | #92 | a set of `drc` fits of different equations to the same data gives a model-averaged ECx and NSEC, as `drc::maED()` gives the ECx | |
| 14 | #41 | `exceedance()` gives the probability that an estimate exceeds a threshold concentration, as in `bayesnec` | |
| 15 | #79 | a vignette sets out how another package adds a `toxval_predict()` method | |
| 16 | #82, #102 | the local comparison test calls `bayesnec::ecx()` with `x_scale = "fitted"` for the estimate on the fitted scale | `bayesnec` #602 |
| 17 | #103 | `ecnsec.draw`, the effect at an NSEC realisation, is 0 rather than negative where that realisation's control is at or below the reference | `bayesnec` #605 |
| 18 | #104 | the ECx and NSEC of a `bnec()` fit to `crf(log(x), ...)` are searched for on a grid evenly spaced on the log scale, as in `bayesnec` | `bayesnec` #604 |
| 19 | #67 | comments and help pages stop citing the refactor documents and decision numbers | all others |

Item 4 is done before items 7 and 9, because their results include the censored
share of the realisations, which item 4 adds to the results of `ecx()` and
`nsec()`.

Under item 8, each group of a grouped fit is given its own grid and control
concentration, which T41 left to be decided separately. With the default,
`x_control = "own"`, each group's control is read at that group's lowest
concentration, so no group is predicted at a control concentration at which it
was not tested. The warning #80 asks for is still needed where the groups share
one control concentration, under `x_control = "shared"` or a number, and it is
therefore part of the same item.

One question is to be settled by RF before item 14 starts. The realisations of
a `drc` fit are bootstrap replicates, not posterior draws, so the share of them
exceeding a threshold is not a posterior probability. The options for what
`exceedance()` returns for such a fit are listed on #41.

Under item 19, RF also decides whether `REFACTOR-human.md`,
`REFACTOR-claude.md`, `BAYESNEC-human.md`, `BAYESNEC-claude.md` and
`PHASE1_DEPENDENCY_UNTANGLE.md` are removed, as Ayla Pearson asked on #67 for
the end of the refactor.

## Issues not in this run

- #40 asks for the grid to be replaced by root-finding. Items 6 and 18 extend
  the grid, so #40 is left until they have been merged.
- For #25 (a model-averaged N(S)EC), #17 (the NOEC) and #63 (further ECx
  definitions), a definition is to be agreed before code is written.
- #21 stays open for the model classes that follow glm, such as `gam` and
  `glmer`.
- #22 asks whether toxval should fit models, which is not planned at this stage
  (RF, 2026-10-19).
- #45 was written on the assumption that `bayesnec` would import `toxval`,
  which T35 ruled out. It is to be rescoped as the CRAN release of phase 6.

## Corrections to `CLAUDE.md` for RF

A session does not edit `CLAUDE.md` unless RF asks. Three statements in its
section 2 are contradicted by merged work:

- it lists `nsec_multi()`, which T37 made defunct;
- it states that `drc` fits are supported by `nsec()` only, although `ecx()`
  has accepted them since phase 4 (T27);
- it describes phase 4 as still to come.
