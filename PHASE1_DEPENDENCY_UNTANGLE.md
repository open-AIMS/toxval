# Phase 1 — the bayesnec/toxval dependency untangle

**Point a Claude Code session at this file.** It is the executable form of
**toxval Tier 0** in `notes/implementation/01_work_queue.md`, which states the
shape of the fix but not the steps. Read that file and
`notes/implementation/02_decisions.md` first for the reasoning; this file is the
plan.

> **Superseded in part, 2026-08-19.** `REFACTOR-claude.md` §4 is now the
> authoritative phase order, and it differs from [§4 below](#4-execution-plan):
> `toxval` must reach **CRAN** before `bayesnec` can import it, so the tibble
> change comes *before* submission and `bayesnec` relocates and adapts in a
> single step. See toxval #39 and §4.0 here. The architecture in §3 still
> stands; the ordering in §4 does not.

> **"Tier" is ambiguous across the two repos.** Both `toxval` and `bayesnec`
> have a `notes/implementation/01_work_queue.md` with a Tier 1 and a Tier 2.
> They are different queues. **bayesnec Tier 1 is complete** (PRs #197–#208,
> merged 14–17 Aug 2026); **toxval Tier 1 has not started.** Every reference
> below is qualified.

> **Attended work.** This spans two repositories with a hard ordering constraint
> (see [Ordering](#ordering-is-a-hard-constraint)). Do not run it unattended, and
> do not start Tier 1 in either repo until it lands.

**Status verified 2026-08-19.** `toxval` main at `aa1e158`, **not on CRAN**;
`bayesnec` dev at `fd5cf5e7`, on CRAN at 2.1.3.1, **bayesnec Tier 1 complete**
(all eight items merged). `toxval` Tier 1 has not started — no toxval issue is
closed.

---

## 1. The problem

`toxval` has `bayesnec` in `Imports`, so the dependency runs backwards from where
it is going. Both packages register the same S3 methods, and both export the same
generics. **This collision is live today** — it is not something the migration
introduces.

Registered in **both** packages:

| | bayesnec | toxval |
|---|---|---|
| `predict.bayesnecfit` | ✓ | ✓ |
| `predict.bayesmanecfit` | ✓ | ✓ |
| `nsec.brmsfit` | ✓ | ✓ |
| `nsec.drc` | ✓ | ✓ |
| `ecx` generic | exported | exported |
| `nsec` generic | exported | exported |

Plus a dispatch hazard that is not an exact duplicate: `toxval` registers
`ecx.bnecfit` / `nsec.bnecfit` while `bayesnec` registers `ecx.bayesnecfit`,
`ecx.bayesmanecfit`, `nsec.bayesnecfit`, `nsec.bayesmanecfit`. `bnecfit` is the
parent class, so which implementation runs depends on class order and on which
package attached last.

`bayesnec` additionally owns `ecnsec` and the whole `bayesnechurdlefit` family,
which `toxval` does not have at all.

## 2. The surface is much smaller than it looks

`toxval`'s actual *code* dependency on `bayesnec` is four things, and three of
them sit in a single helper:

| where | what |
|---|---|
| `R/helpers.R:28` | `bayesnec::pull_out()` |
| `R/helpers.R:30` | `model.frame(object$bayesnecformula, ...)` — needs `bayesnec`'s `model.frame` method for `bayesnecformula` |
| `R/helpers.R:32` | `bayesnec::bnec_newdata()` |
| `R/predict.R:42` | `bayesnec::pull_brmsfit()` |

All three in `helpers.R` are inside `newdata_eval()`, and all three are in the
branch that handles `bayesnecfit` / `bayesmanecfit` objects. Everything else that
mentions `bayesnec` is a roxygen link or an example.

**Consequence: the untangle is mostly deletion from `toxval`.** Once `toxval`
stops accepting `bnecfit` objects, it needs nothing from `bayesnec` at all.

## 3. Target architecture

**`toxval` knows nothing about `bayesnec` classes.**

- Owns the `ecx` / `nsec` generics, the estimator machinery (`ecx_x_*`,
  `nsec_fct`, `modify_posterior`, `nsec_multi`), and the `brmsfit` and `drc`
  methods.
- Exposes the estimator on **plain inputs** — a posterior matrix and an `x_vec` —
  so any caller can drive it without a fitted object. This is what makes
  toxval #21 (`glm` and other classes) tractable later.
- Drops `ecx.bnecfit`, `nsec.bnecfit`, `predict.bayesnecfit`,
  `predict.bayesmanecfit` and the `bnecfit` branch of `newdata_eval()`.
- `bayesnec` moves from `Imports` to `Suggests` (still needed by examples and
  tests that build fixtures from real `bnec()` fits).

**`bayesnec` owns everything that knows about its own classes.**

- `Imports: toxval`, and re-exports the `ecx` / `nsec` generics so
  `library(bayesnec)` alone still works for existing users.
- Keeps `ecnsec` and every `bayesnecfit` / `bayesmanecfit` / `bayesnechurdlefit`
  method. Each unwraps to a `brmsfit` plus a grid and calls into `toxval`'s
  plain-input estimator.
- Keeps its own `predict` methods and its own grid construction. **Note:** as of
  `bayesnec` #205 there is a single internal `prediction_grid()` that
  `bnec_newdata()`, `expand_nec()` and `posterior_on_grid()` all delegate to —
  build the unwrapping on that, not on a fourth copy.
- Deletes `R/ecx.R` and `R/nsec.R`, keeping only the class-aware wrappers.

Every method then lives in exactly one package, which is what closes the
collision.

## 4. Execution plan

### Ordering is a hard constraint

`bayesnec` **cannot** declare `Imports: toxval` until a `toxval` carrying the new
API is installable, or `bayesnec`'s R CMD check fails on a function that does not
exist yet. The interim needs a `Remotes:` entry. The steps below are in the only
order that works.

### Step 0 — file the tracking issue

No issue exists for this in either repo. File one in `open-AIMS/toxval`, link it
from `open-AIMS/bayesnec`, and reference it from both PRs.

### Step 1 — `toxval`: add the plain-input API (additive only)

Add the estimator entry points that take a posterior matrix and `x_vec` and
return the estimate. Change no existing behaviour and remove nothing. Existing
methods become thin callers of the new entry points, so the suite proves the
refactor is behaviour-preserving before anything is deleted.

**This is `toxval_pred`.** `REFACTOR-claude.md` §3.3/§3.4 specifies exactly this
object — realisations, an x grid and metadata — as the seam the metric functions
compute on. Build it once, there, rather than prototyping a separate plain-input
API here and replacing it later. It also makes `toxval_pred` **public API**,
since `bayesnec` constructs one from outside the package.

**Done when** the new functions are exported and tested, the full suite passes
unchanged, and R CMD check is clean.

### Step 2 — `toxval`: shed the bayesnec dependency

Delete `ecx.bnecfit`, `nsec.bnecfit`, `predict.bayesnecfit`,
`predict.bayesmanecfit` and the `bayesmanecfit` branch of `newdata_eval()`. Move
`bayesnec` from `Imports` to `Suggests`. Guard any example or test that uses a
`bnec()` fit behind `requireNamespace("bayesnec")`.

**Done when** `grep -rn "bayesnec::" R/` returns nothing, `NAMESPACE` has no
`bayesnecfit` / `bayesmanecfit` / `bnecfit` method, and R CMD check is clean with
`bayesnec` not installed.

**This is *a* breaking release, but not the last one.** The output-shape change
(the `toxval` tibble) is also breaking and now lands *before* CRAN submission —
see the note at the top of this file. Bump the version, write the NEWS entry
naming `bayesnec` as the new home of the `bnecfit` methods, and tag it, but do
not treat this as the API `bayesnec` will consume.

### Step 0b — release `bayesnec`'s current backlog first  *(added 2026-08-20)*

**Ship what is already on `bayesnec` `dev` to CRAN before the untangle lands
there.** CRAN has 2.1.3.1; `dev` is at 2.1.3.7 and carries the `brms >= 2.23.0`
requirement, the `disp()` dispersion sub-model and variance functions, and the
whole of bayesnec Tier 1 — zero-inflated families, the `cens()` aterm, hurdle
models, `get_priors()`, failed-model reporting. If `Imports: toxval` lands on
`dev` first, none of that can be released until `toxval` is published on CRAN,
which is a long way off (Step 2b, toxval #45).

**And keep the untangle on a branch until `toxval` is on CRAN**, so `dev` stays
submittable. CRAN can require a submission at short notice — a new R version, a
dependency change, a policy tightening, typically with a two-week deadline. A
`dev` that imports a package CRAN cannot see is not submittable, and reverting
the untangle under a deadline is the worst time to do it.

### Step 2b — `toxval` to CRAN  *(added 2026-08-19)*

**`bayesnec` cannot release `Imports: toxval` until `toxval` is on CRAN.** A CRAN
package's `Imports` must resolve from CRAN, and `Remotes:` is a
remotes/devtools field that CRAN **ignores**. The `Remotes:` interim in Step 3
works for GitHub installs only.

`bayesnec` is on CRAN (2.1.3.1). `toxval` is not on CRAN at all. So CRAN
readiness for `toxval` is a prerequisite of this whole exercise, and it is not
small. **toxval #45** tracks scoping it.

Note the constraint binds only at *submission*: `dev` can carry
`Imports: toxval` plus `Remotes:` indefinitely for GitHub installs. What it
cannot do is go to CRAN in that state — see Step 0b. It also means the API `toxval` publishes should be the *final* one — see
the note at the top of this file.

### Step 3 — `bayesnec`: consume toxval

On a branch off `dev`: add `Imports: toxval`, re-export the generics, rewrite the
class methods to unwrap and delegate, delete `R/ecx.R` and `R/nsec.R`, **and
adapt `bind_ecx()` / `plot` / `summary` to the `toxval` tibble** (they currently
consume `ecx()` positionally as a length-3 vector).

`Remotes: open-AIMS/toxval` is fine for testing this branch before `toxval` is
published, but it cannot be part of a CRAN submission.

**Done when** every `ecx()` / `nsec()` / `ecnsec()` test in `bayesnec` passes
against the delegated implementation, with no change in returned values.

### Step 4 — land and drop `Remotes:`

Publish `toxval` on CRAN first, then merge the `bayesnec` PR. Remove any
`Remotes:` entry before submitting `bayesnec`.

### Step 5 — reconcile the duplicated issues

- Close `bayesnec` #166 as a duplicate of toxval #29.
- File toxval issues for `bayesnec` #39 and #44, which have no toxval equivalent.
- `bayesnec` #195 is substantially better here already — `toxval` threads
  `hormesis_def` through `ecx_x_*` with a live `modify_posterior()`, where
  `bayesnec`'s equivalent is commented out.
- `bayesnec` #196 **does not apply here**: `toxval` takes `xform` as a
  user-supplied function rather than deriving it from the formula, so the failure
  mode is designed out. Do not adopt `bayesnec`'s approach during this work.

## 5. Verification

Run in both repos before each merge:

```r
devtools::document(); devtools::test(); devtools::check()
```

Then the collision check that motivates the whole exercise:

```r
library(bayesnec); library(toxval)   # must produce no masking warnings
conflicts(detail = TRUE)$`package:toxval`
```

`toxval` has an `air.toml`, so format R code with `air` before committing.

## 6. Hazards

- **Do not restructure dispatch during toxval Tier 1 #32** (the R CMD check
  generic/method consistency warning). Align signatures only; those methods are
  moving.
- **toxval Tier 1 and the refactor collide.** See
  `notes/implementation/01_work_queue.md`, "Sequencing against the refactor".
- **Reference semantics.** `bayesnec` #39 notes that root-finding forces the
  `type` reference semantics to be pinned down. Do not change any estimator
  semantics during Phase 1 — this is a relocation, not a bug fix. The estimates
  must be identical before and after.
- **Test fixtures.** `toxval` builds fixtures from real `bnec()` fits. Once
  `bayesnec` is only in `Suggests`, those must be pre-built and stored, not
  fitted at check time.

## 7. Loose ends found on 2026-08-18

- **`origin/becky_tests_ecx`** (fork only) has six commits of `ecx` tests —
  `type = relative/absolute/direct`, `hormesis_def = "max"`, poisson — but an
  **unrelated history** ("Initial commit from ZIP download"), so it cannot be
  merged. It looks superseded: upstream `tests/testthat/test-ecx.R` is 1189 lines
  against the branch's 910, with more `hormesis_def` coverage. Spot-check before
  deleting; the tests bear directly on toxval #1/#8/#12/#14.
- **`notes/implementation/` was untracked**, so invisible to a fresh clone, to
  CI and to collaborators. Resolved 2026-08-19: `01_work_queue.md`,
  `02_decisions.md` and this file are now committed; `00_protocol.md` (session
  operating instructions) and `04_alignment_audit.md` (a working document) stay
  local.
- Every other local and remote branch is merged into `upstream/main`. There is no
  other unmerged work.
