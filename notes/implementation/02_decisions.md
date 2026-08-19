# Decisions — toxval

Settled 2026-08-14; amended 2026-08-19 after the review of PR #42. A session
implements these rather than re-opening them.

**"Tier" is repo-specific.** `bayesnec` has its own
`notes/implementation/01_work_queue.md` with its own Tier 1 and Tier 2.
**bayesnec Tier 1 is complete**; **toxval Tier 1 has not started.** References
below mean *toxval* tiers unless stated.

---

## T1 — Branching and review

Branch per issue off `dev` (fork-only integration branch), push to `origin`
(`beckyfisher/toxval`), PR into `open-AIMS/toxval` **`main`**. Never push to
`upstream`. Sessions do not merge.

## T2 — Formatting

`air.toml` is present: format every R file touched with `air` before committing.
This differs from `bayesnec`, which has no `air.toml` and where formatting is
forbidden.

## T3 — Guards error, they do not coerce

Where an argument combination is invalid — two `type` values (#7),
`group_var == x_var` (#15) — **error naming the arguments**. Do not silently use
the first value with a warning. A toxicity estimate produced from a silently
altered request is the kind of number that ends up in a report.

Distinguish this from **#6 and #24**, which are about a *valid* estimate landing
at the edge of the tested range. Those **warn** and return the estimate.

## T4 — Fixes here are canonical, but `bayesnec`'s code is still evidence

`bayesnec` is deleting its own `ecx()`/`nsec()`, not maintaining them. **Do not
port fixes back.**

**Amended 2026-08-19.** This decision originally also said "do not consult
`bayesnec`'s copies as a reference". That is too strong and following it would
have produced the wrong answer on #19. Measured across the full fixture suite,
`toxval::ecx.bnecfit` is the **outlier**: it builds one scalar reference from
medians, while `toxval::ecx.brmsfit` and `bayesnec::ecx.bayesnecfit` both compute
the reference per draw. Two of the three agree and the odd one out is ours.

The narrow claim still holds exactly: `bayesnec` is worse in one specific
respect, the automatic `crf()` back-transformation, and that must not be adopted
(T6). Everywhere else, treat its implementation as a comparison point — the
scripts in `notes/pr42/` exist to make that comparison cheap.

## T5 — Tier 0 is attended

The dependency untangle is not autonomous work. It spans two repositories, the
PRs must land in order, and `bayesnec` needs a `Remotes:` entry in the interim.
Do not attempt it, and do not modify `DESCRIPTION` dependencies.

## T6 — `xform` stays user-supplied

`bayesnec` derives the back-transformation for an inline `crf(log(x + 1))` by
substituting into the parsed call, which silently drops the `+ 1` and returns
`ecx` on a different scale from `nec` (`bayesnec` #196, measured at about 3.3%
error). `toxval` avoids this by taking `xform` as a function from the user.

**Keep it that way.** During Tier 0 there will be a temptation to adopt
`bayesnec`'s automatic back-transformation for convenience. Do not.

## T7 — #19 is the keystone

Several issues filed as bugs are really about the *definition* of "effect":
#1, #8, #12 and #14. Resolving #19 resolves the ambiguity behind them, and doing
them piecemeal first would mean fixing the same thing four times, differently.

**#19 is not in the unattended queue** and neither are its dependants. If a
toxval Tier 1 issue turns out to depend on the definition, stop and say so —
that is a useful finding, not a failure.

**Amended 2026-08-19.** #19 has since split into two halves that behave
differently:

- The **`ecx`** half — per-draw versus scalar reference — is a single decision
  and **still blocks** the refactor, because the regression net cannot be locked
  until it is answered.
- The **`nsec`** half — which control the reference comes from under model
  averaging — is **no longer a blocking decision**. `REFACTOR-human.md` §3.8
  exposes it as `anchor = c("model", "component", "control")`, so only the
  *default* has to be agreed, and a default can be revisited.

**#1 and #8** (`hormesis_def = "max"`) are largely absorbed by the direction
decision (#20): `direction` becomes a property of the result and replaces
`hormesis_def` rather than fixing it. See `REFACTOR-human.md` §3.6.

---

## Still to decide

Listed so they are not mistaken for oversights.

- **#3** — `ecx_val` as a proportion rather than a percentage. Breaking; needs a
  deprecation path. Interacts with #9, and now with the closed `metric`
  vocabulary and its separate `ecx_val` column (`REFACTOR-human.md` §3.1).
- ~~**#4** — always return a tibble.~~ **Decided 2026-08-19**: yes, a `toxval`
  tibble subclass. See `REFACTOR-human.md` §3.1 and PR #42 / #44.
- **#22** — whether `toxval` takes on model-fitting wrappers at all. Scope
  question for the author; it changes what the package *is*. **Now has a forcing
  case**: `anchor = "control"` (`REFACTOR-human.md` §3.8) needs a control-only
  fit, which cannot be recovered from an already-fitted object. It needs only
  `brms`, so it does not threaten the untangle.
- **#17, #18, #25** — NOEC, model-averaged N(S)EC, N(S)EC. Each needs its
  definition agreed before any code.
- **bayesnec #39** — root-finding in place of the grid, now **toxval #40**. The
  spike referenced elsewhere as `03_uniroot_spike.md` was never written; the
  code is `bayesnec/ignore/uniroot.R`. The numerical part is tractable, but it
  forces the `type` reference semantics to be made explicit, which is #19 again.

## Issues — status 2026-08-19

- ~~The **Tier 0 untangle** has no issue.~~ Filed: **#39**.
- ~~**bayesnec #39** and **#44** have no counterpart here.~~ Filed: **#40**
  (root-finding) and **#41** (hypothesis method).
- When #29 is fixed, close **bayesnec #166** as a duplicate of it. *(still to
  do)*
- Filed since: **#43** — `nsec.drc` intervals invert a pointwise confidence band;
  **#45** — scope CRAN readiness, which blocks #39; and **bayesnec #216** —
  model-averaged output is not reproducible.
