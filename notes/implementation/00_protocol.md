# Autonomous implementation protocol — toxval

How a Claude Code session should work through `01_work_queue.md` without
supervision. Read this first, then `02_decisions.md`, then the queue.

**This repo does not work like `bayesnec`.** It uses a fork workflow and it is
formatted with `air`. Read this section carefully before the first commit.

## Git: fork workflow

```
upstream  open-AIMS/toxval      authoritative. NEVER push here.
origin    beckyfisher/toxval    the fork. Push here.
```

**Work in a worktree, never in the shared clone.** Other sessions use
`/mnt/c/Rworking/toxval`, and a `git checkout` there switches the working tree
out from under them — this has already happened once, and a commit landed on the
wrong branch as a result. Create one worktree at the start of the run and do
every issue in it:

```bash
git -C /mnt/c/Rworking/toxval fetch upstream main
git -C /mnt/c/Rworking/toxval worktree add /mnt/c/Rworking/toxval-<run> main
cd /mnt/c/Rworking/toxval-<run>
```

Remove it with `git worktree remove` when the run is done. This matches how
`bayesnec` already works.

`main` is the integration branch in both repos — upstream has `main`, `refactor`
and `gh-pages`. The local `main` tracks `upstream/main`, so sync it before
branching, from your worktree:

```bash
git fetch upstream
git merge --ff-only upstream/main
git push origin main
```

Then one branch per issue, off `main`:

```bash
git checkout -b issue-<n>-<slug> main
# ... work ...
git push -u origin issue-<n>-<slug>
gh pr create --repo open-AIMS/toxval --base main \
  --head beckyfisher:issue-<n>-<slug>
```

**PRs target `main` on `open-AIMS/toxval`**, from the fork. That is what every
recent merged PR did (#35, #36, #38). **Do not merge** — the author reviews.

Leave the shared clone alone — it is on `main` and other sessions rely on that.

## Formatting

`air.toml` is present, so **all R code must be formatted with `air` before
committing**. This is the opposite of `bayesnec`, where there is no `air.toml`
and formatting is forbidden. If `air` is unavailable, say so in the PR body
rather than committing unformatted code or hand-approximating its style.

## Definition of done, per issue

1. The behaviour described in the issue is fixed.
2. **Many issues in this queue already have a test in `tests/testthat/`** that
   documents the broken behaviour — sometimes asserting the wrong answer
   deliberately, with a comment saying it should fail once fixed. Find it,
   correct it to assert the right behaviour, and say in the PR which test you
   changed and why. Do not delete such a test.
3. New behaviour gets a test of its own, covering the main case and one edge
   case.
4. `devtools::document()` if roxygen changed; commit the regenerated `man/`.
5. **Do not edit `NEWS.md`.** It carries a `fledge` banner
   (`maintained by https://fledge.cynkra.com, contributors should not edit this
   file`), so hand-written entries are overwritten by the next
   `fledge::bump_version()`. `fledge` harvests commit messages instead, so put
   the user-visible summary in the commit subject and body.

   **This differs from `bayesnec`**, whose `NEWS.md` is hand-maintained and
   *does* want an entry per issue. Do not "fix" one repo to match the other.
6. `air` run over any R file touched.
7. `devtools::test()` passes. Push and let R CMD check run; fix failures before
   asking for review.

## Hard constraints

- **No new package dependencies.** Stop the issue and note it instead.
- **Do not edit** `CLAUDE.md`, `.github/`, `air.toml`, or `DESCRIPTION`
  dependency lists.
- **Do not touch `bayesnec`.** It is a separate repository with its own queue.
  If an issue here appears to need a change there, stop and report it.
- Log the session under `prompts/` per `CLAUDE.md` section 6.

## When to stop rather than guess

Stop the issue, write the finding in the PR body (or `notes/blocked_<n>.md` if
there is nothing to push), and move to the next if:

- fixing it requires **deciding what the correct toxicological answer is** —
  most of the open `ecx` issues are of the form "this output makes no sense",
  and several of those are genuinely about *definition*, not code. See #19;
- it requires a **user-visible API change** not sanctioned in `02_decisions.md`;
- it needs a **new dependency**;
- roughly **90 minutes** pass with no progress.

**A clear write-up of why an issue is a definition problem rather than a bug is
a successful outcome.** Several of these were filed as bugs and are not.

## Context you will need

`toxval` owns the toxicity-estimate layer: `ecx()`, `nsec()`, `nsec_multi()`,
and methods for `bnecfit` (bayesnec), `brmsfit` and `drc` fits. It is becoming
a **dependency of `bayesnec`**, which currently carries its own older copies of
`ecx()`/`nsec()`.

Two consequences worth holding in mind:

- Fixes here are the *canonical* ones. `bayesnec`'s copies are being deleted,
  not maintained, so do not port anything back.
- `toxval` presently has `bayesnec` in its `Imports`, which is the wrong
  direction and must reverse. That untangle is **Tier 0 in the queue and is not
  autonomous work** — do not attempt it.

## Verification

```r
devtools::load_all(".")
devtools::test()
testthat::test_file("tests/testthat/test-ecx.R")
```

`tests/testthat/setup.R` builds the fixtures the tests share. Prefer those over
fitting anything new — a fitted `bnecfit` is slow to produce and rarely needed
to demonstrate an estimator fix.
