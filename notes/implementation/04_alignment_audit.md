# Alignment audit — 2026-08-19

Written after the review of `open-AIMS/toxval` PR #42. Checks the planning
documents in **both** repos against each other and against what has been decided
on GitHub, and lists where they now disagree.

Nothing here is a new decision. It is a list of drift.

---

## 1. Inventory

### `toxval`

| where | what | tracked? |
|---|---|---|
| `REFACTOR-human.md` | the architecture. Ayla's, rewritten in PR #44 from the #42 review | yes (PR #44) |
| `notes/pr42/README.md` + 3 `.R` | measurements behind the review claims | yes (PR #44) |
| `notes/pr42/draft-github-posts.md` | what was posted where | **no** — deliberate, content is live on GitHub |
| `prompts/review-pr42-refactor-plan.md` | session log | yes (PR #44) |
| `notes/implementation/00_protocol.md` | how an unattended session works here — fork flow, `air`, definition of done | **no** |
| `notes/implementation/01_work_queue.md` | Tier 0/1/2 issue queue | **no** |
| `notes/implementation/02_decisions.md` | T1–T7 settled decisions, plus "still to decide" | **no** |
| `notes/implementation/03_uniroot_spike.md` | **referenced twice, does not exist** | — |
| `PHASE1_DEPENDENCY_UNTANGLE.md` | the untangle plan, written 2026-08-18 | **no** |

### `bayesnec`

| where | what |
|---|---|
| `notes/implementation/00_protocol.md` | scope boundaries — `ecx`/`nsec`/`ecnsec`/`zero_crossings` out of scope, migrating |
| `notes/implementation/01_work_queue.md` | Tier 1 (run now) / Tier 2 (deferred until the untangle lands) |
| `notes/implementation/02_deferred.md` | out of scope, incl. the five migrating issues |
| `notes/implementation/03_decisions.md` | D5–D10, incl. D8 ordering and D10 sequencing |
| `ignore/uniroot.R` | probably the spike the toxval notes point at — in a gitignored folder |

### GitHub, as of this audit

| | |
|---|---|
| toxval #19 | two comments: reference semantics, then the mechanism + the `anchor` argument |
| toxval #20 | direction as a property of the result |
| toxval #25 | `n(s)ec` vocabulary, `threshold` slot, shared component index |
| toxval #39 | two comments: the split and ordering, then CRAN prerequisite + reorder + correction |
| toxval #43 | **new** — `nsec.drc` intervals invert a confidence band |
| toxval PR #42 | the review |
| toxval PR #44 | the revised plan |
| bayesnec #216 | **new** — model-averaged output is not reproducible |

---

## 2. Conflicts to resolve

### 2.1 `Remotes:` will not work — both repos assume it will

`PHASE1` steps 3–4 and `bayesnec` D8 both plan for `bayesnec` to declare
`Imports: toxval` with a `Remotes: open-AIMS/toxval` entry in the interim.

`bayesnec` is on CRAN (2.1.3.1); `toxval` is not on CRAN at all. A CRAN
package's `Imports` must resolve from CRAN, and `Remotes:` is a
remotes/devtools field that CRAN ignores. So that interim works for GitHub
installs and **cannot be released**.

**Consequence:** publishing `toxval` on CRAN is a prerequisite of the untangle,
not a follow-up. Recorded in `REFACTOR-human.md` §4 and on toxval #39; **not yet
in `PHASE1` or in the `bayesnec` notes.**

### 2.2 Two different orderings are now written down

`PHASE1` (2026-08-18) predates Ayla's PR and the tibble decision. Its order is:
plain-input API → shed `bayesnec` (*"this is the breaking release"*) → `bayesnec`
consumes → drop `Remotes:`.

`REFACTOR-human.md` §4 now runs: decide → regression net → spine → shed
`bayesnec` → move metrics → tibble → **CRAN** → one `bayesnec` PR.

These disagree about which release is the breaking one and about where CRAN
sits. §4 is the current one. `PHASE1` should be superseded or folded in rather
than left to be read as live.

### 2.3 `PHASE1` Step 1 and `toxval_pred` are the same work

Step 1 says "add the estimator entry points that take a posterior matrix and
`x_vec`". That is `toxval_pred` (`REFACTOR-human.md` §3.3/§3.4). Building both
would mean building the seam twice. Said on #39; not in `PHASE1`.

### 2.4 `02_decisions` T4 is too strong

> Do not port fixes back, and do not consult `bayesnec`'s copies as a
> reference — they are the older and, in at least one respect, worse
> implementation.

Measured on the full fixture suite, `toxval::ecx.bnecfit` is the **outlier**: it
uses a single scalar reference, while `toxval::ecx.brmsfit` and
`bayesnec::ecx.bayesnecfit` both compute the reference per draw. Two of the
three agree, and the odd one out is toxval's.

T4's intent — do not adopt `bayesnec`'s automatic `crf()` back-transformation
(T6) — still holds exactly. But "do not consult as a reference" is wrong as a
blanket rule and would have led to the wrong answer on #19.

### 2.5 `02_decisions` "still to decide" is stale

- **#4, always return a tibble** — decided. `REFACTOR-human.md` §3.1.
- **#3, `ecx_val` as a proportion** — still open, and now interacts with the
  closed `metric` vocabulary and its `ecx_val` column (§3.1).
- **#22, fitting wrappers** — still open, but now has a forcing case:
  `anchor = "control"` needs a control-only fit (§3.8).

### 2.6 `02_decisions` "issues to file" is stale

- "The Tier 0 untangle — no issue exists" → **#39 exists.**
- "bayesnec #39 and #44 have no counterpart here" → **toxval #40 and #41 exist.**
- "close bayesnec #166 as a duplicate of #29 when fixed" → still to do.

### 2.7 T7 — the shape of the #19 keystone has changed

T7 says #19 blocks #1, #8, #12 and #14, and that none is in the unattended
queue. Now:

- the **`ecx`** half of #19 is a single decision (per-draw reference) and still
  blocks the refactor;
- the **`nsec`** half is exposed as `anchor` rather than resolved, so it no
  longer blocks — only the default needs agreeing;
- **#1 and #8** (`hormesis_def = "max"`) are largely absorbed by the `direction`
  decision (#20), which replaces `hormesis_def` rather than fixing it.

### 2.8 toxval's Tier 1 and the refactor collide on sequencing

**Status checked 2026-08-19.** `bayesnec`'s Tier 1 **has** landed — PRs #197–#208
merged 14–17 Aug, issues #176/#180/#188/#191/#211/#213/#214 closed 14–18 Aug,
matching `PHASE1`'s status line. **`toxval`'s Tier 1 has not**: zero closed
issues on `open-AIMS/toxval`, most recent merged PR #38 on 2026-08-07 (a week
before these notes were written), and no fork branch carrying the work. The
conflict below therefore still applies.

`01_work_queue` Tier 1 is twelve issues designed as an unattended run **on the
current architecture**. `REFACTOR-human.md` phase 1 is "capture current
estimates as golden values".

If Tier 1 runs first, the golden values move. If the refactor runs first, Tier 1
lands in rewritten code. They were planned independently and have not been
reconciled.

Suggested split:

| Tier 1 item | verdict |
|---|---|
| #33 commented code, #31 test messages, #32 generic/method consistency | **safe either way** — mechanical, no behaviour change |
| #37 TODO markers | safe, but flagged in the queue itself as behaviour-changing; do it before the net is locked |
| #5, #7, #15, #6/#24, #11, #13/#10, #34, #29 | **fold into the refactor.** All are behavioural, and #34, #13/#10 and the `xform` defect are already listed in `REFACTOR-human.md` §3.7 |

### 2.9 `bayesnec`'s scope boundary predates #216

`bayesnec` `00_protocol.md` puts everything touching `ecx`/`nsec`/`ecnsec`/
`zero_crossings` out of scope, listing #39, #44, #166, #195, #196 as toxval's.

**bayesnec #216 is new and is not in that list**, and it is genuinely
`bayesnec`'s: the unseeded resampling lives in `w_nec_calc()` and
`w_post_pred_calc()` (`R/helpers.R`), which are model-averaging code and are
**staying**. Only the call sites in `ecx.bayesmanecfit` / `nsec.bayesmanecfit`
are moving.

So #216 needs adding to `bayesnec`'s queue as in-scope, with a note that the
`helpers.R` half is the part to fix.

### 2.10 `bayesnec` Tier 2 #120 now overlaps the untangle's last step

#120 is deferred until the untangle lands because it changes
`predict`/`plot`/`autoplot` for `bayesmanecfit`. The untangle's final step now
*also* changes `plot`/`summary`/`bind_ecx` to consume the `toxval` tibble. Same
files. Worth doing together rather than in sequence.

### 2.11 `03_uniroot_spike.md` does not exist

Referenced by `01_work_queue.md:207` and `02_decisions.md:75`. The likely
content is `bayesnec/ignore/uniroot.R`, which is in a gitignored directory.
toxval **#40** now covers the work. Either write the spike up or point the
references at #40.

### 2.12 "Tier 1" is ambiguous across the two repos

Both repos have `notes/implementation/01_work_queue.md`, both use "Tier 1" and
"Tier 2", the contents differ, the completion states differ, and the documents
cross-reference each other. `bayesnec` Tier 1 is done; `toxval` Tier 1 is not
started.

This has already caused one misreading in conversation. Worth qualifying every
reference as "toxval Tier 1" or "bayesnec Tier 1", or renaming one set.

### 2.13 D6 anticipated the `anchor` argument

Not a conflict — worth noting the two line up. `bayesnec` D6 splits #148
diagnostics and assigns to toxval "how *NSEC* responds to `sig_val`, **to the
reference definition** and to resolution". That reference-definition sensitivity
is now a first-class argument (`anchor`, §3.8), so D6's diagnostic has something
concrete to vary.

---

## 3. The coordination gap

`notes/implementation/` and `PHASE1_DEPENDENCY_UNTANGLE.md` are **untracked in
`toxval`**. They are invisible to a fresh clone, to CI, and to Ayla — who is now
actively working on this repo and whose PR #42 is being reshaped by decisions
that live only in those files.

`PHASE1` §7 already flagged this as undecided. It is more pressing now: the plan
on PR #44 cites reasoning (#19's status, the Tier 0/1/2 split, T4/T6) that a
reader cannot see.

Options: commit them, move the parts that matter onto issues, or accept that
they are private and stop citing them in shared documents.

---

## 4. Suggested order of fixing

1. **Qualify every "Tier 1" reference by repo** (§2.12). Cheap, and it stops the
   two queues being read as one.
2. **Decide whether `notes/implementation/` and `PHASE1` are shared** (§3). It
   determines whether the rest is edited in place or migrated to issues.
3. **Supersede `PHASE1`** — §2.1, §2.2 and §2.3 all live there.
4. **Amend `02_decisions`** — T4 (§2.4), the two stale lists (§2.5, §2.6), T7
   (§2.7).
5. **Reconcile toxval Tier 1 against the refactor phases** (§2.8) — the one with
   real wasted-work risk.
6. **`bayesnec` notes** — add #216 (§2.9), note the #120 overlap (§2.10), and
   correct D8's `Remotes:` assumption (§2.1).
7. **Resolve the dangling spike reference** (§2.11).
