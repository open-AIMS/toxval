## Session: phase1-regression-net
Date: 2026-09-06
Model: Claude Opus 4.8 (1M context)

### Prompts and Responses

**User:** Phase 0 was just completed. Time to start working on phase 1. Do not make the
changes unless I directly tell claude to make changes. Generally I want to make the changes,
you help with the planning, figuring out issue and checking things once done.

**Claude:** Established the planning role (no edits unless told). Read `REFACTOR-human.md` §4,
`REFACTOR-claude.md` §4 and §3.9 (ecnsec/golden), the test infrastructure (`setup.R`,
`helper-fixtures.R`, the 12 `.rda` fixtures), the `if (FALSE)`/TODO ledger, and the `ecx.R`
method bodies to ground the classification.

Key code confirmations:
- `ecx.bnecfit` (R/ecx.R:146) builds ONE scalar reference from posterior medians
  (`median(control_posterior) - median(dif_valsC) * ecx_val/100`) — the estimator #19/T9
  deletes. So its values are **expected to move**.
- `ecx.brmsfit` (R/ecx.R:336-337) already computes per realisation via
  `apply(p_samples, 1, ecx_x_<type>, ...)`, each realisation using its own `y[1]` control.
  So its values are **must not move** (only `relative`→`range` label remap under T9).
- Nothing is seeded. Deterministic today: brmsfit, single bayesnecfit, drc, nsec_multi.
  Non-deterministic: any bayesmanecfit path (model averaging, unseeded, bayesnec #216).

Proposed the Phase 1 structure: capture current estimates as golden values with all
grid-affecting args pinned (resolution, x_range, prob_vals, ecx_val) so the §3.7 default
unification cannot masquerade as a regression; split into three buckets (must-not-move /
expected-to-move-because-X / cannot-be-pinned-by-equality due to #216); baseline green on
current code.

Raised two blocking decisions via AskUserQuestion: (1) storage mechanism for golden values;
(2) how to treat the non-reproducible model-averaged fixtures.

---

**User:** [clarification] Prefer updating tests in place while changing each function, not a
separate golden-value framework. Add a `# EXPECTED-CHANGE <minimal description>` comment to
every test whose assertion the refactor will change (replacing the existing `# TODO` markers);
keep `if (FALSE)` wrappers where the test currently fails; minimal detail, no code line refs.
Also: challenged the "flaky manec tests" claim — the fixtures are not regenerated between runs,
so why would results move?

**Claude:** Agreed the in-place marker approach fits the repo idiom. Then settled the
reproducibility question empirically (`Rscript`, `devtools::load_all`):
- `bnec_model_1` (DATASET.R:175) is `bnec(y ~ crf(x, model = "nechorme"))` — a SINGLE-model
  `bayesnecfit`, no averaging, deterministic. User was right; my earlier lumping was wrong.
- `manec_example` IS non-reproducible, but via call-time resampling, not fixture regeneration:
  `predict.bayesmanecfit` (R/predict.R:67) calls `posterior_predict()`, which re-draws a
  weighted sample across component models each call using the unseeded global RNG (bayesnec
  #216). Proven: two unseeded `ecx(manec_example)` calls differ; `set.seed(10)` before each
  makes them bit-identical. toxval has no `sample()`/`set.seed()` of its own.

Scanned all four test files for hard-value assertions on `manec_example`: only ONE exists
(test-ecx.R:947 "bayesmanecfit works"), already widened to tol 0.02-0.05. Every other manec
test asserts structure or relative comparisons, immune to jitter. So: NO tolerance increases
needed; my "latently flaky" warning was overstated. Recommended leaving all tolerances as-is
(including the deterministic 0.01 tests — tightening would need full-precision re-recording for
marginal gain) and treating the one manec value test as an EXPECTED-CHANGE (gains seed +
tighter tolerance once §3.4/T11 makes averaging reproducible).

---

**User:** [agenda decisions] 1. Leave tolerances as-is, flag test-ecx.R:947. 2. Markers do NOT
capture the phase-5 tibble shape change (the output rewrite changes things too broadly). 3. Flag
the `predict` tests even though the change is their removal. 4. Driver list accepted. Do nothing
yet.

**Claude:** Recorded a correction found while checking scope: toxval's `nsec()` DOES compute
`ecnsec` (nsec.R:138-139, `ecnsec_relativeP` attribute), contradicting the PHASE1 doc's "toxval
does not have ecnsec at all" — so #49/T8 changes existing behaviour and ecnsec-attribute
assertions are EXPECTED-CHANGE. Confirmed the phase-5 blast radius is ~113 structural-assertion
lines across test-ecx.R + test-nsec.R, which is why the shape change is excluded from marking.

Phase 1 marking spec now fully settled:
- Token `# EXPECTED-CHANGE <minimal description>`, no code-line/issue/phase tags.
- Replace existing `# TODO` markers that describe a coming change; keep `if (FALSE)` wrappers
  where the enclosed test currently fails (their comment converts to EXPECTED-CHANGE).
- Flag ONLY targeted value/behaviour changes, NOT the phase-5 tibble rewrite.
- Flag `predict.bayesnec*` tests as being removed (methods move to bayesnec, phase 3).
- Drivers: #19/T9 (ecx.bnecfit reference), T9 rename (relative->range, new relative),
  #20/T10 (hormesis_def -> direction), §3.4/T11 (drc bootstrap + seed), #34 (drc curveid),
  #49/T8 (ecnsec), #13/#10 (x_range), #7 (multi-type error), Tier-1 bugs #5, #6/#24, #15,
  #11, #29, and the `if (FALSE)` blocks that get enabled.
- test-ecx.R:947 ("bayesmanecfit works"): EXPECTED-CHANGE (gains seed + tighter tolerance once
  averaging is reproducible).

Next step (not started, awaiting go): produce the enumerated marker list — per test block: file,
line, driver, and suggested minimal comment text — for the user to apply.

---

**User:** Ok, generate the read-only list.

**Claude:** Read all five test files in full and produced the enumerated EXPECTED-CHANGE list
(read-only, no edits), grouped by file with test/line, driver, and suggested minimal comment.
Summary of the flag set:
- test-ecx.R: 23 tests — #20 (hormesis_def) block, T9 (type "relative"/"range" + vocab/message),
  #19 (all ecx.bnecfit/bayesnecfit hard-value tests), #13/#10 (x_range), #15 (group_var==x_var),
  #6/#24 (bound warning), typo fix.
- test-nsec.R: 19 tests — #49 (ecnsec redefinition, brms/drc ecnsec attrs; nsec estimate itself
  unchanged for brms), §3.4 (drc parametric bootstrap changes estimate + needs seed), #20 (horme/
  hormesis_def), #13/#10 (x_range), #34 (drc curveid/x_var).
- test-nsec-multi.R: none (already the direction model; deterministic; only phase-5 relabel).
- test-predict.R: all 6 predict.bayesnec* tests -> removal (methods move to bayesnec, phase 3).
- test-helpers.R: modify_posterior x2 (#20), newdata_eval x3 (bnecfit branch -> bayesnec removal).
- if(FALSE) blocks: keep wrappers, convert 4 comments (one, test-nsec.R:447, is disabled for
  covr not failure — flagged for user confirmation).

Surfaced beyond the confirmed driver list for the user's call: #9 (multiple ecx_val, test-ecx
:466), #3 (ecx_val proportion, test-ecx:476), and the `posterior`->`draws` §3.5 deprecation
(broad ~15+ test sweep; recommended treating as a known sweep like the tibble change, not
flagged individually). Left pure-comparison tests unflagged where the assertion survives
(flagged test-ecx:1150 for eyeball); noted #5/#11/#29 add new tests rather than change existing
ones; noted fixtures will need skip_if_not_installed("bayesnec") guards at phase 3.

---
