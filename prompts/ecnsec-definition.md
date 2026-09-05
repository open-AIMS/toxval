## Session: ecnsec definition (#49)
Date: 2026-09-04
Model: Claude Opus 5 (1M context), claude-opus-5[1m]

### Prompts and Responses

**User:** The refactor plan has been merged into main. Can you update local, and
then Ayla and I agreed I would work on issue 19, 20 and 49 before she then
tackles the actual refactor.

**Claude:** Synced `main` to `upstream/main` (26 commits, through `ed48ca1`) and
pushed the fork to match. Read #19, #20 and #49 against
`notes/implementation/02_decisions.md` and reported their readiness: #19 and #20
have written proposals with measurements already on the issues and need
ratifying rather than more work; #49 has no proposal, and its evidence is read
from source at `aa1e158` rather than demonstrated numerically. Offered to
measure #49 first.

**User:** I do not think measuring it is necessary, ecnsec should be defined in a
consistent manner to ecx by default, and should take any arguments that ecx
takes.

**Claude:** Derived what that resolves to. `ecx` maps a percentage to a response
value; `ecnsec` is the inverse map, so defining them consistently fixes `ecnsec`
as the algebraic inverse of `ecx_x_<type>()`:

| `type` | `ecx` reference (`R/ecx.R:181-254`) | `ecnsec` |
|---|---|---|
| `absolute` | `y[1] - y[1] * (p/100)` | `(1 - R / y[1]) * 100` |
| `relative` | `max(y) - (max(y) - min(y)) * (p/100)` | `(1 - (R - min(y)) / (max(y) - min(y))) * 100` |

Checked against current `main` rather than the `aa1e158` the issue cites. The
issue's reading holds; #48 has since corrected the `MARGIN = 2` hormesis
reference in `nsec.brmsfit`, but the `ecnsec` formulas are unchanged.

`nsec.bnecfit` under `type = "absolute"` (`R/nsec.R:138`) already implements the
decision. Its `relative` branch (`:110-115`) takes the range from the curve
endpoints where `ecx_x_relative()` uses `range(y)`, which agree only for a
monotonic curve. `nsec.brmsfit` (`:232-234`, `:273-275`) and `nsec.drc`
(`:434-436`) apply the relative formula unconditionally and have no `type`
argument.

Reported the direction of the change at defaults as algebra rather than
measurement: for a decreasing curve with a non-negative minimum,
`(R - min)/(max - min) <= R / y[1]`, so `nsec.brmsfit` and `nsec.drc` will report
a smaller `ecnsec` than they do now. The NSEC estimate is unaffected.

Also established what "the arguments `ecx` takes" excludes: `ecx_val` and
`sig_val` are different quantities, and `hormesis_def` is retired by #20, so
neither is threaded further. The argument actually aligned is `type`, which makes
this an S3 signature change and ties it to #32.

**Claude (question):** Asked what `ecnsec` should be under `type = "direct"`,
having assumed there was no value to report.

**User:** I'm not sure I understand what you are asking. A type = "direct" can
still have a value for an ecnsec.

**Claude:** The premise was wrong. In `nsec` the reference is always derived from
`sig_val` and the control posterior (`R/nsec.R:117-121`); `type` never touches
it, and selects only the denominator. So a reference value always exists under
`direct`. Restated the question as what is reported when there is no denominator,
and set out two readings: the reference on the response scale, or a fallback to
the `absolute` percentage.

**User:** the reference itself, on the response scale.

**Claude:** Recorded that. `direct` then means the same thing in both directions
of the map — `ecx` takes a response value as input, `nsec` reports one as output.

Derived one consequence and recorded it as a stated assumption for review rather
than asking a third time: the `nsec` reference is a scalar across realisations,
and `ecnsec` varies between realisations only through its denominator, so under
`direct` there is no variation. `ecnsec` equals the `reference` column and
`ecnsec.low`/`ecnsec.high` are `NA` — `NA` rather than bounds equal to the
estimate, since equal bounds assert an interval of zero width where the intent is
that none is defined. This follows the `REFACTOR-claude.md` §3.1 column rule,
which already gives `control` as `NA` under `type = "direct"`.

### Changes made

Branch `issue-49-ecnsec-definition` off `main`.

- `REFACTOR-claude.md` — new §3.9 stating the definition in full: the formula
  table, what each method does today, the `direct` interval result, the change at
  default settings, and the tests. §3.1 updated so `type` is a descriptor column
  for `nsec` as well as `ecx`, and the sentence describing `ecnsec` as a
  percentage change from the control corrected, since that describes the
  `absolute` case only.
- `REFACTOR-human.md` — §2 decision table given a status column with #49 marked
  settled and the answer stated in three sentences; §3.1 descriptor list and
  `control` rationale updated to match.
- `notes/implementation/02_decisions.md` — decision T8 recording the decision and
  pointing to §3.9 for the full statement, per the §13 rule that a decision is
  stated in full in exactly one document. T1 amended: it still specified
  branching off `dev`, which was dropped in #50.

Draft comment for #49 written for review before posting; not posted.

### Reasoning not visible in the diff

**Why the full statement is in `REFACTOR-claude.md` and not `02_decisions.md`.**
The specification is the document a session implements from, and the existing T7
/ §3.8 pair sets the pattern: the decisions register records that a decision was
made and points to where it is stated. Duplicating the formulas in both would
guarantee they drift.

**Why the change at defaults was reported as algebra.** The inequality
`(R - min)/(max - min) <= R / y[1]` holds for any decreasing curve with a
non-negative minimum, so the direction is known without running a fixture. The
magnitude is not, and was not claimed. Sizing it needs phase 1 golden values,
which is where it belongs.

**What was deliberately not touched.** `R/ecx.R` is governed by #19, which is
still open; `ecnsec` inverts whatever reference construction that settles on, so
the two decisions compose rather than conflict. The `hormesis_def == "max"`
branches were left for #20 rather than realigned here, since #20 retires the
argument entirely and realigning it first would be work done twice.

---
## Session: the ecx reference and the type vocabulary (#19)
Date: 2026-09-04
Model: Claude Opus 5 (1M context), claude-opus-5[1m]

### Prompts and Responses

**User:** I posted a comment on 19, resolving the open questions. Can you examine
them and comment here on any gaps in the logic?

**Claude:** Reviewed the proposed ECx definitions and reported eight gaps, with
the factual claims checked against `bayesnec` 2.1.3.7 under R 4.6.1 by reading
the model definitions rather than fitting anything. The measurements that
mattered: 9 of the 23 equations have a `bot` parameter and 14 do not, and every
model group mixes the two (`decline` 6 and 8, `all` 9 and 14); `bnec()` fits one
family across a set, so `manec_example` is gaussian throughout; of the ten
accepted families, five have no upper bound.

The gaps were: `relative` undefined for a model-averaged fit; the drop-with-a-
warning branch unreachable under a family-based criterion; `absolute` needing the
same identifiability guard in the increasing direction; `absolute` using 0 on a
gaussian fit; `relative` changing meaning without a deprecation path; hormesis
not being identified by a non-`NA` increasing ECx; the #49 formula table needing
revision; and asymptote discovery being unspecified for `drc` and `brmsfit`.

**User:** [answered each in turn — amended `relative` to be refused for a
model-averaged fit unless all models have `bot`; confirmed boundedness is judged
by family and per equation and accepted that `absolute` and `relative` may use
different model sets; asked for clarification on the increasing bound; recorded
that `absolute` on gaussian is intended and follows OECD, citing bayesnec draft
vignette 7; accepted the deprecation; amended the hormesis wording; queried the
#49 point; proposed a user-supplied asymptote argument for custom formulae]

**Claude:** Checked the OECD claim and withdrew that gap. `bayesnec`
`vignette("example7")` and `notes/oecd_flooring_scoping.md` record TG 201's
`%I_r = (µ_C − µ_T)/µ_C × 100`, which rearranges to `µ_T = µ_C(1 − p/100)` — the
`absolute` reference — and exceeds 100% for a negative response rather than
truncating. Two consequences followed rather than objections: the rationale
belongs in toxval's own documentation, and `ecx_val` must not be capped at 100
(it is uncapped at `R/ecx.R:285-287`, and that should be deliberate).

Confirmed `drc` resolvability concretely: `fct$names` gives the parameter letters
and `fct$fixed` which are fixed and to what, with `LL.4()` leaving the lower
limit `c` free and `LL.3()` fixing it at 0 — which collapses `relative` onto
`absolute` for the same reason a `bot`-free equation does.

Reported that three points remained. The amendment to `relative` contradicts the
answer on partial drops for a bounded family, and more substantially does not
achieve what it was meant to: requiring every component to have `bot` does not
make the `bot` values agree. The increasing `absolute` bound of 1 breaks for a
response above 1. The hormesis signature is conditional on `ecx_val` and the
control.

**User:** [still confused on the model-averaging point]

**Claude:** Restated it with measured numbers from `manec_example`. Both
components have `bot`, so the amended rule permits the call, yet
`nec4param` (`bot` −8.42, control 2.17) puts the EC10 reference at 1.11 and
`ecx4param` (`bot` −5.69, control 2.31) puts it at 1.51 — 36% apart, both
labelled EC10 relative. Set out three resolutions: accept and document, refuse
for all model-averaged fits, or define a pooled asymptote. Recommended the first.

Also noted that on this fit `bot` is estimated at −8.42 with a 95% interval of
−13.58 to −5.68 against an observed response range of roughly 0 to 2.2, so it is
an extrapolated asymptote and `relative` inherits that uncertainty.

For the increasing bound, showed the failure concretely: control 20 with an
assumed bound of 1 gives a reference of 18.1, below the control, so an increasing
ECx would be measured against a decreased reference. Proposed erroring when the
control is at or above the assumed bound.

**User:** for 3, Accept it and say so. Can you write a follow up comment on 19
that is human readable that captures these decisions more clearly (leave my
comment as the human version). Update any of the claude facing things to ensure
these decisions are recorded.

**Claude:** Read "Accept it and say so" as the first of the three resolutions
offered for the model-averaging point, since the request for a consolidated
summary presumes every question answered, and flagged that reading for
correction. Recorded the decisions.

### Changes made

Branch `issue-19-ecx-reference`, stacked on `issue-49-ecnsec-definition` because
it rewrites §3.9, which is not yet merged.

- `REFACTOR-claude.md` — new §3.10 stating the decision in full: the per-draw
  reference, the four-value `type` vocabulary in both directions, `relative` as a
  component-level quantity with the `manec_example` measurement, the refusal
  rules, the OECD basis for `absolute` on an unbounded family, the increasing
  bound and its guard, asymptote discovery for `drc` and `brms`, and the
  deprecation of `relative` into `range`. §3.1 gains an `asymptote` descriptor
  column and records that `type` takes four values; §3.9's formula table
  rewritten to the new vocabulary with `range` added; §2 scope table given a
  status column.
- `REFACTOR-human.md` — §2 decision table updated, with a paragraph each for #19
  and #49 and the statement that #19 is gated on #20.
- `notes/implementation/02_decisions.md` — decision T9; T7's "`ecx` half still
  blocks" superseded.
- `notes/implementation/01_work_queue.md` — Tier 2 rows for #19 and #12/#14.
- `notes/drafts/issue-19-summary-comment.md` — draft follow-up comment, not
  posted.

### Reasoning not visible in the diff

**Why `relative` is accepted as a component-level quantity rather than refused.**
The alternative rules all fail on the same fact: the asymptote is a parameter of
the equation, so under model averaging the components cannot share one. Refusing
mixed sets removes some cases without removing the property, and defining a
pooled asymptote would require deciding what the 14 `bot`-free equations
contribute to it. §3.8 already accepts exactly this for the NEC, so accepting it
for `relative` keeps one rule rather than two.

**Why the `ecx` commuting result had to be qualified.** It justified `ecx` taking
no `anchor` argument, but it was measured under `absolute`, where the reference
depends only on the draw's own control. Under `relative` the reference also
depends on which component produced the draw, so the result does not carry over
and the justification is now type-specific.

**Why `ecx_val` is recorded as deliberately uncapped.** Nothing in the code
enforces a cap, so the decision is invisible in the diff. TG 201 producing more
than complete inhibition is the case that requires it, and without the note a
later reader would add the guard as an obvious omission.

---
## Session: direction as a property of the result (#20)
Date: 2026-09-04
Model: Claude Opus 5 (1M context), claude-opus-5[1m]

### Prompts and Responses

**User:** yes post the comment and push both branches. I also made a final
comment on 20 "A curve with no crossing in one direction emits a row with NA. The
direction was looked for and not found, which is information." I think that has
no other blocking decisions.

**Claude:** Posted the #19 summary and pushed both branches. Checked #20 against
`REFACTOR-claude.md` §3.6 rather than agreeing that nothing else was open, and
found one under-specification that the new §3.10 depends on.

§3.6 stated that the estimator looks for both crossings "of the reference" —
singular. That does not work for the case #20 exists to support: a monotonic
increasing response never crosses a reference set below its control, so both
directions would return `NA`. Each direction needs its own reference.

This is not a new decision. `nsec_multi` already builds two — `reference_dec` at
`sig_val` and `reference_inc` at `1 - sig_val` (`R/helpers.R:44-59`) — and
records both under `reference_vals`. Since #20 asks for `nsec_multi`'s behaviour
to be generalised, the existing implementation settles it. Recorded rather than
raised as a question. `type = "direct"` is the exception, taking one supplied
value for both directions.

### Changes made

Same branch, `issue-19-ecx-reference`.

- `REFACTOR-claude.md` §3.6 — the `NA`-row decision recorded in place of the "To
  pin down" block, and the per-direction reference stated with the `helpers.R`
  precedent. §2 scope table updated.
- `REFACTOR-human.md` — §2 table and a paragraph for #20.
- `notes/implementation/02_decisions.md` — decision T10.
- `notes/implementation/01_work_queue.md` — Tier 2 rows for #20 and #1/#8.
- `notes/drafts/issue-20-summary-comment.md` — draft comment, not posted.

### Reasoning not visible in the diff

**Why the per-direction reference was recorded rather than asked about.** There
is no coherent alternative — a single reference makes the increasing case
unreachable — and the existing `nsec_multi` code already implements the answer.
Raising it as an open question would have implied a choice that does not exist.

**Why the hormesis signature is recorded as indicative rather than diagnostic.**
Whether the increasing crossing exists depends on `ecx_val` and the control
level, so the same curve is classified differently at different `ecx_val`.
Hormesis is a shape property of the curve and does not depend on any `ecx_val`,
so the signature is useful to a reader but must not be implemented as a test.

---
## Session: frequentist realisations (#43)
Date: 2026-09-05
Model: Claude Opus 5 (1M context), claude-opus-5[1m]

### Prompts and Responses

**User:** for 3, I agree with the approach can you add a short succinct summary on
the issue that this is decided and update whatever you need to for ayla/the
plan/claude to know that. For 1, I'm not sure what you need, where do I look in
the issues to understand what I need to decide?

**Claude:** Recorded #43 as decided. The approach was already specified in
`REFACTOR-claude.md` §3.4 and needed ratifying rather than deciding, so the
comment states the decision and the reasoning without restating the section.

Also recorded that `bayesnec` #216 was closed on 2026-08-21, which removes the
reason the `anchor` default was left unratified — the magnitude of the change
could not be sized while the model-averaged resampling was unseeded.

### Changes made

Same branch, `decisions-19-20-49`, so PR #51 updates with it.

- `notes/implementation/02_decisions.md` — decision T11; the closing note of T10
  now points to it; the issue-status list updated for #43 and #216.
- `REFACTOR-claude.md` — §2 scope table marks #43 settled against §3.4, and the
  note below it now names the `anchor` default as the one phase 0 item still
  open.
- `notes/drafts/issue-43-comment.md` — the comment as posted.

### Reasoning not visible in the diff

**Why #43 did not gate phase 0.** It determines how uncertainty is generated for
a `drc` fit, not what any estimate means, so the regression net can be captured
before it lands. The three decisions that did gate phase 0 (#19, #20, #49) all
change which number an estimator returns.

**Why the consequence is recorded as a change rather than a fix.** The current
`drc` intervals are a different quantity, not a wrong version of the new one, so
substituting them silently would misrepresent what happened to anyone comparing
published values.

---
