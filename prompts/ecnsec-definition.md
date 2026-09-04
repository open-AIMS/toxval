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
