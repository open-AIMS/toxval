## Session: phase-2 crossings review (#59)
Date: 2026-09-08
Model: Claude Opus 5 (1M context), claude-opus-5[1m]

### Prompts and Responses

**User:** Can you help me work through Ayla's comments on
https://github.com/open-AIMS/toxval/pull/59?

**Claude:** Three points were raised: two as `TODO` markers in
`tests/testthat/test-crossings.R`, and one flagged in the pull request body as
settled rather than open. No inline review comments exist on #59.

Analysis and the decisions taken follow. The brief was to make the minimum
change that resolves Ayla's questions, so `find_crossings()` is unchanged apart
from documentation.

**1. The endpoint asymmetry.** Ayla asks whether it is acceptable that departure
from the reference at the first grid point counts as a crossing while arrival at
the last does not.

It is not an endpoint rule. It is a tie-break: a zero in `d = y - reference` is
assigned to the left of its interval, under `lo >= 0 & hi < 0` and
`lo <= 0 & hi > 0`. Only `n - 1` intervals are scanned, so the last grid point
starts no interval and can never be a crossing location, while the first can.

The convention is kept, because the alternative (`lo > 0 & hi <= 0`) returns `NA`
for a curve that sits on the reference and then departs, which is the quantity
`nsec` estimates:

| curve, `reference = 2` | current | alternative |
|---|---|---|
| `c(2, 2, 2, 3, 4)` | `inc = 2` | `NA` |
| `c(2, 2, 2, 1, 0)` | `dec = 2` | `NA` |

**Change made.** No code change. The `TODO` was replaced with the decision, and
the two plateau curves above were added as tests, so the reason the convention
is what it is has an assertion behind it. An `@details` block in
`R/crossings.R` states the convention where the code is, rather than only in a
test comment.

**Considered and rejected: a guard on tangential touches.** The same tie-break
reports a crossing where the curve touches the reference and returns the way it
came — `y = c(4, 2, 4)` against `reference = 2` gives `inc = 1`, and
`y = c(0, 2, 0)` gives `dec = 1`, although in each the curve is on the same side
before and after. A guard conditioning a zero run on the side approached from
was written, tested and reverted.

It was rejected for two reasons. The case needs an interior fitted value to
equal the reference bit-for-bit in double precision, which will not occur with
posterior draws, so it changes no number a real fit produces. And the guard's
answer is not right either. Walking the minimum of a U-shaped curve down through
the reference, `dec` and `inc` converge on the touch point — 1.996 and 2.004 at
a minimum of 3.499 against `reference = 3.5` — so the limit of a shrinking
excursion is *both* crossings at the same x. The existing rule returns one, the
guard returns none, and neither is the limit.

That points at a definition rather than a defect. Under a change-of-side reading
a tangency is not a crossing; under a direction-of-travel reading — the curve
reaches the reference level, and the direction is whether it was falling or
rising when it got there — it is both, which is the limit, is symmetric, and
removes the asymmetry Ayla raised. The second reading would also change the
existing assertion at `test-crossings.R:137`. Left open for #59 rather than
settled here, because it is a change to what the function means and the brief
was the minimum change.

**2. Whether an `NA` at the crossing should bridge.** Ayla's code does not
bridge; her comment at `test-crossings.R:180` says she thinks it should return 3
rather than `NA`.

The code is right and the comment is not. `modify_posterior()`
(`R/helpers.R:12-20`) only ever writes a *leading* block of `NA`s
(`x_vec < target`), so it cannot produce an interior gap. Once T10 removes it,
an interior `NA` is a prediction failure, and bridging interpolates across a
region the model did not fit.

Her test returns 3 under bridging only because its data is linear. Measured on a
decay curve with one grid point dropped:

```
grid 0:5, y = 10 * exp(-x), x = 2 missing, reference = 1.5
true crossing          1.8971
bridged over the gap   2.3690   (+24.9%)
same grid, no gap      1.9360   (+2.0%)
```

Bridging multiplies the interpolation error twelvefold and issues no warning.

**Change made.** No code change. The `TODO` was replaced with the decision and
the measurement, and the speculative inline comment removed.

**3. No crossing returns `NA` rather than `max(x_vec)`.** Confirmed as settled by
T10. One consequence was found that the plan does not record.

`nsec.R:144`, `ecx.R:396` and `ecx.R:405` call `quantile()` with no `na.rm`.
They are safe today only because `nsec_fct()` (`R/helpers.R:122-134`) and the
three `ecx_x_*()` functions return `max(x_vec)` rather than `NA`. The first
non-crossing draw will error with `missing values and NaN's not allowed if
'na.rm' is FALSE`, so this is an error path rather than a change in values.

That forces a decision at phase 4 which is not yet recorded.
`REFACTOR-claude.md:490` settles the aggregate case — a direction with no
crossing emits a row with `NA` — but not the mixed case, where some draws cross
and some do not. Setting `na.rm = TRUE` drops the non-crossing draws, so where
30 per cent never cross the reported median is the 35th percentile of the full
posterior. The previous `max(x_vec)` treated them as censored at the highest
tested concentration instead. Neither is obviously right, and the choice should
not fall out of an `na.rm` argument.

**No change made.** To be raised on #59 and, if agreed, opened as an issue before
phase 4.

---
