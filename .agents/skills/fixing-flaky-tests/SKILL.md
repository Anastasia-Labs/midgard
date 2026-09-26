---
name: fixing-flaky-tests
description: Measures, root-causes and fixes an intermittent Midgard test, then proves the fix with a rerun count sized to the measured failure rate. Use when a vitest suite, emulator scenario, Postgres-backed node test or CI step fails only sometimes, passes on rerun, times out under load, or fails in CI but not locally; before raising a timeout, adding a retry, skipping or deleting a test to get green; and when someone claims a flake is fixed.
---

# Fixing flaky tests

A flake is a test whose result changes while the code does not. Work from a
**measured rate**: how many runs failed, out of how many, where. A summary,
one red run or one green run is not a rate.

The rules for what a test should assert, and the traps that make a green
result meaningless, are in the sibling [writing-tests](../writing-tests/SKILL.md)
skill and its [mistakes-we-make.md](../writing-tests/references/mistakes-we-make.md).
This skill is about intermittence.

## 1. Measure the rate

Get it from CI history and from local reruns. Read
[references/measuring.md](references/measuring.md) for the copy-ready `gh`
commands and the local escalation ladder. `[review]`

Classify what you measured:

| Pattern                                              | It is                     | Next                                 |
| ---------------------------------------------------- | ------------------------- | ------------------------------------ |
| Passes and fails interleaved, same SHA or same code  | a flake                   | step 2                               |
| Fails on every run from some SHA onward              | a regression              | bisect; this skill does not apply    |
| Fails on every run on one machine, passes on another | an environment difference | find the difference; do not rerun it |

Done when you can write "failed f of n runs" with the run ids or the local
command, and `k ≈ n / f` is the rate's denominator.

## 2. Reproduce and find the cause

Reproduce locally with the rerun script, at the lowest escalation level that
fails:

```sh
node .agents/skills/fixing-flaky-tests/scripts/rerun.mjs --times 20 --k <k> -- <command...>
```

It runs the command N times in sequence under `nice -n 19`, prints the first
failure's tail, and exits 0 only if every run passed
`[script: .agents/skills/fixing-flaky-tests/scripts/rerun.mjs]`.

Then match the symptom against
[references/midgard-flake-sources.md](references/midgard-flake-sources.md):
emulator clocks and `zeroTime`, wall-clock validity bounds, timers racing a
budget, shared Postgres shards, `synchronous_commit`, wasm linear memory,
machine load, ports and file parallelism. Read it before you form a theory.

Done when you can state the mechanism in one sentence and point at the line
that makes the result depend on timing, order, load or environment. If you
cannot, say "cause not found" and stop at the report; do not apply a fix you
cannot explain. [review]

## 3. Choose one outcome

Ask first: does this test catch a realistic regression that nothing else
catches? (The gate in [writing-tests](../writing-tests/SKILL.md#1-the-gate-two-questions).)

- **Fix it.** Remove the source of nondeterminism: inject the clock, drive
  the emulator's time, give the fork its own resource, poll for the state the
  contract promises.
- **Re-level it.** Move the assertion to a cheaper, deterministic level from
  the [writing-tests level table](../writing-tests/SKILL.md#3-pick-the-cheapest-level-that-catches-it),
  for example a decision extracted into a pure function and tested with
  explicit inputs. The expensive test keeps only what needs the composition.
- **Delete it.** Only with the owner's explicit approval, and the request
  names either the test that now covers the regression (`path:line`) or the
  coverage that is lost. `[review]`

## 4. Fix without masking

These moves turn the test green without removing the cause. Each one is a
fix only under the condition in the last column. `[review]` No check detects
any of them; as of 2026-09-25 no vitest config sets `retry`
(`grep -rn "retry:" demo/*/vitest.config.*`), and nothing stops one being added.

| Masking move                                                | What it hides                                        | Acceptable only when                                                                                                                                                                                                                     |
| ----------------------------------------------------------- | ---------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| vitest `retry`, a retry loop, rerunning CI until green      | the failure itself                                   | never, for a test; a bounded poll for a state the production caller also waits for is not a retry (it still fails hard with a state dump, as in `e7b91208e`)                                                                             |
| a bigger timeout                                            | a hang, or a slowdown that will cross the new budget | you measured the real cost and the budget sits too close to it under contention, as in `ac1b630e1` (~22 s cost, 60 s budget, 2 of 4 CI runs timed out). A second raise for the same test is a diagnosis, not another raise (same commit) |
| a `sleep` before an assertion                               | an ordering bug; fails again on a slower machine     | never; wait for the condition                                                                                                                                                                                                            |
| `it.skip`, `skipIf`, a silent early return                  | everything the test checked                          | a missing prerequisite, with the reason printed (see `3e2c712ca` in [mistakes-we-make.md](../writing-tests/references/mistakes-we-make.md))                                                                                              |
| `.only`, `-t` filters, running the file alone               | the neighbour that shares state with it              | as a diagnostic step, never as the result                                                                                                                                                                                                |
| one fork (`MIDGARD_NODE_TEST_FORKS=1`)                      | a resource shared between files                      | as a diagnostic; to cut memory, lower the fork count and say so                                                                                                                                                                          |
| a looser assertion (`toThrow()` without a matcher, a range) | the wrong failure passing as the right one           | the contract really is looser; cite the caller that shows it                                                                                                                                                                             |
| regenerating a golden, pin or ledger to the observed value  | the nondeterminism, now committed                    | the output is deterministic and the change is intended ([regenerating-goldens-and-ledgers](../regenerating-goldens-and-ledgers/SKILL.md))                                                                                                |
| raising `heapMb`                                            | wasm memory, which lives outside the V8 heap         | never for wasm; lower the fork count or split the file (`demo/midgard-test-support/vitest.js:50-53`)                                                                                                                                     |

## 5. Validate with a sized rerun

A clean run proves little unless N is large relative to the rate. For a flake
that fails about 1 in k runs, run it

**N = max(3k, 20) times**, all passing.

Why 3k: a flake with rate 1/k passes one run with probability (1 − 1/k), so
it passes N runs by luck with probability (1 − 1/k)^N. At N = 3k that is
about e^−3 ≈ 5% (k = 10: 0.9^30 ≈ 4.2%). The floor of 20 keeps a small k from
producing a handful of runs. The script prints this for your N and k, and
warns when N is below max(3k, 20)
`[script: .agents/skills/fixing-flaky-tests/scripts/rerun.mjs]`.

Rules for the loop:

- The same command and the same escalation level that reproduced it in
  step 2, run on the fixed code.
- Any failure sends you back to step 2. A later clean batch does not cancel it.
- If N × one run's cost is too large, narrow the command, or report the N you
  ran and the rate it rules out (the script prints it: N clean runs rule out
  about a 3/N failure rate). Do not call that a fix. [review]

Done when the script exits 0 with N ≥ max(3k, 20) and no warning.

## 6. Report

```
Test:          <path>::<name>
Measured rate: <f> of <n> (<CI run ids | local command>), k ≈ <n/f>
Reproduced at: <escalation level and command, or "not reproduced">
Load:          <uptime / nproc at the time, for timing flakes>
Cause:         <one sentence, with the path:line that depends on timing, order, load or environment>
Outcome:       fix | re-level (to <level>) | delete (approved by <owner>, coverage: <path:line or "lost: ...">)
Change:        <what changed and why it removes the cause, not the symptom>
Validation:    rerun.mjs --times <N> --k <k> -- <command>  →  <N>/<N> passed
Not checked:   <what you did not run, e.g. CI, other packages>
```

Report "cause not found" or "not reproduced" plainly when that is the
result. A wrong cause costs more than none. `[review]`
