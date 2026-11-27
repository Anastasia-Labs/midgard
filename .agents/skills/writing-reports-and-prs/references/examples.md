# Before and after, from this repository

Both examples are real. The "after" versions use only facts on the record
(the diff, the issue comment, the PR's check list); nothing in them is
invented.

## A commit body: "green" hides a red suite and 108 skipped tests

Before (the committed body's verification sentence):

```text
Full fault-proofs and watcher suites re-run once against a fresh blueprint
(byte-identical to the one on disk) and reconciled with the earlier
baseline; the watcher journey suite and node-tools typecheck are green.
```

What the evidence posted on the issue showed:

- the full watcher suite exited 1 (2 files and 1 test red), both reds
  also present in the baseline and passing when re-run alone;
- the journey suite passed 203 tests and skipped 108, because 28 live and
  run-directory journey files skip without `MIDGARD_WATCHER_JOURNEY_RUN_DIR`
  and no devnet was up.

"Reconciled" and "green" are both true in a sense, and both hide the two
facts a reader most needs. After:

```text
Verified 2026-09-21 against a fresh testnet blueprint, byte-identical to
the one on disk; per-run output and the baseline delta are on the issue.

- fault-proofs full vitest suite: 4466 collected, 4462 passed, 4 skipped,
  exit 0 (baseline 4111 passed; +351 explained on the issue)
- watcher full vitest suite: 1487 collected, 1478 passed, 1 failed,
  8 skipped, exit 1. Both red files (user-event-origin hook timeout,
  user-event-runtime line 730) are baseline reds and pass alone.
- watcher journeys (vitest.watcher-journeys.config.ts): 203 passed,
  108 skipped, exit 0
- tsc --noEmit in fault-proofs, watcher and node-tools: exit 0

Not checked: the 28 live and run-directory journey files, which skip
without MIDGARD_WATCHER_JOURNEY_RUN_DIR; no devnet was up.
```

## A PR body: one line on a nine-file type change

Before (title "Update operator redeemer types and associated transactions"):

```text
Simple types update
```

The diff also does something the body does not mention: it repoints
`.pre-commit-config.yaml` (a symlink into `/nix/store`) to a different store
path. GitHub lists no checks on the PR.

After:

```markdown
## Summary

- operator redeemers in the Haskell off-chain code (registered, active,
  retired, operator directory) identify the anchor element by its
  `TxOutRef` instead of by spending-input index
- the removed-node and root input indices, and `Deinit`'s input index, are
  gone
- `.pre-commit-config.yaml` is repointed to a new Nix store path
  (unrelated to the type change)

## Verification

Not checked: no GitHub checks ran on this PR, and no off-chain build or
test run is recorded here.
```

The "after" is not longer for its own sake. It adds the one change the title
does not predict and states plainly that nothing was run, which is what a
reviewer needs to decide how hard to read the diff.
