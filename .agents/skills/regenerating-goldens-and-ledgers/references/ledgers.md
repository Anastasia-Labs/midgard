# Execution ledgers

A ledger records a measured cost, not a derived value, so "regenerate" means
"measure again". The measurement is only as good as the tree and compiler it ran
on. All facts below are as of 2026-09-25.

## Execution ledgers

Ten ledgers, one verifier each, in `onchain/aiken/scripts/`:

| Ledger (`*-exec-ledger-v1.json`) | Verifier                                                  | CI step                                                                           |
| -------------------------------- | --------------------------------------------------------- | --------------------------------------------------------------------------------- |
| `native-tx-carriage`             | `verify-carriage-exec-ledger-v1.mjs`                      | [ci: Aiken CI/Pin the §8.10 carriage execution ledger]                            |
| `native-tx-q1x`                  | `verify-q1x-exec-ledger-v1.mjs`                           | [ci: Aiken CI/Pin the Q1x family execution ledger]                                |
| `native-tx-q31`                  | `verify-q31-exec-ledger-v1.mjs`                           | [ci: Aiken CI/Pin the Q31 reference-input-no-idx family execution ledger]         |
| `native-tx-q21`                  | `verify-q21-exec-ledger-v1.mjs`                           | [ci: Aiken CI/Pin the Q21 transition-trace family execution ledger]               |
| `tx-order-mint`                  | `verify-tx-order-mint-exec-ledger-v1.mjs`                 | [ci: Aiken CI/Pin the tx-order mint material-carriage execution ledger]           |
| `canonical-decodability`         | `verify-canonical-decodability-exec-ledger-v1.mjs`        | [ci: Aiken CI/Pin the canonical-decodability committed-preimage execution ledger] |
| `committed-field-shape`          | `verify-committed-field-shape-exec-ledger-v1.mjs`         | [ci: Aiken CI/Pin the committed-field-shape slot-verdict execution ledger]        |
| `native-script-decoding-engine`  | `verify-native-script-decoding-engine-exec-ledger-v1.mjs` | none [review]                                                                     |
| `native-script-scan`             | `verify-native-script-scan-exec-ledger-v1.mjs`            | none [review]                                                                     |
| `transition-trace-descriptor`    | `verify-transition-trace-descriptor-exec-ledger-v1.mjs`   | none [review]                                                                     |

Run from `onchain/aiken/` (the verifiers run the aiken `demo/flake.lock` pins):

```sh
node scripts/verify-<lane>-exec-ledger-v1.mjs            # check
node scripts/verify-<lane>-exec-ledger-v1.mjs --update   # re-take
```

The transition-trace-descriptor verifier also needs `MIDGARD_AIKEN_ENV=testnet`
(`verify-transition-trace-descriptor-exec-ledger-v1.mjs:97-100`). The
carriage verifier has a make target, `make carriage-exec-ledger-v1` (Makefile:55).

Every reading goes through `exec-ledger-measure-v1.mjs` and
`run-focused-check.mjs`, which runs only the pinned compiler
[script: scripts/lib/aiken.mjs]. Each is a focused `aiken check` over the ledger's `modules[].module` selectors,
so a verifier takes as long as those modules take to compile and run. It is not
a light check.

### What `--update` absorbs, and what it refuses

The verifiers split failures into two classes
(`verify-carriage-exec-ledger-v1.mjs:56-61`,
`exec-ledger-within-basis-v1.mjs:122-126`):

- **Drift**: a measured number no longer matches the recorded one. `--update`
  rewrites these.
- **Structural**: a selector that did not run, a claim the ledger references
  that does not exist, a basis edited away from the one the lane is judged at,
  or a recorded judgement the fresh reading contradicts. These fail in both
  modes, and the ledger is not rewritten. The within-basis lanes share
  `checkWithinBasisExecLedger`; the carriage verifier carries its own copy of
  the same split [runtime: checkWithinBasisExecLedger].

So `--update` cannot launder a broken measurement. It can launder a real
regression, because a regression is a drift. The next section is the defence.

### Triage before re-taking

1. **Rerun on a clean tree.** The scripts can run no compiler but the pinned
   one. A reading taken on a
   tree with unrelated uncommitted `.ak` edits is measuring those edits.
2. **Read the drift, row by row.** Which rows moved, by how much, on which axis,
   and which did not move? Where a ledger has fixture-only control rows (the
   carriage ledger's `tier3_corner_fixture_only`, for one), they separate "the
   fixture got dearer" from "the code under test got dearer".
3. **Name the cause.** Find the commit that moved the numbers. The carriage
   ledger's own note bisects a re-take against builds of the intervening
   commits rather than assuming a cause
   (`native-tx-carriage-exec-ledger-v1.json`, `note`, "#606 re-take").
4. **Classify it.**
   - _Expected re-measure_: the cause is a change whose cost effect is intended
     or neutral, for example a compiler pin advance. Example: `7bb5f52cd`
     (2026-08-28), "consolidate on aiken v1.1.23+5adf783 and re-take exec
     ledgers".
   - _Accepted regression_: the cost rose, and someone decided to accept it.
     It is still pinned at the accepted number. The Q21 step's CI comment says
     so directly: "A regression that is understood and accepted still has to be
     held to the number it was accepted at"
     (`.github/workflows/aiken-ci.yml:227-234`). Accepting one is a decision for
     whoever owns the lane, not for the person re-taking the ledger [review].
   - _Structural_: the verifier already refuses it. Fix the selector, basis or
     claim, not the numbers.
5. **Write the cause note.** Each re-take appends a dated paragraph to the
   ledger's `note` that names the cause and the rows it moved. The convention
   dates from `f01c6ffcb` (2026-08-16), which gave every ledger a cause note and
   corrected two notes that asserted a guard which had been deleted [review].
6. **Move the published figures in the same commit.** The carriage ledger backs
   `docs/spec/midgard-tx.md` §8.10, and its verifier says to move that section in
   the same commit as the `--update`
   (`verify-carriage-exec-ledger-v1.mjs:228-232`) [review].

Blind spot: three verifiers have no CI step, so their ledgers can sit red for
any length of time. When the affected-channels script lists one, run it by hand.

## Van Rossem fit has no ledger

There are no fit ledgers. Every transaction a fault-proof emulator test submits
is held to the Van Rossem limits (16,384 signed bytes, 16.5M memory, 10G CPU)
at submission, and a publication (zero execution units) to the 15,872-byte
publication target, by the suite's submit guard
(`demo/midgard-fault-proofs/tests/support/emulator/van-rossem-submit-guard.ts`,
limits in `demo/midgard-fault-proofs/src/proof-fit/van-rossem.ts`). A
transaction over a limit fails the test that submitted it, naming the axis and
the negative margin. There is nothing to regenerate.
