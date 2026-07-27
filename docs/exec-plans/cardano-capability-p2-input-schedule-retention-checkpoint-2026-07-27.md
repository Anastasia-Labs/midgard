# Cardano capability P2 input-schedule retention checkpoint — 2026-07-27

Authority:

- `cardano-capability-proof-completion.md`, P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.

This checkpoint replaces the two active spend/reference-input whole-field
recomputations. It does not enter observer replay, retained-DA expansion,
script material, P3 or later phases, deployment, limits, or compatibility
work.

## Result

InputSets already authenticates every field-0 and field-1 item, orders their
union, rejects cross-field duplicates, and commits a linked resolution
schedule. ResolveInputs now retains that exact original head as immutable
`resolution_schedule_hash` while consuming only
`remaining_schedule_hash`. The original head is carried unchanged through
ScriptSources and NativeScripts, then seeds ValueAndMint replay.

The ScriptSources stage-two and ValueAndMint stage-zero transitions now use
`NoAuxiliaryWitness`. Neither transition decodes or authenticates a complete
pair of transaction-field preimages. Their explicit state transitions remain,
so the established accepted trace still contains exactly `22` ScriptSources
states and `8` ValueAndMint states.

ScriptSources terminal replay is accepted only when:

- the remaining schedule is empty;
- `replay_cursor == resolved_input_count`; and
- `replay_accumulator == resolved_inputs_accumulator`.

The fixed control encodings are mirrored across TypeScript and Aiken:

| Control | Exact base array length | Immutable head index |
| --- | ---: | ---: |
| ResolveInputs | `11` | `10` |
| ScriptSources | `30` (`31` with extension) | `29` |
| NativeScripts | `26` | `25` |
| ValueAndMint | `12` | nested NativeScripts control index `25` |

The obsolete pair-preimage witness constructor is removed from the
pre-activation V1 schema in both TypeScript and Aiken. All later auxiliary
tags shift together, and the resolver parser is updated to the same exact
constructor layout; no compatibility branch is introduced.

## Maximum agreement

The checked Cardano boundary corpus supplies:

- `maximum-spend-inputs`: `434` spend inputs and no reference inputs;
- `maximum-reference-inputs`: one spend input and `433` reference inputs.

TypeScript exhaustively reconstructs, orders, and folds all `434` entries for
each fixture. Aiken receives only the final bounded replay item plus five
frontier peaks and agrees on:

- the final linked-schedule node;
- the resolved-input accumulator; and
- the `434`-item resolved-context frontier commitment.

Positive mixed and empty-reference controls pass. Cross-field duplicates,
altered original head, altered source kind/key/tail, premature terminal,
terminal count mismatch, and terminal accumulator mismatch reject.

## Verification

From `demo/midgard-validation/`:

```sh
./node_modules/.bin/tsc --noEmit

./node_modules/.bin/vitest run \
  tests/input-resolution-schedule-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true --reporter=verbose

./node_modules/.bin/vitest run \
  tests/validation-machine.test.ts \
  -t 'replays an accepted transaction through bounded field-reveal instructions' \
  --pool=forks --poolOptions.forks.singleFork=true --reporter=verbose
```

The boundary test passes twice byte-for-byte (`2` tests per run), and the
accepted deterministic replay passes with exact phase counts, witness
positions, and no pair-preimage auxiliary.

From `onchain/aiken/`:

```sh
aiken build
aiken check -m maximum_spend_input_schedule_terminal_matches_typescript
aiken check -m maximum_reference_input_schedule_terminal_matches_typescript
aiken check -m input_sets_accepts_bounded_disjoint_spend_and_reference_inputs
aiken check -m input_sets_proves_spend_reference_overlap_unilaterally
aiken check -m script_sources_binds_resolved_input_replay_schedule
```

All five focused tests and the full build pass. The maximum terminal tests use
`53,050,643` memory / `21,790,185,248` CPU and `52,995,419` memory /
`21,765,271,610` CPU respectively. These are diagnostic cross-language
construction results, not L1 proof-fit claims.

`aiken fmt --check` is not credited as green evidence for these two existing
large modules under Aiken `v1.1.22+39d6b04`: formatting the multiline terminal
vector bindings into the formatter's preferred shape makes the same compiler
abort in `tipo/exhaustive.rs:578` before tests run. The checkpoint preserves
the source form proven by `aiken build` and the named checks; `git diff
--check`, TypeScript typecheck, and ESLint are green.

## Remaining P2 gate

The ordered-fields row remains `PARTIAL`. The active whole-field observer
decoder in CEK context construction is the next dependency-ordered blocker.
The whole-raw redeemer scanner, breadth/depth Data fixtures, script
envelope/program material, maximum incremental CBOR scans, and remaining
maximum-specific ordered-field terminal vectors also remain open. Activation
stays fail closed.
