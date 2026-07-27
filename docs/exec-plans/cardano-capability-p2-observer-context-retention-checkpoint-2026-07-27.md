# Cardano capability P2 observer-context retention checkpoint — 2026-07-27

Authority:

- `cardano-capability-proof-completion.md`, P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.

This checkpoint removes the remaining active whole-field field-3 consumer in
CEK context construction. It does not enter redeemer Data traversal, breadth
fixtures, script material, later P2 families, P3 or later phases, deployment,
release evidence, limits, or compatibility work.

## Result

CEK stage 5 no longer receives a complete `required_observers` preimage.
Instead, each step receives the established `TransactionFieldChunkWitness`
and authenticates:

- collection field `3`, the exact item count, and the exact reverse index
  `observer_count - observer_items.length - 1`;
- one exact 28-byte item and its bounded-collection membership against
  `required_observers_hash`;
- one exact chunk at index `0` and its bounded-item commitment; and
- strict local reverse ordering, `current_hash < previous_observer`.

The control retains only an authenticated total count, the preceding
28-byte hash, and a `DataSequenceSummaryV1`. Revealing highest index first and
prepending each semantic item reconstructs ascending canonical order without
retaining raw observers.

Cardano context entries prepend the summary of
`constr_data(1, [b_data(hash)]) -> i_data(0)` and finalize as a map. Midgard
entries prepend `b_data(hash)` and finalize as a list. Observer count uses the
same transaction-size-derived collection guardrail as the other ordered
fields: `16,384`, the one-byte-per-item lower encoding floor under the
preserved Cardano transaction envelope. It is not an observer capability cap.
An otherwise authenticated count of `225` is admissible by this verifier.

The empty field is recognized only by exact equality with
`bounded_collection_v1.from_items(3, [])` and finalizes using
`NoAuxiliaryWitness`. Every nonempty collection finishes with a separate
`NoAuxiliaryWitness` only after the retained item-summary length equals the
authenticated count.

The final control relation is self-validating at every later stage:

```text
observer_summary ==
  finalize_cek_observer_items_v1(
    observer_items,
    language_tag == 128,
  )
```

The exact TypeScript/Aiken context-control encoding is now a definite array of
`24` elements. The three new fields are mirrored at the same positions in both
implementations.

## Boundary and mutation evidence

The tracked signed-Cardano boundary fixture is inserted in strict ascending
script-hash order and crosses the unchanged production Cardano-to-Midgard
bridge. TypeScript exhaustively folds its exact `224` committed 28-byte
observers in reverse for both Cardano and Midgard encodings. Both terminal
summaries equal the established semantic codec. The accepted transaction is
`16,338` bytes, leaving `46` bytes of margin. Its `6,722`-byte field uses
`224` item reveals, each with one 28-byte chunk; the largest encoded auxiliary
reveal is `492` bytes.

The exact adjacent `225`-observer/native-script signed transaction is `16,410`
bytes and is rejected by the preserved Cardano `maxTxSize = 16,384` envelope,
not by CEK observer validation. Duplicate, descending, non-28-byte, and
transaction-size-derived guardrail-plus-one collections reject fail closed.

Aiken production-verifier vectors prove:

- the exact tracked field-3 source and commitment accept a valid proof at
  index `223` of `224`, followed by the exact cross-language terminal fold;
- changing the proof and successor count to `225` while retaining the
  committed `224`-item source rejects because the authenticated commitment,
  count, and index relation no longer agree; it is not a count-225 ban;
- the empty field accepts only the exact no-auxiliary finalization; and
- altered successor, item index, item count, field index, item length, chunk
  length, local order, and final summary all reject.

The wrong-summary control retains valid length, CBOR-length, and memory fields
but substitutes another 32-byte root. It therefore proves the exact
summary-to-items relation rather than only a root-shape check.

The deterministic PlutusV3 observer trace now carries one field-3
`TransactionFieldChunkWitness`, followed by its no-auxiliary finalizer, and no
CEK `TransactionFieldPreimageWitness`.

## Verification

From `demo/midgard-validation/`:

```sh
./node_modules/.bin/tsc --noEmit
./node_modules/.bin/vitest run tests/cek-observer-boundary-v1.test.ts
./node_modules/.bin/vitest run \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts
./node_modules/.bin/vitest run tests/retained-da-boundary-v1.test.ts
./node_modules/.bin/vitest run \
  tests/validation-machine.test.ts --testTimeout=15000
```

From `demo/midgard-fault-proofs/`:

```sh
./node_modules/.bin/vitest run \
  tests/cardano-capability-retained-da-v1.test.ts
```

From `demo/`:

```sh
./node_modules/.bin/eslint \
  midgard-validation/src/cek-context.ts \
  midgard-validation/tests/cek-observer-boundary-v1.test.ts \
  midgard-validation/tests/helpers/ordered-collection-boundary-v1.ts
```

The focused CEK test passes `2/2`, the signed observer/native-script boundary
and retained-boundary controls pass `1/1` each, and the strict production
retained-DA corpus passes `2/2`. The deterministic validation-machine file
passes `21/21`.

From `onchain/aiken/`, the local verification below used
`/home/gumbo/.aiken/bin/aiken` at `v1.1.22+39d6b04`. The project declaration
and CI/release compiler gate remain `v1.1.21`; these local results do not
replace the required `v1.1.21` release check.

```sh
aiken check --skip-tests
aiken check \
  -m 'midgard/validation_machine_v1.{cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree}' \
  -e --plain-numbers
aiken check \
  -m 'midgard/script_context_v1.{observer_context_collections_are_not_capped_at_16}' \
  -e --plain-numbers
aiken check \
  -m 'midgard/validation_machine_v1.{cek_context_observer_empty_field_has_exact_no_auxiliary_path}' \
  -e --plain-numbers
aiken check \
  -m 'midgard/validation_machine_v1.{cek_context_observer_rejects_malformed_proofs_order_and_successors}' \
  -e --plain-numbers
aiken check \
  -m 'midgard/validation_machine_v1.{cek_context_observer_complete_summary_relation_is_exact}' \
  -e --plain-numbers
aiken check \
  -m 'midgard/validation_machine_v1.{cross_language_cek_context_control_vectors}' \
  -e --plain-numbers
aiken build
```

Every exact selector above collects and passes exactly one test. The exact
maximum-224 source-authenticated first-item/terminal vector uses `16,056,853`
memory / `8,260,908,361` CPU. It includes the valid first-item reveal, exact
terminal relation, and the mismatched-count rejection. The direct 17-item
no-old-cap control uses
`10,364,842` / `4,775,277,396`; exact-empty, malformed-control, later-stage
exact-summary, and cross-language control vectors use `10,018,461` /
`5,186,438,999`, `60,280,315` / `29,968,466,229`, `9,080,403` /
`3,997,404,332`, and `317,776` / `207,616,560` memory / CPU respectively.
The malformed vector bundles many negative assertions and is diagnostic
construction evidence, not one applied transition. These are not complete
publication or settlement transaction proof-fit claims.

## Remaining P2 gate

The ordered-fields aggregate row remains `PARTIAL`. The exact TypeScript
maximum performs all `224` item folds, while Aiken proves the exact first
reveal and terminal relation against the same source and commitment; a full
applied `224`-transition TypeScript/Aiken lifecycle is not yet recorded. Data
breadth/depth, script envelope/program material, and maximum incremental-CBOR
evidence remain open. Activation stays fail closed.
