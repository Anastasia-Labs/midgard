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
entries prepend `b_data(hash)` and finalize as a list. The established
semantic maximum is `16`. An authenticated count of `17` fails closed.

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

TypeScript exhaustively folds `16` ordered 28-byte hashes in reverse for both
Cardano and Midgard encodings. Both terminal summaries equal the established
semantic codec. Adjacent `17`, a duplicate, descending order, and a non-28-byte
hash reject.

Aiken production-verifier vectors prove:

- a valid field-3 proof at index `15` of `16`, followed by the exact complete
  terminal fold;
- an otherwise valid field-3 proof at index `16` of `17` rejects at the
  semantic bound;
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

From `demo/`:

```sh
pnpm --filter @al-ft/midgard-validation typecheck
pnpm --filter @al-ft/midgard-validation test -- \
  --run tests/cek-observer-boundary-v1.test.ts
pnpm --filter @al-ft/midgard-validation test -- \
  --run tests/validation-machine.test.ts
pnpm exec eslint \
  midgard-validation/src/cek-context.ts \
  midgard-validation/src/validation-machine.ts \
  midgard-validation/tests/cek-observer-boundary-v1.test.ts \
  midgard-validation/tests/validation-machine.test.ts
```

The boundary test passes `3/3`; the deterministic validation-machine file
passes `21/21`.

From `onchain/aiken/`, using the pinned
`aiken v1.1.21+42babe5` binary:

```sh
aiken check --skip-tests
aiken check -m cek_context_observer_maximum_16_replays_and_finalizes -e
aiken check \
  -m cek_context_observer_adjacent_17_fails_closed \
  -m cek_context_observer_empty_field_has_exact_no_auxiliary_path \
  -m cek_context_observer_rejects_malformed_proofs_order_and_successors
aiken check -m cek_context_observer_complete_summary_relation_is_exact -e
aiken check -m cross_language_cek_context_control_vectors -e
aiken build
```

All six named vectors and the build pass. The maximum-16 vector uses
`66,738,627` memory / `30,370,522,289` CPU. The adjacent-17, exact-empty, and
malformed-control vectors use `31,322,692` / `13,850,424,020`,
`10,260,761` / `5,225,206,999`, and `61,565,447` / `30,175,734,901`
memory / CPU respectively. The later-stage exact-summary relation uses
`9,267,779` memory / `4,034,245,204` CPU. These are diagnostic construction
results, not L1 proof-fit claims.

## Remaining P2 gate

The ordered-fields bounded-reveal column is closed, but the aggregate row
remains `PARTIAL` until the remaining maximum-specific terminal vectors pass.
The next dependency-ordered blocker is the active whole-`raw_cbor` redeemer
scanner and its obsolete `9,215`-byte cap. Data breadth/depth, script
envelope/program material, and maximum incremental-CBOR evidence remain open.
Activation stays fail closed.
