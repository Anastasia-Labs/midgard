# Cardano capability P2 redeemer-item retention checkpoint — 2026-07-27

Authority:

- `cardano-capability-proof-completion.md`, P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.

This checkpoint replaces the five active whole-redeemer challenged-transition
paths as one atomic production change. It does not enter separate Data
breadth/depth fixtures, script material, later P2 families, P3 or later phases,
deployment, release evidence, limits, or compatibility work.

## Result

No production validation auxiliary now carries a complete
`MidgardRedeemerWitness` or raw Data preimage. The same retained field-8 item
proof is used by:

1. frontier ingestion;
2. discovery match scanning;
3. unused-redeemer scanning;
4. CEK execution-limit selection; and
5. CEK-context Data selection.

An opening control binds the exact item index, field item count, total encoded
length, bounded-item commitment, purpose tag, pointer index, Data payload
offset and length, and execution-memory/step units. The canonical definite
array header and execution-unit tail are accepted only from their exact
authenticated source spans. Each span is at most `132` bytes and therefore
requires one `4,095`-byte chunk proof or, only at a boundary, its immediately
adjacent second proof.

The frontier leaf is derived only as
`redeemer_item_leaf_hash(item_index, item_commitment)`. Data-mode openings
retain the exact `cek_data_traverse_v1` control in the redeemer-item control.
The validation state stores only its hash and accepts each successor only
after exact source-span authentication. The frontier leaf is admitted and
the CEK purpose/map/current-redeemer folds advance only after the traversal
reaches a well-formed terminal control whose final Data summary matches the
retained descriptor.

The obsolete challenged-transition `raw_cbor` scanner and its `9,215`-byte
whole-preimage cap are removed. The undeployed whole-redeemer auxiliary
constructors and codec branches are also removed in place; no compatibility
path remains. Activation behavior is unchanged and remains fail closed.

## Boundary and mutation evidence

The checked `balanced-nested-redeemer` corpus transaction is decoded from its
canonical retained transaction bytes. Its only field-8 item authenticates:

- item index `0` of count `1`;
- purpose tag `0`, pointer index `1`;
- Data length `15,982`;
- terminal Data root
  `26ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20`;
- terminal CBOR length `15,982` and memory `47,924`; and
- every requested source span at no more than `132` bytes.

The TypeScript maximum trace includes a real two-chunk crossing and rejects
when its required adjacent proof is removed. Focused small vectors bind exact
outer header/tail metadata and reject altered purpose, index/count/length,
commitment, source chunk, traversal control, and terminal summary relations.
Aiken independently authenticates the canonical header/tail descriptor,
rejects malformed header/tail/chunk controls, and decodes the exact
TypeScript-generated terminal control. The established maximum nested
redeemer terminal vector continues to agree with TypeScript after the
production switch.

The accepted PlutusV3 validation trace passes the rebuilt fault-proof ABI with
the retained begin/step witnesses. A structural regression assertion rejects
any generated auxiliary containing `redeemer`, `rawCbor`, or `dataCborHex`.
Production source inspection finds no remaining `MidgardRedeemerWitness`
auxiliary use in the validation machine, matching codec, or ABI guard.

## Verification

From `demo/`:

```sh
pnpm --filter @al-ft/midgard-core typecheck
pnpm --filter @al-ft/midgard-validation typecheck
pnpm --filter @al-ft/midgard-fault-proofs typecheck
pnpm --filter @al-ft/midgard-validation test -- \
  --run tests/redeemer-item-proof-v1.test.ts
pnpm --filter @al-ft/midgard-validation test -- \
  --run tests/validation-machine.test.ts
pnpm --filter @al-ft/midgard-fault-proofs test -- \
  --run tests/validation-dispute-submit.test.ts
pnpm --filter @al-ft/midgard-validation build
node midgard-validation/scripts/generate-validation-one-step-aiken-fixture.mjs
pnpm exec eslint \
  midgard-core/src/redeemer-item-proof-v1.ts \
  midgard-validation/src/validation-machine.ts \
  midgard-validation/src/validation-machine-data.ts \
  midgard-validation/tests/redeemer-item-proof-v1.test.ts \
  midgard-validation/tests/validation-machine.test.ts \
  midgard-fault-proofs/src/validation-dispute/submit.ts
```

From `onchain/aiken/`, using the pinned
`aiken v1.1.21+42babe5` binary:

```sh
aiken check --skip-tests
aiken check -m redeemer_item_descriptor_authenticates_header_tail_and_exact_metadata
aiken check -m redeemer_item_rejects_mutated_header_tail_and_chunk_evidence
aiken check -m redeemer_item_terminal_data_summary_agrees_with_typescript
aiken check -m maximum_cardano_nested_redeemer_terminal_matches_typescript
aiken check -m typescript_generated_one_step_boundary_is_authenticated
aiken check -m typescript_generated_canonical_decode_step_is_exact
aiken build
```

The focused retained-item TypeScript tests pass `2/2`; the maximum production
proof trace completes in about `13` seconds. The deterministic validation
machine passes `21/21`, the fault-proof ABI suite passes `5/5`, and the
regenerated one-step fixture contains `73` bounded transitions. All six named
Aiken vectors pass.
The maximum nested redeemer terminal uses `2,190,000` memory and
`880,130,000` CPU; these are diagnostic construction results, not an L1
proof-fit claim.

## Remaining P2 gate

The active whole-redeemer transition blocker is closed. The aggregate Data row
remains `PARTIAL` until distinct constructor/list breadth, map-pair breadth,
and iterative unary-depth fixtures pass through both retained classifications
and matching TypeScript/Aiken terminal folds. Script envelope/program
material, maximum incremental-CBOR evidence, and remaining ordered-field
terminal vectors also remain open. Activation stays fail closed.
