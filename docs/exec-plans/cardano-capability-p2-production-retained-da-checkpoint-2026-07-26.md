# Cardano capability P2 production retained-DA checkpoint — 2026-07-26

Authority:

- `cardano-capability-proof-completion.md`, P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.

This checkpoint corrects the retained-DA evidence boundary. The earlier
validation helper SDK-encoded and decoded a synthetic payload with empty
committed roots and no validation traces, then folded directly from decoded
preimages. It did not call production `reconstructDaPayloadV1` and could not
pass its strict count/root requirements.

The correction remains inside `@al-ft/midgard-fault-proofs`, which already
depends on validation/core/SDK and owns `reconstructDaPayloadV1`. No reverse
fault-proofs dependency was added to `@al-ft/midgard-validation`.

## Checked boundary corpus

The validation boundary helper can emit a labeled, test-only corpus row after
deriving the exact canonical Midgard transaction from each established
fixture. The checked corpus contains `12` rows:

- ordered fields `0` through `8`, including coupled fields `3/7`, `4/6`, and
  `5/7`;
- maximum inline-datum blob;
- maximum nested output Value;
- balanced nested datum;
- balanced schema-parallel redeemer; and
- the mixed `16,126`-byte canonical fixture.

Every row binds the complete canonical bytes, domain-separated transaction ID,
and proof commitment. The strict consumer recomputes the ID and commitment
before constructing a payload.

The maximum redeemer corpus row is the collateral-free schema-parallel
fixture. The genuine Cardano N=1 and maximum collateralized transactions still
reject with `E_CONVERSION_UNSUPPORTED_FEATURE` / `collateral_inputs`. Nothing
strips collateral or weakens that policy. The maximum row has neither
withdrawals nor mint.

## Production reconstruction

For each corpus row, the focused fault-proofs helper constructs one
internally consistent payload containing the same transaction as:

- one normal `transactions` source plus same-key
  `transaction_preimages`; and
- one forced `forced_transactions` source retaining
  `operator_validity = TxIsValid` plus same-key
  `forced_transaction_preimages`.

It also constructs:

- real transaction, forced-transaction, transition-trace, event-to-step, and
  validation-trace roots;
- exactly two L1-shaped source events, transition steps, event mappings, and
  validation descriptors;
- exact payload and header counts;
- the exact header hash; and
- the mandatory identity envelope.

The test calls production `reconstructDaPayloadV1` with both the expected
header hash and committed header. Bounded proof work begins only after that
call succeeds. Normal folds consume
`reconstruction.transactions[].fullTransactionCbor`; forced folds consume the
returned `reconstruction.forcedTransactions[].fullTransactionCbor`, which is
the same-key forced preimage already authenticated by reconstruction.

For both classifications and all `12` rows, every generic canonical
field/item chunk proof verifies, no chunk exceeds `4,095` bytes, and the
complete chunk reconstruction reproduces the checked canonical transaction
byte exactly.

This focused production test does not replay each row-specific ordered,
output/Value, or Data semantic trace/finalizer. Those typed paths run in the
`12` source boundary tests that produce the corpus. The evidence composes
through exact canonical-byte identity: regenerating the corpus from those
tests produces the checked JSON fixture byte for byte, and production
reconstruction returns those same bytes before generic bounded reconstruction
begins.

This is P2 retention/reveal evidence only. The two trace records are strict
reconstruction scaffolding; the test does not apply ledger state, invoke
resolvers, or claim P3/P4 semantics.

## Fail-closed evidence

The same focused suite deliberately changes:

- the committed transaction root while preserving an internally matching
  header hash, which rejects with `rootMismatch`;
- the declared validation-trace count, which rejects with `countMismatch`;
  and
- the forced preimage to a different valid canonical corpus transaction under
  the committed forced-order key, which rejects with `malformedPayload` before
  authenticated forced bytes can be returned.

No mismatch is bypassed or normalized away.

## Verification

From `demo/`:

```sh
MIDGARD_BOUNDARY_CORPUS_JSONL=/tmp/midgard-cardano-p2-boundary-corpus-v1.jsonl \
pnpm --filter @al-ft/midgard-validation exec vitest run \
  tests/ordered-collection-boundary-v1.test.ts \
  tests/ordered-collection-signer-witness-boundary-v1.test.ts \
  tests/ordered-collection-spend-inputs-boundary-v1.test.ts \
  tests/ordered-collection-reference-inputs-boundary-v1.test.ts \
  tests/ordered-collection-observer-native-script-boundary-v1.test.ts \
  tests/ordered-collection-mint-boundary-v1.test.ts \
  tests/ordered-collection-redeemer-boundary-v1.test.ts \
  tests/blob-chunk-boundary-v1.test.ts \
  tests/nested-value-boundary-v1.test.ts \
  tests/nested-data-boundary-v1.test.ts \
  tests/nested-redeemer-data-boundary-v1.test.ts \
  tests/retained-da-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true
```

Result: **PASS** (`12` files / `12` tests; `79.15 s`). Rebuilding the JSON
fixture from the emitted rows is byte-identical to the checked corpus
(`sha256 68ff4ab46ede05f6194b84e9f82f1abdb9f0436339356d8baa5ac897a2492ec3`).

Generate the checked JSON from that fresh JSONL file with:

```sh
node midgard-fault-proofs/tests/fixtures/build-cardano-capability-p2-boundary-corpus-v1.mjs \
  /tmp/midgard-cardano-p2-boundary-corpus-v1.jsonl \
  midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json
```

```sh
pnpm --filter @al-ft/midgard-fault-proofs run typecheck
pnpm --filter @al-ft/midgard-fault-proofs exec vitest run \
  tests/cardano-capability-retained-da-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true
```

Result: **PASS** (typecheck; `1` file / `2` tests; `16.19 s`).

## Scope stop

This checkpoint stops before script-envelope/program-material work, another
P2 family, P3/P4/P5/P6, Docker/live deployment, release-digest construction,
limits, compatibility, or unrelated cleanup. Canonical V1 activation remains
fail closed because other P2 matrix rows remain partial.
