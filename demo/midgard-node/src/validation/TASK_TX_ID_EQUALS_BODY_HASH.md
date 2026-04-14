# Task: Pre-Launch Body-Hash Transaction IDs

## Objective

Before launch, make the Midgard ledger transaction id equal to the Midgard
native transaction body hash.

This is a fresh-start cutover:

- delete existing state
- start from clean state
- do not implement migration or compatibility handling

## Baseline

The canonical launch baseline is:

- `protocolVersion = 1n`
- `tx.version = 1n`
- `ledgerTxId = compact.transactionBodyHash`

There is one canonical ruleset only.

Do not implement:

- old tx-id support
- alias lookups
- dual-ID behavior
- mixed-version runtime logic
- migration tooling

## Canonical Semantics

### Identity

- `ledgerTxId = compact.transactionBodyHash`
- `envelopeHash = blake2b256(encodeMidgardNativeTxCompact(tx.compact))`

`envelopeHash` is diagnostic only.

### Signatures

- pubkey witnesses sign `compact.transactionBodyHash`
- no code path signs or verifies against the full tx id

### Outrefs

- produced outrefs use the canonical ledger tx id

### Conflicts

If two different payloads share one body-hash tx id:

- same tx id plus same bytes: idempotent
- same tx id plus different bytes: reject with a stable conflict code

## Implementation

### 1. Collapse Versioning

- make `1n` the only supported native tx version
- remove the extra native tx version branch
- fold current `v2`-gated features, including datum-witness handling, into the
  canonical `v1` semantics
- make `1n` the only supported protocol version used by this node

### 2. Replace Tx-ID Semantics

- redefine the canonical ledger tx id to equal `transactionBodyHash`
- keep `envelopeHash` available only as an explicit diagnostic value
- update tx-id helpers and all call sites to use the canonical semantics

### 3. Submission And Validation

- `/submit` derives and returns the canonical ledger tx id
- converted Cardano ingress, if supported, emits canonical Midgard txs with
  `tx.version = 1n`
- Phase A compares queued `tx_id` with the recomputed body-hash tx id
- witness verification uses the body hash

### 4. Storage, Replay, And Trie

- tx tables remain keyed by the canonical ledger tx id
- insertion paths reject same-id/different-payload conflicts explicitly
- immutable validation compares stored primary key with recomputed body-hash tx
  id
- replay, block processing, and trie derivation use canonical body-hash tx ids

### 5. Builders And APIs

- all node-built Midgard-native txs emit `tx.version = 1n`
- `/tx-status` accepts only the canonical ledger tx id
- no `id_kind`, `auto`, alias, or compatibility lookup mode exists

## Rollout Rule

Delete all existing node state before using this cutover.

That includes any old local SQL state, mempool state, block state, trie state,
and generated fixtures that encoded the previous tx-id rule.

## Detailed Task List

- [ ] Set the canonical launch protocol version to `1n`.
- [ ] Set the canonical launch native tx version to `1n`.
- [ ] Remove support for the extra native tx version branch.
- [ ] Fold current `v2`-gated features into canonical `v1` semantics.
- [ ] Redefine canonical ledger tx id to equal `transactionBodyHash`.
- [ ] Update tx-id helpers and all call sites to the canonical semantics.
- [ ] Make Phase A tx-id integrity checks canonical to body-hash tx ids.
- [ ] Make produced outref construction canonical to body-hash tx ids.
- [ ] Make immutable validation canonical to body-hash tx ids.
- [ ] Make mempool, block, replay, and trie utilities canonical to body-hash tx
      ids.
- [ ] Add same-id/different-payload conflict detection to tx-table insertion
      paths.
- [ ] Update submit-path normalization and API responses to expose `ledgerTxId`
      and `txVersion`.
- [ ] Remove alternate-identifier lookup paths and compatibility handling.
- [ ] Update local builder flows to emit canonical `tx.version = 1n`.
- [ ] Update docs and tests for the single-version body-hash baseline.

## Test Plan

### Codec

- tx id equals `transactionBodyHash`
- `envelopeHash` remains distinct from `ledgerTxId`
- encode/decode round-trip preserves `tx.version = 1n`
- any non-`1n` tx version is rejected
- canonical `v1` supports the required launch feature set, including datum
  witnesses

### Validation

- Phase A accepts valid txs whose witnesses sign the body hash
- Phase A rejects mismatched queued tx ids
- converted Cardano ingress, if supported, emits canonical `tx.version = 1n`

### Storage And Replay

- identical payload reinsertion is idempotent
- different payload under the same tx id is rejected explicitly
- immutable validation rejects rows whose key does not match the body-hash tx id
- replay and trie derivation remain deterministic under the canonical tx-id rule

### End To End

- node-built transfer submits and resolves `/tx-status` under the body-hash rule
- public tx APIs accept only canonical ledger tx ids
- alternate-identifier lookup attempts are rejected deterministically

## Documentation

Document clearly:

- what `/submit` returns
- what `/tx-status` expects
- that the canonical tx id is the body hash
- that `envelopeHash` is diagnostic only
- that the only supported protocol version is `1n`
- that the only supported native tx version is `1n`
- that existing state must be deleted before rollout

## Acceptance Criteria

- the canonical Midgard ledger tx id equals `transactionBodyHash`
- signatures are validated against the body hash
- produced outrefs use the body-hash tx id
- only `protocolVersion = 1n` and `tx.version = 1n` are supported
- no compatibility lookup path exists
- the body-hash ruleset is enforced uniformly across submit, validation,
  storage, replay, trie, and APIs
