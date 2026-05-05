# T15 Provider Convenience Methods

## Scope

Add Lucid-style provider/query convenience methods to `LucidMidgard`, adapted to
Midgard L2 semantics and Midgard node APIs.

Convenience methods must query Midgard state through `MidgardProvider` and
`MidgardNodeProvider`. They must not read Cardano L1 state, and they must not
introduce Blockfrost, Maestro, or Kupmios adapters.

Compatibility note: `@lucid-evolution/lucid` `0.4.30` exposes convenience
methods such as `utxosAt`, `utxosAtWithUnit`, `utxoByUnit`, `utxosByOutRef`,
`delegationAt`, `awaitTx`, `datumOf`, and `metadataOf`. `lucid-midgard` should
keep the useful read/query ergonomics where Midgard has equivalent semantics and
explicitly omit Cardano-only methods.

## Dependencies

- T04 provider client.
- T06 builder fluent API.
- T10 local validation.
- T11 submit, status, and chain.
- T13 API parity map.
- T14 provider switching and overrides.
- Architecture docs `04`, `07`, and `08`.

## Deliverables

- `LucidMidgard.currentSlot()`.
- Explicit `unixTimeToSlot` decision: unsupported until Midgard provider
  protocol info exposes a native slot/time conversion model. The method must be
  absent from the public type surface in this task.
- `LucidMidgard.utxosAt(address)`.
- `LucidMidgard.utxosAtWithUnit(address, unit)`.
- `LucidMidgard.utxosByOutRef(outRefs, options?)`, with default
  `{ missing: "error" }` strict behavior. Optional `{ missing: "omit" }` may be
  added only if omitted results remain ordered by the original request order.
- Capability-gated `LucidMidgard.utxoByUnit(unit)` only when a Midgard provider
  exposes a Midgard-native unit index.
- `LucidMidgard.awaitTx(txId, options?)` backed by Midgard transaction status.
- `LucidMidgard.txStatus(txId)` or an equivalent direct status helper.
- `LucidMidgard.datumOf(utxo, type?)` for Midgard-native datum data that is
  present inline, supplied as a witness, or fetched through an explicit Midgard
  datum capability.
- Provider capability errors for unsupported convenience queries.
- Deterministic output ordering for batch outref queries.
- Structured errors for missing UTxOs, duplicate outrefs, malformed units,
  unsupported unit indexes, ambiguous `utxoByUnit` results, missing datums,
  datum hash mismatches, unavailable status endpoints, unknown status values,
  and wrong-tx-id provider status responses.
- Documentation showing which Lucid `0.4.30` query methods are supported,
  adapted, or intentionally omitted.
- Explicit omission of:
  - `delegationAt`
  - Cardano reward/delegation queries
  - `metadataOf` while Midgard native auxiliary data is not a usable protocol
    surface
  - any Cardano governance query helpers

## Acceptance Criteria

- `currentSlot()` reads from Midgard provider protocol info or current-slot
  capability.
- `unixTimeToSlot` is absent unless a future Midgard-native slot/time model is
  added to protocol info and covered by tests.
- `utxosAt(address)` delegates to Midgard UTxO lookup and validates returned
  outrefs and output bytes.
- `utxosAt(address)` accepts only address queries. Lucid credential-query forms
  are rejected or absent unless a Midgard-native credential index is introduced.
- `utxosAt(address)` returns deterministic canonical ordering by `TxOutRef`
  (`txHash`, then `outputIndex`) unless the provider exposes a stronger Midgard
  ordering contract documented here.
- `utxosAtWithUnit(address, unit)` returns only UTxOs containing the requested
  unit and preserves deterministic ordering.
- `utxosByOutRef(outRefs)` uses Midgard by-outref lookup and returns results in
  request order or fails according to explicit `options`.
- Duplicate outrefs in `utxosByOutRef` fail before provider calls.
- `utxoByUnit(unit)` never scans unrelated addresses or Cardano state. If the
  provider lacks a Midgard-native unit index, it fails with a capability error.
- `utxoByUnit(unit)` fails distinctly for zero matches and multiple matches.
- `awaitTx` distinguishes durable admission from validation acceptance.
- `awaitTx` has explicit defaults for terminal statuses, timeout, polling
  interval, and abort/cancellation behavior. It must not poll forever by
  default.
- `awaitTx` preserves Midgard reject codes and details when a transaction is
  rejected.
- `awaitTx` rejects unknown status values and wrong-tx-id provider responses as
  structured provider payload errors.
- `awaitTx` does not claim block finality unless a future Midgard provider
  capability exposes finality.
- `datumOf` verifies datum hashes when both hash and bytes are available.
- Convenience methods expose structured diagnostics sufficient for CLI and test
  assertions.
- Unsupported Cardano-only methods are absent from the public type surface, not
  implemented as no-op compatibility stubs.

## Required Tests

- `currentSlot()` parses provider current slot as an integer string.
- `utxosAt` success with valid Midgard output bytes.
- `utxosAt` rejects malformed provider payloads.
- `utxosAt` rejects or omits Lucid credential-query forms at the type/runtime
  boundary.
- `utxosAt` returns deterministic canonical outref ordering.
- `utxosAtWithUnit` filters lovelace and multi-asset outputs correctly.
- `utxosByOutRef` preserves requested order.
- `utxosByOutRef` rejects duplicate requested outrefs.
- `utxosByOutRef` reports missing outrefs according to strict options.
- `utxosByOutRef` default missing behavior is strict error.
- `utxosByOutRef({ missing: "omit" })`, if implemented, preserves request-order
  projection for returned results.
- `utxoByUnit` succeeds with exactly one indexed result.
- `utxoByUnit` fails on unsupported provider capability.
- `utxoByUnit` fails on zero matches.
- `utxoByUnit` fails on multiple matches.
- `awaitTx` succeeds on accepted status.
- `awaitTx` returns structured rejection with node reject code and detail.
- `awaitTx` fails with a capability error when status lookup is unavailable.
- `awaitTx` times out by default instead of polling forever.
- `awaitTx` supports abort/cancellation.
- `awaitTx` rejects unknown status values and wrong-tx-id status payloads.
- `datumOf` decodes inline datum.
- `datumOf` rejects datum hash mismatch.
- Type tests prove `delegationAt` and `metadataOf` are absent unless future
  Midgard-native protocol features explicitly add them.
- Type tests prove `unixTimeToSlot` is absent unless future Midgard protocol
  info explicitly adds a slot/time model.

## Non-Goals

- Do not add Cardano staking, withdrawal, delegation, DRep, governance, or
  committee features.
- Do not add Blockfrost, Maestro, Kupmios, or other Cardano-read provider
  adapters.
- Do not query Cardano L1 datum, metadata, reward, delegation, or governance
  services.
- Do not add block finality guarantees beyond provider-exposed Midgard status.
- Do not hide missing provider capabilities behind empty arrays or silent
  fallbacks.
