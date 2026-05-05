# T14 Provider Switching and Overrides

## Scope

Add Lucid-style provider switching and explicit UTxO override ergonomics while
preserving Midgard provider semantics.

`lucid-midgard` must continue to use `MidgardProvider` as the provider
interface and `MidgardNodeProvider` as the concrete HTTP provider. Provider
switching must target Midgard L2 node APIs only. Overrides must be explicit,
diagnosable, and scoped to local builder behavior; they must never be silently
treated as authoritative node state.

Compatibility note: `@lucid-evolution/lucid` `0.4.30` exposes
`switchProvider(provider)` and `overrideUTxOs(utxos)` over Cardano providers and
Cardano UTxOs. `lucid-midgard` may keep similar names, but the accepted provider
and UTxO types are Midgard-native.

## Dependencies

- T04 provider client.
- T05 wallet and signers.
- T06 builder fluent API.
- T08 balancing and fees.
- T10 local validation.
- T13 API parity map.
- Architecture docs `03`, `04`, `05`, and `08`.

## Deliverables

- `LucidMidgard.config()` returning an immutable configuration snapshot.
- `LucidMidgard.switchProvider(provider, options?)`, where `options` is limited
  to explicit protocol-info fallback/profile controls needed for provider
  validation.
- Atomic provider validation before a switch is committed.
- Provider generation and snapshot tracking so existing builders and in-flight
  completions cannot observe half-switched state. Builders capture the provider
  snapshot at `newTx()` time and continue using that snapshot; they do not fail
  merely because the instance provider later changes.
- Provider-derived configuration refresh after switching:
  - network
  - `apiVersion`
  - `midgardNativeTxVersion`
  - current slot
  - fee parameters
  - submission limits
  - validation strictness profile
- Cache invalidation for provider-derived protocol data.
- Wallet/network revalidation after provider switch.
- `overrideUTxOs(utxos)` for selected-wallet input overrides.
- `clearUTxOOverrides()` for removing instance-level overrides.
- Override generation and snapshot tracking. Builders capture override state at
  `newTx()` time; clearing or replacing overrides cannot change an in-flight
  completion.
- Completion-level preset wallet inputs, adapted from Lucid `0.4.30`
  `presetWalletInputs`, with Midgard-native diagnostics.
- Structured diagnostics showing whether wallet inputs came from:
  - provider state
  - instance-level override
  - completion-level preset inputs
- Structured errors for incompatible providers, unavailable protocol info,
  malformed override UTxOs, duplicate override outrefs, and provider capability
  mismatch.
- Ownership/spendability validation for wallet UTxO overrides. Override UTxOs
  must match the selected wallet payment credential unless they are passed
  through a separate local-validation pre-state option that is not used for coin
  selection.
- Documentation explaining that overrides are local construction inputs, not a
  way to rewrite Midgard node truth.

## Acceptance Criteria

- `switchProvider` accepts only `MidgardProvider` implementations.
- `switchProvider` rejects Cardano provider objects, including Blockfrost,
  Maestro, and Kupmios providers.
- Provider switching is atomic: failed validation leaves the previous provider
  and configuration unchanged.
- New builders created after a successful switch use the new provider snapshot.
- Builders created before a switch keep their original provider snapshot; they
  must not silently mix old and new provider data.
- In-flight `complete()` calls are snapshot-safe.
- A selected wallet is revalidated against the switched provider network before
  new transactions can complete.
- Protocol info fallback values remain explicit diagnostics and are never
  reported as node-derived production facts.
- UTxO overrides validate outrefs, output bytes, assets, address network, and
  duplicate keys before use.
- UTxO overrides affect wallet input selection only unless the caller explicitly
  supplies them as local validation pre-state.
- Local validation reports identify override-derived pre-state as
  non-authoritative.
- Completion-level preset inputs do not duplicate inputs already supplied to
  `collectFrom`.
- Instance-level overrides and completion-level preset inputs use deterministic
  snapshot semantics and cannot be affected by later override mutation.
- No override path may hide missing node state, double-spend rejection, tx-id
  conflict, or validation failure.

## Required Tests

- Successful switch between two mock `MidgardProvider` instances.
- Failed switch on network mismatch preserves the original provider.
- Failed switch on native transaction version mismatch preserves the original
  provider.
- Failed switch when protocol info is unavailable without explicit fallback.
- Builder created before switch does not mix old and new provider data.
- In-flight completion remains deterministic across a concurrent provider
  switch.
- Builder created before override changes keeps its original override snapshot.
- In-flight completion remains deterministic across concurrent override changes.
- Provider-derived fee and submission-limit caches refresh after switching.
- Selected wallet network mismatch after switch fails before completion.
- `overrideUTxOs` supplies wallet inputs for coin selection.
- Clearing overrides returns wallet input lookup to the provider.
- Duplicate override outrefs are rejected.
- Malformed override output bytes are rejected.
- Completion preset inputs do not duplicate collected inputs.
- Override-derived local validation pre-state is marked in diagnostics.
- Cardano provider-shaped objects are rejected by type tests or runtime
  validation.
- `Koios`, `Emulator`, and wildcard Lucid provider export shaped objects are
  rejected by type tests or runtime validation.
- Wallet UTxO override that does not match the selected wallet payment
  credential is rejected for coin selection.

## Non-Goals

- Do not add Blockfrost, Maestro, Kupmios, or other Cardano-read provider
  adapters.
- Do not add Cardano staking, withdrawal, delegation, DRep, governance, or
  committee features.
- Do not add a direct database provider in this task.
- Do not mutate existing completed or signed transactions when the provider
  changes.
- Do not silently rebuild transactions during provider switch, retry, or
  override changes.
- Do not add compatibility modes for prior `demo/midgard-node` behavior.
