# API Parity Map

This map compares `@al-ft/lucid-midgard` with
`@lucid-evolution/lucid@0.4.30`. The audited Lucid source is the published
`dist/index.d.ts` type surface in `@lucid-evolution/lucid` `0.4.30`.

Lucid is an ergonomics reference only. Midgard-native transaction bytes,
Midgard body hashes, Midgard provider state, durable admission semantics, and
Phase A/Phase B validation boundaries are authoritative.

## Status Values

- `same`: implemented with the same public name and Midgard semantics.
- `adapted`: implemented or planned with the same ergonomic role but different
  Midgard semantics.
- `planned`: covered by a checked-in follow-up task.
- `midgard-only`: Midgard-specific surface not present in Lucid.
- `unsupported`: intentionally absent because Midgard does not support the
  protocol feature or because the Lucid surface is Cardano-domain only.

## Top-Level Lucid Surface

| Lucid entry point | Midgard status | Decision |
| --- | --- | --- |
| `config` | adapted | Returns immutable Midgard provider/configuration snapshots. |
| `wallet` | same | Returns selected `MidgardWallet`; signs Midgard body hashes. |
| `walletAddress` | midgard-only | Convenience wrapper for selected Midgard wallet address. |
| `overrideUTxOs` | adapted | Selected-wallet Midgard UTxO overrides with snapshot semantics. |
| `clearUTxOOverrides` | midgard-only | Removes selected-wallet override inputs for subsequent builders. |
| `switchProvider` | adapted | Atomic `MidgardProvider` switching after protocol-info validation. |
| `newTx` | same | Creates a Midgard-native builder. |
| `fromTx` | adapted | Imports canonical Midgard native full tx bytes/objects only; no Cardano fallback. |
| `selectWallet.fromSeed` | adapted | Supported; seed wallet signs Midgard body hashes. |
| `selectWallet.fromPrivateKey` | adapted | Supported with explicit address binding. |
| `selectWallet.fromAPI` | unsupported | Cardano browser APIs do not prove Midgard body-hash signing. |
| `selectWallet.fromAddress` | adapted | Read-only Midgard wallet construction with optional explicit UTxO overrides. |
| `selectWallet.fromExternalSigner` | midgard-only | Supported for signers that prove Midgard body-hash witness identity. |
| `currentSlot` | adapted | Delegates to Midgard protocol info/provider current-slot capability. |
| `unixTimeToSlot` | unsupported | Absent until Midgard protocol info exposes a native slot/time model. |
| `utxosAt` | adapted | Address-only Midgard UTxO query with provider payload validation. |
| `utxosAtWithUnit` | adapted | Midgard UTxO filtering by canonical asset unit. |
| `utxoByUnit` | adapted | Capability-gated Midgard-native unit-index query. |
| `utxosByOutRef` | adapted | Deterministic strict by-outref query with optional missing omission. |
| `delegationAt` | unsupported | Midgard has no staking/delegation surface. |
| `awaitTx` | same | Existing Midgard status polling; T15/T22 harden ergonomics. |
| `awaitTxSafe` | midgard-only | Safe-result wrapper for direct Midgard transaction status polling. |
| `awaitTxProgram` | midgard-only | Lazy `Effect` wrapper for direct Midgard transaction status polling. |
| `txStatus` | midgard-only | Direct Midgard transaction status lookup with tx-id payload validation. |
| `txStatusSafe` | midgard-only | Safe-result wrapper for direct Midgard transaction status lookup. |
| `txStatusProgram` | midgard-only | Lazy `Effect` wrapper for direct Midgard transaction status lookup. |
| `datumOf` | adapted | Extracts inline Midgard datums and verifies optional datum hashes. |
| `metadataOf` | unsupported | Midgard native auxiliary metadata is not a supported user surface. |

## Builder Surface

| Lucid builder entry point | Midgard status | Decision |
| --- | --- | --- |
| `readFrom` | same | Reads Midgard reference UTxOs. |
| `collectFrom` | same | Collects Midgard spend UTxOs and optional spend redeemer. |
| `pay.ToAddress` | same | Creates ordinary Midgard output. |
| `pay.ToAddressWithData` | adapted | Use `pay.ToAddress(..., { datum })` or `pay.ToContract`. |
| `pay.ToContract` | same | Creates Midgard output with datum. |
| `pay.ToProtectedAddress` | midgard-only | Creates protected Midgard output. |
| `addSigner` | same | Adds required Midgard address witness key hash. |
| `addSignerKey` | same | Alias to `addSigner` for explicit key hashes. |
| `registerStake` | unsupported | Midgard has no staking. |
| `deRegisterStake` | unsupported | Midgard has no staking. |
| `withdraw` | unsupported | Midgard observers are not Cardano withdrawals. |
| `register.Stake` | unsupported | Midgard has no staking certificates. |
| `register.DRep` | unsupported | Midgard has no governance certificates. |
| `deregister.Stake` | unsupported | Midgard has no staking certificates. |
| `deregister.DRep` | unsupported | Midgard has no governance certificates. |
| `mintAssets` | same | Mints Midgard native multi-assets by policy. |
| `validFrom` | same | Sets Midgard validity lower bound. |
| `validTo` | same | Sets Midgard validity upper bound. |
| `delegateTo` | unsupported | Midgard has no delegation. |
| `delegate.*` | unsupported | Midgard has no delegation or vote delegation. |
| `registerAndDelegate.*` | unsupported | Midgard has no staking/delegation certificates. |
| `updateDRep` | unsupported | Midgard has no DRep governance surface. |
| `authCommitteeHot` | unsupported | Midgard has no committee certificate surface. |
| `resignCommitteeHot` | unsupported | Midgard has no committee certificate surface. |
| `attachMetadata` | unsupported | Midgard native auxiliary metadata is not a supported user surface. |
| `attach.Script` | same | Attaches Midgard-supported script source. |
| `attach.NativeScript` | midgard-only | Attaches Cardano-native script bytes for Midgard native witness encoding. |
| `attach.Datum` | midgard-only | Adds Midgard datum witness material for script finalization. |
| `attach.SpendingValidator` | adapted | Typed PlutusV3/MidgardV1 validator helper over `attach.Script`. |
| `attach.MintingPolicy` | adapted | Typed PlutusV3/MidgardV1 policy helper over `attach.Script`. |
| `attach.ObserverValidator` | midgard-only | First-class Midgard observer validator attachment. |
| `attach.ReferenceScriptMetadata` | midgard-only | Supplies explicit trusted language/hash metadata for reference scripts. |
| `attachObserverScript` | midgard-only | Convenience alias for `attach.ObserverValidator`. |
| `attach.CertificateValidator` | unsupported | Midgard has no certificate validators. |
| `attach.WithdrawalValidator` | unsupported | Midgard observers are not withdrawals. |
| `attach.VoteValidator` | unsupported | Midgard has no vote validators. |
| `attach.ProposeValidator` | unsupported | Midgard has no proposal validators. |
| `observe` | midgard-only | Adds Midgard observer execution intent. |
| `receiveRedeemer` | midgard-only | Adds Midgard protected-output receive redeemer intent. |
| `compose` | adapted | Deterministically composes compatible Midgard builder fragments. |
| `setMinFee` | adapted | Sets a Midgard fee floor while preserving provider fee policy convergence. |
| `mint` | midgard-only | Convenience wrapper over `mintAssets` for grouped Midgard mint intents. |
| `complete` | same | Completes Midgard native tx bytes. |
| `completeSafe` | adapted | Returns `MidgardResult<CompleteTx, LucidMidgardError>` for expected completion failures. |
| `completeProgram` | adapted | Lazy `Effect` wrapper around Midgard-native completion. |
| `chain` | adapted | Completes and returns optimistic local Midgard outputs plus updated wallet UTxOs. |
| `chainSafe` | adapted | Safe-result wrapper for local Midgard output chaining. |
| `chainProgram` | adapted | Lazy `Effect` wrapper for local Midgard output chaining. |
| `config` | adapted | Returns the immutable Midgard config snapshot captured by the builder. |
| `rawConfig` | adapted | Returns the validated Midgard protocol-info snapshot captured by the builder. |
| `lucidConfig` | unsupported | Lucid CML transaction-builder config is Cardano-domain. |
| `getPrograms` | unsupported | Lucid Effect program internals are not exposed as Midgard builder API. |
| `debugSnapshot` | midgard-only | Immutable builder-state diagnostic snapshot. |
| `snapshot` | midgard-only | Alias for immutable builder-state diagnostic snapshot. |

## Signing And Submission Lifecycle

| Lucid entry point | Midgard status | Decision |
| --- | --- | --- |
| `sign.withWallet` | adapted | Current sign builder path produces detached Midgard witness bundles through `.partial()`. |
| `sign.withPrivateKey` | adapted | Current sign builder path produces detached Midgard witness bundles through `.partial()`. |
| `sign.withExternalSigner` | midgard-only | External body-hash signer path for detached Midgard witness bundles. |
| `sign.withWitness` | midgard-only | Manual canonical vkey witness attachment for detached bundles. |
| `sign.withWitnesses` | midgard-only | Manual canonical vkey witness attachment for detached bundles. |
| `partialSign.withWallet` | adapted | Detached Midgard witness bundles through `CompleteTx.sign`/`partialSign`. |
| `partialSign.withWalletEffect` | adapted | Lazy `Effect` wrapper for detached witness bundles. |
| `partialSign.withWalletProgram` | midgard-only | Lazy `Effect` wrapper alias for detached witness bundles. |
| `partialSign.withWalletSafe` | adapted | Safe-result wrapper for detached witness bundles. |
| `partialSign.withPrivateKey` | adapted | Detached Midgard witness bundles. |
| `partialSign.withPrivateKeyEffect` | adapted | Lazy `Effect` wrapper for detached witness bundles. |
| `partialSign.withPrivateKeyProgram` | midgard-only | Lazy `Effect` wrapper alias for detached witness bundles. |
| `partialSign.withPrivateKeySafe` | adapted | Safe-result wrapper for detached witness bundles. |
| `partialSign.withExternalSigner` | midgard-only | Detached bundles from an identity-bound external body-hash signer. |
| `partialSign.withExternalSignerEffect` | midgard-only | Lazy `Effect` wrapper for external body-hash signer bundles. |
| `partialSign.withExternalSignerProgram` | midgard-only | Lazy `Effect` wrapper alias for external body-hash signer bundles. |
| `partialSign.withExternalSignerSafe` | midgard-only | Safe-result wrapper for external body-hash signer bundles. |
| `partialSign.withWitness` | midgard-only | Detached bundle from a supplied canonical vkey witness. |
| `partialSign.withWitnesses` | midgard-only | Detached bundle from supplied canonical vkey witnesses. |
| `assemble` | adapted | Assembles Midgard partial witness bundles, returning `CompleteTx` only when witnesses are complete. |
| `TxSignBuilder.complete` | adapted | Completes the Midgard partial-sign builder only when expected witnesses are complete. |
| `TxSignBuilder.completeSafe` | adapted | Safe-result wrapper for sign-builder completion. |
| `TxSignBuilder.completeProgram` | adapted | Lazy `Effect` wrapper for sign-builder completion. |
| `submit` | adapted | Existing submit sends witness-complete Midgard native bytes from `CompleteTx`. |
| `submitSafe` | adapted | Safe-result wrapper for Midgard durable admission. |
| `submitProgram` | adapted | Lazy `Effect` wrapper for Midgard durable admission. |
| `toCBOR` | adapted | Exposes Midgard native full transaction CBOR hex only. |
| `toTransaction` | unsupported | Would imply Cardano `CML.Transaction`; use Midgard native tx accessors. |
| `toJSON` | adapted | Exposes JSON-safe Midgard tx id, tx CBOR hex, metadata, and admission where present. |
| `toHash` | adapted | Returns Midgard tx id/body hash hex. |
| `CompleteTx.tx` | midgard-only | Current accessor for decoded Midgard native full tx. |
| `CompleteTx.txCbor` | midgard-only | Current accessor for Midgard native full tx bytes. |
| `CompleteTx.txHex` | midgard-only | Current accessor for Midgard native full tx hex. |
| `CompleteTx.txId` | midgard-only | Current accessor for Midgard tx id bytes. |
| `CompleteTx.txIdHex` | midgard-only | Current accessor for Midgard tx id hex. |
| `CompleteTx.metadata` | midgard-only | Current immutable completion metadata snapshot. |
| `CompleteTx.producedOutputs` | midgard-only | Derives local `MidgardUtxo` outputs from exact native output CBOR bytes. |
| `CompleteTx.producedOutput` | midgard-only | Fetches one locally produced output by native body output index. |
| `CompleteTx.sign` | adapted | Shortcut signs the Midgard body hash and returns the signed `CompleteTx` compatibility shape. |
| `CompleteTx.partialSign` | adapted | Detached partial witness bundle API; alias of the partial-capable sign surface. |
| `CompleteTx.assemble` | adapted | Assembles partial witness bundles against this Midgard native tx. |
| `CompleteTx.toPartialWitnessBundle` | midgard-only | Exports accumulated address witnesses as a JSON-safe bundle. |
| `CompleteTx.toPartialWitnessBundleCbor` | midgard-only | Exports accumulated address witnesses as canonical bundle CBOR. |
| `CompleteTx.validate` | midgard-only | Runs shared local preflight validation and returns reject-preserving reports. |
| `CompleteTx.validateSafe` | midgard-only | Safe-result wrapper for shared local preflight validation. |
| `CompleteTx.validateProgram` | midgard-only | Lazy `Effect` wrapper for shared local preflight validation. |
| `CompleteTx.submit` | adapted | Guarded submit path for a witness-complete signed `CompleteTx`. |
| `CompleteTx.submitSafe` | adapted | Safe-result submit wrapper with durable admission payload validation. |
| `CompleteTx.submitProgram` | adapted | Lazy `Effect` submit wrapper. |
| `CompleteTx.status` | midgard-only | Current direct status helper with tx-id payload validation. |
| `CompleteTx.statusSafe` | midgard-only | Safe-result wrapper for current direct status helper. |
| `CompleteTx.statusProgram` | midgard-only | Lazy `Effect` wrapper for current direct status helper. |
| `CompleteTx.awaitStatus` | midgard-only | Current polling helper with Midgard status semantics. |
| `CompleteTx.awaitStatusSafe` | midgard-only | Safe-result wrapper for current polling helper. |
| `CompleteTx.awaitStatusProgram` | midgard-only | Lazy `Effect` wrapper for current polling helper. |
| `PartiallySignedTx.tx` | midgard-only | Decoded Midgard native full tx with an incomplete or unprovable address witness set. |
| `PartiallySignedTx.txCbor` | midgard-only | Midgard native full tx bytes for continued partial collection. |
| `PartiallySignedTx.txHex` | midgard-only | Midgard native full tx hex for explicit partial import/export. |
| `PartiallySignedTx.txId` | midgard-only | Midgard tx id/body hash bytes. |
| `PartiallySignedTx.txIdHex` | midgard-only | Midgard tx id/body hash hex. |
| `PartiallySignedTx.metadata` | midgard-only | Immutable metadata snapshot; object is not submit-capable. |
| `PartiallySignedTx.assemble` | adapted | Continues partial witness collection without exposing `submit` until complete. |
| `PartiallySignedTx.toPartialWitnessBundle` | midgard-only | Exports accumulated witnesses as a JSON-safe partial bundle. |
| `PartiallySignedTx.toPartialWitnessBundleCbor` | midgard-only | Exports accumulated witnesses as canonical bundle CBOR. |
| `SubmittedTx.tx` | midgard-only | Current accessor for submitted Midgard transaction object. |
| `SubmittedTx.admission` | midgard-only | Durable admission metadata, not validation acceptance. |
| `SubmittedTx.producedOutputs` | midgard-only | Delegates to submitted transaction local output derivation. |
| `SubmittedTx.producedOutput` | midgard-only | Delegates one-output local derivation by native body output index. |
| `SubmittedTx.status` | midgard-only | Direct Midgard status lookup. |
| `SubmittedTx.statusSafe` | midgard-only | Safe-result wrapper for submitted status lookup. |
| `SubmittedTx.statusProgram` | midgard-only | Lazy `Effect` wrapper for submitted status lookup. |
| `SubmittedTx.awaitStatus` | midgard-only | Midgard status polling helper. |
| `SubmittedTx.awaitStatusSafe` | midgard-only | Safe-result wrapper for submitted status polling. |
| `SubmittedTx.awaitStatusProgram` | midgard-only | Lazy `Effect` wrapper for submitted status polling. |

## Divergence And Migration Notes

Midgard keeps some Lucid names where the ergonomics are useful, but the
semantics are not Cardano semantics:

- Hash domain: Lucid signs Cardano transaction body hashes. `lucid-midgard`
  signs `nativeTx.compact.transactionBodyHash`, which is also the Midgard tx id.
- Serialization: Lucid emits Cardano transaction CBOR. `lucid-midgard` emits
  Midgard native full transaction bytes and must not submit Cardano CBOR on the
  normal path.
- Submission: Lucid-style submit returns a hash for a Cardano provider submit.
  Midgard submit returns durable admission metadata first; `accepted`,
  `rejected`, and `committed` are later Midgard status states.
- Local evaluation: Lucid exposes a Cardano-domain local evaluation flag.
  Midgard does not expose that option. Use
  `complete({ localValidation: "none" | "phase-a" | "phase-b" })`, with Phase B
  requiring explicit local pre-state.
- Protocol parameters: Lucid providers expose Cardano protocol parameters.
  Midgard providers expose `GET /protocol-info`, including Midgard native
  transaction version, fee parameters, submission limits, current slot, and
  validation strictness.
- Providers: Lucid provider adapters read Cardano L1 state. Midgard provider
  implementations must read Midgard L2 state through `MidgardProvider`.

## Provider Exports

`lucid-midgard` exports only Midgard-aware providers:

- `MidgardProvider`
- `MidgardNodeProvider`

It must not re-export `@lucid-evolution/provider` or expose Cardano/Lucid
provider adapters such as `Blockfrost`, `Maestro`, `Kupmios`, `Koios`,
`Emulator`, or future Lucid provider exports. Those read or emulate Cardano L1
state, not Midgard L2 state.

## Package Export Rule

`lucid-midgard` may import specific Lucid primitives internally, such as `CML`
types needed for addresses and witnesses, but it must not wildcard-export Lucid
packages. Public exports must be reviewed through this parity map before they
are added.
