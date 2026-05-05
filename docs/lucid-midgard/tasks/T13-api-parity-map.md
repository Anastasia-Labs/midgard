# T13 API Parity Map

## Scope

Create an explicit parity map between `lucid-midgard` and
`@lucid-evolution/lucid` `0.4.30`, using Lucid only as an ergonomics reference.
The audited source for this task is the Lucid `0.4.30` published
`dist/index.d.ts` type surface. Midgard-native transaction semantics,
validation, signing, provider reads, and submission behavior remain
authoritative.

The map must classify every relevant public Lucid entry point as one of:

- Supported with the same name.
- Supported with Midgard-specific adaptation.
- Midgard-only.
- Intentionally unsupported.

The map must cover top-level methods, wallet selection, builder methods,
signing/submission/chaining methods, provider convenience methods, provider
exports, and the Cardano L1 surfaces that Midgard intentionally does not
support.

## Dependencies

- T01 package scaffold.
- T03 core types.
- T04 provider client.
- T05 wallet and signers.
- T06 builder fluent API.
- T11 submit, status, and chain.
- Architecture docs `00`, `01`, and `04`.

## Deliverables

- Checked-in API parity matrix for `lucid-midgard` versus
  `@lucid-evolution/lucid` `0.4.30`.
- Classification and rationale for top-level methods:
  - `config`
  - `wallet`
  - `overrideUTxOs`
  - `switchProvider`
  - `newTx`
  - `fromTx`
  - `selectWallet`
  - `selectWallet.fromSeed`
  - `selectWallet.fromPrivateKey`
  - `selectWallet.fromAPI`
  - `selectWallet.fromAddress`
  - `currentSlot`
  - `unixTimeToSlot`
  - `utxosAt`
  - `utxosAtWithUnit`
  - `utxoByUnit`
  - `utxosByOutRef`
  - `delegationAt`
  - `awaitTx`
  - `datumOf`
  - `metadataOf`
- Classification and rationale for builder methods:
  - `readFrom`
  - `collectFrom`
  - `pay.ToAddress`
  - `pay.ToAddressWithData`
  - `pay.ToContract`
  - `addSigner`
  - `addSignerKey`
  - `mintAssets`
  - `validFrom`
  - `validTo`
  - `attach.Script`
  - `attach.SpendingValidator`
  - `attach.MintingPolicy`
  - `attach.ObserverValidator`
  - `attachMetadata`
  - `observe`
  - `compose`
  - `setMinFee`
  - `complete`
  - `completeSafe`
  - `completeProgram`
  - `chain`
  - `chainSafe`
  - `chainProgram`
  - `config`
  - `rawConfig`
  - `lucidConfig`
  - `getPrograms`
- Classification and rationale for signing/submission lifecycle methods:
  - `sign.withWallet`
  - `sign.withPrivateKey`
  - `partialSign.withWallet`
  - `partialSign.withWalletEffect`
  - `partialSign.withWalletSafe`
  - `partialSign.withPrivateKey`
  - `partialSign.withPrivateKeyEffect`
  - `partialSign.withPrivateKeySafe`
  - `assemble`
  - `complete`
  - `completeSafe`
  - `completeProgram`
  - `submit`
  - `submitSafe`
  - `submitProgram`
  - `toCBOR`
  - `toTransaction`
  - `toJSON`
  - `toHash`
- Explicit unsupported classification for:
  - `registerStake`
  - `deRegisterStake`
  - `withdraw`
  - `register.Stake`
  - `register.DRep`
  - `deregister.Stake`
  - `deregister.DRep`
  - `delegateTo`
  - `delegate.ToPool`
  - `delegate.VoteToDRep`
  - `delegate.VoteToPoolAndDRep`
  - `registerAndDelegate.*`
  - `updateDRep`
  - `authCommitteeHot`
  - `resignCommitteeHot`
  - certificate, withdrawal, vote, and propose validators
  - Cardano transaction metadata helpers
- Explicit provider export decision:
  - Keep `MidgardProvider`.
  - Keep `MidgardNodeProvider`.
  - Do not re-export `@lucid-evolution/provider`.
  - Do not add or leak Cardano/Lucid provider adapters, including `Blockfrost`,
    `Maestro`, `Kupmios`, `Koios`, `Emulator`, or future Lucid provider exports,
    because they read or emulate Cardano L1 state, not Midgard L2 state.
- Explicit package export strategy that prevents wildcard Lucid re-exports from
  leaking unsupported provider, wallet, metadata, or governance surfaces.
- Compatibility notes explaining where `lucid-midgard` intentionally diverges
  from Lucid `0.4.30`, including:
  - Midgard-native body hashes instead of Cardano transaction body hashes.
  - Native transaction bytes instead of Cardano transaction CBOR.
  - Durable admission versus final validation acceptance.
  - `complete({ localValidation })` instead of Lucid's Cardano
    local-evaluation completion option.
  - Midgard provider protocol information instead of Cardano protocol
    parameters.
- Type-level or compile-time expectations for supported and unsupported API
  names.

## Acceptance Criteria

- Every relevant `@lucid-evolution/lucid` `0.4.30` public method is classified.
- Every unsupported method has a production-grade rationale tied to Midgard L2
  semantics.
- No Cardano staking, withdrawal, delegation, DRep, governance, committee, or
  metadata method is marked as supported unless a later Midgard-native protocol
  feature explicitly requires it.
- No Cardano provider adapter is presented as a Midgard provider.
- `MidgardProvider` and `MidgardNodeProvider` remain the named provider
  abstraction and concrete HTTP provider.
- API names retained from Lucid have documented Midgard semantics.
- API names that differ from Lucid have documented migration guidance.
- The map identifies which follow-up work belongs to T14 through T26.
- The map does not introduce compatibility shims, fallback aliases, or legacy
  behavior for `demo/midgard-node`.

## Required Tests

- Type tests proving supported top-level API names compile.
- Type tests proving supported builder API names compile.
- Negative type tests proving unsupported Cardano staking, withdrawal,
  delegation, DRep, governance, committee, and metadata methods are absent.
- Negative type tests proving `Blockfrost`, `Maestro`, and `Kupmios` are not
  exported by `lucid-midgard`.
- Negative type tests proving `Koios`, `Emulator`, `@lucid-evolution/provider`
  wildcard exports, and other Lucid/Cardano provider adapters are not exported.
- Documentation consistency test or lint check ensuring every classified method
  has a status and rationale.
- Snapshot test for the parity matrix so future Lucid-inspired additions must
  update the map intentionally.

## Non-Goals

- Do not implement missing parity methods in this task except for minimal type
  assertions needed to document the surface.
- Do not add Cardano staking, withdrawal, delegation, DRep, governance, or
  committee features.
- Do not add Blockfrost, Maestro, Kupmios, or other Cardano-read providers.
- Do not add compatibility aliases for prior Midgard native transaction shapes.
- Do not make Lucid `0.4.30` behavior authoritative where it conflicts with
  Midgard protocol correctness.
