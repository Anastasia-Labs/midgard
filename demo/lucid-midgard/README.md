# lucid-midgard

Midgard-native L2 transaction builder primitives inspired by lucid-evolution.
The library builds and signs Midgard native transaction bytes; it does not build
or submit Cardano transactions.

## Quickstart

```ts
import {
  LucidMidgard,
  MidgardNodeProvider,
  walletFromExternalSigner,
} from "@al-ft/lucid-midgard";

const provider = await MidgardNodeProvider.create({
  endpoint: "http://127.0.0.1:3000",
});

const midgard = await LucidMidgard.new(provider, {
  network: "Preview",
  networkId: 0,
});

midgard.selectWallet.fromExternalSigner(walletFromExternalSigner({
  address,
  keyHash,
  signBodyHash,
}));

const tx = await midgard
  .newTx()
  .pay.ToAddress(destination, { lovelace: 2_000_000n })
  .complete();

const signed = await tx.sign.withWallet().complete();
const submitted = await signed.submit();
await submitted.awaitStatus({ until: "accepted" });
```

## Provider Contract

Use `MidgardProvider` or `MidgardNodeProvider`. A provider must expose Midgard
UTxOs, Midgard native transaction submission, transaction status, protocol fee
parameters, and `GET /protocol-info` facts. `supportedScriptLanguages` must
advertise exactly the stable protocol tags for `PlutusV3` and `MidgardV1`.

`MidgardNodeProvider.create(...)` fails closed when protocol info is unavailable
unless an explicit fallback is supplied. Fallback diagnostics are visible through
`provider.diagnostics()` and `midgard.config()`.

## Provider Switching And UTxO Overrides

`midgard.switchProvider(provider, expectations)` validates network, native
transaction version, API version, and protocol diagnostics before replacing the
active provider. `overrideUTxOs(utxos)` and `clearUTxOOverrides()` let tests,
offline flows, and controlled callers provide selected-wallet inputs without
silently changing provider state.

Convenience methods include `currentSlot`, `utxosAt`, `utxosAtWithUnit`,
`utxoByUnit`, `utxosByOutRef`, `datumOf`, `txStatus`, and `awaitTx`.

## Builder Surface

The fluent builder supports `collectFrom`, `readFrom`, `pay.ToAddress`,
`pay.ToContract`, `pay.ToProtectedAddress`, `addSigner`, `addSignerKey`,
`mintAssets`, `mint`, `validFrom`, `validTo`, `setMinFee`, `compose`,
`complete`, `completeSafe`, `completeProgram`, `chain`, `chainSafe`, and
`chainProgram`.

`setMinFee` is a fee floor. Provider fee policy remains authoritative for
balanced transactions unless the caller supplies an explicit `feePolicy`.

## Observer Validators

Observers are Midgard script executions, not withdrawal or reward-account
operations.

```ts
await midgard
  .newTx()
  .attach.ObserverValidator({ language: "MidgardV1", script })
  .observe(observerScriptHash, redeemer)
  .complete();
```

The supported non-native validator languages are `PlutusV3` and `MidgardV1`.
Reference scripts without locally inspectable script bytes require explicit
trusted metadata through `readFrom(..., { trustedReferenceScripts })` or
`attach.ReferenceScriptMetadata(...)`.

## Partial Signing

Signing verifies witnesses against the Midgard native body hash. Detached
partial bundles are bound to one transaction id/body hash.

```ts
const bundleA = await tx.sign.withWallet(walletA).partial();
const bundleB = await tx.sign.withPrivateKey(privateKeyB).partial();
const signed = tx.assemble([bundleA, bundleB]);
```

`PartiallySignedTx` is non-submit-capable. The sign builder also exposes
`complete`, `completeSafe`, and `completeProgram` for direct construction of a
fully witnessed transaction when the supplied witnesses are complete.

## Local Chaining

`CompleteTx.producedOutputs()` derives optimistic local UTxOs from the Midgard
transaction id, native body output index, and exact output CBOR. These outputs
can feed a later builder before provider confirmation:

```ts
const [, produced, firstTx] = await first.chain();
const secondTx = await midgard
  .newTx()
  .collectFrom([produced[0]])
  .pay.ToAddress(nextAddress, produced[0].assets ?? {})
  .complete();
```

Local chained outputs are optimistic until node status reports acceptance or a
later durable state.

## Safe And Effect APIs

Promise APIs throw structured `LucidMidgardError` subclasses. Safe APIs return
`{ ok: true, value } | { ok: false, error }`. Program APIs return lazy
`Effect` values and perform no provider or wallet work until run.

```ts
const safe = await tx.submitSafe();
const program = tx.awaitStatusProgram({ until: "committed", timeoutMs: 30_000 });
```

Promise-style callers should catch `LucidMidgardError` subclasses and log
`error.toJSON()` where available. Safe-result and Effect examples in
`examples/usage.ts` preserve the structured error code instead of converting
failures into strings or booleans.

## Durable Admission

`submit` returns `SubmittedTx` with durable admission metadata. Admission is not
final validation acceptance. Use `status` or `awaitStatus` to observe `queued`,
`accepted`, `rejected`, and `committed` states. Rejected status preserves node
reject codes and details.

## Protected Outputs

`pay.ToProtectedAddress(...)` preserves the protected-output marker in the exact
output CBOR. Local output derivation and chaining keep that byte representation
instead of rebuilding outputs from decoded display fields.

## Runnable Examples

See [examples/usage.ts](./examples/usage.ts) for runnable in-memory examples
covering balanced transfers, provider switching, UTxO overrides, observer
validators, imports, composition, local chaining, signing, submission, status,
safe-result errors, Effect errors, and protected outputs.
