# Public API

The public API should be familiar to lucid-evolution users while preserving
Midgard-native semantics. The names below describe the current implemented
surface.

## Entry Point

```ts
const midgard = await LucidMidgard.new(provider, "Preprod");
midgard.selectWallet.fromSeed(seedPhrase);

const completed = await midgard
  .newTx()
  .collectFrom([utxo])
  .pay.ToAddress(destination, { lovelace: 3_000_000n })
  .complete();

const signed = await completed.sign();
const submitted = await signed.submit();
await submitted.awaitStatus({ until: "accepted" });
const txId = signed.toHash();
```

`LucidMidgard` owns configuration, provider access, wallet selection, protocol
parameters, and transaction builder creation.

## Builder Lifecycle

The lifecycle should be staged:

1. `TxBuilder`: records intent.
2. `CompleteTx`: canonicalizes, balances, computes roots, computes tx id.
3. `CompleteTx` with a complete address witness set: current submit-capable
   signed transaction shape.
4. `PartiallySignedTx`: explicit non-submit-capable partial witness collection.
5. `SubmittedTx`: carries durable admission result and status helpers.

Each stage must expose immutable data or immutable snapshots. Mutating a builder
after completion should require starting from a new builder or using an
explicit `clone`.

`LucidMidgard.fromTx(input, options?)` imports only canonical Midgard native v1
full transactions. Accepted inputs are native full objects, native CBOR bytes,
native hex, `{ txCbor }`, `{ txHex }`, and existing `CompleteTx` instances.
Cardano transaction/body/witness bytes are rejected rather than converted.
Signed raw imports require resolved spend-input pre-state unless every expected
address witness can be derived from the body alone.

## Fluent Surface

Initial required methods:

- `collectFrom(utxos, redeemer?)`
- `readFrom(utxos, { trustedReferenceScripts? })`
- `pay.ToAddress(address, assets, options?)`
- `pay.ToContract(address, datum, assets, options?)`
- `pay.ToProtectedAddress(address, assets, options?)`
- `addSigner(keyHashOrAddress)`
- `validFrom(slotOrPosix)`
- `validTo(slotOrPosix)`
- `attach.Script(script)`
- `attach.NativeScript(nativeScript)`
- `attach.ReferenceScriptMetadata(metadata)`
- `attach.Datum(datum)`
- `mintAssets(assets, redeemer?)`
- `observe(scriptHash, redeemer?)`
- `compose(other, ...others)`
- `complete(options?)`
- `completeSafe(options?)`
- `completeProgram(options?)`
- `chain(options?)`
- `chainSafe(options?)`
- `chainProgram(options?)`

Names may be adjusted during implementation, but the API must distinguish
ordinary outputs from protected outputs because protected outputs affect Phase B
receive validation.

`compose` merges compatible builder fragments left-to-right and recomputes all
derived roots, redeemer indexes, language views, and script integrity during
final completion. `chain` completes the builder and returns
`[newWalletUtxos, derivedOutputs, completeTx]`; `derivedOutputs` are optimistic
local `MidgardUtxo` values derived from the native tx id, body output indexes,
and exact output CBOR bytes.

Safe builder methods return
`MidgardResult<T, LucidMidgardError>` for expected Midgard errors. Unexpected
defects are rethrown. Program methods return lazy `Effect` values and perform no
provider or wallet calls until the effect is run.

## Completion Options

`complete` should accept explicit options:

```ts
type CompleteOptions = {
  readonly changeAddress?: Address;
  readonly localValidation?: "none" | "phase-a" | "phase-b";
  readonly feePolicy?: "provider" | { minFeeA: bigint; minFeeB: bigint };
  readonly maxFeeIterations?: number;
};
```

Defaults:

- Fee policy comes from the provider.
- Local validation defaults to `"none"`; callers opt into `"phase-a"` or
  `"phase-b"` preflight explicitly.
- Script budget enforcement defaults to true when Phase B simulation runs.
- No compatibility or conversion mode exists.

## Local Preflight

`complete({ localValidation })` runs shared validation during finalization and
rejects if the transaction does not pass the requested phase. Completed
transactions also expose explicit preflight without mutating metadata:

```ts
const phaseA = await tx.validate("phase-a");
const phaseB = await tx.validate("phase-b", { localPreState });
const safe = await tx.validateSafe("phase-a");
const program = tx.validateProgram("phase-b", { localPreState });
```

Preflight reports preserve shared node reject codes and reason details. They
are local checks only and never imply final node acceptance.

## Signing API

Signing must be explicit about the Midgard hash domain:

```ts
const bundle = await completeTx.sign.withWallet().partial();
const signed = completeTx.assemble([bundleFromA, bundleFromB]);
```

Partial witness bundles are detached, JSON-safe or canonical-CBOR envelopes
bound to one Midgard native transaction id/body hash. Assembly verifies every
vkey witness against `nativeTx.compact.transactionBodyHash`, rejects mismatched
or unexpected witnesses, and returns a submit-capable `CompleteTx` only when the
expected address witness set is complete. It returns `PartiallySignedTx` only
when `allowPartial: true` is explicit. `PartiallySignedTx` has no `submit`
method. The `completeTx.sign(wallet?)` shortcut remains a thin compatibility
path for single-wallet signing and returns the signed `CompleteTx` shape.

## Observer Validators

Observers are Midgard script purposes, not Cardano withdrawals. Attach the
validator and add the required observer intent explicitly:

```ts
const tx = await midgard
  .newTx()
  .attach.ObserverValidator({ language: "MidgardV1", script })
  .observe(observerHash, redeemer)
  .complete();
```

`attachObserverScript(...)` is an alias for `attach.ObserverValidator(...)`.
Supported observer validator languages are only `PlutusV3` and `MidgardV1`.

## Reference Script Metadata

Reference inputs are resolved from local output CBOR when the referenced output
contains a script ref. When local bytes do not expose the required language
branch, callers must provide explicit trusted metadata keyed by the reference
outref:

```ts
tx.readFrom([referenceUtxo], {
  trustedReferenceScripts: [{
    txHash,
    outputIndex,
    language: "MidgardV1",
    scriptHash,
    scriptCborHash,
  }],
});
```

`attach.ReferenceScriptMetadata(...)` supplies the same metadata separately.
When a local script ref is present, the builder verifies the declared language,
script hash, and optional script CBOR hash against the decoded bytes. Without
local script bytes, the metadata is treated as trusted caller input and Phase B
will still recompute against node state.

## Submission API

```ts
const submitted = await signed.submit();
await submitted.awaitStatus({ until: "accepted" });
```

Submission returns durable admission information first. Acceptance into the
mempool is a later validation result, so the API must not label a `202` submit
response as final validity acceptance.

`submitSafe`/`submitProgram`, `statusSafe`/`statusProgram`, and
`awaitStatusSafe`/`awaitStatusProgram` expose the same checked behavior as
safe-result or lazy `Effect` values. Durable admission payloads are validated for
tx id, HTTP admission status, node admission status
(`queued | validating | accepted | rejected`), required duplicate flag shape,
and timestamp field shape before a `SubmittedTx` is exposed. HTTP `202` means a
new admission and must be `queued` with `duplicate=false`; HTTP `200` means an
already-admitted same-byte transaction and must carry `duplicate=true`.

## Error Model

Errors should be structured:

- `BuilderInvariantError`
- `CanonicalizationError`
- `InsufficientFundsError`
- `FeeConvergenceError`
- `SigningError`
- `LocalValidationError`
- `SubmitError`
- `StatusError`

Where node validation rejects with stable reject codes, `lucid-midgard` should
preserve those codes verbatim.
