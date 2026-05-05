# Provider and Wallet

`lucid-midgard` needs provider and wallet abstractions that are specific to
Midgard L2, even when backed by Cardano/Lucid primitives.

## Provider Interface

Target provider shape:

```ts
type MidgardProvider = {
  getUtxos(address: Address): Promise<MidgardUtxo[]>;
  getUtxoByOutRef(outRef: OutRef): Promise<MidgardUtxo | undefined>;
  getUtxosByOutRefs?(outRefs: OutRef[]): Promise<MidgardUtxo[]>;
  getUtxosByUnit?(unit: string): Promise<MidgardUtxo[]>;
  getProtocolInfo(): Promise<MidgardProtocolInfo>;
  getProtocolParameters(): Promise<MidgardProtocolParameters>;
  getCurrentSlot(): Promise<bigint>;
  submitTx(txCborHex: string): Promise<SubmitTxResult>;
  getTxStatus(txId: string): Promise<TxStatus>;
};
```

The initial concrete implementation should be `MidgardNodeProvider`, backed by
the node HTTP API:

- `GET /utxos?address=...`
- `GET /utxo?txOutRef=...`
- `POST /utxos?by-outrefs`
- `POST /submit`
- `GET /tx-status?tx_hash=...`
- `GET /protocol-info`

`GET /protocol-info` is the canonical source for API version, Midgard-native
transaction version, network, current slot, supported non-native script language
tags, protocol fee parameters, submission limits, and validation strictness
profile. `supportedScriptLanguages` must exactly match the shared Midgard
profile: `PlutusV3` with tag `2` and `MidgardV1` with tag `128`. Missing,
extra, `PlutusV1`, `PlutusV2`, unknown, or future tags fail closed until an
explicit protocol migration updates the shared constants and fixtures.

If this endpoint is missing in the target node, `MidgardNodeProvider` must fail
closed unless the caller supplies an explicit fallback configuration.
Fallback-derived values must be visible in diagnostics and must not be silently
treated as production node facts.

`submitTx` returns durable admission metadata, not final validity. The response
status must be a node admission state (`queued | validating | accepted |
rejected`), `duplicate` is required, HTTP `202` means a new queued admission,
and HTTP `200` means an already-admitted same-byte transaction.

The library must not hard-code production fee policy, slot assumptions, network
identity, or maximum transaction size.

## Query Convenience

`LucidMidgard` exposes Midgard-native query helpers over the active provider:

- `currentSlot()`
- `utxosAt(address)`
- `utxosAtWithUnit(address, unit)`
- `utxosByOutRef(outRefs, { missing })`
- `utxoByUnit(unit)` when the provider exposes a native unit index
- `datumOf(utxo, { expectedHash })` for inline Midgard datums

Address queries are address-string only. Credential scans and Cardano provider
queries are intentionally absent. Batch outref queries reject duplicate request
keys, default to strict missing-result errors, and preserve request order when
`{ missing: "omit" }` is used.

`utxoByUnit` must use a provider-side Midgard unit index. It must not scan
unrelated addresses or Cardano state, and it rejects zero or multiple indexed
matches as structured provider payload errors.

## Provider Switching

`LucidMidgard.switchProvider(provider)` accepts only `MidgardProvider`
implementations. Before replacing the current provider, the library fetches and
validates `GET /protocol-info` through `provider.getProtocolInfo()`. The switch
is atomic: if the candidate provider is missing Midgard methods, cannot return
protocol info, reports a different network, reports an unsupported
Midgard-native transaction version, or fails wallet-network revalidation, the
previous provider and configuration snapshot remain active.

Builders capture the provider snapshot at `newTx()` time. Later provider
switches do not affect existing builders or in-flight `complete()` calls. New
builders use the new provider generation after a successful switch.

`LucidMidgard.config()` returns an immutable snapshot containing provider
generation, protocol-info source diagnostics, current slot, fee parameters,
submission limits, validation strictness, network identity, and UTxO override
generation. Fallback protocol-info values remain visible as fallback-derived
diagnostics.

## Local Provider

A local provider may be useful inside `demo/midgard-node`, but it should be a
separate adapter. The public package should not depend on node database modules.

## Wallet Interface

Wallet selection should mirror Lucid ergonomics while making the signing domain
explicit:

```ts
type MidgardWallet = {
  address(): Promise<Address>;
  rewardAddress?(): Promise<Address>;
  keyHash(): Promise<string>;
  signBodyHash(bodyHash: Uint8Array): Promise<VKeyWitness>;
};
```

Initial wallet adapters:

- `fromSeed(seedPhrase)`
- `fromPrivateKey(privateKey)`
- `fromExternalSigner(signer)`
- `fromAddress(address, utxos?)` for read-only construction with explicit
  Midgard UTxO overrides.

Browser wallet support should be designed separately because ordinary Cardano
wallet APIs may sign Cardano transaction hashes rather than arbitrary Midgard
body hashes. Any browser adapter must prove it signs the correct bytes.

`fromAddress` is intentionally read-only. It can construct and balance
transactions from explicit local inputs, but signing through that wallet fails
unless the caller supplies a real Midgard body-hash signer separately.

## Signing Rules

Every vkey witness must verify against `nativeTx.compact.transactionBodyHash`.
The library should verify witnesses immediately after signing and before
submission.

Cardano-domain signatures are invalid for Midgard-native transactions. The API
must not include a path that completes a Cardano transaction and relies on node
conversion as a normal submission method.

## Network Id

The provider or explicit configuration must determine expected network id. The
builder must reject address/network mismatches before producing bytes.

Network id may be omitted by using Midgard native network id `255`, but any
script bundle requiring observer reconstruction must provide an explicit
network id because Phase A rejects observer-bearing Plutus bundles without it.
