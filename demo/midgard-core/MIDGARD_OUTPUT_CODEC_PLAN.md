# Midgard-Owned Output Codec Plan

## Purpose

Move Midgard L2 ledger output bytes out of Cardano/CML
`TransactionOutput` encoding and into Midgard-owned strict CBOR codecs under
`demo/midgard-core/src/codec`.

The current native transaction codec is already owned by `midgard-core`, but
its output preimage still stores CML/Cardano TxOut CBOR byte strings. That
leaks Cardano semantics into persistent L2 state:

- CML supports datum hashes; Midgard outputs do not.
- CML supports PlutusV1 and PlutusV2 reference scripts; Midgard outputs do not.
- CML does not support MidgardV1 reference scripts; the current code encodes
  MidgardV1 bytes as PlutusV3 reference scripts and loses recoverable version
  information.
- CML does not model protected outputs; the current code patches the Cardano
  address header byte outside an authoritative Midgard address codec.
- Inline script witnesses and reference scripts currently do not share one
  Midgard-owned versioned script wire format.

The target state is a canonical Midgard output and script-witness format that
preserves all Midgard semantics directly in bytes and can be decoded without
trusted side-channel metadata.

## Scope

In scope:

- Add Midgard-owned output, value, address, datum, native-script, and
  versioned-script codecs under `demo/midgard-core/src/codec`.
- Make Midgard native transaction output preimages contain the new output CBOR.
- Make inline script witnesses use `MidgardVersionedScript`, the same script
  format used by output `script_ref`.
- Keep `NativeCardano` as a supported Midgard script language, but implement
  Midgard-owned native-script encode/decode/hash/verification instead of using
  CML as the authoritative native-script codec.
- Update node validation and `lucid-midgard` construction paths to use the new
  codecs.
- Update durable node writers/readers, manager tooling, scripts, and fixtures
  that currently create or decode CML-shaped Midgard L2 outputs.
- Cut over by using a clean local database/local durable state and a full
  on-chain redeploy/reset.
- Keep CML only as an explicit adapter for real Cardano/L1 values and helper
  operations that remain intentionally Cardano-domain.

Out of scope:

- Forking or modifying CML.
- Building a full replacement for CML.
- Preserving backward compatibility with old CML-shaped L2 outputs.
- Writing a migration tool for old development-state CML-shaped outputs.
- Changing L1 Cardano reference script publication semantics.

## Resolved Decisions

- Inline script witnesses use `MidgardVersionedScript`. Reference scripts and
  inline witnesses therefore have one script format.
- `NativeCardano` remains supported as a Midgard script variant alongside
  `PlutusV3` and `MidgardV1`.
- Native scripts are encoded, decoded, hashed, and verified by Midgard-owned
  code. CML native-script decoding is not part of the authoritative Midgard
  protocol path.
- Midgard output addresses support Cardano Shelley payment address families
  except pointer addresses. Byron addresses, pointer addresses, and
  stake/reward address inputs are not supported.
- The protected flag is encoded in the unused high bit of the Midgard address
  header, using the existing `0x80` protected mask. Protected status is derived
  from address bytes, not from a separate output field.
- Canonical Midgard address text uses the same Bech32 HRPs as Cardano Shelley
  payment addresses: `addr` for mainnet and `addr_test` for non-mainnet. The
  Bech32 payload is the exact Midgard address bytes, including the protected
  bit when set.
- The cutover path is clean local DB/local state plus full explicit on-chain
  redeploy/reset. There is no legacy decode mode or migration tool.

## Target Encoding

These decisions are protocol requirements. Implementation should freeze them in
tests and docs.

### Canonical CBOR Rules

All new protocol bytes in this plan use canonical deterministic Midgard CBOR:

- Definite-length arrays, maps, and byte strings only.
- Minimal unsigned integer and length encodings only.
- No indefinite values, tags, floats, simple values, `undefined`, duplicate map
  keys, unknown fields, or trailing bytes.
- Decoders enforce byte-exact canonical form by requiring
  `encode(decode(bytes)) === bytes`.
- Non-asset maps use ascending integer keys or deterministic CBOR key order as
  specified by the map shape.
- Asset maps use the explicit Midgard value ordering rules below.
- Inner datum, native-script, versioned-script envelope, address, and value
  payloads are also canonicalized and byte-checked, not treated as opaque
  trusted bytes. Raw UPLC script byte payloads remain Cardano-style opaque
  script bytes inside the canonical versioned-script envelope.

### TypeScript Shapes

The protocol-facing types should be explicit and should not expose CML objects:

```ts
type MidgardTxOutput = {
  address: MidgardAddress;
  value: MidgardValue;
  datum?: MidgardDatum;
  script_ref?: MidgardVersionedScript;
};

type MidgardAddress = Uint8Array;

type MidgardValue = {
  lovelace: bigint;
  assets: Map<PolicyIdHex, Map<AssetNameHex, bigint>>;
};

type MidgardDatum = {
  kind: "inline";
  cbor: Uint8Array;
};

type MidgardVersionedScript =
  | {
      language: "NativeCardano";
      scriptBytes: Uint8Array;        // canonical native-script CBOR
      nativeScript: MidgardNativeScript;
    }
  | { language: "PlutusV3"; scriptBytes: Uint8Array }
  | { language: "MidgardV1"; scriptBytes: Uint8Array };

type MidgardNativeScript =
  | { type: "sig"; keyHash: Uint8Array }
  | { type: "all"; scripts: readonly MidgardNativeScript[] }
  | { type: "any"; scripts: readonly MidgardNativeScript[] }
  | {
      type: "atLeast";
      required: bigint;
      scripts: readonly MidgardNativeScript[];
    }
  | { type: "after"; slot: bigint }
  | { type: "before"; slot: bigint };
```

CML, Lucid, Bech32, and JSON-friendly structures may be used at adapter
boundaries, but not as the authoritative decoded representation of Midgard L2
output or script-witness bytes.

Lucid-facing/provider-facing output APIs keep the current user-facing shape and
camelCase field names:

```ts
type PublicMidgardTxOutput = {
  readonly address: string;       // canonical Midgard Bech32, may be protected
  readonly assets: Assets;
  readonly datum?: PublicMidgardDatum | null;
  readonly scriptRef?: PublicMidgardScript | null;
};

type PublicMidgardDatum = {
  readonly kind: "inline";
  readonly cbor: string;          // PlutusData CBOR hex
};

type PublicMidgardScript =
  | { readonly type: "Native"; readonly script: string }
  | { readonly type: "PlutusV3"; readonly script: string }
  | { readonly type: "MidgardV1"; readonly script: string };

type NodeUtxoResponse = {
  readonly outref: string;      // TxOutRef CBOR hex
  readonly outputCbor: string;  // MidgardTxOutput CBOR hex
};
```

Rules:

- Public `address` stores canonical Midgard Bech32 text using `addr` or
  `addr_test`, including the protected bit when the output is protected.
- Public output shapes do not include a separate `protected` field. Protection
  is derived from the `address` string by the Midgard address codec.
- Core/protocol structures use `script_ref`; Lucid/provider structures use
  `scriptRef`.
- Node HTTP UTxO endpoints expose stored protocol bytes as `outputCbor`.
  Lucid/provider clients decode `outputCbor` into the public output shape above.
  Normal node/provider output objects do not expose a separate `protected`
  field.

### Output Shape

`MidgardTxOutput` is a canonical CBOR map:

```text
{
  0: address_bytes,
  1: value,
  2: inline_datum?,  // canonical PlutusData CBOR bytes; no datum-hash variant
  3: script_ref?     // MidgardVersionedScript
}
```

Rules:

- Keys must be unsigned integers.
- Required keys are `0` and `1`.
- Optional keys are `2` and `3`.
- Unknown keys fail closed unless an explicit future protocol revision reserves
  them.
- Datum hashes are not representable.
- `script_ref` is the field name in core/protocol TypeScript structures even
  though the type is `MidgardVersionedScript`. Lucid-facing/public structures
  keep their current camelCase `scriptRef` field at adapter boundaries.
- Decoders must reject any output whose canonical re-encode does not byte-match
  the original bytes.

### Versioned Script Shape

`MidgardVersionedScript` replaces `CML.Script` in Midgard-owned protocol
encoding. It is used for both output `script_ref` and inline script witnesses.

Wire shape:

```text
[language_tag, script_bytes]
```

Initial language tags:

- `0`: `NativeCardano`
- `2`: `PlutusV3`
- `128`: `MidgardV1`, matching the existing `0x80` MidgardV1 tag/hash prefix

Rules:

- `PlutusV1` and `PlutusV2` are not valid Midgard versioned scripts.
- Unknown tags fail closed.
- `PlutusV3` payload is raw UPLC script bytes, not outer CML `Script` CBOR.
- `MidgardV1` payload is raw UPLC script bytes.
- Raw UPLC payloads are opaque at codec decode, following Cardano Plutus script
  serialization/hash behavior. The codec does not parse UPLC or reject empty
  payloads; invalid or non-executable UPLC fails when execution/evaluation uses
  the script.
- `NativeCardano` payload is canonical Midgard native-script CBOR, defined
  below. It is not decoded by CML in authoritative Midgard paths.
- Decoding a `NativeCardano` versioned script returns both the canonical
  native-script bytes and the parsed `MidgardNativeScript` AST. The canonical
  bytes are used for hashing and byte-exact re-encode checks; the AST is used
  for Midgard-owned native-script verification.
- The same raw UPLC bytes with tag `2` and tag `128` are distinct script
  values and must produce distinct script CBOR, output CBOR, output roots, and
  script hashes/language views where relevant.

Script hash formulas:

- `NativeCardano`: `blake2b224(0x00 || canonical_native_script_cbor)`.
- `PlutusV3`: `blake2b224(0x03 || raw_uplc_bytes)`, matching Cardano
  PlutusV3 script hash semantics.
- `MidgardV1`: `blake2b224(0x80 || raw_uplc_bytes)`, matching the current
  implementation's MidgardV1 domain byte.

### Native Script Shape

`NativeCardano` keeps Cardano native-script semantics but must be implemented
with a Midgard-owned native-script codec. The wire grammar mirrors the Cardano
native-script variant set so existing native-script behavior can be preserved
without using CML as the protocol codec:

```text
[0, key_hash]                    // sig
[1, [native_script*]]            // all
[2, [native_script*]]            // any
[3, required, [native_script*]]   // atLeast
[4, slot]                        // after
[5, slot]                        // before
```

Rules:

- `key_hash` is exactly 28 bytes.
- `required` and `slot` are non-negative canonical integers.
- Child script lists are canonical definite-length arrays.
- `all` over an empty child list evaluates to true, matching Cardano.
- `any` over an empty child list evaluates to false, matching Cardano.
- `atLeast` evaluates to true when at least `required` children evaluate true.
  `required = 0` is valid and evaluates true. `required > children.length` is
  valid CBOR and evaluates false unless enough children can evaluate true, which
  is impossible when `required > children.length`.
- `[4, slot]` is Cardano `invalid_before` / start-from-slot and evaluates true
  iff the transaction validity lower bound exists and is greater than or equal
  to `slot`.
- `[5, slot]` is Cardano `invalid_hereafter` / before-slot and evaluates true
  iff the transaction validity upper bound exists and is strictly less than
  `slot`.
- Missing validity lower/upper bounds make the corresponding time-lock script
  evaluate false.
- Native-script verifier inputs are:
  - `validityIntervalStart?: bigint`
  - `validityIntervalEnd?: bigint`
  - `witnessSigners: ReadonlySet<keyHashHex>`
- Recursive native-script decoding enforces byte-exact canonical re-encode.
- Native-script hash semantics match Cardano native scripts exactly:
  `blake2b224(0x00 || canonical_native_script_cbor)`.
- Native-script verification against keys/signatures/time validity is
  Midgard-owned validation logic, not a CML `NativeScript.verify` dependency,
  and must match Cardano behavior fixture-for-fixture.
- Native-script decode returns the parsed AST and the canonical CBOR bytes that
  hash to the script hash. Validation consumes the AST; hashing and fixtures
  consume the canonical bytes.
- Midgard does not define a stricter native-script recursion or node-count
  limit than Cardano. Effective limits are the same as Cardano's serialized
  transaction/script-size constraints plus Midgard's existing native tx CBOR
  size limits. Implementation must avoid process stack overflow without
  rejecting a script that Cardano would accept under those limits.
- CML native-script objects may appear only in explicit L1/Cardano adapter
  functions, not in validation, output decode, script-source resolution, or
  durable state.

### Inline Script Witness Shape

The native transaction witness-set script preimage stores a canonical list of
`MidgardVersionedScript` values:

```text
script_tx_wits = [MidgardVersionedScript*]
```

Rules:

- Inline raw UPLC byte strings are no longer valid script witnesses.
- CML `Script` wrappers are no longer valid script witnesses.
- PlutusV1 and PlutusV2 witnesses reject.
- The script-witness root is computed over the canonical witness-list bytes.
- Phase A and Phase B resolve inline script sources from explicit
  `MidgardVersionedScript` language tags.
- `MidgardV1` script witnesses are recoverable from transaction CBOR alone.

### Value Shape

Midgard output values are owned by `midgard-core`, not CML `Value`.

Wire shape:

```text
[coin, assets]

assets = {
  policy_id_bytes: {
    asset_name_bytes: quantity
  }
}
```

Rules:

- `coin` is a non-negative integer.
- Policy IDs are exactly 28 bytes.
- Asset names follow Cardano's token-name bound and must be at most 32 bytes.
- Output quantities are non-negative.
- Zero-quantity assets reject on decode and are omitted or rejected on encode.
- Empty inner asset maps reject.
- Empty outer asset maps are allowed.
- Policy entries are sorted lexicographically by raw policy id bytes.
- Asset-name entries are sorted lexicographically by raw asset-name bytes.
- The value codec must use a custom asset-map encoder/decoder that preserves
  and validates raw-byte lexicographic ordering. It must not rely on generic
  JavaScript `Map` insertion order or generic RFC 8949 CBOR map ordering for
  policy/asset maps.
- Decoders reject semantically equivalent values encoded with different asset
  order, non-minimal integers/lengths, duplicate policies/assets, or alternate
  shapes.
- Value decoders enforce byte-exact canonical form by requiring
  `encodeMidgardValue(decodeMidgardValue(bytes)) === bytes`.

### Datum Shape

Midgard outputs support inline datum only.

Rules:

- Output key `2` stores a byte string containing canonical PlutusData CBOR.
- Datum hashes are not representable.
- The datum codec must match the current Cardano Conway ledger PlutusData
  encoding rules, including constructor, map, list, integer, and byte-string
  forms.
- Constructor encodings follow Cardano's current PlutusData constructor tags,
  including compact constructor tags and the general constructor form used by
  Cardano when an alternative is outside the compact range.
- The datum decoder must parse the inner PlutusData bytes and require
  canonical re-encode equality for the datum itself.
- Non-canonical PlutusData, indefinite collections/byte strings, non-minimal
  integers/lengths, malformed constructors, duplicate/non-canonical map keys,
  tags not valid for Cardano PlutusData, and trailing bytes reject.
- Invalid or non-canonical PlutusData rejects before validation uses the datum.

### Address Shape

`address_bytes` are Midgard-owned address bytes. The protected flag is encoded
directly inside those bytes and is not carried as a separate output boolean.

Rules:

- The protected flag is bit `0x80` of the first address header byte.
- To interpret the address family, the decoder clears bit `0x80` and then
  interprets the remaining header as a Shelley payment address header.
- The network id is the low nibble of the address header after clearing
  `0x80`. Canonical Midgard mainnet Bech32 (`addr`) requires network id `1`.
  Canonical Midgard testnet/non-mainnet Bech32 (`addr_test`) requires network
  id `0`. Other network ids reject unless a future explicit network policy
  allows them.
- Supported Shelley payment address families after clearing the protected bit:
  - base payment/stake addresses, Cardano header types `0`, `1`, `2`, and `3`
  - enterprise payment addresses, Cardano header types `6` and `7`
- Base address byte length is exactly `57` bytes:
  `1` header byte, `28` payment credential bytes, and `28` stake credential
  bytes.
- Enterprise address byte length is exactly `29` bytes:
  `1` header byte and `28` payment credential bytes.
- Unsupported Midgard families after clearing the protected bit:
  - pointer addresses, header types `4` and `5`
  - Byron/bootstrap addresses
  - any reserved or unknown family
- Stake/reward addresses are separate from Shelley payment addresses and are not
  valid Midgard output addresses.
- Protected enterprise Midgard address headers overlap Cardano reward-address
  byte values. Therefore Cardano-address adapters must reject stake/reward
  addresses before conversion; raw Midgard output decode follows the Midgard
  protected-bit rules and must not call CML to reinterpret those bytes as
  Cardano rewards.
- Address bytes must decode enough to recover a payment credential for Phase B.
- All supported Cardano Shelley payment address bytes are valid unprotected
  Midgard address bytes, except pointer addresses which Midgard intentionally
  rejects.
- Protected output semantics are derived only from Midgard address bytes.
- The protected mask constant should live in the core Midgard address codec;
  production paths must not patch arbitrary CML TxOut bytes.
- CML address conversion is adapter-only and may be used only after clearing the
  protected bit and validating that the resulting address is a supported
  Shelley payment address.
- Required address codec APIs:
  - `decodeMidgardAddressBytes`
  - `encodeMidgardAddressBytes`
  - `decodeMidgardAddressText`
  - `encodeMidgardAddressText`
  - `isProtectedMidgardAddress`
  - `protectMidgardAddress`
  - `unprotectMidgardAddress`
  - `midgardAddressLookupVariants`
  - `paymentCredentialFromMidgardAddress`

### Midgard Bech32 Address Text

Midgard address text is Bech32 over exact `MidgardAddress` bytes.

Rules:

- Mainnet Midgard address text uses HRP `addr`.
- Non-mainnet Midgard address text uses HRP `addr_test`.
- The Bech32 data payload is the exact Midgard address bytes.
- Protected addresses are encoded by setting `0x80` in the first address byte
  and recomputing the Bech32 checksum with the same HRP.
- Unprotected Cardano Shelley base and enterprise payment addresses are already
  valid Midgard address text.
- Protected Midgard address text is valid Bech32 and valid Midgard address
  text, but must not be parsed with CML as a Cardano address.
- Decoding Midgard address text must use the Midgard address codec. It must
  accept protected headers by clearing `0x80` for family validation and reject
  pointer, Byron/bootstrap, stake/reward, reserved, and unknown address forms.
- Encoding Midgard address text is canonical. Re-encoding decoded address bytes
  must reproduce the same lowercase Bech32 string.
- Node DB columns named `address TEXT` store canonical Midgard address text,
  not necessarily CML-parseable Cardano bech32.
- For address lookup APIs, `address=<midgard-bech32>` identifies the underlying
  payment address after clearing the protected bit. Lookup should query both the
  unprotected and protected canonical Midgard address text variants for that
  payment address and return each output with its exact canonical Midgard
  Bech32 address.
- Exact-match diagnostics may expose a separately named parameter if needed,
  but the default wallet-facing address lookup is by underlying payment address,
  not by protected bit.

### Node And Provider UTxO API Shape

Node HTTP endpoints expose stored protocol bytes. Provider/Lucid APIs decode
those bytes into wallet-friendly objects.

Node response shape for `GET /utxos`, `GET /utxo`, and
`POST /utxos?by-outrefs`:

```ts
type NodeUtxoResponse = {
  readonly outref: string;      // TxOutRef CBOR hex
  readonly outputCbor: string;  // MidgardTxOutput CBOR hex
};
```

Rules:

- The old `value` field name for output CBOR is replaced by `outputCbor`; this
  is a pre-release API cleanup, not a compatibility change.
- `/utxos?address=<addr...>` and `/txs?address=<addr...>` parse the input with
  `decodeMidgardAddressText`, derive protected and unprotected canonical
  address-text variants for the same payment address, query both variants, and
  return exact stored outputs.
- Provider-side `getUtxos(address)` consistency checks must accept either
  protected or unprotected returned output addresses when they share the same
  underlying payment address as the requested address.
- Provider-side `getUtxoByOutRef` and `getUtxosByOutRefs` validate outrefs and
  decode `outputCbor`, but do not require an address filter.
- Public/provider decoded outputs use `address`, `assets`, `datum`, and
  `scriptRef`. They do not include a separate `protected` field.
- Diagnostic endpoints or logs may display derived protected status only as
  non-authoritative metadata.

### Versioning And Cutover

This repository has not released a live Midgard L2 wire format. This plan is a
pre-release format replacement, not a backward-compatible migration.

Rules:

- Do not add a compatibility mode, legacy alias, dual decoder, or fallback path
  for old CML-shaped L2 outputs.
- Do not require a native transaction version bump solely for compatibility
  with old development-state bytes.
- `MIDGARD_NATIVE_TX_VERSION` remains the current native tx version `1n` unless
  a separate explicit protocol decision changes it. This pre-release format
  replacement updates the single current version rather than preserving or
  negotiating an older format.
- Keep protocol-info, fixtures, docs, and builders aligned with the single
  current native transaction format.
- Cutover requires a clean local database/local durable state and a full
  explicit on-chain redeploy/reset before deposits, L2 transactions,
  commitments, merges, benchmarks, or readiness checks.
- This plan does not add a broad startup/readiness scan of historical durable
  state. The cutover procedure is clean state plus redeploy; after cutover,
  normal write/read/validation paths fail closed when they encounter malformed
  or non-canonical Midgard protocol bytes.

## Known Affected Areas

Core codec:

- `demo/midgard-core/src/codec/cbor.ts`
- `demo/midgard-core/src/codec/native.ts`
- `demo/midgard-core/src/codec/output.ts`
- `demo/midgard-core/src/codec/script-language-views.ts`
- `demo/midgard-core/src/codec/index.ts`
- `demo/midgard-core/package.json`
- new or updated `demo/midgard-core/src/codec/address.ts`
- new or updated `demo/midgard-core/src/codec/value.ts`
- new or updated `demo/midgard-core/src/codec/datum.ts`
- new or updated `demo/midgard-core/src/codec/native-script.ts`
- new or updated `demo/midgard-core/src/codec/versioned-script.ts`

Lucid and builder:

- `demo/lucid-midgard/src/core/types.ts`
- `demo/lucid-midgard/src/core/output.ts`
- `demo/lucid-midgard/src/core/scripts.ts`
- `demo/lucid-midgard/src/core/assets.ts`
- `demo/lucid-midgard/src/provider.ts`
- `demo/lucid-midgard/src/builder.ts`
- `demo/lucid-midgard/tests/*`

Validation:

- `demo/midgard-validation/src/midgard-output.ts`
- `demo/midgard-validation/src/script-source.ts`
- `demo/midgard-validation/src/script-context.ts`
- `demo/midgard-validation/src/phase-a.ts`
- `demo/midgard-validation/src/phase-b.ts`

Node durable state:

- `demo/midgard-node/src/commands/listen-router.ts`
- `demo/midgard-node/src/commands/utxos.ts`
- `demo/midgard-node/src/database/utils/ledger.ts`
- `demo/midgard-node/src/database/confirmedLedger.ts`
- `demo/midgard-node/src/database/latestLedger.ts`
- `demo/midgard-node/src/database/mempoolLedger.ts`
- `demo/midgard-node/src/database/mempool.ts`
- `demo/midgard-node/src/database/mempoolTxDeltas.ts`
- `demo/midgard-node/src/database/deposits.ts`
- `demo/midgard-node/src/database/addressHistory.ts`
- `demo/midgard-node/src/database/migrations/sql/0001_initial_schema.sql`
- `demo/midgard-node/src/fibers/fetch-and-insert-deposit-utxos.ts`
- `demo/midgard-node/src/transactions/state-queue/merge-to-confirmed-state.ts`
- `demo/midgard-node/src/transactions/submit-deposit.ts`
- `demo/midgard-node/src/workers/utils/mpt.ts`
- `demo/midgard-node/src/commands/submit-l2-transfer.ts`
- `demo/midgard-node/src/commands/protocol-info.ts`
- `demo/midgard-node/src/genesis.ts`
- `demo/midgard-node/src/tx-context.ts`
- `demo/midgard-node/src/utils.ts`

Manager, tooling, and scripts:

- `demo/midgard-manager/packages/tx-generator/src/lib/client/node-client.ts`
- `demo/midgard-manager/packages/tx-generator/src/**/*.ts`
- `demo/midgard-manager/packages/tx-generator/tests/node-client.test.ts`
- `demo/midgard-node/scripts/throughput-valid-stress.mjs`
- `demo/midgard-node/scripts/throughput-nominal-activity.mjs`
- any script that decodes, patches, displays, or reserializes Midgard output
  CBOR through CML

This list is a starting inventory. Implementation must run a fresh `rg` audit
for `CML.TransactionOutput`, `CML.Script`, `CML.NativeScript`,
`MIDGARD_PROTECTED_ADDRESS_HEADER_MASK`, `script_ref`, `scriptTxWits`, and
`decodeMidgardTxOutput` before final cleanup.

## Task Plan

### T01: Freeze The Midgard Wire Spec

Deliverables:

- Add or update protocol/spec documentation describing output, value, address,
  datum, native-script, versioned-script, and inline script-witness CBOR
  shapes.
- Define field names, tags, required fields, optional fields, invalid forms,
  canonicalization requirements, script hash formulas, native-script
  evaluation rules, public/core API boundaries, native tx version policy, and
  hash/root implications.
- State that this is a pre-release clean replacement of the current development
  output bytes, not a compatibility migration.

Acceptance criteria:

- The spec states that CML/Cardano `TransactionOutput` is not the Midgard L2
  output wire format.
- The spec states that `MidgardV1` scripts are recoverable from both output
  `script_ref` bytes and inline witness bytes.
- The spec states that datum hashes, PlutusV1 scripts, PlutusV2 scripts, and
  unknown versioned-script tags fail closed.
- The spec states that NativeCardano scripts are encoded by the Midgard-owned
  native-script codec.
- The spec states that script hash domain bytes are `0x00` for NativeCardano,
  `0x03` for PlutusV3, and `0x80` for MidgardV1.
- The spec states that `MIDGARD_NATIVE_TX_VERSION` remains `1n` for this
  pre-release replacement.
- The spec states that Lucid-facing output APIs keep `address`, `assets`,
  `datum`, and `scriptRef`, but do not carry a separate `protected` output
  field.
- The spec states that no default decoder accepts old CML-shaped L2 outputs or
  old raw inline script-witness byte strings.
- The spec states that cutover uses clean DB/local state plus full on-chain
  redeploy/reset, not an implicit or best-effort conversion.
- Protocol-info, docs, builders, and fixtures all describe one current native
  transaction format and do not advertise legacy output support.

### T02: Add Strict Midgard CBOR Substrate

Deliverables:

- Add strict CBOR reader/writer helpers in
  `demo/midgard-core/src/codec/cbor.ts` or a dedicated exported
  `demo/midgard-core/src/codec/strict-cbor.ts`.
- The reader must preserve original map-entry order, raw key bytes, and full
  source byte spans needed to detect duplicate keys and prove byte-exact
  canonical re-encoding.
- The writer must expose canonical unsigned integers, byte strings, arrays, and
  maps without relying on generic JavaScript `Map` insertion behavior for
  protocol-critical ordering.
- Add a fixture harness for canonical and non-canonical byte cases that every
  higher-level codec can reuse.

Acceptance criteria:

- Strict CBOR decode rejects trailing bytes, duplicate map keys by raw key
  bytes, indefinite arrays/maps/byte strings, non-minimal integer and length
  encodings, tags, floats, simple values, `undefined`, and malformed major
  types.
- Strict CBOR helpers make it possible for each protocol codec to require
  `encode(decode(bytes)) === bytes`.
- Non-asset maps can be decoded with ordered integer-key expectations.
- Asset maps can be decoded with custom raw-byte lexicographic ordering instead
  of generic CBOR map ordering.
- Negative fixtures cover non-minimal integers, non-minimal lengths, duplicate
  keys, unsorted maps, indefinite values, tags, floats, undefined, and trailing
  bytes.
- Production output, value, datum, native-script, versioned-script, and inline
  script-witness codecs use these strict helpers rather than ad hoc CBOR decode
  behavior.

### T03: Add Core Types, Codec API Surface, And Package Exports

Deliverables:

- Add exported Midgard-owned protocol types in `demo/midgard-core/src/codec`.
- Add public codec modules for address, value, datum, native script,
  versioned script, output, and strict CBOR helpers. Expected files are
  `address.ts`, `value.ts`, `datum.ts`, `native-script.ts`,
  `versioned-script.ts`, `output.ts`, and `cbor.ts` or `strict-cbor.ts` under
  `demo/midgard-core/src/codec`.
- Update `demo/midgard-core/src/codec/index.ts`,
  `demo/midgard-core/src/index.ts`, `demo/midgard-core/package.json` exports,
  and the `tsup` build script entrypoints for every public codec module.
- Define adapter-only modules for conversion to/from Lucid/CML/Cardano
  structures where needed.

Acceptance criteria:

- The public codec export surface exposes `MidgardTxOutput`,
  `MidgardAddress`, `MidgardValue`, `MidgardDatum`,
  `MidgardVersionedScript`, `MidgardNativeScript`, and the named encode/decode
  helpers for each.
- `@al-ft/midgard-core/codec` re-exports the public codec APIs.
- Any directly imported public codec module has a matching package subpath
  export and `tsup` entrypoint. Required subpath exports include
  `./codec/address`, `./codec/value`, `./codec/datum`,
  `./codec/native-script`, `./codec/versioned-script`, `./codec/output`, and
  the strict CBOR module selected by T02.
- Downstream packages do not import new codec modules through deep source paths.
- Adapter-only helpers are explicitly named as adapters and are not used by
  authoritative protocol decode paths.
- `pnpm --dir demo/midgard-core build` succeeds with the new entrypoints.

### T04: Add Midgard Value Codec

Deliverables:

- Add encode/decode helpers for Midgard output values.
- Add validation for coin, policy IDs, asset names, quantities, duplicate
  assets, empty inner maps, and canonical ordering.
- Add adapter helpers between Midgard values and CML/Lucid value types outside
  the protocol codec path where needed.

Acceptance criteria:

- A value round trip preserves lovelace and multi-asset quantities exactly.
- Negative output quantities reject.
- Zero-quantity asset entries reject on decode and are omitted or rejected on
  encode.
- Invalid policy ID lengths reject.
- Asset names over 32 bytes reject.
- Empty inner asset maps reject.
- Policy IDs and asset names are emitted in lexicographic raw-byte order,
  independent of construction/insertion order.
- Value encode/decode uses a custom asset-map encoder/decoder and does not use
  generic `encodeCbor(Map)` for the nested policy/asset maps.
- Value decode rejects any input whose canonical re-encode does not byte-match
  the original bytes.
- Validation and tx balancing can sum decoded Midgard values without decoding
  a CML `Value` from output bytes.

### T05: Add Midgard Datum Codec

Deliverables:

- Add `MidgardDatum` encode/decode helpers for inline datum only.
- Implement current Cardano Conway PlutusData canonicalization in the
  `midgard-core` datum codec.
- Treat CML, if used at all, as a fixture/test oracle or explicit adapter, not
  as the authoritative datum decoder in production protocol paths.

Acceptance criteria:

- Inline datum bytes round-trip through the Midgard datum codec and require
  byte-exact canonical re-encode equality.
- Datum hashes are not representable and reject anywhere an output datum is
  decoded.
- Constructor, map, list, integer, and byte-string PlutusData forms match the
  current Cardano Conway ledger encoding.
- Compact constructor tags and the general constructor form follow Cardano's
  current PlutusData rules.
- Non-canonical PlutusData, malformed constructors, invalid PlutusData tags,
  duplicate/non-canonical map keys, indefinite collections/byte strings,
  non-minimal integers/lengths, and trailing bytes reject.
- Output decode rejects before validation if the inner datum bytes are invalid
  or non-canonical.

### T06: Add Midgard Address Codec

Deliverables:

- Add encode/decode helpers for `MidgardAddress`.
- Add encode/decode helpers for canonical Midgard Bech32 address text using
  `addr` and `addr_test` HRPs.
- Implement protected status with the `0x80` header bit in the core address
  codec.
- Add helpers to derive protected status, recover payment credentials, and
  convert to/from supported Cardano/Lucid address formats at adapter
  boundaries.
- Add helpers to derive the unprotected and protected canonical Midgard Bech32
  variants for the same underlying payment address.
- Export the required helpers `decodeMidgardAddressBytes`,
  `encodeMidgardAddressBytes`, `decodeMidgardAddressText`,
  `encodeMidgardAddressText`, `isProtectedMidgardAddress`,
  `protectMidgardAddress`, `unprotectMidgardAddress`,
  `midgardAddressLookupVariants`, and `paymentCredentialFromMidgardAddress`.
- Remove CML address normalization from the production output codec path.
- Keep old byte-patch helpers only as explicitly named diagnostic utilities if
  still needed during cutover diagnostics.

Acceptance criteria:

- Protected output decode derives protected status from address bytes.
- Protected and unprotected address bytes round-trip without CML TxOut
  normalization or output-byte patching.
- Base and enterprise Shelley payment address bytes and Bech32 text are
  accepted as unprotected Midgard addresses.
- Protected base and enterprise Midgard address bytes and Bech32 text round-trip
  with the same `addr` or `addr_test` HRP and the protected bit preserved.
- Base addresses are exactly 57 bytes and enterprise addresses are exactly 29
  bytes.
- `addr` text requires network id `1`; `addr_test` text requires network id
  `0`; other network ids reject unless a future explicit policy changes this.
- Cardano address adapters reject pointer, Byron/bootstrap, stake/reward,
  reserved, and unknown address families.
- Raw Midgard output decode rejects pointer, Byron/bootstrap, reserved, and
  unknown families after clearing the protected bit.
- Raw Midgard output decode rejects stake/reward address lengths/forms that are
  not valid protected or unprotected base/enterprise Midgard addresses.
- Phase B derives receive/pubkey protection requirements from the decoded
  Midgard address.
- The protected mask is centralized in the Midgard core address codec.
- Old CML TxOut protected-address patching rejects in production output decode.

### T07: Add Midgard Native Script Codec

Deliverables:

- Add `MidgardNativeScript` encode/decode helpers.
- Add canonical recursive validation for the six supported native-script
  variants.
- Add Midgard-owned native-script hashing and verification.
- Native-script decode returns both parsed AST and canonical CBOR bytes.
- Implement Cardano-identical native-script semantics for empty `all`, empty
  `any`, `atLeast`, time locks, hash domain bytes, signer inputs, and
  effective size/depth constraints.
- Add explicit L1/Cardano adapter helpers only where a real Cardano transaction
  needs conversion.

Acceptance criteria:

- Native script CBOR round-trips through the Midgard-owned codec.
- Native script decode rejects unknown variants, malformed key hashes,
  non-canonical integers/lengths, duplicate semantic forms, tags, floats,
  indefinite arrays/maps/byte strings, and trailing bytes.
- Native script hash fixtures prove
  `hash = blake2b224(0x00 || canonical_native_script_cbor)` and match Cardano
  native-script hashes without using CML as the production hashing authority.
- Native script evaluation fixtures prove empty `all` is true, empty `any` is
  false, `atLeast(0, children)` is true, `atLeast(n, children)` is false when
  fewer than `n` children evaluate true, `[4, slot]` requires an existing lower
  bound `>= slot`, and `[5, slot]` requires an existing upper bound `< slot`.
- Native script verifier inputs are explicit and limited to
  `validityIntervalStart?: bigint`, `validityIntervalEnd?: bigint`, and
  `witnessSigners: ReadonlySet<keyHashHex>`.
- The parsed AST is used for verification and the canonical bytes are used for
  hashing and fixture equality.
- Native script parsing/evaluation has no Midgard-specific semantic recursion
  or node-count limit stricter than Cardano under the existing serialized
  transaction/script-size limits, and implementation cannot crash the process on
  deeply nested but otherwise valid native-script CBOR.
- Native script signature/time verification works without
  `CML.NativeScript.verify` in production Midgard validation.
- CML native-script decoding is absent from output decode, script-source
  resolution, durable state, Phase A, and Phase B production paths.

### T08: Add Midgard Versioned Script Codec

Deliverables:

- Add `MidgardVersionedScript` encode/decode helpers.
- Add explicit versioned-script tag constants. Reuse the existing numeric
  values `2` for PlutusV3 and `0x80` for MidgardV1, and add
  `NativeCardano = 0`.
- Keep NativeCardano out of Plutus script language views and cost-model maps;
  native scripts do not have a Plutus cost model.
- Route `NativeCardano` payloads through the new Midgard native-script codec.
- Decode `NativeCardano` into a versioned script value that includes canonical
  native-script bytes plus the parsed native-script AST.

Acceptance criteria:

- `[0, native_script_cbor]` decodes as `NativeCardano` only if the native script
  payload is valid canonical Midgard native-script CBOR.
- `[2, raw_uplc]` decodes as `PlutusV3`.
- `[128, raw_uplc]` decodes as `MidgardV1`.
- Same raw UPLC bytes tagged as `PlutusV3` and `MidgardV1` produce different
  versioned-script CBOR and different containing output CBOR.
- PlutusV1 and PlutusV2 forms reject everywhere in Midgard protocol bytes.
- Unknown tags reject.
- The decoder never infers `MidgardV1` from a PlutusV3 CML wrapper or trusted
  metadata.
- Script hashes are computed with the fixed domain bytes `0x00` for
  NativeCardano, `0x03` for PlutusV3, and `0x80` for MidgardV1.
- PlutusV3 and MidgardV1 UPLC payload bytes are treated as Cardano-style opaque
  script bytes at codec decode. Empty payloads are accepted at serialization and
  hash time and fail only if later execution/evaluation rejects them.
- No authoritative versioned-script path depends on `CML.Script`.

### T09: Add Midgard Output Codec

Deliverables:

- Add `encodeMidgardTxOutput` and `decodeMidgardTxOutput` that compose the
  strict address, value, datum, and versioned-script codecs.
- Return structured decoded values without `CML.TransactionOutput`.
- Make output decode the only authoritative parser for Midgard L2 output bytes.

Acceptance criteria:

- `demo/midgard-core/src/codec/output.ts` owns full output encode/decode, not
  only protected-address byte patching.
- Output decode rejects trailing bytes, duplicate map keys, indefinite CBOR,
  unknown keys, missing required keys, wrong field types, non-minimal
  integers/lengths, non-canonical map ordering, tags, floats, undefined, datum
  hashes, and semantic alternate forms.
- Output decode rejects any input whose canonical re-encode does not byte-match
  the original bytes.
- Output decode rejects PlutusV1, PlutusV2, unknown versioned-script tags,
  unsupported address families, non-canonical values, and non-canonical inline
  datum bytes.
- Output encode emits keys `0`, `1`, optional `2`, and optional `3` in canonical
  order.
- No output codec path depends on `CML.TransactionOutput`.

### T10: Integrate New Outputs And Script Witnesses Into Native Tx Codec

Deliverables:

- Update Midgard native tx materialization and decode paths so output preimages
  are lists of Midgard output CBOR byte strings.
- Update witness-set script preimages so inline script witnesses are canonical
  lists of `MidgardVersionedScript` values.
- Define the script witness preimage exactly as
  `script_tx_wits = [MidgardVersionedScript*]`, where each script is the
  canonical `[language_tag, script_bytes]` array.
- Ensure output roots and script-witness roots are computed over the new
  canonical bytes.
- Move CML/Cardano conversion boundary decisions for `native.ts` into this
  task, before validation cutover.
- Keep Cardano conversion helpers isolated from production Midgard L2 decode.

Acceptance criteria:

- A native transaction built with new outputs decodes through shared
  `midgard-core` codecs.
- Output root/preimage consistency checks use the new output bytes.
- Script-witness root/preimage consistency checks use the new
  `MidgardVersionedScript` witness-list bytes.
- Old CML-shaped output bytes reject in the production native decode or Phase A
  path.
- Old raw inline script-witness byte strings reject in the production native
  decode or Phase A path.
- Transaction ID derivation remains compact body hash and changes when output
  bytes or script-witness bytes change.
- `native.ts` does not parse production Midgard output preimages as
  `CML.TransactionOutput`.
- `native.ts` does not parse production Midgard script witnesses as
  `CML.Script` or `CML.NativeScript`.
- Cardano conversion helpers reject Midgard-only semantics such as MidgardV1
  scripts and protected Midgard addresses unless a future lossless adapter is
  explicitly specified.

### T11a: Migrate Validation Output Model

Deliverables:

- Update `demo/midgard-validation` output decoding and Phase A/Phase B logic to
  consume Midgard output structures.
- Replace uses of `output.address()`, `output.amount()`, `output.datum()`, and
  `output.script_ref()` in validation with Midgard-owned fields.

Acceptance criteria:

- `demo/midgard-validation/src/midgard-output.ts` no longer returns
  `CML.TransactionOutput`.
- Phase A rejects malformed Midgard output CBOR before any CML TxOut decode is
  attempted.
- Phase A rejects malformed Midgard script witnesses before validation uses
  them.
- Validation value accounting uses `MidgardValue`, not decoded CML `Value` from
  output bytes.
- Receive/pubkey protection checks derive protected status from decoded
  Midgard address bytes.
- Existing validation reject codes are preserved or intentionally updated with
  documented replacements.

### T11b: Migrate Script Source Resolution

Deliverables:

- Update script-source decoding so both inline and reference sources come from
  `MidgardVersionedScript`.
- Remove reference-script metadata requirements that exist only to recover
  MidgardV1 from CML PlutusV3 bytes.
- Remove compatibility that treats decoded PlutusV3 and supplied MidgardV1 as
  equivalent because their raw UPLC bytes match.

Acceptance criteria:

- Phase B resolves `MidgardV1` reference scripts directly from decoded output
  bytes.
- Phase B resolves `MidgardV1` inline witnesses directly from decoded witness
  bytes.
- Receive purposes require `MidgardV1` by decoded versioned-script tag or a
  matching Midgard script source, not by trusted metadata.
- PlutusV3 sources no longer automatically expose a MidgardV1 hash unless the
  decoded versioned-script tag is `MidgardV1` or an explicit future
  dual-language source format is specified.
- A PlutusV3 source and a MidgardV1 source with the same raw UPLC bytes remain
  different script sources, hashes, roots, and language identities.

### T11c: Migrate Native Script Verification

Deliverables:

- Replace CML native-script verification with Midgard-owned native-script
  verification in validation.
- Wire explicit verifier inputs from transaction signers and validity interval
  bounds.

Acceptance criteria:

- NativeCardano scripts in reference scripts and inline witnesses verify through
  the Midgard native-script verifier.
- Missing signers, signer matches, empty `all`, empty `any`, `atLeast`, lower
  bound, upper bound, and missing-bound cases follow the native-script rules in
  this plan.
- No production Phase A or Phase B path calls `CML.NativeScript.verify`.

### T11d: Migrate Script Context And Language Views

Deliverables:

- Update script-context construction to use Midgard output fields.
- Update required language views and cost-model inputs to use resolved
  `MidgardVersionedScript` languages.
- Keep NativeCardano out of Plutus cost-model/language-view maps.

Acceptance criteria:

- Required script language views are computed from resolved script languages
  and include `MidgardV1` only when a MidgardV1 source is actually used.
- NativeCardano scripts do not create Plutus cost-model entries.
- Script context input/reference-input ordering remains lexicographic by
  `TxOutRef`, and output order remains authored order.
- Existing script-context tests are updated to assert Midgard output fields
  instead of CML output methods.

### T12: Migrate Lucid-Midgard Builder And Provider Types

Deliverables:

- Update `lucid-midgard` output builders to emit new Midgard output CBOR.
- Update `lucid-midgard` inline script attachment to emit
  `MidgardVersionedScript` witnesses.
- Update UTxO normalization and provider readback to decode new output bytes.
- Keep the Lucid-facing output shape using `address`, `assets`, `datum`, and
  `scriptRef`. The `address` string is canonical Midgard Bech32 and may contain
  the protected bit.
- Remove the Lucid-facing output `protected` field and any output authoring
  option that carries protection separately from the address string. Protection
  is encoded by passing a protected Midgard Bech32 address.
- Replace CML script-ref normalization with Midgard versioned-script
  normalization.
- Remove the compatibility rule that treats a supplied `MidgardV1` script ref
  as matching a decoded `PlutusV3` CML script ref.
- Preserve CML address/value convenience adapters only at API boundaries.
- Update provider decoding from node UTxO responses to read `outputCbor`.
- Provider address consistency checks compare payment-address identity after
  clearing the protected bit, not literal Bech32 equality.

Acceptance criteria:

- Creating an output with `scriptRef: { type: "MidgardV1", ... }` decodes
  back as `MidgardV1`.
- Attaching an inline `MidgardV1` script witness decodes back as `MidgardV1`.
- Creating an output whose `address` is protected Midgard Bech32 encodes the
  protected flag in Midgard address bytes and decodes back to the same canonical
  protected Midgard Bech32 address.
- Public/provider UTxO output objects do not include `protected`; clients derive
  protection from `output.address` with the Midgard address codec.
- Builder completion no longer accepts a CML TxOut CBOR as authoritative
  Midgard output CBOR by default.
- Provider UTxO consistency checks compare structured Midgard output fields
  against decoded Midgard output bytes.
- Provider `getUtxos(address)` accepts returned protected and unprotected output
  addresses when they share the same underlying payment address as the requested
  Midgard address.
- Provider `getUtxoByOutRef` and `getUtxosByOutRefs` decode `outputCbor` and
  validate requested outrefs without requiring an address filter.
- A supplied `MidgardV1` script ref and a decoded `PlutusV3` script ref with
  the same raw UPLC bytes are not considered compatible.
- A supplied inline `MidgardV1` witness and decoded inline `PlutusV3` witness
  with the same raw UPLC bytes are not considered compatible.

### T13: Update Durable Writers And On-Demand Readers

Deliverables:

- Ensure ledger tables continue to store output bytes, now in the Midgard-owned
  format.
- Keep ledger and deposit `address TEXT` columns as canonical Midgard Bech32
  address text derived from the authoritative output bytes.
- Update fresh genesis seeding, deposit ingestion/projection, mempool ledger
  writes, mempool tx deltas, latest/confirmed ledger writes, block
  confirmation, merge, and MPT state writes.
- Update durable readers for `confirmed_ledger`, `latest_ledger`,
  `mempool_ledger`, `deposits_utxos.ledger_output`, `mempool_tx_deltas`, and
  MPT values where those readers decode Midgard output bytes.
- This task does not add a startup/readiness full-state scanner. Malformed
  durable bytes fail when a normal read/validation path touches them.

Acceptance criteria:

- Fresh genesis writes new Midgard output CBOR.
- Deposit projection writes new Midgard output CBOR to
  `deposits_utxos.ledger_output`.
- Mempool admission and produced-output deltas write new Midgard output CBOR.
- Confirmed/latest ledger updates write new Midgard output CBOR.
- Confirmed/latest/mempool/deposit address columns store exact canonical
  Midgard Bech32 text, including the protected bit when set.
- MPT seeding and updates hash/store values containing new Midgard output CBOR.
- No durable writer constructs a Midgard L2 output by creating a
  `CML.TransactionOutput`.
- Durable readers decode stored bytes through the shared Midgard output codec.
- Durable readers verify any stored `address TEXT` value matches canonical
  Midgard Bech32 text derived from the decoded output bytes when that row is
  read.
- Normal read/validation paths reject old CML-shaped output bytes and malformed
  new-format output bytes instead of attempting fallback decode.
- Normal read/validation paths reject old raw inline witness bytes if durable tx
  state includes script-witness preimages.
- There is no fallback path that silently rewrites old durable bytes.

### T14: Update Node API, Commands, And Logs

Deliverables:

- Update command/API formatting paths that currently decode CML TxOuts for
  address, value, datum, or script-ref display.
- Update JSON UTxO response shapes to use `{ outref, outputCbor }`.
- Update `/utxos` and `/txs` address lookups to parse Midgard Bech32 text and
  query both protected/unprotected canonical address variants for the same
  payment address.
- Update internal logs that report decoded output/script data.

Acceptance criteria:

- `GET /utxos`, `GET /utxo`, and `POST /utxos?by-outrefs` return UTxO entries
  with `outref` and `outputCbor`; normal responses no longer use `value` as the
  output-CBOR field name.
- `/utxos`, `/utxo`, status/debug commands, and internal logs report
  `MidgardV1` script refs accurately.
- `/utxos?address=<addr...>` / `/utxos?address=<addr_test...>` and
  address-history lookups parse the input as Midgard Bech32 address text, clear
  the protected bit for payment-address lookup, query both protected and
  unprotected canonical Midgard address text variants, and return each output
  with its exact canonical Midgard Bech32 address.
- API responses include canonical Midgard address text for each output. Any
  wallet-compatible Cardano address display is derived by clearing the
  protected bit and is not the authoritative stored address for protected
  outputs.
- API/provider output objects do not include a separate `protected` flag in the
  normal UTxO output shape. Diagnostic endpoints may display derived protection
  only as non-authoritative debug metadata.
- Inline witness diagnostics report `NativeCardano`, `PlutusV3`, and
  `MidgardV1` by decoded Midgard tag.
- No production API path claims a MidgardV1 reference script is PlutusV3.
- No command decodes Midgard L2 output bytes with `CML.TransactionOutput`.

### T15: Update Manager Tooling And Scripts

Deliverables:

- Update manager tx-generator client and tests that currently decode or create
  CML `TransactionOutput` values for Midgard L2 outputs.
- Update manager tx-generator client to consume node `outputCbor` response
  fields.
- Update throughput/stress scripts that parse, patch, display, or reserialize
  Midgard output CBOR through CML.
- Audit any remaining local scripts for CML TxOut output handling.

Acceptance criteria:

- Manager tx-generator decodes node output bytes through the shared Midgard
  output codec.
- Manager tests build fixtures using Midgard output CBOR, not
  `CML.TransactionOutput`.
- Throughput/stress scripts do not use CML to inspect or modify Midgard output
  CBOR.
- Any script that cannot support the new format is removed, updated, or clearly
  marked non-production diagnostic-only.

### T16: Document Operator Clean Redeploy Procedure

Deliverables:

- Document the clean cutover procedure for development/test operators.
- Name all local durable state that must be cleared.
- Name the on-chain state that must be redeployed/reset.
- State that mixed clean-local/old-on-chain state is invalid.

Acceptance criteria:

- The procedure requires stopping node processes before reset.
- The procedure covers Postgres ledgers, mempool/deposit state, MPT LevelDB
  stores, cached deployment/genesis/protocol-state files, and benchmark/test
  state.
- The procedure requires full on-chain redeploy/reset of contracts, reference
  scripts, scheduler, state queue, hub oracle, operator set, and protocol UTxOs
  before deposits, L2 transactions, commitments, merges, benchmarks, or
  readiness checks.
- The procedure explicitly forbids combining clean local state with previously
  deployed on-chain protocol state.
- The procedure states that the implementation does not scan historical durable
  state at startup to detect old development formats. Operators must perform the
  clean-state plus redeploy cutover before running the node with the new codec.

### T17: Add Conformance And Regression Tests

Deliverables:

- Add unit tests for address, value, datum, native-script, versioned-script, and
  output codecs.
- Add native tx round-trip tests with new output bytes and new inline script
  witness bytes.
- Add validation tests for MidgardV1 reference-script and inline-witness
  recovery.
- Add golden CBOR fixtures for minimal output, protected base address,
  protected enterprise address, inline datum, multi-asset ordering,
  NativeCardano versioned script, PlutusV3 versioned script, MidgardV1
  versioned script using the same raw UPLC bytes, inline MidgardV1 witness,
  NativeCardano hash/verifier edge cases, and a whole native tx fixture with
  output root, script-witness root, and tx id.
- Add negative tests for invalid, non-canonical, and legacy shapes.
- Add package export/build tests or build verification for all new
  `@al-ft/midgard-core` codec entrypoints.
- Add final verification commands to the implementation notes or PR checklist.

Acceptance criteria:

- Strict CBOR substrate fixtures reject non-minimal integers/lengths,
  non-canonical map ordering, duplicate keys, indefinite values, tags, floats,
  undefined, and trailing bytes.
- `MidgardV1` versioned script round-trips from output CBOR as `MidgardV1`.
- `MidgardV1` inline script witness round-trips from tx CBOR as `MidgardV1`.
- `PlutusV3` versioned script round-trips from output CBOR as `PlutusV3`.
- `NativeCardano` versioned script round-trips through the Midgard native-script
  codec.
- NativeCardano hash fixtures prove the `0x00` domain byte. PlutusV3 hash
  fixtures prove the `0x03` domain byte. MidgardV1 hash fixtures prove the
  existing `0x80` domain byte.
- NativeCardano verifier fixtures cover empty `all`, empty `any`, `atLeast`
  bounds, lower-bound inclusive time locks, upper-bound exclusive time locks,
  missing validity bounds, signer success, and signer failure.
- Datum hash output forms reject.
- Datum fixtures prove current Cardano Conway PlutusData canonicalization and
  rejection of non-canonical inner datum payloads.
- Protected output status round-trips through Midgard address bytes.
- Public/provider UTxO fixtures use canonical Midgard Bech32 in `address` and
  do not include a separate `protected` field.
- Pointer, Byron/bootstrap, stake/reward adapter inputs, reserved, and unknown
  address families reject according to the address rules above.
- Unknown output keys reject.
- Unknown versioned-script tags reject.
- PlutusV1 and PlutusV2 versioned-script tags reject.
- Old CML-shaped output CBOR rejects in production validation.
- Old raw inline script-witness byte strings reject in production validation.
- Output root changes when only versioned-script language changes from PlutusV3
  to MidgardV1.
- Script-witness root changes when only inline witness language changes from
  PlutusV3 to MidgardV1.
- Value asset order tests prove encoder output is independent of construction
  order, asset names over 32 bytes reject, and decoder rejects non-canonical
  ordering produced by generic CBOR map ordering when it differs from raw-byte
  lexicographic order.
- Non-minimal integers/lengths, non-canonical map ordering, duplicate keys,
  duplicate assets, indefinite values, tags, floats, undefined, and trailing
  bytes reject.
- Any output/value/datum/native-script/versioned-script bytes whose canonical
  re-encode differs from the original bytes reject.
- Final targeted verification includes:
  - `pnpm --dir demo/midgard-core build`
  - `pnpm --dir demo/midgard-core test`
  - `pnpm --dir demo/midgard-validation build`
  - `pnpm --dir demo/midgard-validation test`
  - `pnpm --dir demo/lucid-midgard build`
  - `pnpm --dir demo/lucid-midgard test`
  - `pnpm --dir demo/midgard-manager/packages/tx-generator build`
  - `pnpm --dir demo/midgard-manager/packages/tx-generator test`
  - targeted `midgard-node` native transaction, validation, durable
    writer/reader, MPT, API, and clean deposit-flow emulator tests updated by
    this workstream.

### T18: Clean Up CML Leakage From Core And Validation Paths

Deliverables:

- Remove CML `TransactionOutput` usage from the authoritative output codec.
- Remove CML `Script` usage from the authoritative versioned-script codec.
- Remove CML `NativeScript` usage from authoritative native-script decode,
  hash, verify, and script-source resolution.
- Audit remaining CML imports in `demo/midgard-core/src/codec`,
  `demo/midgard-validation`, `demo/lucid-midgard`, and manager/script paths.
- Classify each remaining import as one of:
  - real Cardano/L1 conversion adapter
  - JSON/Bech32/Lucid convenience adapter
  - explicitly named diagnostic-only helper
  - candidate for removal in a follow-up
- This is a manual implementation audit, not a static/CI gate.

Acceptance criteria:

- `demo/midgard-core/src/codec/output.ts` has no CML dependency for normal
  output encode/decode.
- The Midgard native-script codec has no CML dependency for decode, encode,
  hash, or verify.
- Any remaining CML use in `demo/midgard-core/src/codec/native.ts` is limited
  to real Cardano conversion or explicitly documented adapter code.
- There is no production path where CML `Script` is used to represent a
  MidgardV1 versioned script.
- There is no production path where CML `TransactionOutput` is used to decode
  authoritative Midgard L2 output bytes.
- There is no production path where CML `NativeScript` is used to decode or
  verify authoritative Midgard `NativeCardano` script bytes.
- The implementation notes include the final `rg` audit command(s) used and a
  short classification of every remaining CML hit in touched packages.

## Overall Acceptance Criteria

The workstream is complete when all of the following are true:

- Midgard L2 output bytes are defined by `demo/midgard-core/src/codec`, not by
  CML `TransactionOutput`.
- Midgard inline script-witness bytes are defined by
  `MidgardVersionedScript`, not raw script bytes or CML `Script`.
- MidgardV1 versioned scripts are recoverable from output CBOR alone.
- MidgardV1 inline script witnesses are recoverable from transaction CBOR
  alone.
- NativeCardano scripts are supported through a Midgard-owned native-script
  codec and verifier.
- Protected output status is recoverable from output CBOR alone and does not
  require a standalone output boolean.
- Protected output status is encoded in and derived from Midgard address bytes.
- Public/provider output shapes encode protection only in canonical Midgard
  Bech32 `address` text and do not include a separate `protected` field.
- `@al-ft/midgard-core` package exports and build entrypoints expose every new
  public codec module.
- Datum hashes, PlutusV1 scripts, PlutusV2 scripts, unknown output fields,
  unsupported Midgard address families, unsupported Cardano address adapter
  inputs, and unknown versioned-script tags fail closed.
- `midgard-node`, `midgard-validation`, `lucid-midgard`, manager tooling, and
  scripts share the same core output/script codec where they touch Midgard
  protocol bytes.
- No default compatibility layer accepts old CML-shaped L2 output bytes or old
  raw inline script-witness bytes.
- Clean DB/local-state reset plus full on-chain redeploy/reset is the documented
  cutover path.
- The implementation does not add a broad startup/readiness scan of historical
  durable state; incompatible bytes fail closed when normal decode, read, or
  validation paths encounter them.
- Protocol-info reporting, docs, builders, and test fixtures describe the
  single current pre-release native transaction format.

## Remaining Implementation Notes

- The protected-address `0x80` decision means Midgard protected enterprise
  address bytes overlap Cardano reward-address header byte values. This is
  acceptable only because Midgard owns the L2 address format. Cardano-address
  adapters must reject stake/reward addresses before conversion.
- A fresh pre-implementation audit should confirm every current CML TxOut,
  CML Script, CML NativeScript, protected-address patch, and raw script witness
  usage has an assigned task above.
