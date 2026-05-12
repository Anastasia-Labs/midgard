# Migration: replace CBOR transaction codec with `@al-ft/midgard-ts` binary encoding

**Goal:** Replace the CBOR-based "Midgard native" transaction encoding (the
`MidgardNativeTx*` "root + preimage-CBOR" model in `@al-ft/midgard-core/codec`)
with the 8-byte-aligned canonical binary codec implemented in
`@al-ft/midgard-ts`. Adopt midgard-ts `Transaction`/`TransactionBody`/
`TransactionOutput`/… types everywhere they flow. Everything except the
encoding (CLI surface, DB schema column *names*, validation rules, MPT keying,
test structure, benchmark structure) stays as-is.

**Non-goals / what stays CBOR:** Inner Cardano structures Midgard does *not*
re-encode — Plutus datums, scripts (`script_ref`), redeemers, raw L1 address
bytes, cost-model/script-language views, and the `outref`/`TransactionInput`
CBOR used as MPT/ledger keys. These continue to use the CBOR helpers from
`@al-ft/midgard-core` (`codec/cbor`, `codec/address`, `codec/datum`,
`codec/native-script`, `codec/versioned-script`, `codec/value`,
`codec/script-language-views`, `codec/errors`). midgard-ts already stores
`datum` / `script_ref` / `redeemers` / `native_scripts` as raw bytes.

**Tracking:** Tick boxes here as work lands; commit the doc with the code.
Re-derive a per-session task list from the next unchecked items.

---

## Data-model mapping (old → new)

| Old (`@al-ft/midgard-core/codec/native`) | New (`@al-ft/midgard-ts`) |
| --- | --- |
| `MidgardNativeTxFull { version, compact, body, witnessSet }` | `Transaction { body, witness_set, is_valid }` (no version byte) |
| `MidgardNativeTxBodyFull` with `spendInputsRoot`+`spendInputsPreimageCbor`, `outputsRoot`+`outputsPreimageCbor`, `referenceInputsRoot`+`referenceInputsPreimageCbor`, `mintRoot`+`mintPreimageCbor`, `requiredSignersRoot`+`requiredSignersPreimageCbor`, `requiredObserversRoot`+`requiredObserversPreimageCbor`, `scriptIntegrityHash`, `auxiliaryDataHash`, `fee`, `validityIntervalStart`, `validityIntervalEnd`, `networkId` | `TransactionBody { inputs: OutputReference[], outputs: TransactionOutput[], fee, ttl?, auxiliary_data_hash?, validity_interval_start?, mint?: Mint, script_data_hash?, required_signers?: Hash28[], network_id?, reference_inputs?: OutputReference[], required_observers?: Hash28[] }` |
| `MidgardNativeTxWitnessSetFull` with `addrTxWitsRoot`+`addrTxWitsPreimageCbor`, `scriptTxWitsRoot`+`scriptTxWitsPreimageCbor`, `redeemerTxWitsRoot`+`redeemerTxWitsPreimageCbor` | `TransactionWitnessSet { vkey_witnesses?: VKeyWitness[] (={vkey,signature} raw bytes), scripts?: VersionedScript[] ({language: NativeCardano\|PlutusV3\|MidgardV1, bytes}), redeemers?: Uint8Array (raw CBOR) }` |
| `MidgardVersionedScript { language, scriptBytes, nativeScript? }` (midgard-core) | `VersionedScript { language: "NativeCardano" \| "PlutusV3" \| "MidgardV1", bytes }` (midgard-ts `types/script.ts`) |
| `MidgardNativeTxCompact { version, transactionBodyHash, transactionWitnessSetHash, validity }` | `TransactionCompact { transaction_body_hash, transaction_witness_set_hash, is_valid }` — derive via `deriveTransactionCompact(tx)` |
| `MidgardNativeTxBodyCompact` (roots inline) | `TransactionBodyCompact { inputs_hash, outputs_hash, fee, ttl?, auxiliary_data_hash?, validity_interval_start?, mint_hash?, script_data_hash?, required_signers_hash?, network_id?, reference_inputs_hash?, required_observers_hash? }` — derive via `deriveTransactionBodyCompact(body)` |
| `MidgardNativeTxWitnessSetCompact` | `TransactionWitnessSetCompact { vkey_witnesses_hash?, native_scripts_hash?, redeemers_hash?, plutus_v3_scripts_hash? }` — derive via `deriveTransactionWitnessSetCompact(ws)` |
| `MidgardTxOutput` (midgard-core: `{address, value: MidgardValue, datum?, script_ref?}`) | `TransactionOutput { address: Uint8Array, value: Value, datum?: Uint8Array, script_ref?: Uint8Array }` (`Value = {type:"Coin",coin} | {type:"MultiAsset",coin,assets:[[PolicyId,[[name,amt]]]]}`) |

### Function/name mapping

| Old | New |
| --- | --- |
| `encodeMidgardNativeTxFull(tx)` | `encodeTransaction(tx)` |
| `decodeMidgardNativeTxFull(bytes)` | `decodeTransaction(bytes)` |
| `encodeMidgardNativeTxCompact` / `decodeMidgardNativeTxCompact` | `encodeTransactionCompact` / `decodeTransactionCompact` |
| `encodeMidgardNativeTxBodyCompact` / `decodeMidgardNativeTxBodyCompact` | `encodeTransactionBodyCompact` / `decodeTransactionBodyCompact` |
| `encodeMidgardNativeTxWitnessSetCompact` / `decode…` | `encodeTransactionWitnessSetCompact` / `decode…` |
| `encodeMidgardNativeTxBodyFull` / `decode…` | `encodeTransactionBody` / `decodeTransactionBody` |
| `deriveMidgardNativeTxBodyCompactFromFull(body)` | `deriveTransactionBodyCompact(body)` |
| `deriveMidgardNativeTxWitnessSetCompactFromFull(ws)` | `deriveTransactionWitnessSetCompact(ws)` |
| `deriveMidgardNativeTxCompact(body, ws, validity)` | `deriveTransactionCompact(tx)` |
| `computeMidgardNativeTxIdFromFull(tx)` / `…FromCompact` | `transactionId(tx)` / `transactionBodyHash(body)` (both `Uint8Array`; wrap in `Buffer` where `.equals` / `.toString("hex")` are used) |
| `decodeMidgardNativeByteListPreimage(cbor)` | gone — `body.inputs` / `body.outputs` are already structured arrays; for the not-yet-migrated lucid-midgard builder it is still re-exported transitionally from `midgard-tx-codec` |
| `cardanoTxBytesToMidgardNativeTxFull(bytes)` / `…Bytes(bytes)` | `cardanoTxBytesToMidgardTx(bytes)` / `cardanoTxBytesToMidgardTxBytes(bytes)` |
| `cmlToMidgard(cmlTx)` / `midgardToCml(tx, network?)` | same names, from `@al-ft/midgard-ts` (note: midgard-ts uses `@dcspark/cardano-multiplatform-lib-nodejs`; pass *bytes* across the boundary, not CML objects, to avoid the dcSpark-vs-lucid-evolution CML class mismatch) |
| `decodeMidgardTxOutput` / `encodeMidgardTxOutput` (midgard-core) | `decodeTransactionOutput` / `encodeTransactionOutput` (midgard-ts) + an `Assets`↔`Value` bridge for lucid-midgard's higher-level output type |
| `computeHash32` (midgard-core, `Buffer`) | `computeHash32` (midgard-ts, `Uint8Array`) — same blake2b-256; wrap in `Buffer` where needed |
| `MIDGARD_NATIVE_TX_VERSION` | re-exported as `1n` from `midgard-tx-codec`; the binary `Transaction` itself has no version field |

### ✅ RESOLVED — `MidgardV1` scripts now have a tagged representation in midgard-ts

Chose **Option 1**: extended the midgard-ts codec spec with a language-tagged
script type. New `midgard-ts/src/types/script.ts`:
`VersionedScript { language: "NativeCardano" | "PlutusV3" | "MidgardV1", bytes }`
— encoding: static = `language(u64)` + `bytes_len(u64)`; dynamic = `bytes + pad`.
`NativeCardano` bytes = CML `NativeScript` CBOR; `PlutusV3`/`MidgardV1` bytes =
raw flat-encoded script.

Format changes (⚠️ this is a wire-format change to midgard-ts):
- `TransactionOutput.script_ref`: `Uint8Array | undefined` → `VersionedScript | undefined`.
- `TransactionWitnessSet`: dropped `native_scripts` (bit 1) + `plutus_v3_scripts`
  (bit 3); replaced with a single `scripts: VersionedScript[] | undefined` (bit 1).
  Bits now: 0 = `vkey_witnesses`, 1 = `scripts`, 2 = `redeemers`.
- `TransactionWitnessSetCompact`: `vkey_witnesses_hash`, `scripts_hash`,
  `redeemers_hash` (was `native_scripts_hash` + `plutus_v3_scripts_hash`).
- `derived.ts` `deriveTransactionWitnessSetCompact`: `scripts_hash` =
  `blake2b256(encodeVersionedScriptVec(ws.scripts))`.
- `cardano.ts`: `cmlToMidgard` builds `scripts` from CML native + plutus-v3 lists
  (tagged), and `script_ref` from `CML.Script` (native → `NativeScript` CBOR,
  plutus-v3 → raw bytes; v1/v2 → `ConversionError`); `midgardToCml` splits
  `scripts` back by tag and **throws** if any `MidgardV1` script is present
  (can't represent a MidgardV1 script in a Cardano tx — by design).
- `midgard-ts/src/validation/phase-a.ts` updated for the new shape.

⚠️ **midgard-ts's own jest tests are now stale** (`tests/round-trip.test.ts`,
`tests/cardano-roundtrip.test.ts` construct `native_scripts` / `plutus_v3_scripts`
/ `script_ref: bytes`) — fix when porting them (Phase 6 / Phase 0 last item).

### Other open semantic gaps to resolve before/while doing Phase 4

- **Script references — language tags.** The old midgard-core codec models a
  ref script as `MidgardVersionedScript { language: "NativeCardano" | "PlutusV3"
  | "MidgardV1", scriptBytes, nativeScript? }`. midgard-ts `TransactionOutput.
  script_ref` is just `Uint8Array` and `cardano.ts` treats it as the CBOR of a
  `CML.Script` (so a `MidgardV1` ref script can't round-trip through CML).
  Decide: does the new on-wire output need to preserve the `MidgardV1`/native/
  plutus-v3 distinction? If yes, midgard-ts's `TransactionOutput` encoding needs
  a tagged `script_ref` (or a Midgard-side wrapper). If `MidgardV1` ref scripts
  aren't actually used, document that and just carry CML-Script CBOR.
- **Datum.** Old: `{ kind: "inline", cbor }` (hashes rejected). midgard-ts:
  `datum?: Uint8Array` = raw PlutusData CBOR. Straightforward 1:1 (inline only).
- **Value / Assets.** Old midgard-core `MidgardValue = { lovelace, assets:
  Map<policyHex, Map<nameHex, bigint>> }`; lucid-midgard's public layer uses
  `Assets` (flat `Record<unitHex, bigint>`). midgard-ts `Value = {type:"Coin",
  coin} | {type:"MultiAsset", coin, assets:[[PolicyId,[[name,amt]]]]}` with raw
  `Uint8Array` keys. Need `assetsToValue` / `valueToAssets` bridges in
  `lucid-midgard/src/core/output.ts` (and similar for `Mint`).
- **lucid-midgard is not incrementally buildable.** `builder.ts`, `provider.ts`,
  `core/output.ts`, `wallet.ts` all reference the old native codec; the package
  build breaks until they're all converted. Do Phase 4 as one focused unit, not
  file-by-file across sessions.

### Notes / decisions baked in so far

- **Compact-field hashing:** in `midgard-ts/src/derived.ts`, each compact field
  hash = `blake2b256` of that field's canonical binary encoding *exactly as it
  appears in the full structure's dynamic section* (`inputs` from the static
  section). `transactionId` = `blake2b256(encodeTransactionBodyCompact(deriveTransactionBodyCompact(body)))`.
  If a Rust `midgard` reference crate exists, double-check these match it.
- **`txCbor` field name** kept everywhere (now holds midgard-ts binary bytes).
- **`outref`/ledger keys** stay as CBOR of `CML.TransactionInput` (Cardano
  interop / MPT keying — not the Midgard tx encoding). `OUTPUT` column now holds
  `encodeTransactionOutput(...)` bytes.

---

## Checklist

### Phase 0 — midgard-ts as a workspace package  ✅ DONE
- [x] `demo/midgard-ts/package.json` → `@al-ft/midgard-ts`, tsup ESM+CJS+`.d.ts`, exports `.`/`./cardano`/`./validation`, deps `@dcspark/cardano-multiplatform-lib-nodejs` + `@noble/hashes`
- [x] `demo/midgard-ts/tsconfig.json` (bundler resolution)
- [x] `pnpm-workspace.yaml` += `midgard-ts`
- [x] `midgard-ts/src/hash.ts` (blake2b-256, `computeHash32`, `ensureHashMatch`, `bytesEqual`)
- [x] `midgard-ts/src/derived.ts` (`deriveTransactionBodyCompact`, `deriveTransactionWitnessSetCompact`, `deriveTransactionCompact`, `transactionBodyHash`, `transactionWitnessSetHash`, `transactionId`)
- [x] `midgard-ts/src/cardano.ts` += `cardanoTxBytesToMidgardTx` / `cardanoTxBytesToMidgardTxBytes`
- [x] `midgard-ts/src/types/script.ts` — language-tagged `VersionedScript` (NativeCardano/PlutusV3/MidgardV1) + Vec helpers; wired into `TransactionOutput.script_ref` and `TransactionWitnessSet.scripts` (replaced `native_scripts`/`plutus_v3_scripts`); `derived.ts`, `cardano.ts`, `validation/phase-a.ts` updated
- [x] `midgard-ts/src/index.ts` re-exports `hash` / `derived` / `cardano` / `types/script`
- [x] `pnpm install` + `pnpm --dir midgard-ts build` clean
- [ ] fix midgard-ts's jest tests for the new `VersionedScript` shape (`tests/round-trip.test.ts`, `tests/cardano-roundtrip.test.ts` still use `native_scripts`/`plutus_v3_scripts`/`script_ref: bytes`); then port to vitest or wire `pnpm --dir midgard-ts test` into CI

### Phase 1 — midgard-node codec module  ✅ DONE
- [x] `midgard-node/package.json` += `@al-ft/midgard-ts` dep; `prebuild`/`typecheck` build midgard-ts first
- [x] `midgard-node/src/midgard-tx-codec/index.ts` → `export * from "@al-ft/midgard-ts"` + explicit re-exports of still-CBOR midgard-core helpers (`address`, `script-language-views`, `errors`, `cbor`) + `MIDGARD_NATIVE_TX_VERSION` + transitional `decodeMidgardNativeByteListPreimage`
- [ ] clean up `midgard-tx-codec/{native,output,cbor,errors,hash}.ts` siblings (currently still point at midgard-core; harmless but stale — delete or repoint once consumers stop importing them directly)

### Phase 2 — midgard-node consumers
- [x] `src/utils.ts` — `findSpentAndProducedUTxOs` / `breakDownTx` use `decodeTransaction` / `transactionId` / `encodeTransactionOutput` / `midgardAddressToText`; `body.inputs`/`body.outputs` structured
- [x] `src/database/immutable.ts` — `decodeTransaction` + `transactionId`
- [x] `src/commands/audit-blocks-immutable.ts` — `decodeTransaction` + `transactionId`
- [x] `src/commands/listen-utils.ts` — `normalizeSubmitTxHexToNative` + Cardano-witness check on midgard-ts `Transaction` / `vkey_witnesses`
- [ ] `src/commands/protocol-info.ts` — currently compiles via re-exported `MIDGARD_NATIVE_TX_VERSION`; revisit whether protocol-info should still surface a "native tx version"
- [ ] `src/commands/submit-l2-transfer.ts` — `selectedInputsFromCompletedTx` reads `completed.tx.body.spendInputsPreimageCbor`; finishes once lucid-midgard `CompleteTx` is migrated → use `completed.tx.body.inputs` (`OutputReference[]`), label `${hex(ref.tx_id)}#${ref.index}`
- [ ] `src/workers/utils/mpt.ts` — uses `decodeMidgardAddressText` (still exported, OK) + `encode/decodeMidgardTxOutput` from lucid-midgard; switch to midgard-ts `TransactionOutput` once lucid-midgard migrates; confirm MPT leaf bytes (`outref`/`output`) policy is intentional
- [ ] `src/fibers/fetch-and-insert-deposit-utxos.ts` — imports `computeHash32` from `midgard-tx-codec/hash.js` (still midgard-core, `Buffer`); fine, or switch to midgard-ts `computeHash32` + `Buffer.from`
- [ ] `src/fibers/*` and any remaining `src/commands/*` referencing the old types — sweep `grep -rn "MidgardNativeTx\|spendInputsPreimageCbor\|outputsPreimageCbor\|PreimageCbor\|computeMidgardNativeTxId\|decodeMidgardNativeTx\|encodeMidgardNativeTx" midgard-node/src`

### Phase 3 — DB serialization (midgard-node)
- [ ] `src/database/mempool.ts` — `txCbor: Buffer` now midgard-ts `encodeTransaction` bytes; update any decode sites
- [ ] `src/database/mempoolLedger.ts` — `OUTPUT: Buffer` now `encodeTransactionOutput` bytes
- [ ] `src/database/mempoolTxDeltas.ts` — currently uses `cborg` to encode spent/produced; spent stays CBOR `TransactionInput`, produced `[outRef, output]` where `output` is now midgard-ts bytes — decide encoding (keep cborg wrapper around midgard-ts bytes, or switch wholesale)
- [ ] `src/database/utils/ledger.ts` / `tx.ts` — column types unchanged (`BYTEA`/`TEXT`); just confirm callers
- [ ] migration/checksum tooling (`db:migrate` / `db:checksum` / `db:verify`) — old rows are CBOR, new rows are binary; decide on a hard cutover vs. a versioned column / wipe-and-reindex

### Phase 4 — lucid-midgard  (the unblocker; biggest item)  — UNBLOCKED (Option 1 done); IN PROGRESS
- [x] `lucid-midgard/package.json` — added `@al-ft/midgard-ts` dep + `pnpm --dir ../midgard-ts run build` in prebuild
- [x] `src/core/output.ts` — `encode/decodeMidgardTxOutput` now go through midgard-ts `encode/decodeTransactionOutput`. Added bridges `coreValueToMidgardTsValue`/`midgardTsValueToCoreValue`, `coreScriptRefToMidgardTs`/`midgardTsScriptRefToCore` (the old `MidgardVersionedScript` language strings `"NativeCardano"|"PlutusV3"|"MidgardV1"` map 1:1 to midgard-ts `VersionedScript.language`; `scriptBytes`↔`bytes`; NativeCardano decode-side rebuilds `nativeScript` via `decodeMidgardNativeScript`), `coreOutputToMidgardTs`/`midgardTsOutputToCore`; local `encode/decodeCoreMidgardTxOutput` now delegate to midgard-ts. The higher-level `MidgardTxOutput` (`core/types.ts`) shape is unchanged. `lucid-midgard` builds.
- [ ] **NOT done — `codec/index.ts` left as-is** on purpose (still re-exports `@al-ft/midgard-core/codec`, so `builder.ts`'s `materializeMidgardNativeTxFromCanonical` / `MidgardNativeTxFull` imports still resolve). Plan was to add midgard-ts imports directly in the files that need them, not blanket-re-export (avoids `computeHash32`/`Hash32`/`ensureHashMatch` collisions).
- [ ] **`src/builder.ts` (6,668 ln) — the critical remaining piece.** Boundary-adapter approach (smallest diff): keep builder building `MidgardNativeTxFull` internally, but at the encoding boundary swap the wire format to midgard-ts binary:
  - add `nativeFullToMidgardTs(tx: MidgardNativeTxFull): MidgardTsTransaction` — decode the preimage CBORs into structured arrays: `spendInputsPreimageCbor`/`referenceInputsPreimageCbor` → `CML.TransactionInput.from_cbor_bytes(b)` → `{tx_id, index}`; `outputsPreimageCbor` → each element is **already a midgard-ts `TransactionOutput` encoding** (because `encodeMidgardTxOutput` was migrated above) → `decodeTransactionOutput(b)`; `addrTxWitsPreimageCbor` → `CML.Vkeywitness.from_cbor_bytes(b)` → `{vkey, signature}`; `scriptTxWitsPreimageCbor` → list of `MidgardVersionedScript` CBOR (`decodeMidgardVersionedScript`) → `{language, bytes}` via `coreScriptRefToMidgardTs`; `mintPreimageCbor` → `decodeMidgardNativeMint` → midgard-ts `Mint`; `requiredSignersPreimageCbor`/`requiredObserversPreimageCbor` → list of Hash28; scalars: `fee`; `validity_interval_start = validityIntervalStart === -1n ? undefined : Number(...)`; **`ttl` ⟵ `validityIntervalEnd` (`=== -1n ? undefined : Number(...)`)** — confirm this mapping is right; `auxiliary_data_hash`/`script_data_hash` ⟵ `auxiliaryDataHash`/`scriptIntegrityHash` (treat the "empty" sentinels `EMPTY_NULL_ROOT` / `EMPTY_SCRIPT_INTEGRITY_HASH` as `undefined`); `network_id = networkId === 255n ? undefined : Number(...)`; `is_valid = compact.validity === "TxIsValid"` (⚠️ midgard-ts `is_valid` is a bool; the old `validity` enum has 6 codes — decide how non-`TxIsValid` maps, probably `is_valid=false` + the code lives elsewhere).
  - add `midgardTsToNativeFull(tx: MidgardTsTransaction): MidgardNativeTxFull` — build a `MidgardNativeTxCanonical` from the midgard-ts tx (re-encode the preimage CBORs: inputs/ref-inputs as CBOR-list-of-`CML.TransactionInput`-bytes, outputs as CBOR-list-of-`encodeTransactionOutput`-bytes, vkey-wits as CBOR-list-of-`CML.Vkeywitness`-bytes, scripts as CBOR-list-of-`encodeMidgardVersionedScript`-bytes, mint via `encodeMidgardNativeMint`, signers/observers as CBOR lists; scalars back; `validityIntervalEnd ⟵ ttl ?? -1n`, etc.), then `materializeMidgardNativeTxFromCanonical(canonical)`.
  - replace `encodeMidgardNativeTxFull(x)` (~12 sites) → `Buffer.from(encodeTransaction(nativeFullToMidgardTs(x)))`; `computeMidgardNativeTxIdFromFull(x)` (~6 sites) → `Buffer.from(transactionId(nativeFullToMidgardTs(x)))`; `decodeMidgardNativeTxFull(bytes)` (~4 sites incl. `get tx()`) → `midgardTsToNativeFull(decodeTransaction(bytes))`.
  - keep `verifyMidgardNativeTxFullConsistency` / `deriveMidgardNativeTxCompact` / `materializeMidgardNativeTxFromCanonical` working on the internal old model — no change.
- [ ] `src/provider.ts` (1,016 ln) — wherever it decodes/encodes Midgard txs/outputs
- [ ] `src/wallet.ts` (274 ln) — signing produces `{vkey, signature}` raw-byte witnesses
- [ ] update lucid-midgard's own tests
- [ ] ⚠️ **incoherent intermediate state right now**: midgard-node's `breakDownTx` / `utils.ts` decode tx bytes via `decodeTransaction` (midgard-ts binary), but `lucid-midgard/builder.ts` still emits CBOR `MidgardNativeTxFull` bytes from `encodeMidgardNativeTxFull`. End-to-end submit/commit will NOT work until the builder.ts boundary swap above lands. (Output bytes *are* already consistent — both sides use midgard-ts `encode/decodeTransactionOutput`.)

### Phase 5 — midgard-validation
- [ ] Decide: **(a)** rewrite `phase-a.ts` (689) / `phase-b.ts` (**1,298**) / `script-context.ts` (290) / `midgard-redeemers.ts` (209) / `midgard-output.ts` / `types.ts` / `ledger.ts` to operate on midgard-ts `Transaction`, **or (b)** adopt `@al-ft/midgard-ts/validation` (already has phase-A/phase-B + reject codes against the new types) and delete the bespoke modules, re-exposing the same public surface midgard-node imports (`@/validation/*`)
- [ ] `midgard-node/src/validation/*.ts` (thin re-export shims) — repoint
- [ ] `midgard-node/src/utils.ts` already dropped its `@/validation/midgard-output.js` import — make sure nothing else depends on the removed `midgardOutputAddressText`
- [ ] `midgard-validation/package.json` — add `@al-ft/midgard-ts`

### Phase 6 — tests & benchmarks
- [x] `tests/benchmarks/midgard-tx-codec.bench.ts` — rewritten; `pnpm bench:codec:quick` green
- [ ] `tests/benchmarks/native-phase-a.bench.ts` — update to midgard-ts `Transaction` + (new) validation
- [ ] `tests/benchmarks/validation-benchmark.bench.ts` — same
- [ ] midgard-node test files referencing the codec: `midgard-native-tx-codec.test.ts`, `native-transaction-integration.test.ts`, `database.test.ts`, `phase-a-cardano-signature-bridge.test.ts`, `protocol-info.test.ts`, `midgard-output-helpers.ts`, `submit-l2-transfer.test.ts`, `listen-admission-auth.test.ts`, `merge-error-codes.test.ts`, `midgard-local-script-eval.test.ts`, `validation-parallelization.test.ts`, `cbor-root-normalization.test.ts` (the last may be obsolete — root indefinite-array normalization is a CBOR concern)
- [ ] regenerate any committed fixtures that hold old-format bytes (`tests/txs/txs_0.json` is *Cardano* CBOR — stays; but any `*-native-*` fixtures are now stale)

### Phase 7 — finish
- [ ] `pnpm install` clean
- [ ] `pnpm --dir midgard-ts build && pnpm --dir lucid-midgard build && pnpm --dir midgard-sdk build && pnpm --dir midgard-node typecheck` clean
- [ ] `pnpm --dir midgard-node test` (emulator) green
- [ ] re-run `bench:codec:quick|full`, `bench:phase-a:native:quick|full`, `bench:validation:quick|full`; capture numbers
- [ ] consider deleting now-dead `@al-ft/midgard-core/codec/native.ts` (and its `MidgardNativeTx*` exports) once nothing imports it; keep the rest of `midgard-core/codec` (the still-CBOR helpers)
- [ ] grep the whole `demo/` tree for `MidgardNativeTx`, `PreimageCbor`, `spendInputsRoot`, `nativeTx` to catch stragglers

---

## How to run a session

1. Open this file, find the first unchecked box (top-down within a phase; phases are mostly ordered, but Phase 4 unblocks parts of Phases 2–3).
2. Make a session task list from those items.
3. After each item: tick the box here, commit doc+code together.
4. Tree won't `typecheck` until Phases 2–5 are complete — that's expected for an in-place migration. Use `pnpm bench:codec:quick` as the smoke test that the codec layer is intact.
