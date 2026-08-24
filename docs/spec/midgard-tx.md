# MidgardTx — compact transaction format, flat field commitments, and preimage carriage (V1)

- **Status:** implementation-normative. This is the format authority for the
  MidgardTx compact transaction type, its canonical encoding, the nine flat
  blake2b-256 field commitments, the uniform enveloped preimage grammar, and
  the three-tier field-preimage carriage convention.
- **Authority:** per `docs/spec/README.md`, this document wins over
  `technical-spec/` on concrete detail; `GOAL_SPEC.md` binds it by reference
  at scheme altitude. Rationale lives in
  `docs/midgard/decisions/0004-compact-tx-flat-field-hash-reversion.md`;
  decision trail: wayfinder map
  [#552](https://github.com/Anastasia-Labs/midgard/issues/552).
- **Owner/approver:** repository owner (Philip DiSarro).
- **Last reviewed:** 2026-08-09 (initial authoring 2026-08-08, Phase 0 of the
  flat reversion program; §10 added in Phase 3 by
  [#570](https://github.com/Anastasia-Labs/midgard/issues/570), §11 by
  [#571](https://github.com/Anastasia-Labs/midgard/issues/571), §12 by
  [#572](https://github.com/Anastasia-Labs/midgard/issues/572); §8.6's frozen
  mint-redeemer wire format by
  [#573](https://github.com/Anastasia-Labs/midgard/issues/573); §8.10 and
  erratum E1 in Phase 4 by
  [#574](https://github.com/Anastasia-Labs/midgard/issues/574); erratum E2 in
  Phase 5 by [#575](https://github.com/Anastasia-Labs/midgard/issues/575)).
- **Version:** `native_tx_version_v1 = 1`. Pre-launch, this format replaces
  the counted bounded-collection commitment scheme in place (GOAL_SPEC §3
  invariant 13); there is no compatibility path to the retired scheme.
- **Provisional values:** the constants marked _provisional_ in §8.3 were
  pinned by analysis and are re-measured in Phase 4 of the reversion
  program; falsification by measurement is an amendment-level erratum to
  this document by design, and does not reopen GOAL_SPEC acceptance
  criteria. **One has been falsified and repaired**: `K` was superseded by §8.3
  erratum E1 (2026-08-09), which re-pinned it from 15,900 to 15,148 bytes; the
  re-pin has since landed in both languages, so the window of preimage lengths
  that carried no admissible carriage is closed.
  `maxTier1RedeemerPreimageBytes` remains provisional and unmeasured, though E1
  narrows the headroom it was reasoned from.
- **Errata:** §8.3 erratum E1 — `K` re-pinned by Phase-4 measurement; §8.3
  erratum E2 — limits on faulting the witness-set fields (§2.5 fields 6–8):
  field 6 is not faultable at C20.6's admissible script-witness cardinality,
  on execution and on carriage (limits 1 and 2, which stand). Limit 3 — the
  outright tier-3 refusal at every witness-set field — is **RESOLVED** by
  [#606](https://github.com/Anastasia-Labs/midgard/issues/606) (owner ruling
  2026-08-16): the §8.6 certificate datum carries a mint-welded `field_hash`
  and the door requires it to equal the commitment derived from the anchored
  structures, so tier 3 is admissible at every field. Raised by the Phase-5
  Q1x rebind ([#575](https://github.com/Anastasia-Labs/midgard/issues/575));
  see E2's disposition for the assignment history.

## 1. Scope and notation

This document defines, for canonical V1:

1. the compact transaction types and their canonical CBOR encodings (§2);
2. transaction identity — the two-level hash derivation (§3);
3. the nine per-field commitments (§4);
4. the uniform enveloped field-preimage grammar and the per-field item
   encodings (§5);
5. canonicality rules, including the datum/redeemer canonicity predicate
   (§6);
6. the mandatory access invariants every consumer of the nine commitments
   observes (§7);
7. the three-tier publication-carriage convention for field preimages (§8);
8. the resumable walk and its checkpoint encoding (§10);
9. intra-item access — the Value bookmark, the Canonical-Data Acceptor, and
   the native-script checkpointable pushdown (§11); and
10. witness-minimal fault statements, including per-asset conservation (§12).

**Nothing in this document is deferred.** §10 (resumable walk and checkpoints)
landed with [#570](https://github.com/Anastasia-Labs/midgard/issues/570), §11
(intra-item access) with
[#571](https://github.com/Anastasia-Labs/midgard/issues/571) and §12 (fault
statements) with
[#572](https://github.com/Anastasia-Labs/midgard/issues/572); documents that
bind any of them by reference — `GOAL_SPEC.md` §3.1(2) — now name a definition
rather than a note. The four sections stack: §7 governs every consumer of the
nine commitments, §10 reaches one item's bytes and carries the place between
transactions, §11 says what a rule may do inside those bytes, and §12 says what
a challenger may claim from what it found there.

Byte strings are written in hex (`82`, `58 20 …`). `array(n)`, `map(n)`,
`bytes(n)`, `uint`, `int` denote definite-length canonical CBOR heads:
minimal-width length/value encodings with fail-closed rejection of any
non-minimal form, except where this document explicitly pins a fixed-width
form. `‖` is byte concatenation. `blake2b_256` is the Plutus builtin (32-byte
digest). "Encoders" means both implementation twins — Aiken
(`onchain/aiken/lib/midgard/fraud-proofs/native-tx/`) and TypeScript
(`demo/midgard-core/src/codec/`) — which MUST emit byte-identical output for
every value in this document's domain, pinned by cross-language golden
vectors including the empty case for every field.

## 2. Compact transaction types

### 2.1 `NativeTxBodyCompact`

Twelve fields, in declaration and wire order:

| #   | field                     | type                             |
| --- | ------------------------- | -------------------------------- |
| 0   | `spend_inputs_hash`       | 32-byte hash                     |
| 1   | `reference_inputs_hash`   | 32-byte hash                     |
| 2   | `outputs_hash`            | 32-byte hash                     |
| 3   | `fee`                     | uint                             |
| 4   | `validity_interval_start` | int (`-1` = none)                |
| 5   | `validity_interval_end`   | int (`-1` = none)                |
| 6   | `required_observers_hash` | 32-byte hash                     |
| 7   | `required_signers_hash`   | 32-byte hash                     |
| 8   | `mint_hash`               | 32-byte hash                     |
| 9   | `script_integrity_hash`   | 32-byte hash                     |
| 10  | `auxiliary_data_hash`     | 32-byte hash                     |
| 11  | `network_id`              | uint (`0`, `1`, or `255` = none) |

Canonical encoding (`encode_native_tx_body_compact`): `8c` (array(12))
followed by the twelve elements in order — every 32-byte hash as
`58 20 ‖ h32`, the integers as canonical minimal CBOR.

### 2.2 `NativeTxWitnessSetCompact`

Three fields, in declaration and wire order: `addr_tx_wits_hash`,
`script_tx_wits_hash`, `redeemer_tx_wits_hash` — each a 32-byte hash.
Canonical encoding: `83 ‖ 58 20 addr ‖ 58 20 script ‖ 58 20 redeemer`.

### 2.3 `NativeTxCompact`

`{ body: NativeTxBodyCompact, witness_set_hash: 32-byte hash,
validity_code: uint ≤ 5 }`. Versioned encoding
(`encode_native_tx_compact_for_version`):
`84 ‖ uint(version) ‖ body ‖ 58 20 witness_set_hash ‖ uint(validity_code)`.

### 2.4 `NativeTxFieldPreimageLengthsV1`

Nine byte lengths — **byte lengths only; item counts appear nowhere in this
structure** (counts live solely in the preimage headers, §5.2). Wire order
(`89` + nine uints):

```
spend_inputs, reference_inputs, outputs, required_observers,
required_signers, mint, script_witnesses, address_witnesses, redeemers
```

Note the wire order places `script_witnesses` at position 6 and
`address_witnesses` at position 7 (transposed relative to the record
declaration); both twins already agree on this and MUST NOT change it.

### 2.5 The nine committed fields

Field indices are fixed and normative:

| index | field                    | commitment slot                     |
| ----- | ------------------------ | ----------------------------------- |
| 0     | spend inputs             | `body.spend_inputs_hash`            |
| 1     | reference inputs         | `body.reference_inputs_hash`        |
| 2     | outputs                  | `body.outputs_hash`                 |
| 3     | required observers       | `body.required_observers_hash`      |
| 4     | required signers         | `body.required_signers_hash`        |
| 5     | mint                     | `body.mint_hash`                    |
| 6     | script witnesses         | `witness_set.script_tx_wits_hash`   |
| 7     | address (vkey) witnesses | `witness_set.addr_tx_wits_hash`     |
| 8     | redeemer witnesses       | `witness_set.redeemer_tx_wits_hash` |

## 3. Transaction identity (unchanged derivation)

The reversion changes the **definition and therefore the value** of the nine
32-byte field hashes; it changes nothing about the shapes or the derivation
order:

- **Level 1 — witness-set hash:**
  `witness_set_hash = blake2b_256(encode_native_tx_witness_set_compact(ws))`
  over the §2.2 encoding.
- **Level 2 — transaction id:**
  `tx_id = blake2b_256("MidgardNativeTxBodyV1" ‖ uint(version) ‖ body_cbor)`
  where `body_cbor` is the §2.1 encoding (domain string as raw ASCII bytes).
- The full-transaction commitment (`"MidgardNativeTxFullV1"` domain) and the
  proof-source commitment (`"MidgardNativeTxProofSourceV1"` domain, over
  `83` followed by `bytes(compact_cbor)`,
  `bytes(witness_set_compact_cbor)`, and
  `bytes(field_preimage_lengths_cbor)`) keep their current forms.

All fixtures and golden vectors that embed any of the nine hashes, the
witness-set hash, or a tx-id regenerate under this document; none migrate.

## 4. The nine field commitments

For each field `i` in 0..8:

```
field_hash_i = blake2b_256(preimage_i)
```

**Plain hashing.** No domain tag, no version prefix, no field index in the
hash input. The retired counted scheme's domains
(`MidgardBoundedCollectionItemV1`, `MidgardBoundedCollectionCommitmentV1`,
`MidgardBoundedItemChunkV1`, `MidgardBoundedItemCommitmentV1`,
`MidgardValidationMerkle*V1`) are prohibited legacy surface.

**Field identity is positional.** A field hash's meaning comes solely from
its slot in `NativeTxBodyCompact` / `NativeTxWitnessSetCompact` (§2.5).
Because fields 0/1 and 3/4 share item encoders, identical content aliases
across those field pairs; this is an accepted consequence of plain hashing,
made safe by the following invariant:

> **Positional-identity invariant (normative).** Every rule that verifies a
> preimage against a field hash MUST obtain the expected hash from the
> committed compact structure in view (or from a value transitively
> committed by the tx-id, e.g. a §8.6 certificate's redeemer-supplied
> compact structures re-derived to the tx-id). Free-standing field-hash
> arguments are prohibited in dispute entry points.

## 5. The uniform enveloped preimage grammar

### 5.1 Grammar (all nine fields)

```
preimage       = definite_array_header(N) ‖ wrapped_item_0 ‖ … ‖ wrapped_item_{N-1}
wrapped_item_i = definite_bytes_header(len(enc_i)) ‖ enc_i
```

- `definite_array_header(N)`: `80+N` for N ≤ 23, `98 NN` for N ≤ 255,
  `99 NNNN` for N ≤ 65,535 — minimal width, fail closed on wider forms.
- `definite_bytes_header(L)`: `40+L` for L ≤ 23, `58 LL`, `59 LLLL` —
  minimal width, fail closed.
- An **empty field encodes as exactly `80`** — all nine fields, including
  mint.
- The per-item byte-string envelope applies to **all nine fields**. (Under
  the retired scheme fields 6 and 8 concatenated raw item CBOR and field 5
  hashed a raw map; those forms are prohibited. The envelope is what buys
  O(1) top-level skips: one head decode plus a byte jump per item.)

Decoders fail closed on any deviation: wrapper/length mismatch,
non-minimal header, item count disagreeing with the walked content, or
trailing bytes after item `N-1`.

### 5.2 Item count

`N` — the leading array header — is the **only** place a field's item count
exists. It is mirrored nowhere: not in `NativeTxCompact`, not in
`NativeTxFieldPreimageLengthsV1`. Count-consuming rules (count-fault
variants, exact-count/order/dedup rules, input-set uniqueness) consume
**reveal-derived counts**: they read `N` from the preimage after the
(measured-free) hash check and, where the rule's semantics require it, walk
the full field content.

### 5.3 Per-field canonical item encodings

| #   | field              | `enc_i`                                                                                                                                  | width       |
| --- | ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| 0   | spend inputs       | `82 ‖ 58 20 ‖ tx_id(32) ‖ 19 ‖ index_be16`                                                                                               | fixed 38 B  |
| 1   | reference inputs   | same as field 0                                                                                                                          | fixed 38 B  |
| 2   | outputs            | `encode_midgard_tx_output` (§5.5)                                                                                                        | variable    |
| 3   | required observers | raw 28-byte hash                                                                                                                         | fixed 28 B  |
| 4   | required signers   | raw 28-byte hash                                                                                                                         | fixed 28 B  |
| 5   | mint               | `encode_mint_policy_item`: `82 ‖ 58 1C policy_id(28) ‖ map(k) ‖ assets` (§5.6)                                                           | variable    |
| 6   | script witnesses   | `encode_midgard_versioned_script`: `82 ‖ uint(language_tag) ‖ bytes(script_bytes)`                                                       | variable    |
| 7   | address witnesses  | `encode_midgard_address_witness`: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)`                                                            | fixed 101 B |
| 8   | redeemer witnesses | `encode_midgard_redeemer_witness`: `84 ‖ uint(purpose_tag) ‖ uint(index) ‖ bytes(redeemer_cbor) ‖ 82 ‖ uint(ex_memory) ‖ uint(ex_steps)` | variable    |

Item-level rules:

- **Fields 0/1 — the fixed 3-byte output index.** The input's output index
  is **always** encoded as the fixed 3-byte form `19 XXXX` (CBOR uint16
  head, big-endian value 0–65,535), even for values 0–23. This is the sole
  deliberately non-minimal encoding in the format. It makes every
  spend/reference-input item exactly 38 bytes with wrapper `58 26`, giving
  **stride 40** and pure arithmetic access:
  `item_offset(i) = header_len + 40·i`, with `enc_i` at
  `item_offset(i) + 2`. The fixed width picks a different canon; it does
  not waive uniqueness — `18 XX`, minimal one-byte, and wider index forms
  all reject.
- **Fields 0/1 — this encoding is also the ledger out-ref key.** An
  out-ref has exactly one byte form in Midgard, and it is the field-0/1
  item above. The same 38 bytes serve three consumers, and they are
  required to be identical:

  | consumer                                      | derivation                   |
  | --------------------------------------------- | ---------------------------- |
  | field-0/1 preimage items                      | `enc_0` / `enc_1` (this row) |
  | ledger MPF trie key                           | the same bytes, unchanged    |
  | ledger database `outref` column / primary key | the same bytes, unchanged    |

  On-chain this is literal: `ledger_outref_key`
  (`onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak`) is a
  direct call to `encode_midgard_tx_input`, the field-0/1 item encoder. In
  TypeScript the one spelling is
  `encodeMidgardSpendInputItemV1`
  (`demo/midgard-core/src/codec/native-tx-field-items-v1.ts`), reached
  through `outRefToCbor` / `utxoOutRefCbor`
  (`demo/lucid-midgard/src/core/output.ts`) and `midgardOutRefToCbor`
  (`demo/midgard-validation/src/validation-candidate.ts`). CML's
  `TransactionInput` CBOR is **not** an admissible spelling: it minimises
  the index and so yields 36 bytes for indices 0–23 and 37 for 24–255, keys
  the on-chain side never computes. (From 256 up the minimal index is
  already `19 XXXX`, so the two agree there and only indices below 256 can
  carry a stale key.) Decoders of a trie key or `outref` column are
  `decode_midgard_tx_input_cbor` / `decodeMidgardSpendInputItemV1`, which
  both enforce the exact 38-byte width and the `0x19` index head — the
  width is what rejects a non-minimal `59 0020` tx_id header, which a
  positional reader would otherwise decode to the same out-ref.

  Consequences that follow from the table rather than from any separate
  rule: `maxTransactionAggregateFieldBytes` bounds trie keys too (§5.4);
  the 0–65,535 index domain is the ledger's index domain; and a
  development ledger written under any other spelling must be reset, not
  migrated (pre-mainnet, per `AGENTS.md`).

- **Fields 3/4 — asserted 28-byte width.** Every observer/signer item MUST
  be exactly 28 bytes (wrapper `58 1C`, **stride 30**,
  `item_offset(i) = header_len + 30·i`). Both encoder twins enforce the
  width; decoders reject any other length.
- **Field 7** items are structurally fixed at 101 bytes (wrapper `58 65`,
  stride 103): the encoder asserts a 32-byte verification key and 64-byte
  Ed25519 signature.
- **Field 6 — the `language_tag` value set.** Exactly three values are
  admissible, and the canonical encoder emits exactly these byte forms:

  | value | language        | canonical bytes | script-hash prefix |
  | ----- | --------------- | --------------- | ------------------ |
  | 0     | `NativeCardano` | `00`            | `0x00`             |
  | 3     | `PlutusV3`      | `03`            | `0x03`             |
  | 128   | `MidgardV1`     | `18 80`         | `0x80`             |

  Any other value rejects. Twins:
  `midgard_script_language_to_tag` / `midgard_script_language_from_tag` in
  `onchain/aiken/lib/midgard/fraud-proofs/native-tx/components.ak`, and
  `MidgardVersionedScriptTags` / `MidgardScriptHashPrefixes` in
  `demo/midgard-core/src/codec/versioned-script.ts`. For `NativeCardano`,
  `script_bytes` carries the canonical Midgard native-script CBOR; for
  `PlutusV3` and `MidgardV1` it carries the raw script payload.

- **Field 8 — the `purpose_tag` value set.** Exactly seven values are
  admissible. Every one is ≤ 23, so each occupies exactly one byte equal to
  its value:

  | value | bytes | purpose   |
  | ----- | ----- | --------- |
  | 0     | `00`  | `Spend`   |
  | 1     | `01`  | `Mint`    |
  | 2     | `02`  | `Cert`    |
  | 3     | `03`  | `Reward`  |
  | 4     | `04`  | `Vote`    |
  | 5     | `05`  | `Propose` |
  | 6     | `06`  | `Receive` |

  Values 0–5 reuse Cardano's own `RedeemerTag` numbering; 6 (`Receive`) is
  Midgard-only. Any other value rejects
  (`midgard_redeemer_purpose_from_tag`, same Aiken module). Two narrower
  sets sit inside this one and are deliberately not the format's bound: the
  Midgard builder emits only `Spend`, `Mint`, `Reward`, and `Receive`
  (`RedeemerTags`, `demo/lucid-midgard/src/builder/script-materialization.ts`),
  and the Cardano↔Midgard conversion bridge admits only `Spend`, `Mint`,
  and `Reward`, rejecting the rest as lossy
  (`ensureSupportedCardanoRedeemerTag`,
  `demo/midgard-core/src/codec/native-redeemer.ts`). `index`, `ex_memory`,
  and `ex_steps` are canonical minimal CBOR uints and MUST be non-negative.

- **Fields 2/5/6/8** are variable-width; top-level access is by enveloped
  walk (one head decode + byte jump per skipped item). Their interior
  encodings (`encode_midgard_tx_output`, mint policy items, versioned
  scripts, redeemer witnesses) are unchanged from the current canonical
  encoders except as §5.6 states for mint and as the two tag tables above
  pin for fields 6 and 8.
- All integers other than the field-0/1 output index remain canonical
  minimal CBOR.

### 5.4 Field-level byte bounds

- `maxTransactionAggregateFieldBytes = 32,768` (retained; owner ruling —
  tightening it would be a capability change outside the reversion's
  scope). No field preimage exceeds it, and
  `maxSpendInputsPreimageBytes = 32,768` equals it for field 0.
- Admissible item cardinality per field is the minimum of the 16,384-item
  consensus guardrail, the field's byte bound under this grammar (e.g.
  fields 0/1: `header_len + 40·N ≤ 32,768` ⇒ N ≤ 819 at the preimage
  bound), and any Cardano shape bound (spend inputs:
  `maximum_cardano_spend_redeemer_count = 296`, the operative spend
  maximum). Derived cardinality numbers are re-derived from this grammar,
  never migrated from counted-era tables.

### 5.5 Output items (field 2)

`encode_midgard_tx_output` (unchanged): a definite CBOR map keyed `0..3` —
`a2`/`a3`/`a4` by presence — with `0 → bytes(encode_midgard_address)`
(raw Midgard address payload: 1 header byte ‖ 28-byte payment hash ‖
optional 28-byte stake hash), `1 →` inline `encode_midgard_value`
(`82 ‖ uint(lovelace) ‖ policy-asset map`), `2 → bytes(datum_cbor)` when an
inline datum is present, `3 → encode_midgard_versioned_script` when a
reference script is present. Within the Value's policy-asset map, policy
groups and asset names appear in canonical key order (length-first, then
byte-lexicographic compare), duplicates reject, every policy group is
non-empty, an asset name is at most 32 bytes, and asset quantities are
strictly positive; `datum_cbor` MUST satisfy the §6.2 canonicity
predicate. The group-emptiness and name-width clauses restate conditions
`decode_canonical_output` has always enforced on this field; they are written
here because §11.1 reads the same value without materialising it and a
refusal needs a basis in the field's own grammar, not in field 5's.

A reference script under key `3` inherits the field-6 grammar in full,
including §5.3's language-specific payload rule: at `language_tag` 0
(`NativeCardano`) the `script_bytes` payload MUST be canonical Midgard
native-script CBOR under the structure-scan bounds
(`native_script_scan_v1` / `native-script.ts`), and
`decode_canonical_output` is `None` for any output whose tag-0 payload is
not — no descriptor exists for such an output, and no transaction carrying
one is admissible into `transactions_root` as valid. (Ruled on #633.
Delivery: decision 0005 R5 item 9 dropped out of the #617 wave under A3's
ExUnits precondition — the measured one-shot cost is ~82x the GOAL_SPEC
§3.3 basis at the reachable worst case — so the shipped one-shot decoder
does not yet enforce this clause; enforcement rides the #633 re-ruling and
the next regeneration. The staged machine (`ledger_output_proof_v1` →
`native_script_scan_v1`) and the off-chain codec enforce it today.)

### 5.6 Mint items (field 5)

The field-5 preimage is the **enveloped list of per-policy items** under the
§5.1 grammar — this replaces the retired raw-map `encode_mint_preimage`
form. Each item is `encode_mint_policy_item`:
`82 ‖ 58 1C policy_id ‖ map(k) ‖ asset entries`, where each asset entry is
`bytes(asset_name ≤ 32) ‖ int(quantity ≠ 0)`. Policy items appear in
canonical key order (length-first, then lexicographic byte compare), assets
likewise within each policy; duplicates reject. An empty mint field encodes
as `80` like every other field.

## 6. Canonicality

### 6.1 One valid byte form

For every value in this format there is exactly one valid byte encoding, and
decoders fail closed on all others. The field-0/1 fixed-width index (§5.3)
picks a different canon — it does not create a second admissible spelling.

### 6.2 Datum/redeemer canonicity predicate (L1 parity re-pin)

The canonicity predicate for `datum_cbor` (output items) and
`redeemer_cbor` (redeemer-witness items) is **membership in the image of
the Plutus `serialiseData` builtin** — exactly the byte forms `serialiseData`
emits and cardano-ledger's `decodeData` accepts. In particular, and
overriding the retired Aiken-stdlib-v3.1.0 round-trip pin:

- **canonical tag-2/3 bignums are canonical-acceptable**: integers with
  `|i| ≥ 2⁶⁴`, minimal magnitude ≥ 9 bytes, no leading zero, 64-byte
  chunking for long magnitudes; and
- **tag-102 constructor encodings are canonical-acceptable**:
  `d8 66 82 ‖ uint(alternative) ‖ args-list` for alternatives ≥ 128, with a
  minimal uint64 alternative.

The remaining grammar rows are unchanged: minimal integer heads below 2⁶⁴;
definite byte strings ≤ 64 bytes, indefinite 64-byte chunking above;
non-empty lists and constructor argument lists indefinite (`9f … ff`), empty
exactly `80`; **maps definite-length** with entry order preserved; constr
tags `d8 79+alt` (0–6) / `d9 0500+alt−7` (7–127) / tag 102 (≥ 128); exactly
one item consuming exactly the declared bytes; no text, simple, float, or
other tags. Any deviation is non-canonical and rejects (or is faultable at
dispute time).

## 7. Access invariants (normative for every consumer)

Every consumer of the nine field commitments — dispute machine, proof
families, watcher, builders — observes:

1. **Authenticate-once, lazy per-field.** A consumer's first touch of field
   `i` verifies `blake2b_256` over the full preimage against the
   positionally-extracted `field_hash_i`; untouched fields are never
   authenticated. Post-authentication access is offset-and-slice against
   the authenticated bytes.
2. **No offset table in the format.** Item boundaries are walk-derived (or
   arithmetic, for fixed-stride fields) from the authenticated bytes
   themselves; there is no second committed data structure to trust or
   dispute. Arithmetic derivation locates an item; it does not excuse the
   accessor from reading it. A fixed-stride accessor MUST decode the item's
   own `definite_bytes_header` and require it to be the minimal-width form
   for exactly `stride − 2` payload bytes — inferring those two bytes from
   the stride would admit several byte forms for one logical field, in
   violation of §5.1 and §6.1, and would leave a non-canonically committed
   preimage unfaultable. The stronger case that clause gestures at — a
   committed preimage that is not a §5.1 envelope at all, which every door
   below aborts on rather than rejects — is faulted by §12.7 and is not left
   to any accessor to notice.
3. **Abort, never clamp.** An item accessor MUST fail unless
   `0 ≤ i < N` and the item's full byte range lies within the preimage
   length. Slice-primitive clamping must never reach a caller — two
   clamped out-of-range reads are byte-equal and would fabricate equality
   evidence from a valid block.
4. **Count consistency at view construction.** For fixed-stride fields,
   `header_len + stride·N == total_length` MUST hold (from the
   authenticated header in tiers 1–2; against the mint-verified certificate
   `total_length` in tier 3). For variable-width fields the equivalent check
   is the full-content walk, which tiers 1–2 MUST run at view construction:
   the walked items MUST account for exactly `N` and end exactly at
   `total_length`, so a header that miscounts or undercounts its items is
   refused there. Tier 3 cannot afford that walk (§8.6, _Consumption_), so a
   variable-width field carried under tier 3 has **no authenticated item
   count** and an accessor MUST abort rather than hand back the header's
   self-asserted one. Reads stay available: the envelope walk that serves
   them fails closed the moment it leaves the committed bytes.
5. **Positional identity** (§4): expected hashes only via positional
   extraction from committed structures.
6. **Positions, not bytes**, in any resumable state: checkpoints carry
   offsets, indices, fixed-width scalars, and 32-byte digests — never
   verbatim preimage content.

## 8. Field-preimage carriage (three tiers)

How preimage bytes reach a consuming (dispute) transaction. The tiering is
mandated simplest-fitting-first (GOAL_SPEC §3.2): the tier-1 direct and
tier-2 single-reference paths stay enabled wherever they fit, and the tier-3
fallback never becomes mandatory complexity for ordinary proofs. Certification
exists only where flat hashing does not otherwise authenticate a partial
read.

### 8.1 Tier 1 — redeemer carriage

If the preimage fits the consuming transaction's byte budget
(§8.3 `maxTier1RedeemerPreimageBytes`), the step carries the preimage bytes
in its own redeemer, hashes them against the positionally-extracted field
hash, and slices in place. No UTxO, no certificate.

### 8.2 Tier 2 — single raw UTxO

If the preimage fits one publication transaction (`preimage_len ≤ K`), it is
published once as a **nothing-but-bytes inline datum** in a single output at
the prover's own key address. Each consuming step references that output,
hashes the whole preimage against the committed field hash (measured free at
≤ 32 KB), and slices. No certificate — the flat hash is directly checkable.

### 8.3 Carriage constants

These sit between the tier-2 and tier-3 definitions on purpose: tier 2's
bound _is_ `K`, and tier 3 is defined as the `preimage_len > K` case, so both
neighbours read against this table.

| constant                            | value                               | status                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ----------------------------------- | ----------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `K` (chunk size / tier-2 bound)     | ~~15,900 bytes~~ → **15,148 bytes** | **FALSIFIED, re-pinned and applied — erratum E1 below is normative for this row.** 15,148 is the measured reserve-clearing publication frontier, and both `chunk_bytes_k` and `MIDGARD_CHUNK_BYTES_K_V1` now read it.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| `maxTier1RedeemerPreimageBytes`     | **14,336 bytes**                    | **FALSIFIED at the signed-transaction layer; repricing escalated (#611, 2026-08-17).** The evidence-layer reading stands (#580: 15,848-B one-step evidence at the cap, 536 B unspent in the 16,383-B envelope), but the complete **signed** step transaction at the cap — measured for the first time by `complete-item-proof-fit-emulator-v1.test.ts` on the deployed route (resolver sourced by reference) — is **17,389 B against `maxTxSize` 16,384, margin −1,005**. The bisected fitting frontier is a **13,357-B item (13,361-B preimage): exactly 16,384 signed bytes, zero margin, no reserve**; one more byte overflows. The parameter is deliberately NOT re-pinned here — repricing is owner authority and rides the #611 escalation (see the §8.11 erratum update). |
| `maxTransactionAggregateFieldBytes` | 32,768 bytes                        | retained                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| maximum tier-3 chunk count          | `⌈32,768 / K⌉ = 3`                  | derived; unchanged by the re-pin                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

**Every number below this table that is quoted at `K` is quoted at the repaired
15,148**, because the fixtures, the goldens and the compiled validator all sit
there now — the §8.10 three-chunk corner splits `[15,148, 15,148, 2,467]`, and the
§11.4 and §12.5 rows were re-taken at `chunk_bytes_k` when it moved. Where a
superseded figure is retained it is labelled as superseded, because part of what
E1 records is where the earlier analysis went wrong.

Basis. Both values were pinned **provisional-pending-Phase-4-measurement**: each
by analysis over existing measurements, not by a measurement of the final
publication or step transaction — neither of which existed at the time.
Falsification by Phase-4 measurement is an amendment-level erratum to this
table (the _Provisional values_ bullet in this document's front matter) and
does not reopen any GOAL_SPEC acceptance criterion.

**The `K` bullet below is the superseded analysis and is retained, not
corrected, because erratum E1 is partly a statement about where it went wrong.**
Read it as the reasoning that was falsified; the measurement and the re-pinned
value are in E1 immediately after this list.

- **K = 15,900** — the split the #556 prototype bench
  (`proto-556-flat-dispute-bench-v1`, 2026-08-06, case 3) actually
  exercised: a maximal two-chunk reconstruction of 15,900 + 484 = 16,384
  bytes — one `maxTxSize` envelope — hashed in a single `blake2b_256` at
  1,341 mem / 17.4M CPU. What #556 establishes is that **reconstruction
  cost never constrains K**; #558 then carried 15.9 KB forward as the
  working chunk size. #556 did _not_ measure publication capacity, and the
  484-byte remainder is that bench's ragged tail, not a measured
  publication-transaction overhead. The capacity claim behind K is
  analysis: a tier-2/3 chunk is a bare nothing-but-bytes inline datum at a
  key address, and the measured framing for that shape is small —
  `maxFieldPublicationDatumBytes` 4,574 →
  `maxFieldPublicationUnsignedTransactionBytes` 4,675, i.e. 101 bytes of
  unsigned framing (`MIDGARD_V1_ENVELOPE_MEASUREMENTS`,
  `demo/midgard-core/src/consensus-profile-v1.ts`) — leaving room for a
  15,900-byte chunk plus datum envelope, output, fee, and one vkey witness
  inside 16,384. **Phase-4 cross-check, mandatory:** the counted-era
  _complete-item_ publication — a heavier script-custody shape — measured
  two item-size frontiers: `maxExactCompleteItemPublicationBytes` 15,570,
  the largest item whose signed publication lands exactly on `maxTxSize`
  (16,384), and `maxReliableCompleteItemPublicationBytes` 15,073, the
  largest whose publication lands on `maxTxSize` minus the 512-byte
  `proofItemEnvelopeReliabilityReserveBytes`. The reserve is a
  **transaction-side** budget, not an item-side one: the two frontiers are
  497 item bytes apart because that shape's non-item framing is itself 15
  bytes lighter at the smaller size (814 B at 15,570 → 799 B at 15,073).
  Both frontiers are pinned by the "pins the exact applied publication
  frontiers and reliability reserve" case in
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`.
  (**Corrected 2026-08-14**, owner ruling: these two were 15,489 and 14,993,
  about 80 bytes below the shape they describe. The error was internal to
  `MIDGARD_V1_ENVELOPE_MEASUREMENTS` — the same block's
  `maxReliableCompleteItemPublicationDatumBytes` 15,624,
  `...MinAdaLovelace` 68,231,610 and `...FeeLovelace` 853,925 are
  measurements of that same publication and all three land on an item size
  of 15,073. The publication carries no script, so no blueprint change can
  move it.)

  **#580 — the 64-byte tier-1 overhang.** `maxSinglePublicationCompleteItemBytes`
  is 14,396, but §8.4's tier-1 ceiling of 14,336 admits an item of at most
  **14,332** once the 4-byte single-item field-2 envelope is counted. Items in
  (14,332, 14,396] are therefore publishable but **not inline-carriable** — a
  64-byte gap between the publication cap and tier-1 admissibility. It surfaced
  on 2026-08-14 when the publication-maximum case was corrected to select field 2
  rather than field 0. The case itself now runs as "carries one complete item at
  the applied publication maximum through the tier-2 door" in
  `demo/midgard-validation/tests/complete-item-carriage-tiers-emulator-v1.test.ts`,
  which is where a >tier-1 publication belongs.

  **#580 disposition (2026-08-15): real, correct, and not a capability gap. The
  assertion stays, unchanged, as an anti-conflation guard.** The two constants
  answer different questions — 14,396 is the largest complete item _one
  publication transaction_ carries as an inline datum, 14,336 the largest field
  preimage a _step redeemer_ carries — and neither bounds the other. §8.4's
  ladder is a partition, so nothing in the band is stranded: an item in
  (14,332, 14,396] has a field preimage in (14,336, 14,400], which selects tier 2
  `RawUtxo`, and the tier-2 door carries it end to end with every stage inside
  `maxTxSize` — measured green at exactly 14,396 by the row named above. The
  overhang is the tier-1/tier-2 split point sitting 64 bytes below the
  tier-2/tier-3 one, which is the ladder working rather than a hole in it.
  Keeping the assertion is still right, and its value is that **equating the two
  constants would widen tier-1 acceptance onto a basis the deployed step route
  does not match** — the same regression commit `92426384` refused when it
  declined to move `maxReliableDirectCompleteItemBytes` from 8,273 to 13,282. No
  policy-cap change is required and none is taken. One coverage residual is
  recorded rather than closed: the band is measured at both endpoints and at no
  point strictly inside it.

  Returning to the Phase-4 cross-check: K = 15,900 exceeds both complete-item
  publication frontiers above — `maxExactCompleteItemPublicationBytes` 15,570
  and `maxReliableCompleteItemPublicationBytes` 15,073, not the tier-1 figures
  of the #580 note. That is expected, because the tier-2/3
  publication drops the counted proof envelope and the script address, but
  Phase 4 MUST measure the real signed key-address chunk publication and
  re-pin K downward if that transaction does not clear `maxTxSize` with the
  same 512-byte reserve. The certification transaction re-carries no chunk
  bytes and so never constrains K.

- **maxTier1RedeemerPreimageBytes = 14,336** — `maxTxSize` (16,384) minus a
  round 2,048-byte allowance for step machinery (thread-continuity input
  and continuing output, control datum, redeemer framing, reference-input
  entries, script context). This allowance is **an engineering choice, not
  a measurement**: no bench has measured the flat-format step transaction's
  fixed byte overhead. That measurement is #557's pending M2 ("fixed
  per-step overhead in the real thread harness"), executed in Phase 4. It
  is set between two measured anchors — bare Conway proof-transaction
  framing of 395 bytes (`concreteConwayProofTransactionFramingBytes`:
  14,546 argument bytes in a 14,941-byte transaction) and the counted-era
  direct-carriage bound of 8,273 raw item bytes
  (`maxReliableDirectCompleteItemBytes`, in a 15,872-byte proof
  transaction, i.e. ~7.6 KB of overhead). The counted figure is far heavier
  only because that redeemer also carried chunk proofs, frontiers, and
  sibling vectors, all of which the flat format deletes; 2,048 is ~5x the
  measured bare framing and well inside the deleted counted overhead.
  Phase 4 measures the real step transaction at the final grammar and
  re-pins.

Execution-fit for any carriage-dependent path is judged at the single
declared budget basis of GOAL_SPEC §3.3: 13,200,000 memory units.

#### Erratum E1 — `K` is falsified by Phase-4 measurement (2026-08-09)

**Amendment-level erratum**, raised by
[#574](https://github.com/Anastasia-Labs/midgard/issues/574) under the
_Provisional values_ clause in this document's front matter. It does not reopen
any GOAL_SPEC acceptance criterion.

The mandatory Phase-4 cross-check demanded above — "Phase 4 MUST measure the
real signed key-address chunk publication and re-pin `K` downward if that
transaction does not clear `maxTxSize` with the same 512-byte reserve" — has
been taken, and `K = 15,900` does not clear `maxTxSize` **at all**, reserve or
no reserve.

| reading                                                                    | measured                                                              |
| -------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| signed publication of a 15,900-byte chunk                                  | **16,648 B**                                                          |
| overrun against `maxTxSize` (16,384)                                       | **+264 B**                                                            |
| largest publishable preimage (signed transaction lands **on** `maxTxSize`) | **15,644 B** — 16,384 B signed                                        |
| largest publishable preimage with the 512-byte reserve                     | **15,148 B** — 15,872 B signed                                        |
| non-payload framing at the exact frontier                                  | **740 B** (245 B fixed + 3 B datum head + 492 B payload-proportional) |

Every row is a real signed emulator transaction at mainnet
`coinsPerUtxoByte` (4,310); the measurement is
`§8.3 Phase-4 exit measurement — the tier-2 raw-UTxO bound` in
`demo/midgard-validation/tests/field-preimage-carriage-fit-emulator-v1.test.ts`,
and §8.10 states how to re-take it.

**The frontiers are pinned at one-byte resolution.** They are frontiers, so a
sweep quantised to anything coarser reports the largest quantised preimage under
the frontier rather than the frontier itself. An earlier revision of this
erratum swept at the field-1 stride of 40 bytes and published 15,643 / 15,123
— one and twenty-five bytes short respectively, and the exact row's stated
property ("lands on `maxTxSize`") was false of the 16,383-byte transaction it
reported. Both rows above are the real thing: 15,645 bytes is the first payload
that does not fit, and 15,149 the first that does not clear the reserve.

**Framing is not a constant, and that is the shape of the whole result.** The
740 bytes above decompose into exactly three terms:

- **245 bytes of fixed transaction framing** — body, one input, the change
  output, the fee, one vkey witness. Genuinely payload-independent.
- **The CBOR head of the inline datum's byte-string wrapper** — 1 byte below a
  24-byte datum, 2 below 256, **3 below 65,536** and 5 above it. Across the
  whole of the carriage ladder the datum sits in the third band and this term
  is a flat 3, which is why an earlier revision of this erratum folded it into
  the first and published "248 bytes of fixed, payload-independent framing".
  That band is bounded on both sides. Below it the collapsed model _overstates_
  by up to two bytes, which refuses nothing that would have fitted; above it the
  collapsed model _understates_ by two, which is the direction that hands a
  builder a transaction the ledger rejects. It is modelled rather than
  documented around.
- **≈ 3.125 % of the payload.** Above 64 bytes a Plutus Data byte string is
  serialised as an indefinite-length string of 64-byte definite chunks
  (`5f 5840 … ff`), and each chunk pays a two-byte head. At the exact frontier
  that is 492 bytes; at 15,900 it is 500; at 14,336 it is 450.

`midgardCarriagePublicationBytesV1` in
`demo/midgard-core/src/codec/native-tx-carriage-v1.ts` is that decomposition as
a function, and the emulator measurement asserts it reproduces the real signed
transaction size **to the byte at every payload size it is sampled at**, on both
sides of the 24-, 64- and 256-byte boundaries as well as across the ladder. The
two publishable frontiers above are **derived from it** rather than written
down, so the cost model and the bound cannot drift apart.

**Where the analysis went wrong.** The 15,900 estimate was carried forward from
#556's _reconstruction_ bench and justified by the 101 bytes of framing measured
for an **unsigned** 4,574-byte publication. A real publication is signed and has
change, and the datum it carries is not the payload. #556's 101 bytes are the
gap between a 4,574-byte _datum_ and the 4,675-byte _unsigned_ transaction
holding it; the 248 bytes of fixed framing measured here are that envelope plus
a vkey witness, an input, a change output and the fee, and the remaining 492 of
the 740 are the payload's own Plutus Data encoding, which the 4,574-byte figure
had already absorbed and the analysis therefore never re-applied at carriage
scale. #556 never measured a publication and said so; the error was in reading
its silence as an absence of cost.

**The re-pin.** `K` becomes **15,148 bytes** — the reserve-clearing frontier,
chosen over the 15,644-byte exact frontier for the same reason the counted era
chose `maxReliableCompleteItemPublicationBytes` over
`maxExactCompleteItemPublicationBytes`: a bound that lands on the limit to the
byte is not a bound anyone can build against. `⌈32,768 / 15,148⌉ = 3`, so the
maximum tier-3 chunk count is unchanged and no other constant in this table
moves. (The floor below which the chunk count would become 4 is 10,923; the
re-pin is nowhere near it.)

**What the outage was, before the re-pin landed.** This is recorded because the
size of the consequence is the reason the erratum is amendment-level, and because
the shape of it is what the repair had to close. Under the superseded `K`:

- **Tier 2** carries a whole preimage in one publication, so a preimage in
  `(15,148, 15,900]` published as a transaction over the reserve — a 15,500-byte
  preimage publishes as 16,235 bytes, 363 over the reserve and 149 under
  `maxTxSize` even without one — and above 15,644 it does not fit `maxTxSize` at
  all.
- **Tier 3** fared worse, not better. The chunker cuts at `chunk_bytes_k`, and the
  §8.3 guard is a refusal, **not a re-split**, so at `K` = 15,900 _every_ tier-3
  plan — at every preimage length from 15,901 bytes to the §5.4 cap — had a first
  chunk of exactly 15,900 bytes, which publishes as 16,648 bytes and is 264 over
  `maxTxSize`. There was no tier-3 preimage whose carriage could be published.

The unpublishable window was therefore the whole of **(15,148, 32,768]** — every
size above the reliable frontier up to the §5.4 aggregate cap — and not the
`(15,148, 15,900]` sliver a previous revision of this erratum named. That
revision was wrong by a factor of about 23 in the width of the outage and wrong
in kind about tier 3, which it described as merely mis-partitioned when in fact it
did not function.

**With the re-pin applied the window is empty, and that is a property of the
repair rather than a coincidence.** `K` is now _defined_ as the reserve-clearing
publication frontier, so the largest chunk the chunker can cut is the largest
chunk that can be published: tier 2 admits exactly the preimages that fit one
publication, and every chunk of every tier-3 plan — including the two full-`K`
chunks of the §8.10 corner — publishes inside the reserve. An implementation MUST
still refuse to publish carriage larger than 15,148 bytes and MUST fail closed
rather than build such a publication; what has changed is that no honest §8.4 plan
asks it to.

**The prohibition is on publication, not on planning.** A previous revision told
implementations not to _plan_ a field into the affected window. That is the
wrong instrument, and under the real window it is incoherent: since every
tier-3 plan is affected, a plan-time refusal would refuse every tier-3 preimage
that exists, and with it the certificate derivation, the content-addressed
healing check and the §8.10 corner measurement — all of which are correct today
and are precisely the part of the ladder that still works. A plan is a statement
about bytes and the §8.4 split is the pure function that healing and
certification are defined over; it stays total. The refusal belongs where a
transaction is built.

**The build-time guard stays, and is now a guard rather than the mitigation.**
`midgardFieldCarriagePublishabilityV1` reports every chunk of a plan that
`maxTxSize` will not accept and by how much, and
`buildUnsignedFieldPreimagePublicationV1Program` refuses to build one, naming this
erratum. A caller may raise the builder's limit (bounded by the §5.4 cap) for
measurement work, so it is a fail-closed default rather than an inescapable
invariant. What the re-pin changed is what the guard catches: before it, _every_
tier-3 plan, which is how the outage became visible at build time instead of at
submission; after it, only a chunk list that did not come from this chunker, or a
deliberately raised limit. The guard is still deliberately not a re-split, because
the §8.4 chunk boundaries are verified on-chain against `chunk_bytes_k` and
re-cutting them off-schedule would produce carriage the compiled validator
rejects.

**The re-pin is applied in both languages, and what it re-cut.** `K` is the
_split_, not merely a bound, so moving it moved every chunk boundary in the
system, and all of it moved in one commit rather than being carried as a live
spec/code divergence:

- `chunk_bytes_k` in `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak` and
  `MIDGARD_CHUNK_BYTES_K_V1` in
  `demo/midgard-core/src/codec/native-tx-field-access-v1.ts` both read 15,148.
  The TypeScript half is asserted **equal to the derived frontier**
  (`MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES_V1`) rather than merely equal to a
  literal, so `K` cannot drift away from the measurement that fixes it.
- The #569 cross-language straddle vector re-derived from its producer:
  `chunkLengths [15,148, 855]`, and the straddling item moved from 397 to **378**
  (payload `[15,125, 15,163)`, crossing chunk 0's end at 15,148). The producer
  refuses to emit a vector whose named item is not the sole straddling read, so
  the index cannot fall behind a future re-pin.
- The #568 chunk-count goldens re-derived from their producer.
- The §8.10 three-chunk corner re-cut to `[15,148, 15,148, 2,467]` on both the
  door side and the certification side, with the two straddling reads now at items
  378 and 757; §8.10's execution ledger re-taken and its table and readings moved
  in the same commit.
- The §12.5 tier-2 fixtures re-taken at the new bound: 378 field-1 items / 15,123
  bytes, and 1,372 field-6 items / 15,095 bytes.
- The emulator suite re-measured. Its raised-`maxTxSize` blocks were the ones the
  outage forced, and all but the frontier sweep — which must build past the limit
  to find it — now run at the real 16,384 with full-`K` chunks going through the
  ledger.

`expect total_length > chunk_bytes_k` is compiled into
`native_tx_field_access_v1`'s tier-3 view construction and into
`native_tx_carriage_v1`'s certification, precisely so §8.4's partition is a
property of the format rather than a convention; with the two halves agreeing,
that partition and this document's `K` are the same number. Both `K` doc comments
point here.

**What #574 discharges, and what it defers.** #574's AC-1 asks that publish
tooling carry a preimage of any size up to the §5.4 cap. **Half of that is not
discharged as of #574 and was discharged by E1's repair instead.** At the
superseded `K` only `[1, 15,148]` was carriable at the real `maxTxSize`, and the
tier-3 end-to-end exercise ran on an emulator configured with an inflated
`maxTxSize` — honest as a measurement of the _format_, and not evidence that the
carriage was publishable on L1. **The re-pin closes that half.** Every chunk of
every §8.4 plan up to the §5.4 cap now publishes inside the reserve, and the
emulator blocks that had to be inflated — the tier ladder, the tier-3 corner
healing, the min-Ada measurements and the certification round trip — run at the
real 16,384 with full-`K` chunks going through the ledger. The one block that
still raises the limit is the frontier sweep, which has to build transactions past
the limit in order to find where the limit is.

What #574 discharged on its own, and stands unchanged: correct fail-closed publish
tooling; tier-invisible reads through one authenticated view; healing at every
publishable size; and a byte-exact publication cost model derived from, and pinned
against, real signed transactions. The re-pin's own crossing is recorded in E1
above: the §8.10 corner rows in
`onchain/aiken/lib/midgard/native-tx-carriage-v1.test.ak` and
`onchain/aiken/validators/field-preimage-certificate-handlers.test.ak` (whose
`[15,900, 15,900, 963]` split became `[15,148, 15,148, 2,467]`), the #569 straddle
vector, the #568 chunk-count goldens, and the §8.10 execution ledger
(`onchain/aiken/scripts/native-tx-carriage-exec-ledger-v1.json`, re-taken with
`--update` in the same commit as the spec table) all moved together. Two further
limits of the discharge, stated so silence does not imply them: no dispute
transaction is built for any tier — every read in these tests is an in-process
codec call over an authenticated view, not an on-chain step — and tier 1 never
reaches a ledger at all, so E1's tier-1 wire-cost caution rests on arithmetic, not
on a submitted transaction.

**Scope note.** §9's conformance rewrite, the SDK golden generator, the wire
golden tests and the CI step that gates them are #568/#573 surface, not #574's.
They were crossed into deliberately: #573 froze the shared surface with a
carry-forward obligation that any lane adding a cross-language wire type extends
the golden channel rather than starting a parallel one, and #574 adds the §8.6
certificate and §8.8 carriage wire types. Adding them to the existing channel is
the discharge of that obligation; a second channel would have been the defect.
The §8.10 execution ledger and its CI step are a second, smaller crossing, taken
for the same reason: a ledger no workflow runs would reproduce in a new place
exactly the unfalsifiable-cost-claim defect it was added to close.

**The tier-1 bound is not falsified by this erratum, but it is not untouched by
it either.** `maxTier1RedeemerPreimageBytes` is a bound on the _step_
transaction, not on a publication, and its measurement is #557's pending M2
("fixed per-step overhead in the real thread harness"). Nothing in #574 measures
a step transaction, so nothing here falsifies or confirms 14,336; it remains
provisional on its original footing. Two things measured here do bear on it, and
neither was stated in the first revision of this erratum:

> **#580 UPDATE (2026-08-15) — the allowance is now measured, and 14,336
> stands.** The Phase-7 pass measured the step side that #574 could not, through
> `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts` (`keeps
stage-4 one-step evidence O(1) in output size at every admissible output`)
> against the Phase-6 blueprint. At the cap, a 14,336-byte preimage produces a
> **14,795-byte auxiliary** — confirming the 450-byte Plutus-Data chunking figure
> below to within 9 bytes — inside a **15,848-byte one-step evidence** against a
> 16,383-byte envelope. The step framing over the preimage is therefore **1,512
> bytes** of the 2,048-byte allowance, leaving **536 bytes** unspent. The bound
> **stands, not falsified**, and the paragraph below reasoned in the right
> direction: the allowance really is materially tighter than it was set for.
> 536 bytes is the whole remaining headroom, so this is the first figure to
> re-take whenever step machinery grows. The same row shows the bound's other
> half working: one byte past the cap the auxiliary collapses to **10–14 bytes**
> (tier-2 `RawUtxo` at a 14,778-byte preimage, tier-3 `Certified` at 16,388),
> because §8 carriage above tier 1 is reference indices rather than payload.
>
> **What this does NOT discharge.** The reading is of the one-step _evidence_
> CBOR, which is what rides the redeemer — not of a complete signed step
> transaction at the cap, which no suite in this tree builds. By the
> by-reference series in this section a redeemer of 15,848 bytes sits in a
> transaction of roughly 16,278 (redeemer + ~430 bytes of transaction framing),
> inside `maxTxSize` but well short of the 512-byte reliability reserve the
> publication side carries. #557's M2 is therefore **narrowed, not closed**: the
> encoding half is measured, the signed-transaction half is not, and whether the
> tier-1 bound should carry a reliability reserve of its own the way `K` does is
> a parameter question for CG5's target-network binding rather than something
> this pass settles.
>
> **#611 UPDATE (2026-08-17) — the signed-transaction half is now measured, and
> it FALSIFIES the bound.** The new row in
> `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`
> (`measures the complete signed tier-1 step transaction at the 14,336-byte
preimage cap`) builds and submits the at-cap authenticate transaction on the
> emulator against the applied resolver, on both bases. Deployed route
> (resolver sourced from the published reference script, one-step argument
> inline in the redeemer): **17,389 signed bytes, margin −1,005** against
> `maxTxSize` 16,384. Embedded-resolver variant: **20,518 signed bytes, margin
> −4,134** — so the published reference script is load-bearing for step
> liveness anywhere near the cap. The ~16,278 estimate above under-counted the
> framing: beside the redeemer, the signed transaction carries the thread
> input, the continuation output with its authenticated datum, the required
> signer, the resolver reference input and a change output — ~1.8 KB of
> transaction framing over the one-step evidence, roughly four times the ~430-B
> by-reference-series figure, which was taken from a bare measurement
> transaction with none of that protocol shape. The bisected fitting frontier
> on the deployed route: a **13,357-byte item (13,361-byte preimage) lands at
> exactly 16,384 signed bytes — zero margin, no reserve — and one more byte
> overflows** (probe series in the row's `MIDGARD_PRINT_PROOF_FIT` output).
> With a K-style 512-byte reserve the reliable frontier would sit near a
> ≈12,845-byte item (≈12,849-byte preimage; arithmetic, not probed). Whether
> the repair is a smaller cap, a tier-1 reliability reserve, or a documented
> reference/chunked-route requirement above the frontier is CG5 parameter
> authority — **escalated on #611, not decided here**, and the parameter is
> not re-pinned by this update.

- **The 740-byte framing is not a floor.** It was published as "a lower bound on
  any transaction of this family", and the same measurement contradicts that:
  framing is 723 bytes at a 15,123-byte payload and 698 bytes at 14,336, because
  most of it is the payload-proportional term. What is payload-independent is
  245 bytes; the datum's CBOR head adds a further flat 3 across the ladder's
  band (the decomposition above).
- **The Plutus-Data chunking cost applies to tier-1 redeemer carriage too, and
  it is the larger half of what the tier-1 allowance is spent on.** Tier 1
  carries its preimage as a `ByteArray` field of a redeemer, which is Plutus
  Data like any other, so a 14,336-byte preimage occupies **14,786 bytes** on
  the wire — 450 bytes of chunking overhead before any step machinery exists at
  all. Against the 2,048-byte allowance that is 22 % already spent on the
  encoding of the payload itself, leaving ≈ 1,598 bytes for the
  thread-continuity input, the continuing output, the control datum, the
  redeemer framing, the reference-input entries and the script context. That is
  materially tighter than the allowance was reasoned about, and #557's M2 should
  be taken with it in view. It is a caution, not a re-pin: no step transaction
  has been measured, and this erratum does not measure one.

#### Erratum E2 — limits on faulting the witness-set fields (2026-08-09; limit 3 resolved 2026-08-16)

**Amendment-level erratum**, raised by
[#575](https://github.com/Anastasia-Labs/midgard/issues/575) under the
_Provisional values_ clause. It re-pins nothing; it records three limits that a
reader of §8 and §10 would otherwise have to discover by running out of budget —
or, for limit 3 while it stood, by being slashed. Limits 1 and 2 stand; limit 3
is **RESOLVED** by [#606](https://github.com/Anastasia-Labs/midgard/issues/606)
(owner ruling 2026-08-16 — the welded-`field_hash` repair, recorded in full at
limit 3 below).

Two family steps prove an **absence** over a witness-set field:
`missing-native-script-tx` step-06 over field 6 — "the required native script is
not among the transaction's script witnesses" — and `missing-signature` step-04
over field 7. An absence claim is the one shape that has to see every item, so
each means a walk (`fold_opened_field`) over the whole field.

Limit 1 is theirs and it is about **budget**: both walks pass the GOAL_SPEC §3.3
basis before their field's admissible cardinality is reached. Limit 2 is about
field 6 in particular, where being variable-width costs it its authenticated
item count under tier-3 carriage. Limit 3 was neither: it applied to all three
witness-set fields whether or not anything walks them, and it was a
**soundness** limit rather than a budget one.

**Limit 1 — execution.** Both of the wave's unbounded walks are measured
through the real step at both ends of their admissible range and pinned in
`onchain/aiken/scripts/native-tx-q1x-exec-ledger-v1.json`, where each
high-cardinality row is recorded as `basisFit: "exceeds"` with an
`infeasibility` note and a cross-reference back to this erratum:

| reading                          | memory         | cpu                |
| -------------------------------- | -------------- | ------------------ |
| step-06 at 1 script witness      | 627,443        | 203,567,716        |
| step-06 at 224 script witnesses  | **35,584,117** | **11,892,113,320** |
| step-04 at 1 address witness     | 585,338        | 189,224,646        |
| step-04 at 318 address witnesses | **40,237,813** | **12,841,836,720** |
| GOAL_SPEC §3.3 basis             | 13,200,000     | 8,000,000,000      |

(Rows re-taken 2026-08-16 with the #606 regeneration — the compiled step
validators moved with the repair, so the ledger rows moved by fractions of a
percent; every derived figure below — the marginal costs, the ≈81/≈101 fit
points and the binding axis — survives the re-take unchanged.)

`missing-native-script-tx` step-06 walks field 6 at C20.6's 224-witness Cardano
envelope: ≈ 2.7× the memory basis and ≈ 1.5× the cpu basis. From the two
readings the marginal cost is ≈ 156,800 memory and ≈ 52,400,000 cpu per witness,
so the walk fits the memory basis at roughly **81** witnesses and the cpu basis
at roughly **150**; memory is the binding axis. Above ≈ 81 native script
witnesses a missing-native-script fault cannot be finalized in one L1
transaction.

`missing-signature` step-04 walks field 7 at 318 address witnesses — the widest
field 7 §5.4's aggregate cap admits at §5.3's 103-byte stride: ≈ 3.0× the memory
basis and ≈ 1.6× the cpu basis, marginal cost ≈ 125,100 memory and ≈ 39,900,000
cpu per witness, fitting the memory basis at roughly **101** witnesses and the
cpu basis at roughly **196**. Memory is the binding axis here too.

The two together separate the walk from what it walks: field 7 is fixed-stride
and field 6 is variable-width, so step-04 pays no per-item envelope decode and
is still O(N). The linear cost is the **visiting**, which an absence claim
cannot avoid, not the decoding — which is why carriage cannot remedy it and
§10's resumable walk can.

**Where the field-7 row sits relative to carriage, stated plainly.** 318 address
witnesses is a 32,757-byte field 7, which is over the §8.3 tier-1 redeemer bound
and over `K`, so on L1 that preimage travels under tier 3 (admissible for a
witness-set field since #606 resolved limit 3 below). The row is nonetheless
taken under tier-1 carriage in the harness at a width tier 1 could not carry,
and it is published as a _walk-cost_ reading rather than as a reachable
configuration;
`q1x_f6_address_witness_fixture_sits_at_the_admissible_cardinality` asserts
exactly that, so the fact cannot go unnoticed. It does not soften the limit,
because execution binds first and by a wide margin: the widest field 7 tier-2
carriage can deliver is ≈ 154 witnesses, and the walk leaves the memory basis at
≈ 101. The operative statement is **≈ 101 address witnesses**, and it is reached
before either carriage bound.

**Limit 2 — carriage.** The walk needs the field's _authenticated_ item count,
and for a variable-width field that count is authenticated only under tiers 1
and 2. Under tier 3 (Certified) the §5.1 header's number is the prover's own
assertion, so `field_item_count` aborts rather than return it. A field-6
preimage too large for tier-2 carriage therefore cannot be walked at all — the
step aborts, loudly and unconditionally, rather than clamping.

**Limit 3 — RESOLVED (#606, owner ruling 2026-08-16): a witness-set field may
be carried under tier 3.** As found by the #575 round-2 review this was a
**soundness** limit, not a budget one, and until #606 landed it was enforced
as an outright refusal. The finding, its interim enforcement and its
resolution are all recorded here, because part of what an erratum records is
where the earlier analysis went wrong and what closed it.

The hole, as found. Tiers 1 and 2 put the whole preimage in the consumer's
hands, so the §8.8 door hashes it against
`field_commitment_at(body, witness_set, field_index)` and the content is bound
to structures the disputing thread already anchored. Tier 3 exists precisely
because the preimage is too large to hold, so the door never hashes it: the
§8.6 certificate is the binding instead — and at the time, the certificate's
authority was a token named `(tx_id, field_index)` and nothing more.

For fields 0–5 that name was enough. The minting policy re-derives the
transaction id from the body it was handed and takes `expected_hash` off that
same body, and §3's id preimage **is** the body — so a certificate can only be
minted for the field the named transaction actually committed.

For fields 6–8 it was not enough, for the same reason §2.5's anchor has two
arms. The minter reads `witness_set_hash` off the _tail_ of its own redeemer's
`native_tx_compact_cbor`, and §3's id preimage does not reach that tail. A
certifier could therefore present the committed transaction's genuine body —
so the token was minted under the committed transaction's own name — followed
by the `witness_set_hash` of any witness set it chose, and certify a field 6,
7 or 8 preimage that transaction never committed. Both directions of the §2.5
absence rules followed: an empty field 7 makes "the required signature is
absent" true of every transaction, and a fabricated field 7 makes an
invalid-signature fault provable against a signature that was never carried.
Both slash an honest operator. Until the repair, a witness-set field was
therefore **refused tier-3 carriage** outright at
`fraud_proofs/field_opening_v1.carriage_reaches_the_anchor`.

**The repair (#606, owner ruling 2026-08-16, superseding the asset-name shape
the 2026-08-14 deferral described).** The §8.6 datum gained `field_hash` —
the §4 flat commitment of the certified preimage — and the mint welds it:
`certificate.field_hash` must equal the same commitment the chunk
concatenation is verified against, so `field_hash ↔ chunk_digests` is one
mint-verified statement. The asset name became a single constant
(`"MIDGARD_FIELD_PREIMAGE_CERT"`); the derivation was retired, because with
the commitment in the datum the name carries no security weight. The door's
certificate selection then requires the datum's `field_hash` to equal the
commitment it derives from the **anchored** structures — for a witness-set
field, the chain that reaches the anchored `witness_set_hash`. A certificate
minted over a fabricated witness set wears the fabricated hash in its datum
and fails that equality; the mint-level acceptance of the fabrication is
unchanged and harmless, because the forged object can no longer be spent at
any door. `carriage_reaches_the_anchor` was deleted with the repair rather
than left as a guard that cannot fail. The assignment history: written
against #579, moved to #604 with the #575 off-chain remediation (owner ruling
2026-08-13), deferred from #604 to #606 as an on-chain identity move once
#604 measured the cost (owner ruling 2026-08-14), delivered by #606 with the
2026-08-16 amendment; the interim exposure the 2026-08-14 deferral accepted
never reached a live system.

One hypothesis remains recorded as **falsified** so it is not re-tried:
carrying `witness_set_hash` in the thread anchor alone does _not_ repair the
hole. The anchor already carried it — `WitnessAnchor { tx_id,
witness_set_hash }`, checked in `anchored_native_tx` — and that is what closes
the tiers-1/2 forgery, because under those tiers the door hashes the preimage
itself. Under tier 3 the door never hashes the preimage, and there was
nothing in the _token_ for a step to check the anchored `witness_set_hash`
against — which is precisely why the repair had to put the commitment where
the door can compare it: the mint-verified datum.

The off-chain builders' matching refusal (the #604 hardening that duplicated
the door's) lifted with the repair, keeping door parity: builders emit tier-3
carriage for fields 6–8 exactly as the door now accepts it.

Vectors: `field_opening_v1.test`'s tier-3 block states the premise in both
directions — the minting predicate accepts the fabrication on a witness field
(wearing the fabricated `field_hash`) and refuses it on a body field — then
pins the closure from both sides: the forged certificate is refused at all
three witness-set fields, the view, walk and second-open entry points
(`certified_carriage_is_refused_at_the_address_witness_field` and its
siblings), and the welded-hash positives open fields 5–8 against transactions
that genuinely commit the preimage.
`missing_signature_step_04_rejects_certified_carriage` asserts the
forged-certificate rejection at a real step. Disabling the weld expect flips
exactly the vectors that stand on it (mutation-verified at the #606 landing).

**What these are not.** Limits 1 and 2 are not introduced by the #575 rebind:
the retired idiom needed the same item count, and it reproduced and re-hashed
the whole script-witness collection inside the step, so it is not credible that
it was cheaper — but that comparison has **not been measured**, no counted-era
step-06 row exists, and nothing here should be read as a measured claim about
the retired idiom's cost. What is measured is the row above. Limit 3 was
likewise not introduced by the rebind — the tier-3 ladder and the §8.6
certificate are #573/#574 surfaces and the gap was in the wire format, not in
the rebind — but it was **found** by #575's review, held shut by #575's
refusal, and closed by #606's weld.

Nor is either surviving limit remediable by carriage choice: limit 1 is
execution, not wire size, and limit 2 is precisely a statement about carriage.
The resolution for limits 1 and 2 is §10's resumable walk — a checkpoint in
thread state and a fault spread over several transactions — which is
[#565](https://github.com/Anastasia-Labs/midgard/issues/565)'s work, with the
deployed-identity half in
[#579](https://github.com/Anastasia-Labs/midgard/issues/579). What #575 owed
and has delivered is that the limits are **measured and asserted** rather than
latent: the ledger row above goes red if the figure moves in either direction,
including if the step ever starts fitting.

### 8.4 Tier 3 — chunked + certified digest-manifest

For `preimage_len > K`: raw chunks at the prover's key address plus one
small certified digest-manifest UTxO at script custody — the only tier with
on-chain certification, because a flat field hash authenticates the whole
preimage and nothing smaller. Once the preimage is split across
publications, the design provides no other way to verify an individual
chunk before reconstruction, so the certificate supplies that binding.

**Deterministic split rule.** Chunk `j` = bytes `[j·K, (j+1)·K)`; the last
chunk is ragged; minimum-necessary chunks by construction. Determinism makes
independent publishers byte-compatible: identical chunks, identical digest
vectors, interchangeable certificates — anyone's republication heals
anyone's certificate.

**The boundary is enforced, not assumed.** A consumer MUST reject a
certificate whose `total_length ≤ K`. The tiering is a partition, not a
preference: a preimage that fits tier 1 or tier 2 has exactly one admissible
carriage, so one field cannot be carried two ways and simplest-fitting-first
is a property of the format rather than a convention builders are trusted to
follow. Without the lower bound a single-chunk manifest would certify a
preimage of any size, and every structural check tiers 1–2 run at view
construction (§7, item 4) could be side-stepped by re-carrying the same bytes
under tier 3.

### 8.5 Custody

- **Raw carriage (tiers 2–3) is unauthenticated data.** No consumer trusts
  provenance; content is verified by hash at consumption — wrong bytes
  simply fail. Raw chunk/preimage UTxOs live at the prover's own key
  address, ada-only, min-Ada reclaimed by ordinary key spend. Carriage
  UTxOs live under a signer whose UTxO set is managed exclusively by the
  fault-proof tooling; step builders exclude them from inputs and
  collateral.
- **The certificate is authenticated data**: minting policy validates
  content at mint, the token lands at the script address, and spending
  requires burning the token plus the owner's signature.

### 8.6 `FieldPreimageCertificateV1`

```
FieldPreimageCertificateV1 {
  owner: VerificationKeyHash,        -- min-Ada reclaim authority, set by minter
  tx_id: ByteArray,                  -- the L2 tx's id (32 B)
  field_index: Int,                  -- 0..8
  field_hash: ByteArray,             -- the §4 flat field commitment (32 B),
                                     --   mint-welded to chunk_digests (#606)
  total_length: Int,                 -- preimage byte length (ragged-last + offset math)
  chunk_digests: List<ByteArray>,    -- blake2b-256 per chunk, in order;
                                     --   length = ceil(total_length / K)
}
```

`field_hash` was added by [#606](https://github.com/Anastasia-Labs/midgard/issues/606)
(owner ruling 2026-08-16, resolving erratum E2 limit 3): it is the same
commitment the mint checks the chunk concatenation against, restated in the
mint-verified datum so a consumer can compare it to a commitment it
authenticated itself. Where the hash "came from" is irrelevant; that the
chunks were verified against it is everything.

**Mint (certification).** The certification redeemer carries `compact_cbor`
(and `witness_set_compact_cbor`). The policy re-derives the tx-id through
the unchanged §3 derivation, extracts the expected field hash positionally
from the supplied structures (satisfying §4 via the
transitively-committed-by-tx-id clause), verifies
`blake2b_256(chunk_0 ‖ … ‖ chunk_{n-1})` over the redeemer-ordered
referenced raw chunks against that hash, requires the datum's `field_hash`
to equal that same hash (the #606 weld: `field_hash ↔ chunk_digests` inside
one mint-verified datum), and checks `total_length` and every per-chunk
digest against the actual bytes. Order is supplied by the redeemer's
reference-input indices and verified in one shot; per-chunk authentication
at certification time is unnecessary.

The redeemer is consensus wire format on the same footing as §8.8's carriage
types — **constructor order is frozen** (Constr tags 0/1), because an
off-chain minter emits these tags and the compiled policy branches on them:

```aiken
pub type FieldPreimageCertificateMintRedeemerV1 {
  Certify {
    compact_cbor: ByteArray,
    witness_set_compact_cbor: ByteArray,
    chunk_ref_input_indices: List<Int>,   // all-chunks-positional, ≤ 3 (§8.3)
    output_index: Int,                    // the certificate output
  }
  Retire                                  // burn; the spend handler holds authority
}
```

`Certify` carries no identity — publication and certification are
permissionless (§8.7), so the policy checks content and never who supplied it.
The certificate itself is read from the named output's inline datum rather than
from the redeemer, so every field a consumer relies on is proved rather than
asserted: `tx_id` against the §3 re-derivation (32 bytes by construction),
`field_index` against the positional §2.5 extraction (which refuses an index
outside 0..8), `field_hash` against the reconstruction's own expected hash,
`total_length` and every `chunk_digests` entry against the referenced bytes.

**Implementations MUST reject a `field_index` outside 0..8 and a `tx_id` that
is not 32 bytes.** This is a requirement on any conforming §8.6 producer, not
a report of what this mint happens to do. It was previously carried by the
retired asset-name derivation — the two bounds were what made that 33-byte
preimage unambiguous — and #606's constant name removed the derivation, not
the requirement. Both remain enforced on the certification path (the §3
re-derivation yields 32 bytes by construction; the positional extraction has
no slot outside 0..8), and `field_hash` joins them under the weld: it MUST be
the 32-byte §4 commitment of the certified preimage, which the mint's equality
against the reconstruction is what enforces.

`owner` is the exception and is only length-checked (28 bytes), because it is
the minter's own choice of min-Ada reclaim authority and no consuming step
reads it — it has to be a spendable key hash or the output is dead, and that
is the whole of the requirement. `witness_set_compact_cbor` is required for
all nine fields, not only 6–8, because certification happens once per field
and one unconditional code path is worth more than one saved hash.

**Certificate output shape.** The output the redeemer names carries the
certificate as an inline datum, no reference script, and exactly one asset of
the certificate policy — the constant name at quantity 1, stated as the whole
per-policy asset list, because the design requires that no second name of the
policy ride in alongside the proved one. Its address is the certificate
script's own payment credential with **no stake credential**, so certificates
live at one enumerable address and the design requires the deposit's staking
rights to stay unassigned rather than being pointed elsewhere on the way past.

**Token.** Quantity 1; one **constant asset name** for every certificate of
the policy ([#606](https://github.com/Anastasia-Labs/midgard/issues/606),
owner ruling 2026-08-16, superseding the retired
`blake2b_256(field_index_byte ‖ tx_id)` derivation). Duplicate certificates
are permitted and harmless — each is independently sound; two certificates
for the same `(tx_id, field_index)` (or even the same content) may coexist as
same-name tokens, and consumers disambiguate by **datum**, never by token
alone.

The constant is normative, because the minting policy pins it and an
off-chain minter has to reproduce it:

```
asset_name = "MIDGARD_FIELD_PREIMAGE_CERT"     -- ASCII, 27 bytes
```

The name is branding, not identity: everything the retired derivation encoded
is in the mint-verified datum, and with `field_hash` in the datum the name
carries no security weight — but the mint's single-pair check still requires
exactly this name at quantity 1, so a token of the policy is always the
constant name over a datum the mint proved. Discovery moves with it: an
indexer enumerates the single certificate address (§8.5 pins it to one shape)
and filters by datum, rather than looking a derived name up.

**One multi-handler validator.** The same script carries the `mint` and
`spend` handlers, so the policy id and the spend credential are one script
hash — mint sends to its own address; spend burns its own policy plus owner
signature. No external reference-script bootstrap, no cyclic dependency.

**Consumption.** A certificate serves any step, thread, or game disputing
the same transaction, indefinitely; it is game-, block-, and
source-agnostic. A consuming step selects a certificate by the policy's
constant-name token on a reference input, then matches the datum's
`(tx_id, field_index)` only against **authenticated** sources (the thread's
already-authenticated disputed transaction) — never redeemer-supplied
identity — and requires the datum's `field_hash` to equal the commitment it
derived from those authenticated structures (#606; for a witness-set field,
the chain that reaches the anchored `witness_set_hash`). A certificate
minted over a fabricated witness set wears the fabricated hash in its datum
and fails that equality at the door. Post-certification single-chunk access
authenticates at O(one chunk hash) against the digest vector (worst case two
chunk hashes on a straddling item); for a fixed-stride field `count` derives
from the mint-verified `total_length` with no chunk hash spent.

**Wiring constraint (normative, for whoever wires a consumer).** The
`certificate_policy_id` that the access door checks the manifest UTxO's token
against MUST reach the consuming script as a **compile-time validator
parameter**, the same applied-parameter mechanism every other cross-script hash
in this system uses. It MUST NOT arrive in a redeemer, a datum, or any other
run-time argument, and no consuming script may accept it from one. That single
value is the whole of tier 3's authentication: the door trusts a certificate
because a token of that policy sits on the reference input carrying it, and the
mint that proved the certificate's content is that policy. A redeemer-supplied
policy id lets a prover name a policy they control, mint a token of it over a
datum nobody checked, and hand the door a fully-formed "certificate" for a
field the transaction never committed — collapsing the §8.4 binding without
tripping a single check the door runs. It is the one parameter in §8 where a
run-time source is not a weaker check but no check at all.

**A variable-width field has no tier-3 count.** Fields 2, 5, 6 and 8 have no
arithmetic count, so theirs lives only in the §5.1 header, and no affordable
check authenticates it here. The §5.1 full-content walk that tiers 1–2 run at
construction is not the same cost under tier 3: a chunk is re-verified every
time a read lands in it, so an `N`-item walk spends `N` `blake2b_256` hashes
over a whole chunk rather than one over the preimage. A consumer therefore
MUST abort when asked for such a field's item count (§7, item 4) instead of
returning the header's number: a rule that consumes an unauthenticated count
would be satisfiable by a preimage its producer miscounted on purpose.
Item reads are unaffected. Two consequences are recorded rather than fixed,
both of them fail-closed or answer-preserving:

- a variable-width field above `K` cannot serve a count-consuming rule under
  tier 3 at all — a liveness limit, and one that bites only where the
  per-read chunk re-verification already makes high-index access impractical;
- a fixed-stride field's tier-3 count is derived by length and never consults
  the §5.1 header, so a preimage whose header miscounts is refused at tiers
  1–2 and simply parsed by length here. The answer is still the true item
  count, never an inflated one. Closing that gap would cost one chunk hash on
  the cheapest path in the format, against the "no chunk hash spent"
  guarantee above; an authenticated `item_count` field on the certificate is
  the way to close it if a later ticket decides the guarantee is worth
  spending.

### 8.7 Publisher, funding, reuse, cleanup

Publication and certification are **permissionless** — the policy checks
content, never identity; in practice the challenger publishes. Cleanup is
owner-discretionary; no forced cleanup, no time-locks. A mid-game yank (raw
chunk spend or certificate burn) is self-healing: republish identical bytes
anywhere and/or re-certify from raws; worst case is fees and delay.
**Content addressing is mandatory**: carriage is identified by digests,
never by `OutputReference` — nothing in the dispute machine may reference
carriage by UTxO identity.

### 8.8 Carriage wire types (consumers outside the validation machine)

The single access door (`authenticated_field_view` in the lib-level
field-access module) speaks all three tiers through frozen sum types.
**Constructor order is frozen consensus wire format** (Constr tags 0/1/2);
off-chain builders emit exactly these tags:

```aiken
pub type FieldCarriageV1 {
  Inline { preimage: ByteArray }                                              // tier 1
  RawUtxo { ref_input_index: Int }                                            // tier 2
  Certified { cert_ref_input_index: Int, chunk_ref_input_indices: List<Int> } // tier 3
}

pub type FieldViewV1 {
  Whole { bytes: ByteArray, count: Int, stride: Int }                         // tiers 1–2
  Chunked { chunks: List<ByteArray>, chunk_digests: List<ByteArray>,
            count: Int, stride: Int }                                         // tier 3
}
```

Tier-3 chunk lists are all-chunks-positional: element `k` is the
reference-input index of chunk `k` (≤ 3 chunks under §8.3). Reference-input
indexing is positional (redeemer-supplied indices). The §7 invariants
(abort-never-clamp, count consistency, positional identity) are normative
for the door and every accessor built on it.

### 8.9 Relationship to adjacent machinery

The MPF proof-chunk carriage (issue #545 idiom) remains a parallel
convention with separate types; MPF trie roots, DA payload framing, and the
`mpf-chunked-verify` validators are not field commitments and are untouched
by this document. The counted-era carriage constants
(`maxTransactionFieldChunkBytes = 4,095`,
`maxSinglePublicationCompleteItemBytes = 14,396`) are superseded by §8.3 and
are prohibited in new surface.

### 8.10 Cost claims — the carriage exit measurements

This is the **Phase-4 lane exit criterion**, and like every other number in this
document it is established by measurement rather than asserted. Execution
figures are taken against the GOAL_SPEC §3.3 basis of 13,200,000 memory units
and 8,000,000,000 CPU units; byte figures are taken against the deployment floor
`minSupportedL1MaxTxBytes = 16,384`. All of it is **provisional pending Phase-7
confirmation**: Phase 7 re-takes the execution rows against the final blueprint,
and what is below is the Phase-4 signal.

Three measurements were owed, and the third produced erratum E1 (§8.3).

#### The three-chunk corner

#556 established a two-chunk reconstruction _in fixture_. What was still owed is
the three-chunk corner opened through the real door with the four reference
inputs a consuming step carries — one certificate and three chunks. The fixture
is field 1 (stride 40) at 819 items and 32,763 bytes, the largest fixed-stride
preimage under the §5.4 cap; it splits `[15,148, 15,148, 2,467]` — at the
**repaired** `K` (§8.3 E1), which is where the compiled validator and every
fixture now sit — both chunk boundaries fall inside an item, and
`tier3_corner_fixture_sits_at_the_three_chunk_corner` asserts every one of those
so the rows stay quoted where they were taken.

The rows are a controlled family: each builds the same fixture and opens the
same door, and they differ **only** in which items are read. Row 0 stops before
opening the door at all, so the fixture — which dominates every absolute figure,
exactly as §12.5 found — subtracts out.

| #   | seam test                             | what it adds                    | memory  | CPU         |
| --- | ------------------------------------- | ------------------------------- | ------- | ----------- |
| 0   | `tier3_corner_fixture_only`           | fixture only, door unopened     | 401,937 | 227,660,288 |
| 1   | `tier3_corner_open_only`              | + the door                      | 640,675 | 299,793,903 |
| 2   | `tier3_corner_one_read`               | + one item in chunk 0           | 795,817 | 393,852,884 |
| 3   | `tier3_corner_two_reads`              | + a second item in chunk 0      | 952,563 | 488,343,213 |
| 4   | `tier3_corner_straddling_read`        | one item across chunks 0/1      | 833,470 | 420,795,629 |
| 5   | `tier3_corner_second_straddling_read` | one item across chunks 1/2      | 856,415 | 414,687,933 |
| 6   | `tier3_corner_last_chunk_read`        | one item in the 2,467-byte tail | 828,524 | 364,033,650 |

**Rows 1–6 re-taken under #592, by one fixed per-view amount, and row 0 did not
move then.** Every measured row rose by exactly **+5,944 mem / +1,838,206 cpu**.
The cause is that tier 3 is now read two ways: `certified_view` keeps the lazy,
chunk-by-chunk form these rows measure, and `authenticated_whole_field_view` —
which the validation machine needs, because its phases consume §5.2's item count
and the lazy view refuses to answer it for a variable-width field — materialises
the same chunks whole. Both need the identical §8.4/§8.6 manifest checks, so
those moved into one `certified_chunks` returning a `CertifiedChunksV1` record,
and a tier-3 view construction now pays one record construction and destructure
more than it did. The shift is **per view**, not per read, and the measurements
say so: it is the same figure on the open-only row and on every read row, so it
cancels out of readings 2, 3 and 4 below, which #592 left unchanged to the unit.

**All rows re-taken again under #606 (2026-08-16), including row 0.** The
repair welds the §4 commitment into the certificate datum, so the fixture's
producer now hashes the whole 32,763-byte preimage once at certificate
construction — that is the CPU rise every row shares, and it subtracts out of
every published difference. The door's own movement (the constant-name token
check plus the welded-hash equality replacing the derived-name check and its
33-byte hash) nets to −475 mem / +344,101 cpu on the open cost; readings 2–4
are unchanged to the unit.

Absolute units, not rounded figures, because the readings are **differences** and
a difference of rounded numbers is not a measurement. Every row is the
`execution_units` field of the structured `aiken check` report for that test.

**Every execution figure in this section is pinned, not transcribed.** An
earlier revision of §8.10 wrote these numbers into the table by hand and nothing
in the repository asserted them — a grep for any of them returned hits only in
this document, so the validator could have drifted arbitrarily far from the
published cost without a suite going red. A cost claim nothing can falsify is
not a measurement. The rows, the derived deltas below, and the binding axis of
the read budget now live in
`onchain/aiken/scripts/native-tx-carriage-exec-ledger-v1.json` and are checked
against a fresh measurement by

```
MIDGARD_AIKEN_BIN=<fork> node scripts/verify-carriage-exec-ledger-v1.mjs
```

from `onchain/aiken/`, which re-runs both modules through
`run-focused-check.mjs`, compares every reading to the unit, recomputes every
subtraction this section publishes, and re-derives which axis binds. A
legitimate re-take is recorded with `--update`, which rewrites the ledger and so
requires this table to move in the same commit. Aiken tests cannot assert their
own execution units — the units are the check report's observation of the test,
not a value in scope — which is why the pin lives one level up rather than
inside the tests, as the byte-level constants' pins do.

The rows are neutralisation-pinned rather than merely green, and the ledger runs
the neutralisation selectors in the same invocation as the rows so a re-take
cannot quietly drop one: `tier3_corner_refuses_a_tampered_tail_chunk` is the
same fixture with one byte of the ragged tail changed, and it is refused — so
the reads being measured are reads that consult the certificate's digest vector,
not reads that would have returned something for any bytes at all.

Four readings.

1. **Opening the corner costs 238,738 mem / 72.13 M CPU and no chunk hash**
   (row 1 − row 0). That is 1.81 % of the memory basis and 0.90 % of the CPU
   basis, for four reference inputs resolved, the certificate's constant-name
   token found and its datum's `(tx_id, field_index, field_hash)` matched
   (#606), the split shape checked and the count derived.
   §8.6's "no chunk hash spent" for a fixed-stride count is not a figure of
   speech: three chunks — two of 15,148 bytes and one of 2,467 — sit in the view
   unhashed.
2. **One item read costs 155,142 mem / 94.06 M CPU** (row 2 − row 1) — a wrapper
   read and a payload read, each re-verifying the 15,148-byte chunk they land in.
3. **Reads are linear and there is nothing to amortise.** Row 3 − row 2 is
   156,746 mem / 94.49 M CPU, within 1 % of the first read. Tier 3 re-verifies
   per read exactly as §8.4 says, and a re-take that found a cheaper second read
   would be finding a bug, not an improvement.
4. **Per-read cost tracks the chunk touched, not the preimage.** Against row 2:
   a straddling read at boundary 0/1 adds 37,653 mem / 26.94 M CPU (one further
   full-chunk verification); at boundary 1/2 it adds 60,598 mem / 20.84 M CPU,
   less CPU because the third chunk is 2,467 bytes rather than 15,148; and
   reading wholly inside that ragged tail **saves** 29.82 M CPU. This is the
   property tier 3 is sold on, and it is now measured rather than argued. The
   saving is on the CPU axis only, and the two axes disagree: row 6 costs 32,707
   memory units _more_ than row 2 (828,524 against 795,817) while costing 29.82 M
   CPU less, because the read still allocates a view over three chunks and only
   the hashing shrinks. Quoting the CPU saving without the memory rise would be
   quoting half a measurement — the tail read is cheaper on the axis that is
   quoted and dearer on the other one.

   **The ragged tail's CPU saving shrank with §8.3 erratum E1's repair of `K`,
   and that is the arithmetic rather than a regression.** At the superseded
   `K` = 15,900 the tail was 963 bytes against a 15,900-byte full chunk and the
   saving was 36.89 M CPU; at 15,148 the tail is 2,467 bytes against a
   15,148-byte full chunk, so both the ratio and the absolute saving are
   smaller. The property being measured — per-read cost tracks the chunk
   touched — is unchanged and is what the two figures agree on.

**The per-step read budget at the corner is ≈ 83 items**, and the two axes agree
almost exactly: `(13,200,000 − 238,738) / 155,142 = 83.5` by memory and
`(8,000,000,000 − 72,133,615) / 94,058,981 = 84.3` by CPU. **Memory is the
binding axis** — 83.5 is the smaller of the two, so the ≈ 83 figure is the memory
one and a budget taken from the CPU axis alone would be optimistic by most of a
read. The margin between the axes is thin (0.9 %) and **the binding axis has
already swapped once**: at the superseded `K` = 15,900 CPU bound it at 82.2
against memory's 83.5, and §8.3 erratum E1's repair cut the per-read chunk hash
enough to move CPU above memory. That is why both axes are published rather than
only the binding one, and why the ledger records which axis binds as a derived
value rather than leaving it in prose. A dispute needing more reads than that over
one field at the corner is a dispute that must checkpoint (§10), which is what
§10 is for.

#### Certificate mint and spend

Each figure is paired with a control that builds the same transaction and does
not run the handler, so what is published is the handler's own work. These rows
are in the same ledger and are checked by the same command as the corner rows
above; the ledger names their module and selectors, so the table below and the
measurement cannot drift apart.

The neutralisation here is `certificate_mint_rejects_a_tampered_corner_chunk`:
the corner certification with one byte of the ragged tail changed, refused.
Without it the corner row would be a measurement of a handler that might have
returned `True` for anything.

Both `mint` rows are taken at the **repaired** `K` = 15,148 (§8.3 E1): the
smaller is a `K + 1` two-chunk certification and the corner splits
`[15,148, 15,148, 2,467]`. They moved on the CPU axis when `K` did — a full chunk
is 752 bytes shorter to hash — and were re-taken in the same commit as the
re-pin.

| handler          | size               | control mem | measured mem | control CPU | measured CPU | **handler cost**                  | % of basis      |
| ---------------- | ------------------ | ----------- | ------------ | ----------- | ------------ | --------------------------------- | --------------- |
| `mint` (Certify) | `K + 1`, 2 chunks  | 428,459     | 1,068,325    | 174,578,313 | 435,662,448  | **639,866 mem / 261,084,135 CPU** | 4.85 % / 3.26 % |
| `mint` (Certify) | 32,763 B, 3 chunks | 461,145     | 1,199,026    | 252,874,606 | 610,309,281  | **737,881 mem / 357,434,675 CPU** | 5.59 % / 4.47 % |
| `spend` (retire) | any                | 258,535     | 486,878      | 134,044,978 | 259,766,844  | **228,343 mem / 125,721,866 CPU** | 1.73 % / 1.57 % |

(Rows re-taken 2026-08-16 with #606: the controls fell because the fixture's
retired asset-name derivations went with the constant name, the mint's handler
cost is all but unmoved — the weld expect is one 32-byte equality against a
commitment the reconstruction already computed — and the spend handler traded
the name derivation's hash for a larger datum to decode: −8,332 mem /
+12.78 M CPU, still the smallest row in the table by memory.)

Both axes carry their control and their measured reading, so every published
cost is a subtraction the reader can perform. The earlier revision of this table
published the CPU deltas alone, which made them un-recomputable and therefore
un-checkable — the one thing a controlled measurement is for.

Certification at the corner is 5.59 % of the memory basis and 4.47 % of the CPU
basis, so it fits its transaction with an order of magnitude to spare, and the
step from two chunks to three costs 98,015 mem — one more chunk digest and one
more full chunk of reconstruction hash. Retirement is size-independent by
construction: the spend handler reads the datum's `owner`, names the constant
token (#606) and checks it does not survive, and never touches carriage.

**Min-Ada, at mainnet `coinsPerUtxoByte` = 4,310.** From
`§8.6 Phase-4 exit measurement — certificate min-Ada and the one-transaction question`:

| output                                        | payload  | inline datum | min-Ada (lovelace) | min-Ada         |
| --------------------------------------------- | -------- | ------------ | ------------------ | --------------- |
| certificate manifest (3 digests)              | —        | 210 B        | 2,064,490          | **2.0645 ADA**  |
| full chunk (`K` bytes, repaired `K` = 15,148) | 15,148 B | 15,624 B     | 68,231,610         | **68.2316 ADA** |
| ragged tail chunk                             | 2,467 B  | 2,547 B      | 11,869,740         | **11.8697 ADA** |

(The manifest row moved with #606: the datum gained the 32-byte mint-welded
`field_hash` plus its 2-byte CBOR head, 176 → 210 bytes, and the deposit
followed at `coinsPerUtxoByte` = 4,310. The chunk rows are datum-only bytes
and did not move.)

The payload and datum columns are separate on purpose: min-Ada is charged on the
serialised output, so it is the **datum** column it is proportional to, and the
gap between the two is the §8.3 E1 Plutus-Data chunking cost (≈ 3.125 %) turning
up as deposit. An earlier revision of this table labelled the datum column with
the payload figure, which is what the two columns exist to keep apart. All three
rows were re-taken when erratum E1 repaired `K`; the manifest row is unmoved
because three digests are three digests either way, while the full chunk fell
from 71.5762 ADA (15,900 B in a 16,400-byte datum) and the ragged tail rose from
5.1849 (963 B in 996) as the split moved.

The asymmetry is the point of tier 3: certifying is nearly free, and the deposit
that is actually large sits on raw carriage the publisher reclaims by an ordinary
key spend (§8.5). A three-chunk corner ties up ≈ 150 ADA in reclaimable deposits
for the life of the dispute — **derived**, as the sum of the measured rows above
(2 × 68.2316 + 11.8697 + 2.0645 = 150.3974), and stated to the ADA because that
is the resolution the claim is made at. (The total is all but unmoved by erratum
E1's repair of `K`, and that is arithmetic rather than luck: the deposit tracks
the serialised bytes, and re-cutting the same 32,763-byte preimage moves bytes
between the full chunks and the tail without changing how many there are.) The four rows it sums are the pinned
quantities; this total is a convenience.

**Last-chunk publication and certification do not fit one transaction, and the
reason is structural rather than budgetary.** §8.6 resolves chunks from
_reference inputs_, and the Cardano ledger resolves reference inputs against the
UTxO set as it stands **before** the transaction; an output the same transaction
creates is therefore not available to it, at any size and under any protocol
parameters. A second and independent reason is measured: a signed full-`K`
publication is already 15,872 bytes on its own — the whole reserve-clearing
budget (erratum E1) — and adding the
§8.6 redeemer (531 B over a 400-byte compact structure and a 100-byte witness
set) and the manifest output (210 B since #606's welded `field_hash`) puts a
lower bound of 16,613 bytes on the combination against a
16,384-byte limit — over budget even before the minting-policy witness. (At the
superseded `K` the publication alone was 16,648 bytes and the bound was 17,355;
E1's repair brought the publication to 15,872 and the bound to 16,579, and
#606's larger manifest raises it to 16,613 — still over, in the same
direction.) **A
tier-3 publication is therefore always `n + 1` transactions**, and builders
must not be written expecting otherwise.

#### The tier-2 raw-UTxO bound

The flat successor to the counted era's `maxSinglePublicationCompleteItemBytes`
= 14,396. Measured as real signed emulator transactions by
`§8.3 Phase-4 exit measurement — the tier-2 raw-UTxO bound` in
`demo/midgard-validation/tests/field-preimage-carriage-fit-emulator-v1.test.ts`,
run by `pnpm --dir demo/midgard-validation exec vitest run
tests/field-preimage-carriage-fit-emulator-v1.test.ts`:

| bound                        | preimage     | signed transaction |
| ---------------------------- | ------------ | ------------------ |
| exact (lands on `maxTxSize`) | **15,644 B** | 16,384 B           |
| reliable (512-byte reserve)  | **15,148 B** | 15,872 B           |

Both are swept at **one-byte resolution**: 15,645 is the first payload that does
not fit and 15,149 the first that does not clear the reserve. Non-payload
framing at the exact frontier is **740 bytes**, of which 245 is
payload-independent and 3 is the datum's own CBOR head (§8.3 E1).

The framing and chunking figures §8.3 E1 and this section quote inline — 740,
723 and 698 bytes of non-payload framing at 15,644, 15,123 and 14,336; 492, 500
and 450 bytes of Plutus-Data chunking overhead; and the 16,235 / 363 / 149
worked tier-2 example — are **derivations of the cost model, and are asserted**
in `demo/midgard-core/tests/native-tx-carriage-v1.test.ts` rather than left as
prose a reader has to recompute.

**What the flat bound actually beats, measured like for like.** The counted era's
two frontiers are `maxExactCompleteItemPublicationBytes` = 15,570 and
`maxReliableCompleteItemPublicationBytes` = 15,073, and those are the
comparable numbers: they are frontiers, found the same way, judged against the
same 16,384-byte floor and the same 512-byte reserve. Against them the flat
bound is **+74 B at the exact end and +75 B at the reliable end**. The counted
reliable publication's transaction measured 15,872 bytes, the same figure the
flat reliable frontier lands on; the flat format buys 75 more payload bytes
inside an identical transaction budget. The one-byte difference between the two
ends is accounted for and is not two different gains: across the 512-byte reserve
the counted shape's non-payload framing steps by 15 (814 B at 15,570 → 799 B at
15,073) while the flat shape's steps by 16 (740 B at 15,644 → 724 B at 15,148).

**Correction, 2026-08-14 (owner ruling).** This section previously reported
**+155 B at both ends** and called it "the same gain twice, which is what one
expects when the deleted proof envelope is a fixed cost". That figure was
**overstated by roughly half**, and the tidiness of the coincidence was part of
why it went unchallenged. The cause was not in the flat measurement but in the
counted-era frontiers it subtracts from: they were pinned at 15,489 and 14,993,
about 80 bytes below what the counted publisher actually reaches, while the three
sibling measurements of that same publication in the same
`MIDGARD_V1_ENVELOPE_MEASUREMENTS` block had recorded 15,073's datum bytes,
min-Ada and fee all along. The gain is +74 / +75 B. It is smaller and it is not
symmetric, and both of those are the measurement rather than the story.

A previous revision of this section compared against
`maxSinglePublicationCompleteItemBytes` = 14,396 and reported **+1,247 B** exact
and **+727 B** reliable, attributing the difference to "the deleted proof
envelope and script custody showing up as capacity". That is wrong by about
17×, and the stated cause is not the cause: 14,396 is an **applied policy cap**
the counted publisher was configured with, not a measured frontier — at that cap
the counted publisher produced a 15,256-byte transaction and retained 1,128
bytes of unused headroom below the deployment floor. Comparing a frontier to a
cap measures the size of the cap's safety margin, not the format's gain. (Those
last two are **counted-era** figures, quoted from that format's own publisher
and not derivable from the flat cost model above; 1,128 is `16,384 − 15,256`.
They are illustrative of why the comparison was wrong, and nothing in the flat
format depends on them.)

This is also the measurement that produced **erratum E1**: `K = 15,900` overruns
`maxTxSize` by 264 bytes and was re-pinned to the reliable frontier, 15,148 —
applied in both languages. See §8.3.

### 8.11 Forced-order material carriage (normative)

An **L1 forced order** commits to an L2 transaction it wants included: its datum
is `TxOrderPayloadV1 { tx_id, transaction_commitment, source }`, and its
`source`'s compact structures carry §4's nine field commitments. This subsection
is normative for how that transaction's material reaches L1. (Owner ruling,
2026-08-11; it supersedes the earlier all-fields-empty stopgap.)

**Durable availability is L1 history plus digest addressing, not a live UTxO.**
The order **mint** is the only on-chain reader of the material. The order's spend
path verifies settlement `forced_transactions_root` membership and never touches
a preimage; a later dispute re-carries the bytes digest-checked under this
section. So once the mint has authenticated a field's preimage against its
committed hash, those bytes are permanent L1 history — which is what the operator
and the node's ingestion walk read — and nothing afterwards depends on the
carriage UTxO continuing to exist.

**The order datum carries no carriage identity.** No `OutputReference` list, no
preimage bytes: §8.7's mandatory content-addressing rule applies here with no
exception, so the nine commitments are the whole material directory. A consumer
that wants a field looks it up by digest. (The 2026-08-11 ruling text cites §8.5
for this rule; §8.5 is _Custody_ and the content-addressing requirement is
§8.7's. The correction is recorded on #594 and every downstream citation is
written against §8.7.)

**Carriage is prover-chosen per non-empty field, supplied in the mint
redeemer.** For each of §2.5's nine slots whose committed hash is not
`field_commitment(#"80")`, the order's mint redeemer supplies one §8.8
`FieldCarriageV1`, and the mint authenticates it against that slot's commitment
through the field-access door:

- The vector is **positional over the non-empty slots in ascending field
  index**, not `(field_index, carriage)` pairs. §4 has no field-index domain
  separation, so a supplied index would have to be checked against the slot it
  claims before it could be used, and the mint's walk over the nine commitments
  already knows that slot.
- The vector MUST be **exhausted exactly**. A short vector leaves a field with
  material uncarried; a spare entry lets two distinct redeemers authenticate one
  order, the second naming reference inputs nothing verified.
- An **empty slot consumes no entry, and its declared length MUST be 1.** A slot
  whose committed hash equals `field_commitment(#"80")` has no carriage, so the
  door never runs there and nothing else in the walk would ever look at the §2.4
  entry for it — yet that entry is inside `transaction_commitment`. §5.1 fixes the
  empty preimage at the one-byte header `80`, so the mint MUST assert
  `declared_length == 1` at every empty slot. Skipping the assertion leaves one
  committed claim per empty field unchecked, which is nine of them for the
  canonically-empty order.
- The mint MUST additionally require each authenticated preimage's own length to
  equal the §2.4 `NativeTxFieldPreimageLengthsV1` entry for its slot. The two are
  independently committed statements about one field and §4's flat hash does not
  collapse them (§12's `reject_field_preimage_size` is the dispute-side form of
  the same disagreement); for an order it is the creator's payload contradicting
  itself and the mint fails closed.
- The door entry point MUST be the **whole-materialising** one. Under the lazy
  tier-3 form a `Certified` field's chunk bytes stay unhashed until an accessor
  touches one, and this mint touches no items — so a tier-3 order would be
  authenticated with the manifest checked and the named chunk reference inputs
  never read, which is precisely the availability claim being made.

**Cost of the walk (measured, #594).** It splits exactly where `whole_view`'s
§5.1 count-consistency check splits, and the earlier one-line claim — "bounded by
§5.4's 32,768-byte aggregate" — is true about bytes and wrong about execution:

- For the five **fixed-stride** fields (0, 1, 3, 4, 7) the cost is §12.5's tier-2
  per-step full-preimage re-hash and is bounded by §5.4's **per-field** byte
  bound, as published. Measured: field 0 at 819 items / 32,763 bytes costs
  1,088,129 memory units against 1,008,355 at one item — 79,774 more for 32,758
  more bytes.
- For the four **variable-width** fields (2, 5, 6, 8) it additionally pays §5.1's
  `walk_to_end`, one item head per item. Measured at ≈21,062 memory units per
  item, which reaches §3.3's execution basis near **536 items** while §5.4's byte
  bound admits 16,382 minimum-width items in one such field. On those slots the
  item count, not the byte count, is the operative bound; the worst shape this
  mint admits at §5.4's bound measures 344,075,442 memory units, 26× the basis.
  Pinned by `onchain/aiken/scripts/tx-order-mint-exec-ledger-v1.json`; the
  over-basis shape and its erratum are recorded on #594 for #580.

  **#606 re-take (2026-08-16).** The E2 certificate repair cost this lane a
  **constant**: both variable-width rows rose by the same +6,500 memory /
  +1,040,000 cpu, so the per-item price is unchanged to the unit
  (21,062.03 memory / 6,275,214 cpu) and only the intercept moved,
  1,885,921 → 1,892,421 memory — which is enough to take the published ceiling
  from 537 to **536**. Cpu at the crossing is about 4.02G against the 8G basis,
  so memory still binds. Unlike §12.7 and §12.8 this figure remains an
  **extrapolation from the two rows rather than a bisected reading** — this
  lane has no boundary-pair selectors pinning its crossing, and adding them was
  not part of the re-take. The aggregate-bound row's own share rose with it,
  from about 1.51M to about 1.53M memory, 11% of the §3.3 basis to 12%. The
  344,075,442 reading above is **pre-#606 and deliberately not re-taken** (no
  selector pins it); extrapolating the re-measured pair puts that shape within
  0.05% of it, so its 26× judgement is unaffected.

- **The bound the mint enforces is per-field, not aggregate.** `whole_view`
  checks `total_length ≤ max_transaction_aggregate_field_bytes` at each opening
  and nothing in the walk sums the nine, so nine fields at 32,768 bytes each are
  mint-admissible. §5.4's aggregate is a property of a valid L2 transaction that
  the consensus rules enforce elsewhere; it is not a mint guard, and this
  subsection does not claim it as one. Whether the mint should also enforce the
  aggregate is recorded on #594.

**A burn carries no vector.** The tx-order minting policy's redeemer is the
shared user-event mint redeemer wrapped beside the carriage vector, so both the
authenticating mint and the NFT burn parse the same type. A burn reads no
material, so the policy MUST require its vector to be **empty** rather than
ignore it: an unread wire field is a second spelling of the same transaction
(§6.1), and two burn redeemers differing only in bytes nothing reads would both
be admissible. An authenticating mint's vector is not length-constrained by this
rule — the walk above is what consumes and exhausts it.

**Tier selection is the creator's, under the L1 transaction budget.**

| tier        | where the bytes are                                                     | what gates the choice                                                                                                                                 |
| ----------- | ----------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Inline`    | the order transaction's own mint redeemer                               | **no on-chain threshold constant** — the L1 16,384-byte transaction limit is the gate, so the inline/reference split is an off-chain planning concern |
| `RawUtxo`   | one nothing-but-bytes inline datum published in a **prior** transaction | `preimage_len ≤ K`; referenced datums are not part of the order transaction's bytes                                                                   |
| `Certified` | §8.4 chunks plus one §8.6 manifest, published in **prior** transactions | `preimage_len > K`, enforced by the door — §8.4's partition still holds at the top of the ladder                                                      |

Tiers 1 and 2 are indistinguishable to the door below `K`: it hashes the same
bytes against the same commitment whether they arrived in a redeemer or a
referenced datum. What separates them is whose byte budget pays, which is a
property of the consuming transaction and not of the preimage — so a field that
fits a redeemer alone may still have to be published when eight siblings also
want the redeemer. Off-chain planning takes the aggregate reserve
largest-field-first, so a small field never spends budget a large one then cannot
have.

**Custody: the creator's own key, and reclaim is unconditional.** Raw
preimage/chunk UTxOs are published at the **order creator's own wallet address**,
and the §8.6 certificate records the creator's payment key hash as its min-Ada
authority. Reclaim is an **ordinary key spend at any time after the mint**:

- **no reclaim contract** — nothing script-guards the min-Ada;
- **no time gate** — not a maturity window, not the order's inclusion time, not
  the tx-order NFT's lifetime.

That is sound because of the first paragraph: the mint has already read the
bytes, they are in L1 history, and no later reader depends on the UTxO. §8.5's
custody rule (raw carriage at the prover's own key address, min-Ada reclaimed by
ordinary key spend) and §8.7's cleanup rule both apply as written — cleanup is
owner-discretionary, there are no forced-cleanup or time-lock rules, and a yank
is self-healing because republication of identical bytes is interchangeable.

**The mint is parameterized by the §8.6 certificate minting policy id**, which the
door consults on tier 3 only. It is the same deployment role the validation-trace
validators take (§8.6, _Consumption_): one deployed certificate policy serves
both readers.

## 9. Conformance

1. Aiken and TypeScript twins emit byte-identical preimages, compact
   encodings, and hashes for every value in this document's domain; shared
   golden vectors pin every field including the empty (`80`) case, the
   fixed-index boundary values (0, 23, 24, 255, 256, 65,535), the 28-byte
   width assertion, mint policy/asset ordering, and the §6.2 acceptance
   boundaries (2⁶⁴ ± 1 bignums, constructor alternatives 127/128).
   The **frozen wire types** are held to the same standard and by the same
   means: the §8.8 `FieldCarriageV1`/`FieldViewV1` sums, the §8.6
   `FieldPreimageCertificateV1` datum and the §8.6
   `FieldPreimageCertificateMintRedeemerV1` are pinned by shared vectors that
   the off-chain producer encodes and the on-chain decoder both **decodes and
   re-serialises** — decoding alone would miss an encoder emitting a shape the
   decoder tolerates. Every constructor tag is pinned positionally against the
   Aiken declaration order rather than restated, and the 64-byte Plutus Data
   chunking boundary — the one place two Data encoders agree on everything
   short and still diverge — is pinned from **both** sides rather than merely
   crossed: vectors at 63 and 64 bytes that MUST remain a single definite byte
   string, and vectors above it that MUST become an indefinite-length string of
   64-byte definite chunks, so a `>=` written where the rule says `>` is caught
   at the exact width where it is the only difference. Every wire type whose
   shape can reach that width carries a crossing vector.
   `FieldPreimageCertificateV1` is the sole exception and is one structurally,
   not by omission: each of its fields is fixed-width and at most 32 bytes
   (owner 28, tx-id 32, `field_hash` 32 since #606, each digest 32), so no
   value of that type can carry a byte string wide enough to chunk. The
   conclusion survives #606's addition of `field_hash` for the same structural
   reason it held before it — a 32-byte hash cannot reach 64.
2. Decoders are fail-closed everywhere: non-minimal heads (outside the
   pinned fixed-width index), wrapper/length mismatches, count/length
   inconsistency, trailing bytes, non-canonical datum/redeemer payloads,
   and any retired counted-scheme surface all reject. Each of the four §8.6/§8.8
   frozen wire types carries at least one trailing-bytes vector and at least two
   wrong-shape vectors, and each negative vector names the layer that must
   refuse it, so a vector that would be rejected by the CBOR parse anyway
   cannot be counted as coverage of the type cast.

   **One known asymmetry, recorded rather than glossed.** On the consensus side
   this holds outright: `cbor.deserialise` returns `None` unless the payload is
   exactly one CBOR item, so trailing bytes are refused. Off-chain, the
   `Data.from` path in `@lucid-evolution/lucid` is **not** fail-closed on
   trailing bytes — it decodes the leading item and discards the remainder. The
   golden channel pins that behaviour explicitly (the decoded value must
   re-encode to a strictly shorter prefix of the vector) rather than asserting a
   refusal that does not happen. Closing it needs a canonicality guard on the
   off-chain decode path and is not carried by the vector set alone.

3. Negative-vector suites cover the §7 invariants: out-of-range index,
   straddling-item reads, short/empty-slice equality attempts, certificate
   `(tx_id, field_index)` mismatch, certificate **`field_hash` mismatch**
   against the commitment the consumer derived from its own authenticated
   structures — #606's door equality, and since #606 the load-bearing one, so
   a suite that covers the identity pair and not the hash covers the weaker
   half — count/total_length inconsistency, and wrong-field carriage.
4. The §10 walk is proved at its seam, not at its mechanics: interrupt-and-
   resume equals the uninterrupted walk; the checkpoint wire form is
   `field_walk_checkpoint_bytes` long at every position of every field; two
   walks over same-shaped preimages that differ in every byte the §5.1/§5.3
   grammar leaves free — the array header and the wrapper/item heads are
   forced to agree, nothing else is — serialise identically, and the fixture
   pair asserts that per-byte rather than claiming it; and the two cost claims
   (§10.7) are established from runner measurements rather than asserted —
   every number §10.7 quotes is a row of that report, including the controls,
   so a re-take is reading the report and never reconstructing a comparison by
   hand.
5. Every refusal on the §10 walk's operational path — resume, advance,
   relocate, access, and the §10.3 decode — is accounted for in the §10.8
   table, as one of exactly two things:

   - **isolated**, by a negative vector built so that every check the fixture
     reaches before and after the named one is satisfied. Neutralising the
     named check turns that vector red and no other; that is what "isolates"
     means here and it is verified by running the neutralisation, not
     asserted. A vector that can only trip several checks at once is marked
     as a composite and does not count as coverage for any of them.
   - a **backstop**: a refusal that no fixture can isolate, because a check
     that runs earlier already refuses everything it would. A backstop is
     admissible only with the earlier check named, and only in the table —
     "uncovered" is not a category. Backstops are kept rather than deleted
     because the implication that makes them redundant usually lives in
     another module, and they are the line that notices if it stops holding.

   An implementation whose guard set does not partition this way has not met
   this clause.

6. The §11 intra-item mechanisms are held to the same two clauses, one level
   down. Their refusals partition into isolated-or-backstop exactly as item 5
   requires, and §11.5 is that table; their cost claims are established from
   runner measurements exactly as item 4 requires, and §11.4 is that report,
   including the controls that show what each mechanism is an alternative to.
   Two §11-specific conditions join them: a §11.2 implementation MUST keep
   canonicity and materialisability as separate predicates, so that the §6.2
   forms stay canonical while the materialisation path declines them; and a
   §11.1 implementation MUST NOT answer "absent" without both halves of §11.1's
   evidence — the structural pass and the monotone floor — in place.
7. §12's fault statements are held to items 4, 5 and 6 on the same terms:
   §12.6 is their guard table and §12.5 is their measured report. Three
   §12-specific conditions join them. A statement MUST NOT carry preimage
   content, and the implementation MUST make that checkable rather than
   conventional — the wire length is a function of the named unit alone, and
   two statements about one logical fault over content-disjoint preimages
   serialise identically. A per-asset statement MUST name exactly one unit and
   its adjudication MUST read only that unit. And an adjudication MUST re-run
   the per-code shape rule: §12.1's type is deliberately public, so a statement
   may reach it without having passed through the §12.2 decoder.
8. The cross-language **vector and fixture families of the nine-field encoding**
   have producers, and their seeds are not hand-maintained. A "seed" is the input
   a family is derived from — a canonical transaction's bytes, a genuine
   signed-Cardano boundary, a declared construction's parameters — as distinct
   from the derived constants an implementation then asserts. For those families,
   both halves are held:

   - every derived constant, in either language, is emitted by a script that also
     runs in `--check` mode, so a constant that no producer emits is a failure
     rather than a green test; and
   - the seed is itself produced — from a construction stated in parameters, or
     from a search whose implementation is the vector's single source — so that a
     format change is absorbed by re-running producers and never by editing
     constants.

   Concretely, the families in scope are the `n01`–`n09` native-transaction
   vectors, the three native-transaction conformance fixtures (high-cardinality,
   size-balanced, and the ordinary core golden) with the Aiken constants derived
   from each, the maximum-Cardano ordered-collection boundary constants, and the
   field-access, field-item and carriage channels. Their **terminal-fold proof
   structures** are the known exception: those are still mirrored by hand into
   Aiken struct literals that a name-keyed constant rebinder cannot reach, and
   issue #590 tracks bringing them to the same standard. This item states the
   standard, not a claim that every value in the tree already meets it.

   The standard is not a style preference. A hand-maintained seed makes the two
   languages' agreement unfalsifiable in exactly the case that matters: after the
   format moves, the only route to a green suite is a human transcribing bytes
   between them, and a transcription error is indistinguishable from agreement.
   Where a generated constant and a hand-written test module must coexist, the
   generator rebinds named constants in place and never reformats the module
   around them.

## 10. The resumable walk and its checkpoints

Normative for the dispute machine. §7 says what every consumer of the nine
commitments must observe; this section says how a consumer that cannot finish
inside one transaction carries its place to the next one. The reference
implementation is
`onchain/aiken/lib/midgard/native-tx-machine-walk-v1.ak`, and its seam suite
is the same path with `.test.ak`.

### 10.1 What a walk is

A **walk** is a position over one field of one transaction, advanced by
offset-and-slice reads against an authenticated `FieldViewV1` (§8.8). It has
exactly two operations that move it — take the next item, or relocate forward
by `n` items — and one that opens it.

Opening a walk authenticates the field through the single §8.8 door, which
performs the §7.1 hash check, and then **derives** the opening position from
the resulting view: item index 0 at byte offset `header_len`. The opening
position is never a redeemer argument. That is the base case of the
inductive argument in §10.2: every position a walk can be at is either derived
from authenticated bytes or reached from such a position by advances that each
read authenticated bytes.

A walk holds no bytes of its own. Reads go through the view, so tier is an
encoding detail the walk never branches on and lazy chunk verify (§8.4)
continues to apply unchanged underneath it.

### 10.2 The checkpoint

```
FieldWalkCheckpointV1 {
  tx_id: ByteArray,        -- 32 B; the L2 transaction whose field this walks
  field_index: Int,        -- 0..8, the §2.5 positional index
  total_length: Int,       -- the authenticated preimage length
  item_count: Int,         -- the authenticated item count (§5.2)
  next_item_index: Int,    -- items [0, next_item_index) are done
  next_offset: Int,        -- byte offset of item next_item_index's §5.1 wrapper
}
```

Six fields — five scalars and one 32-byte hash — which is why the §10.3 wire
form is `86`, an array of six. There is **no accumulator**: what a rule learns from
a walk is the rule's own business and already has a home in the machine's
committed work state, whereas folding it in here would give every consuming
rule one shared state shape and put rule-specific bytes one refactor away from
a structure §7.6 requires to stay positional.

`field_index` travels with the position because §4's plain hashing removed
field-index domain separation: fields 0/1 and 3/4 alias on identical content,
so the index is the only thing that tells them apart.

`total_length` and `item_count` pin the **shape** of the view the position was
taken against, so a checkpoint cannot be resumed against a differently-shaped
carriage of the same field.

**What a resume verifies.** Resuming re-opens the field through the door — the
follow-on transaction holds different bytes in a different script context and
pays its own single §7.1 hash check, which it has no way not to — and then
binds the checkpoint to the fresh view:

1. `tx_id` equals the resuming transaction's disputed transaction id;
2. the field is named by the **checkpoint**, not by a fresh argument, so a
   resume cannot be pointed at a different slot than the one that was opened;
3. `total_length` and `item_count` equal the fresh view's;
4. `0 ≤ next_item_index ≤ item_count` and
   `header_len ≤ next_offset ≤ total_length`;
5. a completed walk (`next_item_index == item_count`) has
   `next_offset == total_length` — §5.1 leaves no trailing bytes, so a finished
   walk has exactly one admissible offset;
6. for a **fixed-stride** field the position is recomputed:
   `next_offset == header_len + stride · next_item_index`. It is a function of
   the index alone, so a forged offset cannot survive, and the check is O(1);
7. for a **variable-width** field the position costs a full re-walk to
   recompute and is deliberately not recomputed. What is checked in O(1) is
   that the offset lands on a decodable §5.1 item head whose item ends inside
   the authenticated bytes.

Item 7 is the one place a resume cannot verify by arithmetic, and §10.6 is how
the format closes it rather than living with it. A caller that carries
checkpoints through anything weaker than an authenticated thread MUST treat a
variable-width position as prover-asserted.

**A field with no authenticated count cannot be walked.** A variable-width
field under tier-3 carriage has no authenticated item count (§7 invariant 4,
§8.6), so opening a walk over one aborts. It does not fall back to the §5.1
header's self-asserted number, and it does not walk countless.

### 10.3 Checkpoint wire form

A checkpoint's wire form is **exactly 53 bytes, always** — independent of the
field, the carriage tier, the preimage, and the position:

```
86
  58 20 ‖ tx_id(32)
  41    ‖ field_index(1)
  43    ‖ total_length(3, big-endian)
  43    ‖ item_count(3, big-endian)
  43    ‖ next_item_index(3, big-endian)
  43    ‖ next_offset(3, big-endian)
```

Fixed-width scalars, not canonical-minimal ones. §5.3 already establishes that
this format pins a fixed width where a constant size is worth more than
minimality, and here it is worth a great deal: a constant length is what makes
"positions, not bytes" **checkable** rather than merely asserted. Two walks
that reach the same position over different preimages serialise to identical
bytes, which a structure carrying preimage content could not do. Three bytes
hold every in-range value: `total_length ≤ 32,768` (§5.4),
`item_count ≤ 65,535` (§5.1), and both positions are bounded by them.

Decoding is fail-closed and canonical in the §6.1 sense — exactly one
admissible spelling. The decoder re-encodes what it read and requires the
input back, which is simultaneously the canonicity check and the range check
without a second reader of the same grammar to drift from the first.

The thread-carried commitment is

```
checkpoint_hash = blake2b_256("MidgardFieldWalkCheckpointV1" ‖ checkpoint_wire)
```

with the domain string as raw ASCII bytes. It is new surface: none of §4's
prohibited counted-scheme domains is reused.

### 10.4 Advancing

**Take the next item.** Decode the §5.1 wrapper at `next_offset` through the
same head reader every other §5.1 consumer uses (§5.1, §6.1 — one grammar, one
verdict), slice the payload, and advance to `payload_offset + length`. For a
fixed-stride field the wrapper MUST be the one form the stride admits
(§7 invariant 2): payload two bytes in, `stride − 2` bytes long. The advance
fails closed if it would leave the authenticated bytes, and the final advance
MUST land exactly on `total_length` (§5.1, no trailing bytes).

**Relocate by `n` items.** For a fixed-stride field this is one
multiplication — the return on §5.3's fixed 3-byte output index. For a
variable-width field there is nothing to compute from, so it walks. Relocating
is not reading: §7 invariant 2 requires an accessor to decode the item's own
wrapper, and a relocation that skips an item does not access it. The next
read at that position does decode it. `n` MUST be non-negative and MUST NOT
carry the position past `item_count`: on the fixed-stride path a relocation
touches no bytes at all, so neither bound is re-established by anything a
later read would do.

**Budgeted folds.** A step visits at most `budget` items and returns the
position it stopped at. That is the whole of interruptibility: a step takes as
many items as its transaction can afford at the GOAL_SPEC §3.3 budget basis,
commits the returned checkpoint, and the next step resumes from it. `budget` is
a count of items and MUST be non-negative: the recursion stops at zero, so a
negative budget is not "no items" but "no limit".

### 10.5 The fixed-stride shortcut

For fields 0 and 1 an item is located by
`item_offset(i) = header_len + 40·i` and read by one slice: **no walk is
entered and the cost does not grow with `i`** (§5.3). The wrapper is still
decoded and held to the stride, so the O(1) path admits exactly one byte form
for one logical item. Both the item accessor and the count accessor are guarded
on the stride — each on its own, since a view that answers "how many spend
inputs" for a field that has none is as wrong as one that answers "which" —
so a variable-width view cannot be read as inputs by accident. Fields 3/4
(stride 30) and 7 (stride 103) relocate by the same arithmetic.

### 10.6 Threading a walk

A computation thread carries the 32-byte `checkpoint_hash` of the position it
stopped at — one digest whatever the field holds — and a resuming step
re-supplies the 53 positional bytes that hash to it. The thread state is
therefore a fixed-size commitment and the bytes it commits are re-derivable by
anyone from public data.

This is also what closes §10.2's item 7. A step that took a raw checkpoint
from a redeemer would be trusting the prover's arithmetic on a variable-width
position; here the position is pinned by a digest the **previous** step
committed, so the only resumable positions are ones a walk over authenticated
bytes actually reached. Dispute entry points MUST resume through the
commitment, never from a redeemer-supplied checkpoint.

**What the walk core enforces, and what it cannot.** The checkpoint MUST be an
opaque type whose constructor and whose wire-form decoder are both private to
the walk core, and the commitment-taking resume MUST be the only resume it
exports. Those three together mean a caller can obtain a position in exactly
three ways — derived from an authenticated view when the walk is opened,
advanced from such a position, or returned by the commitment-taking resume —
and in particular cannot construct one and hand it to the advance operations,
which is the shape the §10.2 item 7 gap would otherwise take. An implementation
that exports the constructor, the decoder, or the checkpoint-taking resume has
not met this clause: any one of the three restores the ability to present a
variable-width position that no check can catch.

What the walk core cannot enforce is where the `committed` digest comes from.
A dispute entry point that sourced it from a redeemer rather than from thread
state would be back to trusting the prover's arithmetic, and no library can
decide that for its callers. That is why the MUST above is on entry points and
is normative rather than structural.

### 10.7 Cost claims

Two claims this section makes are about cost, and both are established by
measurement against the GOAL*SPEC §3.3 basis of 13,200,000 memory units rather
than asserted. The reference measurements are the seam suite's runner report at
the grammar of this document; they are re-taken whenever the grammar moves, and
every number quoted below is a row of that report, so a re-take is reading four
`authenticate_once*_`rows and two`spend*input_lookup_at*_` rows rather than
reconstructing a control by hand.

1. **A dispute touching a field pays that field's full-preimage hash check at
   most once, however many items it reads.** The controlled comparison is one
   field, one fixture, one set of reads, varying only the number of times the
   door is opened. At 64 items of field 0 the difference between one opening
   and sixty-four is the difference between fitting the budget basis and
   exceeding it: 9.79 M against 13.27 M memory units either side of the
   13,200,000 basis, holding the relocation pattern constant.

   The four rows, in the order a re-take should read them:

   | seam test                                      | opens | relocations | memory  |
   | ---------------------------------------------- | ----- | ----------- | ------- |
   | `authenticate_once_one_open_one_read`          | 1     | 0           | 1.89 M  |
   | `authenticate_once_one_open_every_read`        | 1     | 0           | 8.78 M  |
   | `authenticate_once_one_open_every_relocation`  | 1     | 64          | 9.79 M  |
   | `authenticate_once_reopen_per_item_costs_more` | 64    | 64          | 13.27 M |

   The third row is the control the decisive claim rests on, and it exists so
   that the 9.79 M is a runner measurement like every other number here rather
   than something a maintainer has to reconstruct.

   A re-take must not attribute the whole raw gap to hashing. The per-item-reopen
   control differs from the single-open fold in **two** ways — it re-opens the
   door and it relocates once per item — and the 4.49 M between them (8.78 M
   against 13.27 M) decomposes into roughly 3.48 M of door-opens (row 4 against
   row 3, same relocations) and roughly 1.01 M of access pattern (row 3 against
   row 2, same single open). The conclusion is unaffected, but the margin over
   the basis is 0.57%, so a re-take that models the gap as 63 hash checks will
   mis-predict where the line falls.

2. **Spend-input lookup is an arithmetic slice, not a walk.** Reading item 0
   and item 295 of a 296-item field 0 differ by a residue attributable to the
   surrounding assertion, not to traversal, while the same comparison over a
   variable-width field grows linearly in the index by four orders of
   magnitude more per step. The rows are `spend_input_lookup_at_index_0` and
   `spend_input_lookup_at_index_295`, both 7.82 M.

### 10.8 Guard coverage

§9's conformance item 5 requires every refusal on the walk's operational path
to be either isolated by a vector or listed as a backstop with the earlier
check that makes it unreachable. This is that list, for the reference
implementation. Twenty-three refusals: sixteen isolated, seven backstops.

| #   | refusal                                                     | isolated by / backstop because                                                                                                                            |
| --- | ----------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | §10.2 item 1 — `tx_id` matches the resuming transaction     | `resume_rejects_a_checkpoint_from_another_transaction`                                                                                                    |
| 2   | §10.2 item 3 — `total_length` matches the view              | `resume_rejects_a_forged_total_length`                                                                                                                    |
| 3   | §10.2 item 3 — `item_count` matches the view                | `resume_rejects_a_forged_item_count`                                                                                                                      |
| 4   | §10.2 item 4 — `next_item_index ≥ 0`                        | backstop: §10.3's decoder re-encodes before the binding check runs, and the encoder asserts the same condition                                            |
| 5   | §10.2 item 4 — `next_item_index ≤ item_count`               | backstop: same, the encoder asserts the same condition on the same two fields                                                                             |
| 6   | §10.2 item 4 — `next_offset ≥ header_len`                   | `resume_rejects_an_offset_inside_the_array_header`                                                                                                        |
| 7   | §10.2 item 4 — `next_offset ≤ total_length`                 | backstop: the encoder asserts it against the checkpoint's own `total_length`, which guard 2 pins to the view's                                            |
| 8   | §10.2 item 5 — a completed walk sits at `total_length`      | `resume_rejects_a_walk_that_declares_itself_finished_early`                                                                                               |
| 9   | §10.2 item 6 — fixed-stride offset recompute                | `resume_rejects_a_forged_fixed_stride_offset`                                                                                                             |
| 10  | §10.2 item 7 — the item at the offset ends inside the field | `resume_rejects_a_position_whose_item_runs_past_the_field`                                                                                                |
| 11  | §10.6 — the position hashes to the thread's commitment      | `resume_rejects_a_position_the_thread_did_not_commit`                                                                                                     |
| 12  | §10.3 — the wire form has one admissible spelling           | `checkpoint_decode_refuses_a_non_canonical_spelling`                                                                                                      |
| 13  | §10.4 — a completed checkpoint cannot be stepped            | backstop: every obtainable complete checkpoint sits at `total_length`, where the §5.1 head read refuses first (see §10.6 on why no other can be obtained) |
| 14  | §10.4 — the wrapper declares `stride − 2` bytes             | `walk_next_refuses_a_wrapper_whose_length_misses_the_stride`                                                                                              |
| 15  | §10.4 — the wrapper's payload begins two bytes in           | backstop: §5.1 heads are minimal-width, so the payload offset is a function of the declared length and guard 14 already pins that                         |
| 16  | §10.4 — the advance stays inside the authenticated bytes    | backstop: the §8.8 read that follows refuses every extent outside the bytes                                                                               |
| 17  | §10.4 — the final advance lands exactly on `total_length`   | `walk_next_refuses_a_final_advance_that_misses_the_end`                                                                                                   |
| 18  | §10.4 — a relocation stops at `item_count`                  | `walk_skip_refuses_to_pass_the_item_count_on_a_fixed_stride_field`                                                                                        |
| 19  | §10.4 — a relocation moves forward                          | `walk_skip_refuses_a_negative_relocation`                                                                                                                 |
| 20  | §10.4 — a fold's budget is a count of items                 | `walk_fold_refuses_a_negative_budget`                                                                                                                     |
| 21  | §10.5 — the input accessor is guarded on the stride         | `spend_input_at_refuses_a_variable_width_view_of_input_shaped_items`                                                                                      |
| 22  | §10.5 — an input item is `spend_input_item_bytes` wide      | backstop: given guard 21, §7.4's extent already pins the width to `stride − 2`; it is the line that notices if the two constants stop agreeing            |
| 23  | §10.5 — the count accessor is guarded on the stride         | `spend_input_count_refuses_a_variable_width_view`                                                                                                         |

Four vectors in the suite are composites. They are marked as such in the suite
and are not counted above, because each of them is the _natural_ shape of an
attack and the isolating vector is the contrived one — dropping them would lose
the realistic case:

- `resume_rejects_a_reshaped_view` disagrees with the fresh view on guards 2, 3
  and 9 at once;
- `walk_next_refuses_to_step_past_the_end` is the ordinary shape of guard 13 and
  inherits its redundancy;
- `walk_next_refuses_a_one_byte_wrapper_on_a_fixed_stride_field` trips guard 14
  on its way to guard 15, which is what makes guard 15 unisolable;
- `walk_skip_refuses_to_pass_the_item_count` runs guard 18's bound on the
  variable-width path, where over-relocating walks into guard 13 instead.

The ten range assertions in `encode_field_walk_checkpoint` are the wire form's
construction domain and are not itemised here. They are re-run on every decode —
that is what makes the decoder fail-closed, and guard 12 is what proves the
re-encode load-bearing — and three of them are the reason guards 4, 5 and 7 are
backstops. They are not separately vectored, because each is the same condition
as the guard it makes redundant, checked one step earlier: no fixture can
attribute a refusal to one site rather than the other.

## 11. Intra-item access

Normative for the dispute machine. §7 says what every consumer of the nine
commitments observes; §10 says how a consumer that cannot finish carries its
place to the next transaction. Both stop at the same place: **one item's
bytes**. This section says what a rule may do inside them.

Three item interiors have structure a rule needs to reach into, and each needs
a different mechanism because each is a different shape:

| case | interior            | §5 origin                                | mechanism                               |
| ---- | ------------------- | ---------------------------------------- | --------------------------------------- |
| A    | a multiasset value  | §5.5, inside a field-2 output item       | the **Value bookmark** (§11.1)          |
| B    | a datum or redeemer | §5.5/§5.3, field-2 and field-8 items     | the **Canonical-Data Acceptor** (§11.2) |
| C    | a native script     | §5.3, a field-6 item at `language_tag` 0 | the **checkpointable pushdown** (§11.3) |

The reference implementations are
`onchain/aiken/lib/midgard/native-tx-intra-item-v1.ak` (cases A and B) and
`onchain/aiken/lib/midgard/native-tx-script-pushdown-v1.ak` (case C), with
their seam suites at the same paths plus `.test.ak`. The §6.2 predicate and its
recursive companion live in
`onchain/aiken/lib/midgard/canonical-plutus-data-v1.ak`.

All three interiors are three grammars spelled in one CBOR — a definite head of
a known major type, minimal width, then bytes at an offset — so §6.1's
one-grammar-one-verdict rule applies across them and not merely within each.
The reference implementation reads all three through a single
`onchain/aiken/lib/midgard/intra-item-bytes-v1.ak`; an implementation that gave
each mechanism its own copy of the head reader would have three chances for one
logical number to acquire a second spelling, and §11.5's guards 14–16 would have
to be established three times over.

**What authenticates these bytes.** Nothing in §11 hashes a field. An item
reaches an intra-item mechanism from §10's `walk_next` or from
`field_item_at`, both of which read it through the §8.8 door after the door's
§7.1 hash check. §11 is therefore a pure function of bytes §7 already
authenticated, and it never takes a `FieldViewV1`: re-reading interior bytes
through the view would make every byte pay tier-3's per-read chunk
verification, which is the opposite of what §8.4's guarantee is for.

**Where §7.6 applies, and where it does not.** §7.6 binds _resumable_ state —
what a thread carries between transactions — to positions rather than verbatim
bytes. Case C is resumable and pays §7.6 in full (§11.3). Cases A and B are
not: they live and die inside one transaction, and a bookmark that carries the
item it was opened on is how the format makes it impossible to pair a position
with bytes it did not come from. An implementation that thread-carries a §11.1
bookmark or a §11.2 path has left this document's domain and owes §7.6 an
answer of its own.

**Opacity, wherever a position is state.** §10.6's clause applies unchanged to
the intra-item positions that _are_ state: a caller must not be able to
construct one. Case A's bookmark and case C's cursor are opaque types whose
constructors are private, case C's wire-form decoder is private, and case C's
only exported resume is the commitment-taking one. A public constructor for
either would let a prover place a position at a byte offset of its choosing
_inside_ an item's payload and read from there with nothing checked — which is
the same capability the §10.6 clause withholds, one level down.

It does **not** extend to every offset a §11.2 reader will accept, and this
document does not claim it does. The three typed leaf readers each take a raw
caller-chosen offset, deliberately. They carry no state, they are pure functions
of bytes §7 has already authenticated, and they answer `None` — never a clamped
or partial answer, and never an abort — for an offset that does not begin an
item of the kind asked for. That last word is load-bearing and is not free: the
shared interior head reader is §7.3 abort-never-clamp, so it _aborts_ on a head
§6.1 does not admit, and a leaf reader that read the head first would abort on
`d9 0079 80` rather than decline it. Each of the three therefore decides the item
with §6.2's scan before reading it. There is nothing there to forge because
there is nothing there to resume:
an offset only _means_ something by having come from the path accessor, and a
caller that invents one learns exactly what those bytes say and no more.

### 11.1 The Value bookmark (case A)

A **Value bookmark** is a validated position inside the §5.5 policy-asset map
of one output item. It has one operation — look up the quantity of a
`(policy_id, asset_name)` unit — and that operation is **monotone**: each
lookup must name a unit strictly after the last one looked up, and the position
only ever moves forward.

Monotonicity is the whole mechanism. A per-asset conservation rule (§1's user
story 11) reads `k` units of an `n`-unit value in canonical order; without a
bookmark each lookup restarts the scan and the rule is `O(n·k)`, and with one
the rule is `O(n + k)`.

**What makes a zero answer evidence.** A lookup that returns `0` is claiming
the unit is absent, and two things have to hold for that to be true rather than
merely unobserved:

1. **The whole value is canonically ordered**, established by a structural pass
   when the bookmark is opened. That pass enforces §5.5 in place:
   minimal-width heads, the 28-byte policy width, the 32-byte asset-name cap,
   strictly increasing policy keys and strictly increasing asset keys within a
   group, non-empty policy groups, and strictly positive quantities. Strictness
   is what makes a duplicate key a refusal rather than a silently shadowed
   entry. Without this pass the sweep could pass a unit and then meet it.
2. **The requested unit is ahead of the bookmark**, established by the monotone
   floor. Without it, re-asking for a unit the sweep has already passed would
   sweep forward, find only larger keys, and report `0` — fabricating absence
   evidence for an asset the value holds. A backwards or repeated request
   therefore **aborts**; it does not answer.

Both halves are mandatory and neither is a performance choice. The structural
pass is the analogue, one level down, of §7 invariant 4's count-consistency
check: the container is validated once, and reads against it are then cheap.

The bookmark's byte-level movement is offset-and-slice throughout — no part of
the value is materialised as `Data` — and §7.3's abort-never-clamp applies to
every read.

### 11.2 The Canonical-Data Acceptor (case B)

The acceptor is the recursive companion to §6.2's predicate. §6.2 decides
**canonicity** — is this a byte form `serialiseData` emits. The acceptor adds
the second question §6.2's re-pin created, and the interior access that makes
the answer usable.

**Canonicity and materialisability are different questions.** §6.2 re-pinned
two forms as canonical that the Aiken-stdlib `cbor.deserialise` path cannot
produce `Data` from:

- **tag-2/3 bignums** (`|i| ≥ 2⁶⁴`). The stdlib's major-6 arm computes
  `constr_data(tag − 121, …)` with no tag-range guard, so `c2`/`c3` yield a
  negative alternative. Under PlutusV3 builtin semantics variant E — mainnet
  protocol major version 11, enacted 2026-07-18 — that alternative is a
  `Word64`, so a negative one **fails the machine on real L1**. Asking is an
  abort, not a decline.
- **tag-102 constructors** (alternatives ≥ 128), which the stdlib declines
  outright.

`is_materialisable_plutus_data_v1` is canonicity **and** the absence of either
form at any depth, decided in the same walk. Every materialisation entry point
MUST screen with it before calling `deserialise`; a datum that fails the screen
is **declined**, and declining to materialise is **not** declaring
non-canonical. §6.2 still accepts these bytes and MUST continue to — that
acceptance is the L1 parity the re-pin exists to restore, and an implementation
that made bignums non-canonical to make them convenient would have given it
back.

**The screen is recursive, and that is the point.** The head-byte screen this
replaces read byte zero only, so a bignum nested inside an otherwise ordinary
canonical datum still reached the decoder and still aborted. That residual is
closed here: the flag is set at every tag site the walk passes, so depth is not
a way around it, and the materialisation path is total on canonical input.

**Interior access.** A rule reaches a datum's child by **path**: a list of
indices, each an index into the item at the current position counted in reading
order — a constructor's arguments, a list's elements, and for a map its keys
and values interleaved, so `0` is entry 0's key and `1` its value. One rule for
all three containers means a path never depends on knowing which container it
is walking through. The empty path addresses the datum itself. An index out of
range, a path through a leaf, and a non-canonical datum all yield no answer;
there is no clamping and no partial answer, and the datum's canonicity is
decided before any path step is taken, for the same reason §11.1 validates the
value before serving a lookup.

Three typed leaf readers accompany it, and each reads a form the `Data` route
cannot: the **integer** reader covers majors 0/1 _and_ canonical tag-2/3
bignums, definite or 64-byte chunked; the **byte-string** reader covers
definite and chunked payloads; the **alternative** reader covers all three §6.2
constructor spellings including tag 102. Between them a rule about a datum the
materialisation path declines is still a rule that can be stated.

**What the acceptor is for.** Not speed. §11.4 measures interior access as
_more_ expensive than `cbor.deserialise` on every datum the builtin can take,
because the builtin is a builtin and the acceptor is interpreted. It is for the
datums the builtin cannot take at all, where it is the only route, and its
budget claim is only that it fits — which it does with two orders of magnitude
to spare. Ordinary datum access SHOULD continue to materialise.

### 11.3 The native-script checkpointable pushdown (case C)

A field-6 item at `language_tag` 0 carries a **recursive** script. Two things
follow, and they are the whole of this subsection.

**The recursion is data, not call depth.** The traversal carries an explicit
frame stack, so where it has got to is a value rather than a shape of the
evaluator. A recursive checker cannot stop in the middle and cannot say where
it stopped; a pushdown does both, which is what makes a script traversal
interruptible on the same terms §10.4 gives a field walk.

The three compound node kinds reduce to one **threshold** frame — `all` is "all
of them", `any` is "one of them", `n-of-k` is "n" — so the fold that pops a
frame has one rule rather than three:

```
NativeScriptFrameV1 { kind, remaining, satisfied, required }
```

A step either reads the next node (pushing a frame for a compound node,
resolving a leaf or a childless compound to a verdict) or folds a finished
subtree's verdict into its parent. The verdict is the one
`midgard/native_script_v1` computes recursively over materialised `Data`; that
module remains the definition of what a native script means, and this one is
how a dispute affords it. `after`/`before` and signature semantics are
unchanged.

**A tree position is not one offset.** Resuming needs the pending frames as
well as the byte cursor, and §7.6 forbids carrying them verbatim. They travel
as a **hash chain**:

```
empty_stack_root = blake2b_256("MidgardNativeScriptFrameV1")
stack_root_i     = blake2b_256("MidgardNativeScriptFrameV1" ‖ stack_root_{i-1} ‖ frame_wire_i)
frame_wire       = kind(1) ‖ remaining(3) ‖ satisfied(3) ‖ required(3)
```

with the domain strings as raw ASCII bytes, folded bottom-to-top, and the
scalars fixed-width for the §10.3 reason: two stacks at the same logical
position commit identically whatever scripts produced them.

**Cursor wire form.** Exactly **87 bytes, always** — independent of the script,
its depth and the position:

```
87
  58 20 ‖ script_digest(32)
  58 20 ‖ stack_root(32)
  43    ‖ script_length(3, big-endian)
  43    ‖ offset(3, big-endian)
  43    ‖ stack_depth(3, big-endian)
  43    ‖ nodes_visited(3, big-endian)
  41    ‖ pending(1)
```

The head is `87` because the array has seven elements, and that is a
requirement rather than a formality. On-chain the structure is written and read
by one encoder and one decoder, so a header disagreeing with the element count
would round-trip and the commitment would still be sound; a CBOR reader would
run off the end of the input looking for an element that is not there. §7.6's
carried state is a **wire** form, so an off-chain twin has to be able to decode
it, and a conforming implementation MUST emit bytes a CBOR decoder accepts.

`pending` is `0` (no verdict awaiting a parent), `1` (false) or `2` (true).
Decoding is fail-closed and canonical in the §6.1 sense: the decoder re-encodes
what it read and requires the input back, which is simultaneously the canonicity
check, the range check, **and** the check on the re-supplied frames, because the
stack-root element is computed from them. The thread-carried commitment is

```
cursor_hash = blake2b_256("MidgardNativeScriptWalkV1" ‖ cursor_wire)
```

New surface: neither domain reuses any of §4's prohibited counted-scheme
domains.

**What a resume verifies.** Three independent things, and the traversal is
unsound without any one:

1. the 87 carried bytes hash to the digest the previous step committed, so the
   position is one a traversal actually reached;
2. the re-supplied payload re-digests to the cursor's `script_digest` and has
   its length, so the position belongs to _this_ script;
3. the re-supplied frames re-derive to the committed stack root, so the pending
   thresholds are the ones the traversal built rather than ones chosen to make
   an unsatisfied script pass.

**Authenticate-once, per transaction.** The payload is digested when a walk is
opened or resumed and not again; the budgeted fold checks it once and the steps
under it do not. A traversal taking a thousand steps in one transaction pays one
`blake2b_256` over the script, on the same terms §7.1 gives a field.

**Bounds.** `max_native_script_depth` and `max_native_script_node_count` are
`midgard/native_script_v1`'s, unchanged, and both are enforced during the
traversal rather than assumed: a script cannot be made expensive by being made
big or deep. The node reader additionally holds a compound's **child count** to
`max_native_script_node_count`, which is the node bound one step early rather
than a third bound of this document's own: a compound with more children than
that has more nodes than that, and `check_native_script` answers `None` for it,
so refusing costs no verdict the definition would have given. That
distinction — a bound the definition also has, against one it does not — is what
the two paragraphs below turn on.

**Both bounds are the definition's, in the definition's units, and this is where
a pushdown silently diverges.** `midgard/native_script_v1` measures depth over
**nodes**, counting a leaf as depth 1, so a node read with `d` frames open sits
at depth `d + 1`. A frame is only ever pushed with a child still to read — a
childless compound resolves on the spot, and a partly-consumed frame is
re-pushed only while children remain — so a stack of `d` frames commits to
reading a node at depth `d + 2`. The frame bound is therefore
`max_native_script_depth − 1`, and an implementation that writes
`frames < max_native_script_depth` instead admits scripts one level deeper than
the definition: sixteen nested `all` nodes around one signature is depth 17 in
17 nodes, inside the node bound, refused by `check_native_script` — and a
pushdown with that off-by-one returns a verdict for it. That is a divergence in
the **permissive** direction, which is the one thing §11.3's equivalence claim
must not permit, and a conforming implementation's seam suite MUST carry vectors
at the bound on both sides of it, not merely on shallow scripts.

**A bound the definition does not have is a divergence too, and it costs more
than it saves.** The paragraph above is the permissive direction; `n-of-k`'s
threshold is the other one. A script's bytes carry `n` with no ceiling of their
own, and `check_native_script` answers `Some(valid: False)` for
`at_least(33, [signature])` exactly as it does for `at_least(2, [signature])` —
two nodes at depth two, inside every bound it has. An implementation that
asserted `n ≤ max_native_script_node_count` on the frame **aborts** on those
bytes instead, and an abort is not a verdict: a script the definition calls
invalid becomes one no dispute can resolve, which in a fraud proof is a lost
capability rather than a safety margin.

The threshold is therefore **capped, not bounded**. A frame carries
`min(n, max_native_script_node_count + 1)`, and the cap moves no verdict: `n` is
read exactly once, as `satisfied ≥ n` when the frame pops, and `satisfied`
starts at zero and rises by at most one per child, so it can never exceed the
frame's child count, which is held to `max_native_script_node_count`. Every
threshold at or above the cap is unmet for every reachable `satisfied`, so
mapping them all onto one value is a change of representation and not of
meaning. What it buys is that the frame's wire scalar stays three bytes wide for
any `n` an eight-byte CBOR uint head can spell. A conforming implementation's
seam suite MUST carry equivalence vectors with `n` above the child count and
above the node bound, on both verdicts, and MUST show a capped threshold
surviving a checkpoint — the cap is part of what the hash chain commits, so an
implementation that applied it on one side of a resume and not the other would
fail to resume rather than answer wrongly.

A completed traversal MUST have consumed the payload exactly — a script with
trailing bytes evaluated to nothing, so the verdict refuses rather than
reporting the prefix's answer. `budget` counts steps and MUST be non-negative,
for §10.4's reason: the recursion stops at zero, so a negative budget would mean
"no limit".

### 11.4 Cost claims

Every claim §11 makes about cost is established by measurement against the
GOAL_SPEC §3.3 basis of 13,200,000 memory units, not asserted. The reference
measurements are the two seam suites' runner report at the grammar of this
document; they are re-taken whenever the grammar moves, and every number below
is a row of that report, so a re-take is reading twelve rows rather than
reconstructing a comparison by hand. Each mechanism's row is paired with the
control that shows what a rule would otherwise have had to do, on the same
fixture, for the same answer.

| #   | seam test                                    | what it does                                                                  | memory      |
| --- | -------------------------------------------- | ----------------------------------------------------------------------------- | ----------- |
| 1   | `budget_value_bookmark_sweeps_64_units`      | 64 ordered lookups over a 64-unit value through one bookmark                  | **12.22 M** |
| 2   | `budget_materialised_value_reads_64_units`   | control: the same 64 answers via `decode_canonical_output`                    | **20.15 M** |
| 3   | `budget_value_bookmark_single_unit`          | one lookup into the same value                                                | 7.14 M      |
| 4   | `budget_datum_interior_access`               | a child at depth two of a small datum                                         | 0.93 M      |
| 5   | `budget_datum_materialised_access`           | control: the same child via `cbor.deserialise`                                | 0.18 M      |
| 6   | `budget_wide_datum_interior_access`          | the last leaf of a 24-leaf datum                                              | 4.87 M      |
| 7   | `budget_wide_datum_materialised_access`      | control: the same leaf via `cbor.deserialise`                                 | 1.07 M      |
| 8   | `budget_native_script_pushdown_traversal`    | an eight-node three-level script, whole traversal                             | 1.42 M      |
| 9   | `budget_native_script_recursive_control`     | control: `native_script_v1.check_native_script`                               | 1.26 M      |
| 10  | `budget_native_script_checkpoint_and_resume` | the same traversal interrupted at five steps, committed, resumed and finished | 1.79 M      |
| 11  | `budget_value_bookmark_open_only`            | the bookmark's structural pass over the same value, no lookup after it        | 4.77 M      |
| 12  | `budget_materialised_value_decode_only`      | control: `decode_canonical_output` on the same item, no lookup after it       | 10.87 M     |

Three readings, and the third is the one a re-take must not lose:

1. **Case A is a cost case, and the cost is mostly fixed.** Rows 1 and 2 are the
   same 64 answers over the same fixture: 12.22 M inside the basis against
   20.15 M outside it. Rows 11 and 12 decompose that 7.93 M gap rather than
   leaving it to a story. **6.10 M** of it is what the two paths pay before
   answering anything: `decode_canonical_output` deserialises the output to
   `Data` _and re-encodes the whole thing_ to prove canonicity (10.87 M), where
   the bookmark reads the value in place with offset-and-slice (4.77 M). The
   remaining **1.83 M** is the sweep — 64 lookups cost 7.45 M through the
   bookmark against 9.28 M of searching the materialised asset list, so the
   bookmark is ahead at the margin too, by much less than the headline suggests.

   The two fixed passes are **not** the same proof, and this document does not
   claim they are. `decode_canonical_output` also decides the output's datum and
   its reference script, which `open_value_bookmark` never looks at; what the
   bookmark's pass establishes is §11.1's own precondition — canonical order
   over the whole value — and nothing wider. The comparison is legitimate
   because row 2 is the cheapest way a rule could have obtained the same 64
   answers without §11.1, not because the two passes prove the same things.

2. **Case C's discipline is affordable.** Row 10 against row 8 is what
   interruption costs: one extra cursor encode, one digest, one chain
   re-derivation and one extra payload digest, for 0.37 M. Row 9 shows the
   pushdown is not buying its interruptibility with a worse verdict path
   either — 1.42 M against the recursive checker's 1.26 M is the price of
   carrying the stack explicitly, and it is small.
3. **Case B is a capability case, not a cost case, and the rows say so.** Rows
   5 and 7 are _cheaper_ than rows 4 and 6, at both sizes and by roughly 4x.
   `cbor.deserialise` is a builtin; the acceptor is interpreted Aiken. §11.2's
   justification is that the builtin cannot be asked at all about §6.2's
   re-pinned forms — on a bignum it aborts the machine — and that the acceptor
   fits the basis with two orders of magnitude to spare. Anyone re-taking these
   rows and finding the same ordering has reproduced the design, not found a
   regression.

### 11.5 Guard coverage

§9's conformance item 6 requires every refusal on §11's operational paths to be
either isolated by a vector or listed as a backstop with the check that makes it
unreachable named, on the same terms §10.8 states for the walk. This is that
list. **Thirty-six refusals: twenty-nine isolated, seven backstops.**

| #   | refusal                                                                                | isolated by / backstop because                                                                                                                                                                                                                                                                                                                     |
| --- | -------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | §5.5 — the output map head is `a2`/`a3`/`a4`                                           | `value_bookmark_refuses_a_non_output_map_head`                                                                                                                                                                                                                                                                                                     |
| 2   | §5.5 — key `0`, the address, comes first                                               | `value_bookmark_refuses_a_first_key_that_is_not_the_address`                                                                                                                                                                                                                                                                                       |
| 3   | §5.5 — the address wrapper is the two-byte `58 LL` form                                | `value_bookmark_refuses_a_non_two_byte_address_wrapper`                                                                                                                                                                                                                                                                                            |
| 4   | §5.5 — the address payload is 29 or 57 bytes                                           | `value_bookmark_refuses_a_non_canonical_address_width`                                                                                                                                                                                                                                                                                             |
| 5   | §5.5 — key `1`, the value, follows the address                                         | `value_bookmark_refuses_a_second_key_that_is_not_the_value`                                                                                                                                                                                                                                                                                        |
| 6   | §5.5 — the value is the two-element `[coin, multiasset]`                               | `value_bookmark_refuses_a_value_that_is_not_a_pair`                                                                                                                                                                                                                                                                                                |
| 7   | §5.5 — a policy id is 28 bytes                                                         | `value_bookmark_refuses_a_short_policy_id`                                                                                                                                                                                                                                                                                                         |
| 8   | §5.5 — policy keys strictly increase                                                   | `value_bookmark_refuses_unordered_policies`, `value_bookmark_refuses_a_duplicate_policy`                                                                                                                                                                                                                                                           |
| 9   | §5.5 — a policy group is non-empty                                                     | `value_bookmark_refuses_an_empty_policy_group`                                                                                                                                                                                                                                                                                                     |
| 10  | §5.5 — an asset name is at most 32 bytes                                               | `value_bookmark_refuses_an_oversized_asset_name`                                                                                                                                                                                                                                                                                                   |
| 11  | §5.5 — asset keys strictly increase within a group                                     | `value_bookmark_refuses_unordered_asset_names`                                                                                                                                                                                                                                                                                                     |
| 12  | §5.5 — quantities are strictly positive                                                | `value_bookmark_refuses_a_zero_quantity`                                                                                                                                                                                                                                                                                                           |
| 13  | §11.1 — the structural pass ends inside the item                                       | backstop: every read it makes goes through the shared reader below, which bound-checks before it reads, so a pass that would end past the item has already refused at the read that took it there. Kept, and not only for §9.5's reason: the scan's end offset has no other consumer, so an implementation that deletes this line deletes the pass |
| 14  | §6.1 — an interior head is minimal-width                                               | `value_bookmark_refuses_a_non_minimal_interior_head`, `…_two_byte_head`, `…_four_byte_head`, `…_eight_byte_head` — one vector per wide form, because a bound that only runs at the width the fixtures happen to reach is a bound nobody has checked                                                                                                |
| 15  | §6.1 — an interior head is one of the five definite widths                             | `value_bookmark_refuses_an_indefinite_interior_head`                                                                                                                                                                                                                                                                                               |
| 16  | §6.1 — an interior head's major type is the one the call site names                    | `value_bookmark_refuses_an_interior_head_of_the_wrong_major_type`                                                                                                                                                                                                                                                                                  |
| 17  | §5.5 — a lookup names a 28-byte policy id                                              | `value_quantity_refuses_a_short_policy_id`                                                                                                                                                                                                                                                                                                         |
| 18  | §5.5 — a lookup names an asset name of at most 32 bytes                                | `value_quantity_refuses_an_oversized_asset_name`                                                                                                                                                                                                                                                                                                   |
| 19  | §11.1 — the monotone floor                                                             | `value_bookmark_refuses_a_repeated_lookup`, `value_bookmark_refuses_a_backwards_lookup`                                                                                                                                                                                                                                                            |
| 20  | §6.2 — a byte-string chunk is at most the chunk size                                   | backstop: the stitch is reached only through the §6.2 scan, which has already decided the chunked string and caps a chunk there, so the stitch never meets a longer one                                                                                                                                                                            |
| 21  | §11.3 — a walk is opened over a non-empty payload                                      | `open_refuses_an_empty_payload`                                                                                                                                                                                                                                                                                                                    |
| 22  | §11.3 — a payload is at most 2²⁴ − 1 bytes                                             | backstop: the cursor encoder asserts the same condition on the same field, which is a three-byte wire scalar, and §5.4's field byte bound is orders of magnitude below it — no §5-admissible payload reaches either. It is the line that notices if those two constants stop agreeing                                                              |
| 23  | §11.3 — a node's array header agrees with its tag                                      | `run_refuses_a_node_whose_arity_disagrees_with_its_tag`                                                                                                                                                                                                                                                                                            |
| 24  | §5.3 — a signature node's key hash is 28 bytes                                         | `run_refuses_a_signature_of_the_wrong_key_width`                                                                                                                                                                                                                                                                                                   |
| 25  | §11.3 — the tag is one of the six node kinds                                           | `run_refuses_an_unknown_node_tag`                                                                                                                                                                                                                                                                                                                  |
| 26  | §11.3 — the node bound                                                                 | `run_refuses_more_nodes_than_the_bound_without_a_wide_child`                                                                                                                                                                                                                                                                                       |
| 27  | §11.3 — the depth bound, in the definition's units                                     | `run_refuses_a_script_one_level_past_the_depth_bound`                                                                                                                                                                                                                                                                                              |
| 28  | §11.3 — a fold has a parent to fold into, and `remaining` never goes negative          | backstop: the budgeted fold stops at a complete walk, which is exactly the state with an empty stack and a pending verdict, so the fold is never entered without a parent; `remaining` is positive when a frame is pushed and the frame is popped at zero                                                                                          |
| 29  | §11.3 resume 1 — the cursor hashes to the thread's commitment                          | `resume_rejects_a_position_the_thread_did_not_commit`                                                                                                                                                                                                                                                                                              |
| 30  | §11.3 — the carried bytes are `native_script_cursor_bytes` long                        | backstop: guard 31's re-encode produces exactly that many bytes, so an input of any other length cannot equal it. This check refuses the same inputs a step sooner and keeps the reads under it in range                                                                                                                                           |
| 31  | §11.3 — the wire form has one admissible spelling, which is also the frame-stack check | backstop: the commitment check (guard 29) re-encodes the same walk from the same fields and the same re-supplied frames, so every input this would refuse also hashes to something other than the committed digest                                                                                                                                 |
| 32  | §11.3 resume 2 — the payload re-digests to the cursor's script                         | `resume_rejects_a_same_length_different_script`                                                                                                                                                                                                                                                                                                    |
| 33  | §11.3 resume 2 — the payload's length matches the cursor's                             | backstop: guard 32 pins the payload by digest, and a payload of a different length has a different digest                                                                                                                                                                                                                                          |
| 34  | §11.3 — a fold's payload is the one the walk was opened on                             | `run_rejects_a_payload_the_walk_was_not_opened_on`                                                                                                                                                                                                                                                                                                 |
| 35  | §11.3 — `budget` is a count of steps                                                   | `run_refuses_a_negative_budget`                                                                                                                                                                                                                                                                                                                    |
| 36  | §11.3 — a completed traversal consumed the payload exactly                             | `verdict_refuses_a_script_with_trailing_bytes`                                                                                                                                                                                                                                                                                                     |

Four vectors in the suites are composites. They are marked as such and are not
counted above, because each is the _natural_ shape of an attack and the
isolating vector is the contrived one:

- `resume_rejects_a_substituted_frame_stack` and
  `resume_rejects_a_shortened_frame_stack` trip guards 29 and 31 at once —
  either alone refuses them, which is what makes guard 31 unisolable;
- `resume_rejects_a_different_script` trips guards 32 and 33 at once, because a
  differently-shaped script is also a differently-sized one;
- `run_refuses_more_nodes_than_the_bound` — 33 signatures under one `any` — is
  the ordinary shape of guard 26, and the child-count bound refuses it before
  the running node count can. An earlier revision of this table cited that as
  the reason guard 26 could not be isolated. It was wrong: a script gets big by
  being _bushy_, not only by having one enormous node, and guard 26's vector is
  49 nodes with no child count above sixteen.

Every "isolated by" row above is verified by neutralising the named check and
confirming that exactly the listed vectors turn red — run, not asserted; every
"backstop" row is a neutralisation that turned **nothing** red, which is what
put it in that column rather than an argument that it should be there. That
distinction is not pedantic: the entry this table previously carried for guard
26 was an _argued_ backstop, and arguing is how it came to be false.

Two families of assertion are not itemised, for §10.8's reason — they are the
construction domain of a value some other guard already fixes, and no fixture
can attribute a refusal to one site rather than the other. Neutralising each of
them turns nothing red. §11's three modules — the shared interior reader and the
two mechanism modules — carry **71** assertions in all; the thirty-six rows above
account for **41** of them, and these two families are the remaining **30**.

- The range assertions in the frame and cursor encoders, which are re-run on
  every decode, together with the compound reader's `child_count` bounds and its
  `required ≥ 0`, one step earlier on the value that becomes a frame. Each names
  something another guard already fixes: `kind` by guard 25's tag domain;
  `remaining` and `satisfied` by the child-count bound, since a frame is pushed
  with `remaining = child_count` and `satisfied` starts at zero and rises once
  per child; the cursor's scalars by the traversal that produced them and by
  guards 21–22 on the two that have their own rows. The frame's `required` bound
  is fixed **by the cap** rather than by an earlier assertion of the same
  condition, which is the one place this family's argument differs: §11.3 caps
  `n` at `max_native_script_node_count + 1` instead of refusing it, so the
  encoder's `required` range is met by construction on every frame the traversal
  builds, and a resume handed a frame outside it fails the stack-root
  re-derivation. That distinction matters because the same line written as a
  _bound_ on `n` would not be in this family at all — it would be an
  unaccounted-for refusal, and a §11.3-conforming one does not exist.
- The offset and length bounds in the shared interior reader's byte and slice
  primitives (§7.3's abort-never-clamp), which run on every read all three
  mechanisms make.

Both families are kept rather than deleted for the reason §7.3 gives: the
failure that replaces a stated bound is a machine error with nothing
attributable said — a three-byte wire scalar handed a wider number fails inside
the integer-to-bytes builtin, and a fraud proof cannot cite that.

## 12. Fault statements

Normative for the dispute machine. §10 says how a consumer reaches one item's
bytes and carries its place between transactions; §11 says what a rule may do
inside them. This section says what a challenger **claims**, and it exists
because the two layers below it make a much smaller claim possible than the
counted scheme did.

The reference implementation is
`onchain/aiken/lib/midgard/native-tx-fault-statement-v1.ak`, and its seam suite
is the same path with `.test.ak`.

### 12.1 What a statement is

A **fault statement** names one wrong thing about one transaction. It carries:

- `tx_id`, `field_index` and `item_index` — the **address** of the fault, for
  the fault kind that has one;
- for a per-asset fault, exactly one `(policy_id, asset_name)` unit, which is
  that kind's whole address; and
- `claimed`, the quantity the operator committed, for the arithmetic to
  disagree with.

It carries **no preimage bytes**, and that is the whole of witness minimality
here. The fraudulent item's bytes reach the adjudication from the authenticated
`FieldViewV1` — through §10's walk, which read them from bytes the §8.8 door
had already hashed against the positionally-extracted field commitment (§7.1).
A step therefore pays the authenticate-once check for the fields it touches and
**nothing beyond it**: there is no re-supply of the field, of the item, or of
any digest over either.

The property is structural rather than conventional, and it is checkable. A
statement's wire length is `fault_statement_frame_bytes` plus the two name
encodings and nothing else, so it is a function of the **named unit** alone;
the `fault_item_predicate` form names no unit and is therefore a constant 55
bytes whatever the field holds. And a single statement's wire bytes adjudicate
the same logical fault in **two transactions that share no item at any index**,
proving against both — which a witness carrying the fraudulent item could not
do, since the item it carried would be wrong for one of the two. The seam suite
runs exactly that, from one encoding decoded twice, rather than claiming it.

**A statement is an accusation, not state, and its type is public.** §10.6
withholds `FieldWalkCheckpointV1`'s constructor because a caller that could
write a position would be reading authenticated bytes at an offset nobody
checked. A statement is the opposite: an accusation anybody may write is what a
permissionless dispute game needs, and nothing here is believed because it was
stated. The §12.3/§12.4 adjudications hold a statement against authenticated
bytes, and a false accusation simply fails to prove. An implementation that made
this type opaque would restrict who can accuse without making any accusation
safer.

**One statement, one accusation.** The per-code shape rule is normative: a
`fault_item_predicate` statement MUST carry no unit and no claimed quantity, and
a `fault_asset_conservation` statement MUST carry a full 28-byte policy id and
an asset name of at most 32 bytes. A statement carrying both a bad item and a
unit would be two accusations in one witness, and a refusal could not be
attributed to either.

**Every scalar a code does not use has exactly one spelling**, and this is
normative for the same reason, one turn further. §12.4 reads fields 2 and 5 from
item 0 of each and consults the statement's own `field_index` and `item_index`
for nothing, so a `fault_asset_conservation` statement MUST carry
`field_index = 2` and `item_index = 0`. Left free they would be bytes that enter
`statement_hash` without entering any verdict: two statements differing only
there would be two distinct thread commitments that adjudicate identically off
the same evidence, which is a statement that does not say what it commits to.
The rule is enforced by the encoder **and** re-run at adjudication, because a
statement is a plain type a challenger may write by hand.

### 12.2 Statement wire form

```
87
  58 20 ‖ tx_id(32)
  41    ‖ code(1)
  41    ‖ field_index(1)
  43    ‖ item_index(3, big-endian)
  bytes(policy_id)      -- minimal-width §5.1 head; `40` when absent
  bytes(asset_name)     -- minimal-width §5.1 head; `40` when absent
  49    ‖ sign(1) ‖ magnitude(8, big-endian)
```

Seven elements, so the head is `87` — a requirement rather than a formality, for
§11.3's reason: §7.6's carried state is a **wire** form, and an off-chain twin
has to be able to decode it.

`claimed` is signed where every quantity in this format is not, so it travels as
a sign byte (`00`/`01`) and an eight-byte magnitude rather than as a CBOR `int`:
two's complement would spend a ninth byte to say the same thing, and a CBOR
`int` would be variable-width, which is the one property this form exists to
deny. Decoding is fail-closed and canonical in the §6.1 sense — the decoder
re-encodes what it read and requires the input back, which is simultaneously the
canonicity check, the range check **and** the per-code shape check.

The thread-carried commitment is

```
statement_hash = blake2b_256("MidgardNativeTxFaultStatementV1" ‖ statement_wire)
```

with the domain string as raw ASCII bytes. New surface: it reuses none of §4's
prohibited counted-scheme domains and neither §10.3's nor §11.3's.

### 12.3 Proving a single bad item

`prove_item_fault` adjudicates a `fault_item_predicate` statement: is the item
at `item_index` of `field_index` one the caller's predicate refuses?

**Exactly one item is read.** The walk relocates to the named index — one
multiplication on a fixed-stride field (§10.5), a walk on a variable-width one —
and one `walk_next` reads that item. The returned checkpoint sits one past the
accused index, so "exactly one item was consumed" is observable at the seam
rather than argued, and an adjudication that runs out of budget composes with
§10.4's fold instead of starting over.

Three conditions are normative and none is redundant:

1. **The statement's transaction is the walk's.** The checkpoint reports the
   tx id the §8.8 door authenticated against, and it MUST equal the statement's.
   Without this check a statement is a claim about _some_ transaction: the same
   bytes would prove against any transaction whose field happened to hold a
   refused item at the named index, and the accusation would name no one.
2. **The statement's field is the walk's.** §4's plain hashing removed
   field-index domain separation, so a view of field 1 answers every read a view
   of field 0 would. Without this check a statement about a spend input could be
   adjudicated against the reference inputs.
3. **Relocation is forward-only**, inherited from §10.4. An adjudication that
   has already passed the accused item cannot go back for it, which is what stops
   a statement from being re-adjudicated at a position the walk has left.

The predicate itself is the **caller's**. This section adjudicates one; it does
not define the protocol's per-item rules, which live with the families that own
them.

### 12.4 Per-asset conservation

A conservation statement names exactly one unit, and every read its
adjudication makes names that unit.

- **The outputs side** folds the next `budget` field-2 items into a running
  total, opening each as a §11.1 Value bookmark and asking it for the named unit
  once. No output is materialised and no other asset is touched. The fold is
  budgeted like every other §10 fold, so a caller may take the field in as many
  rounds as it likes and no round re-reads another's items.
- **The mint side** sweeps field 5's per-policy items (§5.6) monotonically,
  stopping at the first policy key that sorts at or after the requested one, and
  reads into that one group only. Every other group is skipped by the §5.1
  envelope without its assets being decoded at all. It is budgeted on the outputs
  fold's terms, and it carries the last policy key it read across **budget
  rounds** so that the order check spans the boundary between two of them.

**Both sides begin at item 0, and this is normative.** A sweep is an assertion
about a _field_, not about a range of it: the mint side's `0` means "this
transaction does not mint this unit", and the outputs side's total means "this is
what these outputs hold". A sweep opened part-way through a field asserts neither.
The attack is concrete and cheap, because §10.4's forward relocation is public
and free: stand the walk past the policy group that carries the accused unit,
open a mint sweep there, and the very next key sorts after the request — the
monotone early stop fires, the sweep reports a **finished** `0` for a unit the
transaction really minted, and every other condition below is satisfied. An
implementation MUST refuse to open a sweep at any position other than item 0 of
its field.

**A conservation adjudication is one invocation's work**, and this is the
contract rather than an implementation limit. The two sweeps deliberately have no
wire form, no decoder and no commitment-resume constructor — the exact opposite
of §10.3's checkpoint — because a sweep is a running _measurement_ and not a
position, and the §10.6 apparatus that makes a position safe to carry would have
to be repeated over state that §12.5's own measurement (reading 2) says a
variable-width field near the tier-2 bound cannot afford to re-authenticate per
step in any case. Fields 2 and 5 are both variable-width. What follows, and is
stated rather than left to be discovered: **re-opening a sweep in a follow-on
transaction is a fresh whole-field measurement, never a resume of a partial
one** — the item-0 rule above makes it so — and a transaction whose fields 2 and
5 cannot both be swept within one invocation's budget is outside this
adjudication's reach. A family that needs conservation over such a transaction
carries the residue itself; §12 does not pretend to.

**The two sides are two types.** An implementation MUST NOT let one measurement
stand for the other: the outputs fold's value is not a mint sweep's, they answer
about different fields, and only one of them carries an order key. In the
reference implementation `OutputUnitSweepV1` and `MintUnitSweepV1` are separate
opaque types, which turns three of this section's conditions — an outputs fold
cannot be advanced by the mint sweep, a mint sweep cannot be advanced by the
outputs fold, and the adjudication's two arguments cannot be exchanged — from
runtime refusals into signatures.

The transaction's own committed fields account for a net creation of the unit
equal to `outputs_total − mint_total`, and the statement is **proven** when that
disagrees with `claimed`.

**What this equation deliberately excludes.** The resolved-input half of a full
conservation law is not in this document's domain — it lives in the machine's
ledger state, against UTxOs this transaction only references. It is therefore
not admitted here as a redeemer argument, because that would be exactly the
full-field re-supply §12.1 forbids. A family that needs the full law composes
this statement with its own authenticated input accounting.

**A zero answer is evidence, on both sides, and for §11.1's reasons.** Absence is
only observable if the container is ordered, and both sweeps enforce the order
**as they go** rather than assuming it: §5.5's, inside the Value bookmark's
structural pass, and §5.6's, across the mint field's policy items and within the
matched group. The mint field's order check spans **budget rounds** — the last
policy key travels in the sweep rather than in a call argument, so a round
compares its first key against the previous round's last, and an unordered field
cannot slip a key past a sweep that paused. Two further §5.6 conditions are
load-bearing for the same reason
and are enforced here rather than left to the encoder: a policy group is
non-empty, and a mint quantity is non-zero. A zero-quantity entry would be a
second spelling of an absent asset — which is precisely the shape a
conservation fault would hide behind.

The order check reaches exactly the prefix the sweep reads, which is what
licenses stopping early and is all that licenses it: no key at or after the
stopping point is examined, so a §5.6 violation _there_ — the requested policy
repeated after the matched group — is out of the sweep's reach. Curing that
inside the sweep means reading every remaining item, which is the early stop the
monotone design exists for.

**Name the residue rather than a mitigation for it.** A field that repeats the
accused policy after the matched group would have its second entry's quantity
dropped, which understates `mint_total` and can make an honest `claimed`
disagree. This section does not establish the whole field's order and MUST NOT be
read as doing so. What it relies on is that such a field is a **§5.6 format
fault, not a conservation fault**: the ordering rule is the encoder's (§5.6), it
is checked whole where a transaction is canonically decoded before admission —
adjacent machinery in §8.9's sense, outside this document — and a field that
reached a commitment without satisfying it is faultable by the family that proves
format faults, not by this one. A §12.4 verdict is therefore sound _relative to a
field that satisfies §5.6_, and this sweep re-checks for free the part of that
premise it can reach. §11.1's `sweep_to_unit` carries the identical residue one
level down, and neither is discharged by the other.

**Budget exhaustion is not absence.** Each side's measurement carries whether it
_finished_, and a sweep that stopped because it ran out of budget carries
`is_final = false`. This is the distinction the section turns on: a mint sweep
that has not yet reached the requested policy holds a running `0`, and read as an
answer it says the transaction minted none of a unit it may well have minted —
which makes `outputs_total − mint_total` disagree with an honest `claimed` and
"proves" a fault that does not exist. Three states end a mint sweep and each is a
verdict: the unit's group was found, a key sorting at or after it was met, or the
field ran out. Running out of budget is none of them. The outputs fold finishes
exactly when its walk does, since every output holds a value and there is no
sound early stop on that side.

**The two totals are evidence, not arithmetic.** They are not integers the
adjudication accepts from a caller. Each arrives as the sweep that produced it,
carrying the transaction it was taken over, the field, the unit, the quantity and
`is_final`; and the adjudication MUST refuse a statement unless both sides were
measured over the accused transaction, over fields 2 and 5 respectively, from
item 0 of each, about the statement's own `(policy_id, asset_name)`, and to a
verdict. Accepting two bare integers would readmit through the back door exactly
the full-field re-supply §12.1 forbids: a caller could assert any pair of totals
it liked, including the fabricated `0` of an unfinished sweep. Two of those
conditions need not be checks at all: "over fields 2 and 5 respectively" is
discharged by the two types above, and "from item 0" by the constructor that is
the only way to obtain either of them.

### 12.5 Cost claims — the tier-2 per-step full-preimage re-hash

This is the **Phase-3 lane exit criterion**, and like every other number in this
document it is established by measurement against the GOAL_SPEC §3.3 basis of
13,200,000 memory units rather than asserted. It is
**provisional pending Phase-7 confirmation**: Phase 7 re-takes it against the
final blueprint, and the rows below are the Phase-3 signal.

§10.2 makes a resuming transaction pay its own §7.1 hash check — "it holds
different bytes in a different script context and has no way not to." The
question this criterion answers is what that costs at the tier-2 bound, where it
is largest. The rows are four, in two pairs; each pair holds the fixture, the
field and the reads constant and varies **only** how many times the §8.8 door is
opened, so the difference within a pair is one step's re-open and nothing else.
Both fixtures sit at `chunk_bytes_k` (§8.3) rather than merely being large, and
the suite asserts that so the numbers stay quoted where they were taken.

| #   | seam test                        | field                     | preimage | opens | memory               |
| --- | -------------------------------- | ------------------------- | -------- | ----- | -------------------- |
| 1   | `tier2_fixed_stride_one_open`    | 1 (stride 40, 378 items)  | 15,123 B | 1     | 10,011,478 (10.01 M) |
| 2   | `tier2_fixed_stride_two_opens`   | 1 (stride 40, 378 items)  | 15,123 B | 2     | 10,105,299 (10.11 M) |
| 3   | `tier2_variable_width_one_open`  | 6 (variable, 1,372 items) | 15,095 B | 1     | 58,753,191 (58.75 M) |
| 4   | `tier2_variable_width_two_opens` | 6 (variable, 1,372 items) | 15,095 B | 2     | 86,638,113 (86.64 M) |

**Both fixtures moved with §8.3 erratum E1's repair of `K`** (15,900 → 15,148),
because "at the tier-2 bound" is a statement about `K`: the largest fixed-stride
field-1 preimage that still fits is 378 items rather than 397, and the largest
whole number of `script_item`'s seven-wide cycle is 1,372 items rather than 1,442.
All four rows were re-taken at the new fixtures in the same commit as the re-pin;
`tier2_fixtures_sit_at_the_tier_two_bound` asserts that one stride more would
leave the bound, so "largest" is checked rather than asserted.

Absolute units, not only the rounded figures, because the readings below are
**differences** and a difference of rounded numbers is not a measurement. Every
row is the `execution_units.mem` field of the structured `aiken check` report
for that test, taken by

```
MIDGARD_AIKEN_BIN=<fork> node scripts/run-focused-check.mjs \
  midgard/native-tx-fault-statement-v1.test \
  tier2_fixtures_sit_at_the_tier_two_bound \
  tier2_fixed_stride_one_open tier2_fixed_stride_two_opens \
  tier2_variable_width_one_open tier2_variable_width_two_opens
```

from `onchain/aiken/`. That helper is the repository's runner-report path: it
requires the report to name exactly the declared module and to collect exactly
the declared tests, so a selector that silently matched nothing cannot be
published as a measurement.

Three readings, and the third is the one a re-take must not lose.

1. **The re-hash proper is free at the tier-2 bound, and now measurably so.**
   Row 2 against row 1 is **93,821 units (≈ 0.09 M)** — one tier-2 carriage
   extraction, one `blake2b_256` over 15,123 bytes, and §7 invariant 4's
   arithmetic count check. That is **0.71 % of the basis**, which is what §8.2's
   "measured free at ≤ 32 KB" had been asserting and had never been shown. A
   dispute over a fixed-stride field may therefore be spread over as many steps
   as its item budget needs without the re-authentication becoming the
   constraint.
2. **A variable-width field's step re-pays much more than the hash.** Row 4
   against row 3 is **27,884,922 units (≈ 27.88 M)** — 2.11× the basis on its
   own. The hash is the
   same 0.09 M; the remaining 27,791,101 (≈ 27.79 M) is §7 invariant 4's
   **full-content walk**,
   which `whole_view` must run at construction for a variable-width field because
   that walk is the only way to know where its items end. At 1,372 items that is
   ≈ 20,256 units per item, and it is re-paid in full by every step. (The
   per-item figure is what survives E1's re-pin unchanged — it was ≈ 20,255 at
   1,442 items — which is the evidence that the walk is linear in items and that
   the row moved because the fixture did, not because the door did.)

   The consequence is a design conclusion, not a caveat: **a variable-width field
   near the tier-2 bound cannot be resumed across steps under tier-2 carriage.**
   Either the field is small enough that its walk fits the step's budget, or it
   is carried under tier 3 — where `certified_view` deliberately does not run the
   walk (§8.4) and pays per-read chunk verification instead. §8's
   simplest-fitting-first mandate therefore has a second, measured edge to it:
   for variable-width fields the ladder's rungs differ in what a _step_ costs, not
   only in what a publication costs. §12.4 is where this conclusion is spent:
   fields 2 and 5 are both variable-width, which is why a conservation
   adjudication is one invocation's work and its sweeps have no wire form.

3. **Rows 3 and 4 are not budget verdicts, and a re-take must not read them as
   such.** Both exceed the basis outright, because building a 1,372-item fixture
   inside a test dominates both arms. Only the **difference** within a pair is
   attributable to the door, which is why the rows come in pairs at all and why
   neither pair's absolute figure is quoted as a fit. The same caution applies to
   rows 1 and 2, whose 10.0 M is likewise mostly fixture.

Phase 7 re-takes all four rows against the final blueprint. Should the
variable-width delta move materially, the conclusion in reading 2 — not merely
the number — is what has to be re-examined.

### 12.6 Guard coverage

§9's conformance items 5 and 6 require every refusal on an operational path to
be either isolated by a vector or listed as a backstop with the earlier check
that makes it unreachable. This is that list for §12. **Thirty-eight refusals:
twenty-one isolated, three composite, fourteen backstops.** Three further
conditions are not refusals at all — the two sweep types discharge them at
compile time. One of the three held a row here and keeps its number, marked; the
other two are named in the note below the table.

| #   | refusal                                                                                               | isolated by / backstop because                                                                                                                                                                                                                                                                                                     |
| --- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | §12.1 — an item-fault statement names no unit                                                         | `encode_refuses_an_item_fault_that_names_a_unit`                                                                                                                                                                                                                                                                                   |
| 2   | §12.1 — an item-fault statement claims no quantity                                                    | `encode_refuses_an_item_fault_that_claims_a_quantity`                                                                                                                                                                                                                                                                              |
| 3   | §12.1 — a conservation statement names a 28-byte policy id                                            | `encode_refuses_a_short_policy_id`                                                                                                                                                                                                                                                                                                 |
| 4   | §12.2 — the wire form has one admissible spelling (the decoder re-encodes and demands the input back) | `decode_refuses_an_unvalidated_wrapper_byte`                                                                                                                                                                                                                                                                                       |
| 5   | §12.3 — the statement's field is the walk's                                                           | `item_fault_refuses_a_statement_about_another_field`                                                                                                                                                                                                                                                                               |
| 6   | §12.3 — the code is an item-fault code                                                                | `item_fault_refuses_a_conservation_statement`                                                                                                                                                                                                                                                                                      |
| 7   | §12.3 — the statement is well-shaped at adjudication                                                  | `item_fault_refuses_a_malformed_statement`                                                                                                                                                                                                                                                                                         |
| 8   | §12.4 — an outputs sweep opens only on a field-2 walk                                                 | `conservation_refuses_a_walk_over_another_field` (composite: the guard is the first refusal, but §11.1's structural pass also declines a field-6 item, so the vector cannot attribute to one site)                                                                                                                                 |
| 9   | §12.4 — a mint sweep opens only on a field-5 walk                                                     | `mint_unit_quantity_refuses_a_walk_over_another_field` (composite with guard 23, on the same terms)                                                                                                                                                                                                                                |
| 10  | §12.4 — the code is a conservation code                                                               | `conservation_refuses_an_item_statement`                                                                                                                                                                                                                                                                                           |
| 11  | §12.4 — the statement is well-shaped at adjudication                                                  | `conservation_refuses_a_malformed_statement`                                                                                                                                                                                                                                                                                       |
| 12  | §5.6 — mint policy keys strictly increase                                                             | `mint_unit_quantity_refuses_an_unordered_field`                                                                                                                                                                                                                                                                                    |
| 13  | §5.6 — a mint quantity is non-zero                                                                    | `mint_unit_quantity_refuses_a_zero_quantity`                                                                                                                                                                                                                                                                                       |
| 14  | §10.4 — relocation is forward-only                                                                    | `item_fault_refuses_a_backwards_statement` (composite with §10.8 guard 19, which it re-uses rather than re-establishes)                                                                                                                                                                                                            |
| 15  | §12.2 — the quantity element's `49` wrapper                                                           | backstop: guard 4's re-encode produces that byte, so an input carrying any other cannot equal it                                                                                                                                                                                                                                   |
| 16  | §12.2 — the sign byte is `00` or `01`                                                                 | backstop **confirmed by neutralisation**: removing it turns nothing red, because guard 4's re-encode spells the sign from the decoded value's own sign. `decode_refuses_a_non_canonical_spelling` is the composite vector over the pair                                                                                            |
| 17  | §12.2 — `claimed`'s magnitude fits eight bytes                                                        | backstop: the wire scalar's construction domain, kept for §7.3's reason — a wider number fails inside the integer-to-bytes builtin with nothing attributable said                                                                                                                                                                  |
| 18  | §12.2 — a name encoding is at most 32 bytes                                                           | backstop: guards 1–3's shape rule already fixes both names, one step earlier and on the same values                                                                                                                                                                                                                                |
| 19  | §12.4 — the outputs fold's unit is a 28-byte policy id                                                | backstop: §11.1's own lookup asserts the same condition on the same value (§11.5 guard 17)                                                                                                                                                                                                                                         |
| 20  | §12.4 — the outputs fold's asset name is at most 32 bytes                                             | backstop: same, §11.5 guard 18                                                                                                                                                                                                                                                                                                     |
| 21  | §12.4 — the mint sweep's unit is a 28-byte policy id                                                  | backstop: the §5.6 item's own policy width (guard 23) refuses every group the request could match, so a malformed request answers absent rather than wrongly                                                                                                                                                                       |
| 22  | §12.4 — the mint sweep's asset name is at most 32 bytes                                               | backstop: the group scan's own name bound (guard 25) on the values it compares against                                                                                                                                                                                                                                             |
| 23  | §5.6 — a mint item is `82 ‖ 58 1C policy_id ‖ …`                                                      | backstop: the item reached the sweep through §10's `walk_next`, and a field-5 item that is not this shape has no §5.6 spelling at all; kept as the line that notices if §5.6 and this reader stop agreeing                                                                                                                         |
| 24  | §5.6 — a policy group is non-empty                                                                    | backstop: §5.5/§5.6's encoders give an empty group no spelling, and §11.5 guard 9 isolates the same condition one level down                                                                                                                                                                                                       |
| 25  | §5.6 — a mint asset name is at most 32 bytes                                                          | backstop: §11.5 guard 10 isolates the same condition on the same grammar                                                                                                                                                                                                                                                           |
| 26  | §5.6 — asset keys strictly increase within a group                                                    | backstop: §11.5 guard 11 isolates the same condition on the same grammar                                                                                                                                                                                                                                                           |
| 27  | §12.4 — a fold's budget is a count of items                                                           | backstop: §10.8 guard 20 isolates the same condition on `walk_fold`, and these folds stop on the same zero                                                                                                                                                                                                                         |
| 28  | §12.4 — a mint sweep's budget is a count of items                                                     | backstop: same                                                                                                                                                                                                                                                                                                                     |
| 29  | §12.3 — the statement's transaction is the walk's                                                     | `item_fault_refuses_a_statement_about_another_transaction`                                                                                                                                                                                                                                                                         |
| 30  | §12.4 — both sweeps were taken over the accused transaction                                           | `conservation_refuses_sweeps_of_another_transaction`                                                                                                                                                                                                                                                                               |
| 31  | §12.4 — both sweeps are about the statement's own unit                                                | `conservation_refuses_a_sweep_about_another_unit`                                                                                                                                                                                                                                                                                  |
| 32  | §12.4 — the outputs argument is an outputs sweep and the mint argument a mint sweep                   | **not a refusal**: `asset_conservation_fault_is_proven` takes an `OutputUnitSweepV1` and a `MintUnitSweepV1`, so a transposed call does not compile. `conservation_refuses_the_two_sides_transposed` was this row's vector and has been deleted — a `fail` test cannot be written against a type error                             |
| 33  | §12.4 — the adjudication refuses an unfinished mint sweep                                             | `conservation_refuses_a_budget_exhausted_mint_sweep`                                                                                                                                                                                                                                                                               |
| 34  | §12.4 — the adjudication refuses an unfinished outputs fold                                           | `conservation_refuses_a_budget_exhausted_outputs_fold`                                                                                                                                                                                                                                                                             |
| 35  | §12.4 — an unfinished sweep will not report a quantity                                                | `conservation_refuses_the_running_total_of_an_unfinished_fold` and `mint_unit_quantity_refuses_a_budget_exhausted_sweep`, on the two sides                                                                                                                                                                                         |
| 36  | §5.6 — mint policy keys strictly increase **across budget rounds**                                    | `mint_unit_quantity_refuses_an_unordered_field_across_budget_rounds` (guard 12 is the single-round vector; this is the one an order key scoped to a single `sweep_mint_unit` call would not catch. Renamed from `…_across_steps`: a sweep does not cross a step, and the old name claimed a protocol this section does not have)   |
| 37  | §12.4 — a sweep begins at item 0 of its field                                                         | `conservation_refuses_a_fold_opened_past_the_start` and `mint_unit_quantity_refuses_a_sweep_opened_past_the_target`, on the two sides. The mint vector is the sharp one: opened one `walk_skip` past the accused policy's group, a neutralised sweep returns `is_final` with a fabricated `0` that guards 30, 31 and 33 all accept |
| 38  | §12.1 — a conservation statement's `field_index` has one spelling                                     | `encode_refuses_a_conservation_statement_that_names_another_field` and `conservation_refuses_a_statement_that_names_another_field`, at the encoder and at the adjudication                                                                                                                                                         |
| 39  | §12.1 — a conservation statement's `item_index` has one spelling                                      | `encode_refuses_a_conservation_statement_that_names_a_starting_item` and `conservation_refuses_a_statement_that_names_a_starting_item`, on the same two seams                                                                                                                                                                      |

Guards 29–36 were added by the first review of this section and guards 37–39 by
the second. Each was confirmed by neutralisation — removing the check turns the
named vector or vectors red and nothing else — which is the standard §9 item 6
asks for and the reason none of them is listed as a backstop.

**Two conditions this table used to count as refusals are now signatures.**
`accumulate_output_unit` takes an `OutputUnitSweepV1`, so a mint sweep driven as
outputs does not compile, and `conservation_refuses_a_mint_sweep_driven_as_outputs`
has been deleted alongside guard 32's vector; symmetrically, `sweep_mint_unit`
takes a `MintUnitSweepV1`, so an outputs fold driven as mint does not compile
either — that condition never had a vector, because no fixture could reach it
past §11.1's structural pass. The two deleted vectors were previously composites —
§11.1's Value bookmark declines a §5.6 policy item one step later, so no fixture
could attribute either refusal to its own site — and a condition a fixture cannot
isolate but a type can hold is better held by the type. What that costs is the
"line that notices if the two sides stop being distinguishable at the seam"; what
replaces it is that they cannot stop, because they are no longer one type.

**What the first neutralisation sweep established.** Every refusal this module
implemented at the time — guards 1–13 and 16 — was weakened one at a time, the
module's vectors were re-run under the fork runner after each weakening, and the
vectors that turned red were recorded. The sweep collected 33 tests on all
fifteen runs — the module's whole count then; it now has 48 — so no result is a
zero-collection artifact. Its findings, all three of which changed this table:

- Guards 1–7 and 10–13 each turned **exactly** their own named vector red and
  nothing else. Those eleven rows are isolated in the strong sense.
- Guard 16 turned **nothing** red, which is what a backstop looks like from the
  outside and is now recorded as measured rather than argued. The same run also
  showed that `decode_refuses_a_non_canonical_spelling` — the sign-byte vector,
  and the row-4 entry this table previously carried — is a composite over guards
  16 and 4 rather than an isolator of either, since either check alone refuses
  it. `decode_refuses_an_unvalidated_wrapper_byte` was added to isolate guard 4
  and re-swept: the decoder reads `code`, `field_index` and `item_index` at
  fixed offsets and never inspects the `87` head or the `41`/`43` wrappers, so a
  corrupted wrapper decodes to a well-formed statement and only the re-encode
  notices.
- Guards 8 and 9 also turned nothing red. Their vectors still pass, and in a
  real run the field guard is the **first** refusal — but §11.1's structural
  pass and §5.6's item shape refuse the same fixtures one step later, so no
  fixture can attribute to the guard. Both rows are now marked composite rather
  than isolated, which is what the sweep showed and what the table previously
  got wrong.

**What the second sweep established.** Guards 37, 38 and 39 were each weakened
alone and the module's 48 vectors re-run under the same runner. Each turned
**exactly** its own two vectors red and nothing else, and all three runs
collected 48 tests:

| weakened guard                      | vectors that turned red                                                                                                             |
| ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| 37 — a sweep begins at item 0       | `conservation_refuses_a_fold_opened_past_the_start`, `mint_unit_quantity_refuses_a_sweep_opened_past_the_target`                    |
| 38 — `field_index` has one spelling | `encode_refuses_a_conservation_statement_that_names_another_field`, `conservation_refuses_a_statement_that_names_another_field`     |
| 39 — `item_index` has one spelling  | `encode_refuses_a_conservation_statement_that_names_a_starting_item`, `conservation_refuses_a_statement_that_names_a_starting_item` |

Two vectors per row rather than one because each of these conditions is met at
two seams — the encoder and the adjudication for 38 and 39, the two sides of the
equation for 37 — and a vector at one seam says nothing about the other.

Three of this section's own backstops — rows 15, 17 and 18 — were **not** swept:
each is refused by guard 4's re-encode or by guards 1–3's shape rule one step
earlier, so neutralising it alone changes no verdict and the sweep would report
the empty result it reports for guard 16 without distinguishing "backstop" from
"dead". The cross-section backstops (19–28) are argued for the same reason plus
a second one: weakening them means editing sections this one does not own. That
residue is Phase-7 work; §11.5's own history — an _argued_ backstop, its guard
26, that was simply false — is why it is named here instead of left implicit.

Two families of assertion are not itemised, for §10.8's and §11.5's reason —
they are the construction domain of a value another guard already fixes, and no
fixture can attribute a refusal to one site rather than the other: the range
assertions inside `fault_statement_shape_is_exact` that guards 1–3 and 7/11
already run, and the offset and length bounds in the shared interior reader
(§11.5's second family), which run on every read this section makes.

### 12.7 Canonical decodability of a committed field preimage

**Normative.** Owner ruling, 2026-08-11 ([#593 Ruling
2](https://github.com/Anastasia-Labs/midgard/issues/593#issuecomment-5261103820));
executed as #596. It adds a fault kind and changes nothing else — in
particular it does not change §8.8's doors, §12.1's statement type, or any
verdict the validation machine renders.

#### The fault

> **`canonical-decodability`.** The committed preimage of field `i` of a
> committed transaction is not a §5.1 envelope.

Under §4 an operator commits `blake2b_256(preimage_i)` over **arbitrary
bytes**. §5.1 says what those bytes may be; nothing in §4 makes them be it.
So a block can commit a field whose preimage is a truncated envelope, a
mis-declared count, a non-minimal header, or no envelope at all — and every
consumer of that field aborts, because every §8.8 view door ends in the §7.4
count-consistency check and that check is an `expect`.

Aborting is the **correct** answer to a prover supplying the wrong bytes
(§7.3): wrong bytes are not evidence and a step built on them must not
succeed. It is the wrong outcome for bytes the operator itself committed,
and the difference is who chose them. Where a prover's mistake costs the
prover a transaction, an operator's commitment costs the protocol a fault it
cannot state: the `CanonicalDecode` phase exists to render a verdict about
exactly this field and cannot be reached, so **no step is producible by
anyone** and the dispute stalls instead of rejecting. A stall is not a
rejection — the block is never faulted, nothing is slashed, and the operator
has bought the outcome by committing garbage. That is the escape hatch this
section closes.

It is closed by direct fault rather than by making the doors non-aborting.
Softening them machine-wide would put a `None`-returning reader on the hot
path of every family and every machine phase, to serve one adjudication;
worse, it would make "this field did not decode" a value that could be
clamped, defaulted or ignored at any of the dozens of sites that consume a
view. The doors' abort semantics stay **exactly** as they are.

#### The boundary against §12.3, stated precisely

§12.3's `fault_item_predicate` adjudicates a **bad item inside a well-formed
envelope**. This section adjudicates a **byte string that is not an
envelope**. They are disjoint claims about disjoint sets of committed fields,
and neither is a special case of the other:

|                 | §12.3 `fault_item_predicate`                                                                | §12.7 `canonical-decodability`                                                                  |
| --------------- | ------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| what is accused | one item, at `(field_index, item_index)`                                                    | one field, at `field_index`                                                                     |
| the envelope    | must be well-formed — the walk that reaches the item is the door's, and it aborts otherwise | must **not** be well-formed; that is the whole accusation                                       |
| what is read    | exactly one item's bytes (§12.3), through a `FieldViewV1`                                   | the whole preimage, as bytes, through no view at all                                            |
| who decides     | the caller's per-item predicate, which this document does not define                        | §5.1's grammar, defined below and defined nowhere else                                          |
| item index      | carried, and load-bearing                                                                   | **absent** — an ungrammatical envelope has no item indices, so there is nothing for one to name |

The three consequences that follow are normative:

1. **A field cannot be faulted under both.** If the envelope is well-formed,
   §12.7's verdict is `grammatical` and its adjudication refuses; if it is
   not, no §12.3 walk can reach an item, because the door that would open the
   view aborts first. An implementation MUST NOT provide a path that reaches
   §12.3's adjudication over bytes §12.7 would convict — which is what
   keeping the doors aborting guarantees, and is a second reason not to
   soften them.
2. **§12.7 renders no per-field judgement.** §5.3's item encodings, §5.5's
   output rules, §5.6's mint ordering and the §2.4 declared lengths are all
   outside it. A field whose envelope is well-formed and whose items are
   nonsense is §12.3's, or its family's.
3. **The two `CanonicalDecode` verdicts that survive the flat reversion are
   also outside it.** `reject_field_preimage_size` is a disagreement between
   the authenticated preimage's own length and the §2.4
   `NativeTxFieldPreimageLengthsV1` entry for it — two independently
   committed structures, both well-formed. `reject_invalid_field_type` is an
   item whose §5.1-authenticated width is not a legal encoding for its field.
   Both presuppose a decodable envelope and are rendered by the machine, not
   here.

**Name the residue rather than a mitigation for it.** This fault kind is
§5.1's grammar and nothing else, and §5.1's grammar is not the whole of what
`whole_view` refuses. Two of that function's other `expect`s can also abort on
bytes an operator committed, and neither is convictable here:

- **§7.4's fixed-stride arithmetic.** For fields 0, 1, 3, 4 and 7 the door
  settles count consistency as `header_len + stride·N == total_length` rather
  than by walking. A committed preimage that _is_ a §5.1 envelope but whose
  items are not the field's stride satisfies this section's verdict — code 0 —
  and still aborts the door.
- **§5.4's per-field byte bound.** `whole_view` refuses a `total_length` above
  `max_transaction_aggregate_field_bytes`, and a committed preimage above it
  that is otherwise a well-formed envelope is likewise verdict 0 and still
  aborts.

Both leave the same stall this section removes for the ungrammatical case, on
a narrower set of committed bytes. Neither is folded in here, and the reason is
not effort: the verdict above is a function of the bytes alone, while both of
these are functions of the bytes **and the slot**, so admitting them would make
the fault kind field-index-dependent and would put a claim about §5.3's stride
table inside a section whose whole boundary against §12.3 is that it applies no
per-field rule.

**Both are closed by §12.8, and closed as a sibling rather than here.** The
owner ruling of 2026-08-13 took the first of the two mechanisms this section
had named — a sibling fault kind stated over `(field_index, preimage)` — and
§12.8 is it; the second, the machine rendering `reject_invalid_field_type`
from a non-aborting reader, was not taken. Executed as #601. Nothing in this
section moves: its verdict stays a function of the bytes alone and stays
`grammatical` for both shapes above, which is exactly what §12.8 requires of
it, because §12.8 convicts only bytes this section calls grammatical and
renders a non-convicting `not_an_envelope` for everything else. The two fault
kinds therefore **partition** the committed byte strings a door refuses, and
no committed field is faultable under both. §12.8's own residue — the §8
carriage ladder cannot deliver a preimage above §5.4's bound to any step, so
the byte-bound shape is a rule with no carriage above the bound — is named
there.

#### The §5.1 well-formedness predicate

Normative, and stated as a **total function** so that the Aiken and
TypeScript implementations can be written against it independently and agree.
`envelope_verdict(preimage) → code` is defined for every byte string. It
never fails, never clamps, and reads no byte it has not first shown to be in
range.

The codes:

| code | name                       | meaning                                                  |
| ---- | -------------------------- | -------------------------------------------------------- |
| 0    | `grammatical`              | the bytes are a §5.1 envelope                            |
| 1    | `missing_array_header`     | zero bytes; §5.1's shortest form is the one-byte `80`    |
| 2    | `not_an_array_header`      | leading byte outside `80..97`, `98`, `99`                |
| 3    | `non_minimal_array_header` | a `98`/`99` head whose count a narrower form spells      |
| 4    | `truncated_array_header`   | a `98`/`99` head whose own width leaves the preimage     |
| 5    | `missing_item_header`      | items remain to be read and no byte remains to start one |
| 6    | `not_an_item_header`       | an item's leading byte outside `40..57`, `58`, `59`      |
| 7    | `non_minimal_item_header`  | a `58`/`59` head whose length a narrower form spells     |
| 8    | `truncated_item_header`    | a `58`/`59` head whose own width leaves the preimage     |
| 9    | `truncated_item_payload`   | an item's declared payload leaves the preimage           |
| 10   | `trailing_bytes`           | all declared items were read and bytes remain            |

The procedure, over `preimage` of length `T`:

```
if T = 0                            → 1
b ← preimage[0]
if 0x80 ≤ b ≤ 0x97                  → walk(1, b − 0x80)
if b = 0x98:
    if T < 2                        → 4
    n ← preimage[1];  if n < 24     → 3   else walk(2, n)
if b = 0x99:
    if T < 3                        → 4
    n ← preimage[1]·256 + preimage[2]
    if n ≤ 255                      → 3   else walk(3, n)
otherwise                           → 2

walk(off, rem):
    if rem ≤ 0                      → 0 if off = T else 10
    if off ≥ T                      → 5
    h ← preimage[off]
    if 0x40 ≤ h ≤ 0x57              → step(off + 1, h − 0x40, rem)
    if h = 0x58:
        if off + 2 > T              → 8
        L ← preimage[off + 1];  if L < 24 → 7 else step(off + 2, L, rem)
    if h = 0x59:
        if off + 3 > T              → 8
        L ← preimage[off + 1]·256 + preimage[off + 2]
        if L ≤ 255                  → 7 else step(off + 3, L, rem)
    otherwise                       → 6

step(payload_off, L, rem):
    if payload_off + L > T          → 9
    else                            → walk(payload_off + L, rem − 1)
```

Four properties of this definition are normative rather than incidental:

- **The acceptance set is §5.1's, not CBOR's.** The four-byte `9a` array head
  and the `5a` byte-string head are well-formed CBOR and are code 2 and code
  6 respectively. §5.1 caps at `99 NNNN` / `59 LLLL` and this is where that
  cap is enforced for the verdict, exactly as `decode_field_array_header_at`
  enforces it for the doors.
- **The codes are diagnostic, not load-bearing.** The adjudication asks only
  whether the code is 0 (below). A mis-assigned non-zero code convicts the
  same set of blocks. They are distinguished so that a conformance vector can
  say _which_ rule of §5.1 a fixture leaves, which is the difference between
  a suite that pins the grammar and one that pins "something was wrong".
- **The verdict MUST agree with the doors on every input.** `code = 0` if and
  only if `whole_view` would accept the same bytes for a variable-width field
  within §5.4's byte bound.
  The two are separate implementations on purpose — one decides by returning,
  the other by aborting, and a shared `Option`-returning reader would put a
  clampable absence on the doors' hot path — so the agreement is a conformance
  obligation (§9) discharged by vectors in both directions, not an artefact of
  sharing code.
- **The verdict is the whole decision.** An implementation MUST NOT add
  conditions to it. In particular it MUST NOT bound the preimage's length:
  §5.4's byte bound is a property of a valid transaction, and a field
  committed above it that is _also_ not an envelope must stay convictable.

#### Adjudication

Two steps, on the `da-hash-preimage` pattern (GOAL_SPEC.md Q44) and for its
reason: the first binds committed evidence and derives, the second holds the
derivation against the rule.

**Step 01 — bind, authenticate, decide.**

1. The disputed transaction MUST be bound to the challenged block's committed
   `transactions_root` by the shared native inclusion path, including the
   codec precondition every native family runs. A leaf whose key is not its
   own transaction id is `da-hash-preimage`'s fault, not this one's, and the
   compact structure this step extracts a commitment from means nothing until
   the id derived from it is the committed key.
2. The prover supplies **which** of §2.5's nine slots is accused and a §8.8
   `FieldCarriageV1` for it. The accused slot's expected commitment is
   obtained by positional extraction from the committed compact structures
   (§4) — never as a free-standing argument — and the carried bytes are
   hashed against it. A prover supplying anything else **aborts** (§7.3),
   which is what makes the bytes that reach the verdict undeniably the
   committed preimage.
3. `envelope_verdict` is taken over those bytes and `(tx_id, field_index,
verdict)` is pinned into the computation thread. Every member is derived,
   so a fabricated verdict or a re-addressed field index is not forwardable.

**Step 02 — convict.** The proof finalizes when, and only when, the pinned
state satisfies `0 ≤ field_index < 9`, `0 ≤ verdict < 11` and `verdict ≠ 0`.
The two bounds are §12.1's one-spelling rule applied to a state that crosses
a transaction boundary: a state naming a tenth field or a twelfth code is one
step 01 could not have written, and admitting it would let one fault finalize
under many spellings.

Five further conditions are normative:

- **No preimage bytes travel between the steps.** §12.1's witness minimality
  holds for the same reason it holds for a fault statement: the bytes were
  authenticated in the transaction that read them, and re-supplying them
  would be re-supplying a field the first transaction already paid for.
- **The reading path MUST NOT be a view door.** Both §8.8 doors materialise a
  `FieldViewV1` and therefore run §5.1's walk under `expect`; reaching the
  verdict through either would reproduce inside this family the stall it
  exists to end. The door entry point this family uses returns the
  hash-checked **bytes** and runs no §5.1 check at all. It MUST NOT be used
  to read items: nothing it returns carries an authenticated item count or
  item boundary, which is precisely what §7's items 2 and 4 buy and what it
  does not.
- **The field index is a redeemer argument here**, unlike at every read site,
  where §4's loss of field-index domain separation makes a call-site literal
  mandatory. It has to be free because it _is_ the accusation's address
  (§12.1). It is safe because the expected commitment is extracted for the
  slot the index names: a verdict rendered under index `i` is a verdict about
  the bytes slot `i` committed, whichever `i` was chosen. §4's aliasing —
  fields 0/1 and 3/4 commit identically for identical content — does not
  weaken this, because aliased slots commit the _same bytes_ and a
  non-envelope convicted under either index is a non-envelope under both.
- **All three §8 tiers are admissible, at all nine fields.** Tier 3 was
  admissible for the witness-set fields here even while erratum E2's limit-3
  refusal held them shut at the lazy door (a distinction the #606 repair has
  since dissolved — the lazy door now admits them too, through the welded
  `field_hash` equality), and the reason is the same one §8.11 gives for the
  tx-order mint: this path materialises the chunks and hashes the
  concatenation once against the committed commitment, so the §8.6 manifest
  authenticates nothing about content and a substituted chunk changes the
  hash. The manifest still buys §8.4's partition and the datum's
  `(tx_id, field_index, field_hash)` binding under the policy's constant-name
  token. Restricting the family to tier 1 would cap the fault kind at what
  fits one redeemer, leaving a smaller escape hatch open rather than none.
- **An honest block is bound but never convicted.** Step 01 accepts a
  challenge against a well-formed field and forwards `verdict = 0`; step 02
  refuses it. That asymmetry is the family's whole safety property against a
  malicious challenger, and it is where a conformance suite must have a
  vector.

#### Cost claims

Measured, at GOAL_SPEC §3.3's 13,200,000-memory basis, and pinned by
`onchain/aiken/scripts/canonical-decodability-exec-ledger-v1.json` with
`verify-canonical-decodability-exec-ledger-v1.mjs` as its gate. The claim
splits the way the work does.

- **The byte term is cheap and is not the binding axis.** At §5.4's per-field
  bound of 32,768 committed bytes carried as a single item on §8.4's chunked
  route at §8.3's `max_tier3_chunk_count` of three — the largest committed
  preimage any admissible carriage reaches — the rule's own share is about
  **697,743 memory units**, 5.3% of the basis, three-chunk materialisation and
  the whole-preimage `blake2b_256` included.
- **The item term binds.** Measured against fixture-only controls at 250 and
  500 minimum-width items, the §5.1 walk costs **12,032.44 memory units and
  3,044,870.75 cpu units per item** over an intercept of 251,285 / 72,591,751.
- **The ceiling is measured, not fitted.** The single-transaction adjudication
  ceiling for this family is **1,076 items**, and **1,077** is the first
  cardinality over the basis. This was a fit — `(13,200,000 − 251,285) /
12,032.44` — until #580's re-measurement pass (2026-08-15) bisected the
  crossing on the net memory axis and read it; #606's E2 certificate repair
  (2026-08-16) then **falsified that reading and moved it down one item**, and
  it was re-bisected through #580's own net-memory method rather than
  re-fitted. At 1,076 items the rule's own share is **13,193,329 memory /
  3,347,323,381 cpu** — a margin of 6,671 under the basis — and at 1,077 it is
  **13,205,353 memory**, over it. The re-derived fit names the same integer.
  Memory binds at the crossing — cpu is 3.35G against the 8G basis — and the
  marginal cost measured across the boundary pair, 12,024 memory units per
  item, is within 0.07% of the figure the 250/500 pair fitted and is **unmoved
  by the repair**: what #606 costs this family is a constant, so the intercept
  rose (242,085 → 251,285 memory, 71,119,751 → 72,591,751 cpu) and the per-item
  price did not. The four selectors that carry the reading sit beside the
  ledger's rows rather than inside them, because a whole-test reading at this
  cardinality is about 24.2M memory and a within-basis ledger admits only rows
  whose raw reading fits.
- **The residual, named rather than mitigated.** The worst shape this family
  admits is §5.4's byte bound spent on minimum-width items. §5.1's narrowest
  item is the empty one, `40`, so that is a three-byte array header, 32,764
  one-byte items and one trailing byte — 32,768 bytes carrying 32,764 items —
  which the measured marginal cost above puts at roughly 394,200,000 memory
  units, **29.9× the basis**. That figure is still an extrapolation rather
  than a reading, and it is recorded as such; what #580 replaced with a
  measurement is the ceiling it extrapolates past, not this shape. Its
  consequence is exact: a committed field carrying more than **1,076** items
  cannot be adjudicated by this family in one transaction. The hatch that
  leaves is narrower than the one this section closes — an operator would have
  to commit bytes that are _also_ a many-item envelope prefix — but it is a
  hatch, and the repair is §10's resumable walk applied to the verdict rather
  than anything in this section. Raised on #596; measured on #580.

#### Guard coverage

§9's conformance items 5 and 6 require every refusal on an operational path to
be isolated by a vector or listed as a backstop. **Fifteen refusals: thirteen
isolated, two backstops.**

| #   | refusal                                                                        | isolated by / backstop because                                                                                                                                                                  |
| --- | ------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | the carried bytes hash to the accused slot's commitment (§4)                   | `canonical_decodability_step_01_rejects_uncommitted_bytes`, and `canonical_decodability_verdict_refuses_substituted_certified_chunks` at tier 3                                                 |
| 2   | the forwarded verdict is the one the bytes earned                              | `canonical_decodability_step_01_rejects_a_fabricated_verdict`                                                                                                                                   |
| 3   | the forwarded field index is the one that was opened                           | `canonical_decodability_step_01_rejects_a_fabricated_field_index`                                                                                                                               |
| 4   | a body claim names a body slot                                                 | `canonical_decodability_step_01_rejects_a_body_claim_at_a_witness_field`                                                                                                                        |
| 5   | a witness claim names a witness-set slot                                       | `canonical_decodability_step_01_rejects_a_witness_claim_at_a_body_field`                                                                                                                        |
| 6   | the disputed transaction is one the challenged block committed                 | `canonical_decodability_step_01_rejects_a_forged_transactions_root`                                                                                                                             |
| 7   | a grammatical field does not finalize                                          | `canonical_decodability_step_02_rejects_a_grammatical_field`, with `…_step_01_binds_a_grammatical_field_without_convicting` proving the refusal is step 02's rather than an accident of step 01 |
| 8   | `field_index` is in 0..8, upper bound                                          | `canonical_decodability_step_02_rejects_an_out_of_range_field_index`                                                                                                                            |
| 9   | `field_index` is in 0..8, lower bound                                          | `canonical_decodability_step_02_rejects_a_negative_field_index`                                                                                                                                 |
| 10  | `verdict` is a code some walk produces                                         | `canonical_decodability_step_02_rejects_an_unknown_verdict_code`                                                                                                                                |
| 11  | the verdict agrees with the doors, ungrammatical direction                     | the ten `canonical_decodability_door_refuses_*` selectors, one per code, because an abort ends a test and a fold over the table would prove only that some vector aborts                        |
| 12  | the verdict agrees with the doors, grammatical direction                       | `canonical_decodability_door_opens_every_grammatical_vector`                                                                                                                                    |
| 13  | the witness set re-derives to the committed `witness_set_hash`                 | backstop: `field_door_prologue` asserts it for every door entry point and §8.8's own guard coverage isolates it; guards 4 and 5 fix which half a claim may name                                 |
| 14  | §8.4's tier-3 partition (`total_length > K`, derived chunk count, chunk shape) | backstop: `certified_chunks` is shared with both view doors and §8.10's rows isolate it there; this family re-uses it unchanged                                                                 |
| 15  | the derived accusation is produced at step 02's script                         | `canonical_decodability_step_01_rejects_a_foreign_next_step_hash`                                                                                                                               |

The verdict's own decision table is pinned separately and exhaustively:
`canonical_decodability_verdict_table_is_exact` asserts one vector per code,
`…_covers_every_code` asserts the table reaches all eleven and no more, and
`…_pins_the_minimal_width_boundaries` and `…_pins_declared_count_mismatches`
pin §5.1's width boundaries at 23/24 and 255/256 on both the array head and the
item head, and the declared-versus-actual mismatch in both directions.

#### Registration

The family's two validators and their catalogue arms are registered by #579's
single regeneration event (rider 6 of its owner-authorized scope amendment).
Until then the compiled artifacts do not exist and the code is source-only,
per #587's precedent. The identity moves this section's implementation records
for that batch are on #596.

### 12.8 Committed field shape at a slot

**Normative.** Owner ruling, 2026-08-13, taking the first — option (a) — of
the two closure mechanisms §12.7's residue paragraph named: a sibling fault
kind stated over `(field_index, preimage)`. Executed as
[#601](https://github.com/Anastasia-Labs/midgard/issues/601). It adds a fault
kind and changes
nothing else — in particular it does not change §8.8's doors, §12.1's
statement type, §12.7's verdict or its family, or any verdict the validation
machine renders.

#### The fault

> **`committed-field-shape`.** The committed preimage of field `i` of a
> committed transaction is a §5.1 envelope that field `i`'s own §7.4/§5.4
> rules refuse.

§12.7 closed the stall for a committed preimage that is not a §5.1 envelope,
and said in terms that §5.1's grammar is not the whole of what `whole_view`
refuses. Two of that function's other `expect`s abort on bytes an operator
committed while §12.7's verdict is `grammatical`:

- **§7.4's fixed-stride arithmetic.** For fields 0, 1, 3, 4 and 7 the door
  settles count consistency as `header_len + stride·N == total_length`. An
  envelope whose items are not the field's stride passes §5.1 and fails this.
- **§5.4's per-field byte bound.** `whole_view` refuses a `total_length` above
  `max_transaction_aggregate_field_bytes`. An oversize envelope passes §5.1
  and fails this.

Each leaves the identical outcome §12.7 exists to end, on a narrower set of
committed bytes: the door aborts, no step of §12.7's family is producible
(its verdict is 0, and its step 02 refuses), no step of any other family can
reach a view, so **no step is producible by anyone** and the dispute stalls
instead of rejecting. The block is never faulted, nothing is slashed, and the
operator has bought the outcome by committing an envelope of the wrong shape.

It is closed by a **sibling** fault kind rather than by widening §12.7, and
the reason is §12.7's own boundary. That section's verdict is a function of
the committed bytes **alone**, and that is precisely what makes it disjoint
from §12.3: a section that applies no per-field rule cannot be accused of
adjudicating an item. Both shapes above are functions of the bytes **and the
slot**. Folding them in would have made §12.7 field-index-dependent and put a
claim about §5.3's stride table inside it. One more fault kind is the cheaper
price, and it keeps both boundaries exact.

The doors' abort semantics stay **exactly** as they are, for §12.7's reason:
aborting is still the correct answer to a _prover_ supplying the wrong bytes
(§7.3), and softening the doors machine-wide to serve one adjudication would
put a clampable absence on every consumer's hot path.

#### The boundary against §12.7, stated precisely

§12.7 adjudicates a byte string that is **not** a §5.1 envelope. This section
adjudicates a byte string that **is** one and that slot `i` still refuses.
They are disjoint claims about disjoint sets of committed fields:

|                         | §12.7 `canonical-decodability`                            | §12.8 `committed-field-shape`                                  |
| ----------------------- | --------------------------------------------------------- | -------------------------------------------------------------- |
| what is accused         | one field, at `field_index`                               | one field, at `field_index`                                    |
| the envelope            | must **not** be well-formed; that is the whole accusation | must be well-formed; the accusation is about its length        |
| the verdict's arguments | `preimage`                                                | `(field_index, preimage)`                                      |
| what decides            | §5.1's grammar, and nothing per-field                     | §7.4's stride arithmetic and §5.4's byte bound, both per-field |
| what is read            | the whole preimage, as bytes, through no view at all      | the same bytes, plus §5.1's array header — no item content     |

Three consequences follow, and they are normative:

1. **A field cannot be faulted under both.** This section's verdict is
   `not_an_envelope` — a code that does **not** convict — for every byte
   string §12.7's verdict does not call `grammatical`, and its adjudication
   MUST refuse that code. An implementation MUST NOT provide a path that
   convicts under this section a field §12.7 would convict.
2. **This section renders no judgement about items.** §5.3's item encodings,
   §5.5's output rules, §5.6's mint ordering and the §2.4 declared lengths
   are all outside it, exactly as they are outside §12.7. It reads §5.1's
   array header and the preimage's length, and nothing else. A field whose
   envelope is the right shape and whose items are nonsense is §12.3's, or
   the machine's.
3. **The §12.3 boundary is inherited unchanged.** §12.3's
   `fault_item_predicate` adjudicates a bad item inside an envelope **the
   door opens**. This section convicts only envelopes the door _refuses_, so
   the two are disjoint for the same reason §12.7 and §12.3 are: the walk
   that would reach an item aborts first.

**One family of shapes is outside all three, and is named rather than
claimed.** At a fixed-stride field, `whole_view` decides §5.1 by arithmetic
and never walks the items, so it **opens** a preimage whose stride arithmetic
holds whatever the item bytes are — whether they are well-formed items of the
wrong widths, or not §5.1 item heads at all. Neither is this section's: the
door opens the field, so there is no construction-time stall to remove, and
§7.4 item 2 puts the refusal at the accessor (`field_item_extent`) instead.
This section renders `code = 0` for the first and `code = 1` for the second,
and convicts neither. Whether the accessor's refusal is itself adjudicable is
the machine's question — `reject_invalid_field_type` is the verdict in view —
and this section does not decide it.

#### The shape verdict

Normative, and stated as a **total function of the bytes** so that the Aiken
and TypeScript implementations can be written against it independently and
agree. `field_shape_verdict(field_index, preimage) → code` is defined for
every byte string at every `field_index` in `0..8`. It never clamps and reads
no byte it has not first shown to be in range.

The codes:

| code | name               | meaning                                                              |
| ---- | ------------------ | -------------------------------------------------------------------- |
| 0    | `admissible`       | field `i`'s door opens these bytes                                   |
| 1    | `not_an_envelope`  | not a §5.1 envelope — §12.7's fault; does **not** convict here       |
| 2    | `field_byte_bound` | §5.4: `total_length > max_transaction_aggregate_field_bytes`         |
| 3    | `wrong_stride`     | §7.4: `header_len + stride·N ≠ total_length` at a fixed-stride field |

The procedure, over `preimage` of length `T` at slot `i`:

```
stride ← field_stride(i)                    -- §5.3; refuses i outside 0..8
h ← minimal_array_header(preimage)          -- (header_len, N), or absent
if h is absent                              → 1
if envelope_verdict(preimage) ≠ 0           → 1        -- §12.7's, deferred
if T > maxTransactionAggregateFieldBytes    → 2
if stride = 0                               → 0        -- variable-width field
if header_len + stride·N = T                → 0
otherwise                                   → 3
```

Five properties of this definition are normative rather than incidental:

- **The two arguments have different owners, and the asymmetry is
  deliberate.** The preimage is the _operator's_: an abort on it would be the
  stall under adjudication, so the verdict is total over bytes. The
  `field_index` is the _prover's_: `field_stride` refuses one outside §2.5's
  nine, and that refusal is §7.3's correct answer to a prover supplying
  something outside the format. Every caller reaches the verdict through the
  door entry point below, which bounds the index before a byte is read.
- **§5.1's grammar is consulted, not restated.** `envelope_verdict` is
  §12.7's function, used unchanged as the guard. Two spellings of §5.1 would
  be exactly the drift §6.1 forbids, and the disjointness of the two fault
  kinds is only as good as their agreement on what an envelope is. This
  section adds no grammar; it adds the two questions the grammar cannot ask.
  `minimal_array_header` is the narrow half of the door's header decoder —
  three widths and no items — returned as an option rather than as an abort.
- **The order of the questions is the door's order.** §5.4's bound is checked
  before §7.4's arithmetic because that is the `expect` `whole_view` reaches
  first. An oversize envelope at a fixed-stride field whose stride also fails
  is code 2 and not code 3: one committed field earns one accusation (§12.1).
- **The verdict MUST agree with the doors in the two directions that decide
  outcomes, and the third is deliberately not claimed.** `code ∈ {2, 3}` MUST
  imply that `whole_view` aborts on the same bytes at the same field index —
  no field this section convicts is one the machine could have opened — and
  `code = 0` MUST imply that `whole_view` accepts them. The converse of the
  second does **not** hold and must not be asserted: at a fixed-stride field
  `whole_view` settles §5.1 by arithmetic and never walks the items (§7.4
  item 2 puts per-item wrapper canonicality at the accessor instead), so
  bytes whose stride arithmetic holds while their item heads are not §5.1's
  open the door and earn `code = 1` here. That is fail-safe by construction
  rather than by luck: code 1 does not convict, the field is §12.7's to
  fault, and the accessor refuses the read. The two implementations are
  separate on purpose, so what agreement there is remains a conformance
  obligation (§9) discharged by vectors, not an artefact of shared code.
- **Codes 0 and 1 are both non-convicting, and are not interchangeable.**
  Code 1 is this section's boundary against §12.7 and code 0 is the honest
  block. An implementation MUST distinguish them: collapsing them would leave
  a §12.7 fault indistinguishable from a well-shaped field in the computation
  thread, which §12.1's one-spelling rule forbids for a value that crosses a
  transaction boundary.

#### Adjudication

Two steps, on §12.7's pattern and for its reason: the first binds committed
evidence and derives, the second holds the derivation against the rule.

**Step 01 — bind, authenticate, decide.**

1. The disputed transaction MUST be bound to the challenged block's committed
   `transactions_root` by the shared native inclusion path, including the
   codec precondition every native family runs.
2. The prover supplies **which** of §2.5's nine slots is accused and a §8.8
   `FieldCarriageV1` for it, as §12.7's `CommittedFieldClaimV1` — the same
   wire type, reused rather than re-declared, because the accusation the two
   sibling fault kinds make is the same accusation and §6.1's one-spelling
   rule applies to a wire type as much as to a scalar. The accused slot's
   expected commitment is obtained by positional extraction from the
   committed compact structures (§4) and the carried bytes are hashed against
   it. A prover supplying anything else **aborts** (§7.3).
3. `field_shape_verdict` is taken over those bytes **at that slot**, and
   `(tx_id, field_index, verdict)` is pinned into the computation thread.
   Every member is derived, so a fabricated verdict or a re-addressed field
   index is not forwardable. The re-addressing refusal carries more weight
   here than in §12.7, because the verdict _depends_ on the index: a
   re-addressed accusation would be a verdict about one slot's rules filed
   against another slot's bytes.

**Step 02 — convict.** The proof finalizes when, and only when, the pinned
state satisfies `0 ≤ field_index < 9`, `0 ≤ verdict < 4` and
`verdict ∈ {2, 3}`. The membership test MUST name the convicting codes rather
than test `verdict ≠ 0`: code 1 is §12.7's fault and admitting it here would
let one committed field finalize under two fault kinds.

Five further conditions are normative:

- **No preimage bytes travel between the steps**, for §12.7's reason.
- **The step-02 state is a distinct type from §12.7's**, notwithstanding that
  its three members read identically. The verdict code spaces differ — 0..10
  there, 0..3 here — so one type would let a §12.7 code satisfy this
  section's bounds check and mean something else while doing it. The two
  families are held apart by the script hashes their step-01s pin and by
  these two types, not by convention.
- **The reading path MUST NOT be a view door.** Both §8.8 doors materialise a
  `FieldViewV1` and therefore run the very `expect`s under accusation.
  §12.7's `authenticated_committed_preimage` — the non-aborting entry point
  that returns the hash-checked bytes and runs no §5.1, §7.4 or §5.4 check —
  is reused unchanged and is the only door entry point this family uses.
- **The field index is a redeemer argument here**, for §12.7's reason: it
  _is_ the accusation's address (§12.1). It is safe because the expected
  commitment is extracted for the slot the index names, so a verdict rendered
  under index `i` is a verdict about the bytes slot `i` committed. §4's
  aliasing — fields 0/1 and 3/4 commit identically for identical content —
  does not weaken it here either, and for a stronger reason than in §12.7:
  the aliased pairs share a stride (0 and 1 at 40, 3 and 4 at 30), so a
  wrong-stride conviction under either index is a wrong-stride conviction
  under both.
- **An honest block is bound but never convicted.** Step 01 accepts a
  challenge against a well-shaped field and forwards `verdict = 0`; step 02
  refuses it. That asymmetry is the family's whole safety property against a
  malicious challenger, and it is where a conformance suite must have a
  vector — as is the same asymmetry at code 1.

#### Carriage, and this section's own residue

**All three §8 tiers are admissible, at all nine fields**, on §12.7's
argument: this path materialises the carriage and hashes it once against the
committed commitment, so a §8.6 manifest authenticates nothing about content
and a substituted chunk changes the hash. That argument is inherited rather
than re-derived — this family reuses §12.7's door entry point unchanged, and
its own tier-3 vector is taken at a body field, so the witness-set half of the
claim rests on §12.7's reasoning (the carve-out §8.3 erratum E2 used to make
for this path is moot since #606 resolved limit 3 — every door admits the
witness-set fields under tier 3 now).

**The §5.4 shape has a rule and, above the bound, no carriage.** This is
recorded rather than mitigated, exactly as §12.7 recorded the residue this
section closes:

- The verdict above convicts an oversize envelope at any length, and the
  §12.8 family's steps adjudicate it wherever the bytes can be carried.
- The §8 ladder cannot carry a preimage above §5.4's bound to a step.
  `certified_chunks` refuses `total_length > max_transaction_aggregate_field_bytes`
  before it materialises tier 3, and the §8.6 certificate is refused at the
  same bound at minting, so no such certificate can exist. Tiers 1 and 2 are
  bounded far below it by L1's `maxTxSize`.
- Therefore a committed preimage above §5.4's bound is convictable _in the
  rule_ and unreachable _in the carriage_: the stall §12.7 named for that
  shape is closed for every length the ladder can deliver, and open above it.
  The §7.4 shape has no such gap — it lives entirely at lengths at or below
  the bound and is convictable at all three tiers.

Closing the remainder is an amendment to §8, not to this section: the ladder
would have to admit a preimage above §5.4's bound for this adjudication and
for no other consumer, which is a change to the certificate policy and to
`certified_chunks` and is therefore an owner-class decision about the
carriage. It is recorded on #601 and is not taken here.

#### Cost claims

Measured, at GOAL_SPEC §3.3's 13,200,000-memory basis, and pinned by
`onchain/aiken/scripts/committed-field-shape-exec-ledger-v1.json` with
`verify-committed-field-shape-exec-ledger-v1.mjs` as its gate.

The dominant term is §12.7's §5.1 walk, reused here as the envelope guard;
this section's own two questions are `O(1)` over a header read in three bytes.

- **The byte term.** At §5.4's per-field bound of 32,768 committed bytes
  carried as a single item on §8.4's chunked route at §8.3's
  `max_tier3_chunk_count` of three, the rule's own share against a
  fixture-only control is about **716,158 memory units**, 5.4% of the basis,
  three-chunk materialisation and the whole-preimage `blake2b_256` included.
- **The item term binds.** Measured against fixture-only controls at 250 and
  500 minimum-width items, the walk-plus-verdict costs **12,040.88 memory
  units and 3,046,474.22 cpu units per item** over an intercept of 271,198 /
  78,251,615.
- **The ceiling is measured, not fitted, and it reconciles with §12.7's.**
  This family's single-transaction ceiling is **1,074 items**, with **1,075**
  the first over the basis: at 1,074 the rule's own share is **13,193,414
  memory / 3,347,103,163 cpu**, and at 1,075 it is **13,205,438 memory**, over
  it. Memory binds — cpu at the crossing is about 3.35G against the 8G basis.
  This was a fit, deliberately unreconciled with §12.7's own fit, until #580's
  re-measurement pass (2026-08-15) bisected both crossings on the net memory
  axis; #606's E2 certificate repair (2026-08-16) then **falsified both
  readings and moved each down one item**, and both were re-bisected by the
  same method. **Both are still readings**, and the reconciliation survives the
  move intact: the two families really do differ, by two items — §12.7 measures
  1,076 and this one 1,074 — the gap being this family's higher intercept, the
  §7.4 stride arithmetic it asks after the same walk. The fit is again one item
  low (roughly 1,073 against the measured 1,074), exactly as it was before the
  repair. #606's cost here is a constant too: the intercept rose (261,998 →
  271,198 memory, 76,779,615 → 78,251,615 cpu) and the per-item price is
  unchanged to the unit. The four selectors carrying the reading sit beside the
  ledger's rows rather than inside them, because a whole-test reading at this
  cardinality is about 24.1M memory and a within-basis ledger admits only rows
  whose raw reading fits.
- **The residual, named rather than mitigated.** The worst shape this family
  admits is §5.4's byte bound spent on §5.1's narrowest item, `40`: a
  three-byte array header and 32,765 one-byte items is 32,768 bytes carrying
  32,765 items, which the measured marginal cost puts at roughly 394,200,000
  memory units, **29.9× the basis**. That figure is still an extrapolation
  rather than a reading, and is recorded as such; what #580 replaced with a
  measurement is the ceiling it extrapolates past, not this shape. Its
  consequence is exact: a committed field carrying more than **1,074** items
  cannot be adjudicated by this family in one transaction, and the repair is
  §10's resumable walk applied to the verdict.

#### Guard coverage

§9's conformance items 5 and 6 require every refusal on an operational path to
be isolated by a vector or listed as a backstop. **Sixteen refusals: fourteen
isolated, two backstops.**

| #   | refusal                                                                        | isolated by / backstop because                                                                                                                                                                                                                                       |
| --- | ------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | the carried bytes hash to the accused slot's commitment (§4)                   | `committed_field_shape_step_01_rejects_uncommitted_bytes`, `…_rejects_a_substituted_raw_utxo_carriage` at tier 2, and `committed_field_shape_verdict_refuses_substituted_certified_chunks` at tier 3                                                                 |
| 2   | the forwarded verdict is the one the bytes earned at that slot                 | `committed_field_shape_step_01_rejects_a_fabricated_verdict`                                                                                                                                                                                                         |
| 3   | the forwarded field index is the one that was opened                           | `committed_field_shape_step_01_rejects_a_fabricated_field_index`                                                                                                                                                                                                     |
| 4   | a body claim names a body slot                                                 | `committed_field_shape_step_01_rejects_a_body_claim_at_a_witness_field`                                                                                                                                                                                              |
| 5   | a witness claim names a witness-set slot                                       | `committed_field_shape_step_01_rejects_a_witness_claim_at_a_body_field`                                                                                                                                                                                              |
| 6   | the disputed transaction is one the challenged block committed                 | `committed_field_shape_step_01_rejects_a_forged_transactions_root`                                                                                                                                                                                                   |
| 7   | a well-shaped field does not finalize                                          | `committed_field_shape_step_02_rejects_an_admissible_field`, with `…_step_01_binds_a_right_stride_field_without_convicting` proving the refusal is step 02's rather than an accident of step 01                                                                      |
| 8   | **a field §12.7 owns does not finalize here**                                  | `committed_field_shape_step_02_rejects_a_non_envelope`, with `…_step_01_binds_a_non_envelope_without_convicting` proving the same                                                                                                                                    |
| 9   | `field_index` is in 0..8, upper bound                                          | `committed_field_shape_step_02_rejects_an_out_of_range_field_index`                                                                                                                                                                                                  |
| 10  | `field_index` is in 0..8, lower bound                                          | `committed_field_shape_step_02_rejects_a_negative_field_index`                                                                                                                                                                                                       |
| 11  | `verdict` is a code some verdict produces                                      | `committed_field_shape_step_02_rejects_an_unknown_verdict_code`                                                                                                                                                                                                      |
| 12  | the verdict agrees with the doors, refusing direction                          | the six `committed_field_shape_door_refuses_*` selectors, one per convicting shape, because an abort ends a test and a fold over the table would prove only that some vector aborts                                                                                  |
| 13  | the verdict agrees with the doors, opening direction                           | `committed_field_shape_door_opens_every_admissible_vector` and `…_door_opens_the_field_at_the_byte_bound`; the direction that is _not_ claimed is pinned as such by `…_defers_a_fixed_stride_field_the_door_opens`, so a later reader cannot mistake it for a defect |
| 14  | the derived accusation is produced at step 02's script                         | `committed_field_shape_step_01_rejects_a_foreign_next_step_hash`                                                                                                                                                                                                     |
| 15  | the witness set re-derives to the committed `witness_set_hash`                 | backstop: `field_door_prologue` asserts it for every door entry point and §8.8's own guard coverage isolates it; guards 4 and 5 fix which half a claim may name                                                                                                      |
| 16  | §8.4's tier-3 partition (`total_length > K`, derived chunk count, chunk shape) | backstop: `certified_chunks` is shared with both view doors and §8.10's rows isolate it there; this family re-uses it unchanged                                                                                                                                      |

The verdict's own decision table is pinned separately and exhaustively:
`committed_field_shape_verdict_table_is_exact` asserts one vector per code,
`…_covers_every_code` asserts the table reaches all four and no more,
`…_verdict_depends_on_the_slot` asks one envelope of all nine slots and
requires the five fixed-stride ones to convict and the four walked ones not
to, `…_pins_the_stride_boundary` and `…_pins_the_byte_bound_boundary` pin both
rules in both directions, `…_byte_bound_precedes_the_stride` pins the order,
and `…_is_disjoint_from_canonical_decodability` pins the partition against
§12.7 over the whole table in both directions.

#### Registration

The family's two validators and their catalogue arms are registered by #579's
single regeneration event (rider 6 of its owner-authorized scope amendment).
Until then the compiled artifacts do not exist and the code is source-only,
per #587's precedent. The identity moves this section's implementation records
for that batch are on #601.
