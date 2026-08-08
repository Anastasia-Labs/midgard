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
- **Last reviewed:** 2026-08-08 (initial authoring, Phase 0 of the flat
  reversion program).
- **Version:** `native_tx_version_v1 = 1`. Pre-launch, this format replaces
  the counted bounded-collection commitment scheme in place (GOAL_SPEC §3
  invariant 13); there is no compatibility path to the retired scheme.
- **Provisional values:** the constants marked _provisional_ in §8.3 are
  pinned by analysis and are re-measured in Phase 4 of the reversion
  program; falsification by measurement is an amendment-level erratum to
  this document by design, and does not reopen GOAL_SPEC acceptance
  criteria.

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
   observes (§7); and
7. the three-tier publication-carriage convention for field preimages (§8).

**Deferred by design (Phase 3).** The dispute-side mechanisms that _consume_
this format are deliberately not defined here: the resumable walk and its
checkpoint encoding, the Value bookmark, and the Canonical-Data Acceptor.
They are Phase-3 deliverables of the reversion program and are added to this
document as **§10 — resumable walk and checkpoints** by
[#570](https://github.com/Anastasia-Labs/midgard/issues/570) and **§11 —
Value bookmark and Canonical-Data Acceptor** by
[#571](https://github.com/Anastasia-Labs/midgard/issues/571). Until those
sections land, §7 governs every consumer, and §7 invariant 6 in particular
binds any resumable state to positions rather than verbatim bytes. Documents
that bind those mechanisms by reference — `GOAL_SPEC.md` §3.1(2) — name them
against this note, not against a definition that exists today.

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
byte-lexicographic compare), duplicates reject, and asset quantities are
strictly positive; `datum_cbor` MUST satisfy the §6.2 canonicity
predicate.

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
   preimage unfaultable.
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

| constant                            | value              | status          |
| ----------------------------------- | ------------------ | --------------- |
| `K` (chunk size / tier-2 bound)     | **15,900 bytes**   | **provisional** |
| `maxTier1RedeemerPreimageBytes`     | **14,336 bytes**   | **provisional** |
| `maxTransactionAggregateFieldBytes` | 32,768 bytes       | retained        |
| maximum tier-3 chunk count          | `⌈32,768 / K⌉ = 3` | derived         |

Basis. Both values are **provisional-pending-Phase-4-measurement**: each is
pinned by analysis over existing measurements, not by a measurement of the
final publication or step transaction — neither of which exists yet.
Falsification by Phase-4 measurement is an amendment-level erratum to this
table (the _Provisional values_ bullet in this document's front matter) and
does not reopen any GOAL_SPEC acceptance criterion.

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
  two item-size frontiers: `maxExactCompleteItemPublicationBytes` 15,489,
  the largest item whose signed publication lands exactly on `maxTxSize`
  (16,384), and `maxReliableCompleteItemPublicationBytes` 14,993, the
  largest whose publication lands on `maxTxSize` minus the 512-byte
  `proofItemEnvelopeReliabilityReserveBytes`. The reserve is a
  **transaction-side** budget, not an item-side one: the two frontiers are
  496 item bytes apart because that shape's non-item framing is itself 16
  bytes lighter at the smaller size (895 B at 15,489 → 879 B at 14,993).
  Both frontiers are pinned by the "pins the exact applied publication
  frontiers and reliability reserve" case in
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`.
  K = 15,900 exceeds both. That is expected, because the tier-2/3
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
  total_length: Int,                 -- preimage byte length (ragged-last + offset math)
  chunk_digests: List<ByteArray>,    -- blake2b-256 per chunk, in order;
                                     --   length = ceil(total_length / K)
}
```

**Mint (certification).** The certification redeemer carries `compact_cbor`
(and `witness_set_compact_cbor`). The policy re-derives the tx-id through
the unchanged §3 derivation, extracts the expected field hash positionally
from the supplied structures (satisfying §4 via the
transitively-committed-by-tx-id clause), verifies
`blake2b_256(chunk_0 ‖ … ‖ chunk_{n-1})` over the redeemer-ordered
referenced raw chunks against that hash, and checks `total_length` and every
per-chunk digest against the actual bytes. Order is supplied by the
redeemer's reference-input indices and verified in one shot; per-chunk
authentication at certification time is unnecessary.

**Token.** Quantity 1; deterministic asset name derived from
`(tx_id, field_index)` for indexer discovery. Duplicate certificates are
permitted and harmless — each is independently sound.

The derivation is normative, because the minting policy and every consuming
step have to name the same token and an off-chain minter has to reproduce it:

```
asset_name = blake2b_256(field_index_byte ‖ tx_id)
```

— a 33-byte preimage whose first byte is `field_index` (0..8) and whose
remaining 32 are `tx_id`, yielding a 32-byte name. The leading byte is domain
separation, not a length header; with `field_index` bounded to 0..8 and
`tx_id` fixed at 32 bytes the preimage is unambiguous, and both bounds are
enforced rather than assumed. Implementations MUST reject a `field_index`
outside 0..8 and a `tx_id` that is not 32 bytes.

**One multi-handler validator.** The same script carries the `mint` and
`spend` handlers, so the policy id and the spend credential are one script
hash — mint sends to its own address; spend burns its own policy plus owner
signature. No external reference-script bootstrap, no cyclic dependency.

**Consumption.** A certificate serves any step, thread, or game disputing
the same transaction, indefinitely; it is game-, block-, and
source-agnostic. A consuming step matches the certificate's
`(tx_id, field_index)` only against **authenticated** sources (the thread's
already-authenticated disputed transaction) — never redeemer-supplied
identity. Post-certification single-chunk access authenticates at O(one
chunk hash) against the digest vector (worst case two chunk hashes on a
straddling item); for a fixed-stride field `count` derives from the
mint-verified `total_length` with no chunk hash spent.

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

## 9. Conformance

1. Aiken and TypeScript twins emit byte-identical preimages, compact
   encodings, and hashes for every value in this document's domain; shared
   golden vectors pin every field including the empty (`80`) case, the
   fixed-index boundary values (0, 23, 24, 255, 256, 65,535), the 28-byte
   width assertion, mint policy/asset ordering, and the §6.2 acceptance
   boundaries (2⁶⁴ ± 1 bignums, constructor alternatives 127/128).
2. Decoders are fail-closed everywhere: non-minimal heads (outside the
   pinned fixed-width index), wrapper/length mismatches, count/length
   inconsistency, trailing bytes, non-canonical datum/redeemer payloads,
   and any retired counted-scheme surface all reject.
3. Negative-vector suites cover the §7 invariants: out-of-range index,
   straddling-item reads, short/empty-slice equality attempts, certificate
   `(tx_id, field_index)` mismatch, count/total_length inconsistency, and
   wrong-field carriage.
