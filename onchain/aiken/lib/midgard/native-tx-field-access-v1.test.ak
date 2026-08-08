//// Negative-vector family for the single field-access door
//// (`docs/spec/midgard-tx.md` §7 and §9.3).
////
//// Two invariants are normative and are proved here for **all nine fields**:
////
////   * **Abort, never clamp** (§7.3). `bytearray.slice` clamps, so an
////     unguarded accessor answers an out-of-range read with bytes instead of
////     an error — and two clamped reads past the end are byte-equal, which is
////     exactly the shape of a forged "these two items are identical" claim
////     built from a perfectly valid block. Every out-of-range read below must
////     abort.
////   * **Count consistency at view construction** (§7.4, §5.1). The item count
////     lives only in the preimage header, so a preimage the operator actually
////     committed to can still lie about it. Miscounted and truncated
////     preimages are hashed correctly here — the source is built *from* the
////     malformed bytes — and must still be refused on structure alone.
////
//// The tier ladder (§8.1–§8.4) is exercised end to end, including a tier-3
//// read whose item straddles a chunk boundary.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_redeemer_witness,
  encode_midgard_tx_input, encode_midgard_tx_output,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{encode_mint_policy_item}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardAddressWitness, MidgardExecutionUnits,
  MidgardRedeemerWitness, MidgardTxInput, MidgardTxOutput, MidgardValue,
  MidgardVersionedScript, NativeTxBodyCompact, NativeTxCompact,
  NativeTxWitnessSetCompact, PlutusV3Script, PubKeyCredential, SpendRedeemer,
  VerifiedMidgardNativeTxCompact, native_tx_version_v1,
}
use midgard/native_tx_field_access_v1.{
  Certified, Chunked, FieldCarriageV1, FieldPreimageCertificateV1, FieldViewV1,
  Inline, RawUtxo, Whole, authenticated_field_view, chunk_bytes_k,
  empty_field_commitment, encode_field_array_header, encode_field_preimage,
  field_commitment, field_commitment_from_items, field_item_at,
  field_item_count, field_item_extent, field_preimage_certificate_asset_name,
  field_read_range, field_stride, field_total_length, hash28_stride,
  spend_input_stride,
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const sample_tx_id: ByteArray =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const certificate_policy_id: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

fn h32(seed: Int) -> ByteArray {
  blake2b_256(bytearray.from_int_big_endian(seed, 4))
}

fn h28(seed: Int) -> ByteArray {
  bytearray.take(h32(seed), 28)
}

fn h64(seed: Int) -> ByteArray {
  bytearray.concat(h32(seed), h32(seed + 100))
}

/// One representative item per field, at that field's §5.3 width. Fields
/// 2/5/6/8 deliberately vary in length across `seed` so the walk has real work
/// to do; the fixed-stride fields cannot vary and must not.
fn field_item(field_index: Int, seed: Int) -> ByteArray {
  if or {
    field_index == 0,
    field_index == 1,
  } {
    encode_midgard_tx_input(
      MidgardTxInput { tx_id: h32(seed), output_index: seed },
    )
  } else if field_index == 2 {
    encode_midgard_tx_output(
      MidgardTxOutput {
        address: MidgardAddress {
          protected: False,
          network_id: 0,
          payment_credential: PubKeyCredential(h28(seed)),
          stake_credential: None,
        },
        value: MidgardValue { lovelace: 1000000 * seed + 7, assets: [] },
        datum_cbor: None,
        script_ref: None,
      },
    )
  } else if or {
    field_index == 3,
    field_index == 4,
  } {
    h28(seed)
  } else if field_index == 5 {
    encode_mint_policy_item(
      builtin.b_data(h28(seed)),
      builtin.map_data(
        [Pair(builtin.b_data(h28(seed + 1)), builtin.i_data(seed + 1))],
      ),
    )
  } else if field_index == 6 {
    encode_midgard_versioned_script(
      MidgardVersionedScript {
        language: PlutusV3Script,
        script_bytes: bytearray.take(h64(seed), 20 + seed * 9),
      },
    )
  } else if field_index == 7 {
    encode_midgard_address_witness(
      MidgardAddressWitness {
        verification_key: h32(seed),
        signature: h64(seed),
      },
    )
  } else {
    encode_midgard_redeemer_witness(
      MidgardRedeemerWitness {
        purpose: SpendRedeemer,
        index: seed,
        redeemer_cbor: bytearray.take(h32(seed), 1 + seed * 5),
        execution_units: MidgardExecutionUnits {
          memory: 1000 + seed,
          steps: 2000 + seed,
        },
      },
    )
  }
}

fn field_items(field_index: Int) -> List<ByteArray> {
  [
    field_item(field_index, 1),
    field_item(field_index, 2),
    field_item(field_index, 3),
  ]
}

fn field_preimage(field_index: Int) -> ByteArray {
  encode_field_preimage(field_items(field_index))
}

/// Builds the committed structures that carry `preimage`'s flat hash in
/// `field_index`'s slot. Every other slot holds a hash the preimage cannot
/// match, so a test that reads the wrong slot fails rather than passing by
/// accident.
fn source(
  field_index: Int,
  preimage: ByteArray,
) -> Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact> {
  let committed = field_commitment(preimage)
  let slot = fn(index: Int) -> ByteArray {
    if index == field_index {
      committed
    } else {
      zero_hash32
    }
  }
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: slot(7),
    script_tx_wits_hash: slot(6),
    redeemer_tx_wits_hash: slot(8),
  }
  let body = NativeTxBodyCompact {
    spend_inputs_hash: slot(0),
    reference_inputs_hash: slot(1),
    outputs_hash: slot(2),
    fee: 0,
    validity_interval_start: -1,
    validity_interval_end: -1,
    required_observers_hash: slot(3),
    required_signers_hash: slot(4),
    mint_hash: slot(5),
    script_integrity_hash: zero_hash32,
    auxiliary_data_hash: zero_hash32,
    network_id: 255,
  }
  Pair(
    VerifiedMidgardNativeTxCompact {
      tx_id: sample_tx_id,
      version: native_tx_version_v1,
      tx_compact: NativeTxCompact {
        body,
        witness_set_hash: blake2b_256(
          encode_native_tx_witness_set_compact(witness_set),
        ),
        validity_code: 0,
      },
    },
    witness_set,
  )
}

fn view_of(
  field_index: Int,
  preimage: ByteArray,
  carriage: FieldCarriageV1,
  reference_inputs: List<Input>,
) -> FieldViewV1 {
  let Pair(verified, witness_set) = source(field_index, preimage)
  authenticated_field_view(
    verified,
    witness_set,
    field_index,
    carriage,
    reference_inputs,
    certificate_policy_id,
  )
}

/// Tier 1 — the step's own redeemer carries the bytes.
fn inline_view(field_index: Int, preimage: ByteArray) -> FieldViewV1 {
  view_of(field_index, preimage, Inline { preimage }, [])
}

fn key_output(datum: Data) -> Output {
  Output {
    address: address.from_verification_key(h28(9)),
    value: assets.from_lovelace(2000000),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn input_of(index: Int, output: Output) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: h32(200 + index),
      output_index: index,
    },
    output,
  }
}

// ---------------------------------------------------------------------------
// Positive controls: one door, all nine fields
// ---------------------------------------------------------------------------

/// Every item of `field_index` reads back byte-identically through the door,
/// and the reveal-derived count matches. This is the "served by the one door"
/// property, proved field by field.
fn door_serves(field_index: Int) -> Bool {
  let items = field_items(field_index)
  let view = inline_view(field_index, encode_field_preimage(items))
  and {
    field_item_count(view) == 3,
    field_item_at(view, 0) == field_item(field_index, 1),
    field_item_at(view, 1) == field_item(field_index, 2),
    field_item_at(view, 2) == field_item(field_index, 3),
    list.length(items) == 3,
  }
}

test door_serves_field_0_spend_inputs() {
  door_serves(0)
}

test door_serves_field_1_reference_inputs() {
  door_serves(1)
}

test door_serves_field_2_outputs() {
  door_serves(2)
}

test door_serves_field_3_required_observers() {
  door_serves(3)
}

test door_serves_field_4_required_signers() {
  door_serves(4)
}

test door_serves_field_5_mint() {
  door_serves(5)
}

test door_serves_field_6_script_witnesses() {
  door_serves(6)
}

test door_serves_field_7_address_witnesses() {
  door_serves(7)
}

test door_serves_field_8_redeemer_witnesses() {
  door_serves(8)
}

/// `empty_field_commitment` is a pinned literal so that the validation
/// machine's emptiness comparisons stop re-hashing `80` at every site. The pin
/// is only safe while it agrees with the producer, which is what this proves.
test empty_field_commitment_matches_the_producer() {
  and {
    encode_field_preimage([]) == #"80",
    empty_field_commitment == field_commitment(#"80"),
    empty_field_commitment == field_commitment_from_items([]),
  }
}

/// An empty field is exactly `80` in all nine slots, and the door reports zero
/// items rather than refusing to open.
test empty_field_opens_as_the_single_byte_form() {
  let opens = fn(field_index: Int) -> Bool {
    let view = inline_view(field_index, encode_field_preimage([]))
    field_item_count(view) == 0
  }
  and {
    encode_field_preimage([]) == #"80",
    opens(0),
    opens(1),
    opens(2),
    opens(3),
    opens(4),
    opens(5),
    opens(6),
    opens(7),
    opens(8),
  }
}

/// §5.3's fixed strides, asserted against the bytes the encoders actually
/// produce rather than against a restated constant.
test fixed_stride_fields_have_the_declared_item_widths() {
  and {
    bytearray.length(field_item(0, 1)) == 38,
    bytearray.length(field_item(1, 1)) == 38,
    bytearray.length(field_item(3, 1)) == 28,
    bytearray.length(field_item(4, 1)) == 28,
    bytearray.length(field_item(7, 1)) == 101,
    field_stride(0) == spend_input_stride,
    field_stride(3) == hash28_stride,
    // Field 0's fixed 3-byte output index is what buys the constant width:
    // index 1 is spelled `19 0001`, not the minimal `01`.
    bytearray.drop(field_item(0, 1), 35) == #"190001",
  }
}

// ---------------------------------------------------------------------------
// Abort, never clamp (§7.3)
// ---------------------------------------------------------------------------

/// Reads item `index`, which is out of range. The body can only be reached if
/// the accessor answered instead of aborting, so `fail` passes exactly when
/// the read aborted — a clamped answer of any shape fails the test.
fn out_of_range_read(field_index: Int, index: Int) -> Bool {
  let view = inline_view(field_index, field_preimage(field_index))
  bytearray.length(field_item_at(view, index)) >= 0
}

test clamp_never_reaches_caller_field_0() fail {
  out_of_range_read(0, 3)
}

test clamp_never_reaches_caller_field_1() fail {
  out_of_range_read(1, 3)
}

test clamp_never_reaches_caller_field_2() fail {
  out_of_range_read(2, 3)
}

test clamp_never_reaches_caller_field_3() fail {
  out_of_range_read(3, 3)
}

test clamp_never_reaches_caller_field_4() fail {
  out_of_range_read(4, 3)
}

test clamp_never_reaches_caller_field_5() fail {
  out_of_range_read(5, 3)
}

test clamp_never_reaches_caller_field_6() fail {
  out_of_range_read(6, 3)
}

test clamp_never_reaches_caller_field_7() fail {
  out_of_range_read(7, 3)
}

test clamp_never_reaches_caller_field_8() fail {
  out_of_range_read(8, 3)
}

test negative_index_aborts_field_0() fail {
  out_of_range_read(0, -1)
}

test negative_index_aborts_field_3() fail {
  out_of_range_read(3, -1)
}

test negative_index_aborts_field_7() fail {
  out_of_range_read(7, -1)
}

test negative_index_aborts_field_8() fail {
  out_of_range_read(8, -1)
}

/// The forgery §7.3 exists to stop: two different out-of-range indices whose
/// clamped reads would be byte-equal, and therefore "prove" two distinct items
/// identical. Both must abort; neither may produce bytes to compare.
test far_out_of_range_reads_cannot_fabricate_equality() fail {
  let view = inline_view(3, field_preimage(3))
  field_item_at(view, 9) == field_item_at(view, 17)
}

/// Ranged reads past the end abort as well, so no caller can reach clamped
/// bytes by going around the item accessor.
test range_read_past_the_end_aborts() fail {
  let preimage = field_preimage(4)
  let view = inline_view(4, preimage)
  bytearray.length(field_read_range(view, bytearray.length(preimage) - 2, 8)) >= 0
}

test item_extent_stays_inside_the_preimage() {
  let preimage = field_preimage(0)
  let view = inline_view(0, preimage)
  let Pair(offset, length) = field_item_extent(view, 2)
  and {
    // header(1) + stride 40 * 2 + wrapper 2
    offset == 83,
    length == 38,
    offset + length <= bytearray.length(preimage),
  }
}

// ---------------------------------------------------------------------------
// Count consistency (§7.4, §5.1)
// ---------------------------------------------------------------------------

/// A preimage whose header claims one more item than the body carries. The
/// source is rebuilt from these bytes, so the flat hash matches and the only
/// thing that can reject it is the structural check the door runs at view
/// construction.
fn miscounted_preimage(field_index: Int) -> ByteArray {
  let honest = field_preimage(field_index)
  bytearray.push(bytearray.drop(honest, 1), 0x84)
}

fn open_miscounted(field_index: Int) -> Bool {
  let preimage = miscounted_preimage(field_index)
  field_item_count(inline_view(field_index, preimage)) >= 0
}

test miscount_is_refused_field_0() fail {
  open_miscounted(0)
}

test miscount_is_refused_field_1() fail {
  open_miscounted(1)
}

test miscount_is_refused_field_2() fail {
  open_miscounted(2)
}

test miscount_is_refused_field_3() fail {
  open_miscounted(3)
}

test miscount_is_refused_field_4() fail {
  open_miscounted(4)
}

test miscount_is_refused_field_5() fail {
  open_miscounted(5)
}

test miscount_is_refused_field_6() fail {
  open_miscounted(6)
}

test miscount_is_refused_field_7() fail {
  open_miscounted(7)
}

test miscount_is_refused_field_8() fail {
  open_miscounted(8)
}

/// The mirror image: the header undercounts, so the walk finishes early and
/// leaves trailing bytes. §5.1 fails closed on both.
fn undercounted_preimage(field_index: Int) -> ByteArray {
  bytearray.push(bytearray.drop(field_preimage(field_index), 1), 0x82)
}

fn open_undercounted(field_index: Int) -> Bool {
  field_item_count(inline_view(field_index, undercounted_preimage(field_index))) >= 0
}

test trailing_bytes_are_refused_field_0() fail {
  open_undercounted(0)
}

test trailing_bytes_are_refused_field_2() fail {
  open_undercounted(2)
}

test trailing_bytes_are_refused_field_3() fail {
  open_undercounted(3)
}

test trailing_bytes_are_refused_field_6() fail {
  open_undercounted(6)
}

test trailing_bytes_are_refused_field_8() fail {
  open_undercounted(8)
}

/// A preimage cut short mid-item: the header is honest but the bytes run out.
fn truncated_preimage(field_index: Int) -> ByteArray {
  let honest = field_preimage(field_index)
  bytearray.take(honest, bytearray.length(honest) - 3)
}

fn open_truncated(field_index: Int) -> Bool {
  field_item_count(inline_view(field_index, truncated_preimage(field_index))) >= 0
}

test truncation_is_refused_field_0() fail {
  open_truncated(0)
}

test truncation_is_refused_field_1() fail {
  open_truncated(1)
}

test truncation_is_refused_field_2() fail {
  open_truncated(2)
}

test truncation_is_refused_field_3() fail {
  open_truncated(3)
}

test truncation_is_refused_field_4() fail {
  open_truncated(4)
}

test truncation_is_refused_field_5() fail {
  open_truncated(5)
}

test truncation_is_refused_field_6() fail {
  open_truncated(6)
}

test truncation_is_refused_field_7() fail {
  open_truncated(7)
}

test truncation_is_refused_field_8() fail {
  open_truncated(8)
}

/// A non-minimal item count header (`98 03` where `83` is canonical) is
/// refused even though it decodes to the right number.
test non_minimal_count_header_is_refused() fail {
  let honest = field_preimage(3)
  let preimage = bytearray.concat(#"9803", bytearray.drop(honest, 1))
  field_item_count(inline_view(3, preimage)) >= 0
}

/// §5.1 caps the count header at the `99 NNNN` form. The four-byte `9a` head
/// is perfectly good CBOR and still outside this grammar, so it rejects.
test four_byte_count_header_is_outside_the_grammar() fail {
  let honest = field_preimage(3)
  let preimage = bytearray.concat(#"9a00000003", bytearray.drop(honest, 1))
  field_item_count(inline_view(3, preimage)) >= 0
}

// ---------------------------------------------------------------------------
// Item wrapper canonicality on the fixed-stride fields (§5.1, §6.1)
// ---------------------------------------------------------------------------
//
// A fixed stride tells an accessor where an item starts, and that is all it
// tells it. If the accessor infers the two `definite_bytes_header` bytes from
// the stride instead of reading them, every one of these preimages opens and
// hands back the very same payload as the canonical spelling — one logical
// field with several admissible commitments, which is exactly what §6.1
// forbids, and a preimage a producer could commit and never be faulted for.
//
// Every vector below keeps the total byte length and the item count intact, so
// §7.4's arithmetic check at view construction still passes and the *only*
// thing that can reject them is a decoder that reads the wrapper.

/// Rewrites item `index`'s two wrapper bytes to `spelling`, leaving payload,
/// item count and total length untouched.
fn rewrapped_preimage(
  field_index: Int,
  index: Int,
  spelling: ByteArray,
) -> ByteArray {
  let honest = field_preimage(field_index)
  // Three items, so the §5.1 count header is the one-byte `83` form.
  let wrapper_offset = 1 + field_stride(field_index) * index
  bytearray.take(honest, wrapper_offset)
    |> bytearray.concat(spelling)
    |> bytearray.concat(bytearray.drop(honest, wrapper_offset + 2))
}

fn read_rewrapped(field_index: Int, index: Int, spelling: ByteArray) -> Bool {
  let preimage = rewrapped_preimage(field_index, index, spelling)
  bytearray.length(field_item_at(inline_view(field_index, preimage), index)) >= 0
}

/// The rewrite really is byte-length- and count-preserving: the door still
/// opens the view, so the negative tests below are proving the wrapper read
/// and not some incidental structural rejection.
test rewrapped_preimages_still_pass_the_construction_check() {
  let preimage = rewrapped_preimage(3, 0, #"0000")
  and {
    bytearray.length(preimage) == bytearray.length(field_preimage(3)),
    field_item_count(inline_view(3, preimage)) == 3,
  }
}

test noncanonical_item_wrapper_is_refused_field_0() fail {
  read_rewrapped(0, 0, #"ffff")
}

test noncanonical_item_wrapper_is_refused_field_1() fail {
  read_rewrapped(1, 2, #"0000")
}

test noncanonical_item_wrapper_is_refused_field_3() fail {
  read_rewrapped(3, 1, #"0000")
}

test noncanonical_item_wrapper_is_refused_field_4() fail {
  read_rewrapped(4, 2, #"ffff")
}

test noncanonical_item_wrapper_is_refused_field_7() fail {
  read_rewrapped(7, 0, #"0000")
}

/// A well-formed byte-string head of the wrong *width* (`59 LLLL` where §5.3's
/// item calls for `58 LL`) shifts the payload by one and must reject too.
test wider_item_wrapper_form_is_refused_field_7() fail {
  read_rewrapped(7, 1, #"5920")
}

/// A well-formed `58 LL` head declaring the wrong *length* for the stride: the
/// wrapper decodes, but the item it describes is not the item the stride says
/// lives there.
test item_wrapper_length_must_match_the_stride_field_3() fail {
  read_rewrapped(3, 0, #"581b")
}

test item_wrapper_length_must_match_the_stride_field_0() fail {
  read_rewrapped(0, 1, #"5825")
}

/// The two counterexamples that found this, byte for byte. Field 3's item is a
/// raw 28-byte hash, so §5.1 spells its wrapper `58 1c`; field 0's is the
/// 38-byte input encoding, spelled `58 26`.
test field_3_zero_wrapper_counterexample_is_refused() fail {
  let preimage = #"81"
    |> bytearray.concat(#"0000")
    |> bytearray.concat(h28(5))
  bytearray.length(field_item_at(inline_view(3, preimage), 0)) >= 0
}

test field_0_ones_wrapper_counterexample_is_refused() fail {
  let preimage = #"81"
    |> bytearray.concat(#"ffff")
    |> bytearray.concat(field_item(0, 1))
  bytearray.length(field_item_at(inline_view(0, preimage), 0)) >= 0
}

/// The other half of the counterexample: the canonical spelling of the same
/// content is the one the encoder emits, and it opens and reads back.
test the_canonical_spelling_of_those_counterexamples_opens() {
  let observer = h28(5)
  let spend_input = field_item(0, 1)
  let canonical_3 = #"81"
    |> bytearray.concat(#"581c")
    |> bytearray.concat(observer)
  let canonical_0 = #"81"
    |> bytearray.concat(#"5826")
    |> bytearray.concat(spend_input)
  and {
    canonical_3 == encode_field_preimage([observer]),
    canonical_0 == encode_field_preimage([spend_input]),
    field_item_at(inline_view(3, canonical_3), 0) == observer,
    field_item_at(inline_view(0, canonical_0), 0) == spend_input,
  }
}

/// The wrapper read reaches tier 3 as well — the arithmetic path is shared, so
/// a chunked view must refuse the same deviation.
test noncanonical_item_wrapper_is_refused_under_tier_three() fail {
  let preimage = straddle_preimage()
  let corrupted = bytearray.take(preimage, 3)
    |> bytearray.concat(#"0000")
    |> bytearray.concat(bytearray.drop(preimage, 5))
  let chunks = [
    bytearray.take(corrupted, chunk_bytes_k),
    bytearray.drop(corrupted, chunk_bytes_k),
  ]
  expect [chunk_0, chunk_1] = chunks
  bytearray.length(
    field_item_at(
      view_of(
        3,
        corrupted,
        Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
        [
          Input {
            ..certificate_input(0, chunks),
            output: Output {
              ..certificate_input(0, chunks).output,
              datum: InlineDatum(
                FieldPreimageCertificateV1 {
                  owner: h28(3),
                  tx_id: sample_tx_id,
                  field_index: 3,
                  total_length: bytearray.length(corrupted),
                  chunk_digests: list.map(chunks, blake2b_256),
                },
              ),
            },
          },
          input_of(1, key_output(builtin.b_data(chunk_0))),
          input_of(2, key_output(builtin.b_data(chunk_1))),
        ],
      ),
      0,
    ),
  ) >= 0
}

/// Positional identity (§4): a preimage that is perfectly well formed for
/// field 3 cannot be opened in field 4's slot, because the expected hash comes
/// from the committed structure and not from the caller.
test a_field_cannot_be_opened_in_another_slot() fail {
  let preimage = field_preimage(3)
  let Pair(verified, witness_set) = source(3, preimage)
  field_item_count(
    authenticated_field_view(
      verified,
      witness_set,
      4,
      Inline { preimage },
      [],
      certificate_policy_id,
    ),
  ) >= 0
}

/// A witness set the tx-id never committed cannot unlock fields 6–8.
test substituted_witness_set_cannot_open_field_six() fail {
  let preimage = field_preimage(6)
  let Pair(verified, witness_set) = source(6, preimage)
  field_item_count(
    authenticated_field_view(
      verified,
      NativeTxWitnessSetCompact { ..witness_set, addr_tx_wits_hash: h32(77) },
      6,
      Inline { preimage },
      [],
      certificate_policy_id,
    ),
  ) >= 0
}

// ---------------------------------------------------------------------------
// Carriage tiers (§8.1–§8.4)
// ---------------------------------------------------------------------------

test tier_two_raw_utxo_carriage_opens_the_same_view() {
  let preimage = field_preimage(2)
  let reference_inputs = [
    input_of(0, key_output(builtin.b_data(#"00"))),
    input_of(1, key_output(builtin.b_data(preimage))),
  ]
  let view = view_of(
    2,
    preimage,
    RawUtxo { ref_input_index: 1 },
    reference_inputs,
  )
  and {
    field_item_count(view) == 3,
    field_item_at(view, 1) == field_item(2, 2),
  }
}

test tier_two_wrong_reference_input_is_refused() fail {
  let preimage = field_preimage(2)
  let reference_inputs = [
    input_of(0, key_output(builtin.b_data(#"00"))),
    input_of(1, key_output(builtin.b_data(preimage))),
  ]
  field_item_count(
    view_of(2, preimage, RawUtxo { ref_input_index: 0 }, reference_inputs),
  ) >= 0
}

/// Builds a field-3 preimage long enough to need two tier-3 chunks: 600
/// observer items at stride 30 plus a 3-byte header is 18,003 bytes, so
/// chunk 0 is a full `chunk_bytes_k` and chunk 1 carries the ragged tail.
const straddle_item_count: Int = 600

fn repeat_bytes(unit: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else if times == 1 {
    unit
  } else {
    let half = repeat_bytes(unit, times / 2)
    let doubled = bytearray.concat(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, unit)
    }
  }
}

fn straddle_preimage() -> ByteArray {
  let item = h28(5)
  let wrapped = bytearray.concat(#"581c", item)
  bytearray.concat(
    encode_field_array_header(straddle_item_count),
    repeat_bytes(wrapped, straddle_item_count),
  )
}

fn straddle_chunks() -> List<ByteArray> {
  let preimage = straddle_preimage()
  let total = bytearray.length(preimage)
  [
    bytearray.take(preimage, chunk_bytes_k),
    bytearray.drop(preimage, chunk_bytes_k),
  ]
    |> list.filter(fn(chunk) { bytearray.length(chunk) > 0 })
    |> fn(chunks) {
        expect total > chunk_bytes_k
        chunks
      }
}

/// An honest §8.6 manifest for `preimage` carried as `chunks`: it states the
/// real byte length and the real per-chunk digests. Only the §5.1 header
/// inside the bytes ever lies in the vectors below, which is what makes them
/// tests of the door's structural checks rather than of its hash checks.
fn certificate_for(
  index: Int,
  field_index: Int,
  preimage: ByteArray,
  chunks: List<ByteArray>,
) -> Input {
  let datum: Data = FieldPreimageCertificateV1 {
    owner: h28(3),
    tx_id: sample_tx_id,
    field_index,
    total_length: bytearray.length(preimage),
    chunk_digests: list.map(chunks, blake2b_256),
  }
  Input {
    output_reference: OutputReference {
      transaction_id: h32(300 + index),
      output_index: index,
    },
    output: Output {
      address: address.from_script(certificate_policy_id),
      value: assets.from_lovelace(2000000)
        |> assets.add(
            certificate_policy_id,
            field_preimage_certificate_asset_name(sample_tx_id, field_index),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn certificate_input(index: Int, chunks: List<ByteArray>) -> Input {
  certificate_for(index, 3, straddle_preimage(), chunks)
}

fn straddle_reference_inputs() -> List<Input> {
  let chunks = straddle_chunks()
  expect [chunk_0, chunk_1] = chunks
  [
    certificate_input(0, chunks),
    input_of(1, key_output(builtin.b_data(chunk_0))),
    input_of(2, key_output(builtin.b_data(chunk_1))),
  ]
}

fn straddle_view() -> FieldViewV1 {
  view_of(
    3,
    straddle_preimage(),
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
    straddle_reference_inputs(),
  )
}

/// Item 529 starts at 3 + 30·529 = 15,873 and its 28 payload bytes end at
/// 15,903 — three bytes past the chunk-0 boundary. The read must stitch the
/// two chunks together and still return the exact item.
test tier_three_certified_carriage_reads_a_straddling_item() {
  let view = straddle_view()
  let Pair(offset, length) = field_item_extent(view, 529)
  and {
    field_item_count(view) == straddle_item_count,
    offset == 15875,
    length == 28,
    offset < chunk_bytes_k,
    offset + length > chunk_bytes_k,
    field_item_at(view, 529) == h28(5),
    field_item_at(view, 0) == h28(5),
    field_item_at(view, straddle_item_count - 1) == h28(5),
  }
}

test tier_three_out_of_range_item_still_aborts() fail {
  bytearray.length(field_item_at(straddle_view(), straddle_item_count)) >= 0
}

/// Lazy chunk verify has teeth: a chunk whose bytes do not match the
/// certificate's digest is refused when a read reaches it, not trusted because
/// the certificate was well formed.
test tier_three_corrupt_chunk_is_refused_on_touch() fail {
  let chunks = straddle_chunks()
  expect [chunk_0, chunk_1] = chunks
  let corrupted = bytearray.concat(bytearray.take(chunk_1, 4), h28(99))
  let reference_inputs = [
    certificate_input(0, chunks),
    input_of(1, key_output(builtin.b_data(chunk_0))),
    input_of(2, key_output(builtin.b_data(corrupted))),
  ]
  bytearray.length(
    field_item_at(
      view_of(
        3,
        straddle_preimage(),
        Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
        reference_inputs,
      ),
      529,
    ),
  ) >= 0
}

/// A certificate for a different `(tx_id, field_index)` cannot be borrowed:
/// the token name the door demands is derived from the authenticated pair.
test tier_three_certificate_for_another_field_is_refused() fail {
  let chunks = straddle_chunks()
  expect [chunk_0, chunk_1] = chunks
  let stolen = Input {
    ..certificate_input(0, chunks),
    output: Output {
      ..key_output(builtin.b_data(#"00")),
      value: assets.from_lovelace(2000000)
        |> assets.add(
            certificate_policy_id,
            field_preimage_certificate_asset_name(sample_tx_id, 4),
            1,
          ),
    },
  }
  bytearray.length(
    field_item_at(
      view_of(
        3,
        straddle_preimage(),
        Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
        [
          stolen,
          input_of(1, key_output(builtin.b_data(chunk_0))),
          input_of(2, key_output(builtin.b_data(chunk_1))),
        ],
      ),
      0,
    ),
  ) >= 0
}

/// §7.4 in its tier-3 form: the count is derived arithmetically from the
/// certificate's mint-verified `total_length`, so a manifest that misstates
/// the length cannot open at all.
test tier_three_total_length_inconsistency_is_refused() fail {
  let chunks = straddle_chunks()
  expect [chunk_0, chunk_1] = chunks
  let bad_certificate = Input {
    ..certificate_input(0, chunks),
    output: Output {
      ..certificate_input(0, chunks).output,
      datum: InlineDatum(
        FieldPreimageCertificateV1 {
          owner: h28(3),
          tx_id: sample_tx_id,
          field_index: 3,
          total_length: bytearray.length(straddle_preimage()) - 1,
          chunk_digests: list.map(chunks, blake2b_256),
        },
      ),
    },
  }
  field_item_count(
    view_of(
      3,
      straddle_preimage(),
      Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
      [
        bad_certificate,
        input_of(1, key_output(builtin.b_data(chunk_0))),
        input_of(2, key_output(builtin.b_data(chunk_1))),
      ],
    ),
  ) >= 0
}

// ---------------------------------------------------------------------------
// Watcher recompute (§4)
// ---------------------------------------------------------------------------

/// The property the whole reversion exists for: a watcher recomputes any of
/// the nine commitments from raw canonical bytes and `blake2b_256` alone — no
/// leaves, no frontier, no per-item witnesses, no field index mixed in.
test watcher_recomputes_every_commitment_from_bytes_alone() {
  let recomputes = fn(field_index: Int) -> Bool {
    let preimage = field_preimage(field_index)
    let Pair(verified, witness_set) = source(field_index, preimage)
    let
        NativeTxBodyCompact {
          spend_inputs_hash,
          reference_inputs_hash,
          outputs_hash,
          required_observers_hash,
          required_signers_hash,
          mint_hash,
          ..
        }
      = verified.tx_compact.body
    let expected = if field_index == 0 {
      spend_inputs_hash
    } else if field_index == 1 {
      reference_inputs_hash
    } else if field_index == 2 {
      outputs_hash
    } else if field_index == 3 {
      required_observers_hash
    } else if field_index == 4 {
      required_signers_hash
    } else if field_index == 5 {
      mint_hash
    } else if field_index == 6 {
      witness_set.script_tx_wits_hash
    } else if field_index == 7 {
      witness_set.addr_tx_wits_hash
    } else {
      witness_set.redeemer_tx_wits_hash
    }
    blake2b_256(preimage) == expected
  }
  and {
    recomputes(0),
    recomputes(1),
    recomputes(2),
    recomputes(3),
    recomputes(4),
    recomputes(5),
    recomputes(6),
    recomputes(7),
    recomputes(8),
  }
}

/// Fields 0/1 and 3/4 share item encoders, so identical content aliases across
/// those pairs. §4 accepts that consequence of plain hashing and relies on
/// positional identity instead; pinning it here keeps the aliasing a decision
/// rather than a surprise.
test identical_content_aliases_across_field_pairs() {
  and {
    field_commitment(field_preimage(0)) == field_commitment(field_preimage(1)),
    field_commitment(field_preimage(3)) == field_commitment(field_preimage(4)),
  }
}

// ---------------------------------------------------------------------------
// The tier ladder is a partition (§8.4) and the tier-1/tier-3 count symmetry
// ---------------------------------------------------------------------------
//
// §8.4 defines tier 3 as the `preimage_len > K` case, and §8's
// simplest-fitting-first rule only means something if a field that fits a
// simpler tier cannot also be carried by a more complex one. The boundary is
// what makes the ladder a partition; without it a one-chunk "certificate"
// authenticates a preimage of any size, and every structural check tiers 1-2
// run at view construction can be side-stepped by re-carrying the same bytes
// under tier 3.
//
// The second half of this section records what the tiers do and do not agree
// on above the boundary. Tiers 1-2 hold the whole preimage, so §5.1's
// full-content walk is affordable at construction and a header that miscounts
// or undercounts its items is refused outright. Tier 3 cannot run that walk:
// `read_chunked_range` re-verifies the chunk a read lands in every time, so
// walking N variable-width items costs N `blake2b_256` hashes over a whole
// chunk. So the count is authenticated for a fixed-stride field (§7.4
// arithmetic against the mint-verified `total_length`) and refused for a
// variable-width one, rather than answered with a number nobody checked.

/// A single-chunk tier-3 carriage: exactly the shape §8.4's `preimage_len > K`
/// boundary excludes, and the shape that made the round-2 field-6
/// counterexample reachable without a 15,900-byte field.
fn one_chunk_certified_view(
  field_index: Int,
  preimage: ByteArray,
) -> FieldViewV1 {
  view_of(
    field_index,
    preimage,
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1] },
    [
      certificate_for(0, field_index, preimage, [preimage]),
      input_of(1, key_output(builtin.b_data(preimage))),
    ],
  )
}

/// `field_total_length` reads no count, so these vectors fail exactly when the
/// door refuses to *build* the view — not when it refuses to answer for it.
fn open_one_chunk_certified(field_index: Int) -> Bool {
  field_total_length(
    one_chunk_certified_view(field_index, field_preimage(field_index)),
  ) >= 0
}

test tier_three_refuses_a_below_boundary_field_0() fail {
  open_one_chunk_certified(0)
}

test tier_three_refuses_a_below_boundary_field_1() fail {
  open_one_chunk_certified(1)
}

test tier_three_refuses_a_below_boundary_field_2() fail {
  open_one_chunk_certified(2)
}

test tier_three_refuses_a_below_boundary_field_3() fail {
  open_one_chunk_certified(3)
}

test tier_three_refuses_a_below_boundary_field_4() fail {
  open_one_chunk_certified(4)
}

test tier_three_refuses_a_below_boundary_field_5() fail {
  open_one_chunk_certified(5)
}

test tier_three_refuses_a_below_boundary_field_6() fail {
  open_one_chunk_certified(6)
}

test tier_three_refuses_a_below_boundary_field_7() fail {
  open_one_chunk_certified(7)
}

test tier_three_refuses_a_below_boundary_field_8() fail {
  open_one_chunk_certified(8)
}

/// The exact round-2 counterexample, spelled out: field 6 carries three items,
/// the §5.1 header claims four (`84`) or two (`82`), and the flat hash matches
/// because the source is rebuilt from those very bytes.
test the_field_six_counterexample_is_the_reported_one() {
  and {
    list.length(field_items(6)) == 3,
    bytearray.take(field_preimage(6), 1) == #"83",
    bytearray.take(miscounted_preimage(6), 1) == #"84",
    bytearray.take(undercounted_preimage(6), 1) == #"82",
    bytearray.length(miscounted_preimage(6)) == bytearray.length(
      field_preimage(6),
    ),
    bytearray.length(undercounted_preimage(6)) == bytearray.length(
      field_preimage(6),
    ),
    // Tier 1 refuses both; `miscount_is_refused_field_6` and
    // `trailing_bytes_are_refused_field_6` above are the standing proof, and
    // field 6 is well under the tier-3 boundary.
    bytearray.length(field_preimage(6)) < chunk_bytes_k,
  }
}

test tier_three_refuses_the_field_six_miscount_counterexample() fail {
  field_total_length(one_chunk_certified_view(6, miscounted_preimage(6))) >= 0
}

test tier_three_refuses_the_field_six_undercount_counterexample() fail {
  field_total_length(one_chunk_certified_view(6, undercounted_preimage(6))) >= 0
}

/// One item of `field_index` with its §5.1 byte-string wrapper, taken from the
/// producer rather than restated.
fn wrapped_item(field_index: Int) -> ByteArray {
  bytearray.drop(encode_field_preimage([field_item(field_index, 1)]), 1)
}

/// The smallest whole number of items that carries `field_index` past
/// `chunk_bytes_k`, so tier 3 is the admissible carriage and exactly two
/// chunks are needed.
fn over_k_item_count(field_index: Int) -> Int {
  chunk_bytes_k / bytearray.length(wrapped_item(field_index)) + 1
}

fn over_k_preimage(field_index: Int) -> ByteArray {
  let count = over_k_item_count(field_index)
  bytearray.concat(
    encode_field_array_header(count),
    repeat_bytes(wrapped_item(field_index), count),
  )
}

fn two_chunk_certified_view(
  field_index: Int,
  preimage: ByteArray,
) -> FieldViewV1 {
  let chunks = [
    bytearray.take(preimage, chunk_bytes_k),
    bytearray.drop(preimage, chunk_bytes_k),
  ]
  expect [chunk_0, chunk_1] = chunks
  view_of(
    field_index,
    preimage,
    Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] },
    [
      certificate_for(0, field_index, preimage, chunks),
      input_of(1, key_output(builtin.b_data(chunk_0))),
      input_of(2, key_output(builtin.b_data(chunk_1))),
    ],
  )
}

/// Every field really does clear the boundary here, so the tier-3 vectors
/// below are testing behaviour above it rather than the boundary again.
test over_k_preimages_clear_the_tier_three_boundary() {
  let clears = fn(field_index: Int) -> Bool {
    let length = bytearray.length(over_k_preimage(field_index))
    and {
      length > chunk_bytes_k,
      length <= 2 * chunk_bytes_k,
    }
  }
  and {
    clears(0),
    clears(1),
    clears(2),
    clears(3),
    clears(4),
    clears(5),
    clears(6),
    clears(7),
    clears(8),
  }
}

/// §7.4/§8.6 in force: a fixed-stride field's tier-3 count is the count of the
/// items actually committed, derived from the mint-verified `total_length`
/// with no chunk hash spent.
fn fixed_stride_count_agrees(field_index: Int) -> Bool {
  field_item_count(
    two_chunk_certified_view(field_index, over_k_preimage(field_index)),
  ) == over_k_item_count(field_index)
}

test tier_three_count_is_authenticated_field_0() {
  fixed_stride_count_agrees(0)
}

test tier_three_count_is_authenticated_field_1() {
  fixed_stride_count_agrees(1)
}

test tier_three_count_is_authenticated_field_3() {
  fixed_stride_count_agrees(3)
}

test tier_three_count_is_authenticated_field_4() {
  fixed_stride_count_agrees(4)
}

test tier_three_count_is_authenticated_field_7() {
  fixed_stride_count_agrees(7)
}

/// The other four fields are variable-width: above the boundary their count
/// exists only in the §5.1 header, nothing affordable authenticates it, and
/// the door aborts rather than hand back a number nobody checked.
fn variable_width_count(field_index: Int) -> Bool {
  field_item_count(
    two_chunk_certified_view(field_index, over_k_preimage(field_index)),
  ) >= 0
}

test tier_three_refuses_the_count_of_field_2() fail {
  variable_width_count(2)
}

test tier_three_refuses_the_count_of_field_5() fail {
  variable_width_count(5)
}

test tier_three_refuses_the_count_of_field_6() fail {
  variable_width_count(6)
}

test tier_three_refuses_the_count_of_field_8() fail {
  variable_width_count(8)
}

/// Refusing the count does not close the field: reads still serve. The walk
/// is what bounds them, and it fails closed the moment it leaves the
/// committed bytes.
test tier_three_variable_width_reads_still_serve() {
  let reads = fn(field_index: Int) -> Bool {
    field_item_at(
      two_chunk_certified_view(field_index, over_k_preimage(field_index)),
      0,
    ) == field_item(field_index, 1)
  }
  and {
    reads(2),
    reads(5),
    reads(6),
    reads(8),
  }
}

/// The one asymmetry §8.6 leaves standing, recorded rather than assumed. A
/// fixed-stride field's tier-3 count comes from the certificate's
/// mint-verified `total_length`, so the §5.1 header is never read and a
/// header that miscounts cannot be detected — but it also cannot be believed:
/// the answer is the true item count, never the inflated one. Tier 1 refuses
/// the same bytes. Closing the gap would cost one chunk hash on the cheapest
/// path in the format, against §8.6's explicit "no chunk hash spent"
/// guarantee for the tier-3 count.
fn miscounted_over_k_preimage(field_index: Int) -> ByteArray {
  bytearray.push(bytearray.drop(over_k_preimage(field_index), 1), 0x84)
}

test tier_three_fixed_stride_count_ignores_a_miscounted_header() {
  field_item_count(
    two_chunk_certified_view(3, miscounted_over_k_preimage(3)),
  ) == over_k_item_count(3)
}

test tier_one_refuses_that_same_miscounted_header() fail {
  field_item_count(inline_view(3, miscounted_over_k_preimage(3))) >= 0
}

test whole_and_chunked_views_agree_on_the_same_bytes() {
  let preimage = straddle_preimage()
  let chunks = straddle_chunks()
  let chunked = Chunked {
    chunks,
    chunk_digests: list.map(chunks, blake2b_256),
    count: straddle_item_count,
    stride: hash28_stride,
  }
  let whole = Whole {
    bytes: preimage,
    count: straddle_item_count,
    stride: hash28_stride,
  }
  and {
    field_item_at(chunked, 529) == field_item_at(whole, 529),
    field_read_range(chunked, 15898, 6) == field_read_range(whole, 15898, 6),
  }
}
