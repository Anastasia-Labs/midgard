use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
  native_tx_proof_commitment_v1,
}
use midgard/fraud_proofs/native_tx/preimages.{
  encode_address_witness_preimage, encode_hash28_list_preimage,
  encode_input_preimage,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, NativeTxBodyCompact,
  NativeTxCompact, NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  TxIsValid, native_tx_version_v1,
}
use midgard/ledger_state.{
  CekProgramMaterialDatumV1, ForcedInclusionTxV1, NativeTxProofSourceV1,
  TxOrderEventV1, TxOrderPayloadV1,
}
use midgard/native_tx_field_access_v1.{
  Certified, FieldCarriageV1, FieldPreimageCertificateV1, Inline, RawUtxo,
  chunk_bytes_k, empty_field_commitment, encode_field_array_header,
  encode_field_preimage, expected_chunk_count, field_commitment, field_count,
  field_preimage_certificate_asset_name, field_stride, max_tier3_chunk_count,
  max_transaction_aggregate_field_bytes, spend_input_stride, walk_derived_stride,
}
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/tx_order_v1.{
  Datum, MintRedeemer, SpendRedeemer, forced_inclusion_key_value,
  material_carriage_matches_event, verify_order_material,
}

const order_transaction_id =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const commitment_transaction_id =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const input_transaction_id =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const signer = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

/// A required-observer hash, deliberately the **same width** as `signer` and a
/// different value. Fields 3 and 4 are the two slots §5.3 gives `hash28_stride`,
/// so an observer-list preimage and a signer-list preimage of equal item count
/// are structurally interchangeable and differ only in the bytes §4 hashes. That
/// is what lets the negatives below isolate the commitment check from §7.4's
/// count arithmetic.
const observer = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn vector_order_id() -> OutputReference {
  OutputReference {
    transaction_id: #"3333333333333333333333333333333333333333333333333333333333333333",
    output_index: 4,
  }
}

fn vector_source() -> NativeTxProofSourceV1 {
  NativeTxProofSourceV1 {
    compact_cbor: #"01",
    witness_set_compact_cbor: #"0203",
    field_preimage_lengths_cbor: #"04",
  }
}

fn vector_payload() -> TxOrderPayloadV1 {
  TxOrderPayloadV1 {
    tx_id: order_transaction_id,
    transaction_commitment: commitment_transaction_id,
    source: vector_source(),
  }
}

fn vector_event() -> TxOrderEventV1 {
  TxOrderEventV1 { id: vector_order_id(), tx: vector_payload() }
}

fn vector_datum() -> Datum {
  user_events.OptimisticDatum {
    event: vector_event(),
    inclusion_time: 123,
    witness: #"66666666666666666666666666666666666666666666666666666666",
    refund_address: address.from_verification_key(
      #"77777777777777777777777777777777777777777777777777777777",
    ),
    refund_datum: NoDatum,
  }
}

fn vector_forced_inclusion() -> ForcedInclusionTxV1 {
  ForcedInclusionTxV1 {
    tx_id: order_transaction_id,
    source: vector_source(),
    operator_validity: ledger_state.FailedScript,
  }
}

fn vector_spend_redeemer() -> SpendRedeemer {
  SpendRedeemer {
    input_index: 0,
    output_index: 1,
    hub_ref_input_index: 2,
    settlement_ref_input_index: 3,
    burn_redeemer_index: 4,
    membership_proof: transition_trace.RootMembershipProof {
      domain: transition_trace.ForcedTransactionsV1RootDomain,
      root: zero,
      phas_root: #"1111111111111111111111111111111111111111111111111111111111111111",
      count: 1,
      key: cbor.serialise(vector_order_id()),
      value: cbor.serialise(vector_forced_inclusion()),
      proof: [],
    },
    inclusion_proof_script_withdraw_redeemer_index: 5,
    validity_override: ledger_state.FailedScript,
  }
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero,
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

fn sample_v1_transaction(include_signer: Bool) -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    body: MidgardTransactionBody {
      ..empty.body,
      inputs: if include_signer {
        []
      } else {
        [MidgardTxInput { tx_id: input_transaction_id, output_index: 3 }]
      },
      required_signers: if include_signer {
        [signer]
      } else {
        []
      },
    },
  }
}

/// Material in two body slots at once, so a carriage vector has to line up with
/// two of the nine commitments rather than one.
fn spend_input_and_signer_v1_transaction() -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    body: MidgardTransactionBody {
      ..empty.body,
      inputs: [MidgardTxInput { tx_id: input_transaction_id, output_index: 3 }],
      required_signers: [signer],
    },
  }
}

/// Material in fields 3 and 4 — the two slots §5.3 gives the **same** 30-byte
/// `hash28_stride`. Both preimages are one-item hash-28 lists, so each is a
/// perfectly well-formed preimage at the *other's* slot: identical header,
/// identical item count, identical total length. Only §4's flat commitment can
/// tell them apart, which is exactly the property the swap and wrong-bytes
/// negatives need in order to be refuted by neutering that commitment check and
/// nothing else.
fn observer_and_signer_v1_transaction() -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    body: MidgardTransactionBody {
      ..empty.body,
      required_observers: [observer],
      required_signers: [signer],
    },
  }
}

/// Material in field 7, which lives in the witness set rather than the body.
fn address_witness_v1_transaction(
  witness: MidgardAddressWitness,
) -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    witness_set: MidgardTransactionWitnessSet {
      ..empty.witness_set,
      addr_tx_wits: [witness],
    },
  }
}

fn tx_id(tx: MidgardTransaction) -> ByteArray {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
}

fn source(tx: MidgardTransaction) -> NativeTxProofSourceV1 {
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  NativeTxProofSourceV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
  }
}

fn commitment(tx: MidgardTransaction) -> ByteArray {
  native_transaction.midgard_transaction_proof_commitment_v1(tx)
}

fn payload(tx: MidgardTransaction) -> TxOrderPayloadV1 {
  TxOrderPayloadV1 {
    tx_id: tx_id(tx),
    transaction_commitment: commitment(tx),
    source: source(tx),
  }
}

test tx_order_v1_matches_the_canonical_typescript_abi_vectors() {
  and {
    cbor.serialise(vector_order_id()) == #"d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff",
    cbor.serialise(vector_payload()) == #"d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffff",
    cbor.serialise(vector_event()) == #"d8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffffff",
    cbor.serialise(vector_datum()) == #"d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffffff187b581c66666666666666666666666666666666666666666666666666666666d8799fd8799f581c77777777777777777777777777777777777777777777777777777777ffd87a80ffd87980ff",
    cbor.serialise(vector_forced_inclusion()) == #"d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff",
    cbor.serialise(vector_spend_redeemer()) == #"d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff5834d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff80ff05d87c80ff",
  }
}

test forced_inclusion_key_value_matches_the_canonical_v1_vectors() {
  let key, value <- forced_inclusion_key_value(
    tx_order_event_data: vector_event(),
    validity_override: ledger_state.FailedScript,
  )
  and {
    cbor.serialise(key) == #"d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff",
    cbor.serialise(value) == #"d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff",
  }
}

test tx_order_payload_rejects_an_unknown_outer_constructor() fail {
  expect Some(data) = cbor.deserialise(
    #"d87a9f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffff",
  )
  expect payload: TxOrderPayloadV1 = data
  payload.tx_id == order_transaction_id
}

test tx_order_payload_rejects_an_extra_field() fail {
  expect Some(data) = cbor.deserialise(
    #"d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ff00ff",
  )
  expect payload: TxOrderPayloadV1 = data
  payload.tx_id == order_transaction_id
}

test cek_program_material_v1_matches_the_typed_blob_chunk_hash_vector() {
  let chunk = #"6d6174657269616c"
  let preimage = #"486d6174657269616c"
  let root = cek_proof_v1.hash_blob_chunk_v1(chunk)
  let datum = CekProgramMaterialDatumV1 { kind: 3, root, preimage }
  and {
    root == #"941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c",
    cbor.serialise(datum) == #"d8799f035820941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c49486d6174657269616cff",
  }
}

// ---------------------------------------------------------------------------
// §8 forced-order material carriage (#594)
// ---------------------------------------------------------------------------

const certificate_policy_id: PolicyId =
  #"cccccccccccccccccccccccccccccccccccccccccccccccc"

const creator_key = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

fn material(
  tx: MidgardTransaction,
  material_carriage: List<FieldCarriageV1>,
  reference_inputs: List<Input>,
) -> Bool {
  verify_order_material(
    payload(tx),
    material_carriage,
    reference_inputs,
    certificate_policy_id,
  )
}

/// A §8.5 raw carriage UTxO: nothing but bytes, as an inline datum at the order
/// creator's own wallet address. Nothing about this output is authenticated —
/// the door hashes its content, which is the whole point of §8.5.
fn creator_publication(index: Int, bytes: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"9999999999999999999999999999999999999999999999999999999999999999",
      output_index: index,
    },
    output: Output {
      address: address.from_verification_key(creator_key),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(builtin.b_data(bytes)),
      reference_script: None,
    },
  }
}

/// The §8.6 manifest UTxO, carrying the policy's `(tx_id, field_index)` token.
/// The certificate's own mint proved `field_commitment(concat(chunks))` against
/// the committed field hash; what the door does here is require that token to be
/// present before it will read a chunk vector at all.
fn certificate_publication(
  index: Int,
  tx: MidgardTransaction,
  field_index: Int,
  total_length: Int,
  chunk_digests: List<ByteArray>,
) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"8888888888888888888888888888888888888888888888888888888888888888",
      output_index: index,
    },
    output: Output {
      address: address.from_script(certificate_policy_id),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            certificate_policy_id,
            field_preimage_certificate_asset_name(tx_id(tx), field_index),
            1,
          ),
      datum: InlineDatum(
        FieldPreimageCertificateV1 {
          owner: creator_key,
          tx_id: tx_id(tx),
          field_index,
          total_length,
          chunk_digests,
        },
      ),
      reference_script: None,
    },
  }
}

fn spend_input_field_preimage() -> ByteArray {
  encode_input_preimage(
    [MidgardTxInput { tx_id: input_transaction_id, output_index: 3 }],
  )
}

fn signer_field_preimage() -> ByteArray {
  encode_hash28_list_preimage([signer])
}

/// Field 3's preimage. Byte-for-byte the same shape and length as
/// `signer_field_preimage`, and a different value — see `observer`.
fn observer_field_preimage() -> ByteArray {
  encode_hash28_list_preimage([observer])
}

fn authenticate_event() -> user_events.MintRedeemer {
  user_events.AuthenticateEvent {
    nonce_input_index: 0,
    event_output_index: 1,
    hub_ref_input_index: 2,
    witness_registration_redeemer_index: 3,
  }
}

fn burn_event() -> user_events.MintRedeemer {
  user_events.BurnEventNFT {
    nonce_asset_name: #"aabb",
    witness_unregistration_redeemer_index: 4,
  }
}

/// §2.4's declared vector for the canonically-empty transaction: nine
/// one-byte `80` headers.
fn all_empty_declared_lengths() -> NativeTxFieldPreimageLengthsV1 {
  NativeTxFieldPreimageLengthsV1 {
    spend_inputs: 1,
    reference_inputs: 1,
    outputs: 1,
    required_observers: 1,
    required_signers: 1,
    mint: 1,
    address_witnesses: 1,
    script_witnesses: 1,
    redeemers: 1,
  }
}

/// A payload carrying `tx`'s real compact structures beside a **substituted**
/// §2.4 declared length vector, with `transaction_commitment` recomputed over the
/// substitution. The payload is therefore internally consistent everywhere except
/// in the one claim under test, so a refusal cannot be blamed on the commitment.
fn payload_with_declared_lengths(
  tx: MidgardTransaction,
  lengths: NativeTxFieldPreimageLengthsV1,
) -> TxOrderPayloadV1 {
  let honest = source(tx)
  let substituted = NativeTxProofSourceV1 {
    compact_cbor: honest.compact_cbor,
    witness_set_compact_cbor: honest.witness_set_compact_cbor,
    field_preimage_lengths_cbor: encode_native_tx_field_preimage_lengths_v1(
      lengths,
    ),
  }
  TxOrderPayloadV1 {
    tx_id: tx_id(tx),
    transaction_commitment: native_tx_proof_commitment_v1(
      substituted.compact_cbor,
      substituted.witness_set_compact_cbor,
      substituted.field_preimage_lengths_cbor,
    ),
    source: substituted,
  }
}

// -- Tier-independent: emptiness and exhaustion -----------------------------

/// Nine empty fields need no carriage, and the vector is empty because nothing
/// consumed an entry.
test empty_material_activates_the_order() {
  material(empty_v1_transaction(), [], [])
}

/// Exhaustion is checked in both directions. A spare entry would let two
/// distinct redeemers authenticate one order, the second naming reference inputs
/// this walk never read.
test empty_material_refuses_a_spare_carriage_entry() fail {
  material(empty_v1_transaction(), [Inline { preimage: #"80" }], [])
}

/// The other direction: a field with material and no entry left to spend on it.
/// This is what stood here before #594 as the whole of the predicate — material
/// meant refusal — and it survives as the missing-carriage case.
test material_bearing_order_fails_closed_on_a_body_field() fail {
  material(sample_v1_transaction(False), [], [])
}

test material_bearing_order_fails_closed_on_a_signer_field() fail {
  material(sample_v1_transaction(True), [], [])
}

// -- Tier 1: inline, in the order transaction's own mint redeemer -----------

/// Field 0 (spend inputs), body-side.
test inline_carriage_authenticates_a_body_field() {
  material(
    sample_v1_transaction(False),
    [Inline { preimage: spend_input_field_preimage() }],
    [],
  )
}

/// Field 4 (required signers), also body-side but a different slot and a
/// different §5.3 stride, so the positional extraction is exercised twice.
test inline_carriage_authenticates_a_signer_field() {
  material(
    sample_v1_transaction(True),
    [Inline { preimage: signer_field_preimage() }],
    [],
  )
}

/// Field 7 (address witnesses) is read out of the **witness set**, whose hash is
/// committed by the compact body rather than by the tx-id preimage. The door's
/// prologue re-derives it for fields 6–8 only, so a witness-set field is the one
/// that proves the pairing is checked and not assumed (§2.5, §4).
test inline_carriage_authenticates_a_witness_set_field() {
  let witness = MidgardAddressWitness {
    verification_key: commitment_transaction_id,
    signature: bytearray.concat(order_transaction_id, order_transaction_id),
  }
  material(
    address_witness_v1_transaction(witness),
    [Inline { preimage: encode_address_witness_preimage([witness]) }],
    [],
  )
}

/// Two fields with material consume two entries, and the order is the ascending
/// field order the walk reads the nine commitments in.
test inline_carriage_authenticates_two_fields_in_ascending_field_order() {
  material(
    spend_input_and_signer_v1_transaction(),
    [
      Inline { preimage: spend_input_field_preimage() },
      Inline { preimage: signer_field_preimage() },
    ],
    [],
  )
}

/// The same two entries at fields 3 and 4, the two slots that share §5.3's
/// 30-byte stride. The positive counterpart of the swap negative below, and it
/// exists so that negative's only difference from a passing case is the order.
test inline_carriage_authenticates_two_same_stride_fields() {
  material(
    observer_and_signer_v1_transaction(),
    [
      Inline { preimage: observer_field_preimage() },
      Inline { preimage: signer_field_preimage() },
    ],
    [],
  )
}

/// Positional, therefore order-sensitive: §4 removed field-index domain
/// separation, so nothing in the bytes says which slot a preimage came from and
/// only the position in this vector does. Swapping the two entries offers
/// field 4's preimage against field 3's commitment.
///
/// **Fields 3 and 4 deliberately, not 0 and 4.** Both are `hash28_stride`, so
/// each swapped preimage still satisfies `whole_view`'s §7.4 count arithmetic and
/// the walk's declared-length check at the slot it lands in — the *only* thing
/// left to refuse it is the §4 commitment comparison. Across two different
/// strides the arithmetic would refuse first (a 31-byte hash-28 preimage cannot
/// be `1 + 40·1` bytes at field 0), and this negative would survive deleting the
/// commitment check altogether. Mutation-verified in both spellings.
test inline_carriage_supplied_out_of_field_order_is_refused() fail {
  material(
    observer_and_signer_v1_transaction(),
    [
      Inline { preimage: signer_field_preimage() },
      Inline { preimage: observer_field_preimage() },
    ],
    [],
  )
}

/// Tier 1's wrong-bytes rejection: a perfectly well-formed §5.1 field preimage
/// for a *different* spend input does not hash to this order's commitment.
test inline_carriage_with_wrong_bytes_is_refused() fail {
  material(
    sample_v1_transaction(False),
    [
      Inline {
        preimage: encode_input_preimage(
          [MidgardTxInput { tx_id: input_transaction_id, output_index: 4 }],
        ),
      },
    ],
    [],
  )
}

// -- Tier 2: one predeployed raw UTxO at the creator's wallet ---------------

test raw_utxo_carriage_authenticates_from_a_reference_input() {
  material(
    sample_v1_transaction(False),
    [RawUtxo { ref_input_index: 1 }],
    [
      creator_publication(0, #"deadbeef"),
      creator_publication(1, spend_input_field_preimage()),
    ],
  )
}

/// Tier 2's wrong-bytes rejection. §8.5 is explicit that raw carriage is
/// unauthenticated data whose provenance nobody trusts, so the refusal has to
/// come from the hash and from nothing else — the output below is at the same
/// address, with the same shape, and holds the wrong field's preimage.
///
/// "From nothing else" is load-bearing, so the wrong preimage is field 3's
/// offered at field 4: same `hash28_stride`, same item count, same total length,
/// so §7.4's arithmetic and the walk's declared-length check both pass and only
/// §4's commitment refuses. Offering a hash-28 preimage at a `spend_input_stride`
/// slot instead would be refused by the arithmetic before the hash was ever
/// compared, and this negative would then survive deleting the comparison.
test raw_utxo_carriage_with_wrong_bytes_is_refused() fail {
  material(
    sample_v1_transaction(True),
    [RawUtxo { ref_input_index: 0 }],
    [creator_publication(0, observer_field_preimage())],
  )
}

/// A reference-input index past the end is a refusal, not a miss.
test raw_utxo_carriage_with_an_absent_reference_input_is_refused() fail {
  material(sample_v1_transaction(False), [RawUtxo { ref_input_index: 7 }], [])
}

// -- Tier 3: chunked + certified, for a field one publication cannot hold ---

/// 505 observers at §5.3's 30-byte stride behind a three-byte header is
/// `3 + 30·505 = 15,153` bytes — just past `chunk_bytes_k`, so it is the
/// smallest required-observers field that tier 3 is the only admissible carriage
/// for. The split is `[15,148, 5]`: two chunks, the last ragged and non-empty.
const tier3_observer_count: Int = 505

fn tier3_observers() -> List<ByteArray> {
  list.range(1, tier3_observer_count)
    |> list.map(
        fn(index) {
          bytearray.drop(signer, 2)
            |> bytearray.push(index % 256)
            |> bytearray.push(index / 256)
        },
      )
}

fn tier3_transaction() -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    body: MidgardTransactionBody {
      ..empty.body,
      required_observers: tier3_observers(),
    },
  }
}

fn tier3_chunks() -> List<ByteArray> {
  let preimage = encode_hash28_list_preimage(tier3_observers())
  [
    bytearray.take(preimage, chunk_bytes_k),
    bytearray.drop(preimage, chunk_bytes_k),
  ]
}

/// The same field with its **last** observer replaced: identical length,
/// identical split shape, different bytes in the ragged tail. Using another
/// field's genuine split rather than a hand-flipped byte keeps the negative
/// honest — the substituted chunk is exactly the kind of thing a publisher
/// really could have at the creator's address.
///
/// The replacement's last five bytes are what has to differ, because chunk 0
/// ends 23 bytes into the 505th observer and the ragged chunk 1 is that item's
/// tail alone. An earlier revision replaced it with `signer`, whose last five
/// bytes are the same `bb` run as the generated observers' — the substitution was
/// a no-op and the negative below passed for the wrong reason.
fn tier3_variant_chunks() -> List<ByteArray> {
  let preimage = encode_hash28_list_preimage(
    list.concat(
      list.take(tier3_observers(), tier3_observer_count - 1),
      [bytearray.concat(bytearray.take(signer, 23), #"0102030405")],
    ),
  )
  [
    bytearray.take(preimage, chunk_bytes_k),
    bytearray.drop(preimage, chunk_bytes_k),
  ]
}

fn tier3_reference_inputs(chunks: List<ByteArray>) -> List<Input> {
  expect [chunk_0, chunk_1] = chunks
  [
    certificate_publication(
      0,
      tier3_transaction(),
      3,
      bytearray.length(chunk_0) + bytearray.length(chunk_1),
      list.map(tier3_chunks(), blake2b_256),
    ),
    creator_publication(1, chunk_0),
    creator_publication(2, chunk_1),
  ]
}

test certified_carriage_authenticates_a_chunked_field() {
  material(
    tier3_transaction(),
    [Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] }],
    tier3_reference_inputs(tier3_chunks()),
  )
}

/// Tier 3's wrong-bytes rejection, and the reason this mint opens the door's
/// **whole-materialising** entry point rather than its lazy one.
///
/// The certificate below is exactly the one a valid certification produced — its
/// digest vector is over the real chunks — and the chunk reference inputs it
/// names are the right length. Only their content differs. Under the lazy tier-3
/// view nothing here would be read, because this mint touches no items, and the
/// order would be authenticated against bytes nobody checked. The materialising
/// form concatenates and hashes once against §4's commitment, so it refuses.
test certified_carriage_with_wrong_chunk_bytes_is_refused() fail {
  expect [chunk_0, chunk_1] = tier3_chunks()
  expect [_variant_chunk_0, variant_chunk_1] = tier3_variant_chunks()
  material(
    tier3_transaction(),
    [Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] }],
    [
      certificate_publication(
        0,
        tier3_transaction(),
        3,
        bytearray.length(chunk_0) + bytearray.length(chunk_1),
        list.map(tier3_chunks(), blake2b_256),
      ),
      creator_publication(1, chunk_0),
      creator_publication(2, variant_chunk_1),
    ],
  )
}

/// §8.4's lower bound keeps the ladder a partition at the top end: a field that
/// fits one publication cannot be re-carried as a one-chunk "certificate", which
/// would otherwise side-step the structural checks tiers 1–2 run at view
/// construction.
test certified_carriage_below_the_tier_three_boundary_is_refused() fail {
  let preimage = spend_input_field_preimage()
  material(
    sample_v1_transaction(False),
    [Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1] }],
    [
      certificate_publication(
        0,
        sample_v1_transaction(False),
        0,
        bytearray.length(preimage),
        [blake2b_256(preimage)],
      ),
      creator_publication(1, preimage),
    ],
  )
}

/// The manifest is authenticated by its token, not by its shape: an inline datum
/// of the right type at the right address without the policy's
/// `(tx_id, field_index)` token is not a certificate.
test certified_carriage_without_the_policy_token_is_refused() fail {
  expect [chunk_0, chunk_1] = tier3_chunks()
  let certificate = certificate_publication(
    0,
    tier3_transaction(),
    3,
    bytearray.length(chunk_0) + bytearray.length(chunk_1),
    list.map(tier3_chunks(), blake2b_256),
  )
  let untokened = Input {
    ..certificate,
    output: Output {
      ..certificate.output,
      value: assets.from_lovelace(2_000_000),
    },
  }
  material(
    tier3_transaction(),
    [Certified { cert_ref_input_index: 0, chunk_ref_input_indices: [1, 2] }],
    [
      untokened,
      creator_publication(1, chunk_0),
      creator_publication(2, chunk_1),
    ],
  )
}

// -- The declared length vector -------------------------------------------

/// §2.4's declared per-field lengths and the authenticated preimage's own length
/// are two independently committed statements about one field, and §4's flat hash
/// does not collapse them. The validation machine renders that disagreement as
/// `reject_field_preimage_size`; for a forced order it is the creator's own
/// payload contradicting itself, so the mint fails closed.
///
/// The commitment is recomputed over the tampered vector, so the payload is
/// internally consistent everywhere except in the one claim under test.
test declared_field_length_disagreeing_with_the_material_is_refused() fail {
  let base = all_empty_declared_lengths()
  verify_order_material(
    payload_with_declared_lengths(
      sample_v1_transaction(False),
      NativeTxFieldPreimageLengthsV1 {
        ..base,
        spend_inputs: bytearray.length(spend_input_field_preimage()) + 1,
      },
    ),
    [Inline { preimage: spend_input_field_preimage() }],
    [],
    certificate_policy_id,
  )
}

/// The **empty** slot's half of the same rule, which is a separate branch of the
/// walk and needs its own refutation.
///
/// §5.1's empty field is the one-byte header `80`, so an empty slot's declared
/// length is 1. The walk asserts that rather than skipping the slot, and nothing
/// downstream would catch a violation: no carriage entry is consumed at an empty
/// slot, so the §8.8 door never runs there and the declared vector's claim about
/// that slot would go entirely unchecked despite sitting inside
/// `transaction_commitment`. An order declaring two bytes for a field whose
/// commitment says it is empty contradicts itself, and the mint fails closed.
///
/// Field 1 (reference inputs) is empty in the canonically-empty transaction, and
/// it is *after* field 0, so a walk that only checked the first slot would still
/// let this through.
test declared_length_of_an_empty_field_other_than_one_is_refused() fail {
  let base = all_empty_declared_lengths()
  verify_order_material(
    payload_with_declared_lengths(
      empty_v1_transaction(),
      NativeTxFieldPreimageLengthsV1 { ..base, reference_inputs: 2 },
    ),
    [],
    [],
    certificate_policy_id,
  )
}

// -- The burn's empty-vector rule ------------------------------------------

/// A burn reads no material, so it must carry no vector rather than carry one
/// nothing looks at: an unread wire field is where a second spelling of the same
/// transaction hides (§6.1).
///
/// The rule is the mint validator's, and the mint validator has no test harness —
/// which is why it is a library function rather than an inline `when` in
/// `validators/user-events/tx-order-v1.ak`. Testing it here is testing the rule
/// the validator applies, not a copy of it.
test burn_carries_no_material_carriage_vector() {
  material_carriage_matches_event(burn_event(), [])
}

test burn_carrying_a_material_carriage_vector_is_refused() fail {
  material_carriage_matches_event(burn_event(), [Inline { preimage: #"80" }])
}

/// The other side: an authenticating mint's vector is deliberately *not*
/// pre-judged here, at either length. `verify_order_material` is what consumes
/// and exhausts it against the nine commitments, and a length rule here would be
/// a second, weaker copy of that exhaustion rule.
test authenticating_mint_is_not_pre_judged_on_its_carriage_vector() {
  and {
    material_carriage_matches_event(authenticate_event(), []),
    material_carriage_matches_event(
      authenticate_event(),
      [Inline { preimage: #"80" }],
    ),
  }
}

// -- The mint redeemer's own wire shape ------------------------------------

/// The tx-order mint redeemer wraps the shared user-event redeemer rather than
/// widening it, so its wire form is `Constr 0 [<user event redeemer>, <list>]`.
/// Pinned here because off-chain builders emit exactly these bytes.
test tx_order_mint_redeemer_wire_form_is_the_event_plus_the_carriage_vector() {
  and {
    cbor.serialise(
      MintRedeemer { event: authenticate_event(), material_carriage: [] },
    ) == #"d8799fd8799f00010203ff80ff",
    cbor.serialise(MintRedeemer { event: burn_event(), material_carriage: [] }) == #"d8799fd87a9f42aabb04ff80ff",
    cbor.serialise(
      MintRedeemer {
        event: authenticate_event(),
        material_carriage: [
          Inline { preimage: #"80" },
          RawUtxo { ref_input_index: 5 },
          Certified { cert_ref_input_index: 6, chunk_ref_input_indices: [7, 8] },
        ],
      },
    ) == #"d8799fd8799f00010203ff9fd8799f4180ffd87a9f05ffd87b9f069f0708ffffffff",
  }
}

// ---------------------------------------------------------------------------
// §8.11's published cost claim (#594)
// ---------------------------------------------------------------------------
//
// §8.11 publishes a cost for this mint — "§12.5's tier-2 per-step
// full-preimage re-hash, bounded by §5.4's 32,768-byte aggregate over all nine
// fields" — and a published cost claim gets a pin, per the §8.10 precedent.
// `scripts/verify-tx-order-mint-exec-ledger-v1.mjs` takes the rows; the
// fixtures below are the two shapes the claim splits into, and
// `tx_order_mint_material_fixture_sits_at_the_aggregate_bound` is what stops
// either row quietly becoming a cheaper one.
//
// The split is `whole_view`'s. §5.1 count consistency is checked by arithmetic
// for the five fixed-stride fields (0, 1, 3, 4, 7) and by a full `walk_to_end`
// over every item for the four variable-width ones (2, 5, 6, 8), so the claim
// holds as written on one half of §2.5 and understates the other half by a
// per-item term the ledger's note prices.

/// `block` repeated `times` over, built by doubling.
///
/// `encode_field_preimage` over a four-figure item count folds that many
/// `bytearray.concat`s — quadratic in the preimage it builds, and it would put
/// far more into a measured row's fixture than the mint spends authenticating
/// the result. This builds the same bytes with `log2(times)` concatenations,
/// and the shape selector below proves the two agree at a width where the
/// quadratic fold is affordable to run beside it.
fn repeated_bytes(block: ByteArray, times: Int) -> ByteArray {
  if times <= 0 {
    #""
  } else if times == 1 {
    block
  } else {
    let half = repeated_bytes(block, times / 2)
    let doubled = bytearray.concat(half, half)
    if times % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, block)
    }
  }
}

/// One §5.3 field-0 item — `82 ‖ 58 20 tx_id ‖ 19 index_be16` behind its
/// `58 26` wrapper — at the 40-byte stride the door checks field 0 by.
const spend_input_item_block: ByteArray =
  #"58268258200000000000000000000000000000000000000000000000000000000000000000190003"

/// One one-byte item behind §5.1's one-byte header: the minimum non-empty
/// preimage a variable-width slot can carry.
const minimal_variable_width_preimage: ByteArray = #"814100"

/// The largest field 0 §5.4's aggregate bound admits once the other eight slots
/// sit at their minimum non-empty width: `3 + 40·813 = 32,523` bytes, which puts
/// the nine-field aggregate at 32,742 and leaves no room for an 814th item.
const aggregate_bound_spend_input_count: Int = 813

/// The two cardinalities the variable-width rows are read at. Both are inside
/// the §3.3 basis and the pair is what makes the per-item cost falsifiable: a
/// walk that got dearer per item moves the two rows apart, and the ledger's
/// note carries the rate they imply.
const lower_variable_width_item_count: Int = 250

const upper_variable_width_item_count: Int = 500

/// `count` items at §5.3's field-0 stride, behind §5.1's minimal header.
fn fixed_stride_preimage(count: Int) -> ByteArray {
  bytearray.concat(
    encode_field_array_header(count),
    repeated_bytes(spend_input_item_block, count),
  )
}

/// `count` one-byte items behind §5.1's minimal header — the densest shape a
/// variable-width field admits at a given byte count.
///
/// Field 8 is variable-width under §5.3, so `whole_view` cannot check §5.1's
/// count by arithmetic and walks every item instead: one `field_read_range` and
/// one item head per item. The door never decodes an item's *content*, so this
/// shape is admissible to this mint whatever the field's own grammar says about
/// it, and minimum-width items maximise the walk per byte carried.
fn variable_width_preimage(count: Int) -> ByteArray {
  bytearray.concat(
    encode_field_array_header(count),
    repeated_bytes(#"4100", count),
  )
}

/// §5.3's 103-byte stride at one item: field 7's minimum non-empty width.
fn order_shape_address_witness() -> MidgardAddressWitness {
  MidgardAddressWitness {
    verification_key: commitment_transaction_id,
    signature: bytearray.concat(order_transaction_id, order_transaction_id),
  }
}

/// The nine slots at their minimum non-empty width, with `slot` widened to
/// `filler`. Nine non-empty fields is what makes the walk open the door nine
/// times, and minimum everywhere else is what leaves the whole of §5.4's bound
/// to the one slot under measurement.
fn nine_field_preimages(slot: Int, filler: ByteArray) -> List<ByteArray> {
  let minimal = [
    spend_input_field_preimage(),
    spend_input_field_preimage(),
    minimal_variable_width_preimage,
    signer_field_preimage(),
    signer_field_preimage(),
    minimal_variable_width_preimage,
    minimal_variable_width_preimage,
    encode_address_witness_preimage([order_shape_address_witness()]),
    minimal_variable_width_preimage,
  ]
  list.indexed_map(
    minimal,
    fn(index, preimage) {
      if index == slot {
        filler
      } else {
        preimage
      }
    },
  )
}

/// Everything a measured row hands `verify_order_material`, built once.
///
/// A record rather than three functions because the tier-3 certificate's asset
/// name is derived from the fixture's own transaction id: reaching that id from
/// a separate function would hash the widened field a second time and charge it
/// to the wrong row.
type OrderMaterialShapeV1 {
  payload: TxOrderPayloadV1,
  material_carriage: List<FieldCarriageV1>,
  reference_inputs: List<Input>,
}

/// Builds the payload and the carriage for nine non-empty preimages, carrying
/// `certified_slot` through tier 3 and every other slot inline. A negative
/// `certified_slot` carries all nine inline and names no reference input.
///
/// The compact structures are built here rather than reached from a
/// `MidgardTransaction` through `payload`, because a shape defined by its nine
/// *preimages* has no item lists to build it from that cost less than the mint
/// costs to run. Nothing about the payload is weaker for it:
/// `verify_order_material` re-derives the transaction id from the compact body,
/// re-derives the §2.5 witness-set pairing and re-derives the §2.4 length
/// vector, so a fixture that got any of the three wrong would fail rather than
/// measure.
fn order_material_shape(
  preimages: List<ByteArray>,
  certified_slot: Int,
) -> OrderMaterialShapeV1 {
  expect
      [
        spend_inputs, reference_inputs_field, outputs, required_observers,
        required_signers, mint, script_witnesses, address_witnesses, redeemers,
      ]
    = preimages
  let body = NativeTxBodyCompact {
    spend_inputs_hash: field_commitment(spend_inputs),
    reference_inputs_hash: field_commitment(reference_inputs_field),
    outputs_hash: field_commitment(outputs),
    fee: 0,
    validity_interval_start: -1,
    validity_interval_end: -1,
    required_observers_hash: field_commitment(required_observers),
    required_signers_hash: field_commitment(required_signers),
    mint_hash: field_commitment(mint),
    script_integrity_hash: zero,
    auxiliary_data_hash: zero,
    network_id: 255,
  }
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: field_commitment(address_witnesses),
    script_tx_wits_hash: field_commitment(script_witnesses),
    redeemer_tx_wits_hash: field_commitment(redeemers),
  }
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    witness_set,
  )
  let compact_cbor = encode_native_tx_compact_v1(
    NativeTxCompact {
      body,
      witness_set_hash: blake2b_256(witness_set_compact_cbor),
      validity_code: 0,
    },
  )
  let field_preimage_lengths_cbor = encode_native_tx_field_preimage_lengths_v1(
    NativeTxFieldPreimageLengthsV1 {
      spend_inputs: bytearray.length(spend_inputs),
      reference_inputs: bytearray.length(reference_inputs_field),
      outputs: bytearray.length(outputs),
      required_observers: bytearray.length(required_observers),
      required_signers: bytearray.length(required_signers),
      mint: bytearray.length(mint),
      address_witnesses: bytearray.length(address_witnesses),
      script_witnesses: bytearray.length(script_witnesses),
      redeemers: bytearray.length(redeemers),
    },
  )
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(body),
  )
  let reference_inputs = if certified_slot < 0 {
    []
  } else {
    expect Some(certified) = list.at(preimages, certified_slot)
    let chunk_0 = bytearray.take(certified, chunk_bytes_k)
    let chunk_1 = bytearray.take(
      bytearray.drop(certified, chunk_bytes_k),
      chunk_bytes_k,
    )
    let chunk_2 = bytearray.drop(certified, 2 * chunk_bytes_k)
    [
      order_shape_certificate(
        tx_id,
        certified_slot,
        [chunk_0, chunk_1, chunk_2],
      ),
      creator_publication(1, chunk_0),
      creator_publication(2, chunk_1),
      creator_publication(3, chunk_2),
    ]
  }
  OrderMaterialShapeV1 {
    payload: TxOrderPayloadV1 {
      tx_id,
      transaction_commitment: native_tx_proof_commitment_v1(
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
      ),
      source: NativeTxProofSourceV1 {
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
      },
    },
    material_carriage: list.indexed_map(
      preimages,
      fn(index, preimage) {
        if index == certified_slot {
          Certified {
            cert_ref_input_index: 0,
            chunk_ref_input_indices: [1, 2, 3],
          }
        } else {
          Inline { preimage }
        }
      },
    ),
    reference_inputs,
  }
}

/// The §8.6 manifest for the widened slot, at §8.3's maximum chunk count.
fn order_shape_certificate(
  tx_id: ByteArray,
  field_index: Int,
  chunks: List<ByteArray>,
) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"8888888888888888888888888888888888888888888888888888888888888888",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(certificate_policy_id),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            certificate_policy_id,
            field_preimage_certificate_asset_name(tx_id, field_index),
            1,
          ),
      datum: InlineDatum(
        FieldPreimageCertificateV1 {
          owner: creator_key,
          tx_id,
          field_index,
          total_length: list.foldl(
            chunks,
            0,
            fn(chunk, total) { total + bytearray.length(chunk) },
          ),
          chunk_digests: list.map(chunks, blake2b_256),
        },
      ),
      reference_script: None,
    },
  }
}

/// §8.11's claim at §5.4's aggregate bound: nine non-empty fields, 32,742 bytes
/// in total, all the spare width in fixed-stride field 0 and carried through
/// tier 3 at §8.3's maximum chunk count.
fn aggregate_bound_shape() -> OrderMaterialShapeV1 {
  order_material_shape(
    nine_field_preimages(
      0,
      fixed_stride_preimage(aggregate_bound_spend_input_count),
    ),
    0,
  )
}

/// The same nine-field shape with the spare width in variable-width field 8
/// instead, at `count` minimum-width items, carried inline because
/// `3 + 2·count` stays under §8.3's `K` at both cardinalities read.
fn variable_width_shape(count: Int) -> OrderMaterialShapeV1 {
  order_material_shape(
    nine_field_preimages(8, variable_width_preimage(count)),
    -1,
  )
}

fn open_all_nine(shape: OrderMaterialShapeV1) -> Bool {
  let OrderMaterialShapeV1 { payload, material_carriage, reference_inputs } = shape
  verify_order_material(
    payload,
    material_carriage,
    reference_inputs,
    certificate_policy_id,
  )
}

/// **Measured row.** Nine door openings at §5.4's aggregate bound with every
/// byte in a fixed-stride slot: §8.11's claim in the shape where it holds, since
/// `whole_view` settles §5.1 count consistency for fields 0/1/3/4/7 by
/// arithmetic and the only cost that grows with the field is the one
/// `blake2b_256` over its preimage.
test tx_order_mint_material_at_the_aggregate_bound() {
  open_all_nine(aggregate_bound_shape())
}

/// **Fixture-only control for the row above.** Builds every byte the row builds
/// and opens no door. The within-basis ledger shape judges absolute rows and
/// publishes no machine-checked subtraction, so the pair brackets rather than
/// isolates the mint's own cost — see the ledger's note.
test tx_order_mint_material_fixture_only_at_the_aggregate_bound() {
  let OrderMaterialShapeV1 { payload, material_carriage, reference_inputs } = aggregate_bound_shape()
  expect [_certificate, _chunk_0, _chunk_1, tail] = reference_inputs
  expect InlineDatum(tail_datum) = tail.output.datum
  and {
    bytearray.length(payload.tx_id) == 32,
    bytearray.length(payload.transaction_commitment) == 32,
    list.length(material_carriage) == field_count,
    bytearray.length(builtin.un_b_data(tail_datum)) == 2227,
  }
}

/// **Measured row.** The same nine openings with the spare width in a
/// variable-width slot, at the lower of the two cardinalities the ledger reads.
test tx_order_mint_material_at_two_hundred_fifty_variable_width_items() {
  open_all_nine(variable_width_shape(lower_variable_width_item_count))
}

/// **Measured row.** The upper cardinality. Read together with the row above it
/// prices `walk_to_end` per item, which is the term §8.11's byte bound does not
/// carry; the ledger's note states the rate and the cardinality at which it
/// reaches the §3.3 basis.
test tx_order_mint_material_at_five_hundred_variable_width_items() {
  open_all_nine(variable_width_shape(upper_variable_width_item_count))
}

/// Each row measures the shape it says it does. Nine non-empty fields in both
/// fixtures; the aggregate-bound one at §5.4's bound with no room for another
/// item and its carriage at §8.3's maximum chunk count; the variable-width ones
/// at the item counts the ledger's rate is computed from. A fixture that quietly
/// shrinks stops being its row rather than re-pinning it cheaper.
test tx_order_mint_material_fixture_sits_at_the_aggregate_bound() {
  let bound = nine_field_preimages(
    0,
    fixed_stride_preimage(aggregate_bound_spend_input_count),
  )
  expect Some(widened) = list.head(bound)
  let aggregate = list.foldl(
    bound,
    0,
    fn(preimage, total) { total + bytearray.length(preimage) },
  )
  and {
    list.length(bound) == field_count,
    list.all(
      bound,
      fn(preimage) { field_commitment(preimage) != empty_field_commitment },
    ),
    // At the bound, and *at* it rather than merely under it: one more item at
    // §5.3's field-0 stride would not fit.
    aggregate == 32742,
    aggregate <= max_transaction_aggregate_field_bytes,
    aggregate + spend_input_stride > max_transaction_aggregate_field_bytes,
    bytearray.length(widened) == 3 + spend_input_stride * aggregate_bound_spend_input_count,
    bytearray.length(widened) == 32523,
    bytearray.length(widened) > chunk_bytes_k,
    expected_chunk_count(bytearray.length(widened)) == max_tier3_chunk_count,
    field_stride(0) == spend_input_stride,
    // The variable-width rows: the walked slot, at both read cardinalities, and
    // inside `K` so tier 1 is the admissible carriage for them.
    field_stride(8) == walk_derived_stride,
    bytearray.length(variable_width_preimage(lower_variable_width_item_count)) == 2 + 2 * lower_variable_width_item_count,
    bytearray.length(variable_width_preimage(upper_variable_width_item_count)) == 1003,
    bytearray.length(variable_width_preimage(upper_variable_width_item_count)) < chunk_bytes_k,
    // The doubling builder emits exactly what `encode_field_preimage` would, at
    // a width where the quadratic fold is affordable to run beside it.
    variable_width_preimage(3) == encode_field_preimage([#"00", #"00", #"00"]),
    fixed_stride_preimage(2) == encode_field_preimage(
      [
        #"8258200000000000000000000000000000000000000000000000000000000000000000190003",
        #"8258200000000000000000000000000000000000000000000000000000000000000000190003",
      ],
    ),
  }
}

/// The door hashes the whole 32,523 bytes rather than trusting the manifest. The
/// certificate here is the one a real certification produced, its digest vector
/// is over the real chunks and every named chunk is the right length — only the
/// ragged tail's last byte differs, so the refusal comes from §4's commitment
/// and from nothing else. Field 0 is fixed-stride, so the tampered tail also
/// leaves §5.1's count arithmetic satisfied and cannot be refused by it.
test tx_order_mint_material_at_the_aggregate_bound_refuses_a_tampered_tail_chunk() fail {
  let OrderMaterialShapeV1 { payload, material_carriage, reference_inputs } = aggregate_bound_shape()
  expect [certificate, chunk_0, chunk_1, tail] = reference_inputs
  expect InlineDatum(tail_datum) = tail.output.datum
  let tail_bytes = builtin.un_b_data(tail_datum)
  let tampered = bytearray.concat(
    bytearray.take(tail_bytes, bytearray.length(tail_bytes) - 1),
    #"01",
  )
  verify_order_material(
    payload,
    material_carriage,
    [certificate, chunk_0, chunk_1, creator_publication(3, tampered)],
    certificate_policy_id,
  )
}
