use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_canonical_uint_at,
  decode_definite_array_header_at, decode_definite_bytes_at,
  encode_definite_bytes, expect_h32, expect_network_id, expect_non_negative,
  validity_from_code, validity_to_code,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, encode_native_tx_body_compact,
  encode_native_tx_compact_v1, encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_hash28_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor, decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_outputs_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
  encode_address_witness_preimage, encode_hash28_list_preimage,
  encode_input_preimage, encode_mint_preimage, encode_output_preimage,
  encode_redeemer_witness_preimage, encode_script_witness_preimage,
  verify_canonical_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardRedeemerWitness, MidgardScriptWitness,
  MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionBodyPartialPreimages, MidgardTransactionBodyPartialView,
  MidgardTransactionPartialView, MidgardTransactionWitnessSet,
  MidgardTransactionWitnessSetPartialPreimages,
  MidgardTransactionWitnessSetPartialView, MidgardTxInput, MidgardTxOutput,
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxProofSourceCborV1, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact, VerifiedMidgardTransaction,
  native_tx_version_v1,
}
use midgard/native_tx_field_access_v1.{field_commitment}

/// §4 plain hashing: the commitment is `blake2b_256` over the preimage bytes
/// themselves, so authenticating a revealed field is one hash — no per-item
/// leaves, no frontier, no Merkle machinery. The decode that follows is what
/// keeps a hash match from authenticating a non-canonical shape.
fn expect_field_preimage_hash(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> ByteArray {
  expect field_commitment(preimage_cbor) == expected_hash
  preimage_cbor
}

fn decode_midgard_tx_inputs_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardTxInput> {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)
}

fn decode_midgard_tx_outputs_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardTxOutput> {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_outputs_preimage_cbor(preimage_cbor)
}

fn decode_midgard_tx_hash28_list_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<ByteArray> {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_hash28_list_preimage_cbor(preimage_cbor)
}

fn decode_midgard_tx_mint_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> Data {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
}

fn decode_midgard_tx_address_witnesses_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardAddressWitness> {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_address_witnesses_preimage_cbor(preimage_cbor)
}

fn decode_midgard_tx_script_witnesses_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardScriptWitness> {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)
}

fn decode_midgard_tx_redeemer_witnesses_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardRedeemerWitness> {
  expect _authenticated = expect_field_preimage_hash(
    preimage_cbor,
    expected_hash,
  )
  decode_midgard_tx_redeemer_witnesses_preimage_cbor(preimage_cbor)
}

pub fn encode_midgard_transaction_body_full(
  body: MidgardTransactionBody,
) -> ByteArray {
  expect _fee = expect_non_negative(body.fee)
  expect _script_integrity_hash = expect_h32(body.script_integrity_hash)
  expect _auxiliary_data_hash = expect_h32(body.auxiliary_data_hash)
  expect _network_id = expect_network_id(body.network_id)
  #"8c"
    |> bytearray.concat(
        encode_definite_bytes(encode_input_preimage(body.inputs)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_input_preimage(body.reference_inputs)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_output_preimage(body.outputs)),
      )
    |> bytearray.concat(cbor.serialise(body.fee))
    |> bytearray.concat(cbor.serialise(body.validity_interval_start))
    |> bytearray.concat(cbor.serialise(body.validity_interval_end))
    |> bytearray.concat(
        encode_definite_bytes(
          encode_hash28_list_preimage(body.required_observers),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_hash28_list_preimage(body.required_signers),
        ),
      )
    |> bytearray.concat(encode_definite_bytes(encode_mint_preimage(body.mint)))
    |> bytearray.concat(encode_definite_bytes(body.script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(body.auxiliary_data_hash))
    |> bytearray.concat(cbor.serialise(body.network_id))
}

pub fn encode_midgard_transaction_witness_set_full(
  witness_set: MidgardTransactionWitnessSet,
) -> ByteArray {
  #"83"
    |> bytearray.concat(
        encode_definite_bytes(
          encode_address_witness_preimage(witness_set.addr_tx_wits),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_script_witness_preimage(witness_set.script_tx_wits),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_redeemer_witness_preimage(witness_set.redeemer_tx_wits),
        ),
      )
}

/// §4: each of the six body commitments is a flat `blake2b_256` over that
/// field's §5.1 enveloped preimage. Field identity is positional — it comes
/// from the slot the hash lands in, not from anything mixed into the hash.
pub fn midgard_transaction_body_to_compact(
  body: MidgardTransactionBody,
) -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    spend_inputs_hash: field_commitment(encode_input_preimage(body.inputs)),
    reference_inputs_hash: field_commitment(
      encode_input_preimage(body.reference_inputs),
    ),
    outputs_hash: field_commitment(encode_output_preimage(body.outputs)),
    fee: body.fee,
    validity_interval_start: body.validity_interval_start,
    validity_interval_end: body.validity_interval_end,
    required_observers_hash: field_commitment(
      encode_hash28_list_preimage(body.required_observers),
    ),
    required_signers_hash: field_commitment(
      encode_hash28_list_preimage(body.required_signers),
    ),
    mint_hash: field_commitment(encode_mint_preimage(body.mint)),
    script_integrity_hash: body.script_integrity_hash,
    auxiliary_data_hash: body.auxiliary_data_hash,
    network_id: body.network_id,
  }
}

pub fn midgard_transaction_witness_set_to_compact(
  witness_set: MidgardTransactionWitnessSet,
) -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: field_commitment(
      encode_address_witness_preimage(witness_set.addr_tx_wits),
    ),
    script_tx_wits_hash: field_commitment(
      encode_script_witness_preimage(witness_set.script_tx_wits),
    ),
    redeemer_tx_wits_hash: field_commitment(
      encode_redeemer_witness_preimage(witness_set.redeemer_tx_wits),
    ),
  }
}

pub fn midgard_transaction_to_compact(tx: MidgardTransaction) -> NativeTxCompact {
  let witness_set_hash = tx.witness_set
    |> midgard_transaction_witness_set_to_compact
    |> encode_native_tx_witness_set_compact
    |> blake2b_256

  NativeTxCompact {
    body: tx.body |> midgard_transaction_body_to_compact,
    witness_set_hash,
    validity_code: tx.validity |> validity_to_code,
  }
}

pub fn midgard_transaction_proof_source_v1(
  tx: MidgardTransaction,
) -> NativeTxProofSourceCborV1 {
  expect tx.version == native_tx_version_v1
  let compact_cbor = tx
    |> midgard_transaction_to_compact
    |> encode_native_tx_compact_v1
  let witness_set_compact_cbor = tx.witness_set
    |> midgard_transaction_witness_set_to_compact
    |> encode_native_tx_witness_set_compact
  let field_preimage_lengths_cbor = encode_native_tx_field_preimage_lengths_v1(
    NativeTxFieldPreimageLengthsV1 {
      spend_inputs: bytearray.length(encode_input_preimage(tx.body.inputs)),
      reference_inputs: bytearray.length(
        encode_input_preimage(tx.body.reference_inputs),
      ),
      outputs: bytearray.length(encode_output_preimage(tx.body.outputs)),
      required_observers: bytearray.length(
        encode_hash28_list_preimage(tx.body.required_observers),
      ),
      required_signers: bytearray.length(
        encode_hash28_list_preimage(tx.body.required_signers),
      ),
      mint: bytearray.length(encode_mint_preimage(tx.body.mint)),
      address_witnesses: bytearray.length(
        encode_address_witness_preimage(tx.witness_set.addr_tx_wits),
      ),
      script_witnesses: bytearray.length(
        encode_script_witness_preimage(tx.witness_set.script_tx_wits),
      ),
      redeemers: bytearray.length(
        encode_redeemer_witness_preimage(tx.witness_set.redeemer_tx_wits),
      ),
    },
  )
  NativeTxProofSourceCborV1 {
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  }
}

pub fn midgard_transaction_proof_commitment_v1(
  tx: MidgardTransaction,
) -> ByteArray {
  let source = midgard_transaction_proof_source_v1(tx)
  native_tx_proof_commitment_v1(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
}

fn verify_midgard_transaction_field_preimage_against_source_v1(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
  preimage_cbor: ByteArray,
) -> Bool {
  let NativeTxCompact { body, .. } = verified.tx_compact

  when field_index is {
    0 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.spend_inputs
      let decoded = decode_midgard_tx_inputs_preimage(
        preimage_cbor,
        body.spend_inputs_hash,
      )
      encode_input_preimage(decoded) == preimage_cbor
    }
    1 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.reference_inputs
      let decoded = decode_midgard_tx_inputs_preimage(
        preimage_cbor,
        body.reference_inputs_hash,
      )
      encode_input_preimage(decoded) == preimage_cbor
    }
    2 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.outputs
      let decoded = decode_midgard_tx_outputs_preimage(
        preimage_cbor,
        body.outputs_hash,
      )
      encode_output_preimage(decoded) == preimage_cbor
    }
    3 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.required_observers
      let decoded = decode_midgard_tx_hash28_list_preimage(
        preimage_cbor,
        body.required_observers_hash,
      )
      encode_hash28_list_preimage(decoded) == preimage_cbor
    }
    4 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.required_signers
      let decoded = decode_midgard_tx_hash28_list_preimage(
        preimage_cbor,
        body.required_signers_hash,
      )
      encode_hash28_list_preimage(decoded) == preimage_cbor
    }
    5 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.mint
      verify_canonical_mint_preimage_cbor(preimage_cbor, body.mint_hash)
    }
    6 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.script_witnesses
      let decoded = decode_midgard_tx_script_witnesses_preimage(
        preimage_cbor,
        witness_set.script_tx_wits_hash,
      )
      encode_script_witness_preimage(decoded) == preimage_cbor
    }
    7 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.address_witnesses
      let decoded = decode_midgard_tx_address_witnesses_preimage(
        preimage_cbor,
        witness_set.addr_tx_wits_hash,
      )
      encode_address_witness_preimage(decoded) == preimage_cbor
    }
    8 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.redeemers
      let decoded = decode_midgard_tx_redeemer_witnesses_preimage(
        preimage_cbor,
        witness_set.redeemer_tx_wits_hash,
      )
      encode_redeemer_witness_preimage(decoded) == preimage_cbor
    }
    _ -> False
  }
}

/// Verifies one independently revealed V1 field against the compact
/// transaction source. Each branch decodes and re-encodes the field so a hash
/// match alone can never authenticate a non-canonical or unsupported shape.
/// Field 5 is the one branch that walks in place instead of re-encoding —
/// materialising a whole mint map as Plutus `Data` just to compare bytes does
/// not fit the L1 execution-memory envelope — so its canonicality comes from
/// the same §5.1 head decoder plus the ordering, width and non-zero-quantity
/// rules `verify_canonical_mint_preimage_cbor` walks.
///
/// Field indices are consensus ordered:
/// 0 spend inputs, 1 reference inputs, 2 outputs, 3 observers, 4 signers,
/// 5 mint, 6 script witnesses, 7 address witnesses, 8 redeemers.
///
/// **Not a field-access idiom** (AC2). This takes a whole revealed preimage
/// and answers yes/no about it; it hands out no view and no item, and it never
/// accepts a free-standing field hash — the expected hashes come from the
/// tx-id-verified compact structures, satisfying §4. It has no production
/// caller: what binds it today is the §5/§9.1 conformance suite, which needs
/// exactly this decode-and-re-encode round trip to pin the canonical spelling
/// of all nine fields. #570's machine walk core is what replaces it on the
/// dispute path.
pub fn verify_midgard_transaction_field_preimage_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  field_index: Int,
  preimage_cbor: ByteArray,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment

  verify_midgard_transaction_field_preimage_against_source_v1(
    verified,
    witness_set,
    field_lengths,
    field_index,
    preimage_cbor,
  )
}

/// The retired counted scheme's positional field lookup, kept only to feed
/// `verify_midgard_transaction_field_chunk_v1` below. Live positional
/// extraction now lives behind `authenticated_field_view`'s door and is
/// deliberately not exported from anywhere.
fn retired_counted_field_commitment_v1(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> ByteArray {
  let NativeTxCompact { body, .. } = verified.tx_compact
  when field_index is {
    0 -> body.spend_inputs_hash
    1 -> body.reference_inputs_hash
    2 -> body.outputs_hash
    3 -> body.required_observers_hash
    4 -> body.required_signers_hash
    5 -> body.mint_hash
    6 -> witness_set.script_tx_wits_hash
    7 -> witness_set.addr_tx_wits_hash
    8 -> witness_set.redeemer_tx_wits_hash
    _ -> fail
  }
}

/// **RETIRED counted-scheme surface — and one live caller still binds it.**
///
/// Under §4's plain hashing a field commitment is a single hash over the whole
/// preimage, so there is nothing a per-item Merkle opening can be checked
/// against: `bounded_collection_v1.verify_item` compares a counted-scheme
/// Merkle root against a flat `blake2b_256` and is therefore unsatisfiable,
/// and this predicate with it.
///
/// It is **not** true that nothing calls it. The user-events field-receipt
/// mint (`midgard/user_events/tx_field_receipt_v1`, reached from
/// `validators/user-events/tx-field-receipt-v1.ak`) gates its `PublishField`
/// branch on this predicate, so that branch cannot succeed: the counted
/// per-item/per-chunk receipt chain is dead surface until the §8 carriage
/// tiers replace it. That fails closed — no receipt can be minted for a field
/// that was never opened — which is why it is a liveness break and not a
/// soundness one, and `tx-field-receipt-v1.test.ak` records it so the suite
/// stops being silent about it.
///
/// Rebinding is not this ticket's: the receipt chain publishes *items and
/// chunks*, and the replacement for that is the §8.6 permissionless
/// chunk-certificate validator plus its publish/heal tooling (#573, #574),
/// after the machine walk core lands (#570). This survives meanwhile only so
/// the dispute machine and the proof families keep compiling until #570 and
/// the Phase-5 family-rebind lanes delete their counted call sites. New
/// surface uses `authenticated_field_view`
/// (`midgard/native_tx_field_access_v1`).
pub fn verify_midgard_transaction_field_chunk_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment,
    collection_proof.field_index == proof.field_index,
    collection_proof.item_index == proof.item_index,
    collection_proof.item_length == proof.total_length,
    bounded_collection_v1.verify_item(
      retired_counted_field_commitment_v1(
        verified,
        witness_set,
        proof.field_index,
      ),
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, proof),
  }
}

/// Authenticates the bounded mint field against the V1 compact source
/// without recursively validating the entire map in one L1 step. Callers must
/// subsequently stream the canonical CBOR structure through a bounded state
/// machine before treating any mint semantics as valid.
pub fn verify_midgard_transaction_mint_preimage_commitment_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  preimage_cbor: ByteArray,
) -> Bool {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  and {
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment,
    bytearray.length(preimage_cbor) == field_lengths.mint,
    verify_canonical_mint_preimage_cbor(
      preimage_cbor,
      verified.tx_compact.body.mint_hash,
    ),
  }
}

/// Verifies the complete forced-order fragment bundle while decoding the
/// compact proof source only once. Exactly one canonical fragment is required
/// for each of the nine consensus-ordered dynamic fields.
pub fn verify_midgard_transaction_field_preimages_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  preimages: List<ByteArray>,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment
  expect
      [
        spend_inputs, reference_inputs, outputs, required_observers,
        required_signers, mint, script_witnesses, address_witnesses, redeemers,
      ]
    = preimages

  and {
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      0,
      spend_inputs,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      1,
      reference_inputs,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      2,
      outputs,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      3,
      required_observers,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      4,
      required_signers,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      5,
      mint,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      6,
      script_witnesses,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      7,
      address_witnesses,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      8,
      redeemers,
    ),
  }
}

fn encode_midgard_transaction_for_version(
  expected_version: Int,
  tx: MidgardTransaction,
) -> ByteArray {
  expect expected_version == native_tx_version_v1
  expect tx.version == expected_version
  #"84"
    |> bytearray.concat(cbor.serialise(tx.version))
    |> bytearray.concat(encode_midgard_transaction_body_full(tx.body))
    |> bytearray.concat(
        encode_midgard_transaction_witness_set_full(tx.witness_set),
      )
    |> bytearray.concat(cbor.serialise(tx.validity |> validity_to_code))
}

pub fn encode_midgard_transaction_v1(tx: MidgardTransaction) -> ByteArray {
  encode_midgard_transaction_for_version(native_tx_version_v1, tx)
}

pub fn decode_midgard_transaction_body_data(
  data: Data,
) -> MidgardTransactionBody {
  expect
      [
        inputs_preimage_cbor_data, reference_inputs_preimage_cbor_data,
        outputs_preimage_cbor_data, fee_data, validity_interval_start_data,
        validity_interval_end_data, required_observers_preimage_cbor_data,
        required_signers_preimage_cbor_data, mint_preimage_cbor_data,
        script_integrity_hash_data, auxiliary_data_hash_data, network_id_data,
      ]
    = builtin.un_list_data(data)

  MidgardTransactionBody {
    inputs: inputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_inputs_preimage_cbor,
    reference_inputs: reference_inputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_inputs_preimage_cbor,
    outputs: outputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_outputs_preimage_cbor,
    fee: builtin.un_i_data(fee_data),
    validity_interval_start: builtin.un_i_data(validity_interval_start_data),
    validity_interval_end: builtin.un_i_data(validity_interval_end_data),
    required_observers: required_observers_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_hash28_list_preimage_cbor,
    required_signers: required_signers_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_hash28_list_preimage_cbor,
    mint: mint_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_mint_preimage_cbor,
    script_integrity_hash: builtin.un_b_data(script_integrity_hash_data),
    auxiliary_data_hash: builtin.un_b_data(auxiliary_data_hash_data),
    network_id: builtin.un_i_data(network_id_data),
  }
}

pub fn decode_midgard_transaction_witness_set_data(
  data: Data,
) -> MidgardTransactionWitnessSet {
  expect
      [
        addr_tx_wits_preimage_cbor_data, script_tx_wits_preimage_cbor_data,
        redeemer_tx_wits_preimage_cbor_data,
      ]
    = builtin.un_list_data(data)

  MidgardTransactionWitnessSet {
    addr_tx_wits: addr_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_address_witnesses_preimage_cbor,
    script_tx_wits: script_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_script_witnesses_preimage_cbor,
    redeemer_tx_wits: redeemer_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  }
}

fn decode_midgard_transaction_body_at(
  tx_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardTransactionBody> {
  let Pair(offset, field_count) = decode_definite_array_header_at(
    tx_cbor,
    offset,
  )
  expect field_count == 12

  let Pair(offset, inputs_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, reference_inputs_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, outputs_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, fee) = decode_canonical_uint_at(tx_cbor, offset)
  let Pair(offset, validity_interval_start) = decode_canonical_int_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, validity_interval_end) = decode_canonical_int_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, required_observers_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, required_signers_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, mint_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, script_integrity_hash) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, auxiliary_data_hash) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, network_id) = decode_canonical_uint_at(tx_cbor, offset)

  Pair(
    offset,
    MidgardTransactionBody {
      inputs: inputs_preimage_cbor |> decode_midgard_tx_inputs_preimage_cbor,
      reference_inputs: reference_inputs_preimage_cbor
        |> decode_midgard_tx_inputs_preimage_cbor,
      outputs: outputs_preimage_cbor |> decode_midgard_tx_outputs_preimage_cbor,
      fee: expect_non_negative(fee),
      validity_interval_start,
      validity_interval_end,
      required_observers: required_observers_preimage_cbor
        |> decode_midgard_tx_hash28_list_preimage_cbor,
      required_signers: required_signers_preimage_cbor
        |> decode_midgard_tx_hash28_list_preimage_cbor,
      mint: mint_preimage_cbor |> decode_midgard_tx_mint_preimage_cbor,
      script_integrity_hash: expect_h32(script_integrity_hash),
      auxiliary_data_hash: expect_h32(auxiliary_data_hash),
      network_id: expect_network_id(network_id),
    },
  )
}

fn decode_midgard_transaction_witness_set_at(
  tx_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardTransactionWitnessSet> {
  let Pair(offset, field_count) = decode_definite_array_header_at(
    tx_cbor,
    offset,
  )
  expect field_count == 3

  let Pair(offset, addr_tx_wits_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, script_tx_wits_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, redeemer_tx_wits_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )

  Pair(
    offset,
    MidgardTransactionWitnessSet {
      addr_tx_wits: addr_tx_wits_preimage_cbor
        |> decode_midgard_tx_address_witnesses_preimage_cbor,
      script_tx_wits: script_tx_wits_preimage_cbor
        |> decode_midgard_tx_script_witnesses_preimage_cbor,
      redeemer_tx_wits: redeemer_tx_wits_preimage_cbor
        |> decode_midgard_tx_redeemer_witnesses_preimage_cbor,
    },
  )
}

fn decode_midgard_transaction_for_version(
  expected_version: Int,
  tx_cbor: ByteArray,
) -> MidgardTransaction {
  let Pair(offset, field_count) = decode_definite_array_header_at(tx_cbor, 0)
  expect field_count == 4
  expect expected_version == native_tx_version_v1
  let Pair(offset, version) = decode_canonical_uint_at(tx_cbor, offset)
  expect version == expected_version
  let Pair(offset, body) = decode_midgard_transaction_body_at(tx_cbor, offset)
  let Pair(offset, witness_set) = decode_midgard_transaction_witness_set_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, validity_code) = decode_canonical_uint_at(tx_cbor, offset)
  expect offset == bytearray.length(tx_cbor)

  let transaction = MidgardTransaction {
    version,
    validity: validity_code |> validity_from_code,
    body,
    witness_set,
  }
  expect
    encode_midgard_transaction_for_version(expected_version, transaction) == tx_cbor
  transaction
}

pub fn decode_midgard_transaction_v1(tx_cbor: ByteArray) -> MidgardTransaction {
  decode_midgard_transaction_for_version(native_tx_version_v1, tx_cbor)
}

fn verify_midgard_transaction_for_version(
  version: Int,
  tx_id: ByteArray,
  tx_cbor: ByteArray,
) -> VerifiedMidgardTransaction {
  let tx = decode_midgard_transaction_for_version(version, tx_cbor)
  let compact = midgard_transaction_to_compact(tx)
  expect version == native_tx_version_v1
  let compact_cbor = encode_native_tx_compact_v1(compact)
  expect
    native_tx_id_for_version(
      version,
      encode_native_tx_body_compact(compact.body),
    ) == tx_id

  VerifiedMidgardTransaction { tx_id, transaction: tx, compact, compact_cbor }
}

pub fn verify_midgard_transaction_v1(
  tx_id: ByteArray,
  tx_cbor: ByteArray,
) -> VerifiedMidgardTransaction {
  verify_midgard_transaction_for_version(native_tx_version_v1, tx_id, tx_cbor)
}

pub fn partial_view_from_compact(
  compact: VerifiedMidgardNativeTxCompact,
) -> MidgardTransactionPartialView {
  let NativeTxCompact { body, validity_code, .. } = compact.tx_compact
  MidgardTransactionPartialView {
    version: Some(compact.version),
    validity: Some(validity_code |> validity_from_code),
    body: Some(
      MidgardTransactionBodyPartialView {
        inputs: None,
        reference_inputs: None,
        outputs: None,
        fee: Some(body.fee),
        validity_interval_start: Some(body.validity_interval_start),
        validity_interval_end: Some(body.validity_interval_end),
        required_observers: None,
        required_signers: None,
        mint: None,
        script_integrity_hash: Some(body.script_integrity_hash),
        auxiliary_data_hash: Some(body.auxiliary_data_hash),
        network_id: Some(body.network_id),
      },
    ),
    witness_set: None,
  }
}

fn decode_optional_inputs_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardTxInput>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_inputs_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_outputs_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardTxOutput>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_outputs_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_hash28_list_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<ByteArray>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_hash28_list_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_mint_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<Data> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_mint_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_address_witnesses_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardAddressWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_address_witnesses_preimage(
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_script_witnesses_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardScriptWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_script_witnesses_preimage(
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_redeemer_witnesses_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardRedeemerWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_redeemer_witnesses_preimage(
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

pub fn partial_body_view_from_compact_and_preimages(
  body: NativeTxBodyCompact,
  preimages: MidgardTransactionBodyPartialPreimages,
) -> MidgardTransactionBodyPartialView {
  MidgardTransactionBodyPartialView {
    inputs: decode_optional_inputs_preimage(
      preimages.inputs,
      body.spend_inputs_hash,
    ),
    reference_inputs: decode_optional_inputs_preimage(
      preimages.reference_inputs,
      body.reference_inputs_hash,
    ),
    outputs: decode_optional_outputs_preimage(
      preimages.outputs,
      body.outputs_hash,
    ),
    fee: Some(body.fee),
    validity_interval_start: Some(body.validity_interval_start),
    validity_interval_end: Some(body.validity_interval_end),
    required_observers: decode_optional_hash28_list_preimage(
      preimages.required_observers,
      body.required_observers_hash,
    ),
    required_signers: decode_optional_hash28_list_preimage(
      preimages.required_signers,
      body.required_signers_hash,
    ),
    mint: decode_optional_mint_preimage(preimages.mint, body.mint_hash),
    script_integrity_hash: Some(body.script_integrity_hash),
    auxiliary_data_hash: Some(body.auxiliary_data_hash),
    network_id: Some(body.network_id),
  }
}

pub fn partial_witness_set_view_from_compact_and_preimages(
  witness_set: NativeTxWitnessSetCompact,
  preimages: MidgardTransactionWitnessSetPartialPreimages,
) -> MidgardTransactionWitnessSetPartialView {
  MidgardTransactionWitnessSetPartialView {
    addr_tx_wits: decode_optional_address_witnesses_preimage(
      preimages.addr_tx_wits,
      witness_set.addr_tx_wits_hash,
    ),
    script_tx_wits: decode_optional_script_witnesses_preimage(
      preimages.script_tx_wits,
      witness_set.script_tx_wits_hash,
    ),
    redeemer_tx_wits: decode_optional_redeemer_witnesses_preimage(
      preimages.redeemer_tx_wits,
      witness_set.redeemer_tx_wits_hash,
    ),
  }
}

pub fn partial_view_from_compact_and_preimages(
  compact: VerifiedMidgardNativeTxCompact,
  body_preimages: MidgardTransactionBodyPartialPreimages,
  witness_set: Option<NativeTxWitnessSetCompact>,
  witness_set_preimages: MidgardTransactionWitnessSetPartialPreimages,
) -> MidgardTransactionPartialView {
  let witness_set_view = when witness_set is {
    Some(witness_set) -> {
      let NativeTxCompact { witness_set_hash, .. } = compact.tx_compact
      expect
        blake2b_256(encode_native_tx_witness_set_compact(witness_set)) == witness_set_hash
      Some(
        partial_witness_set_view_from_compact_and_preimages(
          witness_set,
          witness_set_preimages,
        ),
      )
    }
    None -> None
  }
  let NativeTxCompact { body, validity_code, .. } = compact.tx_compact

  MidgardTransactionPartialView {
    version: Some(compact.version),
    validity: Some(validity_code |> validity_from_code),
    body: Some(
      partial_body_view_from_compact_and_preimages(body, body_preimages),
    ),
    witness_set: witness_set_view,
  }
}
