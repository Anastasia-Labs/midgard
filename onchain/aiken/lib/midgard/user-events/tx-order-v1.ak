use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId, Value, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{NativeTxFieldPreimageLengthsV1}
use midgard/ledger_state.{
  ForcedInclusionTxV1, MidgardTxValidity, NativeTxProofSourceV1,
  TxFieldPreimageV1, TxFieldReceiptV1, TxOrderEventV1, TxOrderId,
  TxOrderPayloadV1, unsafe_event_to_key_value_pair,
}
use midgard/native_tx_field_access_v1
use midgard/transition_trace
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<TxOrderEventV1>

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  membership_proof: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  validity_override: MidgardTxValidity,
}

const field_receipt_v1_domain: ByteArray =
  #"4d69646761726454784669656c64526563656970745631"

pub fn field_receipt_asset_name(
  tx_order_policy_id: PolicyId,
  tx_order_id: TxOrderId,
  transaction_commitment: ByteArray,
  field_index: Int,
  item_index: Int,
  chunk_index: Int,
) -> AssetName {
  expect bytearray.length(tx_order_policy_id) == 28
  expect bytearray.length(tx_order_id.transaction_id) == 32
  expect tx_order_id.output_index >= 0
  expect bytearray.length(transaction_commitment) == 32
  expect field_index >= 0 && field_index < 9
  expect item_index >= 0
  expect chunk_index >= 0
  field_receipt_v1_domain
    |> bytearray.concat(tx_order_policy_id)
    |> bytearray.concat(tx_order_id.transaction_id)
    |> bytearray.concat(
        bytearray.from_int_big_endian(tx_order_id.output_index, 8),
      )
    |> bytearray.concat(transaction_commitment)
    |> bytearray.concat(bytearray.from_int_big_endian(field_index, 1))
    |> bytearray.concat(bytearray.from_int_big_endian(item_index, 8))
    |> bytearray.concat(bytearray.from_int_big_endian(chunk_index, 8))
    |> blake2b_256
}

fn field_length(
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  when field_index is {
    0 -> lengths.spend_inputs
    1 -> lengths.reference_inputs
    2 -> lengths.outputs
    3 -> lengths.required_observers
    4 -> lengths.required_signers
    5 -> lengths.mint
    6 -> lengths.script_witnesses
    7 -> lengths.address_witnesses
    8 -> lengths.redeemers
    _ -> 0
  }
}

fn next_non_empty_field(
  commitments: List<ByteArray>,
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  if field_index >= 9 {
    9
  } else {
    let commitment = commitments |> list.expect_at(field_index)
    if commitment == native_tx_field_access_v1.empty_field_commitment {
      expect field_length(lengths, field_index) == 1
      next_non_empty_field(commitments, lengths, field_index + 1)
    } else {
      field_index
    }
  }
}

fn canonical_header_size(item_count: Int) -> Int {
  expect item_count >= 0
  if item_count < 24 {
    1
  } else if item_count <= 0xff {
    2
  } else if item_count <= 0xffff {
    3
  } else {
    5
  }
}

fn canonical_bytes_size(payload_size: Int) -> Int {
  expect payload_size >= 0
  canonical_header_size(payload_size) + payload_size
}

fn item_encoded_size(field_index: Int, item_length: Int) -> Int {
  expect item_length >= 0
  when field_index is {
    0 -> canonical_bytes_size(item_length)
    1 -> canonical_bytes_size(item_length)
    2 -> canonical_bytes_size(item_length)
    3 -> canonical_bytes_size(item_length)
    4 -> canonical_bytes_size(item_length)
    5 -> {
      expect item_length > 1
      item_length - 1
    }
    6 -> item_length
    7 -> canonical_bytes_size(item_length)
    8 -> item_length
    _ -> fail
  }
}

fn material_directory(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
) -> Pair<List<ByteArray>, NativeTxFieldPreimageLengthsV1> {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
      source.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
    ) == transaction_commitment
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    source.field_preimage_lengths_cbor,
  )
  let body = verified.tx_compact.body
  Pair(
    [
      body.spend_inputs_hash,
      body.reference_inputs_hash,
      body.outputs_hash,
      body.required_observers_hash,
      body.required_signers_hash,
      body.mint_hash,
      witness_set.script_tx_wits_hash,
      witness_set.addr_tx_wits_hash,
      witness_set.redeemer_tx_wits_hash,
    ],
    lengths,
  )
}

fn receipt_identity_is_valid(
  receipt: TxFieldReceiptV1,
  field_receipt_policy_id: PolicyId,
  tx_order_policy_id: PolicyId,
  tx_order_id: TxOrderId,
  transaction_commitment: ByteArray,
) -> Bool {
  and {
    receipt.field_receipt_policy_id == field_receipt_policy_id,
    receipt.tx_order_policy_id == tx_order_policy_id,
    receipt.tx_order_id == tx_order_id,
    receipt.transaction_commitment == transaction_commitment,
  }
}

fn receipt_descriptor_is_valid(
  receipt: TxFieldReceiptV1,
  commitments: List<ByteArray>,
) -> Bool {
  let proof = receipt.collection_proof
  and {
    proof.field_index >= 0,
    proof.field_index < 9,
    proof.item_index >= 0,
    proof.item_index < proof.item_count,
    receipt.chunk_index >= 0,
    receipt.chunk_index < bounded_item_v1.chunk_count(proof.item_length),
    bounded_collection_v1.verify_item(
      commitments |> list.expect_at(proof.field_index),
      proof,
    ),
  }
}

fn collection_successor_is_valid(
  previous: bounded_collection_v1.ItemProofV1,
  current: bounded_collection_v1.ItemProofV1,
) -> Bool {
  and {
    current.version == previous.version,
    current.field_index == previous.field_index,
    current.item_count == previous.item_count,
    current.item_index == previous.item_index + 1,
    current.frontier == previous.frontier,
  }
}

fn receipt_size_is_valid(
  receipt: TxFieldReceiptV1,
  lengths: NativeTxFieldPreimageLengthsV1,
  size_before_item: Int,
) -> Bool {
  let proof = receipt.collection_proof
  let final_chunk = receipt.chunk_index + 1 == bounded_item_v1.chunk_count(
    proof.item_length,
  )
  let size = if final_chunk {
    size_before_item + item_encoded_size(proof.field_index, proof.item_length)
  } else {
    size_before_item
  }
  and {
    receipt.field_encoded_size == size,
    if final_chunk && proof.item_index + 1 == proof.item_count {
      size == field_length(lengths, proof.field_index)
    } else {
      True
    },
  }
}

/// Verifies the ordered availability-chain position of one independently
/// published field receipt. Each publication links to exactly one predecessor,
/// so complete material may span any number of Cardano transactions.
pub fn verify_receipt_chain_link(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
  field_receipt_policy_id: PolicyId,
  tx_order_policy_id: PolicyId,
  tx_order_id: TxOrderId,
  predecessor: Option<TxFieldReceiptV1>,
  receipt: TxFieldReceiptV1,
) -> Bool {
  let Pair(commitments, lengths) = material_directory(
    transaction_id,
    transaction_commitment,
    source,
  )
  let proof = receipt.collection_proof
  expect
    receipt_identity_is_valid(
      receipt,
      field_receipt_policy_id,
      tx_order_policy_id,
      tx_order_id,
      transaction_commitment,
    )
  expect receipt_descriptor_is_valid(receipt, commitments)

  when predecessor is {
    None -> {
      let first_field = next_non_empty_field(commitments, lengths, 0)
      and {
        receipt.predecessor_receipt_reference == None,
        first_field < 9,
        proof.field_index == first_field,
        proof.item_index == 0,
        receipt.chunk_index == 0,
        receipt_size_is_valid(
          receipt,
          lengths,
          canonical_header_size(proof.item_count),
        ),
      }
    }
    Some(previous) -> {
      let previous_proof = previous.collection_proof
      expect
        receipt_identity_is_valid(
          previous,
          field_receipt_policy_id,
          tx_order_policy_id,
          tx_order_id,
          transaction_commitment,
        )
      expect receipt_descriptor_is_valid(previous, commitments)
      let previous_chunk_count = bounded_item_v1.chunk_count(
        previous_proof.item_length,
      )
      if previous.chunk_index + 1 < previous_chunk_count {
        and {
          proof == previous_proof,
          receipt.chunk_index == previous.chunk_index + 1,
          receipt_size_is_valid(receipt, lengths, previous.field_encoded_size),
        }
      } else if previous_proof.item_index + 1 < previous_proof.item_count {
        and {
          collection_successor_is_valid(previous_proof, proof),
          receipt.chunk_index == 0,
          receipt_size_is_valid(receipt, lengths, previous.field_encoded_size),
        }
      } else {
        let next_field = next_non_empty_field(
          commitments,
          lengths,
          previous_proof.field_index + 1,
        )
        and {
          next_field < 9,
          proof.field_index == next_field,
          proof.item_index == 0,
          receipt.chunk_index == 0,
          receipt_size_is_valid(
            receipt,
            lengths,
            canonical_header_size(proof.item_count),
          ),
        }
      }
    }
  }
}

fn terminal_receipt_is_valid(
  receipt: TxFieldReceiptV1,
  commitments: List<ByteArray>,
  lengths: NativeTxFieldPreimageLengthsV1,
) -> Bool {
  let proof = receipt.collection_proof
  and {
    receipt_descriptor_is_valid(receipt, commitments),
    receipt.chunk_index + 1 == bounded_item_v1.chunk_count(proof.item_length),
    proof.item_index + 1 == proof.item_count,
    receipt.field_encoded_size == field_length(lengths, proof.field_index),
    next_non_empty_field(commitments, lengths, proof.field_index + 1) == 9,
  }
}

pub fn verify_order_receipts(
  reference_inputs: List<Input>,
  receipt_script_hash: ByteArray,
  field_receipt_policy_id: PolicyId,
  tx_order_policy_id: PolicyId,
  tx_order_id: TxOrderId,
  payload: TxOrderPayloadV1,
) -> Bool {
  let Pair(commitments, lengths) = material_directory(
    payload.tx_id,
    payload.transaction_commitment,
    payload.source,
  )
  when payload.terminal_receipt_reference is {
    None -> next_non_empty_field(commitments, lengths, 0) == 9
    Some(receipt_reference) -> {
      expect
          Some(Input {
            output_reference,
            output: Output {
              address,
              value,
              datum: InlineDatum(receipt_datum_data),
              reference_script: None,
            },
          })
        = list.find(
        reference_inputs,
        fn(input) { input.output_reference == receipt_reference },
      )
      expect output_reference == receipt_reference
      expect Script(actual_receipt_script_hash) = address.payment_credential
      expect actual_receipt_script_hash == receipt_script_hash
      expect receipt: TxFieldReceiptV1 = receipt_datum_data
      expect
        receipt_identity_is_valid(
          receipt,
          field_receipt_policy_id,
          tx_order_policy_id,
          tx_order_id,
          payload.transaction_commitment,
        )
      let proof = receipt.collection_proof
      let receipt_asset_name = field_receipt_asset_name(
        tx_order_policy_id,
        tx_order_id,
        payload.transaction_commitment,
        proof.field_index,
        proof.item_index,
        receipt.chunk_index,
      )
      and {
        quantity_of(value, field_receipt_policy_id, receipt_asset_name) == 1,
        terminal_receipt_is_valid(receipt, commitments, lengths),
      }
    }
  }
}

/// A field fragment is one-shot proof publication state. It can only be spent
/// in the transaction that burns the exact tx-order NFT to which its datum was
/// bound at order finalization.
pub fn field_fragment_burn_authorized(
  field: TxFieldPreimageV1,
  mint: Value,
) -> Bool {
  let nonce_asset_name = user_events.out_ref_to_nonce(field.tx_order_id)
  let receipt_asset_name = field_receipt_asset_name(
    field.tx_order_policy_id,
    field.tx_order_id,
    field.transaction_commitment,
    field.proof.field_index,
    field.proof.item_index,
    field.proof.chunk_index,
  )
  and {
    quantity_of(mint, field.tx_order_policy_id, nonce_asset_name) == -1,
    quantity_of(mint, field.field_receipt_policy_id, receipt_asset_name) == -1,
  }
}

pub fn field_receipt_burn_authorized(
  receipt: TxFieldReceiptV1,
  mint: Value,
) -> Bool {
  let nonce_asset_name = user_events.out_ref_to_nonce(receipt.tx_order_id)
  let receipt_asset_name = field_receipt_asset_name(
    receipt.tx_order_policy_id,
    receipt.tx_order_id,
    receipt.transaction_commitment,
    receipt.collection_proof.field_index,
    receipt.collection_proof.item_index,
    receipt.chunk_index,
  )
  and {
    quantity_of(mint, receipt.tx_order_policy_id, nonce_asset_name) == -1,
    quantity_of(mint, receipt.field_receipt_policy_id, receipt_asset_name) == -1,
  }
}

pub fn forced_inclusion_key_value(
  tx_order_event_data: Data<TxOrderEventV1>,
  validity_override: MidgardTxValidity,
  return: fn(TxOrderId, ForcedInclusionTxV1) -> result,
) -> result {
  let Pair(tx_order_event_id_data, tx_order_payload_data) = unsafe_event_to_key_value_pair(
    tx_order_event_data,
  )

  expect tx_order_id: TxOrderId = tx_order_event_id_data
  expect tx_order_payload: TxOrderPayloadV1 = tx_order_payload_data

  return(
    tx_order_id,
    ForcedInclusionTxV1 {
      tx_id: tx_order_payload.tx_id,
      transaction_commitment: tx_order_payload.transaction_commitment,
      source: tx_order_payload.source,
      operator_validity: validity_override,
    },
  )
}

pub fn get_datum(
  reference_inputs: List<Input>,
  tx_order_policy_id: PolicyId,
  tx_order_input_index: Int,
) -> Datum {
  expect
      Input {
        output: Output { datum: InlineDatum(tx_order_datum_data), .. },
        ..
      }
    = utils.get_authentic_input_with_policy_at(
    reference_inputs,
    tx_order_policy_id,
    tx_order_input_index,
  )
  expect tx_order_datum: Datum = tx_order_datum_data
  tx_order_datum
}
