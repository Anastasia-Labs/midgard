use aiken/collection/dict
use aiken/collection/list
use cardano/address.{Script}
use cardano/assets.{PolicyId, Value, tokens}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/ledger_state.{
  NativeTxProofSourceV1, TxFieldPreimageV1, TxFieldReceiptV1,
}
use midgard/user_events/tx_order_v1.{
  field_receipt_asset_name, field_receipt_burn_authorized,
  verify_receipt_chain_link,
}

pub type MintRedeemer {
  PublishField {
    field_reference_input_index: Int,
    predecessor_receipt_reference_input_index: Int,
    receipt_output_index: Int,
    transaction_id: ByteArray,
    source: NativeTxProofSourceV1,
  }
  BurnReceipts { receipt_input_indices: List<Int> }
}

fn verify_publication(
  field_preimage_script_hash: ByteArray,
  receipt_script_hash: ByteArray,
  own_policy: PolicyId,
  tx: Transaction,
  field_reference_input_index: Int,
  predecessor_receipt_reference_input_index: Int,
  receipt_output_index: Int,
  transaction_id: ByteArray,
  source: NativeTxProofSourceV1,
) -> Bool {
  expect  
      Some(Input {
        output_reference: field_reference,
        output: Output {
          address: field_address,
          datum: InlineDatum(field_data),
          reference_script: None,
          ..
        },
      })
    = tx.reference_inputs |> list.at(field_reference_input_index)
  expect Script(actual_field_script_hash) = field_address.payment_credential
  expect actual_field_script_hash == field_preimage_script_hash
  expect field: TxFieldPreimageV1 = field_data
  expect field.field_receipt_policy_id == own_policy

  expect  
      Some(Output {
        address: receipt_address,
        value: receipt_value,
        datum: InlineDatum(receipt_data),
        reference_script: None,
      })
    = tx.outputs |> list.at(receipt_output_index)
  expect Script(actual_receipt_script_hash) = receipt_address.payment_credential
  expect actual_receipt_script_hash == receipt_script_hash
  expect receipt: TxFieldReceiptV1 = receipt_data
  expect receipt.field_receipt_policy_id == own_policy
  expect receipt.tx_order_policy_id == field.tx_order_policy_id
  expect receipt.tx_order_id == field.tx_order_id
  expect receipt.transaction_commitment == field.transaction_commitment
  expect receipt.collection_proof == field.collection_proof
  expect receipt.chunk_index == field.proof.chunk_index
  expect receipt.field_reference == field_reference
  let predecessor = if predecessor_receipt_reference_input_index < 0 {
    expect predecessor_receipt_reference_input_index == -1
    expect receipt.predecessor_receipt_reference == None
    None
  } else {
    expect  
        Some(Input {
          output_reference: predecessor_reference,
          output: Output {
            address: predecessor_address,
            value: predecessor_value,
            datum: InlineDatum(predecessor_data),
            reference_script: None,
          },
        })
      = tx.reference_inputs
      |> list.at(predecessor_receipt_reference_input_index)
    expect Script(actual_predecessor_script_hash) = predecessor_address.payment_credential
    expect actual_predecessor_script_hash == receipt_script_hash
    expect predecessor_receipt: TxFieldReceiptV1 = predecessor_data
    expect receipt.predecessor_receipt_reference == Some(predecessor_reference)
    let predecessor_asset_name = field_receipt_asset_name(
      predecessor_receipt.tx_order_policy_id,
      predecessor_receipt.tx_order_id,
      predecessor_receipt.transaction_commitment,
      predecessor_receipt.collection_proof.field_index,
      predecessor_receipt.collection_proof.item_index,
      predecessor_receipt.chunk_index,
    )
    expect [Pair(actual_predecessor_asset_name, 1)] = predecessor_value
      |> tokens(own_policy)
      |> dict.to_pairs
    expect actual_predecessor_asset_name == predecessor_asset_name
    Some(predecessor_receipt)
  }

  let receipt_asset_name = field_receipt_asset_name(
    field.tx_order_policy_id,
    field.tx_order_id,
    field.transaction_commitment,
    field.proof.field_index,
    field.proof.item_index,
    field.proof.chunk_index,
  )
  expect [Pair(minted_asset_name, 1)] = tx.mint
    |> tokens(own_policy)
    |> dict.to_pairs
  expect minted_asset_name == receipt_asset_name
  expect [Pair(output_asset_name, 1)] = receipt_value
    |> tokens(own_policy)
    |> dict.to_pairs
  expect output_asset_name == receipt_asset_name

  and {
    native_transaction.verify_midgard_transaction_field_chunk_v1(
      transaction_id,
      field.transaction_commitment,
      source.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
      field.collection_proof,
      field.proof,
    ),
    verify_receipt_chain_link(
      transaction_id,
      field.transaction_commitment,
      source,
      own_policy,
      field.tx_order_policy_id,
      field.tx_order_id,
      predecessor,
      receipt,
    ),
  }
}

fn verify_receipt_burns(
  receipt_script_hash: ByteArray,
  own_policy: PolicyId,
  inputs: List<Input>,
  mint: Value,
  burned_tokens: Pairs<ByteArray, Int>,
  receipt_input_indices: List<Int>,
) -> Bool {
  when burned_tokens is {
    [] -> {
      expect [] = receipt_input_indices
      True
    }
    [Pair(asset_name, quantity), ..rest_tokens] -> {
      expect quantity == -1
      expect [input_index, ..rest_indices] = receipt_input_indices
      expect  
          Some(Input {
            output: Output {
              address,
              value,
              datum: InlineDatum(receipt_data),
              reference_script: None,
            },
            ..
          })
        = inputs |> list.at(input_index)
      expect Script(actual_receipt_script_hash) = address.payment_credential
      expect actual_receipt_script_hash == receipt_script_hash
      expect receipt: TxFieldReceiptV1 = receipt_data
      expect receipt.field_receipt_policy_id == own_policy
      expect
        field_receipt_asset_name(
          receipt.tx_order_policy_id,
          receipt.tx_order_id,
          receipt.transaction_commitment,
          receipt.collection_proof.field_index,
          receipt.collection_proof.item_index,
          receipt.chunk_index,
        ) == asset_name
      expect [Pair(input_asset_name, 1)] = value
        |> tokens(own_policy)
        |> dict.to_pairs
      expect input_asset_name == asset_name
      expect field_receipt_burn_authorized(receipt, mint)
      verify_receipt_burns(
        receipt_script_hash,
        own_policy,
        inputs,
        mint,
        rest_tokens,
        rest_indices,
      )
    }
  }
}

pub fn validate(
  field_preimage_script_hash: ByteArray,
  receipt_script_hash: ByteArray,
  redeemer: MintRedeemer,
  own_policy: PolicyId,
  tx: Transaction,
) -> Bool {
  when redeemer is {
    PublishField {
      field_reference_input_index,
      predecessor_receipt_reference_input_index,
      receipt_output_index,
      transaction_id,
      source,
    } ->
      verify_publication(
        field_preimage_script_hash,
        receipt_script_hash,
        own_policy,
        tx,
        field_reference_input_index,
        predecessor_receipt_reference_input_index,
        receipt_output_index,
        transaction_id,
        source,
      )
    BurnReceipts { receipt_input_indices } -> {
      let burned_tokens = tx.mint |> tokens(own_policy) |> dict.to_pairs
      expect list.length(burned_tokens) > 0
      expect
        list.length(list.unique(receipt_input_indices)) == list.length(
          receipt_input_indices,
        )
      verify_receipt_burns(
        receipt_script_hash,
        own_policy,
        tx.inputs,
        tx.mint,
        burned_tokens,
        receipt_input_indices,
      )
    }
  }
}
