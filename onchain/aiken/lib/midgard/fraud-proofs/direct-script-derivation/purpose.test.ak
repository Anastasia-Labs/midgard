use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor

const low = #"1111111111111111111111111111111111111111111111111111111111111111"

const middle =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const high = #"3333333333333333333333333333333333333333333333333333333333333333"

const script_hash = #"22222222222222222222222222222222222222222222222222222222"

fn selected_descriptor(output_index: Int) -> ByteArray {
  let summary = DataSummaryV1 { root: low, cbor_length: 1, memory: 1 }
  descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index,
      total_length: 50,
      item_commitment: low,
      address: bytearray.concat(#"78", script_hash),
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: low,
      cardano_value_size: 5,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
}

fn ascending_purpose(
  index: Int,
  selected_id: ByteArray,
  selected_output_index: Int,
) {
  let inputs = [
    native_types.MidgardTxInput { tx_id: low, output_index: 7 },
    native_types.MidgardTxInput { tx_id: middle, output_index: 2 },
    native_types.MidgardTxInput { tx_id: high, output_index: 9 },
  ]
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, inputs },
    },
  )
  let key = ledger_outref_key(
    OutputReference {
      transaction_id: selected_id,
      output_index: selected_output_index,
    },
  )
  let value = selected_descriptor(selected_output_index)
  let pre_root = mpf.insert(mpf.empty, key, value, []) |> mpf.root
  purpose.open_spend_v1(
    BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: block.tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: pre_root,
    },
    index,
    fixture.inline_body_opening(
      block.compact_cbor,
      list.map(inputs, encode_midgard_tx_input),
    ),
    value,
    RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
    [],
    [fixture.phas_withdraw_redeemer_entry(pre_root, key, value)],
    fixture.test_certificate_policy_id,
  )
}

fn check_purpose(
  index: Int,
  selected_id: ByteArray,
  selected_output_index: Int,
) -> Bool {
  expect Some(opened) = ascending_purpose(
    index,
    selected_id,
    selected_output_index,
  )
  and {
    opened.script_hash == script_hash,
    opened.subject == encode_midgard_tx_input(
      native_types.MidgardTxInput {
        tx_id: selected_id,
        output_index: selected_output_index,
      },
    ),
  }
}

test ascending_spend_ordinal_zero_opens_first_raw_input() {
  check_purpose(0, low, 7)
}

test ascending_spend_ordinal_one_opens_middle_raw_input() {
  check_purpose(1, middle, 2)
}

test ascending_spend_ordinal_two_opens_last_raw_input() {
  check_purpose(2, high, 9)
}

test negative_spend_ordinal_is_refused() fail {
  ascending_purpose(-1, low, 7) != None
}

test spend_ordinal_equal_to_authenticated_count_is_refused() fail {
  ascending_purpose(3, low, 7) != None
}

test spend_ordinal_cannot_substitute_another_outref() fail {
  ascending_purpose(0, high, 9) != None
}
