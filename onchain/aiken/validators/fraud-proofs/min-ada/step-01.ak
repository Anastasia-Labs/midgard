//// Q27 min-Ada step 01: authenticate either the accepted transaction source
//// or the post-block descriptor membership and freeze the predecessor root.

use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{
  NativeTxInclusionCarriage, cancel, continue,
  pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/min_ada/step_01.{
  Args, Datum, MinAdaFaultV1, MinAdaTx, MinAdaUtxo, PostUtxoMembershipV1,
  PostUtxoStateV1, SpendRedeemer,
}
use midgard/fraud_proofs/min_ada/step_02 as step_02
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/hub_oracle as hub
use midgard/ledger_output_commitment_v1
use midgard/state_queue

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { tx_inclusion, post_utxo_membership, fault }) ->
        when fault is {
          MinAdaTx { output_index } -> {
            expect output_index >= 0
            expect Some(inclusion) = tx_inclusion
            expect None = post_utxo_membership
            pass_transaction_claim(
              step_02_validator_script_hash,
              computation_thread_token_policy_id,
              hub_oracle,
              datum,
              inclusion,
              fault,
              own_out_ref,
              tx,
            )
          }
          MinAdaUtxo -> {
            expect None = tx_inclusion
            expect Some(membership) = post_utxo_membership
            pass_post_utxo_claim(
              step_02_validator_script_hash,
              computation_thread_token_policy_id,
              hub_oracle,
              datum,
              membership,
              fault,
              own_out_ref,
              tx,
            )
          }
        }
    }
  }

  else(_) {
    fail
  }
}

fn pass_transaction_claim(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  datum: Option<Datum>,
  tx_inclusion: NativeTxInclusionCarriage,
  fault: MinAdaFaultV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let
      _own_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data,
      _header,
      bad_tx_id,
      bad_tx_view
    <- pass_native_tx_to_next_step_carried(
    computation_thread_token_policy_id,
    hub_oracle,
    datum,
    tx_inclusion,
    own_out_ref,
    tx,
  )
  expect None = m_input_state_data
  expect bad_tx_view.tx_compact.validity_code == 0
  expect output_script_hash == step_02_validator_script_hash
  let expected_output_state: Data = step_02.State {
    bad_tx_id,
    fault,
    post_utxo: None,
  }
  expect output_state_data == expected_output_state
  True
}

fn pass_post_utxo_claim(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  datum: Option<Datum>,
  membership: PostUtxoMembershipV1,
  fault: MinAdaFaultV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, .. } = tx
  expect Some(step_datum) = datum
  let
      _own_script_hash,
      computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    membership.input_index,
    membership.output_index,
    own_out_ref,
    inputs,
    outputs,
  )
  expect None = m_input_state_data
  expect output_script_hash == step_02_validator_script_hash

  let hub.Datum { state_queue: state_queue_policy, .. } = hub.get_datum(
    reference_inputs,
    hub_oracle,
    membership.hub_ref_input_index,
  )
  let header, state_queue_node_key <- state_queue.get_block_datum_v1(
    reference_inputs,
    state_queue_policy,
    membership.state_queue_node_ref_input_index,
  )
  expect
    state_queue_node_key == bytearray.drop(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    )
  let descriptor = ledger_output_commitment_v1.decode(
    membership.descriptor_cbor,
  )
  expect descriptor.output_index == membership.out_ref.output_index
  let expected_output_state: Data = step_02.State {
    bad_tx_id: membership.out_ref.transaction_id,
    fault,
    post_utxo: Some(
      PostUtxoStateV1 {
        out_ref: membership.out_ref,
        descriptor_cbor: membership.descriptor_cbor,
        post_utxos_root: header.utxos_root,
        prev_utxos_root: header.prev_utxos_root,
      },
    ),
  }
  expect output_state_data == expected_output_state
  True
}
