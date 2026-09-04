use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/step_05.{
  Args, Datum, FinalizeSignerScan, ResumeSignerScan, SpendRedeemer, State,
  staged_signer_finalize_batch_limit, staged_signer_resume_batch_limit,
}
use midgard/fraud_proofs/execution_native_script_invalid/step_06
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, address_witnesses_field_index, resume_opened_field_walk,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/native_tx_machine_walk_v1 as machine_walk
use midgard/script_proof_v1
use midgard/validation_merkle_v1

type SignerAccumulatorV1 {
  previous_signer_hash: ByteArray,
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
}

validator main(
  step_06_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(action) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index_of(action),
          output_index_of(action),
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = m_input_state_data
        let Pair(view, resumed) = resume_opened_field_walk(
          opening_of(action),
          WitnessAnchor {
            tx_id: state.bad_tx_id,
            witness_set_hash: state.bad_tx_witness_set_hash,
          },
          address_witnesses_field_index,
          state.signer_checkpoint_hash,
          checkpoint_of(action),
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let Pair(accumulator, next) = machine_walk.walk_fold(
          view,
          resumed,
          budget_of(action),
          SignerAccumulatorV1 {
            previous_signer_hash: state.previous_signer_hash,
            signer_count: state.signer_count,
            signer_peaks: state.signer_peaks,
          },
          fn(accumulator, _index, item) {
            append_signer(accumulator, signer_hash_of(item))
          },
        )
        when action is {
          ResumeSignerScan { item_budget, .. } -> {
            expect item_budget > 0
            expect item_budget <= staged_signer_resume_batch_limit
            expect !machine_walk.walk_is_complete(next)
            expect output_script_hash == own_script_hash
            let expected_output_state: Data = State {
              ..state,
              signer_checkpoint_hash: machine_walk.field_walk_checkpoint_hash(
                next,
              ),
              previous_signer_hash: accumulator.previous_signer_hash,
              signer_count: accumulator.signer_count,
              signer_peaks: accumulator.signer_peaks,
            }
            expect output_state_data == expected_output_state
            True
          }
          FinalizeSignerScan { item_budget, .. } -> {
            expect item_budget >= 0
            expect item_budget <= staged_signer_finalize_batch_limit
            expect machine_walk.walk_is_complete(next)
            expect output_script_hash == step_06_validator_script_hash
            let expected_output_state: Data = step_06.State {
              direction: state.direction,
              execution_index: state.execution_index,
              source_index: state.source_index,
              origin_kind: state.origin_kind,
              item_commitment: state.item_commitment,
              bad_tx_id: state.bad_tx_id,
              script_item_hash: state.script_item_hash,
              validity_interval_start: state.validity_interval_start,
              validity_interval_end: state.validity_interval_end,
              signer_count: accumulator.signer_count,
              signer_peaks: accumulator.signer_peaks,
              phase: step_06.ScriptReady,
            }
            expect output_state_data == expected_output_state
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn input_index_of(action: Args) -> Int {
  when action is {
    ResumeSignerScan { input_index, .. } -> input_index
    FinalizeSignerScan { input_index, .. } -> input_index
  }
}

fn output_index_of(action: Args) -> Int {
  when action is {
    ResumeSignerScan { output_index, .. } -> output_index
    FinalizeSignerScan { output_index, .. } -> output_index
  }
}

fn opening_of(action: Args) {
  when action is {
    ResumeSignerScan { addr_tx_wits_opening, .. } -> addr_tx_wits_opening
    FinalizeSignerScan { addr_tx_wits_opening, .. } -> addr_tx_wits_opening
  }
}

fn checkpoint_of(action: Args) -> ByteArray {
  when action is {
    ResumeSignerScan { checkpoint_bytes, .. } -> checkpoint_bytes
    FinalizeSignerScan { checkpoint_bytes, .. } -> checkpoint_bytes
  }
}

fn budget_of(action: Args) -> Int {
  when action is {
    ResumeSignerScan { item_budget, .. } -> item_budget
    FinalizeSignerScan { item_budget, .. } -> item_budget
  }
}

fn signer_hash_of(item: ByteArray) -> ByteArray {
  let witness = decode_midgard_address_witness_cbor(item)
  expect encode_midgard_address_witness(witness) == item
  utils.get_verification_key_hash(witness.verification_key)
}

fn append_signer(
  accumulator: SignerAccumulatorV1,
  signer_hash: ByteArray,
) -> SignerAccumulatorV1 {
  expect or {
      accumulator.previous_signer_hash == #"",
      bytearray.compare(accumulator.previous_signer_hash, signer_hash) != Greater,
    }
  if accumulator.previous_signer_hash == signer_hash {
    accumulator
  } else {
    SignerAccumulatorV1 {
      previous_signer_hash: signer_hash,
      signer_count: accumulator.signer_count + 1,
      signer_peaks: validation_merkle_v1.append_leaf(
        accumulator.signer_count,
        accumulator.signer_peaks,
        script_proof_v1.signer_leaf_hash(signer_hash),
      ),
    }
  }
}
