use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine/witness as witness
use midgard/validation_resolution_v1 as resolution
use midgard/validation_semantic_v1.{Datum}
use midgard/validation_trace_v1.{Cek}

pub type SpendRedeemer =
  chain.ContextRedeemer

validator main(
  cek_context_control_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: chain.ContextRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        expect Some(step) = datum
        expect Some(prepared) = step.data
        let control = witness.cek_witness_control_v1(action.transition)
        let transition: Data = action.transition
        expect and {
            resolution.prepared_resolution_is_well_formed(prepared),
            prepared.resolution.pre_state.phase == Cek,
            resolution.hash_one_step_evidence(transition, action.auxiliary) == prepared.evidence_hash,
            witness.cek_witness_is_well_formed_v1(
              prepared.resolution.pre_state,
              action.transition,
              control,
            ),
            witness.cek_control_is_context_step_v1(control),
          }
        let prepared_data: Data = prepared
        let control_data: Data = control
        let next: Data = chain.ContextBound {
          prepared: prepared_data,
          control: control_data,
          auxiliary_hash: blake2b_256(cbor.serialise(action.auxiliary)),
          transaction_id: prepared.resolution.pre_state.transaction_id,
          source_kind: prepared.resolution.pre_state.source_kind,
        }
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) {
            hash == cek_context_control_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
