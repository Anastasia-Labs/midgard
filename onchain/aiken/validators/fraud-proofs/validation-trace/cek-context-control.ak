use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_step
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine/machine_types.{CekWitnessControlV1}
use midgard/validation_machine/witness as witness

validator main(
  stage_script_hashes: List<ScriptHash>,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.BoundDatum>,
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
        expect Some(bound) = step.data
        expect cek: CekWitnessControlV1 = bound.control
        let context = witness.cek_context_control_from_cbor(
          cek.context_control_cbor,
        )
        expect and {
            chain.auxiliary_is_bound(bound, action.auxiliary),
            cek.execution_cursor < cek.control.execution_count,
            cek.execution_cpu_limit == 0,
            cek.execution_memory_limit == 0,
            cek.program_envelope_hash == context.program_envelope_hash,
            cek.context_control_cbor == witness.encode_cek_context_control_v1(
              context,
            ),
            witness.cek_context_control_is_well_formed(cek.control, context),
          }
        expect context.stage != 7
        let route = if context.stage == 0 {
          if context.redeemer_context_control_hash == #"" {
            0
          } else {
            1
          }
        } else if context.stage >= 1 && context.stage <= 6 {
          context.stage + 1
        } else if context.stage == 8 {
          8
        } else if context.stage == 9 {
          if builtin.un_constr_data(action.auxiliary).1st == 18 {
            1
          } else {
            9
          }
        } else {
          context.stage
        }
        expect Some(next_hash) = list.at(stage_script_hashes, route)
        let next: Data = chain.ContextStaged {
          bound,
          context,
          native: cek_context_step.native_facts(cek.control),
          execution_cursor: cek.execution_cursor,
        }
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) { hash == next_hash && state == next },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
