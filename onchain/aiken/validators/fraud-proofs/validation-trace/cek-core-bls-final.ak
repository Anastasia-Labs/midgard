use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.CoreDatum>,
    redeemer: chain.ArmRedeemer,
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
        expect bound.group == 8
        expect bound.pre.mode == machine.mode_builtin
        expect
            Pair(37,
            [
              _left_root_data, _right_root_data, left_data, right_data,
              result_data,
            ])
          = builtin.un_constr_data(action.witness)
        expect result: builtin_witness.ValueWitnessV1 = result_data
        expect bound.arm == 37
        expect left: builtin_witness.BlsExpressionWitnessV1 = left_data
        expect right: builtin_witness.BlsExpressionWitnessV1 = right_data
        expect
          builtin_witness.verify_authenticated_bls_final(left, right, result)
        chain.continue_core(
          settle_script_hash,
          computation_thread_policy_id,
          step,
          action,
          2,
          bound.facts,
          own_out_ref,
          tx,
        )
      }
    }
  }

  else(_) {
    fail
  }
}
