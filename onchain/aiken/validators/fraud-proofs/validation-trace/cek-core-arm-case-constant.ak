//// Core group 11: `case` on a constant (`ReturnCaseConstant`). It has its own
//// arm so that the machine arm's other return steps do not carry its code.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1
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
        // `core_group` binds group 11 only to a return step with witness
        // constructor 41, and `continue_core` checks the witness against the
        // bound constructor and hash.
        expect bound.group == 11
        let fields = builtin.un_constr_data(action.witness).2nd
        expect [scrutinee,
              material, count, branches, environment, tail] = fields
        expect scrutinee: cek_builtin_v1.ValueWitnessV1 = scrutinee
        expect material: cek_builtin_v1.SemanticBuiltinWitnessV1 = material
        expect count: Int = count
        expect branches: ByteArray = branches
        expect environment: ByteArray = environment
        expect tail: ByteArray = tail
        expect
          machine.verify_return_case_constant(
            bound.pre,
            bound.post,
            scrutinee,
            material,
            count,
            branches,
            environment,
            tail,
          )
        chain.continue_core(
          settle_script_hash,
          computation_thread_policy_id,
          step,
          action,
          0,
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
