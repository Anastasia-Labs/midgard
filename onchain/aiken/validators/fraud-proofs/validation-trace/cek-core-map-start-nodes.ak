use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_cost_v1 as cost
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
        expect bound.group == 6
        expect bound.pre.mode == machine.mode_builtin
        expect bound.arm == 32
        let item = chain.map_item(bound, action.witness)
        expect material: machine.MapConversionStartWitnessV1 = item.material
        expect projection: chain.MapProjection = bound.facts
        expect budget: cost.BuiltinBudgetV1 = projection.budget
        expect
          machine.verify_authenticated_map_conversion_nodes(
            bound.pre,
            bound.post,
            item.tag,
            item.arguments,
            item.result,
            material,
            budget,
          )
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
