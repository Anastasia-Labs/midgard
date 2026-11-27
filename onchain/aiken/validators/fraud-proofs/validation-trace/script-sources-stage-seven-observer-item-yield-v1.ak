use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/validation_machine/script_sources_late_stages as semantics
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_semantic_yield_v1

validator main(
  observer_dispatcher_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let (state, transition, extra) = validation_semantic_yield_v1.unique_semantic_dispatch_v1(
      [observer_dispatcher_script_hash],
      tx,
    )
    expect
        [
          field_index_data, item_index_data, carriage_data, hash_data,
          count_data, _, _,
        ]
      = extra
    let observer_hash = builtin.un_b_data(hash_data)
    let active_count = builtin.un_i_data(count_data)
    expect carriage: FieldCarriageV1 = carriage_data
    semantics.verify_script_sources_stage_seven_observer_item_facts_v1(
      state.resolution.pre_state,
      transition,
      MachineFieldDoorV1 {
        reference_inputs: tx.reference_inputs,
        certificate_policy_id: field_preimage_certificate_policy_id,
      },
      builtin.un_i_data(field_index_data),
      builtin.un_i_data(item_index_data),
      carriage,
      observer_hash,
      active_count,
    )
  }

  else(_) {
    fail
  }
}
