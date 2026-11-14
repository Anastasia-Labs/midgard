use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/validation_machine/script_sources_late_stages as semantics
use midgard/validation_semantic_yield_v1

validator main(observer_dispatcher_script_hash: ScriptHash) {
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
    semantics.verify_script_sources_stage_seven_observer_bound_semantics_v1(
      state.resolution.pre_state,
      transition,
      observer_hash,
      active_count,
    )
  }

  else(_) {
    fail
  }
}
