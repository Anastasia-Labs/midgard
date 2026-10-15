use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/cek_selection_witness.{decode_selection_auxiliary}
use midgard/cek_selection_yield as selection
use midgard/validation_machine/cek.{
  authenticate_cek_execution_selection, verify_cek_execution_selection_successor,
}
use midgard/validation_machine/witness.{cek_witness_control_v1}

validator authenticate(dispatcher_script_hash: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (_, action, _) = selection.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    let cek = cek_witness_control_v1(action.transition)
    authenticate_cek_execution_selection(
      decode_selection_auxiliary(action.auxiliary),
      cek.control,
      cek.execution_cursor,
    )
  }

  else(_) {
    fail
  }
}

/// `script_decode_policy_id` is the policy of the `script-decode-v1` family,
/// whose record token authenticates the program a Plutus selection runs.
validator successor(
  dispatcher_script_hash: ScriptHash,
  script_decode_policy_id: PolicyId,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, action, _) = selection.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect Some(state) = datum.data
    let auxiliary = decode_selection_auxiliary(action.auxiliary)
    let cek = cek_witness_control_v1(action.transition)
    // A native selection, and a PlutusV3 receive purpose (refused before
    // any program is read), name no record: index -1.
    let record = if action.record_reference_input_index < 0 {
      None
    } else {
      Some(
        selection.decode_record_at(
          tx.reference_inputs,
          action.record_reference_input_index,
          script_decode_policy_id,
        ),
      )
    }
    verify_cek_execution_selection_successor(
      state.resolution.pre_state,
      action.transition,
      auxiliary,
      cek.control,
      cek.execution_cursor,
      cek.completed_cpu,
      cek.completed_memory,
      record,
    )
  }

  else(_) {
    fail
  }
}
