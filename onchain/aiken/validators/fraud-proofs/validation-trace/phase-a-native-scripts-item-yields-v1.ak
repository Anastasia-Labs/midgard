use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/validation_trace/phase_a_native_item_yield_v1.{
  VerifyItem, unique_dispatch,
}
use midgard/validation_machine/phase_a_native_scripts.{
  verify_phase_a_native_item_foreign_payload_v1,
  verify_phase_a_native_item_native_payload_v1,
}
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_resolution_v1.{winning_resolution}

validator native(
  dispatcher_script_hash: ScriptHash,
  award_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(_yield_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (step_datum, redeemer, _) = unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect ct.Continue(VerifyItem { output_index, transition, carriage, .. }) = redeemer
    expect Some(state) = step_datum.data
    expect Some(output) = list.at(tx.outputs, output_index)
    expect output.address.payment_credential == Script(award_script_hash)
    expect InlineDatum(output_data) = output.datum
    expect output_datum: ct.StepDatum<Data> = output_data
    let expected: Data = winning_resolution()
    expect output_datum.data == Some(expected)
    verify_phase_a_native_item_native_payload_v1(
      state.resolution.pre_state,
      transition,
      MachineFieldDoorV1 {
        reference_inputs: tx.reference_inputs,
        certificate_policy_id: field_preimage_certificate_policy_id,
      },
      carriage,
    )
  }

  else(_) {
    fail
  }
}

validator foreign(
  dispatcher_script_hash: ScriptHash,
  award_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(_yield_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (step_datum, redeemer, _) = unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect ct.Continue(VerifyItem { output_index, transition, carriage, .. }) = redeemer
    expect Some(state) = step_datum.data
    expect Some(output) = list.at(tx.outputs, output_index)
    expect output.address.payment_credential == Script(award_script_hash)
    expect InlineDatum(output_data) = output.datum
    expect output_datum: ct.StepDatum<Data> = output_data
    let expected: Data = winning_resolution()
    expect output_datum.data == Some(expected)
    verify_phase_a_native_item_foreign_payload_v1(
      state.resolution.pre_state,
      transition,
      MachineFieldDoorV1 {
        reference_inputs: tx.reference_inputs,
        certificate_policy_id: field_preimage_certificate_policy_id,
      },
      carriage,
    )
  }

  else(_) {
    fail
  }
}
