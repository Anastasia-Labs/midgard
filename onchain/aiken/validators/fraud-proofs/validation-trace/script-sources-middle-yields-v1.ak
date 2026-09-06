use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/bounded_item_v1
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/validation_machine/script_sources_middle_semantics as semantic
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_script_sources_yield_v1.{middle_dispatch}

validator stage_two_advance(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      0,
      0,
    )
    expect [] = fields
    semantic.stage_two_advance(pre, transition)
  }

  else(_) {
    fail
  }
}

validator stage_three_replay(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      1,
      7,
    )
    expect [kind, key, next, value] = fields
    semantic.stage_three_replay(
      pre,
      transition,
      builtin.un_i_data(kind),
      builtin.un_b_data(key),
      builtin.un_b_data(next),
      builtin.un_b_data(value),
    )
  }

  else(_) {
    fail
  }
}

validator stage_three_finish(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      2,
      0,
    )
    expect [] = fields
    semantic.stage_three_finish(pre, transition)
  }

  else(_) {
    fail
  }
}

validator stage_four_begin(
  dispatcher_script_hashes: List<ScriptHash>,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      3,
      29,
    )
    expect [carriage_data] = fields
    expect carriage: FieldCarriageV1 = carriage_data
    semantic.stage_four_begin(
      pre,
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

validator stage_four_finish(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      4,
      0,
    )
    expect [] = fields
    semantic.stage_four_finish(pre, transition)
  }

  else(_) {
    fail
  }
}

validator stage_six_begin_policy(
  dispatcher_script_hashes: List<ScriptHash>,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      5,
      1,
    )
    expect [field_index, item_index, carriage_data] = fields
    expect carriage: FieldCarriageV1 = carriage_data
    semantic.stage_six_begin(
      pre,
      transition,
      MachineFieldDoorV1 {
        reference_inputs: tx.reference_inputs,
        certificate_policy_id: field_preimage_certificate_policy_id,
      },
      builtin.un_i_data(field_index),
      builtin.un_i_data(item_index),
      carriage,
    )
  }

  else(_) {
    fail
  }
}

validator stage_six_fold_asset(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      6,
      39,
    )
    expect [chunk, next_chunk] = fields
    expect proof: bounded_item_v1.ChunkProofV1 = chunk
    expect next: Option<bounded_item_v1.ChunkProofV1> = next_chunk
    semantic.stage_six_asset(pre, transition, proof, next)
  }

  else(_) {
    fail
  }
}

validator stage_six_finish(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (pre, transition, fields) = middle_dispatch(
      dispatcher_script_hashes,
      tx,
      7,
      0,
    )
    expect [] = fields
    semantic.stage_six_finish(pre, transition)
  }

  else(_) {
    fail
  }
}
