use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_traverse_v1 as traversal
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_redeemer_normalization_v1 as envelope

/// Both carriers use these same control gates. The original envelope remains
/// opaque to narrow semantic executors, while every exact control is retained.
pub type TraversalChecked {
  checking_next: Bool,
  envelope: envelope.PreparedScriptSourcesRedeemerEnvelopeV1,
  current_control: item.RedeemerItemProofControlV1,
  claimed_next: Data,
  witness_hash: ByteArray,
  checked_optional_traversal_cbor: ByteArray,
}

pub type CurrentChecked {
  envelope: envelope.PreparedScriptSourcesRedeemerEnvelopeV1,
  current_control: item.RedeemerItemProofControlV1,
  claimed_next: Data,
  witness_hash: ByteArray,
}

pub type ControlsChecked {
  envelope: envelope.PreparedScriptSourcesRedeemerEnvelopeV1,
  current_control: item.RedeemerItemProofControlV1,
  next_control: item.RedeemerItemProofControlV1,
  witness_hash: ByteArray,
}

pub type ExecutionOutput {
  deployment_id: ByteArray,
  family: Int,
  executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
  attestation: Data,
}

pub type TraversalExecution {
  output: ExecutionOutput,
  current: traversal.DataTraverseControlV1,
  next: traversal.DataTraverseControlV1,
  action: traversal.DataTraverseActionV1,
  source_bytes: Option<ByteArray>,
}

pub type OuterExecution {
  output: ExecutionOutput,
  current: item.RedeemerItemProofControlV1,
  next: item.RedeemerItemProofControlV1,
  action: item.RedeemerItemProofActionV1,
  source_bytes: Option<ByteArray>,
}

pub fn auxiliary_item_fields(auxiliary: Data) -> Pair<Data, Data> {
  let Pair(tag, fields) = builtin.un_constr_data(auxiliary)
  expect tag == 18
  expect [redeemer_control, current, witness] = fields
  let Pair(option_tag, option_fields) = builtin.un_constr_data(redeemer_control)
  expect option_tag == 1 && option_fields == []
  Pair(current, witness)
}

pub fn checked_optional_traversal_cbor(
  control: item.RedeemerItemProofControlV1,
) -> ByteArray {
  when control.traversal is {
    None -> #"d87a80"
    Some(inner) ->
      #"d8799f"
        |> bytearray.concat(traversal.encode_control_v1(inner))
        |> bytearray.concat(#"ff")
  }
}

pub fn raw_data_hash(data: Data) -> ByteArray {
  data |> cbor.serialise |> blake2b_256
}

pub fn execution_output(
  state: envelope.PreparedScriptSourcesRedeemerEnvelopeV1,
  witness_hash: ByteArray,
) -> ExecutionOutput {
  let attestation: Data = envelope.ScriptSourcesRedeemerExecutionAttestedStateV1 {
    version: envelope.version,
    domain: envelope.execution_attested_domain,
    deployment_id: state.deployment_id,
    base_provenance_identity: envelope.base_provenance_identity_v1(state),
    envelope_binder_script_hash: state.envelope_binder_script_hash,
    traversal_normalizer_script_hash: state.traversal_normalizer_script_hash,
    outer_normalizer_script_hash: state.outer_normalizer_script_hash,
    semantic_executor_script_hash: state.semantic_executor_script_hash,
    settlement_script_hash: state.settlement_script_hash,
    action_family: state.action_family,
    canonical_action_hash: state.canonical_action_hash,
    authenticated_traversal_action_identity: witness_hash,
    current_pending_item_control_hash: state.current_pending_item_control_hash,
    expected_next_item_control_hash: state.expected_next_item_control_hash,
    actual_next_item_control_hash: state.expected_next_item_control_hash,
    redeemer_count: state.redeemer_count,
    redeemer_total_count: state.redeemer_total_count,
  }
  ExecutionOutput {
    deployment_id: state.deployment_id,
    family: state.action_family,
    executor_script_hash: state.semantic_executor_script_hash,
    settlement_script_hash: state.settlement_script_hash,
    attestation,
  }
}

pub fn execution_output_is_exact(
  output: ExecutionOutput,
  deployment_id: ByteArray,
  family: Int,
  input_script_hash: ScriptHash,
  output_script_hash: ScriptHash,
  output_state: Data,
) -> Bool {
  and {
    output.deployment_id == deployment_id,
    output.family == family,
    output.executor_script_hash == input_script_hash,
    output.settlement_script_hash == output_script_hash,
    output.attestation == output_state,
  }
}

/// Deployment roster index derived from the authenticated action and current
/// traversal stage. Executors independently enforce their complete stage/action.
pub fn executor_index(current: Data, witness: Data, family: Int) -> Int {
  let Pair(witness_tag, witness_fields) = builtin.un_constr_data(witness)
  expect witness_tag == 0
  expect [action, _, _] = witness_fields
  let Pair(action_tag, action_fields) = builtin.un_constr_data(action)
  if family == envelope.fold_map_family {
    0
  } else if family == envelope.finalize_frame_family {
    1
  } else if family == envelope.open_header_family {
    2
  } else if family == envelope.open_tail_family {
    3
  } else if family == envelope.finish_data_family {
    16
  } else if family == envelope.invalid_family {
    if action_tag == 0 {
      17
    } else {
      expect action_tag == 1
      18
    }
  } else {
    expect action_tag == 2
    expect [traversal_action] = action_fields
    let Pair(traversal_action_tag, _) = builtin.un_constr_data(traversal_action)
    if family == envelope.head_family {
      expect traversal_action_tag >= 1 && traversal_action_tag <= 4
      traversal_action_tag + 3
    } else if family == envelope.fold_list_family {
      10
    } else {
      let Pair(control_tag, control_fields) = builtin.un_constr_data(current)
      expect control_tag == 0
      expect Some(optional_traversal) = list.at(control_fields, 15)
      let Pair(option_tag, option_fields) = builtin.un_constr_data(
        optional_traversal,
      )
      expect option_tag == 0
      expect [inner] = option_fields
      let Pair(inner_tag, inner_fields) = builtin.un_constr_data(inner)
      expect inner_tag == 0
      expect Some(stage_data) = list.at(inner_fields, 1)
      let stage = builtin.un_i_data(stage_data)
      if family == envelope.attach_scalar_family {
        expect stage == 1 || stage == 2
        stage + 7
      } else {
        expect family == envelope.advance_family
        expect stage >= 1 && stage <= 5
        stage + 10
      }
    }
  }
}
