use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, Redeemer, ScriptPurpose, Spend,
}
use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_v1 as proof
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  reject_invalid_field_type, reject_invalid_output, reject_native_script_depth,
  reject_native_script_node_count,
}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/shared.{rejected_successor_is_exact}
use midgard/validation_semantic_v1.{Datum}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

pub type Claim {
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  witness: Data,
  control: raw.Frame,
  next_control_cbor: ByteArray,
}

pub fn dispatch(
  dispatchers: List<ScriptHash>,
  expected_role: Int,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Claim {
  expect
      [
        Input {
          output_reference,
          output: Output { datum: InlineDatum(datum_data), .. },
        },
      ]
    = list.filter(
    inputs,
    fn(input) {
      when input.output.address.payment_credential is {
        Script(hash) -> list.has(dispatchers, hash)
        _ -> False
      }
    },
  )
  expect [Pair(_, redeemer)] = list.filter(
    redeemers,
    fn(pair) { pair.1st == Spend(output_reference) },
  )
  expect Pair(1, [action]) = builtin.un_constr_data(redeemer)
  expect
      Pair(0,
      [
        _input_index, _output_index, transition_data, witness_data, control_data,
        next_data, role_data, _ref_index,
      ])
    = builtin.un_constr_data(action)
  expect builtin.un_i_data(role_data) == expected_role
  expect transition: ValidationOneStepWitnessV1 = transition_data
  expect datum: Datum = datum_data
  expect ct.StepDatum { data: Some(state), .. } = datum
  Claim {
    pre: state.resolution.pre_state,
    transition,
    witness: witness_data,
    control: raw.open(builtin.un_b_data(control_data)),
    next_control_cbor: builtin.un_b_data(next_data),
  }
}

pub fn attest(claim: Claim, result: Option<stages.StepResult>) -> Bool {
  when result is {
    None -> False
    Some(stages.Advanced { control }) -> and {
        claim.next_control_cbor != #"",
        raw.encode(control) == claim.next_control_cbor,
      }
    Some(rejection) -> {
      let code = when rejection is {
        stages.InvalidOutput -> reject_invalid_output
        stages.InvalidReferenceScript -> reject_invalid_field_type
        stages.NativeScriptNodeLimit -> reject_native_script_node_count
        stages.NativeScriptDepthLimit -> reject_native_script_depth
        _ -> fail
      }
      and {
        claim.next_control_cbor == #"",
        rejected_successor_is_exact(
          claim.pre,
          claim.transition.claimed_successor,
          code,
        ),
      }
    }
  }
}

pub fn chunk_witness(data: Data) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if tag == 0 {
    expect fields == []
    proof.LedgerOutputProofNoWitness
  } else {
    expect tag == 1
    expect [first, second] = fields
    expect chunk_proof: bounded_item_v1.ChunkProofV1 = first
    expect next_chunk_proof: Option<bounded_item_v1.ChunkProofV1> = second
    proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof }
  }
}

pub fn value_witness(data: Data) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if tag == 0 {
    expect fields == []
    proof.LedgerOutputProofNoWitness
  } else {
    expect tag == 2
    expect [policy, asset, amount, path] = fields
    expect siblings: List<ByteArray> = path
    proof.LedgerOutputProofValue {
      policy_id: builtin.un_b_data(policy),
      asset_name: builtin.un_b_data(asset),
      quantity: builtin.un_i_data(amount),
      siblings,
    }
  }
}

pub fn native_witness(data: Data) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if tag == 4 {
    expect [frame_data] = fields
    expect frame: native_script_scan_v1.NativeScriptFrameV1 = frame_data
    proof.LedgerOutputProofNativeFrame { frame }
  } else {
    chunk_witness(data)
  }
}
