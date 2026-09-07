use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, Redeemer, ScriptPurpose, Spend,
}
use midgard/bounded_item_v1
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_v1 as proof
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  reject_invalid_field_type, reject_invalid_output, reject_native_script_depth,
  reject_native_script_node_count,
}
use midgard/validation_machine/shared.{encode_terminal_rejection_witness}
use midgard/validation_trace_v1

/// The shared step action's claims, read straight out of the dispatcher's
/// authenticated spend. The dispatcher's own generated code checked-decodes
/// its datum and its `StepRedeemer` (transition included) in the same
/// transaction, so this side reads the two structures with direct builtins:
/// only the fields a yield consumes are extracted, and the rejection check
/// works on the successor state's data representation.
pub type Claim {
  pre_program_counter: Int,
  pre_prior_ledger_root: ByteArray,
  /// `ValidationMachineStateV1` on the wire (the transition's
  /// `claimed_successor`), checked by the dispatcher's redeemer decode.
  claimed_successor: Data,
  witness: Data,
  control: raw.Frame,
  next_control_cbor: ByteArray,
  claimed_scalar: Data,
}

/// Open the one dispatcher input's shared nine-field step action. Every yield
/// of a step transaction — the stage yield and the scalar attestation
/// yields — reads the same action through this function, so the claims they
/// verify and the claims the stage yield consumes are one value; the
/// dispatcher never has to compare claims across yields.
fn dispatch_action(
  dispatchers: List<ScriptHash>,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Claim, Int) {
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
        next_data, claimed_scalar, role_data, _ref_indices,
      ])
    = builtin.un_constr_data(action)
  // `StepRedeemer` transition: [work_witness_cbor, claimed_successor]; the
  // dispatcher's checked redeemer decode pins the shape.
  let Pair(_, transition_fields) = builtin.un_constr_data(transition_data)
  expect [_work_witness_cbor, claimed_successor] = transition_fields
  // `StepDatum { fraud_prover, data: Some(prepared) }` with
  // `prepared.resolution.pre_state` the machine pre state; the dispatcher's
  // checked datum decode pins the shape.
  let Pair(_, datum_fields) = builtin.un_constr_data(datum_data)
  expect [_fraud_prover, state_option] = datum_fields
  expect Pair(0, [prepared]) = builtin.un_constr_data(state_option)
  let Pair(_, prepared_fields) = builtin.un_constr_data(prepared)
  expect [_prepared_version, resolution, _evidence_hash] = prepared_fields
  let Pair(_, resolution_fields) = builtin.un_constr_data(resolution)
  expect [_resolution_version,
        pre_state, _operator_hash, _challenger_hash] = resolution_fields
  let Pair(_, pre_fields) = builtin.un_constr_data(pre_state)
  expect
      [
        _machine_version, _event_key_hash, _transaction_id,
        _transaction_commitment, _validation_context_hash, _source_kind,
        prior_ledger_root, _phase, program_counter, _work_root, _cpu, _memory,
        _verdict, _rejection_code_hash, _ledger_delta_root,
      ]
    = pre_fields
  (
    Claim {
      pre_program_counter: builtin.un_i_data(program_counter),
      pre_prior_ledger_root: builtin.un_b_data(prior_ledger_root),
      claimed_successor,
      witness: witness_data,
      control: raw.open(builtin.un_b_data(control_data)),
      next_control_cbor: builtin.un_b_data(next_data),
      claimed_scalar,
    },
    builtin.un_i_data(role_data),
  )
}

pub fn dispatch(
  dispatchers: List<ScriptHash>,
  expected_role: Int,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Claim {
  let (claim, role) = dispatch_action(dispatchers, inputs, redeemers)
  expect role == expected_role
  claim
}

/// An attestation yield serves every stage role, so it pins no role index;
/// the dispatcher's role table decides which attestations a role requires,
/// and `require_authenticated_zero_yield` binds each required role name to
/// the withdrawal that runs this dispatch.
pub fn dispatch_attestation(
  dispatchers: List<ScriptHash>,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Claim {
  let (claim, _role) = dispatch_action(dispatchers, inputs, redeemers)
  claim
}

/// The span-attach step witness (`constr 5 [start, length, chunk_proof,
/// next_chunk_proof]`): the once-per-window chunk merkle verification the
/// span yield performs before the window's `(start, length, digest)` record
/// is spliced into frame item 12.
pub fn span_attach_witness(data: Data) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  expect tag == 5
  expect [start_data, length_data, first_data, second_data] = fields
  expect chunk_proof: bounded_item_v1.ChunkProofV1 = first_data
  expect next_chunk_proof: Option<bounded_item_v1.ChunkProofV1> = second_data
  proof.LedgerOutputProofSpanAttach {
    start: builtin.un_i_data(start_data),
    length: builtin.un_i_data(length_data),
    chunk_proof,
    next_chunk_proof,
  }
}

/// The window step witness of the chunk-cursor stages (`constr 6 [bytes]`,
/// bound to the recorded span window by digest), or the plain no-witness
/// transition (`constr 0 []`).
pub fn window_witness(data: Data) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if tag == 0 {
    expect fields == []
    proof.LedgerOutputProofNoWitness
  } else {
    expect tag == 6
    expect [bytes_data] = fields
    proof.LedgerOutputProofWindow { bytes: builtin.un_b_data(bytes_data) }
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
        rejected_successor_data_is_exact(claim, code),
      }
    }
  }
}

/// `validation_machine/shared.rejected_successor_is_exact` on the successor
/// state's data representation: phase Terminal, verdict Rejected, the exact
/// rejection-code hash, and the exact terminal-rejection work root — clause
/// for clause, with the two enum clauses as data equality on the enums'
/// constructor tags (Terminal = 14, Rejected = 2). Deliberately no clause on
/// `ledger_delta_root`; see the NOTE on the typed function.
fn rejected_successor_data_is_exact(claim: Claim, code: ByteArray) -> Bool {
  let Pair(_, fields) = builtin.un_constr_data(claim.claimed_successor)
  expect
      [
        _machine_version, _event_key_hash, _transaction_id,
        _transaction_commitment, _validation_context_hash, _source_kind,
        _prior_ledger_root, phase, _program_counter, work_root, _cpu, _memory,
        verdict, rejection_code_hash, _ledger_delta_root,
      ]
    = fields
  and {
    phase == builtin.constr_data(14, []),
    verdict == builtin.constr_data(2, []),
    builtin.un_b_data(rejection_code_hash) == validation_trace_v1.hash_rejection_code(
      code,
    ),
    builtin.un_b_data(work_root) == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      claim.pre_program_counter + 1,
      encode_terminal_rejection_witness(code, claim.pre_prior_ledger_root),
    ),
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
