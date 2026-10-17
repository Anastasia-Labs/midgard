use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, Redeemer, ScriptPurpose, Spend,
}
use midgard/bounded_item_v1
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_value_v1 as value
use midgard/native_script_scan_v1

/// The shared step action's claims, read straight out of the dispatcher's
/// authenticated spend. The dispatcher's own generated code checked-decodes
/// its `StepRedeemer` in the same transaction, so this side reads the
/// structure with direct builtins: only the fields a yield consumes are
/// extracted.
pub type Claim {
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
fn dispatcher_action(
  dispatchers: List<ScriptHash>,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Data, List<Data>) {
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
  expect Pair(0, fields) = builtin.un_constr_data(action)
  (datum_data, fields)
}

fn dispatch_action(
  dispatchers: List<ScriptHash>,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Claim, Int) {
  let (_datum, fields) = dispatcher_action(dispatchers, inputs, redeemers)
  expect
      [
        _input_index, _output_index, _transition, witness_data, control_data,
        next_data, claimed_scalar, role_data, _ref_indices,
      ]
    = fields
  (
    Claim {
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

pub fn dispatch_attestation(
  dispatchers: List<ScriptHash>,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Claim {
  let (claim, _role) = dispatch_action(dispatchers, inputs, redeemers)
  claim
}

/// Large constructor stages only advance a control or refuse the step;
/// they never return a terminal rejection. Read exactly the fields they use,
/// while retaining the shared unique dispatcher/spend and exact role gates.
pub type AdvanceClaim {
  witness: Data,
  control: raw.Frame,
  next_control_cbor: ByteArray,
}

pub fn dispatch_advance(
  dispatchers: List<ScriptHash>,
  expected_role: Int,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> AdvanceClaim {
  let (_datum, fields) = dispatcher_action(dispatchers, inputs, redeemers)
  expect
      [
        _input_index, _output_index, _transition, witness, control_data,
        next_data, _claimed_scalar, role_data, _ref_indices,
      ]
    = fields
  expect builtin.un_i_data(role_data) == expected_role
  AdvanceClaim {
    witness,
    control: raw.open(builtin.un_b_data(control_data)),
    next_control_cbor: builtin.un_b_data(next_data),
  }
}

pub fn attest_advance(
  claim: AdvanceClaim,
  result: Option<stages.StepResult>,
) -> Bool {
  when result is {
    Some(stages.Advanced { control }) -> and {
        claim.next_control_cbor != #"",
        raw.encode(control) == claim.next_control_cbor,
      }
    _ -> False
  }
}

/// The span-attach step witness (`constr 5 [chunk_proof,
/// next_chunk_proof]`): the once-per-window chunk merkle verification the
/// span yield performs before the window's `(start, length, digest)` record,
/// derived from the pre control, is spliced into frame item 12.
pub fn span_attach_witness(data: Data) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  expect tag == 5
  expect [first_data, second_data] = fields
  expect chunk_proof: bounded_item_v1.ChunkProofV1 = first_data
  expect next_chunk_proof: Option<bounded_item_v1.ChunkProofV1> = second_data
  proof.LedgerOutputProofSpanAttach { chunk_proof, next_chunk_proof }
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

/// A stage that refuses the output yields its failure class, not a reason:
/// the class is the one-byte marker the dispatcher's action carries in place
/// of a successor control, and each dispatcher maps (class, its own pending
/// coordinates) to the exact rejection reason and checks the rejected
/// successor itself.
pub fn rejection_class_marker(result: stages.StepResult) -> ByteArray {
  when result is {
    stages.InvalidOutput -> #"00"
    stages.InvalidReferenceScript -> #"01"
    stages.NativeScriptNodeLimit -> #"02"
    stages.NativeScriptDepthLimit -> #"03"
    stages.Advanced { .. } -> fail
  }
}

/// The class a one-byte marker names, `0..3` in `rejection_class_marker`
/// order, or `None` for a successor control.
pub fn rejection_class_of(next_control_cbor: ByteArray) -> Option<Int> {
  if bytearray.length(next_control_cbor) == 1 {
    let class = builtin.index_bytearray(next_control_cbor, 0)
    if class <= 3 {
      Some(class)
    } else {
      fail
    }
  } else {
    None
  }
}

pub fn attest(claim: Claim, result: Option<stages.StepResult>) -> Bool {
  when result is {
    None -> False
    Some(stages.Advanced { control }) ->
      raw.encode(control) == claim.next_control_cbor
    Some(rejection) ->
      claim.next_control_cbor == rejection_class_marker(rejection)
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
    expect [index, policy, asset, amount, path, head] = fields
    expect siblings: List<ByteArray> = path
    expect previous: Option<value.LedgerOutputValueHeadV1> = head
    proof.LedgerOutputProofValue {
      asset_index: builtin.un_i_data(index),
      policy_id: builtin.un_b_data(policy),
      asset_name: builtin.un_b_data(asset),
      quantity: builtin.un_i_data(amount),
      siblings,
      previous,
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
