use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/ledger_state
use midgard/mpf_proof_v1
use midgard/rejection_reason_v1.{
  InputAssetAccumulationLimit, MintAssetAccumulationLimit,
  OutputAssetAccumulationLimit, reject_asset_count,
}
use midgard/transition_trace
use midgard/validation_machine/machine_types.{
  ValueAndMintControlV1, ValueAssetMutationWitnessV1,
}
use midgard/validation_machine/shared.{mint_asset_leaf_hash}
use midgard/validation_machine/witness.{
  encode_value_and_mint_control_v1, resolution_schedule_node_hash,
}
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/validation_trace_v1

pub const input_fold: Int = 0

pub const output_fold: Int = 1

pub const mint_fold: Int = 2

pub type CoordinateV1 {
  CoordinateV1 { fold: Int, primary_index: Int, asset_index: Int }
}

pub type BoundV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  coordinate: CoordinateV1,
}

pub type FoldStateV1 {
  bound: BoundV1,
  control: Option<ValueAndMintControlV1>,
  stage: Int,
  decisive_fault_holds: Option<Bool>,
}

pub fn coordinate_is_well_formed_v1(coordinate: CoordinateV1) -> Bool {
  and {
    coordinate.fold >= 0,
    coordinate.fold <= 2,
    coordinate.primary_index >= 0,
    coordinate.asset_index >= 0,
    if coordinate.fold == mint_fold {
      coordinate.asset_index == 0
    } else {
      True
    },
  }
}

pub fn bind_coordinate_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  coordinate: CoordinateV1,
) -> BoundV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect bytearray.length(validation_traces_root) == 32
  expect validation_trace_count > 0
  expect coordinate_is_well_formed_v1(coordinate)
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    let expected = when coordinate.fold is {
      0 ->
        InputAssetAccumulationLimit {
          input_index: coordinate.primary_index,
          asset_index: coordinate.asset_index,
        }
      1 ->
        OutputAssetAccumulationLimit {
          output_index: coordinate.primary_index,
          asset_index: coordinate.asset_index,
        }
      _ -> MintAssetAccumulationLimit { mint_index: coordinate.primary_index }
    }
    Some(substrate.bind_exact_rejection_reason_v1(subject, expected))
  } else {
    None
  }
  BoundV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    coordinate,
  }
}

pub fn initialize_accumulator_v1(
  bound: BoundV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  pre: validation_trace_v1.ValidationMachineStateV1,
  proof: validation_trace_v1.ValidationTraceProof,
  control: ValueAndMintControlV1,
) -> FoldStateV1 {
  expect coordinate_is_well_formed_v1(bound.coordinate)
  expect trace_pre_is_authenticated(bound, membership, pre, proof, control)
  expect and {
      control.stage == bound.coordinate.fold + 2,
      bytearray.length(control.value_accumulator.asset_root) == 32,
      control.value_accumulator.seen_asset_count >= 0,
      control.value_accumulator.seen_asset_count <= ledger_output_v1.max_distinct_asset_count,
      control.value_accumulator.nonzero_asset_count >= 0,
      control.value_accumulator.nonzero_asset_count <= control.value_accumulator.seen_asset_count,
      when bound.coordinate.fold is {
        0 -> and {
            control.replay_cursor >= 0,
            control.replay_cursor < control.native_control.resolved_input_count,
            control.replay_asset_cursor > 0,
            control.replay_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
          }
        1 -> and {
            control.output_cursor >= 0,
            control.output_cursor < control.native_control.output_count,
            control.output_asset_cursor > 0,
            control.output_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
          }
        _ -> and {
            control.mint_cursor >= 0,
            control.mint_cursor < control.native_control.mint_count,
          }
      },
    }
  FoldStateV1 {
    bound,
    control: Some(control),
    stage: 0,
    decisive_fault_holds: None,
  }
}

pub fn skip_fold_v1(state: FoldStateV1, fold: Int) -> FoldStateV1 {
  expect state.stage == fold
  expect state.bound.coordinate.fold != fold
  expect state.decisive_fault_holds == None
  FoldStateV1 { ..state, stage: fold + 1 }
}

fn event_key_matches_subject(
  event_key: ledger_state.EventKey,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  when event_key is {
    ledger_state.L2TransactionEventKey { tx_id } -> and {
        subject.source_kind == substrate.source_kind_accepted,
        tx_id == subject.transaction_id,
      }
    ledger_state.ForcedTransactionEventKey { tx_order_id } -> and {
        subject.source_kind == substrate.source_kind_forced,
        cbor.serialise(tx_order_id) == subject.source_key,
      }
    _ -> False
  }
}

fn trace_pre_is_authenticated(
  bound: BoundV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  pre: validation_trace_v1.ValidationMachineStateV1,
  proof: validation_trace_v1.ValidationTraceProof,
  control: ValueAndMintControlV1,
) -> Bool {
  let descriptor = membership.value
  and {
    event_key_matches_subject(membership.key, bound.subject),
    pre.event_key_hash == blake2b_256(cbor.serialise(membership.key)),
    pre.transaction_id == bound.subject.transaction_id,
    pre.phase == validation_trace_v1.ValueAndMint,
    pre.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      pre.program_counter,
      encode_value_and_mint_control_v1(control),
    ),
    transition_trace.verify_root_membership_with_bytes(
      membership,
      transition_trace.ValidationTracesRootDomain,
      bound.validation_traces_root,
      bound.validation_trace_count,
      cbor.serialise(membership.key),
      cbor.serialise(descriptor),
    ),
    proof.state_hash == validation_trace_v1.hash_machine_state(pre),
    validation_trace_v1.verify_trace_proof(descriptor, proof),
    if bound.subject.source_kind == substrate.source_kind_accepted {
      and {
        pre.source_kind == validation_trace_v1.Normal,
        descriptor.verdict == validation_trace_v1.Accepted,
      }
    } else {
      and {
        pre.source_kind == validation_trace_v1.Forced,
        descriptor.verdict == validation_trace_v1.Rejected,
        descriptor.rejection_code_hash == validation_trace_v1.hash_rejection_code(
          reject_asset_count,
        ),
      }
    },
  }
}

fn mutation_fault(
  control: ValueAndMintControlV1,
  unit: ByteArray,
  quantity: Int,
  mutation: ValueAssetMutationWitnessV1,
) -> Bool {
  expect and {
      bytearray.length(unit) >= 28,
      bytearray.length(unit) <= 60,
      quantity != 0,
      mpf_proof_v1.proof_has_at_most_steps(mutation.delta_proof, 16),
    }
  if mutation.delta_was_present {
    expect Some(_) = mpf_proof_v1.update_root(
      control.value_accumulator.asset_root,
      unit,
      cbor.serialise(mutation.old_delta),
      cbor.serialise(mutation.old_delta + quantity),
      mutation.delta_proof,
    )
    False
  } else {
    expect mutation.old_delta == 0
    expect Some(_) = mpf_proof_v1.insert_root(
      control.value_accumulator.asset_root,
      unit,
      cbor.serialise(quantity),
      mutation.delta_proof,
    )
    control.value_accumulator.seen_asset_count >= ledger_output_v1.max_distinct_asset_count
  }
}

fn finish(state: FoldStateV1, fold: Int, fault: Bool) -> FoldStateV1 {
  expect state.stage == fold
  expect state.bound.coordinate.fold == fold
  expect state.decisive_fault_holds == None
  FoldStateV1 { ..state, stage: fold + 1, decisive_fault_holds: Some(fault) }
}

pub fn authenticate_input_fold_v1(
  state: FoldStateV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  asset_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<FrontierPeak>,
  asset_siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
) -> FoldStateV1 {
  expect Some(control) = state.control
  expect and {
      source_kind == 0,
      control.stage == 2,
      state.bound.coordinate.primary_index == control.replay_cursor,
      state.bound.coordinate.asset_index == asset_index,
      control.replay_asset_cursor == asset_index + 1,
      resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
      blake2b_256(descriptor_cbor) == control.replay_value_hash,
    }
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  expect
    ledger_output_commitment_v1.verify_asset_membership(
      descriptor,
      asset_index,
      policy_id,
      asset_name,
      quantity,
      asset_peaks,
      asset_siblings,
    )
  finish(
    state,
    input_fold,
    mutation_fault(
      control,
      bytearray.concat(policy_id, asset_name),
      quantity,
      mutation,
    ),
  )
}

pub fn authenticate_output_fold_v1(
  state: FoldStateV1,
  output_index: Int,
  descriptor_cbor: ByteArray,
  asset_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<FrontierPeak>,
  asset_siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
) -> FoldStateV1 {
  expect Some(control) = state.control
  expect and {
      control.stage == 3,
      state.bound.coordinate.primary_index == output_index,
      state.bound.coordinate.asset_index == asset_index,
      control.output_cursor == output_index,
      control.output_asset_cursor == asset_index + 1,
      blake2b_256(descriptor_cbor) == control.replay_value_hash,
    }
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  expect
    ledger_output_commitment_v1.verify_asset_membership(
      descriptor,
      asset_index,
      policy_id,
      asset_name,
      quantity,
      asset_peaks,
      asset_siblings,
    )
  finish(
    state,
    output_fold,
    mutation_fault(
      control,
      bytearray.concat(policy_id, asset_name),
      0 - quantity,
      mutation,
    ),
  )
}

pub fn authenticate_mint_fold_v1(
  state: FoldStateV1,
  mint_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
) -> FoldStateV1 {
  expect Some(control) = state.control
  expect and {
      control.stage == 4,
      state.bound.coordinate.primary_index == mint_index,
      control.mint_cursor == mint_index,
      validation_merkle_v1.verify_membership(
        control.native_control.mint_count,
        control.native_control.mint_peaks,
        mint_index,
        mint_asset_leaf_hash(policy_id, asset_name, quantity),
        siblings,
      ),
    }
  finish(
    state,
    mint_fold,
    mutation_fault(
      control,
      bytearray.concat(policy_id, asset_name),
      quantity,
      mutation,
    ),
  )
}

pub fn terminal_contradiction_v1(state: FoldStateV1) -> Bool {
  expect state.stage == 3
  expect Some(fault) = state.decisive_fault_holds
  substrate.terminal_contradiction_v1(state.bound.subject, fault)
}
