//// Narrow reverse-use rule for `unusedScriptWitness` (`0000002f`).

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{UnusedScriptWitness}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub const maximum_scan_batch: Int = 24

pub type BoundWitnessV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  script_index: Int,
}

pub type AuthenticatedWitnessV1 {
  bound: BoundWitnessV1,
  prior_ledger_root: ByteArray,
  language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  item_commitment: ByteArray,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type ReverseScanV1 {
  witness: AuthenticatedWitnessV1,
  alternate_cursor: Int,
  purpose_cursor: Int,
  shadowed: Bool,
  used: Bool,
  checkpoint_hash: ByteArray,
}

pub type DecisionV1 {
  subject: substrate.VerdictSubjectV1,
  script_index: Int,
  unused: Bool,
}

pub type SourceOpeningV1 {
  source_index: Int,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
}

pub type PurposeOpeningV1 {
  frontier_index: Int,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  purpose_subject: ByteArray,
  siblings: List<ByteArray>,
}

pub type TerminalScriptSourcesWitnessV1 {
  witness_cbor: ByteArray,
}

type TerminalScriptSourcesControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  stage: Int,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  redeemer_count: Int,
  redeemer_peaks: List<validation_merkle_v1.FrontierPeak>,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  source_total_count: Int,
  redeemer_total_count: Int,
  purpose_cursor: Int,
  source_cursor: Int,
  redeemer_cursor: Int,
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
}

fn decode_frontier(items: List<Data>) -> List<validation_merkle_v1.FrontierPeak> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect [height, hash] = builtin.un_list_data(item)
      [
        validation_merkle_v1.FrontierPeak {
          height: builtin.un_i_data(height),
          hash: builtin.un_b_data(hash),
        },
        ..decode_frontier(rest)
      ]
    }
  }
}

fn decode_terminal_control(
  witness: TerminalScriptSourcesWitnessV1,
) -> TerminalScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(witness.witness_cbor)
  expect
      [
        compact_cbor, witness_set_compact_cbor, field_preimage_lengths_cbor,
        context_cbor, _, _, _, _, _, stage, source_count, source_peaks,
        redeemer_count, redeemer_peaks, _, _, _, _, purpose_count, purpose_peaks,
        _, _, _, _, _, source_total_count, redeemer_total_count, _, _, _,
        discovery_cbor,
      ]
    = builtin.un_list_data(data)
  expect Some(discovery_data) = cbor.deserialise(
    builtin.un_b_data(discovery_cbor),
  )
  expect
      [
        purpose_cursor, source_cursor, redeemer_cursor, current_purpose_kind,
        current_purpose_index, current_script_hash, current_subject,
        matched_source_index, matched_language_tag, matched_source_leaf, _, _, _,
        execution_count, execution_peaks,
      ]
    = builtin.un_list_data(discovery_data)
  expect and {
      builtin.un_i_data(current_purpose_kind) == -1,
      builtin.un_i_data(current_purpose_index) == -1,
      builtin.un_b_data(current_script_hash) == #"",
      builtin.un_b_data(current_subject) == #"",
      builtin.un_i_data(matched_source_index) == -1,
      builtin.un_i_data(matched_language_tag) == -1,
      builtin.un_b_data(matched_source_leaf) == #"",
    }
  TerminalScriptSourcesControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor),
    field_preimage_lengths_cbor: builtin.un_b_data(field_preimage_lengths_cbor),
    context_cbor: builtin.un_b_data(context_cbor),
    stage: builtin.un_i_data(stage),
    source_count: builtin.un_i_data(source_count),
    source_peaks: source_peaks |> builtin.un_list_data |> decode_frontier,
    redeemer_count: builtin.un_i_data(redeemer_count),
    redeemer_peaks: redeemer_peaks |> builtin.un_list_data |> decode_frontier,
    purpose_count: builtin.un_i_data(purpose_count),
    purpose_peaks: purpose_peaks |> builtin.un_list_data |> decode_frontier,
    source_total_count: builtin.un_i_data(source_total_count),
    redeemer_total_count: builtin.un_i_data(redeemer_total_count),
    purpose_cursor: builtin.un_i_data(purpose_cursor),
    source_cursor: builtin.un_i_data(source_cursor),
    redeemer_cursor: builtin.un_i_data(redeemer_cursor),
    execution_count: builtin.un_i_data(execution_count),
    execution_peaks: execution_peaks |> builtin.un_list_data |> decode_frontier,
  }
}

pub fn bind_witness_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  script_index: Int,
) -> BoundWitnessV1 {
  expect and {
      substrate.subject_is_canonical_v1(subject),
      bytearray.length(validation_traces_root) == 32,
      validation_trace_count > 0,
      script_index >= 0,
    }
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    expect UnusedScriptWitness { script_index: actual } = substrate.rejection_reason_of_v1(
      subject,
    )
    expect actual == script_index
    Some(actual)
  } else {
    None
  }
  BoundWitnessV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    script_index,
  }
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

/// Authenticates the exact producer-committed terminal ScriptSources frontier. The
/// direct predicate is sound under that commitment; a producer that commits a
/// fabricated frontier is instead accountable through the header's trace
/// invalidity proof. No uncommitted off-chain frontier enters the result.
pub fn authenticate_witness_v1(
  bound: BoundWitnessV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  retained_control: TerminalScriptSourcesWitnessV1,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
) -> AuthenticatedWitnessV1 {
  let control = decode_terminal_control(retained_control)
  let descriptor = trace_membership.value
  let source_leaf = script_proof_v1.inline_source_leaf_hash(
    bound.script_index,
    language_tag,
    script_hash,
    total_length,
    item_commitment,
  )
  expect and {
      event_key_matches_subject(trace_membership.key, bound.subject),
      machine_state.event_key_hash == blake2b_256(
        cbor.serialise(trace_membership.key),
      ),
      transition_trace.verify_root_membership_with_bytes(
        trace_membership,
        transition_trace.ValidationTracesRootDomain,
        bound.validation_traces_root,
        bound.validation_trace_count,
        cbor.serialise(trace_membership.key),
        cbor.serialise(descriptor),
      ),
      machine_state.transaction_id == bound.subject.transaction_id,
      if bound.subject.source_kind == substrate.source_kind_accepted {
        and {
          machine_state.source_kind == validation_trace_v1.Normal,
          descriptor.verdict == validation_trace_v1.Accepted,
        }
      } else {
        and {
          machine_state.source_kind == validation_trace_v1.Forced,
          descriptor.verdict == validation_trace_v1.Rejected,
          descriptor.rejection_code_hash == validation_trace_v1.hash_rejection_code(
            rejection_reason_v1.rejection_code_of(
              substrate.rejection_reason_of_v1(bound.subject),
            ),
          ),
        }
      },
      machine_state.phase == validation_trace_v1.ScriptSources,
      machine_state.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        machine_state.program_counter,
        retained_control.witness_cbor,
      ),
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(descriptor, trace_proof),
      machine_state.transaction_commitment == native_tx_proof_commitment_v1(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
      ),
      machine_state.validation_context_hash == validation_trace_v1.hash_validation_context(
        control.context_cbor,
      ),
      if bound.subject.direction == substrate.direction_wrongful_acceptance {
        and {
          control.stage == 11,
          control.source_cursor == bound.script_index,
        }
      } else {
        and {
          control.stage == 12,
          control.source_cursor == control.source_count,
        }
      },
      control.source_total_count == control.source_count,
      control.redeemer_total_count == control.redeemer_count,
      validation_merkle_v1.frontier_is_well_formed(
        control.source_count,
        control.source_peaks,
      ),
      validation_merkle_v1.frontier_is_well_formed(
        control.redeemer_count,
        control.redeemer_peaks,
      ),
      control.purpose_count == control.purpose_cursor,
      validation_merkle_v1.frontier_is_well_formed(
        control.purpose_count,
        control.purpose_peaks,
      ),
      control.redeemer_cursor >= 0,
      control.redeemer_cursor <= control.redeemer_count,
      control.execution_count == control.purpose_cursor,
      validation_merkle_v1.frontier_is_well_formed(
        control.execution_count,
        control.execution_peaks,
      ),
      bound.script_index < control.source_count,
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        bound.script_index,
        source_leaf,
        source_siblings,
      ),
      total_length > 0,
      bytearray.length(script_hash) == 28,
      bytearray.length(item_commitment) == 32,
    }
  AuthenticatedWitnessV1 {
    bound,
    prior_ledger_root: machine_state.prior_ledger_root,
    language_tag,
    script_hash,
    script_total_length: total_length,
    item_commitment,
    source_count: control.source_count,
    source_peaks: control.source_peaks,
    purpose_count: control.purpose_count,
    purpose_peaks: control.purpose_peaks,
  }
}

pub fn initial_reverse_scan_v1(witness: AuthenticatedWitnessV1) -> ReverseScanV1 {
  let seed = blake2b_256(
    #"4d696467617264556e757365645363726970745769746e6573735363616e5631"
      |> bytearray.concat(witness.bound.subject.transaction_id)
      |> bytearray.concat(cbor.serialise(witness.bound.script_index))
      |> bytearray.concat(witness.script_hash)
      |> bytearray.concat(cbor.serialise(witness.purpose_count)),
  )
  ReverseScanV1 {
    witness,
    alternate_cursor: 0,
    purpose_cursor: 0,
    shadowed: False,
    used: False,
    checkpoint_hash: seed,
  }
}

pub fn authenticate_earlier_source_v1(
  state: ReverseScanV1,
  opening: SourceOpeningV1,
) -> ReverseScanV1 {
  expect and {
      state.alternate_cursor < state.witness.bound.script_index,
      opening.source_index == state.alternate_cursor,
      validation_merkle_v1.verify_membership(
        state.witness.source_count,
        state.witness.source_peaks,
        opening.source_index,
        script_proof_v1.inline_source_leaf_hash(
          opening.source_index,
          opening.language_tag,
          opening.script_hash,
          opening.total_length,
          opening.item_commitment,
        ),
        opening.siblings,
      ),
    }
  let next_cursor = state.alternate_cursor + 1
  let next_shadowed = or {
    state.shadowed,
    opening.script_hash == state.witness.script_hash,
  }
  ReverseScanV1 {
    ..state,
    alternate_cursor: next_cursor,
    shadowed: next_shadowed,
    checkpoint_hash: blake2b_256(
      state.checkpoint_hash
        |> bytearray.concat(cbor.serialise(next_cursor))
        |> bytearray.concat(opening.script_hash),
    ),
  }
}

pub fn alternate_sources_complete_v1(state: ReverseScanV1) -> Bool {
  state.alternate_cursor == state.witness.bound.script_index
}

pub fn scan_purpose_v1(
  state: ReverseScanV1,
  opening: PurposeOpeningV1,
) -> ReverseScanV1 {
  expect and {
      alternate_sources_complete_v1(state),
      !state.used,
      state.purpose_cursor < state.witness.purpose_count,
      opening.frontier_index == state.purpose_cursor,
      opening.purpose_kind >= 0,
      opening.purpose_kind <= 3,
      validation_merkle_v1.verify_membership(
        state.witness.purpose_count,
        state.witness.purpose_peaks,
        opening.frontier_index,
        script_proof_v1.purpose_leaf_hash(
          opening.purpose_kind,
          opening.purpose_index,
          opening.script_hash,
          opening.purpose_subject,
        ),
        opening.siblings,
      ),
    }
  let next_cursor = state.purpose_cursor + 1
  let matched = and {
    !state.shadowed,
    opening.script_hash == state.witness.script_hash,
  }
  ReverseScanV1 {
    ..state,
    purpose_cursor: next_cursor,
    used: matched,
    checkpoint_hash: blake2b_256(
      state.checkpoint_hash
        |> bytearray.concat(cbor.serialise(opening.purpose_kind))
        |> bytearray.concat(cbor.serialise(opening.purpose_index))
        |> bytearray.concat(opening.script_hash),
    ),
  }
}

pub fn reverse_scan_complete_v1(state: ReverseScanV1) -> Bool {
  or {
    state.used,
    state.purpose_cursor == state.witness.purpose_count,
  }
}

pub fn decision_v1(state: ReverseScanV1) -> DecisionV1 {
  expect and {
      alternate_sources_complete_v1(state),
      reverse_scan_complete_v1(state),
    }
  DecisionV1 {
    subject: state.witness.bound.subject,
    script_index: state.witness.bound.script_index,
    unused: or {
      state.shadowed,
      !state.used,
    },
  }
}

pub fn terminal_contradiction_v1(state: DecisionV1) -> Bool {
  substrate.terminal_contradiction_v1(state.subject, state.unused)
}

pub fn encode_reverse_scan_v1(state: ReverseScanV1) -> ByteArray {
  cbor.serialise(state)
}
