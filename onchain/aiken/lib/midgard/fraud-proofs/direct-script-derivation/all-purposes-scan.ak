//// One authenticated walk over every script purpose of the transaction, in the
//// producer's purpose-kind order: spends (field 0), mint policies (field 5),
//// required observers (field 3), then protected-script receive outputs
//// (field 2). Each purpose's script hash is resolved through the completed
//// source map, which yields its language and whether it is native.
////
//// The walk folds two set facts that do not depend on the order within a
//// field: the bitmap of Plutus languages used by any execution, and whether
//// any purpose reaches an optional probe hash. A receive credential repeated
//// across outputs, or a spend script shared by several inputs, contributes the
//// same fact twice, so the producer's deduplication and sorting do not change
//// either fact.
////
//// A purpose whose script has no source cannot be resolved here: the lookup
//// requires a present source, so a transaction with an absent source never
//// completes the walk.

use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry.{Proof}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, mint_field_index, outputs_field_index,
  required_observers_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/mint_declared_asset_limit/rule.{policy_header_v1}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_input_cbor}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/mpf_proof_v1
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}
use midgard/native_tx_machine_walk_v1 as walk

/// Source language tags (`script_proof_v1.language_tag`).
pub const native_language_tag: Int = 0

pub const plutus_v3_language_tag: Int = 3

pub const midgard_v1_language_tag: Int = 128

/// Bitmap bits, as `script_language_views_v1.expected_script_integrity_hash`
/// reads them.
pub const plutus_v3_language_bit: Int = 1

pub const midgard_v1_language_bit: Int = 2

/// The facts folded over every execution. `probe` is fixed when the walk
/// opens; `probe_reached` records whether any purpose's script hash equals it.
pub type FactsV1 {
  language_bitmap: Int,
  probe: Option<ByteArray>,
  probe_reached: Bool,
}

/// The walk state. The field index of `cursor` names the purpose kind being
/// walked, and `complete` holds once that field is exhausted.
pub type StateV1 {
  cursor: cursor.CursorV1,
  source_index: source_index.StateV1,
  facts: FactsV1,
  complete: Bool,
}

/// One resolved execution: its script hash and the selected source's language.
pub type ExecutionV1 {
  script_hash: ByteArray,
  language_tag: Int,
}

/// The selected source for a purpose's script hash and its source-map proof.
pub type ExecutionWitnessV1 {
  source: SourceV1,
  proof: Proof,
}

/// A spent input's ledger descriptor under the event pre-root. A script-locked
/// input carries its execution witness; a key-locked input carries none.
pub type SpendWitnessV1 {
  descriptor_cbor: ByteArray,
  proof: Proof,
  execution: Option<ExecutionWitnessV1>,
}

/// The completed walk.
pub type ResultV1 {
  bound: BoundV1,
  source_index: source_index.StateV1,
  facts: FactsV1,
}

pub fn is_native_v1(execution: ExecutionV1) -> Bool {
  execution.language_tag == native_language_tag
}

/// Resolves `script_hash` through the source map. The source must be present.
pub fn execution_v1(
  index: source_index.StateV1,
  script_hash: ByteArray,
  witness: ExecutionWitnessV1,
) -> ExecutionV1 {
  // `source_index.lookup_v1`'s present-source arm. Its 28-byte check is not
  // repeated: every purpose hash here is a 28-byte item, policy header or
  // address credential, and the map holds only 28-byte keys.
  expect and {
      witness.source.script_hash == script_hash,
      mpf_proof_v1.has(
        index.root,
        script_hash,
        cbor.serialise(witness.source),
        witness.proof,
      ),
    }
  ExecutionV1 { script_hash, language_tag: witness.source.language_tag }
}

/// Adds one execution to the facts. A native execution adds no language.
pub fn admit_execution_v1(facts: FactsV1, execution: ExecutionV1) -> FactsV1 {
  let tag = execution.language_tag
  let bitmap = facts.language_bitmap
  let language_bitmap = if tag == plutus_v3_language_tag {
    if bitmap % 2 == 0 {
      bitmap + plutus_v3_language_bit
    } else {
      bitmap
    }
  } else if tag == midgard_v1_language_tag {
    if bitmap < midgard_v1_language_bit {
      bitmap + midgard_v1_language_bit
    } else {
      bitmap
    }
  } else {
    // A selected source's tag is 0, 3 or 128: the ledger descriptor decode
    // (`reference_script_is_well_formed`) and `script_proof_v1.language_tag`
    // admit no other, so the remaining tag is native and adds no language.
    bitmap
  }
  FactsV1 {
    ..facts,
    language_bitmap,
    probe_reached: facts.probe_reached || facts.probe == Some(
      execution.script_hash,
    ),
  }
}

fn admit_v1(
  facts: FactsV1,
  index: source_index.StateV1,
  script_hash: ByteArray,
  witness: ExecutionWitnessV1,
) -> FactsV1 {
  admit_execution_v1(facts, execution_v1(index, script_hash, witness))
}

/// Opens the spend field. Field 0 has a fixed stride, so the walk starts at
/// once and an empty field is complete at opening.
pub fn initial_v1(
  source: BoundIndexV1,
  probe: Option<ByteArray>,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
    source.bound,
    spend_inputs_field_index,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 {
    cursor: field_cursor,
    source_index: source.source_index,
    facts: FactsV1 { language_bitmap: 0, probe, probe_reached: False },
    complete,
  }
}

/// The spend credential of one spent input, from its ledger descriptor under
/// the event pre-root.
pub fn spend_script_hash_v1(
  event_pre_root: ByteArray,
  item: ByteArray,
  descriptor_cbor: ByteArray,
  proof: Proof,
) -> Option<ByteArray> {
  let input = decode_midgard_tx_input_cbor(item)
  let out_ref = OutputReference {
    transaction_id: input.tx_id,
    output_index: input.output_index,
  }
  expect
    mpf_proof_v1.has(
      event_pre_root,
      ledger_outref_key(out_ref),
      descriptor_cbor,
      proof,
    )
  let descriptor = ledger_output_commitment_v1.decode_canonical_schema(
    descriptor_cbor,
  )
  expect descriptor.output_index == input.output_index
  expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
    descriptor.address,
  )
  when address.payment_credential is {
    native_types.ScriptCredential(script_hash) -> Some(script_hash)
    _ -> None
  }
}

/// A batch of spent-input items taken from the spend field, awaiting their
/// ledger descriptors. `resume_script_hash` is the walking step that took them,
/// recorded by that step itself as its own hash.
pub type PendingSpendsV1 {
  resume_script_hash: ByteArray,
  state: StateV1,
  pending: List<ByteArray>,
}

fn take_spends_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  count: Int,
) -> Pair<List<ByteArray>, walk.FieldWalkCheckpointV1> {
  if count == 0 {
    Pair([], position)
  } else {
    let Pair(item, advanced) = walk.walk_next(view, position)
    let Pair(rest, last) = take_spends_v1(view, advanced, count - 1)
    Pair([item, ..rest], last)
  }
}

/// Takes the next `count` (at least one) spent-input items, in field order.
/// Their ledger descriptors are opened by `resolve_spends_v1` in the
/// following step.
pub fn take_spends_batch_v1(
  state: StateV1,
  resume_script_hash: ByteArray,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  count: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> PendingSpendsV1 {
  expect count > 0
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    spend_inputs_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(pending, advanced) = take_spends_v1(view, position, count)
  PendingSpendsV1 {
    resume_script_hash,
    state: StateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      complete: walk.walk_is_complete(advanced),
    },
    pending,
  }
}

fn resolve_each_v1(
  event_pre_root: ByteArray,
  index: source_index.StateV1,
  facts: FactsV1,
  pending: List<ByteArray>,
  witnesses: List<SpendWitnessV1>,
) -> FactsV1 {
  when pending is {
    [] -> {
      expect [] = witnesses
      facts
    }
    [item, ..rest] -> {
      expect [witness, ..others] = witnesses
      let facts = when
        spend_script_hash_v1(
          event_pre_root,
          item,
          witness.descriptor_cbor,
          witness.proof,
        )
      is {
        Some(script_hash) -> {
          expect Some(execution) = witness.execution
          admit_v1(facts, index, script_hash, execution)
        }
        None -> {
          expect None = witness.execution
          facts
        }
      }
      resolve_each_v1(event_pre_root, index, facts, rest, others)
    }
  }
}

/// Resolves every pending spent input, one witness each, in pending order. A
/// script-locked input admits its execution; a key-locked input admits none.
pub fn resolve_spends_v1(
  pending: PendingSpendsV1,
  witnesses: List<SpendWitnessV1>,
) -> StateV1 {
  let state = pending.state
  StateV1 {
    ..state,
    facts: resolve_each_v1(
      state.cursor.bound.event_pre_root,
      state.source_index,
      state.facts,
      pending.pending,
      witnesses,
    ),
  }
}

/// Opens `next_field` once the previous field of the order is exhausted. A
/// fixed-stride field starts walking; a variable field starts certifying.
pub fn open_next_v1(
  state: StateV1,
  previous_field: Int,
  next_field: Int,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect and {
      state.complete,
      state.cursor.field_index == previous_field,
    }
  let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
    state.cursor.bound,
    next_field,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 { ..state, cursor: field_cursor, complete }
}

/// One grammar batch over a variable field. Completion yields the walking
/// cursor at item zero; an empty field is complete there.
pub fn certify_v1(
  state: StateV1,
  field_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  let (field_cursor, _, complete) = cursor.certify_with_completion_at_v1(
    state.cursor,
    field_index,
    opening,
    checkpoint_bytes,
    budget,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 { ..state, cursor: field_cursor, complete }
}

/// The policy id of one mint item: the largest policy header is a pair tag, a
/// 28-byte key and a map header, plus one byte of the nonempty map body.
pub fn mint_policy_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
) -> Pair<ByteArray, walk.FieldWalkCheckpointV1> {
  let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
    view,
    position,
  )
  let prefix = field_read_range(
    view,
    offset,
    if length < 41 {
      length
    } else {
      41
    },
  )
  Pair(policy_header_v1(prefix).policy_id, advanced)
}

fn fold_mint_v1(
  index: source_index.StateV1,
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  facts: FactsV1,
  witnesses: List<ExecutionWitnessV1>,
) -> Pair<FactsV1, walk.FieldWalkCheckpointV1> {
  when witnesses is {
    [] -> Pair(facts, position)
    [witness, ..rest] -> {
      let Pair(policy_id, advanced) = mint_policy_v1(view, position)
      fold_mint_v1(
        index,
        view,
        advanced,
        admit_v1(facts, index, policy_id, witness),
        rest,
      )
    }
  }
}

fn fold_observers_v1(
  index: source_index.StateV1,
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  facts: FactsV1,
  witnesses: List<ExecutionWitnessV1>,
) -> Pair<FactsV1, walk.FieldWalkCheckpointV1> {
  when witnesses is {
    [] -> Pair(facts, position)
    [witness, ..rest] -> {
      let Pair(script_hash, advanced) = walk.walk_next(view, position)
      fold_observers_v1(
        index,
        view,
        advanced,
        admit_v1(facts, index, script_hash, witness),
        rest,
      )
    }
  }
}

fn fold_receive_v1(
  index: source_index.StateV1,
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  facts: FactsV1,
  witnesses: List<Option<ExecutionWitnessV1>>,
) -> Pair<FactsV1, walk.FieldWalkCheckpointV1> {
  when witnesses is {
    [] -> Pair(facts, position)
    [witness, ..rest] -> {
      let Pair(credential, advanced) = receive.next_output_credential_v1(
        view,
        position,
      )
      let facts = when credential is {
        Some(script_hash) -> {
          expect Some(execution) = witness
          admit_v1(facts, index, script_hash, execution)
        }
        None -> {
          expect None = witness
          facts
        }
      }
      fold_receive_v1(index, view, advanced, facts, rest)
    }
  }
}

/// One nonempty batch of adjacent mint items; each witness consumes one.
pub fn advance_mint_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<ExecutionWitnessV1>,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect !list.is_empty(witnesses)
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    mint_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(facts, advanced) = fold_mint_v1(
    state.source_index,
    view,
    position,
    state.facts,
    witnesses,
  )
  StateV1 {
    ..state,
    cursor: cursor.after_v1(state.cursor, advanced),
    facts,
    complete: walk.walk_is_complete(advanced),
  }
}

/// One nonempty batch of adjacent required observers; each witness consumes
/// one.
pub fn advance_observers_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<ExecutionWitnessV1>,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect !list.is_empty(witnesses)
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    required_observers_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(facts, advanced) = fold_observers_v1(
    state.source_index,
    view,
    position,
    state.facts,
    witnesses,
  )
  StateV1 {
    ..state,
    cursor: cursor.after_v1(state.cursor, advanced),
    facts,
    complete: walk.walk_is_complete(advanced),
  }
}

/// One nonempty batch of adjacent outputs; each witness consumes one. Only a
/// protected script credential is a receive purpose and carries a witness.
pub fn advance_receive_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<Option<ExecutionWitnessV1>>,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect !list.is_empty(witnesses)
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    outputs_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(facts, advanced) = fold_receive_v1(
    state.source_index,
    view,
    position,
    state.facts,
    witnesses,
  )
  StateV1 {
    ..state,
    cursor: cursor.after_v1(state.cursor, advanced),
    facts,
    complete: walk.walk_is_complete(advanced),
  }
}

/// The walk is over only when the receive field, the last of the order, is
/// exhausted. Each field opens only after its predecessor is exhausted.
pub fn result_v1(state: StateV1) -> ResultV1 {
  expect and {
      state.complete,
      state.cursor.field_index == outputs_field_index,
    }
  ResultV1 {
    bound: state.cursor.bound,
    source_index: state.source_index,
    facts: state.facts,
  }
}
