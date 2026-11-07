//// The ValueAndMint entry count of `distinctAssetAccumulationLimit`, derived
//// directly from the transaction and its event pre-state.
////
//// The producer folds every asset entry of the spent inputs (in input
//// resolution schedule order), then of each output (in field order), then of
//// the mint field (in flat order), and refuses the first entry reached once
//// `max_distinct_asset_count` entries have been folded. Every entry counts,
//// whether or not its unit was folded before. This module derives the same
//// count and the coordinate of the first refused entry:
////
//// * an input's entry count is its ledger descriptor's `asset_count`, opened
////   under the subject's event pre-root;
//// * an output's entry count is the sum of its policies' asset-map sizes;
//// * a mint item's entry count is its policy's declared asset-map size.
////
//// The output and mint shapes are read from their canonical encodings. Any
//// other encoding makes an earlier phase the transaction's first fault
//// (`OutputNonCanonical`, `MintItemMalformed`, both phase 8), and an input
//// absent from the pre-state makes `InputNotFound` (phase 7) its first fault.
//// So whenever the subject accepts the transaction or records a phase-12
//// reason, the count read here is the producer's.

use aiken/merkle_patricia_forestry.{Proof}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, mint_field_index, outputs_field_index,
  reference_inputs_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{
  InputAssetFault, MintAssetFault, OutputAssetFault, ValueFaultPositionV1,
}
use midgard/fraud_proofs/mint_declared_asset_limit/rule.{policy_header_v1}
use midgard/fraud_proofs/native_tx/codec.{
  byte_at, decode_definite_map_header_at,
}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_input_cbor}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1.{max_distinct_asset_count}
use midgard/mpf_proof_v1
use midgard/native_tx_field_access_v1.{
  FieldViewV1, field_item_at, field_item_count, field_read_range,
}
use midgard/native_tx_machine_walk_v1 as walk

/// Where the next output step resumes inside the output under the cursor:
/// the byte offset of the next policy key or asset name, the policies after
/// the current one, the assets of the current policy still to be skipped,
/// and the entries of this output already counted.
pub type OutputResumeV1 {
  offset: Int,
  policies_remaining: Int,
  assets_remaining: Int,
  asset_index: Int,
}

/// The fold over one field. `entry_count` is the number of entries folded so
/// far, never above `max_distinct_asset_count`. `resume` is set only inside
/// an output that a previous step left part-way; `mint_index` is the flat
/// index of the next mint policy's first asset.
pub type WalkV1 {
  cursor: cursor.CursorV1,
  complete: Bool,
  entry_count: Int,
  resume: Option<OutputResumeV1>,
  mint_index: Int,
}

/// An input-fold crossing awaiting its schedule position: the spent input's
/// field-0 ordinal and item, and the refused asset's index in its value.
pub type RankV1 {
  bound: BoundV1,
  input_ordinal: Int,
  input_item: ByteArray,
  asset_index: Int,
}

/// The completed derivation: the first refused entry, if any.
pub type TerminalV1 {
  bound: BoundV1,
  crossing: Option<ValueFaultPositionV1>,
}

/// A spent input's ledger descriptor under the event pre-root.
pub type InputWitnessV1 {
  descriptor_cbor: ByteArray,
  proof: Proof,
}

pub type AdmitV1 {
  /// The unit fits; the new entry count.
  Admitted(Int)
  /// The unit's entry at this index within the unit is refused.
  Crosses(Int)
}

/// Folds a unit of `entries` entries onto `count`. The producer refuses an
/// entry when `max_distinct_asset_count` entries precede it, so the unit
/// crosses exactly when the total exceeds the bound, at the entry whose index
/// within the unit is the bound minus `count`.
pub fn admit_entries_v1(count: Int, entries: Int) -> AdmitV1 {
  if count + entries > max_distinct_asset_count {
    Crosses(max_distinct_asset_count - count)
  } else {
    Admitted(count + entries)
  }
}

pub type InputsStepV1 {
  InputsWalked(WalkV1)
  InputsCross(RankV1)
}

pub type FieldStepV1 {
  FieldWalked(WalkV1)
  FieldDone(TerminalV1)
}

/// Opens the spend field. It has a fixed stride, so the walk starts at once
/// and an empty field is complete at opening.
pub fn initial_v1(
  bound: BoundV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkV1 {
  let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
    bound,
    spend_inputs_field_index,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  WalkV1 {
    cursor: field_cursor,
    complete,
    entry_count: 0,
    resume: None,
    mint_index: 0,
  }
}

/// The entry count of one spent input: its ledger descriptor's asset count.
/// The pre-root key is the input's own out-ref, so the descriptor proven
/// there is that output's.
pub fn input_entry_count_v1(
  event_pre_root: ByteArray,
  item: ByteArray,
  witness: InputWitnessV1,
) -> Int {
  let input = decode_midgard_tx_input_cbor(item)
  expect
    mpf_proof_v1.has(
      event_pre_root,
      ledger_outref_key(
        OutputReference {
          transaction_id: input.tx_id,
          output_index: input.output_index,
        },
      ),
      witness.descriptor_cbor,
      witness.proof,
    )
  ledger_output_commitment_v1.decode_canonical_schema(witness.descriptor_cbor).asset_count
}

/// A batch of spent-input items taken from the spend field, awaiting their
/// ledger descriptors. `state` is the walk past the batch, `first_ordinal`
/// the field-0 ordinal of its first item, and `resume_script_hash` the
/// taking step, recorded by that step itself as its own hash.
pub type PendingInputsV1 {
  resume_script_hash: ByteArray,
  state: WalkV1,
  first_ordinal: Int,
  pending: List<ByteArray>,
}

fn take_loop(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  count: Int,
) -> Pair<List<ByteArray>, walk.FieldWalkCheckpointV1> {
  if count == 0 {
    Pair([], position)
  } else {
    let Pair(item, advanced) = walk.walk_next(view, position)
    let Pair(rest, last) = take_loop(view, advanced, count - 1)
    Pair([item, ..rest], last)
  }
}

/// Takes the next `count` (at least one) spent-input items, in field order.
/// `walk_next` refuses a position past the field's last item.
pub fn take_inputs_v1(
  state: WalkV1,
  resume_script_hash: ByteArray,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  count: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> PendingInputsV1 {
  expect count > 0
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    spend_inputs_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(pending, last) = take_loop(view, position, count)
  PendingInputsV1 {
    resume_script_hash,
    state: WalkV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, last),
      complete: walk.walk_is_complete(last),
    },
    first_ordinal: walk.walk_next_item_index(position),
    pending,
  }
}

fn resolve_loop(
  state: WalkV1,
  ordinal: Int,
  pending: List<ByteArray>,
  witnesses: List<InputWitnessV1>,
) -> InputsStepV1 {
  when pending is {
    [] -> {
      expect [] = witnesses
      InputsWalked(state)
    }
    [item, ..rest] -> {
      expect [witness, ..others] = witnesses
      let bound = state.cursor.bound
      when
        admit_entries_v1(
          state.entry_count,
          input_entry_count_v1(bound.event_pre_root, item, witness),
        )
      is {
        Crosses(asset_index) ->
          InputsCross(
            RankV1 {
              bound,
              input_ordinal: ordinal,
              input_item: item,
              asset_index,
            },
          )
        Admitted(entry_count) ->
          resolve_loop(
            WalkV1 { ..state, entry_count },
            ordinal + 1,
            rest,
            others,
          )
      }
    }
  }
}

/// Folds a taken batch, one descriptor witness per item, in field order.
pub fn resolve_inputs_v1(
  pending: PendingInputsV1,
  witnesses: List<InputWitnessV1>,
) -> InputsStepV1 {
  resolve_loop(pending.state, pending.first_ordinal, pending.pending, witnesses)
}

/// The crossing input's schedule position: its field-0 ordinal plus the
/// reference inputs ordered before it. The schedule merges both fields by
/// item bytes, a spent input first on equal bytes, so `rank` is the number
/// of reference items strictly below the spent item.
pub fn rank_v1(
  state: RankV1,
  opening: FieldOpeningV1,
  rank: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> TerminalV1 {
  let view = field_opening_v1.opened_field_view(
    opening,
    cursor.anchor_v1(state.bound, reference_inputs_field_index),
    reference_inputs_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  let key = state.input_item
  // `field_item_at` refuses an index outside the field, which bounds `rank`
  // to 0..count on both sides.
  expect or {
      rank == 0,
      bytearray.compare(field_item_at(view, rank - 1), key) == Less,
    }
  expect or {
      rank == field_item_count(view),
      bytearray.compare(key, field_item_at(view, rank)) != Greater,
    }
  TerminalV1 {
    bound: state.bound,
    crossing: Some(
      InputAssetFault {
        input_index: state.input_ordinal + rank,
        asset_index: state.asset_index,
      },
    ),
  }
}

/// Opens `next_field` once `previous_field` is exhausted.
pub fn open_next_v1(
  state: WalkV1,
  previous_field: Int,
  next_field: Int,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkV1 {
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
  WalkV1 { ..state, cursor: field_cursor, complete }
}

/// One grammar batch over a variable field.
pub fn certify_v1(
  state: WalkV1,
  field_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkV1 {
  let (field_cursor, _, complete) = cursor.certify_with_completion_at_v1(
    state.cursor,
    field_index,
    opening,
    checkpoint_bytes,
    budget,
    reference_inputs,
    certificate_policy_id,
  )
  WalkV1 { ..state, cursor: field_cursor, complete }
}

// ## Outputs

/// The offset after the definite byte string at `offset`. Addresses and
/// asset names are at most 255 bytes, so only the two short heads occur.
fn skip_short_bytes_at(item: ByteArray, offset: Int) -> Int {
  let head = byte_at(item, offset)
  if head == 88 {
    offset + 2 + byte_at(item, offset + 1)
  } else {
    expect head >= 64 && head <= 87
    offset + 1 + head - 64
  }
}

/// The offset after the unsigned integer at `offset`.
fn skip_uint_at(item: ByteArray, offset: Int) -> Int {
  let head = byte_at(item, offset)
  if head <= 23 {
    offset + 1
  } else if head == 24 {
    offset + 2
  } else if head == 25 {
    offset + 3
  } else if head == 26 {
    offset + 5
  } else {
    expect head == 27
    offset + 9
  }
}

/// The resume point at the first policy of an output's value. The canonical
/// output is a map whose key 0 holds the address bytes and whose key 1 holds
/// `[coin, {policy: {name: quantity}}]`.
pub fn output_value_start_v1(item: ByteArray) -> OutputResumeV1 {
  // Map head and key 0 are one byte each; key 1 and the value's array head
  // follow the address.
  let coin_offset = skip_short_bytes_at(item, 2) + 2
  let Pair(offset, policies) = decode_definite_map_header_at(
    item,
    skip_uint_at(item, coin_offset),
  )
  OutputResumeV1 {
    offset,
    policies_remaining: policies,
    assets_remaining: 0,
    asset_index: 0,
  }
}

pub type OutputOutcomeV1 {
  /// The output is fully counted; the budget left.
  OutputCounted { entry_count: Int, budget: Int }
  /// The budget ran out inside the output.
  OutputPaused { entry_count: Int, resume: OutputResumeV1 }
  /// The output's entry at `asset_index` is refused.
  OutputCrosses { asset_index: Int }
}

/// Counts an output from `resume`, one budget unit per policy and per skipped
/// asset. A policy's entries are counted from its asset-map size; its assets
/// are skipped only to reach the next policy, so the last policy needs none.
pub fn count_output_v1(
  item: ByteArray,
  resume: OutputResumeV1,
  entry_count: Int,
  budget: Int,
) -> OutputOutcomeV1 {
  if resume.assets_remaining == 0 {
    if resume.policies_remaining == 0 {
      OutputCounted { entry_count, budget }
    } else if budget == 0 {
      OutputPaused { entry_count, resume }
    } else {
      // The 28-byte policy key is `58 1c` and its bytes.
      let Pair(offset, assets) = decode_definite_map_header_at(
        item,
        resume.offset + 30,
      )
      when admit_entries_v1(entry_count, assets) is {
        Crosses(index) ->
          OutputCrosses { asset_index: resume.asset_index + index }
        Admitted(counted) ->
          if resume.policies_remaining == 1 {
            OutputCounted { entry_count: counted, budget: budget - 1 }
          } else {
            count_output_v1(
              item,
              OutputResumeV1 {
                offset,
                policies_remaining: resume.policies_remaining - 1,
                assets_remaining: assets,
                asset_index: resume.asset_index + assets,
              },
              counted,
              budget - 1,
            )
          }
      }
    }
  } else if budget == 0 {
    OutputPaused { entry_count, resume }
  } else {
    count_output_v1(
      item,
      OutputResumeV1 {
        ..resume,
        offset: skip_uint_at(item, skip_short_bytes_at(item, resume.offset)),
        assets_remaining: resume.assets_remaining - 1,
      },
      entry_count,
      budget - 1,
    )
  }
}

fn outputs_loop(
  state: WalkV1,
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  budget: Int,
) -> FieldStepV1 {
  let output_index = walk.walk_next_item_index(position)
  let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
    view,
    position,
  )
  let item = field_read_range(view, offset, length)
  // Opening an output costs one unit, so a step's work is bounded even over
  // outputs that hold no assets.
  let outcome = when state.resume is {
    None ->
      count_output_v1(
        item,
        output_value_start_v1(item),
        state.entry_count,
        budget - 1,
      )
    Some(resume) -> count_output_v1(item, resume, state.entry_count, budget)
  }
  when outcome is {
    OutputCrosses { asset_index } ->
      FieldDone(
        TerminalV1 {
          bound: state.cursor.bound,
          crossing: Some(OutputAssetFault { output_index, asset_index }),
        },
      )
    OutputPaused { entry_count, resume } ->
      FieldWalked(WalkV1 { ..state, entry_count, resume: Some(resume) })
    OutputCounted { entry_count, budget: left } -> {
      let next = WalkV1 {
        ..state,
        cursor: cursor.after_v1(state.cursor, advanced),
        complete: walk.walk_is_complete(advanced),
        entry_count,
        resume: None,
      }
      if next.complete || left == 0 {
        FieldWalked(next)
      } else {
        outputs_loop(next, view, advanced, left)
      }
    }
  }
}

/// Counts outputs from the cursor with `budget` units of work.
pub fn advance_outputs_v1(
  state: WalkV1,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldStepV1 {
  expect budget > 0
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    outputs_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  outputs_loop(state, view, position, budget)
}

// ## Mint

/// The declared asset count of one mint item: the largest policy header is a
/// pair tag, a 28-byte key and a map header, plus one byte of the map body.
fn mint_declared_count_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
) -> Pair<Int, walk.FieldWalkCheckpointV1> {
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
  Pair(policy_header_v1(prefix).declared_count, advanced)
}

fn mint_loop(
  state: WalkV1,
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  count: Int,
) -> FieldStepV1 {
  if walk.walk_is_complete(position) {
    FieldDone(TerminalV1 { bound: state.cursor.bound, crossing: None })
  } else if count == 0 {
    FieldWalked(
      WalkV1 { ..state, cursor: cursor.after_v1(state.cursor, position) },
    )
  } else {
    let Pair(declared, advanced) = mint_declared_count_v1(view, position)
    when admit_entries_v1(state.entry_count, declared) is {
      Crosses(index) ->
        FieldDone(
          TerminalV1 {
            bound: state.cursor.bound,
            crossing: Some(
              MintAssetFault { mint_index: state.mint_index + index },
            ),
          },
        )
      Admitted(entry_count) ->
        mint_loop(
          WalkV1 {
            ..state,
            entry_count,
            mint_index: state.mint_index + declared,
          },
          view,
          advanced,
          count - 1,
        )
    }
  }
}

/// Counts up to `count` mint policies from the cursor. The field's end is the
/// end of the derivation.
pub fn advance_mint_v1(
  state: WalkV1,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  count: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FieldStepV1 {
  expect count > 0
  let Pair(view, position) = cursor.resume_body_at_v1(
    state.cursor,
    mint_field_index,
    opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  mint_loop(state, view, position, count)
}
