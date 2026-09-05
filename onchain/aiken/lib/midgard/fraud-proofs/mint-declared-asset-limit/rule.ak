//// Narrow machine twin for `mintDeclaredAssetLimit` (`0000002c`).

use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_definite_array_header_at,
}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{MintDeclaredAssetLimit}

pub const mint_field_index: Int = 5

pub const max_distinct_asset_count: Int = 16384

/// Field items one step-02 grammar transaction certifies.
pub const staged_policy_budget: Int = 24

/// Work units one step-03 fold transaction may spend. Opening a policy item
/// costs `fold_policy_cost`; every consumed asset entry costs one unit.
pub const staged_fold_budget: Int = 256

pub const fold_policy_cost: Int = 8

pub const outcome_scanning: Int = 0

pub const outcome_crossing: Int = 1

pub const outcome_non_crossing: Int = 2

pub type BoundPolicyV1 {
  subject: substrate.VerdictSubjectV1,
  policy_index: Int,
}

pub type PolicyHeaderV1 {
  policy_id: ByteArray,
  declared_count: Int,
  assets_offset: Int,
}

/// Constant-size fold cursor. `active_policy`, `item_cursor`,
/// `assets_remaining`, `policy_asset_cursor` and `previous_asset` mirror the
/// machine's `MintFoldControlV1` so one policy item can be consumed across
/// several transactions; while a policy is open the carried walk checkpoint
/// stays on that item.
pub type FoldStateV1 {
  subject: substrate.VerdictSubjectV1,
  policy_index: Int,
  target_policy_id: ByteArray,
  target_declared_count: Int,
  checkpoint_hash: ByteArray,
  accumulated_count: Int,
  previous_policy: ByteArray,
  active_policy: ByteArray,
  item_cursor: Int,
  assets_remaining: Int,
  policy_asset_cursor: Int,
  previous_asset: ByteArray,
  outcome: Int,
}

pub type DecisionStateV1 {
  subject: substrate.VerdictSubjectV1,
  policy_index: Int,
  crossing: Bool,
}

pub fn bind_policy_v1(
  subject: substrate.VerdictSubjectV1,
  policy_index: Int,
) -> BoundPolicyV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect policy_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        MintDeclaredAssetLimit { policy_index },
      ),
    )
  } else {
    None
  }
  BoundPolicyV1 { subject, policy_index }
}

/// Reads exactly the scalars `script_sources_begin_mint_policy` inspects
/// before its asset-limit branch, with the machine's own head decoders. The
/// item body is deliberately not decoded: the real machine rejects a crossing
/// before doing so.
pub fn policy_header_v1(item: ByteArray) -> PolicyHeaderV1 {
  let Pair(offset, item_count) = decode_definite_array_header_at(item, 0)
  expect item_count == 2
  let Pair(offset, policy_id) = decode_canonical_bytes_at(item, offset)
  expect bytearray.length(policy_id) == 28
  let Pair(assets_offset, declared_count) = decode_canonical_map_header_at(
    item,
    offset,
  )
  expect and {
      declared_count > 0,
      assets_offset < bytearray.length(item),
    }
  PolicyHeaderV1 { policy_id, declared_count, assets_offset }
}

pub fn initial_fold_v1(
  bound: BoundPolicyV1,
  target_item: ByteArray,
  checkpoint_hash: ByteArray,
) -> FoldStateV1 {
  let header = policy_header_v1(target_item)
  expect bytearray.length(checkpoint_hash) == 32
  FoldStateV1 {
    subject: bound.subject,
    policy_index: bound.policy_index,
    target_policy_id: header.policy_id,
    target_declared_count: header.declared_count,
    checkpoint_hash,
    accumulated_count: 0,
    previous_policy: #"",
    active_policy: #"",
    item_cursor: 0,
    assets_remaining: 0,
    policy_asset_cursor: 0,
    previous_asset: #"",
    outcome: outcome_scanning,
  }
}

/// Opens the policy item at `item_index`: the machine's begin step. At the
/// bound coordinate the declared-count crossing is decided here, before the
/// body; otherwise the item is opened for asset consumption.
pub fn begin_policy_v1(
  state: FoldStateV1,
  item_index: Int,
  item: ByteArray,
) -> FoldStateV1 {
  expect and {
      state.outcome == outcome_scanning,
      state.active_policy == #"",
      item_index >= 0,
      item_index <= state.policy_index,
      state.accumulated_count >= 0,
      state.accumulated_count <= max_distinct_asset_count,
    }
  let header = policy_header_v1(item)
  expect or {
      item_index == 0,
      canonical_bytes_key_precedes(state.previous_policy, header.policy_id),
    }
  let next_count = state.accumulated_count + header.declared_count
  if item_index == state.policy_index {
    expect and {
        header.policy_id == state.target_policy_id,
        header.declared_count == state.target_declared_count,
      }
    if next_count > max_distinct_asset_count {
      FoldStateV1 { ..state, outcome: outcome_crossing }
    } else {
      open_policy(state, header)
    }
  } else {
    expect next_count <= max_distinct_asset_count
    open_policy(state, header)
  }
}

fn open_policy(state: FoldStateV1, header: PolicyHeaderV1) -> FoldStateV1 {
  FoldStateV1 {
    ..state,
    active_policy: header.policy_id,
    item_cursor: header.assets_offset,
    assets_remaining: header.declared_count,
    policy_asset_cursor: 0,
    previous_asset: #"",
  }
}

/// Consumes one asset entry of the open policy item: the machine's
/// `script_sources_fold_mint_asset`. Closing the bound item records the
/// complete non-crossing result.
pub fn consume_asset_v1(
  state: FoldStateV1,
  item_index: Int,
  item: ByteArray,
) -> FoldStateV1 {
  expect and {
      state.outcome == outcome_scanning,
      bytearray.length(state.active_policy) == 28,
      state.assets_remaining > 0,
      state.policy_asset_cursor >= 0,
    }
  let Pair(quantity_offset, asset_name) = decode_canonical_bytes_at(
    item,
    state.item_cursor,
  )
  let Pair(next_cursor, quantity) = decode_canonical_int_at(
    item,
    quantity_offset,
  )
  let next_count = state.accumulated_count + 1
  let item_length = bytearray.length(item)
  expect and {
      bytearray.length(asset_name) <= 32,
      quantity != 0,
      next_count <= max_distinct_asset_count,
      or {
        state.policy_asset_cursor == 0,
        canonical_bytes_key_precedes(state.previous_asset, asset_name),
      },
      if state.assets_remaining == 1 {
        next_cursor == item_length
      } else {
        next_cursor < item_length
      },
    }
  if state.assets_remaining == 1 {
    FoldStateV1 {
      ..state,
      accumulated_count: next_count,
      previous_policy: state.active_policy,
      active_policy: #"",
      item_cursor: 0,
      assets_remaining: 0,
      policy_asset_cursor: 0,
      previous_asset: #"",
      outcome: if item_index == state.policy_index {
        outcome_non_crossing
      } else {
        outcome_scanning
      },
    }
  } else {
    FoldStateV1 {
      ..state,
      accumulated_count: next_count,
      item_cursor: next_cursor,
      assets_remaining: state.assets_remaining - 1,
      policy_asset_cursor: state.policy_asset_cursor + 1,
      previous_asset: asset_name,
    }
  }
}

/// Consumes asset entries of the open item until it closes or `budget` units
/// are spent. Returns the state and the unspent budget.
pub fn consume_assets_v1(
  state: FoldStateV1,
  item_index: Int,
  item: ByteArray,
  budget: Int,
) -> Pair<FoldStateV1, Int> {
  if or {
    budget == 0,
    state.assets_remaining == 0,
  } {
    Pair(state, budget)
  } else {
    consume_assets_v1(
      consume_asset_v1(state, item_index, item),
      item_index,
      item,
      budget - 1,
    )
  }
}

pub fn decision_v1(state: FoldStateV1) -> DecisionStateV1 {
  expect or {
      state.outcome == outcome_crossing,
      state.outcome == outcome_non_crossing,
    }
  DecisionStateV1 {
    subject: state.subject,
    policy_index: state.policy_index,
    crossing: state.outcome == outcome_crossing,
  }
}

pub fn terminal_contradiction_v1(state: DecisionStateV1) -> Bool {
  substrate.terminal_contradiction_v1(state.subject, state.crossing)
}

pub fn encode_bound_policy_v1(value: BoundPolicyV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_fold_state_v1(value: FoldStateV1) -> ByteArray {
  cbor.serialise(value)
}
