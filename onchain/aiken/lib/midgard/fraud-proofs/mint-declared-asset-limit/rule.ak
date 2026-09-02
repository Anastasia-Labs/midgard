//// Narrow machine twin for `mintDeclaredAssetLimit` (`0000002c`).

use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{decode_canonical_int_at, expect_byte}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{MintDeclaredAssetLimit}

pub const mint_field_index: Int = 5

pub const max_distinct_asset_count: Int = 16384

pub const staged_policy_budget: Int = 24

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

pub type FoldStateV1 {
  subject: substrate.VerdictSubjectV1,
  policy_index: Int,
  target_policy_id: ByteArray,
  target_declared_count: Int,
  checkpoint_hash: ByteArray,
  accumulated_count: Int,
  previous_policy: ByteArray,
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

/// Reads exactly the scalars inspected by `script_sources_begin_mint_policy`
/// before its asset-limit branch. The item body is deliberately not decoded:
/// the real machine rejects a crossing before doing so.
pub fn policy_header_v1(item: ByteArray) -> PolicyHeaderV1 {
  let offset = expect_byte(item, 0, 130)
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

/// Completes the policy item exactly as the machine must before advancing to
/// the following policy. Returns its authenticated declared/actual count.
pub fn canonical_policy_asset_count_v1(item: ByteArray) -> Int {
  let header = policy_header_v1(item)
  let terminal = consume_assets(
    item,
    header.assets_offset,
    header.declared_count,
    None,
  )
  expect terminal == bytearray.length(item)
  header.declared_count
}

fn consume_assets(
  item: ByteArray,
  offset: Int,
  remaining: Int,
  previous: Option<ByteArray>,
) -> Int {
  if remaining == 0 {
    offset
  } else {
    let Pair(offset, asset_name) = decode_canonical_bytes_at(item, offset)
    expect bytearray.length(asset_name) <= 32
    expect
      when previous is {
        None -> True
        Some(prior) -> canonical_bytes_key_precedes(prior, asset_name)
      }
    let Pair(offset, quantity) = decode_canonical_int_at(item, offset)
    expect quantity != 0
    consume_assets(item, offset, remaining - 1, Some(asset_name))
  }
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
    outcome: outcome_scanning,
  }
}

pub fn fold_item_v1(
  state: FoldStateV1,
  item_index: Int,
  item: ByteArray,
) -> FoldStateV1 {
  expect and {
      state.outcome == outcome_scanning,
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
      expect canonical_policy_asset_count_v1(item) == header.declared_count
      FoldStateV1 {
        ..state,
        accumulated_count: next_count,
        previous_policy: header.policy_id,
        outcome: outcome_non_crossing,
      }
    }
  } else {
    expect next_count <= max_distinct_asset_count
    expect canonical_policy_asset_count_v1(item) == header.declared_count
    FoldStateV1 {
      ..state,
      accumulated_count: next_count,
      previous_policy: header.policy_id,
    }
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
