//// The address-witness signer frontier. Each page opens field 7 through the
//// shared witness-set door and appends the next run of signers, in field
//// order, to a compact frontier whose leaves are `blake2b_256` of each
//// signer's key hash. Field 7 has a fixed stride, so its authenticated view
//// carries the exact item count and the frontier is complete when it covers
//// every item.

use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/execution_native_script_invalid/types.{EvalV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, address_witnesses_field_index, opened_field_view,
}
use midgard/native_tx_field_access_v1.{
  field_item_count, fixed_stride_item_batch,
}
use midgard/validation_merkle_v1

pub type PageV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  budget: Int,
}

pub type Datum =
  ct.StepDatum<EvalV1>

pub type SpendRedeemer =
  ct.StepRedeemer<PageV1>

/// The key hash of an address-witness item `[vkey, signature]`: the vkey is
/// the 32 bytes after the array and bytes heads.
pub fn signer_of_item_v1(item: ByteArray) -> ByteArray {
  blake2b_224(bytearray.slice(item, 3, 34))
}

pub fn signer_leaf_v1(key_hash: ByteArray) -> ByteArray {
  blake2b_256(key_hash)
}

fn append_signers_v1(
  items: List<ByteArray>,
  count: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> List<validation_merkle_v1.FrontierPeak> {
  when items is {
    [] -> peaks
    [item, ..rest] ->
      append_signers_v1(
        rest,
        count + 1,
        validation_merkle_v1.append_leaf(
          count,
          peaks,
          signer_leaf_v1(signer_of_item_v1(item)),
        ),
      )
  }
}

/// One page: up to `budget` more signers. Returns whether the frontier now
/// covers the whole field.
pub fn page_v1(
  state: EvalV1,
  page: PageV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<Bool, EvalV1> {
  expect page.budget > 0
  let view = opened_field_view(
    page.opening,
    cursor.anchor_v1(state.bound, address_witnesses_field_index),
    address_witnesses_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  let count = field_item_count(view)
  let remaining = count - state.signer_count
  let take = if remaining < page.budget {
    remaining
  } else {
    page.budget
  }
  let peaks = if take == 0 {
    state.signer_peaks
  } else {
    append_signers_v1(
      fixed_stride_item_batch(view, state.signer_count, take),
      state.signer_count,
      state.signer_peaks,
    )
  }
  Pair(
    take == remaining,
    EvalV1 {
      ..state,
      signer_count: state.signer_count + take,
      signer_peaks: peaks,
    },
  )
}
