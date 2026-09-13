use aiken/primitive/bytearray
use midgard/cek_context_chain as chain
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/validation_machine/machine_types.{
  CekContextControlV1, CekRedeemerContextControlV1,
}
use midgard/validation_machine/witness.{
  cek_redeemer_context_control_is_well_formed,
  hash_cek_redeemer_context_control_v1,
}

pub type RedeemerSelectionPending {
  staged: Data,
  control: CekRedeemerContextControlV1,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  redeemer_leaf: ByteArray,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
}

pub fn context_successor(
  staged: chain.ContextStaged,
  next: CekRedeemerContextControlV1,
) -> CekContextControlV1 {
  expect
    cek_redeemer_context_control_is_well_formed(
      staged.native.redeemer_count,
      next,
    )
  let completed = next.cursor == staged.native.redeemer_count
  if completed {
    expect next.active_scan_hash == #""
    expect bytearray.length(next.current_redeemer.root) == 32
    CekContextControlV1 {
      ..staged.context,
      stage: 10,
      redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(next),
    }
  } else {
    CekContextControlV1 {
      ..staged.context,
      redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(next),
    }
  }
}

pub type RedeemerPurposePending {
  selection: RedeemerSelectionPending,
  purpose: Option<DataSummaryV1>,
}

pub type RedeemerInitialPending {
  selection: RedeemerSelectionPending,
  purpose: Option<DataSummaryV1>,
  initial_hash: ByteArray,
}
