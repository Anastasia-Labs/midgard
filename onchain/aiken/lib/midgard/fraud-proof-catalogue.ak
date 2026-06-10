use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use aiken_design_patterns/linked_list/nested as nll
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{Empty}

pub const asset_name: AssetName = "MIDGARD_FRAUD_PROOF_CATALOGUE"

pub const root_key: AssetName = asset_name

pub const node_key_prefix: ByteArray = "MFPC"

pub const node_key_prefix_length: Int = bytearray.length(node_key_prefix)

pub const fraud_proof_id_byte_count = 4

pub const step_byte_count = 4

pub const first_step_id = 0

pub const first_step_key = step_key(first_step_id)

pub type IsLocked =
  Bool

/// Catalogue elements do not carry payload data; fraud-proof steps carry their
/// script through the output's reference-script slot.
pub type Metadata {
  step_count: Int,
}

pub type Datum =
  nll.Element<IsLocked, Metadata, Empty>

pub type SpendRedeemer {
  ListStateTransition
  LockCatalogue { root_input_index: Int, continued_root_output_index: Int }
}

pub type MintRedeemer {
  Init { output_index: Int }
  AddFraudProof {
    fraud_proof_id: Int,
    step_output_count: Int,
    m_root_ref_input_index: Option<Int>,
  }
  AddFraudProofStep {
    fraud_proof_id: Int,
    step_id: Int,
    continued_anchor_output_index: Int,
    step_output_index: Int,
  }
}

pub fn fraud_proof_key_from_id(fraud_proof_id: Int) -> ByteArray {
  bytearray.from_int_big_endian(fraud_proof_id, fraud_proof_id_byte_count)
}

pub fn step_key(step_id: Int) -> ByteArray {
  bytearray.from_int_big_endian(step_id, step_byte_count)
}

pub fn fraud_proof_step_key(
  fraud_proof_key: ByteArray,
  step_id: Int,
) -> ByteArray {
  fraud_proof_key |> bytearray.concat(step_key(step_id))
}

pub fn finalize_linked_list(
  eval: linked_list.ElementEval<Bool>,
  nft_policy_id: PolicyId,
) -> Bool {
  eval
    |> linked_list.run_element_with(
        nft_policy_id,
        root_key,
        node_key_prefix,
        node_key_prefix_length,
      )
}

pub fn finalize_linked_list_root(
  eval: linked_list.RootEval<Bool>,
  nft_policy_id: PolicyId,
) -> Bool {
  eval
    |> linked_list.run_root_with(nft_policy_id, root_key)
}
