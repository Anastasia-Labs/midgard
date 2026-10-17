use midgard/computation_thread as ct
use midgard/fraud_proofs/missing_redeemer/rule.{AuthenticatedStageTenV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  absolute_purpose_index: Int,
  purpose_siblings: List<ByteArray>,
  source_origin_kind: Int,
  source_key: ByteArray,
  source_language_tag: Int,
  source_script_hash: ByteArray,
  source_total_length: Int,
  source_item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
}

pub type Datum =
  ct.StepDatum<AuthenticatedStageTenV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
