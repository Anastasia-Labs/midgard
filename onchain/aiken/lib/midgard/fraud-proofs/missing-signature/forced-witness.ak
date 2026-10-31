use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  verified_tx_id: ByteArray,
  verified_witness_set_hash: ByteArray,
  forced_source_key: ByteArray,
  signer_index: Int,
  required_signer_hash: Option<ByteArray>,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  addr_tx_wits_opening: Option<FieldOpeningV1>,
  witness_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
