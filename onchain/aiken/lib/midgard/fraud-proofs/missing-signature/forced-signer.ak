use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  verified_tx_id: ByteArray,
  verified_witness_set_hash: ByteArray,
  forced_source_key: ByteArray,
  signer_index: Int,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  required_signers_opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
