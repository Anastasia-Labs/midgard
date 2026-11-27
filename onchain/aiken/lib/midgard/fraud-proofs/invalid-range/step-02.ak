use aiken_design_patterns/validity_range_normalization as vrn
use midgard/computation_thread as ct
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub type State {
  subject: substrate.VerdictSubjectV1,
  block_slot: Int,
  bad_tx_normalized_validity_range: vrn.NormalizedTimeRange,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
