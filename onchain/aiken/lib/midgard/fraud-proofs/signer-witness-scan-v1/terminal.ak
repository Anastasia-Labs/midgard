use midgard/computation_thread as ct
use midgard/fraud_proofs/signer_witness_scan_v1/state.{VerdictV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<VerdictV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
