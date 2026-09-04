use midgard/computation_thread as ct
use midgard/fraud_proofs/redeemer_canonicity/rule.{TerminalStateV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<TerminalStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
