use midgard/computation_thread as ct
use midgard/fraud_proofs/script_integrity_hash_missing/state.{DecisionStateV1}

pub type State =
  DecisionStateV1

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
