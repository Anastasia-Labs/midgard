use midgard/computation_thread as ct
use midgard/fraud_proofs/transition_trace/proof.{TransitionFaultProof}

pub type Datum =
  ct.StepDatum<TransitionFaultProof>

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
