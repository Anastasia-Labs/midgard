//// `da-hash-preimage` step-02 schema (GOAL_SPEC.md Q44).
////
//// Step 02 finalizes the proof by adjudicating the evidence triple step 01
//// committed into the computation thread.

use midgard/computation_thread as ct
use midgard/fraud_proofs/da_hash_preimage/rule.{VerdictV1}

pub type State {
  /// Total verdict step 01 derived from the exact authenticated raw leaf.
  verdict: VerdictV1,
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
