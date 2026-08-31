use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/missing_native_script_tx/step_06.{State}

pub type Datum =
  ct.StepDatum<State>

/// The semantic absence accumulator never leaves step08. Its only exits are
/// another authenticated batch at the same validator or final fraud-proof
/// token minting at the exact semantic terminal.
pub type Args {
  ResumeSemanticScan {
    input_index: Int,
    output_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  FinalizeSemanticScan {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
