use midgard/computation_thread as ct
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  ReferenceScriptScanStateV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<ReferenceScriptScanStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
