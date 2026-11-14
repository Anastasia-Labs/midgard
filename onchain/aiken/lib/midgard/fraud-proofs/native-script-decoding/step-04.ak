//// Datum and redeemer wire shapes for the decoding-fault thread's final
//// step: check the proven verdict's shape per direction and finalize into a
//// permanent fraud-proof token (#635, #633).

use midgard/computation_thread as ct
use midgard/fraud_proofs/native_script_decoding/engine.{ScanThreadStateV1}

pub type State =
  ScanThreadStateV1

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
