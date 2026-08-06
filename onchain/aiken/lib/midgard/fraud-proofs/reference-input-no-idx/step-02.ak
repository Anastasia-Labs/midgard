use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{MidgardInputsHash}

pub type State {
  verified_tx_reference_inputs_hash: MidgardInputsHash,
}

pub type Datum =
  ct.StepDatum<State>

/// `reference_inputs_preimage` is the producing transaction list the bad
/// transaction *reads*. It opens the reference-inputs commitment carried from
/// step-01 under consensus field 1, so a prover may only forward a reference
/// input the committed transaction genuinely names.
pub type Args {
  input_index: Int,
  output_index: Int,
  reference_inputs_preimage: List<MidgardTxInput>,
  bad_reference_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
