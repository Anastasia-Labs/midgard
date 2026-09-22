use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

pub type State {
  verified_tx1_id: MidgardTxId,
  verified_tx2_id: MidgardTxId,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for tx1's field-0 preimage.
  tx1_spend_inputs_opening: FieldOpeningV1,
  double_spent_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
