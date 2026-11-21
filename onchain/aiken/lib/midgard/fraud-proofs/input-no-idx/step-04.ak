use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

pub type State {
  producing_tx_id: MidgardTxId,
  bad_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// The prover's chosen §8 carriage for the producing transaction's field-2
  /// preimage.
  outputs_opening: FieldOpeningV1,
  /// `None` takes the output count from walking field 2; `Some(i)` takes it
  /// from the field grammar certificate at reference input `i`, so the
  /// certified count also serves a chunked field.
  grammar_ref_input_index: Option<Int>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
