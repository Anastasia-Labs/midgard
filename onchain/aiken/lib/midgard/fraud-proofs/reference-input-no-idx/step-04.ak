use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// Same shape as `input-no-idx` step-04's `State` — `MidgardTxId` then `Int` —
/// so the two families' step-03/step-04 pair keeps compiling to the same UPLC
/// (PlutusData erases the field names), exactly as step-01's header records.
pub type State {
  producing_tx_id: MidgardTxId,
  bad_reference_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// The prover's chosen §8 carriage for the producing transaction's field-2
  /// preimage. Its authenticated item count is the output count the
  /// out-of-range verdict rests on (§5.2).
  outputs_opening: FieldOpeningV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
