use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/zero_input/rule.{StateV1}

pub type Datum =
  ct.StepDatum<StateV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// The prover's chosen §8 carriage for field 0's preimage — for a genuinely
  /// empty field, the single byte `80`.
  spend_inputs_opening: FieldOpeningV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
