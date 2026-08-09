use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// The thread carries the disputed transaction's **id**. Step-02 re-opens
/// field 0 through the §8.8 door and reads its authenticated item count, rather
/// than comparing a forwarded commitment against the pinned empty-field
/// constant: under §4's plain hashing that constant is the same value in all
/// nine slots, so a hash equality says "some field of this transaction is
/// empty" where the rule needs "field 0 is".
pub type State {
  bad_tx_id: MidgardTxId,
}

pub type Datum =
  ct.StepDatum<State>

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
