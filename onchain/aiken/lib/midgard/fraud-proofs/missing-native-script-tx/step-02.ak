use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// One id, carried the length of the family. Step-02 opens field 0 of the bad
/// transaction and step-06 opens its field 6 — two different fields of the same
/// transaction, both reached through the §8.8 door from this one id.
/// `bad_tx_witness_set_hash` is the second half of the §2.5 anchor, carried the
/// same distance as `bad_tx_id`. §3's transaction id preimage is the body alone,
/// so the id authenticates fields 0–5 and says nothing about the witness set;
/// step-06 opens field 6 and needs the `witness_set_hash` step-01 read off the
/// compact structure the block's counted `transactions_root` committed. It costs
/// the 32 bytes the retired `bad_tx_script_wits_hash` cost, and covers all three
/// witness-set fields instead of one.
pub type State {
  bad_tx_id: MidgardTxId,
  bad_tx_witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_input_index: Int,
  /// The prover's chosen §8 carriage for the bad transaction's field-0
  /// preimage.
  spend_inputs_opening: FieldOpeningV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
