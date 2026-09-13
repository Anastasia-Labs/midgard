use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// Two transaction ids, because two transactions are in play: the one that
/// *produced* the spent output (whose field 2 this step opens) and the bad one
/// (whose field 6 step-06 opens).
/// `bad_tx_witness_set_hash` is the second half of the §2.5 anchor, carried the
/// same distance as `bad_tx_id`. §3's transaction id preimage is the body alone,
/// so the id authenticates fields 0–5 and says nothing about the witness set;
/// step-06 opens field 6 and needs the `witness_set_hash` step-01 read off the
/// compact structure the block's counted `transactions_root` committed. It costs
/// the 32 bytes the retired `bad_tx_script_wits_hash` cost, and covers all three
/// witness-set fields instead of one.
pub type State {
  producing_tx_id: MidgardTxId,
  bad_input_output_index: Int,
  bad_tx_id: MidgardTxId,
  bad_tx_witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for the producing transaction's field-2
  /// preimage.
  outputs_opening: FieldOpeningV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
