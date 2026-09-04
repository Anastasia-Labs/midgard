use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// The bad transaction's §2.5 anchor, both levels of it.
///
/// `bad_tx_id` alone would anchor fields 0–5 and nothing else: §3's id preimage
/// is the body, so the compact structure's trailing `witness_set_hash` is
/// outside it and a step-02 redeemer could supply any value there. Step-01 read
/// the real one off the structure the block's counted `transactions_root`
/// committed, so it is carried — the same 32 bytes the retired
/// `bad_addr_tx_wits_hash` cost, covering all three witness-set fields rather
/// than one.
pub type State {
  bad_tx_id: MidgardTxId,
  bad_tx_witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// The prover's chosen §8 carriage for field 7's preimage, together with the
  /// compact structures `bad_tx_id` authenticates and the witness set the door
  /// checks against their committed `witness_set_hash`.
  addr_tx_wits_opening: FieldOpeningV1,
  bad_addr_tx_wit_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
