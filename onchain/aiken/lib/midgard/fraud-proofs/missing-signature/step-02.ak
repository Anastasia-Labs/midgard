use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// One id, carried the length of the family. Steps 02 and 04 each re-open a
/// different field of the same transaction through the §8.8 door from it —
/// field 4 here, field 7 there — so the thread never carries a field hash whose
/// slot §4's plain hashing no longer records.
///
/// `verified_witness_set_hash` is the second half of the §2.5 anchor. §3's
/// transaction id preimage is the body alone, so the id authenticates fields
/// 0–5 and says nothing about the witness set; step-01 read the real
/// `witness_set_hash` off the compact structure the block's counted
/// `transactions_root` committed, and step-04 needs it to open field 7. It costs
/// the 32 bytes the retired `verified_addr_tx_wits_hash` cost, and covers all
/// three witness-set fields instead of one.
pub type State {
  verified_tx_id: MidgardTxId,
  verified_witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for field 4's preimage.
  required_signers_opening: FieldOpeningV1,
  bad_required_signer_hash_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
