use aiken/crypto.{VerificationKey}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// `verified_witness_set_hash` is the second half of the §2.5 anchor. §3's
/// transaction id preimage is the body alone, so the id authenticates fields
/// 0–5 and says nothing about the witness set; step-01 read the real
/// `witness_set_hash` off the compact structure the block's counted
/// `transactions_root` committed, and step-04 needs it to open field 7. It costs
/// the 32 bytes the retired `verified_addr_tx_wits_hash` cost, and covers all
/// three witness-set fields instead of one.
pub type State {
  missing_required_signer_vkey: VerificationKey,
  verified_tx_id: MidgardTxId,
  verified_witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// The prover's chosen §8 carriage for field 7's preimage, plus the witness
  /// set the door checks against the thread-anchored
  /// `verified_witness_set_hash`.
  addr_tx_wits_opening: FieldOpeningV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
