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
  /// Empty before the first batch; thereafter the §10 checkpoint hash emitted
  /// by the preceding authenticated walk transaction.
  field_walk_checkpoint_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  /// Advance exactly one bounded absence batch and self-loop at step-04.
  Scan {
    input_index: Int,
    output_index: Int,
    addr_tx_wits_opening: FieldOpeningV1,
    /// `None` only at the initial position; every resume supplies the exact
    /// 53 bytes committed by `field_walk_checkpoint_hash`.
    checkpoint_cbor: Option<ByteArray>,
  }
  /// Walk the final (at most one-batch) suffix and mint the permanent proof.
  Finalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    addr_tx_wits_opening: FieldOpeningV1,
    checkpoint_cbor: Option<ByteArray>,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
