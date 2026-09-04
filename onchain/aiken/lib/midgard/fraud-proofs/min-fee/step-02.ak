use cardano/assets.{Lovelace}
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{
  NativeTxCompact, NativeTxWitnessSetCompact,
}
use midgard/ledger_state.{MidgardTxId}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}

/// Everything step-01 read off the block-committed evidence that the fee
/// verdict needs.
///
/// `bad_tx` is the verified compact structure itself — the canonical-size
/// formula reads its scalar fields (`fee`, validity interval, `network_id`,
/// `validity_code`) and its `witness_set_hash` is the second half of the §2.5
/// anchor: §3's transaction id commits the body alone, so without the
/// thread-anchored hash a prover could authenticate an invented witness set
/// and carry inflated witness-field preimages to overstate the canonical size
/// (see `missing_signature/step_04.State.verified_witness_set_hash` for the
/// same argument at another family).
///
/// `min_fee_a` / `min_fee_b` are the disputed block header's own fee
/// schedule — the exact values `validation_claim_v1.validation_context_is_exact`
/// binds into the validation machine's context, read here off the header that
/// `pass_native_tx_to_next_step` authenticated against the state queue.
pub type State {
  bad_tx: NativeTxCompact,
  bad_tx_body_fee: Lovelace,
  bad_tx_id: MidgardTxId,
  min_fee_a: Int,
  min_fee_b: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// The disputed transaction's compact CBOR — re-derived against the
  /// thread-anchored `bad_tx_id` (and, for the witness half,
  /// `bad_tx.witness_set_hash`) before any field is opened.
  native_tx_compact_cbor: ByteArray,
  /// The three-hash compact witness set the door checks against the anchored
  /// `witness_set_hash` before fields 6–8 are opened.
  witness_set: NativeTxWitnessSetCompact,
  /// The prover's chosen §8 carriage for each of the nine field preimages, in
  /// §2.5 index order (0 = spend inputs … 8 = redeemers). Exactly nine.
  field_carriages: List<FieldCarriageV1>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
