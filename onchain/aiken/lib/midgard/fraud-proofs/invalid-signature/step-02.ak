use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, NativeTxWitnessSetCompact,
}
use midgard/ledger_state.{MidgardTxId, MidgardTxWitsHash}

pub type State {
  bad_tx_id: MidgardTxId,
  bad_tx_wits_hash: MidgardTxWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

/// The bad transaction's witness set is committed as a single
/// `witness_set_hash` in the native compact encoding, so reaching an individual
/// signature takes two preimages:
///
/// - `witness_set_preimage` re-encodes to `bad_tx_wits_hash`, which exposes the
///   `addr_tx_wits_hash`, and
/// - `addr_tx_wits_preimage` re-encodes to that `addr_tx_wits_hash`.
///
/// Both are supplied here rather than split across steps: this proof needs only
/// one transaction-inclusion proof (taken in step 01), so it is not subject to
/// the one-`plutarch_phas`-per-transaction limit that forces the
/// `input-no-idx` family to spread its preimages over several steps.
///
/// `bad_address_witness_index` selects the offending witness positionally.
/// Because `encode_address_witness_preimage` is length prefixed, a preimage
/// matching the hash fixes both the witness count and each witness's position,
/// so the index is unambiguous even if a verification key were to appear twice.
pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  witness_set_preimage: NativeTxWitnessSetCompact,
  addr_tx_wits_preimage: List<MidgardAddressWitness>,
  bad_address_witness_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
