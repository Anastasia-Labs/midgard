use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardAddressWitness}
use midgard/ledger_state.{AddressWitsHash, MidgardTxId}

pub type State {
  bad_tx_id: MidgardTxId,
  bad_addr_tx_wits_hash: AddressWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// Canonical native address-witness collection preimage (list of
  /// verification-key/signature pairs, canonically CBOR-encoded per item).
  addr_tx_wits_preimage: List<MidgardAddressWitness>,
  bad_addr_tx_wit_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
