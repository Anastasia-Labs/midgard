use aiken/crypto.{VerificationKey}
use midgard/computation_thread as ct
use midgard/ledger_state.{AddressWits, AddressWitsHash, MidgardTxId}

pub type State {
  bad_tx_id: MidgardTxId,
  bad_addr_tx_wits_hash: AddressWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  addr_tx_wits_preimage: AddressWits,
  bad_addr_tx_wit_verification_key: VerificationKey,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
