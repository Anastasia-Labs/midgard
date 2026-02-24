use aiken/crypto.{Signature, VerificationKey, VerificationKeyHash}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxBodyCompact}

pub type State {
  bad_tx_body: MidgardTxBodyCompact,
  bad_addr_tx_wit_verification_key_hash: VerificationKeyHash,
  verification_key: VerificationKey,
  signature: Signature,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
