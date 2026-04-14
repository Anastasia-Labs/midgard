use aiken/crypto.{Signature, VerificationKey, VerificationKeyHash}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{AddressWitsRoot, MidgardTxBodyCompact}

pub type State {
  bad_tx_body: MidgardTxBodyCompact,
  bad_addr_tx_wits_root: AddressWitsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_addr_tx_wit_verification_key_hash: VerificationKeyHash,
  bad_addr_tx_wit: (VerificationKey, Signature),
  bad_addr_tx_wit_membership_proof: Proof,
  plutarch_phas_withdraw_redeemer_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
