use aiken/crypto.{VerificationKeyHash}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{AddressWitsRoot}

pub type State {
  missing_required_signer_hash: VerificationKeyHash,
  verified_addr_tx_wits: AddressWitsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  signature_exclusion_proof: Proof,
  non_membership_proof_script_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
