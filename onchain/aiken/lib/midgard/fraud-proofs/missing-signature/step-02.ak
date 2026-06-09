use aiken/crypto.{VerificationKeyHash}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{AddressWitsRoot, SignersRoot}

pub type State {
  verified_required_signers_root: SignersRoot,
  verified_addr_tx_wits: AddressWitsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_required_signer_hash_index: Int,
  bad_required_signer_hash: VerificationKeyHash,
  bad_required_signer_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
