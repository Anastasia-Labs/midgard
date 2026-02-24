use aiken/crypto.{Signature, VerificationKey, VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use midgard/common/types.{MerkleRoot}
use midgard/computation_thread as ct

pub type State {
  bad_addr_tx_wits: MerkleRoot<
    VerificationKeyHash,
    (VerificationKey, Signature),
  >,
  bad_required_signer_hash: VerificationKeyHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  signature_exclusion_proof: mpf.Proof,
  plutarch_pexcludes_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
