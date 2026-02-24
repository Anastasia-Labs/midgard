use aiken/crypto.{Signature, VerificationKey, VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardTxWitnessSetCompact}

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  tx_inclusion_args: TxInclusionArgs,
  bad_tx_wits: MidgardTxWitnessSetCompact,
  bad_addr_tx_wit_verification_key_hash: VerificationKeyHash,
  bad_addr_tx_wit: (VerificationKey, Signature),
  bad_addr_tx_wit_membership_proof: mpf.Proof,
  plutarch_phas_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
