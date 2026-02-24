use aiken/crypto.{VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardTxWitnessSetCompact}

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  tx_inclusion_args: TxInclusionArgs,
  bad_tx_wits: MidgardTxWitnessSetCompact,
  bad_required_signer_hash_index: Int,
  bad_required_signer_hash: VerificationKeyHash,
  bad_required_signer_membership_proof: mpf.Proof,
  plutarch_phas_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
