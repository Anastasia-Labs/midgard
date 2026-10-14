use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/dispute_bond_v1.{DisputeBondV1, SpendRedeemer}

/// The validation-dispute bond (`midgard/dispute_bond_v1`): refunded to the
/// prover on any conviction of the challenged block, forfeited to the protocol
/// treasury after `forfeit_after`.
validator main(fraud_proof_policy_id: PolicyId) {
  spend(
    datum: Option<DisputeBondV1>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    dispute_bond_v1.spend(
      fraud_proof_policy_id,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}
