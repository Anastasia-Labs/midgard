use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/dispute_bond_v1.{DisputeBondV1, SpendRedeemer}

/// The challenger bond of a validation case (`midgard/dispute_bond_v1`): spent
/// only by its own case's close, which refunds it when the case convicts and
/// forfeits it to the protocol treasury otherwise.
validator main(case_policy_id: PolicyId) {
  spend(
    datum: Option<DisputeBondV1>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    dispute_bond_v1.spend(case_policy_id, datum, redeemer, own_out_ref, tx)
  }

  else(_) {
    fail
  }
}
