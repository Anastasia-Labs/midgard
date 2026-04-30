use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{Datum, OutputReference, Transaction}

validator spend(payout_policy_id: PolicyId, payout_addr: Address) {
  spend(
    _datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    todo
  }

  else(_) {
    fail
  }
}
