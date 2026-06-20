use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/common/utils

/// Hub oracle UTxOs are reference-only protocol UTxOs.
///
/// This validator is intentionally fail-closed for spending: the hub oracle
/// datum UTxO and hub oracle UTxOs carrying reference scripts live at this
/// address and must never be spent by normal protocol transactions.
validator main(hereafter: Int) {
  mint(_redeemer: Data, _policy_id: PolicyId, tx: Transaction) {
    let Transaction { validity_range, .. } = tx

    // 1. Minting must happen no later than the configured deadline, equivalent
    //    to a native-script `invalid_hereafter` timelock.
    expect validity_range |> utils.is_herebefore(hereafter)

    True
  }

  else(_) {
    trace @"Midgard - Hub Oracle Spending Validator"
    fail
  }
}
