use aiken/crypto.{ScriptHash}
use aiken_design_patterns/stake_validator
use cardano/transaction.{Datum, OutputReference, Transaction}
use midgard/accounting/reserve.{SpendRedeemer}

validator spend(observer_withdraw_script_hash: ScriptHash) {
  spend(
    _datum: Option<Datum>,
    observer_withdraw_redeemer_index: SpendRedeemer,
    _own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    stake_validator.validate_withdraw_minimal(
      withdraw_script_hash: observer_withdraw_script_hash,
      withdrawals: tx.withdrawals,
      withdrawal_index: observer_withdraw_redeemer_index,
    )
  }

  else(_) {
    fail
  }
}
