use cardano/transaction.{OutputReference, Transaction}
use midgard/ledger_state.{TxFieldReceiptV1}
use midgard/user_events/tx_order_v1.{field_receipt_burn_authorized}

validator spend {
  spend(
    datum: Option<TxFieldReceiptV1>,
    _redeemer: Data,
    _own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(receipt) = datum
    field_receipt_burn_authorized(receipt, tx.mint)
  }

  else(_) {
    fail
  }
}
