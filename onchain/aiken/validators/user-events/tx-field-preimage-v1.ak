use cardano/transaction.{OutputReference, Transaction}
use midgard/ledger_state.{TxFieldPreimageV1}
use midgard/user_events/tx_order_v1.{field_fragment_burn_authorized}

validator spend {
  spend(
    datum: Option<TxFieldPreimageV1>,
    _redeemer: Data,
    _own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(field) = datum
    field_fragment_burn_authorized(field, tx.mint)
  }

  else(_) {
    fail
  }
}
