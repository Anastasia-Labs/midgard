use cardano/transaction.{OutputReference, Transaction}
use midgard/ledger_state.{CekProgramMaterialDatumV1}

/// Midgard V1 program material is append-only L1 data availability state.
/// Publication is permissionless because each node is self-authenticating;
/// spending is impossible so an operator cannot erase a forced submitter's
/// material before inclusion or challenge.
validator spend {
  spend(
    _datum: Option<CekProgramMaterialDatumV1>,
    _redeemer: Data,
    _own_out_ref: OutputReference,
    _tx: Transaction,
  ) {
    False
  }

  else(_) {
    fail
  }
}
