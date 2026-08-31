use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NonMembershipCarriage}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_tx_types
use midgard/ledger_state.{MidgardLedgerRoot, MidgardTxsRoot}

pub type State {
  missing_reference_input: MidgardTxInput,
  blocks_prev_utxos_root: MidgardLedgerRoot,
  blocks_transactions_root: MidgardTxsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  non_membership_in_ledger: NonMembershipCarriage,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
