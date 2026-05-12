use cardano/transaction.{OutputReference}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardLedgerRoot, MidgardTxsRoot}

pub type State {
  missing_output_reference: OutputReference,
  blocks_prev_utxos_root: MidgardLedgerRoot,
  blocks_transactions_root: MidgardTxsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  non_membership_proof_in_ledger: Proof,
  non_membership_proof_script_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
