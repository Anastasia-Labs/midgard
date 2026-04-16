use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{
  MidgardInputId, MidgardInputsRoot, MidgardLedgerRoot, MidgardOutput,
  MidgardTxsRoot, WithdrawalsRoot,
}

pub type State {
  bad_tx_reference_inputs_root: MidgardInputsRoot,
  blocks_prev_utxos_root: MidgardLedgerRoot,
  blocks_transactions_root: MidgardTxsRoot,
  blocks_withdrawals_root: WitdhrawalsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  no_reference_input_id: MidgardInputId,
  no_reference_input_resolved: MidgardOutput,
  membership_proof_in_bad_tx: Proof,
  membership_proof_script_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
