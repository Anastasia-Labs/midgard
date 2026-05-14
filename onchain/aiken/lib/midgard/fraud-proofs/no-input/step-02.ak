use midgard/computation_thread as ct
use midgard/ledger_state.{
  MidgardInputs, MidgardInputsHash, MidgardLedgerRoot, MidgardTxsRoot,
}

pub type State {
  bad_tx_inputs_hash: MidgardInputsHash,
  blocks_prev_utxos_root: MidgardLedgerRoot,
  blocks_transactions_root: MidgardTxsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  inputs_preimage: MidgardInputs,
  bad_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
