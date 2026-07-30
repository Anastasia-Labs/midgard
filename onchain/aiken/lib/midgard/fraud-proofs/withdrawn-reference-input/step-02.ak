use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{MidgardInputsHash, WithdrawalsRoot}

pub type State {
  bad_tx_reference_inputs_hash: MidgardInputsHash,
  blocks_withdrawals_root: WithdrawalsRoot,
  blocks_withdrawal_count: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  reference_inputs_preimage: List<MidgardTxInput>,
  bad_reference_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
