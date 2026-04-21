use cardano/transaction.{OutputReference}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{WithdrawalId, WithdrawalInfo, WithdrawalsRoot}

pub type State {
  missing_output_reference: OutputReference,
  blocks_withdrawals_root: WithdrawalsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  withdrawal_id: WithdrawalId,
  withdrawal_info: WithdrawalInfo,
  membership_proof: Proof,
  membership_proof_script_redeemer_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
