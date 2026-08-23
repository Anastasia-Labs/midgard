use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{WithdrawalId, WithdrawalInfo, WithdrawalsRoot}
use midgard/transition_trace

pub type State {
  missing_reference_input: MidgardTxInput,
  blocks_withdrawals_root: WithdrawalsRoot,
  blocks_withdrawal_count: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// Counted-root membership witness for the withdrawal event under the
  /// header's `withdrawals_root` (`WithdrawalsRootDomain`).
  withdrawal_membership: transition_trace.RootMembershipProof<
    WithdrawalId,
    WithdrawalInfo,
  >,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
