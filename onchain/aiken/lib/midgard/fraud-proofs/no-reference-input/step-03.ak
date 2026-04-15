use cardano/transaction.{OutputReference}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{
  MidgardInputId, MidgardInputsRoot, MidgardLedgerRoot, MidgardOutput,
  WithdrawalId, WithdrawalInfo,
}

pub type State {
  missing_output_reference: OutputReference,
  blocks_prev_utxos_root: MidgardLedgerRoot,
  blocks_transactions_root: MidgardTxsRoot,
  blocks_withdrawals_root: WitdhrawalsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type MissingCause {
  NonExistentInPrevLedger {
    non_membership_proof_in_ledger: Proof,
    non_membership_proof_script_redeemer_index: Int,
  }
  SpentInWithdrawal {
    withdrawal_id: WithdrawalId,
    withdrawal_info: WithdrawalInfo,
    membership_proof: Proof,
    membership_proof_script_redeemer_index: Int,
  }
}

pub type Args {
  input_index: Int,
  output_index: Int,
  missing_cause: MissingCause,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
