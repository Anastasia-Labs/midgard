use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NonMembershipCarriage}
use midgard/ledger_state.{MidgardTxId, MidgardTxsRoot}

pub type State {
  missing_reference_input_tx_id: MidgardTxId,
  blocks_transactions_root: MidgardTxsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  non_membership_in_txs: NonMembershipCarriage,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
