use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxId, MidgardTxsRoot}

pub type State {
  missing_ref_input_tx_id: MidgardTxId,
  blocks_transactions_root: MidgardTxsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  non_membership_proof_in_txs: Proof,
  non_membership_proof_script_redeemer_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
