use midgard/common/types.{Proof}
use midgard/ledger_state.{MidgardTxValidity, TxOrderEvent}
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<TxOrderEvent>

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  validity_override: MidgardTxValidity,
}
