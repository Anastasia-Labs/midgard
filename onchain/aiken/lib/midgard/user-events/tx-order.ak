use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{Proof}
use midgard/common/utils
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

pub fn get_datum(
  reference_inputs: List<Input>,
  tx_order_policy_id: PolicyId,
  tx_order_input_index: Int,
) -> Datum {
  expect Input {
    output: Output { datum: InlineDatum(tx_order_datum_data), .. },
    ..
  } =
    utils.get_authentic_input_with_policy_at(
      reference_inputs,
      tx_order_policy_id,
      tx_order_input_index,
    )
  expect tx_order_datum: Datum = tx_order_datum_data
  tx_order_datum
}
