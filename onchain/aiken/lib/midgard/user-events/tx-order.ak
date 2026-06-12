use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{Proof}
use midgard/common/utils
use midgard/ledger_state.{TxOrderEvent}
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
  validity_override: ledger_state.MidgardTxValidity,
}

pub fn set_tx_validity_and_get_compact_info(
  tx_order_event_data: Data<TxOrderEvent>,
  validity_override: ledger_state.MidgardTxValidity,
  return: fn(ledger_state.MidgardTxId, ledger_state.MidgardTxCompact) -> result,
) -> result {
  let Pair(_tx_order_event_id, tx_order_event_tx_data) =
    ledger_state.unsafe_event_to_key_value_pair(tx_order_event_data)

  expect tx_order_event_tx: ledger_state.MidgardTx = tx_order_event_tx_data

  let init_compact_tx = tx_order_event_tx |> ledger_state.midgard_tx_to_compact
  let tx_id = init_compact_tx.body |> utils.serialise_and_hash_32

  let overridden_tx_order_compact_tx =
    ledger_state.MidgardTxCompact {
      ..init_compact_tx,
      validity: validity_override,
    }
  return(tx_id, overridden_tx_order_compact_tx)
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
