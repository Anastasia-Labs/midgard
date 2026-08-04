use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils
use midgard/ledger_state.{TxOrderEvent}
use midgard/transition_trace
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<TxOrderEvent>

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  membership_proof: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  validity_override: ledger_state.MidgardTxValidity,
}

pub fn forced_inclusion_key_value(
  tx_order_event_data: Data<TxOrderEvent>,
  validity_override: ledger_state.MidgardTxValidity,
  return: fn(ledger_state.TxOrderId, ledger_state.ForcedInclusionTx) -> result,
) -> result {
  let Pair(tx_order_event_id_data, tx_order_event_tx_data) =
    ledger_state.unsafe_event_to_key_value_pair(tx_order_event_data)

  expect tx_order_id: ledger_state.TxOrderId = tx_order_event_id_data
  expect tx_order_event_tx: ledger_state.MidgardTxCompact =
    tx_order_event_tx_data

  return(
    tx_order_id,
    ledger_state.ForcedInclusionTx {
      tx_compact: ledger_state.MidgardTxCompactWithoutValidity {
        body: tx_order_event_tx.body,
        wits: tx_order_event_tx.wits,
      },
      operator_validity: validity_override,
    },
  )
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
