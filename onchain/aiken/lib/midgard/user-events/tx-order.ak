use aiken/crypto.{blake2b_256}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{Proof}
use midgard/common/utils
use midgard/fraud_proofs/native_tx/codec.{validity_to_code}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact,
}
use midgard/fraud_proofs/native_tx/transaction as native_tx
use midgard/fraud_proofs/native_tx/types as native_tx_types
use midgard/ledger_state.{TxOrderEvent}
use midgard/user_events

pub const hub_oracle_mint_asset_name: AssetName = "TxOrderMint"

pub const hub_oracle_spend_asset_name: AssetName = "TxOrderSpend"

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
  validity_override: native_tx_types.MidgardTxValidity,
}

pub fn set_tx_validity_and_get_compact_info(
  tx_order_event_data: Data<TxOrderEvent>,
  validity_override: native_tx_types.MidgardTxValidity,
  return: fn(ByteArray, ByteArray) -> result,
) -> result {
  let Pair(_tx_order_event_id, tx_order_event_tx_data) =
    ledger_state.unsafe_event_to_key_value_pair(tx_order_event_data)

  expect tx_order_event_tx: native_tx_types.MidgardTransaction =
    tx_order_event_tx_data
  expect tx_order_event_tx.version == native_tx_types.native_tx_version

  let init_compact_tx =
    tx_order_event_tx |> native_tx.midgard_transaction_to_compact
  let tx_id =
    init_compact_tx.body |> encode_native_tx_body_compact |> blake2b_256

  let overridden_tx_order_compact_tx =
    native_tx_types.NativeTxCompact {
      ..init_compact_tx,
      validity_code: validity_override |> validity_to_code,
    }
  return(tx_id, overridden_tx_order_compact_tx |> encode_native_tx_compact)
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
