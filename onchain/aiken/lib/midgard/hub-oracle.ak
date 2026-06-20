use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils

pub const asset_name: AssetName = "HubOracleMint"

pub type Datum {
  registered_operators: PolicyId,
  active_operators: PolicyId,
  retired_operators: PolicyId,
  scheduler: PolicyId,
  state_queue: PolicyId,
  fraud_proof: PolicyId,
  deposit: PolicyId,
  withdrawal: PolicyId,
  tx_order: PolicyId,
  settlement: PolicyId,
  payout: PolicyId,
  registered_operators_addr: Address,
  active_operators_addr: Address,
  retired_operators_addr: Address,
  scheduler_addr: Address,
  state_queue_addr: Address,
  fraud_proof_addr: Address,
  deposit_addr: Address,
  withdrawal_addr: Address,
  tx_order_addr: Address,
  settlement_addr: Address,
  reserve_addr: Address,
  payout_addr: Address,
  reserve_observer: ScriptHash,
}

pub fn get_datum(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  hub_ref_input_index: Int,
) -> Datum {
  expect datum: Datum =
    utils.get_authentic_input_datum_with_nft_at(
      reference_inputs,
      hub_oracle_script_hash,
      asset_name,
      hub_ref_input_index,
    )
  datum
}

pub fn get_authenticated_script_hash(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  role_asset_name: AssetName,
  reference_input_index: Int,
) -> ScriptHash {
  expect Input {
    output: Output { value, reference_script: Some(script_hash), .. },
    ..
  } = reference_inputs |> list.expect_at(reference_input_index)

  expect assets.has_nft_strict(value, hub_oracle_script_hash, role_asset_name)

  script_hash
}

pub fn get_authenticated_address(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  role_asset_name: AssetName,
  reference_input_index: Int,
) -> Address {
  expect Input {
    output: Output { value, datum: InlineDatum(address_data), .. },
    ..
  } = reference_inputs |> list.expect_at(reference_input_index)

  expect assets.has_nft_strict(value, hub_oracle_script_hash, role_asset_name)

  expect script_address: Address = address_data
  script_address
}
