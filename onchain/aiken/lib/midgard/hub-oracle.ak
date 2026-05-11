use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils

pub const asset_name: AssetName = "MIDGARD_HUB_ORACLE"

pub type Datum {
  registered_operators: PolicyId,
  active_operators: PolicyId,
  retired_operators: PolicyId,
  scheduler: PolicyId,
  state_queue: PolicyId,
  fraud_proof_catalogue: PolicyId,
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
  fraud_proof_catalogue_addr: Address,
  fraud_proof_addr: Address,
  deposit_addr: Address,
  withdrawal_addr: Address,
  tx_order_addr: Address,
  settlement_addr: Address,
  reserve_addr: Address,
  payout_addr: Address,
  reserve_observer: ScriptHash,
}

/// The assumption here is that the UTxO with the datum is authenticated with an
/// NFT which its policy is the same as the address's payment script hash.
pub fn get_datum(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  hub_ref_input_index: Int,
) -> Datum {
  expect Input { output: Output { datum: InlineDatum(hub_datum_data), .. }, .. } =
    utils.get_authentic_input_of(
      reference_inputs,
      hub_oracle_script_hash,
      asset_name,
      hub_ref_input_index,
    )
  expect Pair(0, hub_fields) = builtin.un_constr_data(hub_datum_data)
  expect [
    registered_operators_data, active_operators_data, retired_operators_data,
    scheduler_data, state_queue_data, fraud_proof_catalogue_data,
    fraud_proof_data, deposit_data, withdrawal_data, tx_order_data,
    settlement_data, payout_data, registered_operators_addr_data,
    active_operators_addr_data, retired_operators_addr_data, scheduler_addr_data,
    state_queue_addr_data, fraud_proof_catalogue_addr_data,
    fraud_proof_addr_data, deposit_addr_data, withdrawal_addr_data,
    tx_order_addr_data, settlement_addr_data, reserve_addr_data,
    payout_addr_data, reserve_observer_data,
  ] = hub_fields
  expect registered_operators_addr: Address = registered_operators_addr_data
  expect active_operators_addr: Address = active_operators_addr_data
  expect retired_operators_addr: Address = retired_operators_addr_data
  expect scheduler_addr: Address = scheduler_addr_data
  expect state_queue_addr: Address = state_queue_addr_data
  expect fraud_proof_catalogue_addr: Address = fraud_proof_catalogue_addr_data
  expect fraud_proof_addr: Address = fraud_proof_addr_data
  expect deposit_addr: Address = deposit_addr_data
  expect withdrawal_addr: Address = withdrawal_addr_data
  expect tx_order_addr: Address = tx_order_addr_data
  expect settlement_addr: Address = settlement_addr_data
  expect reserve_addr: Address = reserve_addr_data
  expect payout_addr: Address = payout_addr_data

  Datum {
    registered_operators: builtin.un_b_data(registered_operators_data),
    active_operators: builtin.un_b_data(active_operators_data),
    retired_operators: builtin.un_b_data(retired_operators_data),
    scheduler: builtin.un_b_data(scheduler_data),
    state_queue: builtin.un_b_data(state_queue_data),
    fraud_proof_catalogue: builtin.un_b_data(fraud_proof_catalogue_data),
    fraud_proof: builtin.un_b_data(fraud_proof_data),
    deposit: builtin.un_b_data(deposit_data),
    withdrawal: builtin.un_b_data(withdrawal_data),
    tx_order: builtin.un_b_data(tx_order_data),
    settlement: builtin.un_b_data(settlement_data),
    payout: builtin.un_b_data(payout_data),
    registered_operators_addr,
    active_operators_addr,
    retired_operators_addr,
    scheduler_addr,
    state_queue_addr,
    fraud_proof_catalogue_addr,
    fraud_proof_addr,
    deposit_addr,
    withdrawal_addr,
    tx_order_addr,
    settlement_addr,
    reserve_addr,
    payout_addr,
    reserve_observer: builtin.un_b_data(reserve_observer_data),
  }
}
