use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils

pub const asset_name: AssetName = #""

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
  expect hub_datum: Datum = hub_datum_data
  hub_datum
}
