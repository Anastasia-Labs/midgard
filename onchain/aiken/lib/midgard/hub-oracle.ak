use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use midgard/common/utils

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
/// NFT whose policy is the same as the address's payment script hash.
///
/// The hub-oracle minting policy already fixes the singleton asset name, so
/// readers should authenticate by policy and address rather than hardcoding a
/// specific token name. That keeps the reference input rule compatible with
/// existing deployments that minted the singleton under a non-empty asset name.
fn get_authenticated_input(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  hub_ref_input_index: Int,
) -> Input {
  expect Some(
    Input {
      output: Output {
        address: Address { payment_credential: Script(refs_script_hash), .. },
        value: refs_value,
        ..
      },
      ..
    } as found_ref_input,
  ) = list.at(reference_inputs, hub_ref_input_index)

  expect (refs_nft_policy_id, _, 1) =
    utils.get_single_asset_from_value_apart_from_ada(refs_value)

  expect and {
      refs_script_hash == hub_oracle_script_hash,
      refs_nft_policy_id == hub_oracle_script_hash,
    }

  found_ref_input
}

pub fn get_datum(
  reference_inputs: List<Input>,
  hub_oracle_script_hash: ScriptHash,
  hub_ref_input_index: Int,
) -> Datum {
  expect Input { output: Output { datum: InlineDatum(hub_datum_data), .. }, .. } =
    get_authenticated_input(
      reference_inputs,
      hub_oracle_script_hash,
      hub_ref_input_index,
    )
  expect hub_datum: Datum = hub_datum_data
  hub_datum
}

test get_authenticated_input_accepts_named_singleton() {
  let hub_oracle_script_hash =
    #"281ff0b5cc5c2c142dbf01f6fa799f6cc077237e6fdb3e4055047c1e"
  let hub_oracle_asset_name = #"487562204f7261636c65"
  let hub_input =
    Input {
      output_reference: OutputReference {
        transaction_id: #"1111111111111111111111111111111111111111111111111111111111111111",
        output_index: 0,
      },
      output: Output {
        address: Address {
          payment_credential: Script(hub_oracle_script_hash),
          stake_credential: None,
        },
        value: assets.from_lovelace(5_000_000)
          |> assets.add(hub_oracle_script_hash, hub_oracle_asset_name, 1),
        datum: NoDatum,
        reference_script: None,
      },
    }

  get_authenticated_input([hub_input], hub_oracle_script_hash, 0) == hub_input
}
