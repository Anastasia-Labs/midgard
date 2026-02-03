use aiken/collection/list
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Transaction}
use midgard/hub_oracle as hub

pub fn init(
  hub_oracle_script_hash: PolicyId,
  directory_policy_id: PolicyId,
  root_asset_name: AssetName,
  output_index: Int,
  tx: Transaction,
) -> Bool {
  let Transaction { outputs, mint, .. } = tx

  // 1. An output must exist at specified index.
  expect Some(root_output) = outputs |> list.at(output_index)

  // 2. Linked list must be initiated with a void root data, and
  //    validation that a hub oracle NFT is getting minted.
  linked_list.init(
    nonce_validated: quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == 1,
    produced_element_output: root_output,
    tx_mint: mint,
    root_data_validator: fn(root_data: Data) -> Bool { root_data == Void },
  )
    |> linked_list.run_root_with(directory_policy_id, root_asset_name)
}

pub fn deinit(
  hub_oracle_script_hash: PolicyId,
  directory_policy_id: PolicyId,
  root_asset_name: AssetName,
  input_index: Int,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, mint, .. } = self

  // 1. An input must exist at specified index.
  expect Some(own_input) = inputs |> list.at(input_index)

  // 2. Hub oracle NFT must be getting burnt.
  expect quantity_of(mint, hub_oracle_script_hash, hub.asset_name)

  // 3. Linked list de-initialization must be valid. No other validations
  //    required.
  linked_list.deinit(
    root_input: own_input,
    tx_mint: mint,
    root_data_validator: fn(_) -> Bool { True },
  )
    |> linked_list.run_root_with(directory_policy_id, root_asset_name)
}

pub fn operator_is_not_a_member(
  operator: VerificationKeyHash,
  reference_inputs: List<Input>,
  element_ref_input_index: Int,
) -> linked_list.Eval {
  expect Some(element_input) =
    reference_inputs |> list.at(element_ref_input_index)
  let
    m_smaller_key,
    _element_data,
    larger_link,
  <- linked_list.get_element_info(element_input.output)
  when m_smaller_key is {
    Some(smaller_key) -> and {
        bytearray.compare(smaller_key, operator) == Less,
        bytearray.compare(operator, larger_link) == Less,
      }
    None -> bytearray.compare(operator, larger_link) == Less
  }
}
