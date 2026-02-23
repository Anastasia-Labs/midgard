use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  Input, Mint, Output, Redeemer, ScriptPurpose, Transaction,
}
use midgard/common/constants.{empty_data}
use midgard/common/types.{Empty}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/protocol_parameters.{required_bond, slashing_penalty}
use midgard/settlement
use midgard/state_queue

pub type Datum<node_data> =
  linked_list.Element<Empty, node_data>

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
    nonce_validated: assets.quantity_of(
      mint,
      hub_oracle_script_hash,
      hub.asset_name,
    ) == 1,
    produced_element_output: root_output,
    tx_mint: mint,
    root_validator: fn(_, root_data: Data) -> Bool { root_data == empty_data },
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
  let Transaction { inputs, mint, .. } = tx

  // 1. An input must exist at specified index.
  expect Some(own_input) = inputs |> list.at(input_index)

  // 2. Hub oracle NFT must be getting burnt.
  expect assets.quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == -1

  // 3. Linked list de-initialization must be valid. No other validations
  //    required.
  linked_list.deinit(
    root_input: own_input,
    tx_mint: mint,
    root_validator: fn(_, _) -> Bool { True },
  )
    |> linked_list.run_root_with(directory_policy_id, root_asset_name)
}

/// Shows that the given operator key is between the key and link of the
/// specified linked list element.
pub fn operator_is_not_a_member(
  operator: VerificationKeyHash,
  reference_inputs: List<Input>,
  element_ref_input_index: Int,
) -> linked_list.Eval {
  expect Some(element_input) =
    reference_inputs |> list.at(element_ref_input_index)
  let
    _element_lovelace,
    m_smaller_key,
    _element_data,
    larger_link,
  <- linked_list.get_element_info(element_input.output)
  and {
    when m_smaller_key is {
      Some(smaller_key) -> bytearray.compare(smaller_key, operator) == Less
      None -> True
    },
    when larger_link is {
      Some(larger_key) -> bytearray.compare(operator, larger_key) == Less
      None -> True
    },
  }
}

/// Helper for validating transfer of operators from one set to another.
/// Currently this only consists of:
/// - From registered to active set
/// - From active to retired set
///
/// The return value is the data of the new node at destination/target list.
pub fn validate_operator_transfer_and_get_inserted_data(
  operator: VerificationKeyHash,
  origin_list_policy_id: PolicyId,
  origin_list_redeemer_index: Int,
  origin_list_redeemer_to_operator: fn(Data) -> VerificationKeyHash,
  target_list_anchor_element_input_index: Int,
  target_list_anchor_element_output_index: Int,
  target_list_inserted_node_output_index: Int,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: Value,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  validate_inserted_node_data: fn(Data) -> Bool,
) -> linked_list.Eval {
  // 1. The minting redeemer of the origin operators list must be invoked.
  let origin_list_redeemer_data =
    redeemers
      |> utils.get_redeemer_at(
          expected_purpose: Mint(origin_list_policy_id),
          redeemer_index: origin_list_redeemer_index,
        )

  let operator_from_origin_list_redeemer =
    origin_list_redeemer_to_operator(origin_list_redeemer_data)

  // 2. The operator from target list's redeemer must agree with the one
  //    extracted from the origin list's redeemer.
  expect operator == operator_from_origin_list_redeemer

  // 3. Specified input and outpus related to the linked list must exist.
  expect Some(target_list_anchor_element_input) =
    inputs |> list.at(target_list_anchor_element_input_index)
  expect Some(target_list_anchor_element_output) =
    outputs |> list.at(target_list_anchor_element_output_index)
  expect Some(target_list_inserted_node) =
    outputs |> list.at(target_list_inserted_node_output_index)

  // 4. Ordered insertion in the linked list must be proper.
  let
    anchor_lovelace_change,
    _m_anchor_key,
    _anchor_data,
    inserted_node_lovelace,
    inserted_node_key,
    inserted_node_data,
    _inserted_node_link,
  <-
    linked_list.insert_ascending(
      target_list_anchor_element_input,
      target_list_anchor_element_output,
      target_list_inserted_node,
      mint,
    )

  // 5. ADA of anchor element must be preserved.
  expect anchor_lovelace_change >= 0

  // 6. ADA bond must be transferred from the origin operator node.
  expect inserted_node_lovelace == required_bond

  // 7. The newly inserted node must carry the same operator as the one
  //    provided via the redeemer as its key.
  expect inserted_node_key == operator

  // 8. Inserted node's data must pass the custom validation.
  expect validate_inserted_node_data(inserted_node_data)

  // Done.
  True
}

/// Common requirements for slashing an operator from either the active or
/// retired operators set. Fraud can be either because of a bad block
/// commitment, or an invalid resolution claim on a settlement UTxO. Which is
/// why this function returs the hub oracle datum for endpoints to lookup either
/// the state queue or the settlement UTxO.
fn slash_operator_and_get_hub_datum(
  hub_oracle_script_hash: PolicyId,
  slashed_operator_key: VerificationKeyHash,
  hub_oracle_ref_input_index: Int,
  slashed_operator_anchor_element_input_index: Int,
  slashed_operator_node_input_index: Int,
  slashed_operator_anchor_element_output_index: Int,
  inputs: List<Input>,
  outputs: List<Output>,
  reference_inputs: List<Input>,
  mint: Value,
  fee: Int,
  return: fn(hub.Datum) -> Bool,
) -> linked_list.Eval {
  // 1. Transaction must refer to the authentic hub oracle UTxO to allow
  //    extraction of relevant addresses or policy IDs.
  let hub_datum =
    hub.get_datum(
      reference_inputs,
      hub_oracle_script_hash,
      hub_oracle_ref_input_index,
    )

  // 2. Specified inputs and output for removal of the slashed operator's node
  //    from the linked list must exist at their indices.
  expect Some(slashed_operator_anchor_element_input) =
    inputs |> list.at(slashed_operator_anchor_element_input_index)
  expect Some(slashed_operator_node_input) =
    inputs |> list.at(slashed_operator_node_input_index)
  expect Some(slashed_operator_anchor_element_output) =
    outputs |> list.at(slashed_operator_anchor_element_output_index)

  // 3. Linked list's removal logic must be valid for the specified inputs and
  //    output.
  let
    anchor_lovelace_change,
    _m_anchor_key,
    _anchor_data,
    _slashed_operator_node_lovelace,
    slashed_operator_node_key,
    _slashed_operator_node_data,
    _slashed_operator_node_link,
  <-
    linked_list.remove(
      slashed_operator_anchor_element_input,
      slashed_operator_node_input,
      slashed_operator_anchor_element_output,
      mint,
    )

  // 4. ADA of anchor element must be preserved.
  expect anchor_lovelace_change >= 0

  // 5. Key of the removing node must be that of the fraudulent operator's
  //    specified via the redeemer.
  expect slashed_operator_node_key == slashed_operator_key

  // 6. Slashing penalty must be paid to the Cardano treasury via the
  //    transaction fee. Fraud prover is assumed to be the signer of the
  //    transaction, and therefore will receive the remaining bond.
  expect fee >= slashing_penalty

  // Done.
  return(hub_datum)
}

pub fn remove_operator_for_bad_state(
  hub_oracle_script_hash: PolicyId,
  slashed_operator_key: VerificationKeyHash,
  hub_oracle_ref_input_index: Int,
  operator_anchor_element_input_index: Int,
  operator_slashed_node_input_index: Int,
  operator_anchor_element_output_index: Int,
  state_queue_redeemer_index: Int,
  tx: Transaction,
) -> linked_list.Eval {
  let Transaction {
    inputs,
    outputs,
    reference_inputs,
    mint,
    redeemers,
    fee,
    ..
  } = tx

  let hub_datum <-
    slash_operator_and_get_hub_datum(
      hub_oracle_script_hash,
      slashed_operator_key,
      hub_oracle_ref_input_index,
      operator_anchor_element_input_index,
      operator_slashed_node_input_index,
      operator_anchor_element_output_index,
      inputs,
      outputs,
      reference_inputs,
      mint,
      fee,
    )

  // 2. State queue minting script must be invoked via the 
  //    `RemoveFraudulentBlockHeader` redeemer, and the operator
  //    specified there must agree with the
  //    `slashed_retired_operator_key` redeemer argument here.
  expect state_queue.RemoveFraudulentBlockHeader { fraudulent_operator, .. } =
    redeemers
      |> utils.get_redeemer_at(
          expected_purpose: Mint(hub_datum.state_queue),
          redeemer_index: state_queue_redeemer_index,
        )
  expect fraudulent_operator == slashed_operator_key

  // Done.
  True
}

pub fn remove_operator_for_bad_settlement(
  hub_oracle_script_hash: PolicyId,
  slashed_operator_key: VerificationKeyHash,
  hub_oracle_ref_input_index: Int,
  operator_anchor_element_input_index: Int,
  operator_slashed_node_input_index: Int,
  operator_anchor_element_output_index: Int,
  settlement_input_index: Int,
  settlement_redeemer_index: Int,
  tx: Transaction,
) -> linked_list.Eval {
  let Transaction {
    inputs,
    outputs,
    reference_inputs,
    mint,
    redeemers,
    fee,
    ..
  } = tx

  let hub_datum <-
    slash_operator_and_get_hub_datum(
      hub_oracle_script_hash,
      slashed_operator_key,
      hub_oracle_ref_input_index,
      operator_anchor_element_input_index,
      operator_slashed_node_input_index,
      operator_anchor_element_output_index,
      inputs,
      outputs,
      reference_inputs,
      mint,
      fee,
    )

  // 2. A settlement UTxO must be getting spent via the
  //    `DisproveResolutionClaim` redeemer.
  //
  // TODO: Potential optimization opportunities, i.e. using builtin functions.
  // This requires implicitly imposing the position of `operator` field in the
  // redeemer to be at index 0.
  expect settlement.DisproveResolutionClaim { operator, .. } =
    utils.get_spending_redeemer_data_at(
      hub_datum.settlement_addr,
      settlement_input_index,
      settlement_redeemer_index,
      inputs,
      redeemers,
    )
  expect operator == slashed_operator_key

  // Done.
  True
}
