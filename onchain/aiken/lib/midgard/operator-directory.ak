use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, from_verification_key}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction,
}
use env
use midgard/common/types.{Empty}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/settlement
use midgard/state_queue

pub type SlashingReason {
  SlashOperatorForBadState { state_queue_redeemer_index: Int }
  SlashOperatorForBadSettlement {
    settlement_input_index: Int,
    settlement_redeemer_index: Int,
    bounty_claimant: VerificationKeyHash,
  }
}

/// The operator directory whose minting policy is slashing.
pub type SlashingDirectory {
  ActiveOperatorsDirectory
  RetiredOperatorsDirectory
}

/// Whether the state queue's slashing approach agrees with the directory that
/// slashes for bad state: an active-operators slash must be
/// `SlashActiveOperator`, a retired-operators slash `SlashRetiredOperator`.
/// The approaches that slash nothing never authorize a directory slash.
pub fn bad_state_slashing_approach_matches_directory(
  slashing_approach: state_queue.SlashingApproach,
  slashing_directory: SlashingDirectory,
) -> Bool {
  when slashing_approach is {
    state_queue.SlashActiveOperator { .. } ->
      slashing_directory == ActiveOperatorsDirectory
    state_queue.SlashRetiredOperator { .. } ->
      slashing_directory == RetiredOperatorsDirectory
    _ -> False
  }
}

pub type SlashingArguments {
  slashed_operator: VerificationKeyHash,
  hub_oracle_ref_input_index: Int,
  slashed_operator_anchor_element_input_outref: OutputReference,
  slashed_operator_anchor_element_output_index: Int,
  slashing_reason: SlashingReason,
}

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
    root_validator: fn(_, _, root_data: Data) -> Bool {
      root_data == env.empty_data
    },
  )
    |> linked_list.run_root_with(directory_policy_id, root_asset_name)
}

pub fn deinit(
  hub_oracle_script_hash: PolicyId,
  directory_policy_id: PolicyId,
  root_asset_name: AssetName,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, mint, .. } = tx

  // 1. Hub oracle NFT must be getting burnt.
  expect assets.quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == -1

  // 2. Linked list de-initialization must be valid. No other validations
  //    required.
  linked_list.deinit(
    inputs: inputs,
    tx_mint: mint,
    root_validator: fn(_, _, _) -> Bool { True },
  )
    |> linked_list.run_root_with(directory_policy_id, root_asset_name)
}

/// Shows that the given operator key is between the key and link of the
/// specified linked list element.
pub fn operator_is_not_a_member(
  operator: VerificationKeyHash,
  reference_inputs: List<Input>,
  element_ref_input_index: Int,
) -> linked_list.ElementEval<Bool> {
  expect Some(element_input) = reference_inputs
    |> list.at(element_ref_input_index)
  let
      _element_address,
      _element_lovelace,
      m_smaller_key,
      _element_data,
      larger_link
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

pub fn transferred_operator_bond_tranche_is_exact_v1(
  node_lovelace: Int,
  penalize_for_inactivity: Bool,
) -> Bool {
  node_lovelace == if penalize_for_inactivity {
    env.required_bond - env.inactivity_slashing_penalty
  } else {
    env.required_bond
  }
}

/// The fee of a slash that consumes an operator node holding `node_lovelace`
/// (F04 §2.3). A node is created holding exactly `env.required_bond`, or that
/// less `env.inactivity_slashing_penalty` after a forced retirement, and keeps
/// any lovelace later added to it, so any node holding at least the smaller
/// tranche is slashable. Every profile sets `env.required_bond` to
/// `env.slashing_penalty + env.fraud_prover_reward`, so the fee is what the
/// node holds beyond the reward, capped at `env.slashing_penalty`:
///
/// - `env.slashing_penalty` for a node holding the full bond or more;
/// - `env.slashing_penalty - env.inactivity_slashing_penalty` for a node
///   holding exactly the smaller tranche, raised by any excess up to
///   `env.slashing_penalty`.
///
/// The reward stays exactly `env.fraud_prover_reward` and any excess beyond
/// fee and reward is unconstrained change. The cap keeps the collateral a
/// slasher posts, which grows with the fee, independent of the node.
pub fn fraudulent_operator_slash_economics_are_exact_v1(
  node_lovelace: Int,
  fee: Int,
) -> Bool {
  let beyond_reward = node_lovelace - env.fraud_prover_reward
  if beyond_reward < env.slashing_penalty {
    and {
      beyond_reward >= env.slashing_penalty - env.inactivity_slashing_penalty,
      fee == beyond_reward,
    }
  } else {
    fee == env.slashing_penalty
  }
}

/// The address a bounty for `claimant` is paid to: the enterprise address
/// whose payment credential is the claimant key (F04 §2.3). This is the only
/// place the bounty address rule is stated.
pub fn bounty_address_v1(claimant: VerificationKeyHash) -> Address {
  from_verification_key(claimant)
}

/// The value and attachments of a bounty output: exactly `expected_reward`
/// lovelace and nothing else, with no datum and no reference script.
fn bounty_payload_is_exact(output: Output, expected_reward: Lovelace) -> Bool {
  let Output { value, datum, reference_script, .. } = output
  and {
    (assets.lovelace_of(value) == expected_reward)?,
    (assets.without_lovelace(value) == assets.zero)?,
    (datum == NoDatum)?,
    (reference_script == None)?,
  }
}

/// Whether `output` is a bounty payment of exactly `expected_reward` to
/// `payee`.
pub fn bounty_output_is_exact_v1(
  output: Output,
  payee: Address,
  expected_reward: Lovelace,
) -> Bool {
  and {
    output.address == payee,
    bounty_payload_is_exact(output, expected_reward),
  }
}

/// Exact bounty routing for a slash that consumes an operator's bond (F04
/// §2.3): exactly one output pays `payee`, and that
/// output is exactly `expected_reward` lovelace, ADA only, with no datum and
/// no reference script. The slashed node pays this output and the fee that
/// `slash_fraudulent_operator_and_get_info` (or the duplicate slash) pins,
/// given that the slash is the only one in its transaction
/// (`other_directory_is_untouched_v1`), so one lovelace either way is taken
/// from the treasury's share of the bond or from the payee's.
///
/// No output to the payee is accepted only while `expected_reward` is zero.
/// Every release profile compiles a nonzero reward.
pub fn bounty_routing_is_exact_v1(
  outputs: List<Output>,
  payee: Address,
  expected_reward: Lovelace,
) -> Bool {
  when list.filter(outputs, fn(output) { output.address == payee }) is {
    [] -> (expected_reward == 0)?
    [reward_output, ..other_claimant_outputs] -> and {
        list.is_empty(other_claimant_outputs)?,
        bounty_payload_is_exact(reward_output, expected_reward),
      }
  }
}

/// Allow one operator slash per transaction. A slash that consumes an
/// operator's bond pins the whole transaction fee to its own penalty and pays
/// one bounty, so it must be the only bond-consuming action of the operator
/// directories in its transaction. A slash handler calls this for each other
/// directory policy it holds as a script parameter, and none of them may mint
/// or burn:
///
/// - an active-operators slash: the registered- and retired-operators
///   policies;
/// - a duplicate-registration slash: the retired-operators policy.
///
/// A retired-operators slash needs no call of its own: next to an
/// active-operators slash the active handler refuses, and next to a
/// duplicate-registration slash the registered handler refuses. Two slashes
/// under one directory cannot share a transaction, because a policy runs once
/// with one redeemer and a removal burns exactly one node.
pub fn other_directory_is_untouched_v1(
  mint: Value,
  other_directory_policy: PolicyId,
) -> Bool {
  !pairs.has_key(assets.to_pairs(mint), other_directory_policy)
}

/// Helper for validating transfer of operators from one set to another.
/// Currently this only consists of:
/// - From registered to active set
/// - From active to retired set
///
/// The continuation receives the data of the new node at destination/target
/// list, along with the anchor key and inserted node link from the validated
/// insertion.
pub fn validate_transferred_operator_insertion(
  operator: VerificationKeyHash,
  origin_list_policy_id: PolicyId,
  origin_list_redeemer_index: Int,
  origin_list_redeemer_to_operator_and_penalty_flag: fn(Data) ->
    (VerificationKeyHash, Bool),
  target_list_anchor_element_output_index: Int,
  target_list_inserted_node_output_index: Int,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: Value,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  validate_target_insertion: fn(
    Data,
    Option<linked_list.NodeKey>,
    linked_list.Link,
  ) ->
    Bool,
) -> linked_list.ElementEval<Bool> {
  // 1. The minting redeemer of the origin operators list must be invoked.
  let origin_list_redeemer_data = redeemers
    |> utils.get_redeemer_at(
        expected_purpose: Mint(origin_list_policy_id),
        redeemer_index: origin_list_redeemer_index,
      )

  let (operator_from_origin_list_redeemer, must_penalize) = origin_list_redeemer_to_operator_and_penalty_flag(
    origin_list_redeemer_data,
  )

  // 2. The operator from target list's redeemer must agree with the one
  //    extracted from the origin list's redeemer.
  expect operator == operator_from_origin_list_redeemer

  // 3. Specified outputs related to the linked list must exist.
  expect Some(target_list_anchor_element_output) = outputs
    |> list.at(target_list_anchor_element_output_index)
  expect Some(target_list_inserted_node) = outputs
    |> list.at(target_list_inserted_node_output_index)

  // 4. Ordered insertion in the linked list must be proper.
  let
      _anchor_input,
      anchor_lovelace_change,
      m_anchor_key,
      _anchor_data,
      inserted_node_lovelace,
      inserted_node_key,
      inserted_node_data,
      inserted_node_link
    <- linked_list.insert_ascending(
    continued_anchor_element_output: target_list_anchor_element_output,
    new_node_output: target_list_inserted_node,
    inputs: inputs,
    tx_mint: mint,
  )

  // 5. ADA of anchor element must be preserved.
  expect anchor_lovelace_change >= 0

  expect
    transferred_operator_bond_tranche_is_exact_v1(
      inserted_node_lovelace,
      must_penalize,
    )

  // 7. The newly inserted node must carry the same operator as the one
  //    provided via the redeemer as its key.
  expect inserted_node_key == operator

  // 8. Inserted node's data and insertion context must pass the custom
  //    validation.
  expect
    validate_target_insertion(
      inserted_node_data,
      m_anchor_key,
      inserted_node_link,
    )

  // Done.
  True
}

/// Validations for slashing a fraudulent operator from either the active or
/// retired operators set. Verifies:
/// - Proper penalties are paid through transaction fees.
/// - Removal of the list node.
/// - Correctness of the operator PKH against the removed node key.
/// - Depending on slashing reason, cross validates with state queue or
///   settlement contract. A bad-settlement slash also requires the bounty
///   claimant's signature and pays it exactly the fraud-prover reward.
///
/// Returns various info extracted from the slashing arguments, list node
/// removal, and script context (`tx`).
pub fn slash_fraudulent_operator_and_get_info(
  hub_oracle_script_hash: PolicyId,
  slashing_directory: SlashingDirectory,
  slashing_arguments: SlashingArguments,
  tx: Transaction,
  further_validations: fn(
    VerificationKeyHash,
    Option<linked_list.NodeKey>,
    linked_list.Link,
    hub.Datum,
    List<Input>,
    List<Input>,
    Pairs<ScriptPurpose, Redeemer>,
  ) ->
    Bool,
) -> linked_list.ElementEval<Bool> {
  let
      SlashingArguments {
        slashed_operator,
        hub_oracle_ref_input_index,
        slashed_operator_anchor_element_input_outref,
        slashed_operator_anchor_element_output_index,
        slashing_reason,
      }
    = slashing_arguments

  let
      Transaction {
        inputs,
        outputs,
        reference_inputs,
        mint,
        fee,
        redeemers,
        extra_signatories,
        ..
      }
    = tx

  // 1. Transaction must refer to the authentic hub oracle UTxO to allow
  //    extraction of relevant addresses or policy IDs.
  let hub_datum = hub.get_datum(
    reference_inputs,
    hub_oracle_script_hash,
    hub_oracle_ref_input_index,
  )

  // 2. Specified output for removal of the slashed operator's node from the
  //    linked list must exist at its index.
  expect Some(slashed_operator_anchor_element_output) = outputs
    |> list.at(slashed_operator_anchor_element_output_index)

  // 3. Linked list's removal logic must be valid for the specified inputs and
  //    output.
  let
      _anchor_input,
      anchor_lovelace_change,
      m_anchor_key,
      _anchor_data,
      _slashed_operator_node_input,
      slashed_operator_node_lovelace,
      slashed_operator_node_key,
      _slashed_operator_node_data,
      slashed_operator_node_link
    <- linked_list.remove(
    anchor_input_outref: slashed_operator_anchor_element_input_outref,
    continued_anchor_element_output: slashed_operator_anchor_element_output,
    inputs: inputs,
    tx_mint: mint,
  )

  // 4. ADA of anchor element must be preserved.
  expect anchor_lovelace_change >= 0

  // 5. The slashed operator key indicated in the redeemer must match the key
  //    of the removed node.
  expect slashed_operator_node_key == slashed_operator

  // 6. The transaction fee is the pinned destination of the penalty (F04
  //    §2.3: the penalty is paid to the Cardano treasury as fee): what the
  //    node holds beyond the reward, capped at the penalty, with anything
  //    more left as change. The complementary half, an output paying exactly
  //    `env.fraud_prover_reward` to the bounty claimant, is checked per
  //    slashing reason in step 8: by the state queue's
  //    `RemoveFraudulentBlockHeader` handler for a bad state, and here for a
  //    bad settlement. Fee and bounty belong to this slash alone because a
  //    transaction carries one operator slash (see
  //    `other_directory_is_untouched_v1`).
  expect
    fraudulent_operator_slash_economics_are_exact_v1(
      slashed_operator_node_lovelace,
      fee,
    )

  // 7. Given custom validations must pass.
  expect
    further_validations(
      slashed_operator,
      m_anchor_key,
      slashed_operator_node_link,
      hub_datum,
      inputs,
      reference_inputs,
      redeemers,
    )

  when slashing_reason is {
    SlashOperatorForBadState { state_queue_redeemer_index } -> {
      // 8a. State queue minting script must be invoked via the
      //     `RemoveFraudulentBlockHeader` redeemer, and the operator specified
      //     there must agree with the `slashed_operator` redeemer argument
      //     here. Its slashing approach must be the slash arm for this
      //     directory, so a removal that slashes nothing cannot be paired
      //     with a directory slash.
      expect
          state_queue.RemoveFraudulentBlockHeader {
            fraudulent_operator,
            slashing_approach,
            ..
          }
        = redeemers
        |> utils.get_redeemer_at(
            expected_purpose: Mint(hub_datum.state_queue),
            redeemer_index: state_queue_redeemer_index,
          )
      expect fraudulent_operator == slashed_operator
      expect
        bad_state_slashing_approach_matches_directory(
          slashing_approach,
          slashing_directory,
        )

      // Done.
      True
    }
    SlashOperatorForBadSettlement {
      settlement_input_index,
      settlement_redeemer_index,
      bounty_claimant,
    } -> {
      // 8b. A settlement UTxO must be getting spent via the
      //     `DisproveResolutionClaim` redeemer.
      expect settlement.DisproveResolutionClaim { operator, .. } = utils.get_spending_redeemer_data_at(
        hub_datum.settlement_addr,
        settlement_input_index,
        settlement_redeemer_index,
        inputs,
        redeemers,
      )
      expect operator == slashed_operator

      // 8b-i. The bounty claimant named in the slashing reason must sign: no
      //       earlier step of a settlement disproof authenticates it.
      expect bounty_claimant |> utils.has_signed(extra_signatories)

      // 8b-ii. Exactly `env.fraud_prover_reward` must reach the claimant.
      bounty_routing_is_exact_v1(
        outputs,
        bounty_address_v1(bounty_claimant),
        env.fraud_prover_reward,
      )
    }
  }
}

pub fn cross_validate_slashing_reason(
  fraudulent_operator: VerificationKeyHash,
  target_operators_set_policy_id: PolicyId,
  target_operators_set_redeemer_index: Int,
  redeemer_to_slashing_arguments: fn(Redeemer) -> SlashingArguments,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> SlashingReason {
  let target_operators_set_redeemer_data = utils.get_redeemer_at(
    redeemers: redeemers,
    expected_purpose: Mint(target_operators_set_policy_id),
    redeemer_index: target_operators_set_redeemer_index,
  )
  let SlashingArguments { slashed_operator, slashing_reason, .. } = redeemer_to_slashing_arguments(
    target_operators_set_redeemer_data,
  )
  expect fraudulent_operator == slashed_operator
  slashing_reason
}

test q53_operator_bond_tranches_are_exact() {
  let full = env.required_bond
  let partial = env.required_bond - env.inactivity_slashing_penalty
  and {
    transferred_operator_bond_tranche_is_exact_v1(full, False),
    transferred_operator_bond_tranche_is_exact_v1(partial, True),
    !transferred_operator_bond_tranche_is_exact_v1(full - 1, False),
    !transferred_operator_bond_tranche_is_exact_v1(full + 1, False),
    !transferred_operator_bond_tranche_is_exact_v1(partial - 1, True),
    !transferred_operator_bond_tranche_is_exact_v1(partial + 1, True),
    !transferred_operator_bond_tranche_is_exact_v1(
      env.required_bond - env.inactivity_slashing_penalty - 2,
      True,
    ),
  }
}

test q53_fraud_slash_fee_is_the_held_tranche_penalty_up_to_the_full_penalty() {
  let full = env.required_bond
  let partial = env.required_bond - env.inactivity_slashing_penalty
  let full_fee = env.slashing_penalty
  let partial_fee = env.slashing_penalty - env.inactivity_slashing_penalty
  let slash_fee_is = fraudulent_operator_slash_economics_are_exact_v1
  and {
    // Every profile splits the bond into the penalty and the reward.
    env.required_bond == env.slashing_penalty + env.fraud_prover_reward,
    // The two tranches a node is created with.
    slash_fee_is(full, full_fee),
    slash_fee_is(partial, partial_fee),
    !slash_fee_is(full, full_fee - 1),
    !slash_fee_is(full, full_fee + 1),
    !slash_fee_is(partial, partial_fee - 1),
    !slash_fee_is(partial, partial_fee + 1),
    // Excess raises the fee up to the full penalty.
    slash_fee_is(partial + 1, partial_fee + 1),
    !slash_fee_is(partial + 1, partial_fee),
    !slash_fee_is(partial + 1, partial_fee + 2),
    slash_fee_is(full + 1, full_fee),
    !slash_fee_is(full + 1, full_fee + 1),
    !slash_fee_is(full + 1, full_fee - 1),
    slash_fee_is(full + full, full_fee),
    !slash_fee_is(full + full, full + full - env.fraud_prover_reward),
    // No node below the inactivity tranche is slashable.
    !slash_fee_is(partial - 1, partial_fee - 1),
    !slash_fee_is(partial - 1, partial_fee),
    !slash_fee_is(0, 0),
  }
}

test other_directory_is_untouched_only_without_its_mint_or_burn() {
  let own_policy = #"01"
  let other_policy = #"02"
  let own_burn = assets.from_asset(own_policy, "node", -1)
  and {
    other_directory_is_untouched_v1(own_burn, other_policy),
    !other_directory_is_untouched_v1(
      own_burn |> assets.add(other_policy, "node", -1),
      other_policy,
    ),
    !other_directory_is_untouched_v1(
      own_burn |> assets.add(other_policy, "node", 1),
      other_policy,
    ),
  }
}
