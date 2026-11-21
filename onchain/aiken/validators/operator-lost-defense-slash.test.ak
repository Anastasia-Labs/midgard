//// The slash of an operator that lost a defense of a validation case
//// (`SlashOperatorForLostDefense`), and the burn of its lost-defense receipt,
//// driven through the active- and retired-operators minting handlers and the
//// award validator, which holds the receipts and is their policy.
////
//// A slash consumes the operator's node, burns the receipt and pays the
//// fraud-prover reward to the receipt's bounty claimant, or to the treasury
//// when neither claim of the thread was valid. A later loss of an operator
//// already slashed burns its receipt with no slash, once the operator is in
//// neither set.

use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, from_verification_key}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use env
use fraud_proofs/validation_trace/award_v1 as award_validator
use midgard/computation_thread as ct
use midgard/hub_oracle as hub
use midgard/operator_directory.{
  ActiveOperatorsDirectory, RetiredOperatorsDirectory, SlashingDirectory,
}
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/validation_award_v1.{
  BurnReceipt, CloseAwardedThread, DefenderAlreadySlashed,
  ReceiptBurnPermissionV1, SlashedForThisLoss, SpendReceipt,
}
use midgard/validation_case_v1.{LostDefenseReceiptV1}
use midgard/validation_dispute_v1.{
  ChallengerWins, DisputeWinner, NeitherClaimValid,
}
use midgard/validation_resolution_v1
use operator_directory/active_operators as active_operators_validator
use operator_directory/retired_operators as retired_operators_validator

const hub_oracle_hash: ScriptHash =
  #"a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0"

const active_operators_policy: PolicyId =
  #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2"

const retired_operators_policy: PolicyId =
  #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3"

const scheduler_policy: PolicyId =
  #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4"

const registered_operators_policy: PolicyId =
  #"a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9"

const misc_policy: PolicyId =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// The award validator: the receipt policy and the receipts' address.
const award_hash: PolicyId =
  #"acacacacacacacacacacacacacacacacacacacacacacacacacacacac"

/// The thread expiry script.
const expiry_hash: ScriptHash =
  #"aeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeae"

/// The case validator: the thread-token policy.
const case_hash: PolicyId =
  #"adadadadadadadadadadadadadadadadadadadadadadadadadadadad"

const operator_vkey: VerificationKeyHash =
  #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

const claimant_vkey: VerificationKeyHash =
  #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

const other_vkey: VerificationKeyHash =
  #"b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3"

const case_id: ByteArray =
  #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const other_case_id: ByteArray =
  #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const element_lovelace: Lovelace = 2_000_000

// ## Fixtures

fn outref(seed: ByteArray, index: Int) -> OutputReference {
  OutputReference { transaction_id: seed, output_index: index }
}

fn receipt_outref() -> OutputReference {
  outref(#"1111111111111111111111111111111111111111111111111111111111111111", 0)
}

fn directory_root_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 0)
}

fn slashed_node_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 1)
}

fn element_value(
  lovelace: Lovelace,
  policy: PolicyId,
  asset_name: AssetName,
) -> Value {
  assets.from_lovelace(lovelace) |> assets.add(policy, asset_name, 1)
}

fn hub_datum() -> hub.Datum {
  hub.Datum {
    registered_operators: registered_operators_policy,
    active_operators: active_operators_policy,
    retired_operators: retired_operators_policy,
    scheduler: scheduler_policy,
    state_queue: misc_policy,
    fraud_proof_catalogue: misc_policy,
    fraud_proof: misc_policy,
    deposit: misc_policy,
    withdrawal: misc_policy,
    tx_order: misc_policy,
    settlement: misc_policy,
    payout: misc_policy,
    registered_operators_addr: address.from_script(registered_operators_policy),
    active_operators_addr: address.from_script(active_operators_policy),
    retired_operators_addr: address.from_script(retired_operators_policy),
    scheduler_addr: address.from_script(scheduler_policy),
    state_queue_addr: address.from_script(misc_policy),
    fraud_proof_catalogue_addr: address.from_script(misc_policy),
    fraud_proof_addr: address.from_script(misc_policy),
    deposit_addr: address.from_script(misc_policy),
    withdrawal_addr: address.from_script(misc_policy),
    tx_order_addr: address.from_script(misc_policy),
    settlement_addr: address.from_script(misc_policy),
    reserve_addr: address.from_script(misc_policy),
    payout_addr: address.from_script(misc_policy),
    reserve_observer: misc_policy,
  }
}

fn hub_ref_input() -> Input {
  Input {
    output_reference: outref(
      #"3333333333333333333333333333333333333333333333333333333333333333",
      0,
    ),
    output: Output {
      address: address.from_script(hub_oracle_hash),
      value: element_value(element_lovelace, hub_oracle_hash, hub.asset_name),
      datum: InlineDatum(hub_datum()),
      reference_script: None,
    },
  }
}

fn scheduler_ref_input() -> Input {
  let datum: scheduler.SchedDatum = scheduler.NoActiveOperators
  Input {
    output_reference: outref(
      #"4444444444444444444444444444444444444444444444444444444444444444",
      0,
    ),
    output: Output {
      address: address.from_script(scheduler_policy),
      value: element_value(
        element_lovelace,
        scheduler_policy,
        scheduler.asset_name,
      ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

// ### The receipt

fn receipt_name() -> AssetName {
  validation_case_v1.receipt_token_name(case_id, operator_vkey)
}

fn receipt_of(outcome: DisputeWinner, defender: VerificationKeyHash) -> Data {
  let receipt: Data = LostDefenseReceiptV1 {
    case_id,
    defender_vkh: defender,
    outcome,
    bounty_claimant: claimant_vkey,
  }
  receipt
}

fn receipt_input_with(datum: Data, value: Value) -> Input {
  Input {
    output_reference: receipt_outref(),
    output: Output {
      address: address.from_script(award_hash),
      value,
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn receipt_input(outcome: DisputeWinner) -> Input {
  receipt_input_with(
    receipt_of(outcome, operator_vkey),
    element_value(env.fraud_proof_output_lovelace, award_hash, receipt_name()),
  )
}

fn receipt_burn() -> Value {
  assets.from_asset(award_hash, receipt_name(), -1)
}

fn reward_to(payee: Address) -> Output {
  Output {
    address: payee,
    value: assets.from_lovelace(env.fraud_prover_reward),
    datum: NoDatum,
    reference_script: None,
  }
}

fn payee_of(outcome: DisputeWinner) -> Address {
  if outcome == NeitherClaimValid {
    validation_case_v1.treasury_address
  } else {
    from_verification_key(claimant_vkey)
  }
}

// ### The slash

fn directory_policy(directory: SlashingDirectory) -> PolicyId {
  when directory is {
    ActiveOperatorsDirectory -> active_operators_policy
    RetiredOperatorsDirectory -> retired_operators_policy
  }
}

fn root_name(directory: SlashingDirectory) -> AssetName {
  when directory is {
    ActiveOperatorsDirectory -> active_operators.root_asset_name
    RetiredOperatorsDirectory -> retired_operators.root_asset_name
  }
}

fn node_name(directory: SlashingDirectory, key: ByteArray) -> AssetName {
  when directory is {
    ActiveOperatorsDirectory ->
      bytearray.concat(active_operators.node_asset_name_prefix, key)
    RetiredOperatorsDirectory ->
      bytearray.concat(retired_operators.node_asset_name_prefix, key)
  }
}

fn node_data(directory: SlashingDirectory) -> Data {
  when directory is {
    ActiveOperatorsDirectory -> {
      let data: Data = active_operators.NodeData {
        bond_unlock_time: None,
        inactivity_strikes: 0,
      }
      data
    }
    RetiredOperatorsDirectory -> {
      let data: Data = retired_operators.NodeData { bond_unlock_time: None }
      data
    }
  }
}

/// A directory root linked to `link`.
fn directory_root(
  directory: SlashingDirectory,
  link: Option<ByteArray>,
) -> Output {
  let datum: operator_directory.Datum<Data> = linked_list.Element {
    data: linked_list.Root(#""),
    link,
  }
  let policy = directory_policy(directory)
  Output {
    address: address.from_script(policy),
    value: element_value(element_lovelace, policy, root_name(directory)),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn slashing_arguments(
  receipt_index: Int,
) -> operator_directory.SlashingArguments {
  operator_directory.SlashingArguments {
    slashed_operator: operator_vkey,
    hub_oracle_ref_input_index: 0,
    slashed_operator_anchor_element_input_outref: directory_root_outref(),
    slashed_operator_anchor_element_output_index: 0,
    slashing_reason: operator_directory.SlashOperatorForLostDefense {
      receipt_index,
    },
  }
}

fn slash_redeemer(directory: SlashingDirectory, receipt_index: Int) -> Data {
  when directory is {
    ActiveOperatorsDirectory -> {
      let redeemer: Data = active_operators.SlashOperator {
        slashing_arguments: slashing_arguments(receipt_index),
        operator_removal_scheduler_sync: active_operators.ShowOperatorIsInactive {
          scheduler_ref_input_index: 1,
        },
      }
      redeemer
    }
    RetiredOperatorsDirectory -> {
      let redeemer: Data = retired_operators.SlashOperator {
        slashing_arguments: slashing_arguments(receipt_index),
      }
      redeemer
    }
  }
}

fn burn_for_slash(directory: SlashingDirectory) -> Data {
  let redeemer: Data = BurnReceipt {
    receipt_input_index: 0,
    hub_oracle_ref_input_index: 0,
    permission: SlashedForThisLoss { directory },
  }
  redeemer
}

fn spend_receipt() -> Data {
  let redeemer: Data = SpendReceipt
  redeemer
}

/// The honest lost-defense slash of the operator's node in `directory`, with
/// the receipt of `outcome` at input 0.
fn slash_tx(directory: SlashingDirectory, outcome: DisputeWinner) -> Transaction {
  let node_datum: operator_directory.Datum<Data> = linked_list.Element {
    data: linked_list.Node(node_data(directory)),
    link: None,
  }
  let policy = directory_policy(directory)
  Transaction {
    ..transaction.placeholder,
    inputs: [
      receipt_input(outcome),
      Input {
        output_reference: directory_root_outref(),
        output: directory_root(directory, Some(operator_vkey)),
      },
      Input {
        output_reference: slashed_node_outref(),
        output: Output {
          address: address.from_script(policy),
          value: element_value(
            env.required_bond,
            policy,
            node_name(directory, operator_vkey),
          ),
          datum: InlineDatum(node_datum),
          reference_script: None,
        },
      },
    ],
    reference_inputs: [hub_ref_input(), scheduler_ref_input()],
    outputs: [directory_root(directory, None), reward_to(payee_of(outcome))],
    fee: env.slashing_penalty,
    mint: assets.from_asset(policy, node_name(directory, operator_vkey), -1)
      |> assets.merge(receipt_burn()),
    redeemers: [
      Pair(Spend(receipt_outref()), spend_receipt()),
      Pair(Mint(policy), slash_redeemer(directory, 0)),
      Pair(Mint(award_hash), burn_for_slash(directory)),
    ],
  }
}

fn directory_slashes(
  directory: SlashingDirectory,
  redeemer: Data,
  tx: Transaction,
) -> Bool {
  when directory is {
    ActiveOperatorsDirectory -> {
      expect active: active_operators.MintRedeemer = redeemer
      active_operators_validator.mint.mint(
        hub_oracle_hash,
        registered_operators_policy,
        retired_operators_policy,
        award_hash,
        active,
        active_operators_policy,
        tx,
      )
    }
    RetiredOperatorsDirectory -> {
      expect retired: retired_operators.MintRedeemer = redeemer
      retired_operators_validator.mint.mint(
        hub_oracle_hash,
        award_hash,
        retired,
        retired_operators_policy,
        tx,
      )
    }
  }
}

fn award_burns(redeemer: Data, tx: Transaction) -> Bool {
  expect burn: validation_award_v1.ReceiptRedeemerV1 = redeemer
  award_validator.main.mint(
    case_hash,
    hub_oracle_hash,
    expiry_hash,
    burn,
    award_hash,
    tx,
  )
}

fn award_spends_receipt(tx: Transaction) -> Bool {
  award_validator.main.spend(
    case_hash,
    hub_oracle_hash,
    expiry_hash,
    Some(receipt_of(ChallengerWins, operator_vkey)),
    SpendReceipt,
    receipt_outref(),
    tx,
  )
}

/// Every script of a lost-defense slash: the directory, the receipt burn and
/// the receipt spend.
fn slashes(directory: SlashingDirectory, tx: Transaction) -> Bool {
  and {
    directory_slashes(directory, slash_redeemer(directory, 0), tx),
    award_burns(burn_for_slash(directory), tx),
    award_spends_receipt(tx),
  }
}

fn with_outputs(tx: Transaction, outputs: List<Output>) -> Transaction {
  Transaction { ..tx, outputs }
}

fn with_receipt(tx: Transaction, receipt: Input) -> Transaction {
  expect [_, ..rest] = tx.inputs
  Transaction { ..tx, inputs: [receipt, ..rest] }
}

// ## Both sets, both payees

test active_operator_is_slashed_for_a_lost_defense() {
  slashes(
    ActiveOperatorsDirectory,
    slash_tx(ActiveOperatorsDirectory, ChallengerWins),
  )
}

test retired_operator_is_slashed_for_a_lost_defense() {
  slashes(
    RetiredOperatorsDirectory,
    slash_tx(RetiredOperatorsDirectory, ChallengerWins),
  )
}

test slash_for_a_thread_with_neither_claim_valid_pays_the_treasury() {
  and {
    slashes(
      ActiveOperatorsDirectory,
      slash_tx(ActiveOperatorsDirectory, NeitherClaimValid),
    ),
    slashes(
      RetiredOperatorsDirectory,
      slash_tx(RetiredOperatorsDirectory, NeitherClaimValid),
    ),
  }
}

test slash_for_a_challenger_win_paying_the_treasury_is_refused() fail {
  let tx = slash_tx(ActiveOperatorsDirectory, ChallengerWins)
  directory_slashes(
    ActiveOperatorsDirectory,
    slash_redeemer(ActiveOperatorsDirectory, 0),
    with_outputs(
      tx,
      [
        directory_root(ActiveOperatorsDirectory, None),
        reward_to(validation_case_v1.treasury_address),
      ],
    ),
  )
}

test slash_for_neither_claim_valid_paying_the_claimant_is_refused() fail {
  let tx = slash_tx(RetiredOperatorsDirectory, NeitherClaimValid)
  directory_slashes(
    RetiredOperatorsDirectory,
    slash_redeemer(RetiredOperatorsDirectory, 0),
    with_outputs(
      tx,
      [
        directory_root(RetiredOperatorsDirectory, None),
        reward_to(from_verification_key(claimant_vkey)),
      ],
    ),
  )
}

test slash_paying_the_reward_short_is_refused() fail {
  let tx = slash_tx(ActiveOperatorsDirectory, ChallengerWins)
  let reward = reward_to(payee_of(ChallengerWins))
  directory_slashes(
    ActiveOperatorsDirectory,
    slash_redeemer(ActiveOperatorsDirectory, 0),
    with_outputs(
      tx,
      [
        directory_root(ActiveOperatorsDirectory, None),
        Output {
          ..reward,
          value: assets.from_lovelace(env.fraud_prover_reward - 1),
        },
      ],
    ),
  )
}

// ## The receipt is authentic, names the slashed operator, and is burned

test slash_with_a_receipt_of_another_operator_is_refused() fail {
  let other_name = validation_case_v1.receipt_token_name(case_id, other_vkey)
  let tx = with_receipt(
    slash_tx(ActiveOperatorsDirectory, ChallengerWins),
    receipt_input_with(
      receipt_of(ChallengerWins, other_vkey),
      element_value(env.fraud_proof_output_lovelace, award_hash, other_name),
    ),
  )
  directory_slashes(
    ActiveOperatorsDirectory,
    slash_redeemer(ActiveOperatorsDirectory, 0),
    Transaction {
      ..tx,
      mint: assets.from_asset(
        active_operators_policy,
        node_name(ActiveOperatorsDirectory, operator_vkey),
        -1,
      )
        |> assets.add(award_hash, other_name, -1),
    },
  )
}

test slash_with_a_receipt_without_its_token_is_refused() fail {
  directory_slashes(
    ActiveOperatorsDirectory,
    slash_redeemer(ActiveOperatorsDirectory, 0),
    with_receipt(
      slash_tx(ActiveOperatorsDirectory, ChallengerWins),
      receipt_input_with(
        receipt_of(ChallengerWins, operator_vkey),
        assets.from_lovelace(env.fraud_proof_output_lovelace),
      ),
    ),
  )
}

test slash_with_a_receipt_token_of_another_policy_is_refused() fail {
  directory_slashes(
    RetiredOperatorsDirectory,
    slash_redeemer(RetiredOperatorsDirectory, 0),
    with_receipt(
      slash_tx(RetiredOperatorsDirectory, ChallengerWins),
      receipt_input_with(
        receipt_of(ChallengerWins, operator_vkey),
        element_value(
          env.fraud_proof_output_lovelace,
          misc_policy,
          receipt_name(),
        ),
      ),
    ),
  )
}

test slash_with_a_receipt_token_of_another_case_is_refused() fail {
  directory_slashes(
    ActiveOperatorsDirectory,
    slash_redeemer(ActiveOperatorsDirectory, 0),
    with_receipt(
      slash_tx(ActiveOperatorsDirectory, ChallengerWins),
      receipt_input_with(
        receipt_of(ChallengerWins, operator_vkey),
        element_value(
          env.fraud_proof_output_lovelace,
          award_hash,
          validation_case_v1.receipt_token_name(other_case_id, operator_vkey),
        ),
      ),
    ),
  )
}

test slash_keeping_the_receipt_is_refused() fail {
  let tx = slash_tx(ActiveOperatorsDirectory, ChallengerWins)
  directory_slashes(
    ActiveOperatorsDirectory,
    slash_redeemer(ActiveOperatorsDirectory, 0),
    Transaction {
      ..tx,
      mint: assets.from_asset(
        active_operators_policy,
        node_name(ActiveOperatorsDirectory, operator_vkey),
        -1,
      ),
    },
  )
}

// ## The receipt burn

fn award_burns_for(permission: ReceiptBurnPermissionV1, tx: Transaction) -> Bool {
  let redeemer: Data = BurnReceipt {
    receipt_input_index: 0,
    hub_oracle_ref_input_index: 0,
    permission,
  }
  award_burns(redeemer, tx)
}

test receipt_burn_without_a_slash_is_refused() fail {
  let tx = slash_tx(ActiveOperatorsDirectory, ChallengerWins)
  award_burns_for(
    SlashedForThisLoss { directory: ActiveOperatorsDirectory },
    Transaction {
      ..tx,
      mint: receipt_burn(),
      redeemers: [
        Pair(Spend(receipt_outref()), spend_receipt()),
        Pair(Mint(award_hash), burn_for_slash(ActiveOperatorsDirectory)),
      ],
    },
  )
}

test receipt_burn_naming_the_other_directory_is_refused() fail {
  award_burns_for(
    SlashedForThisLoss { directory: RetiredOperatorsDirectory },
    slash_tx(ActiveOperatorsDirectory, ChallengerWins),
  )
}

test receipt_burn_beside_a_slash_for_another_reason_is_refused() fail {
  let tx = slash_tx(RetiredOperatorsDirectory, ChallengerWins)
  let other_reason: Data = retired_operators.SlashOperator {
    slashing_arguments: operator_directory.SlashingArguments {
      ..slashing_arguments(0),
      slashing_reason: operator_directory.SlashOperatorForBadState {
        state_queue_redeemer_index: 0,
      },
    },
  }
  award_burns_for(
    SlashedForThisLoss { directory: RetiredOperatorsDirectory },
    Transaction {
      ..tx,
      redeemers: [
        Pair(Spend(receipt_outref()), spend_receipt()),
        Pair(Mint(retired_operators_policy), other_reason),
        Pair(Mint(award_hash), burn_for_slash(RetiredOperatorsDirectory)),
      ],
    },
  )
}

test receipt_burn_beside_a_slash_naming_another_receipt_is_refused() fail {
  let tx = slash_tx(ActiveOperatorsDirectory, ChallengerWins)
  award_burns_for(
    SlashedForThisLoss { directory: ActiveOperatorsDirectory },
    Transaction {
      ..tx,
      redeemers: [
        Pair(Spend(receipt_outref()), spend_receipt()),
        Pair(
          Mint(active_operators_policy),
          slash_redeemer(ActiveOperatorsDirectory, 1),
        ),
        Pair(Mint(award_hash), burn_for_slash(ActiveOperatorsDirectory)),
      ],
    },
  )
}

test receipt_burn_of_two_receipts_is_refused() fail {
  let tx = slash_tx(ActiveOperatorsDirectory, ChallengerWins)
  award_burns_for(
    SlashedForThisLoss { directory: ActiveOperatorsDirectory },
    Transaction {
      ..tx,
      mint: assets.add(
        tx.mint,
        award_hash,
        validation_case_v1.receipt_token_name(other_case_id, operator_vkey),
        -1,
      ),
    },
  )
}

test receipt_burn_of_a_forged_receipt_is_refused() fail {
  award_burns_for(
    SlashedForThisLoss { directory: ActiveOperatorsDirectory },
    with_receipt(
      slash_tx(ActiveOperatorsDirectory, ChallengerWins),
      receipt_input_with(
        receipt_of(ChallengerWins, operator_vkey),
        assets.from_lovelace(env.fraud_proof_output_lovelace),
      ),
    ),
  )
}

// ## A later loss of an operator already slashed

fn element_ref(directory: SlashingDirectory, link: Option<ByteArray>) -> Input {
  Input {
    output_reference: outref(
      #"5555555555555555555555555555555555555555555555555555555555555555",
      when directory is {
        ActiveOperatorsDirectory -> 0
        RetiredOperatorsDirectory -> 1
      },
    ),
    output: directory_root(directory, link),
  }
}

fn second_loss_tx(
  active_link: Option<ByteArray>,
  retired_link: Option<ByteArray>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [receipt_input(ChallengerWins)],
    reference_inputs: [
      hub_ref_input(),
      element_ref(ActiveOperatorsDirectory, active_link),
      element_ref(RetiredOperatorsDirectory, retired_link),
    ],
    mint: receipt_burn(),
  }
}

const already_slashed =
  DefenderAlreadySlashed {
    active_operators_element_ref_input_index: 1,
    retired_operators_element_ref_input_index: 2,
  }

test a_later_loss_burns_its_receipt_with_no_slash() {
  let tx = second_loss_tx(None, None)
  and {
    award_burns_for(already_slashed, tx),
    award_spends_receipt(tx),
  }
}

/// The receipt names the key, not the registration: once a slashed key is
/// registered again, an older receipt for it is no longer burned without a
/// slash, and its slash consumes the new node.
test a_later_loss_of_an_active_operator_is_refused() fail {
  award_burns_for(already_slashed, second_loss_tx(Some(operator_vkey), None))
}

test a_later_loss_of_a_retired_operator_is_refused() fail {
  award_burns_for(already_slashed, second_loss_tx(None, Some(operator_vkey)))
}

test a_later_loss_against_elements_of_other_sets_is_refused() fail {
  award_burns_for(
    DefenderAlreadySlashed {
      active_operators_element_ref_input_index: 2,
      retired_operators_element_ref_input_index: 1,
    },
    second_loss_tx(None, None),
  )
}

// ## The receipt output

test receipt_spend_without_its_burn_is_refused() fail {
  award_spends_receipt(
    Transaction { ..second_loss_tx(None, None), mint: assets.zero },
  )
}

test receipt_spend_of_an_awarded_thread_is_refused() fail {
  let thread_datum: Data = ct.StepDatum {
    fraud_prover: claimant_vkey,
    data: Some(validation_resolution_v1.winning_resolution()),
  }
  let thread = Input {
    output_reference: receipt_outref(),
    output: Output {
      address: address.from_script(award_hash),
      value: element_value(
        env.fraud_proof_output_lovelace,
        case_hash,
        receipt_name(),
      ),
      datum: InlineDatum(thread_datum),
      reference_script: None,
    },
  }
  let tx = Transaction { ..second_loss_tx(None, None), inputs: [thread] }
  award_validator.main.spend(
    case_hash,
    hub_oracle_hash,
    expiry_hash,
    Some(thread_datum),
    SpendReceipt,
    receipt_outref(),
    Transaction { ..tx, mint: assets.from_asset(case_hash, receipt_name(), -1) },
  )
}

test receipt_closed_as_an_awarded_thread_is_refused() fail {
  award_validator.main.spend(
    case_hash,
    hub_oracle_hash,
    expiry_hash,
    Some(receipt_of(ChallengerWins, operator_vkey)),
    CloseAwardedThread { input_index: 0, case_input_index: 0 },
    receipt_outref(),
    second_loss_tx(None, None),
  )
}
