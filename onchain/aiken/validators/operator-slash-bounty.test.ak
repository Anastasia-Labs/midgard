//// Handler coverage for the bounty that a bond-consuming operator slash pays
//// to a named claimant: `SlashDuplicateOperator` on the registered-operators
//// policy, and `SlashOperator` for `SlashOperatorForBadSettlement` on the
//// active- and retired-operators policies.
////
//// Each path is exercised through its real minting handler with one honest
//// transaction per bond tranche: a duplicate registration and an active
//// operator hold the full bond, and a retired operator holds either the full
//// bond or the bond less the inactivity penalty. Every `fail` test is the
//// honest transaction with only its bounty side changed, and is named for the
//// check that refuses it:
////
////   claimant did not sign
////     -> `expect bounty_claimant |> utils.has_signed(extra_signatories)`
////   reward to another key, to the claimant key with a stake credential, or
////   no reward output at all
////     -> `bounty_routing_is_exact_v1`: no output pays the claimant, which is
////        accepted only while `expected_reward == 0`
////   reward one lovelace over or under, or merged into a larger claimant
////   output
////     -> `assets.lovelace_of(value) == expected_reward`
////   reward carrying a token
////     -> `assets.without_lovelace(value) == assets.zero`
////   reward carrying a datum
////     -> `datum == NoDatum`
////   reward carrying a reference script
////     -> `reference_script == None`
////   a second output to the claimant
////     -> `list.is_empty(other_claimant_outputs)`
////
//// The section "One operator slash per transaction" runs every directory
//// handler of a transaction that carries two bond-consuming directory
//// actions under different policies. Each slash alone is accepted, and each
//// pair is refused by the slash handler that holds the other policy as a
//// script parameter
////     -> `other_directory_is_untouched_v1`
////
//// The section "Slashed node holding more than its tranche" slashes active and
//// retired nodes, for a bad state and a bad settlement, holding more than the
//// tranche they were created with
////     -> `fraudulent_operator_slash_economics_are_exact_v1`

use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, from_verification_key, with_delegation_key}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Redeemer,
  ScriptPurpose, Spend, Transaction,
}
use env
use midgard/hub_oracle as hub
use midgard/operator_directory
use midgard/operator_directory/active_operators
use midgard/operator_directory/registered_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/settlement
use midgard/state_queue
use midgard/transition_trace
use operator_directory/active_operators as active_operators_validator
use operator_directory/registered_operators as registered_operators_validator
use operator_directory/retired_operators as retired_operators_validator

// ---------------------------------------------------------------------------
// Named script hashes and keys
// ---------------------------------------------------------------------------

const hub_oracle_hash: ScriptHash =
  #"a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0"

const settlement_policy: PolicyId =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

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

const foreign_policy: PolicyId =
  #"abababababababababababababababababababababababababababab"

const operator_vkey: VerificationKeyHash =
  #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

const claimant_vkey: VerificationKeyHash =
  #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

const other_vkey: VerificationKeyHash =
  #"b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3"

/// Registration keys of the duplicate being removed and of the registration
/// that proves it is a duplicate.
const duplicate_registration_key: ByteArray = #"0100"

const proof_registration_key: ByteArray = #"0200"

const element_lovelace: Lovelace = 2_000_000

// ---------------------------------------------------------------------------
// Bounty side of a slash
// ---------------------------------------------------------------------------

/// The bounty side of a slash transaction: the honest shape, or one change to
/// it.
type Bounty {
  Honest
  ClaimantDidNotSign
  RewardToAnotherKey
  RewardToStakedClaimantAddress
  RewardOneLovelaceOver
  RewardOneLovelaceUnder
  RewardMergedIntoLargerClaimantOutput
  RewardCarriesToken
  RewardCarriesDatum
  RewardCarriesReferenceScript
  NoRewardOutput
  SecondClaimantOutput
}

fn reward_output() -> Output {
  Output {
    address: from_verification_key(claimant_vkey),
    value: assets.from_lovelace(env.fraud_prover_reward),
    datum: NoDatum,
    reference_script: None,
  }
}

fn bounty_outputs(bounty: Bounty) -> List<Output> {
  let reward = reward_output()
  when bounty is {
    Honest | ClaimantDidNotSign -> [reward]
    RewardToAnotherKey ->
      [Output { ..reward, address: from_verification_key(other_vkey) }]
    RewardToStakedClaimantAddress ->
      [
        Output {
          ..reward,
          address: from_verification_key(claimant_vkey)
            |> with_delegation_key(other_vkey),
        },
      ]
    RewardOneLovelaceOver ->
      [
        Output {
          ..reward,
          value: assets.from_lovelace(env.fraud_prover_reward + 1),
        },
      ]
    RewardOneLovelaceUnder ->
      [
        Output {
          ..reward,
          value: assets.from_lovelace(env.fraud_prover_reward - 1),
        },
      ]
    RewardMergedIntoLargerClaimantOutput ->
      [
        Output {
          ..reward,
          value: assets.from_lovelace(
            env.fraud_prover_reward + element_lovelace,
          ),
        },
      ]
    RewardCarriesToken ->
      [
        Output {
          ..reward,
          value: reward.value |> assets.add(foreign_policy, "token", 1),
        },
      ]
    RewardCarriesDatum -> [Output { ..reward, datum: InlineDatum(0) }]
    RewardCarriesReferenceScript ->
      [Output { ..reward, reference_script: Some(foreign_policy) }]
    NoRewardOutput -> []
    SecondClaimantOutput ->
      [
        reward,
        Output { ..reward, value: assets.from_lovelace(element_lovelace) },
      ]
  }
}

fn bounty_signatories(bounty: Bounty) -> List<VerificationKeyHash> {
  when bounty is {
    ClaimantDidNotSign -> [other_vkey]
    _ -> [claimant_vkey]
  }
}

// ---------------------------------------------------------------------------
// Shared fixtures
// ---------------------------------------------------------------------------

fn script_address(hash: ScriptHash) -> Address {
  address.from_script(hash)
}

fn outref(seed: ByteArray, index: Int) -> OutputReference {
  OutputReference { transaction_id: seed, output_index: index }
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
    settlement: settlement_policy,
    payout: misc_policy,
    registered_operators_addr: script_address(registered_operators_policy),
    active_operators_addr: script_address(active_operators_policy),
    retired_operators_addr: script_address(retired_operators_policy),
    scheduler_addr: script_address(scheduler_policy),
    state_queue_addr: script_address(misc_policy),
    fraud_proof_catalogue_addr: script_address(misc_policy),
    fraud_proof_addr: script_address(misc_policy),
    deposit_addr: script_address(misc_policy),
    withdrawal_addr: script_address(misc_policy),
    tx_order_addr: script_address(misc_policy),
    settlement_addr: script_address(settlement_policy),
    reserve_addr: script_address(misc_policy),
    payout_addr: script_address(misc_policy),
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
      address: script_address(hub_oracle_hash),
      value: element_value(element_lovelace, hub_oracle_hash, hub.asset_name),
      datum: InlineDatum(hub_datum()),
      reference_script: None,
    },
  }
}

// ---------------------------------------------------------------------------
// Bad-settlement slash (active and retired operators)
// ---------------------------------------------------------------------------

fn settlement_outref(index: Int) -> OutputReference {
  outref(
    #"1111111111111111111111111111111111111111111111111111111111111111",
    index,
  )
}

fn directory_root_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 0)
}

fn slashed_node_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 1)
}

/// A settlement being disproved. The directory handlers read only its address
/// and its spending redeemer.
fn settlement_input(index: Int) -> Input {
  Input {
    output_reference: settlement_outref(index),
    output: Output {
      address: script_address(settlement_policy),
      value: element_value(element_lovelace, settlement_policy, #"c0"),
      datum: InlineDatum(0),
      reference_script: None,
    },
  }
}

fn disprove_redeemer(operator: VerificationKeyHash) -> Redeemer {
  let redeemer: settlement.SpendRedeemer = settlement.DisproveResolutionClaim {
    settlement_input_index: 0,
    settlement_output_index: 0,
    hub_ref_input_index: 0,
    operators_redeemer_index: 0,
    operator,
    operator_is_active: True,
    unresolved_event_ref_input_index: 1,
    unresolved_event_asset_name: #"c1",
    event_type: settlement.Deposit,
    membership_proof: settlement.DepositMembership {
      witness: transition_trace.RootMembershipProof {
        domain: transition_trace.DepositsRootDomain,
        root: #"",
        phas_root: #"",
        count: 1,
        key: #"",
        value: #"",
        proof: [],
      },
    },
    inclusion_proof_script_withdraw_redeemer_index: 1,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn bad_settlement_arguments() -> operator_directory.SlashingArguments {
  operator_directory.SlashingArguments {
    slashed_operator: operator_vkey,
    hub_oracle_ref_input_index: 0,
    slashed_operator_anchor_element_input_outref: directory_root_outref(),
    slashed_operator_anchor_element_output_index: 0,
    slashing_reason: operator_directory.SlashOperatorForBadSettlement {
      settlement_input_index: 0,
      settlement_redeemer_index: 1,
      bounty_claimant: claimant_vkey,
    },
  }
}

/// A directory root linked to the slashed operator's node, and that root
/// continued past it.
fn directory_root(
  policy: PolicyId,
  root_asset_name: AssetName,
  link: Option<ByteArray>,
) -> Output {
  let datum: operator_directory.Datum<Data> = linked_list.Element {
    data: linked_list.Root(#""),
    link,
  }
  Output {
    address: script_address(policy),
    value: element_value(element_lovelace, policy, root_asset_name),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn bad_settlement_tx(
  policy: PolicyId,
  root_asset_name: AssetName,
  node_asset_name: AssetName,
  node_data: Data,
  node_lovelace: Lovelace,
  fee: Lovelace,
  reference_inputs: List<Input>,
  own_redeemer: Redeemer,
  bounty: Bounty,
) -> Transaction {
  let node_datum: operator_directory.Datum<Data> = linked_list.Element {
    data: linked_list.Node(node_data),
    link: None,
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      settlement_input(0),
      Input {
        output_reference: directory_root_outref(),
        output: directory_root(policy, root_asset_name, Some(operator_vkey)),
      },
      Input {
        output_reference: slashed_node_outref(),
        output: Output {
          address: script_address(policy),
          value: element_value(node_lovelace, policy, node_asset_name),
          datum: InlineDatum(node_datum),
          reference_script: None,
        },
      },
    ],
    reference_inputs,
    outputs: [
      directory_root(policy, root_asset_name, None),
      ..bounty_outputs(bounty)
    ],
    fee,
    mint: assets.from_asset(policy, node_asset_name, -1),
    redeemers: [
      Pair(Mint(policy), own_redeemer),
      Pair(Spend(settlement_outref(0)), disprove_redeemer(operator_vkey)),
    ],
    extra_signatories: bounty_signatories(bounty),
  }
}

// ### Retired operator

fn retired_slash_redeemer() -> retired_operators.MintRedeemer {
  retired_operators.SlashOperator {
    slashing_arguments: bad_settlement_arguments(),
  }
}

/// `penalised` selects the tranche: the full bond, or the bond less the
/// inactivity penalty already taken at a forced retirement.
fn run_retired_bad_settlement_slash(penalised: Bool, bounty: Bounty) -> Bool {
  if penalised {
    slash_retired_for_bad_settlement(
      env.required_bond - env.inactivity_slashing_penalty,
      env.slashing_penalty - env.inactivity_slashing_penalty,
      bounty,
    )
  } else {
    slash_retired_for_bad_settlement(
      env.required_bond,
      env.slashing_penalty,
      bounty,
    )
  }
}

/// The bad-settlement slash of a retired node holding `node_lovelace`, paying
/// `fee`.
fn slash_retired_for_bad_settlement(
  node_lovelace: Lovelace,
  fee: Lovelace,
  bounty: Bounty,
) -> Bool {
  let node_data: Data = retired_operators.NodeData { bond_unlock_time: None }
  let own_redeemer: Data = retired_slash_redeemer()
  retired_operators_validator.mint.mint(
    hub_oracle_hash,
    retired_slash_redeemer(),
    retired_operators_policy,
    bad_settlement_tx(
      retired_operators_policy,
      retired_operators.root_asset_name,
      bytearray.concat(retired_operators.node_asset_name_prefix, operator_vkey),
      node_data,
      node_lovelace,
      fee,
      [hub_ref_input()],
      own_redeemer,
      bounty,
    ),
  )
}

// ### Active operator

fn scheduler_ref_input() -> Input {
  let datum: scheduler.SchedDatum = scheduler.NoActiveOperators
  Input {
    output_reference: outref(
      #"4444444444444444444444444444444444444444444444444444444444444444",
      0,
    ),
    output: Output {
      address: script_address(scheduler_policy),
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

fn active_slash_redeemer() -> active_operators.MintRedeemer {
  active_operators.SlashOperator {
    slashing_arguments: bad_settlement_arguments(),
    operator_removal_scheduler_sync: active_operators.ShowOperatorIsInactive {
      scheduler_ref_input_index: 1,
    },
  }
}

fn run_active_bad_settlement_slash(bounty: Bounty) -> Bool {
  slash_active_for_bad_settlement(
    env.required_bond,
    env.slashing_penalty,
    bounty,
  )
}

/// The bad-settlement slash of an active node holding `node_lovelace`, paying
/// `fee`.
fn slash_active_for_bad_settlement(
  node_lovelace: Lovelace,
  fee: Lovelace,
  bounty: Bounty,
) -> Bool {
  let node_data: Data = active_operators.NodeData {
    bond_unlock_time: None,
    inactivity_strikes: 0,
  }
  let own_redeemer: Data = active_slash_redeemer()
  active_operators_validator.mint.mint(
    hub_oracle_hash,
    registered_operators_policy,
    retired_operators_policy,
    active_slash_redeemer(),
    active_operators_policy,
    bad_settlement_tx(
      active_operators_policy,
      active_operators.root_asset_name,
      bytearray.concat(active_operators.node_asset_name_prefix, operator_vkey),
      node_data,
      node_lovelace,
      fee,
      [hub_ref_input(), scheduler_ref_input()],
      own_redeemer,
      bounty,
    ),
  )
}

// ---------------------------------------------------------------------------
// Duplicate-registration slash
// ---------------------------------------------------------------------------

fn registered_node_name(registration_key: ByteArray) -> AssetName {
  bytearray.concat(
    registered_operators.node_asset_name_prefix,
    registration_key,
  )
}

fn registered_node(
  operator: VerificationKeyHash,
  registration_key: ByteArray,
) -> Output {
  let datum: registered_operators.Datum = linked_list.Element {
    data: linked_list.Node(registered_operators.NodeData { operator }),
    link: None,
  }
  Output {
    address: script_address(registered_operators_policy),
    value: element_value(
      env.required_bond,
      registered_operators_policy,
      registered_node_name(registration_key),
    ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn duplicate_slash_redeemer() -> registered_operators.MintRedeemer {
  registered_operators.SlashDuplicateOperator {
    duplicate_operator: operator_vkey,
    anchor_element_input_outref: directory_root_outref(),
    anchor_element_output_index: 0,
    duplicate_node_ref_input_index: 0,
    duplicate_operator_status: registered_operators.DuplicateIsRegistered,
    bounty_claimant: claimant_vkey,
  }
}

fn duplicate_slash_redeemer_data() -> Redeemer {
  let redeemer_data: Data = duplicate_slash_redeemer()
  redeemer_data
}

fn duplicate_slash_tx(bounty: Bounty) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: directory_root_outref(),
        output: directory_root(
          registered_operators_policy,
          registered_operators.root_asset_name,
          Some(duplicate_registration_key),
        ),
      },
      Input {
        output_reference: slashed_node_outref(),
        output: registered_node(operator_vkey, duplicate_registration_key),
      },
    ],
    reference_inputs: [
      Input {
        output_reference: outref(
          #"5555555555555555555555555555555555555555555555555555555555555555",
          0,
        ),
        output: registered_node(operator_vkey, proof_registration_key),
      },
    ],
    outputs: [
      directory_root(
        registered_operators_policy,
        registered_operators.root_asset_name,
        None,
      ),
      ..bounty_outputs(bounty)
    ],
    fee: env.slashing_penalty,
    mint: assets.from_asset(
      registered_operators_policy,
      registered_node_name(duplicate_registration_key),
      -1,
    ),
    redeemers: [
      Pair(Mint(registered_operators_policy), duplicate_slash_redeemer_data()),
    ],
    extra_signatories: bounty_signatories(bounty),
  }
}

fn run_duplicate_slash(bounty: Bounty) -> Bool {
  registered_operators_validator.mint.mint(
    retired_operators_policy,
    hub_oracle_hash,
    duplicate_slash_redeemer(),
    registered_operators_policy,
    duplicate_slash_tx(bounty),
  )
}

// ---------------------------------------------------------------------------
// Duplicate-registration slash: full bond
// ---------------------------------------------------------------------------

test duplicate_slash_accepts_signed_claimant_with_exact_reward() {
  run_duplicate_slash(Honest)
}

test duplicate_slash_refuses_unsigned_claimant() fail {
  run_duplicate_slash(ClaimantDidNotSign)
}

test duplicate_slash_refuses_reward_to_another_key() fail {
  run_duplicate_slash(RewardToAnotherKey)
}

test duplicate_slash_refuses_reward_to_staked_claimant_address() fail {
  run_duplicate_slash(RewardToStakedClaimantAddress)
}

test duplicate_slash_refuses_reward_one_lovelace_over() fail {
  run_duplicate_slash(RewardOneLovelaceOver)
}

test duplicate_slash_refuses_reward_one_lovelace_under() fail {
  run_duplicate_slash(RewardOneLovelaceUnder)
}

test duplicate_slash_refuses_reward_merged_into_larger_output() fail {
  run_duplicate_slash(RewardMergedIntoLargerClaimantOutput)
}

test duplicate_slash_refuses_reward_carrying_token() fail {
  run_duplicate_slash(RewardCarriesToken)
}

test duplicate_slash_refuses_reward_carrying_datum() fail {
  run_duplicate_slash(RewardCarriesDatum)
}

test duplicate_slash_refuses_reward_carrying_reference_script() fail {
  run_duplicate_slash(RewardCarriesReferenceScript)
}

test duplicate_slash_refuses_missing_reward_output() fail {
  run_duplicate_slash(NoRewardOutput)
}

test duplicate_slash_refuses_second_claimant_output() fail {
  run_duplicate_slash(SecondClaimantOutput)
}

// ---------------------------------------------------------------------------
// Bad-settlement slash of an active operator: full bond
// ---------------------------------------------------------------------------

test active_bad_settlement_slash_accepts_signed_claimant_with_exact_reward() {
  run_active_bad_settlement_slash(Honest)
}

test active_bad_settlement_slash_refuses_unsigned_claimant() fail {
  run_active_bad_settlement_slash(ClaimantDidNotSign)
}

test active_bad_settlement_slash_refuses_reward_to_another_key() fail {
  run_active_bad_settlement_slash(RewardToAnotherKey)
}

test active_bad_settlement_slash_refuses_reward_to_staked_claimant_address() fail {
  run_active_bad_settlement_slash(RewardToStakedClaimantAddress)
}

test active_bad_settlement_slash_refuses_reward_one_lovelace_over() fail {
  run_active_bad_settlement_slash(RewardOneLovelaceOver)
}

test active_bad_settlement_slash_refuses_reward_one_lovelace_under() fail {
  run_active_bad_settlement_slash(RewardOneLovelaceUnder)
}

test active_bad_settlement_slash_refuses_reward_merged_into_larger_output() fail {
  run_active_bad_settlement_slash(RewardMergedIntoLargerClaimantOutput)
}

test active_bad_settlement_slash_refuses_reward_carrying_token() fail {
  run_active_bad_settlement_slash(RewardCarriesToken)
}

test active_bad_settlement_slash_refuses_reward_carrying_datum() fail {
  run_active_bad_settlement_slash(RewardCarriesDatum)
}

test active_bad_settlement_slash_refuses_reward_carrying_reference_script() fail {
  run_active_bad_settlement_slash(RewardCarriesReferenceScript)
}

test active_bad_settlement_slash_refuses_missing_reward_output() fail {
  run_active_bad_settlement_slash(NoRewardOutput)
}

test active_bad_settlement_slash_refuses_second_claimant_output() fail {
  run_active_bad_settlement_slash(SecondClaimantOutput)
}

// ---------------------------------------------------------------------------
// Bad-settlement slash of a retired operator: full bond
// ---------------------------------------------------------------------------

test retired_bad_settlement_slash_accepts_signed_claimant_with_exact_reward() {
  run_retired_bad_settlement_slash(False, Honest)
}

test retired_bad_settlement_slash_refuses_unsigned_claimant() fail {
  run_retired_bad_settlement_slash(False, ClaimantDidNotSign)
}

test retired_bad_settlement_slash_refuses_reward_to_another_key() fail {
  run_retired_bad_settlement_slash(False, RewardToAnotherKey)
}

test retired_bad_settlement_slash_refuses_reward_to_staked_claimant_address() fail {
  run_retired_bad_settlement_slash(False, RewardToStakedClaimantAddress)
}

test retired_bad_settlement_slash_refuses_reward_one_lovelace_over() fail {
  run_retired_bad_settlement_slash(False, RewardOneLovelaceOver)
}

test retired_bad_settlement_slash_refuses_reward_one_lovelace_under() fail {
  run_retired_bad_settlement_slash(False, RewardOneLovelaceUnder)
}

test retired_bad_settlement_slash_refuses_reward_merged_into_larger_output() fail {
  run_retired_bad_settlement_slash(False, RewardMergedIntoLargerClaimantOutput)
}

test retired_bad_settlement_slash_refuses_reward_carrying_token() fail {
  run_retired_bad_settlement_slash(False, RewardCarriesToken)
}

test retired_bad_settlement_slash_refuses_reward_carrying_datum() fail {
  run_retired_bad_settlement_slash(False, RewardCarriesDatum)
}

test retired_bad_settlement_slash_refuses_reward_carrying_reference_script() fail {
  run_retired_bad_settlement_slash(False, RewardCarriesReferenceScript)
}

test retired_bad_settlement_slash_refuses_missing_reward_output() fail {
  run_retired_bad_settlement_slash(False, NoRewardOutput)
}

test retired_bad_settlement_slash_refuses_second_claimant_output() fail {
  run_retired_bad_settlement_slash(False, SecondClaimantOutput)
}

// ---------------------------------------------------------------------------
// Bad-settlement slash of a retired operator: bond less the inactivity
// penalty
// ---------------------------------------------------------------------------

test penalised_retired_bad_settlement_slash_accepts_signed_claimant_with_exact_reward() {
  run_retired_bad_settlement_slash(True, Honest)
}

test penalised_retired_bad_settlement_slash_refuses_unsigned_claimant() fail {
  run_retired_bad_settlement_slash(True, ClaimantDidNotSign)
}

test penalised_retired_bad_settlement_slash_refuses_reward_to_another_key() fail {
  run_retired_bad_settlement_slash(True, RewardToAnotherKey)
}

test penalised_retired_bad_settlement_slash_refuses_reward_to_staked_claimant_address() fail {
  run_retired_bad_settlement_slash(True, RewardToStakedClaimantAddress)
}

test penalised_retired_bad_settlement_slash_refuses_reward_one_lovelace_over() fail {
  run_retired_bad_settlement_slash(True, RewardOneLovelaceOver)
}

test penalised_retired_bad_settlement_slash_refuses_reward_one_lovelace_under() fail {
  run_retired_bad_settlement_slash(True, RewardOneLovelaceUnder)
}

test penalised_retired_bad_settlement_slash_refuses_reward_merged_into_larger_output() fail {
  run_retired_bad_settlement_slash(True, RewardMergedIntoLargerClaimantOutput)
}

test penalised_retired_bad_settlement_slash_refuses_reward_carrying_token() fail {
  run_retired_bad_settlement_slash(True, RewardCarriesToken)
}

test penalised_retired_bad_settlement_slash_refuses_reward_carrying_datum() fail {
  run_retired_bad_settlement_slash(True, RewardCarriesDatum)
}

test penalised_retired_bad_settlement_slash_refuses_reward_carrying_reference_script() fail {
  run_retired_bad_settlement_slash(True, RewardCarriesReferenceScript)
}

test penalised_retired_bad_settlement_slash_refuses_missing_reward_output() fail {
  run_retired_bad_settlement_slash(True, NoRewardOutput)
}

test penalised_retired_bad_settlement_slash_refuses_second_claimant_output() fail {
  run_retired_bad_settlement_slash(True, SecondClaimantOutput)
}

// ---------------------------------------------------------------------------
// One operator slash per transaction
// ---------------------------------------------------------------------------
//
// `directory_actions_tx` places one or two operator-directory actions in a
// single transaction with one fee of `env.slashing_penalty` and one bounty
// output. Action `p` continues its directory root at output `p` and has its
// own minting redeemer at index `p`; settlement inputs come first, and each
// action's settlement spend, state queue removal or retired-set insertion
// follows all the directory minting redeemers. A duplicate-registration slash
// is always placed first, so its operator is `operator_vkey`.

/// An operator-directory action placed by `directory_actions_tx`.
type DirectoryAction {
  DuplicateRegistrationSlash
  ActiveBadSettlementSlash
  RetiredBadSettlementSlash
  ActiveBadStateSlash
  RetiredBadStateSlash
  /// A forced retirement after too many inactivity strikes: the active node
  /// is burnt and a retired node is minted for the bond less the inactivity
  /// penalty.
  PenalisedRetirement
}

const second_operator_vkey: VerificationKeyHash =
  #"b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4"

fn action_operator(position: Int) -> VerificationKeyHash {
  if position == 0 {
    operator_vkey
  } else {
    second_operator_vkey
  }
}

fn action_at(actions: List<DirectoryAction>, position: Int) -> DirectoryAction {
  expect Some(action) = list.at(actions, position)
  action
}

fn action_policy(action: DirectoryAction) -> PolicyId {
  when action is {
    DuplicateRegistrationSlash -> registered_operators_policy
    ActiveBadSettlementSlash | ActiveBadStateSlash | PenalisedRetirement ->
      active_operators_policy
    RetiredBadSettlementSlash | RetiredBadStateSlash -> retired_operators_policy
  }
}

fn action_root_asset_name(action: DirectoryAction) -> AssetName {
  when action is {
    DuplicateRegistrationSlash -> registered_operators.root_asset_name
    ActiveBadSettlementSlash | ActiveBadStateSlash | PenalisedRetirement ->
      active_operators.root_asset_name
    RetiredBadSettlementSlash | RetiredBadStateSlash ->
      retired_operators.root_asset_name
  }
}

/// The root and node of each directory are spent from a transaction named
/// after the directory policy.
fn directory_outref(policy: PolicyId, index: Int) -> OutputReference {
  outref(bytearray.concat(policy, #"00000000"), index)
}

fn action_node_key(action: DirectoryAction, position: Int) -> ByteArray {
  when action is {
    DuplicateRegistrationSlash -> duplicate_registration_key
    _ -> action_operator(position)
  }
}

fn action_node_name(action: DirectoryAction, position: Int) -> AssetName {
  let operator = action_operator(position)
  when action is {
    DuplicateRegistrationSlash ->
      registered_node_name(duplicate_registration_key)
    ActiveBadSettlementSlash | ActiveBadStateSlash | PenalisedRetirement ->
      bytearray.concat(active_operators.node_asset_name_prefix, operator)
    RetiredBadSettlementSlash | RetiredBadStateSlash ->
      bytearray.concat(retired_operators.node_asset_name_prefix, operator)
  }
}

fn directory_node(
  policy: PolicyId,
  node_asset_name: AssetName,
  node_data: Data,
  node_lovelace: Lovelace,
  link: Option<ByteArray>,
) -> Output {
  let datum: operator_directory.Datum<Data> = linked_list.Element {
    data: linked_list.Node(node_data),
    link,
  }
  Output {
    address: script_address(policy),
    value: element_value(node_lovelace, policy, node_asset_name),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn action_node(action: DirectoryAction, position: Int) -> Output {
  let policy = action_policy(action)
  let name = action_node_name(action, position)
  when action is {
    DuplicateRegistrationSlash ->
      registered_node(operator_vkey, duplicate_registration_key)
    ActiveBadSettlementSlash | ActiveBadStateSlash | PenalisedRetirement -> {
      let node_data: Data = active_operators.NodeData {
        bond_unlock_time: None,
        inactivity_strikes: if action == PenalisedRetirement {
          env.max_inactivity_strikes
        } else {
          0
        },
      }
      directory_node(policy, name, node_data, env.required_bond, None)
    }
    RetiredBadSettlementSlash | RetiredBadStateSlash -> {
      let node_data: Data = retired_operators.NodeData {
        bond_unlock_time: None,
      }
      directory_node(policy, name, node_data, env.required_bond, None)
    }
  }
}

fn action_directory_inputs(
  action: DirectoryAction,
  position: Int,
) -> List<Input> {
  let policy = action_policy(action)
  let root_name = action_root_asset_name(action)
  let own_inputs = [
    Input {
      output_reference: directory_outref(policy, 0),
      output: directory_root(
        policy,
        root_name,
        Some(action_node_key(action, position)),
      ),
    },
    Input {
      output_reference: directory_outref(policy, 1),
      output: action_node(action, position),
    },
  ]
  when action is {
    PenalisedRetirement ->
      [
        Input {
          output_reference: directory_outref(retired_operators_policy, 0),
          output: directory_root(
            retired_operators_policy,
            retired_operators.root_asset_name,
            None,
          ),
        },
        ..own_inputs
      ]
    _ -> own_inputs
  }
}

fn settles(action: DirectoryAction) -> Bool {
  when action is {
    ActiveBadSettlementSlash | RetiredBadSettlementSlash -> True
    _ -> False
  }
}

fn has_follow_up_redeemer(action: DirectoryAction) -> Bool {
  action != DuplicateRegistrationSlash
}

/// Index of the settlement input that action `position` disproves.
fn settlement_input_index(actions: List<DirectoryAction>, position: Int) -> Int {
  list.count(list.take(actions, position), settles)
}

/// Index of the redeemer that follows action `position`'s minting redeemer.
fn follow_up_redeemer_index(
  actions: List<DirectoryAction>,
  position: Int,
) -> Int {
  list.length(actions) + list.count(
    list.take(actions, position),
    has_follow_up_redeemer,
  )
}

fn action_slashing_arguments(
  actions: List<DirectoryAction>,
  position: Int,
) -> operator_directory.SlashingArguments {
  let action = action_at(actions, position)
  operator_directory.SlashingArguments {
    slashed_operator: action_operator(position),
    hub_oracle_ref_input_index: 0,
    slashed_operator_anchor_element_input_outref: directory_outref(
      action_policy(action),
      0,
    ),
    slashed_operator_anchor_element_output_index: position,
    slashing_reason: when action is {
      ActiveBadStateSlash | RetiredBadStateSlash ->
        operator_directory.SlashOperatorForBadState {
          state_queue_redeemer_index: follow_up_redeemer_index(
            actions,
            position,
          ),
        }
      _ ->
        operator_directory.SlashOperatorForBadSettlement {
          settlement_input_index: settlement_input_index(actions, position),
          settlement_redeemer_index: follow_up_redeemer_index(actions, position),
          bounty_claimant: claimant_vkey,
        }
    },
  }
}

fn scheduler_shows_operator_is_inactive() -> active_operators.OperatorRemovalSchedulerSync {
  active_operators.ShowOperatorIsInactive { scheduler_ref_input_index: 1 }
}

fn registered_action_redeemer(
  position: Int,
) -> registered_operators.MintRedeemer {
  registered_operators.SlashDuplicateOperator {
    duplicate_operator: operator_vkey,
    anchor_element_input_outref: directory_outref(
      registered_operators_policy,
      0,
    ),
    anchor_element_output_index: position,
    duplicate_node_ref_input_index: 2,
    duplicate_operator_status: registered_operators.DuplicateIsRegistered,
    bounty_claimant: claimant_vkey,
  }
}

fn active_action_redeemer(
  actions: List<DirectoryAction>,
  position: Int,
) -> active_operators.MintRedeemer {
  when action_at(actions, position) is {
    PenalisedRetirement ->
      active_operators.RetireOperator {
        active_operator_key: action_operator(position),
        hub_oracle_ref_input_index: 0,
        active_operator_anchor_element_input_outref: directory_outref(
          active_operators_policy,
          0,
        ),
        active_operator_anchor_element_output_index: position,
        retired_operators_redeemer_index: follow_up_redeemer_index(
          actions,
          position,
        ),
        penalize_for_inactivity: True,
        operator_removal_scheduler_sync: scheduler_shows_operator_is_inactive(),
      }
    _ ->
      active_operators.SlashOperator {
        slashing_arguments: action_slashing_arguments(actions, position),
        operator_removal_scheduler_sync: scheduler_shows_operator_is_inactive(),
      }
  }
}

fn retired_action_redeemer(
  actions: List<DirectoryAction>,
  position: Int,
) -> retired_operators.MintRedeemer {
  retired_operators.SlashOperator {
    slashing_arguments: action_slashing_arguments(actions, position),
  }
}

fn action_redeemer(actions: List<DirectoryAction>, position: Int) -> Redeemer {
  when action_at(actions, position) is {
    DuplicateRegistrationSlash -> {
      let redeemer: Data = registered_action_redeemer(position)
      redeemer
    }
    ActiveBadSettlementSlash | ActiveBadStateSlash | PenalisedRetirement -> {
      let redeemer: Data = active_action_redeemer(actions, position)
      redeemer
    }
    RetiredBadSettlementSlash | RetiredBadStateSlash -> {
      let redeemer: Data = retired_action_redeemer(actions, position)
      redeemer
    }
  }
}

/// The state queue removal that a bad-state slash at `position` answers.
fn state_queue_removal_redeemer(
  action: DirectoryAction,
  position: Int,
) -> Redeemer {
  let redeemer: state_queue.MintRedeemer = state_queue.RemoveFraudulentBlockHeader {
    yield_to_ref_input_index: 0,
    fraudulent_operator: action_operator(position),
    fraudulent_blocks_header_hash: #"",
    slashing_approach: if action == ActiveBadStateSlash {
      state_queue.SlashActiveOperator {
        active_operators_redeemer_index: position,
      }
    } else {
      state_queue.SlashRetiredOperator {
        retired_operators_redeemer_index: position,
      }
    },
    fraud_proof_ref_input_index: 0,
    block_removal_approach: state_queue.RemoveLastFraudulentBlock {
      anchor_element_input_outref: outref(
        #"8888888888888888888888888888888888888888888888888888888888888888",
        0,
      ),
      anchor_element_output_index: 0,
    },
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

/// The retired-set insertion of a forced retirement at `position`.
fn retired_insertion_redeemer(
  actions: List<DirectoryAction>,
  position: Int,
) -> Redeemer {
  let redeemer: retired_operators.MintRedeemer = retired_operators.RetireOperator {
    new_retired_operator_key: action_operator(position),
    bond_unlock_time: None,
    hub_oracle_ref_input_index: 0,
    retired_operator_anchor_element_output_index: list.length(actions) + 1,
    retired_operator_inserted_node_output_index: list.length(actions) + 2,
    active_operators_redeemer_index: position,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn follow_up_redeemer(
  actions: List<DirectoryAction>,
  position: Int,
) -> Option<Pair<ScriptPurpose, Redeemer>> {
  let action = action_at(actions, position)
  when action is {
    DuplicateRegistrationSlash -> None
    ActiveBadSettlementSlash | RetiredBadSettlementSlash ->
      Some(
        Pair(
          Spend(settlement_outref(settlement_input_index(actions, position))),
          disprove_redeemer(action_operator(position)),
        ),
      )
    ActiveBadStateSlash | RetiredBadStateSlash ->
      Some(
        Pair(Mint(misc_policy), state_queue_removal_redeemer(action, position)),
      )
    PenalisedRetirement ->
      Some(
        Pair(
          Mint(retired_operators_policy),
          retired_insertion_redeemer(actions, position),
        ),
      )
  }
}

fn action_mint(action: DirectoryAction, position: Int) -> Value {
  let burnt = assets.from_asset(
    action_policy(action),
    action_node_name(action, position),
    -1,
  )
  when action is {
    PenalisedRetirement ->
      burnt
        |> assets.add(
            retired_operators_policy,
            bytearray.concat(
              retired_operators.node_asset_name_prefix,
              action_operator(position),
            ),
            1,
          )
    _ -> burnt
  }
}

/// Outputs a forced retirement at `position` adds after the bounty: the
/// retired root linked to the new retired node, and that node.
fn retired_insertion_outputs(
  action: DirectoryAction,
  position: Int,
) -> List<Output> {
  when action is {
    PenalisedRetirement -> {
      let operator = action_operator(position)
      let node_data: Data = retired_operators.NodeData {
        bond_unlock_time: None,
      }
      [
        directory_root(
          retired_operators_policy,
          retired_operators.root_asset_name,
          Some(operator),
        ),
        directory_node(
          retired_operators_policy,
          bytearray.concat(retired_operators.node_asset_name_prefix, operator),
          node_data,
          env.required_bond - env.inactivity_slashing_penalty,
          None,
        ),
      ]
    }
    _ -> []
  }
}

fn directory_actions_tx(actions: List<DirectoryAction>) -> Transaction {
  let settlement_inputs = list.filter(actions, settles)
    |> list.indexed_map(fn(index, _) { settlement_input(index) })
  let directory_inputs = actions
    |> list.indexed_map(
        fn(position, action) { action_directory_inputs(action, position) },
      )
    |> list.foldr([], list.concat)
  let continued_roots = actions
    |> list.indexed_map(
        fn(_, action) {
          directory_root(
            action_policy(action),
            action_root_asset_name(action),
            None,
          )
        },
      )
  let insertion_outputs = actions
    |> list.indexed_map(
        fn(position, action) { retired_insertion_outputs(action, position) },
      )
    |> list.foldr([], list.concat)
  let mint = actions
    |> list.indexed_map(fn(position, action) { action_mint(action, position) })
    |> list.foldl(assets.zero, assets.merge)
  let own_redeemers = actions
    |> list.indexed_map(
        fn(position, action) {
          Pair(Mint(action_policy(action)), action_redeemer(actions, position))
        },
      )
  let follow_up_redeemers = actions
    |> list.indexed_map(
        fn(position, _) { follow_up_redeemer(actions, position) },
      )
    |> list.filter_map(fn(redeemer) { redeemer })
  Transaction {
    ..transaction.placeholder,
    inputs: list.concat(settlement_inputs, directory_inputs),
    reference_inputs: [
      hub_ref_input(),
      scheduler_ref_input(),
      Input {
        output_reference: outref(
          #"5555555555555555555555555555555555555555555555555555555555555555",
          0,
        ),
        output: registered_node(operator_vkey, proof_registration_key),
      },
    ],
    outputs: list.concat(
      continued_roots,
      [reward_output(), ..insertion_outputs],
    ),
    fee: env.slashing_penalty,
    mint,
    redeemers: list.concat(own_redeemers, follow_up_redeemers),
    extra_signatories: [claimant_vkey],
  }
}

/// Runs the minting handler of action `position` against the transaction that
/// carries all of `actions`.
fn run_directory_action(actions: List<DirectoryAction>, position: Int) -> Bool {
  run_directory_action_in(directory_actions_tx(actions), actions, position)
}

/// Runs the minting handler of action `position` of `actions` against `tx`.
fn run_directory_action_in(
  tx: Transaction,
  actions: List<DirectoryAction>,
  position: Int,
) -> Bool {
  when action_at(actions, position) is {
    DuplicateRegistrationSlash ->
      registered_operators_validator.mint.mint(
        retired_operators_policy,
        hub_oracle_hash,
        registered_action_redeemer(position),
        registered_operators_policy,
        tx,
      )
    ActiveBadSettlementSlash | ActiveBadStateSlash | PenalisedRetirement ->
      active_operators_validator.mint.mint(
        hub_oracle_hash,
        registered_operators_policy,
        retired_operators_policy,
        active_action_redeemer(actions, position),
        active_operators_policy,
        tx,
      )
    RetiredBadSettlementSlash | RetiredBadStateSlash ->
      retired_operators_validator.mint.mint(
        hub_oracle_hash,
        retired_action_redeemer(actions, position),
        retired_operators_policy,
        tx,
      )
  }
}

test one_slash_per_transaction_accepts_lone_duplicate_registration_slash() {
  run_directory_action([DuplicateRegistrationSlash], 0)
}

test one_slash_per_transaction_accepts_lone_active_bad_settlement_slash() {
  run_directory_action([ActiveBadSettlementSlash], 0)
}

test one_slash_per_transaction_accepts_lone_retired_bad_settlement_slash() {
  run_directory_action([RetiredBadSettlementSlash], 0)
}

test one_slash_per_transaction_accepts_lone_active_bad_state_slash() {
  run_directory_action([ActiveBadStateSlash], 0)
}

test one_slash_per_transaction_accepts_lone_retired_bad_state_slash() {
  run_directory_action([RetiredBadStateSlash], 0)
}

test active_slash_refuses_retired_slash_in_same_transaction() fail {
  run_directory_action([ActiveBadSettlementSlash, RetiredBadSettlementSlash], 0)
}

test active_bad_state_slash_refuses_retired_slash_in_same_transaction() fail {
  run_directory_action([ActiveBadStateSlash, RetiredBadSettlementSlash], 0)
}

test active_slash_refuses_retired_bad_state_slash_in_same_transaction() fail {
  run_directory_action([RetiredBadStateSlash, ActiveBadSettlementSlash], 1)
}

test active_slash_refuses_duplicate_registration_slash_in_same_transaction() fail {
  run_directory_action(
    [DuplicateRegistrationSlash, ActiveBadSettlementSlash],
    1,
  )
}

test active_bad_state_slash_refuses_duplicate_registration_slash_in_same_transaction() fail {
  run_directory_action([DuplicateRegistrationSlash, ActiveBadStateSlash], 1)
}

test duplicate_slash_refuses_retired_slash_in_same_transaction() fail {
  run_directory_action(
    [DuplicateRegistrationSlash, RetiredBadSettlementSlash],
    0,
  )
}

test duplicate_slash_refuses_retired_bad_state_slash_in_same_transaction() fail {
  run_directory_action([DuplicateRegistrationSlash, RetiredBadStateSlash], 0)
}

test duplicate_slash_refuses_penalised_retirement_in_same_transaction() fail {
  run_directory_action([DuplicateRegistrationSlash, PenalisedRetirement], 0)
}

// ---------------------------------------------------------------------------
// Slashed node holding more than its tranche
// ---------------------------------------------------------------------------
//
// A node keeps any lovelace added to it while it stays in its directory, so a
// slash accepts a node holding at least the bond less the inactivity penalty.
// The fee is the penalty of the tranche the node holds, raised by any excess
// up to `env.slashing_penalty`; the reward stays exactly
// `env.fraud_prover_reward`, and what is left of the node is unconstrained
// change. Every fee or node refusal below fails
//     -> `fraudulent_operator_slash_economics_are_exact_v1`
// and the reward refusals fail
//     -> `assets.lovelace_of(value) == expected_reward`

/// A pad as large as a whole bond.
const large_padding: Lovelace = env.required_bond

fn inactivity_tranche() -> Lovelace {
  env.required_bond - env.inactivity_slashing_penalty
}

fn inactivity_tranche_fee() -> Lovelace {
  env.slashing_penalty - env.inactivity_slashing_penalty
}

fn with_lovelace(value: Value, lovelace: Lovelace) -> Value {
  assets.add(
    value,
    assets.ada_policy_id,
    assets.ada_asset_name,
    lovelace - assets.lovelace_of(value),
  )
}

/// The lone bad-state slash of `action` with its node holding `node_lovelace`
/// and the transaction paying `fee`.
fn slash_alone_for_bad_state(
  action: DirectoryAction,
  node_lovelace: Lovelace,
  fee: Lovelace,
) -> Bool {
  let tx = directory_actions_tx([action])
  let policy = action_policy(action)
  let node_name = action_node_name(action, 0)
  run_directory_action_in(
    Transaction {
      ..tx,
      fee,
      inputs: list.map(
        tx.inputs,
        fn(input) {
          if assets.quantity_of(input.output.value, policy, node_name) == 1 {
            Input {
              ..input,
              output: Output {
                ..input.output,
                value: with_lovelace(input.output.value, node_lovelace),
              },
            }
          } else {
            input
          }
        },
      ),
    },
    [action],
    0,
  )
}

// ### Bad settlement, active operator

test active_bad_settlement_slash_accepts_node_one_lovelace_over_bond() {
  slash_active_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty,
    Honest,
  )
}

test active_bad_settlement_slash_accepts_node_far_over_bond() {
  slash_active_for_bad_settlement(
    env.required_bond + large_padding,
    env.slashing_penalty,
    Honest,
  )
}

test active_bad_settlement_slash_refuses_padded_node_fee_one_over_penalty() fail {
  slash_active_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty + 1,
    Honest,
  )
}

test active_bad_settlement_slash_refuses_padded_node_fee_one_under_penalty() fail {
  slash_active_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty - 1,
    Honest,
  )
}

test active_bad_settlement_slash_refuses_padded_node_reward_one_lovelace_over() fail {
  slash_active_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty,
    RewardOneLovelaceOver,
  )
}

test active_bad_settlement_slash_refuses_node_under_inactivity_tranche() fail {
  slash_active_for_bad_settlement(
    inactivity_tranche() - 1,
    inactivity_tranche_fee() - 1,
    Honest,
  )
}

// ### Bad settlement, retired operator

test retired_bad_settlement_slash_accepts_node_one_lovelace_over_bond() {
  slash_retired_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty,
    Honest,
  )
}

test retired_bad_settlement_slash_accepts_node_far_over_bond() {
  slash_retired_for_bad_settlement(
    env.required_bond + large_padding,
    env.slashing_penalty,
    Honest,
  )
}

test retired_bad_settlement_slash_accepts_inactivity_tranche_one_lovelace_over() {
  slash_retired_for_bad_settlement(
    inactivity_tranche() + 1,
    inactivity_tranche_fee() + 1,
    Honest,
  )
}

test retired_bad_settlement_slash_accepts_inactivity_tranche_far_over() {
  slash_retired_for_bad_settlement(
    inactivity_tranche() + large_padding,
    env.slashing_penalty,
    Honest,
  )
}

test retired_bad_settlement_slash_refuses_padded_node_fee_one_over_penalty() fail {
  slash_retired_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty + 1,
    Honest,
  )
}

test retired_bad_settlement_slash_refuses_padded_node_fee_one_under_penalty() fail {
  slash_retired_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty - 1,
    Honest,
  )
}

test retired_bad_settlement_slash_refuses_padded_inactivity_tranche_at_its_unpadded_fee() fail {
  slash_retired_for_bad_settlement(
    inactivity_tranche() + 1,
    inactivity_tranche_fee(),
    Honest,
  )
}

test retired_bad_settlement_slash_refuses_padded_inactivity_tranche_fee_one_over() fail {
  slash_retired_for_bad_settlement(
    inactivity_tranche() + 1,
    inactivity_tranche_fee() + 2,
    Honest,
  )
}

test retired_bad_settlement_slash_refuses_padded_node_reward_one_lovelace_under() fail {
  slash_retired_for_bad_settlement(
    env.required_bond + 1,
    env.slashing_penalty,
    RewardOneLovelaceUnder,
  )
}

test retired_bad_settlement_slash_refuses_node_under_inactivity_tranche() fail {
  slash_retired_for_bad_settlement(
    inactivity_tranche() - 1,
    inactivity_tranche_fee() - 1,
    Honest,
  )
}

// ### Bad state, active operator

test active_bad_state_slash_accepts_node_one_lovelace_over_bond() {
  slash_alone_for_bad_state(
    ActiveBadStateSlash,
    env.required_bond + 1,
    env.slashing_penalty,
  )
}

test active_bad_state_slash_accepts_node_far_over_bond() {
  slash_alone_for_bad_state(
    ActiveBadStateSlash,
    env.required_bond + large_padding,
    env.slashing_penalty,
  )
}

test active_bad_state_slash_refuses_padded_node_fee_one_over_penalty() fail {
  slash_alone_for_bad_state(
    ActiveBadStateSlash,
    env.required_bond + 1,
    env.slashing_penalty + 1,
  )
}

test active_bad_state_slash_refuses_padded_node_fee_one_under_penalty() fail {
  slash_alone_for_bad_state(
    ActiveBadStateSlash,
    env.required_bond + 1,
    env.slashing_penalty - 1,
  )
}

test active_bad_state_slash_refuses_node_under_inactivity_tranche() fail {
  slash_alone_for_bad_state(
    ActiveBadStateSlash,
    inactivity_tranche() - 1,
    inactivity_tranche_fee() - 1,
  )
}

// ### Bad state, retired operator

test retired_bad_state_slash_accepts_node_one_lovelace_over_bond() {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    env.required_bond + 1,
    env.slashing_penalty,
  )
}

test retired_bad_state_slash_accepts_node_far_over_bond() {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    env.required_bond + large_padding,
    env.slashing_penalty,
  )
}

test retired_bad_state_slash_accepts_inactivity_tranche() {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    inactivity_tranche(),
    inactivity_tranche_fee(),
  )
}

test retired_bad_state_slash_accepts_inactivity_tranche_one_lovelace_over() {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    inactivity_tranche() + 1,
    inactivity_tranche_fee() + 1,
  )
}

test retired_bad_state_slash_refuses_padded_node_fee_one_over_penalty() fail {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    env.required_bond + 1,
    env.slashing_penalty + 1,
  )
}

test retired_bad_state_slash_refuses_padded_node_fee_one_under_penalty() fail {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    env.required_bond + 1,
    env.slashing_penalty - 1,
  )
}

test retired_bad_state_slash_refuses_padded_inactivity_tranche_at_its_unpadded_fee() fail {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    inactivity_tranche() + 1,
    inactivity_tranche_fee(),
  )
}

test retired_bad_state_slash_refuses_node_under_inactivity_tranche() fail {
  slash_alone_for_bad_state(
    RetiredBadStateSlash,
    inactivity_tranche() - 1,
    inactivity_tranche_fee() - 1,
  )
}
