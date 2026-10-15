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

use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, from_verification_key, with_delegation_key}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Redeemer, Spend,
  Transaction,
}
use env
use midgard/hub_oracle as hub
use midgard/operator_directory
use midgard/operator_directory/active_operators
use midgard/operator_directory/registered_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/settlement
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

fn settlement_outref() -> OutputReference {
  outref(#"1111111111111111111111111111111111111111111111111111111111111111", 0)
}

fn directory_root_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 0)
}

fn slashed_node_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 1)
}

/// The settlement being disproved. The directory handlers read only its
/// address and its spending redeemer.
fn settlement_input() -> Input {
  Input {
    output_reference: settlement_outref(),
    output: Output {
      address: script_address(settlement_policy),
      value: element_value(element_lovelace, settlement_policy, #"c0"),
      datum: InlineDatum(0),
      reference_script: None,
    },
  }
}

fn disprove_redeemer() -> Redeemer {
  let redeemer: settlement.SpendRedeemer = settlement.DisproveResolutionClaim {
    settlement_input_index: 0,
    settlement_output_index: 0,
    hub_ref_input_index: 0,
    operators_redeemer_index: 0,
    operator: operator_vkey,
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
      settlement_input(),
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
      Pair(Spend(settlement_outref()), disprove_redeemer()),
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
  let (node_lovelace, fee) = if penalised {
    (
      env.required_bond - env.inactivity_slashing_penalty,
      env.slashing_penalty - env.inactivity_slashing_penalty,
    )
  } else {
    (env.required_bond, env.slashing_penalty)
  }
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
      env.required_bond,
      env.slashing_penalty,
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

fn registered_node(registration_key: ByteArray) -> Output {
  let datum: registered_operators.Datum = linked_list.Element {
    data: linked_list.Node(
      registered_operators.NodeData { operator: operator_vkey },
    ),
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
        output: registered_node(duplicate_registration_key),
      },
    ],
    reference_inputs: [
      Input {
        output_reference: outref(
          #"5555555555555555555555555555555555555555555555555555555555555555",
          0,
        ),
        output: registered_node(proof_registration_key),
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
