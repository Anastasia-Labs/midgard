//// The case validator and its configuration, driven through the deployed
//// validators: the configuration NFT, the case NFT mint, and the case close in
//// each of its rows, with the challenger bond spent beside it.

use aiken/collection/list
use aiken/interval
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, Script}
use cardano/assets
use cardano/script_context.{ScriptContext, Spending}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
}
use env
use fraud_proofs/validation_trace/award_v1 as award_validator
use fraud_proofs/validation_trace/case_config_v1 as config_validator
use fraud_proofs/validation_trace/case_v1 as case_validator
use fraud_proofs/validation_trace/timeout_v1 as timeout_validator
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1, SettleWithCase}
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/operator_directory/active_operators.{UpdateBondHoldNewDefense}
use midgard/validation_award_v1.{CloseAwardedThread, MintReceipt}
use midgard/validation_case_config_v1.{CaseConfigV1, config_asset_name}
use midgard/validation_case_v1.{
  BurnThreadToken, CaseClose, CaseRedeemerV1, CaseThreadV1, CaseV1, Defend,
  ThreadClose, ThreadHeaderV1, VerifySource,
}
use midgard/validation_dispute_v1.{
  ChallengerWins, DefenderWins, DisputeWinner, NeitherClaimValid,
}
use midgard/validation_resolution_v1
use midgard/validation_trace_v1.{ValidationMachineStateV1, ValidationTraceProof}
use operator_directory/active_operators as active_operators_validator

const hub_policy = #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const case_policy = #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const config_policy =
  #"c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2"

const thread_policy =
  #"c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3"

const fraud_proof_policy =
  #"a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6"

const fraud_proof_script =
  #"b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6"

const source_hash = #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1"

const bond_hash = #"d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6"

const expiry_hash = #"d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7"

const prover = #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1"

const other_key = #"e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2"

const header_hash = #"08080808080808080808080808080808080808080808080808080808"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const h32_c =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const bond_lovelace = 10_000_000_000

const case_lovelace = 5_000_000

const fee = 300_000

const started = 1_000_000

const thread_name =
  #"0000000608080808080808080808080808080808080808080808080808080808"

fn out_ref(id: ByteArray, index: Int) -> OutputReference {
  OutputReference { transaction_id: id, output_index: index }
}

fn case_out_ref() -> OutputReference {
  out_ref(h32_a, 0)
}

fn bond_out_ref() -> OutputReference {
  out_ref(h32_a, 1)
}

fn event_key() -> ledger_state.EventKey {
  ledger_state.L2TransactionEventKey { tx_id: h32_b }
}

fn case_id() -> ByteArray {
  validation_case_v1.case_nft_name(header_hash, event_key(), out_ref(h32_c, 0))
}

fn bond_marker() -> ByteArray {
  validation_case_v1.bond_marker_name(case_id())
}

/// What VerifySource mints under the case policy: the case NFT and its bond
/// marker.
fn case_tokens() -> assets.Value {
  assets.from_asset(case_policy, case_id(), 1)
    |> assets.add(case_policy, bond_marker(), 1)
}

// ## Reference inputs

fn config() -> CaseConfigV1 {
  CaseConfigV1 {
    source_v1: source_hash,
    game_v1: #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2",
    boundary_v1: #"d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3",
    award_v1: #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4",
    timeout_v1: #"d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5",
    dispute_bond_v1: bond_hash,
    thread_expiry_v1: expiry_hash,
  }
}

fn config_output(datum: Data, value: assets.Value) -> Output {
  Output {
    address: address.from_script(config_policy),
    value,
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn config_nft() -> assets.Value {
  assets.from_lovelace(2_000_000)
    |> assets.add(config_policy, config_asset_name, 1)
}

fn config_reference() -> Input {
  let datum: Data = config()
  Input {
    output_reference: out_ref(h32_c, 7),
    output: config_output(datum, config_nft()),
  }
}

fn hub_datum() -> hub.Datum {
  let s = address.from_script
  hub.Datum {
    registered_operators: #"a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0",
    active_operators: #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1",
    retired_operators: #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2",
    scheduler: #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3",
    state_queue: #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4",
    fraud_proof_catalogue: #"a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5",
    fraud_proof: fraud_proof_policy,
    deposit: #"a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7",
    withdrawal: #"a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8",
    tx_order: #"a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9",
    settlement: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    payout: #"abababababababababababababababababababababababababababab",
    registered_operators_addr: s(
      #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0",
    ),
    active_operators_addr: s(
      #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1",
    ),
    retired_operators_addr: s(
      #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2",
    ),
    scheduler_addr: s(
      #"b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3",
    ),
    state_queue_addr: s(
      #"b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4",
    ),
    fraud_proof_catalogue_addr: s(
      #"b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5",
    ),
    fraud_proof_addr: s(fraud_proof_script),
    deposit_addr: s(#"b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7"),
    withdrawal_addr: s(
      #"b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8",
    ),
    tx_order_addr: s(
      #"b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9",
    ),
    settlement_addr: s(
      #"babababababababababababababababababababababababababababa",
    ),
    reserve_addr: s(#"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),
    payout_addr: s(#"bcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbc"),
    reserve_observer: #"bdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbd",
  }
}

fn hub_reference() -> Input {
  let datum: Data = hub_datum()
  Input {
    output_reference: out_ref(h32_c, 8),
    output: Output {
      address: address.from_script(hub_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(hub_policy, hub.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

// ## The configuration NFT

const init_utxo = OutputReference { transaction_id: h32_c, output_index: 9 }

fn config_mint_tx(outputs: List<Output>, mint: assets.Value) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: init_utxo,
        output: Output {
          address: address.from_verification_key(prover),
          value: assets.from_lovelace(10_000_000),
          datum: NoDatum,
          reference_script: None,
        },
      },
    ],
    outputs,
    mint,
  }
}

fn config_mints(tx: Transaction) -> Bool {
  config_validator.main.mint(init_utxo, Void, config_policy, tx)
}

fn one_config_nft() -> assets.Value {
  assets.from_asset(config_policy, config_asset_name, 1)
}

test config_mint_locks_the_configuration_at_its_own_address() {
  let tx = config_mint_tx([config_reference().output], one_config_nft())
  config_mints(tx)
}

test config_mint_without_the_init_utxo_is_refused() fail {
  let tx = config_mint_tx([config_reference().output], one_config_nft())
  config_mints(Transaction { ..tx, inputs: [] })
}

test config_mint_of_another_name_is_refused() fail {
  let datum: Data = config()
  let value = assets.from_lovelace(2_000_000)
    |> assets.add(config_policy, "other", 1)
  config_mints(
    config_mint_tx(
      [config_output(datum, value)],
      assets.from_asset(config_policy, "other", 1),
    ),
  )
}

test config_mint_of_two_tokens_is_refused() fail {
  let datum: Data = config()
  let value = assets.from_lovelace(2_000_000)
    |> assets.add(config_policy, config_asset_name, 2)
  config_mints(
    config_mint_tx(
      [config_output(datum, value)],
      assets.from_asset(config_policy, config_asset_name, 2),
    ),
  )
}

test config_mint_to_another_address_is_refused() fail {
  config_mints(
    config_mint_tx(
      [
        Output {
          ..config_reference().output,
          address: address.from_verification_key(prover),
        },
      ],
      one_config_nft(),
    ),
  )
}

test config_mint_with_a_datum_that_is_not_a_configuration_is_refused() fail {
  config_mints(
    config_mint_tx([config_output(1, config_nft())], one_config_nft()),
  )
}

test config_spend_is_refused() fail {
  config_validator.main.else(
    init_utxo,
    ScriptContext {
      transaction: transaction.placeholder,
      redeemer: Void,
      info: Spending { output: init_utxo, datum: None },
    },
  )
}

// ## The case NFT mint

fn opener_at(script_hash: ByteArray) -> Input {
  Input {
    output_reference: out_ref(h32_c, 0),
    output: Output {
      address: address.from_script(script_hash),
      value: assets.from_lovelace(case_lovelace)
        |> assets.add(thread_policy, thread_name, 1),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn case_mint(redeemer: CaseRedeemerV1, tx: Transaction) -> Bool {
  case_validator.main.mint(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    redeemer,
    case_policy,
    tx,
  )
}

fn verify_source_mint_tx(opener: Input, mint: assets.Value) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [opener],
    reference_inputs: [config_reference()],
    mint,
  }
}

const verify_source =
  VerifySource { opener_input_index: 0, case_output_index: 0 }

test case_nft_mints_with_a_source_opener() {
  case_mint(
    verify_source,
    verify_source_mint_tx(opener_at(source_hash), case_tokens()),
  )
}

test case_nft_mint_with_an_opener_of_another_script_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(opener_at(bond_hash), case_tokens()),
  )
}

test case_nft_mint_of_a_doubled_case_nft_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(source_hash),
      case_tokens() |> assets.add(case_policy, case_id(), 1),
    ),
  )
}

test case_nft_mint_without_the_bond_marker_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(source_hash),
      assets.from_asset(case_policy, case_id(), 1),
    ),
  )
}

/// A marker is only ever minted beside the case NFT it is named from, so no
/// marker exists ahead of its case.
test case_nft_mint_with_the_marker_of_another_case_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(source_hash),
      assets.from_asset(case_policy, case_id(), 1)
        |> assets.add(
            case_policy,
            validation_case_v1.bond_marker_name(h32_c),
            1,
          ),
    ),
  )
}

test case_nft_mint_with_a_third_token_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(source_hash),
      case_tokens() |> assets.add(case_policy, h32_c, 1),
    ),
  )
}

test case_nft_mint_without_the_configuration_is_refused() fail {
  case_mint(
    verify_source,
    Transaction {
      ..verify_source_mint_tx(opener_at(source_hash), case_tokens()),
      reference_inputs: [],
    },
  )
}

/// A configuration-shaped datum at another address, without the NFT, is not
/// the configuration.
test case_nft_mint_with_a_look_alike_configuration_is_refused() fail {
  let datum: Data = config()
  let look_alike = Input {
    output_reference: out_ref(h32_c, 7),
    output: Output {
      ..config_output(datum, assets.from_lovelace(2_000_000)),
      address: address.from_verification_key(other_key),
    },
  }
  case_mint(
    verify_source,
    Transaction {
      ..verify_source_mint_tx(opener_at(source_hash), case_tokens()),
      reference_inputs: [look_alike],
    },
  )
}

test case_policy_burn_without_a_case_input_is_refused() fail {
  case_mint(
    CaseClose {
      bond_input_index: 1,
      bond_output_index: 0,
      remainder_output_index: 1,
    },
    Transaction {
      ..transaction.placeholder,
      inputs: [opener_at(source_hash)],
      mint: assets.from_asset(case_policy, case_id(), -1),
    },
  )
}

test case_policy_burn_beside_a_case_input_defers_to_the_case_spend() {
  case_mint(
    CaseClose {
      bond_input_index: 1,
      bond_output_index: 0,
      remainder_output_index: 1,
    },
    Transaction {
      ..transaction.placeholder,
      inputs: [case_input(undefended())],
      mint: assets.from_asset(case_policy, case_id(), -1),
    },
  )
}

// ## The thread-token burn with no case

const case_thread_name =
  #"4444444444444444444444444444444444444444444444444444444444444444"

fn held_thread_token() -> assets.Value {
  assets.from_lovelace(2_000_000)
    |> assets.add(case_policy, case_thread_name, 1)
}

fn thread_holder(value: assets.Value) -> Input {
  Input {
    output_reference: out_ref(h32_b, 3),
    output: Output {
      address: address.from_script(config().game_v1),
      value,
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn withdrawal_of(script_hash: ByteArray) -> Pairs<address.Credential, Int> {
  [Pair(Script(script_hash), 0)]
}

fn thread_burn_tx(
  mint: assets.Value,
  withdrawals: Pairs<address.Credential, Int>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [thread_holder(held_thread_token())],
    reference_inputs: [config_reference()],
    mint,
    withdrawals,
  }
}

fn one_thread_burn() -> assets.Value {
  assets.from_asset(case_policy, case_thread_name, -1)
}

test thread_token_burns_beside_the_expiry_withdrawal() {
  case_mint(
    BurnThreadToken,
    thread_burn_tx(one_thread_burn(), withdrawal_of(config().thread_expiry_v1)),
  )
}

test thread_token_burn_without_the_expiry_withdrawal_is_refused() fail {
  case_mint(BurnThreadToken, thread_burn_tx(one_thread_burn(), []))
}

test thread_token_burn_beside_another_withdrawal_is_refused() fail {
  case_mint(
    BurnThreadToken,
    thread_burn_tx(one_thread_burn(), withdrawal_of(config().game_v1)),
  )
}

test thread_token_burn_of_two_tokens_is_refused() fail {
  case_mint(
    BurnThreadToken,
    thread_burn_tx(
      one_thread_burn() |> assets.add(case_policy, h32_a, -1),
      withdrawal_of(config().thread_expiry_v1),
    ),
  )
}

test thread_token_burn_of_a_doubled_token_is_refused() fail {
  case_mint(
    BurnThreadToken,
    thread_burn_tx(
      assets.from_asset(case_policy, case_thread_name, -2),
      withdrawal_of(config().thread_expiry_v1),
    ),
  )
}

test thread_token_mint_under_the_burn_redeemer_is_refused() fail {
  case_mint(
    BurnThreadToken,
    thread_burn_tx(
      assets.from_asset(case_policy, case_thread_name, 1),
      withdrawal_of(config().thread_expiry_v1),
    ),
  )
}

test thread_token_burn_without_the_configuration_is_refused() fail {
  let tx = thread_burn_tx(
    one_thread_burn(),
    withdrawal_of(config().thread_expiry_v1),
  )
  case_mint(BurnThreadToken, Transaction { ..tx, reference_inputs: [] })
}

test case_spend_under_the_thread_burn_redeemer_is_refused() fail {
  case_validator.main.spend(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    Some(undefended()),
    BurnThreadToken,
    case_out_ref(),
    Transaction {
      ..transaction.placeholder,
      inputs: [case_input(undefended())],
      reference_inputs: [config_reference()],
      mint: assets.from_asset(case_policy, case_id(), -1),
    },
  )
}

// ## The case close

fn opened() -> CaseV1 {
  validation_case_v1.open_case(
    header_hash,
    event_key(),
    h32_a,
    4,
    h32_b,
    prover,
    h32_c,
    started,
  )
}

/// No defense: convicts once the defense window has passed.
fn undefended() -> CaseV1 {
  opened()
}

/// One defense, won by the defender.
fn defender_won() -> CaseV1 {
  CaseV1 { ..opened(), open_threads: 0, defender_won: True }
}

/// One defense, ended with neither claim valid.
fn no_valid_claim() -> CaseV1 {
  CaseV1 { ..opened(), open_threads: 0, neither_claim_valid: True }
}

/// One defense still open.
fn one_thread_open() -> CaseV1 {
  CaseV1 { ..opened(), open_threads: 1 }
}

fn after_window() -> Int {
  validation_case_v1.defense_deadline(opened()) + 1
}

fn case_value() -> assets.Value {
  assets.from_lovelace(case_lovelace)
    |> assets.add(thread_policy, thread_name, 1)
    |> assets.add(case_policy, case_id(), 1)
}

fn case_input(case: CaseV1) -> Input {
  let datum: Data = case
  Input {
    output_reference: case_out_ref(),
    output: Output {
      address: address.from_script(case_policy),
      value: case_value(),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn bond_datum_of(id: ByteArray) -> Data {
  let datum: Data = DisputeBondV1 {
    challenged_header_hash: header_hash,
    fraud_prover: prover,
    case_id: id,
  }
  datum
}

fn bond_input_holding(
  script: ByteArray,
  lovelace: Int,
  datum: Data,
  tokens: assets.Value,
) -> Input {
  Input {
    output_reference: bond_out_ref(),
    output: Output {
      address: address.from_script(script),
      value: assets.merge(assets.from_lovelace(lovelace), tokens),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// A bond output holding the case's bond marker.
fn bond_input_with(script: ByteArray, lovelace: Int, datum: Data) -> Input {
  bond_input_holding(
    script,
    lovelace,
    datum,
    assets.from_asset(case_policy, bond_marker(), 1),
  )
}

/// A bond-shaped output at the bond script naming the case, without the
/// case's marker.
fn decoy_bond_input() -> Input {
  bond_input_holding(
    bond_hash,
    bond_lovelace,
    bond_datum_of(case_id()),
    assets.zero,
  )
}

/// `tx` with the decoy in place of the bond.
fn with_decoy_bond(case: CaseV1, tx: Transaction) -> Transaction {
  Transaction { ..tx, inputs: [case_input(case), decoy_bond_input()] }
}

fn bond_input() -> Input {
  bond_input_with(bond_hash, bond_lovelace, bond_datum_of(case_id()))
}

fn tagged(to: Address, lovelace: Int, tag: OutputReference) -> Output {
  let tag_data: Data = tag
  Output {
    address: to,
    value: assets.from_lovelace(lovelace),
    datum: InlineDatum(tag_data),
    reference_script: None,
  }
}

fn prover_address() -> Address {
  address.from_verification_key(prover)
}

fn burn() -> assets.Value {
  assets.from_asset(thread_policy, thread_name, -1)
    |> assets.merge(assets.negate(case_tokens()))
}

const lock_lovelace = 1_500_000

/// The case NFT and the bond marker, locked at the fraud-proof address by a
/// convicted close.
fn case_lock(lovelace: Int) -> Output {
  Output {
    address: address.from_script(fraud_proof_script),
    value: assets.merge(assets.from_lovelace(lovelace), case_tokens()),
    datum: NoDatum,
    reference_script: None,
  }
}

fn fraud_proof_output(lovelace: Int, fraud_prover: ByteArray) -> Output {
  let datum: Data = fraud_proof.Datum { fraud_prover }
  Output {
    address: address.from_script(fraud_proof_script),
    value: assets.from_lovelace(lovelace)
      |> assets.add(fraud_proof_policy, thread_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

const close =
  CaseClose {
    bond_input_index: 1,
    bond_output_index: 0,
    remainder_output_index: 1,
  }

fn close_tx(
  case: CaseV1,
  lower: Int,
  outputs: List<Output>,
  mint: assets.Value,
) -> Transaction {
  let redeemer: Data = close
  Transaction {
    ..transaction.placeholder,
    inputs: [case_input(case), bond_input()],
    reference_inputs: [config_reference(), hub_reference()],
    outputs,
    mint,
    fee,
    validity_range: interval.between(
      lower,
      lower + validation_dispute_v1.dispute_max_validity_range_milliseconds,
    ),
    redeemers: [Pair(Spend(case_out_ref()), redeemer)],
  }
}

/// The close's two spends: the case, and the bond beside it.
fn closes(case: CaseV1, tx: Transaction) -> Bool {
  and {
    case_validator.main.spend(
      hub_policy,
      config_policy,
      thread_policy,
      bond_lovelace,
      Some(case),
      close,
      case_out_ref(),
      tx,
    ),
    dispute_bond_v1.spend(
      case_policy,
      Some(
        DisputeBondV1 {
          challenged_header_hash: header_hash,
          fraud_prover: prover,
          case_id: case_id(),
        },
      ),
      SettleWithCase { case_input_index: 0 },
      bond_out_ref(),
      tx,
    ),
  }
}

/// The honest close of a case that does not convict: the bond goes to the
/// treasury and the rest of the case to the challenger's payee.
fn standing_close(case: CaseV1, lower: Int) -> Transaction {
  close_tx(
    case,
    lower,
    [
      tagged(validation_case_v1.treasury_address, bond_lovelace, bond_out_ref()),
      tagged(prover_address(), case_lovelace, case_out_ref()),
    ],
    burn(),
  )
}

fn fp_lovelace() -> Int {
  env.fraud_proof_output_lovelace
}

/// The honest convicted close: the bond is refunded, and the fraud-proof
/// output and the case NFT lock are funded from the case.
fn convicted_close_locking(
  lower: Int,
  fp_output: Output,
  lock: Output,
  remainder: Int,
  mint: assets.Value,
) -> Transaction {
  close_tx(
    undefended(),
    lower,
    [
      tagged(prover_address(), bond_lovelace, bond_out_ref()),
      tagged(prover_address(), remainder, case_out_ref()),
      fp_output,
      lock,
    ],
    mint,
  )
}

fn convicted_close_with(
  lower: Int,
  fp_output: Output,
  remainder: Int,
  mint: assets.Value,
) -> Transaction {
  convicted_close_locking(
    lower,
    fp_output,
    case_lock(lock_lovelace),
    remainder - lock_lovelace,
    mint,
  )
}

fn convicting_mint() -> assets.Value {
  assets.from_asset(thread_policy, thread_name, -1)
    |> assets.add(fraud_proof_policy, thread_name, 1)
}

fn convicted_close() -> Transaction {
  convicted_close_with(
    after_window(),
    fraud_proof_output(fp_lovelace(), prover),
    case_lovelace - fp_lovelace(),
    convicting_mint(),
  )
}

// ### Row: convicted (no defense, or every defense lost)

test close_convicted_refunds_the_bond_and_mints_the_fraud_proof() {
  closes(undefended(), convicted_close())
}

test close_convicted_at_the_defense_deadline_is_refused() fail {
  closes(
    undefended(),
    convicted_close_with(
      validation_case_v1.defense_deadline(opened()),
      fraud_proof_output(fp_lovelace(), prover),
      case_lovelace - fp_lovelace(),
      convicting_mint(),
    ),
  )
}

test close_convicted_without_the_fraud_proof_mint_is_refused() fail {
  closes(
    undefended(),
    close_tx(
      undefended(),
      after_window(),
      [
        tagged(prover_address(), bond_lovelace, bond_out_ref()),
        tagged(prover_address(), case_lovelace, case_out_ref()),
      ],
      burn(),
    ),
  )
}

test close_convicted_forfeiting_the_bond_is_refused() fail {
  closes(
    undefended(),
    close_tx(
      undefended(),
      after_window(),
      [
        tagged(
          validation_case_v1.treasury_address,
          bond_lovelace,
          bond_out_ref(),
        ),
        tagged(
          prover_address(),
          case_lovelace - fp_lovelace() - lock_lovelace,
          case_out_ref(),
        ),
        fraud_proof_output(fp_lovelace(), prover),
        case_lock(lock_lovelace),
      ],
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_fraud_proof_at_another_address_is_refused() fail {
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      Output {
        ..fraud_proof_output(fp_lovelace(), prover),
        address: address.from_script(bond_hash),
      },
      case_lovelace - fp_lovelace(),
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_fraud_proof_for_another_prover_is_refused() fail {
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      fraud_proof_output(fp_lovelace(), other_key),
      case_lovelace - fp_lovelace(),
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_fraud_proof_below_its_minimum_is_refused() fail {
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      fraud_proof_output(fp_lovelace() - 1, prover),
      case_lovelace - fp_lovelace() + 1,
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_fraud_proof_at_its_maximum_is_accepted() {
  let top = fp_lovelace() + env.max_dispute_move_fee_lovelace
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      fraud_proof_output(top, prover),
      case_lovelace - top,
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_fraud_proof_above_its_maximum_is_refused() fail {
  let top = fp_lovelace() + env.max_dispute_move_fee_lovelace + 1
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      fraud_proof_output(top, prover),
      case_lovelace - top,
      convicting_mint(),
    ),
  )
}

test close_convicted_with_another_token_in_the_fraud_proof_is_refused() fail {
  let fp_output = fraud_proof_output(fp_lovelace(), prover)
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      Output {
        ..fp_output,
        value: fp_output.value |> assets.add(other_key, "x", 1),
      },
      case_lovelace - fp_lovelace(),
      convicting_mint(),
    ),
  )
}

test close_convicted_with_a_short_remainder_is_refused() fail {
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      case_lovelace - fp_lovelace() - env.max_dispute_move_fee_lovelace - 1,
      convicting_mint(),
    ),
  )
}

/// With only the case and the bond spent, the case pays the fee, up to the
/// cap.
test close_convicted_paying_the_fee_from_the_case_is_accepted() {
  closes(
    undefended(),
    convicted_close_with(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      case_lovelace - fp_lovelace() - fee,
      convicting_mint(),
    ),
  )
}

test close_convicted_paying_the_fee_beside_a_wallet_input_from_the_case_is_refused() fail {
  let tx = convicted_close_with(
    after_window(),
    fraud_proof_output(fp_lovelace(), prover),
    case_lovelace - fp_lovelace() - fee,
    convicting_mint(),
  )
  let wallet = Input {
    output_reference: out_ref(h32_b, 5),
    output: Output {
      address: address.from_verification_key(other_key),
      value: assets.from_lovelace(5_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
  closes(
    undefended(),
    Transaction {
      ..tx,
      inputs: [case_input(undefended()), bond_input(), wallet],
    },
  )
}

test close_convicted_burning_the_case_nft_is_refused() fail {
  closes(
    undefended(),
    close_tx(
      undefended(),
      after_window(),
      [
        tagged(prover_address(), bond_lovelace, bond_out_ref()),
        tagged(prover_address(), case_lovelace - fp_lovelace(), case_out_ref()),
        fraud_proof_output(fp_lovelace(), prover),
      ],
      burn() |> assets.add(fraud_proof_policy, thread_name, 1),
    ),
  )
}

test close_convicted_locking_the_case_nft_elsewhere_is_refused() fail {
  closes(
    undefended(),
    convicted_close_locking(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      Output { ..case_lock(lock_lovelace), address: prover_address() },
      case_lovelace - fp_lovelace() - lock_lovelace,
      convicting_mint(),
    ),
  )
}

test close_convicted_locking_the_case_nft_with_another_token_is_refused() fail {
  closes(
    undefended(),
    convicted_close_locking(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      Output {
        ..case_lock(lock_lovelace),
        value: case_lock(lock_lovelace).value |> assets.add(other_key, "x", 1),
      },
      case_lovelace - fp_lovelace() - lock_lovelace,
      convicting_mint(),
    ),
  )
}

test close_convicted_locking_the_case_nft_in_the_fraud_proof_output_is_refused() fail {
  let fp_output = fraud_proof_output(fp_lovelace(), prover)
  closes(
    undefended(),
    close_tx(
      undefended(),
      after_window(),
      [
        tagged(prover_address(), bond_lovelace, bond_out_ref()),
        tagged(prover_address(), case_lovelace - fp_lovelace(), case_out_ref()),
        Output {
          ..fp_output,
          value: fp_output.value |> assets.add(case_policy, case_id(), 1),
        },
      ],
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_lock_at_its_maximum_is_accepted() {
  closes(
    undefended(),
    convicted_close_locking(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      case_lock(fp_lovelace()),
      case_lovelace - 2 * fp_lovelace(),
      convicting_mint(),
    ),
  )
}

test close_convicted_with_the_lock_above_its_maximum_is_refused() fail {
  closes(
    undefended(),
    convicted_close_locking(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      case_lock(fp_lovelace() + 1),
      case_lovelace - 2 * fp_lovelace() - 1,
      convicting_mint(),
    ),
  )
}

// ### Row: the defender won

test close_defender_won_forfeits_the_bond() {
  closes(defender_won(), standing_close(defender_won(), started))
}

test close_defender_won_refunding_the_bond_is_refused() fail {
  closes(
    defender_won(),
    close_tx(
      defender_won(),
      started,
      [
        tagged(prover_address(), bond_lovelace, bond_out_ref()),
        tagged(prover_address(), case_lovelace, case_out_ref()),
      ],
      burn(),
    ),
  )
}

test close_defender_won_minting_a_fraud_proof_is_refused() fail {
  closes(
    defender_won(),
    close_tx(
      defender_won(),
      started,
      [
        tagged(
          validation_case_v1.treasury_address,
          bond_lovelace,
          bond_out_ref(),
        ),
        tagged(prover_address(), case_lovelace - fp_lovelace(), case_out_ref()),
        fraud_proof_output(fp_lovelace(), prover),
      ],
      convicting_mint(),
    ),
  )
}

// ### Row: neither claim valid

test close_no_valid_claim_forfeits_the_bond_after_the_window() {
  closes(no_valid_claim(), standing_close(no_valid_claim(), after_window()))
}

test close_no_valid_claim_before_the_window_closes_is_refused() fail {
  closes(no_valid_claim(), standing_close(no_valid_claim(), after_window() - 1))
}

// ### Row: a thread still open at D_case

test close_with_an_open_thread_at_d_case_is_refused() fail {
  closes(
    one_thread_open(),
    standing_close(
      one_thread_open(),
      started + validation_dispute_v1.max_dispute_duration_milliseconds,
    ),
  )
}

test close_with_an_open_thread_after_d_case_forfeits_the_bond() {
  closes(
    one_thread_open(),
    standing_close(
      one_thread_open(),
      started + validation_dispute_v1.max_dispute_duration_milliseconds + 1,
    ),
  )
}

// ### Custody of the case NFT and the thread token

test close_keeping_the_case_nft_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction {
      ..tx,
      mint: assets.from_asset(thread_policy, thread_name, -1),
    },
  )
}

test close_keeping_the_thread_token_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction { ..tx, mint: assets.from_asset(case_policy, case_id(), -1) },
  )
}

test close_with_another_mint_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction { ..tx, mint: burn() |> assets.add(other_key, "x", 1) },
  )
}

test close_keeping_the_bond_marker_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction {
      ..tx,
      mint: assets.from_asset(thread_policy, thread_name, -1)
        |> assets.add(case_policy, case_id(), -1),
    },
  )
}

test close_convicted_burning_the_bond_marker_is_refused() fail {
  closes(
    undefended(),
    convicted_close_locking(
      after_window(),
      fraud_proof_output(fp_lovelace(), prover),
      Output {
        ..case_lock(lock_lovelace),
        value: assets.from_lovelace(lock_lovelace)
          |> assets.add(case_policy, case_id(), 1),
      },
      case_lovelace - fp_lovelace() - lock_lovelace,
      convicting_mint() |> assets.add(case_policy, bond_marker(), -1),
    ),
  )
}

test close_convicted_locking_the_case_nft_without_the_bond_marker_is_refused() fail {
  let tx = convicted_close_locking(
    after_window(),
    fraud_proof_output(fp_lovelace(), prover),
    Output {
      ..case_lock(lock_lovelace),
      value: assets.from_lovelace(lock_lovelace)
        |> assets.add(case_policy, case_id(), 1),
    },
    case_lovelace - fp_lovelace() - lock_lovelace,
    convicting_mint(),
  )
  let marker_out = Output {
    address: prover_address(),
    value: assets.from_lovelace(1_200_000)
      |> assets.add(case_policy, bond_marker(), 1),
    datum: NoDatum,
    reference_script: None,
  }
  closes(
    undefended(),
    Transaction { ..tx, outputs: list.concat(tx.outputs, [marker_out]) },
  )
}

// ### A decoy bond: a bond-shaped output naming the case, without its marker

test close_convicted_with_a_decoy_bond_is_refused() fail {
  closes(undefended(), with_decoy_bond(undefended(), convicted_close()))
}

test close_defender_won_with_a_decoy_bond_is_refused() fail {
  closes(
    defender_won(),
    with_decoy_bond(defender_won(), standing_close(defender_won(), started)),
  )
}

test close_no_valid_claim_with_a_decoy_bond_is_refused() fail {
  closes(
    no_valid_claim(),
    with_decoy_bond(
      no_valid_claim(),
      standing_close(no_valid_claim(), after_window()),
    ),
  )
}

test close_with_an_open_thread_after_d_case_with_a_decoy_bond_is_refused() fail {
  closes(
    one_thread_open(),
    with_decoy_bond(
      one_thread_open(),
      standing_close(
        one_thread_open(),
        started + validation_dispute_v1.max_dispute_duration_milliseconds + 1,
      ),
    ),
  )
}

/// The case spend alone refuses a decoy, whichever bond spend runs beside it.
test case_spend_alone_refuses_a_decoy_bond() fail {
  case_validator.main.spend(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    Some(defender_won()),
    close,
    case_out_ref(),
    with_decoy_bond(defender_won(), standing_close(defender_won(), started)),
  )
}

test close_with_a_bond_holding_the_marker_of_another_case_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction {
      ..tx,
      inputs: [
        case_input(defender_won()),
        bond_input_holding(
          bond_hash,
          bond_lovelace,
          bond_datum_of(case_id()),
          assets.from_asset(
            case_policy,
            validation_case_v1.bond_marker_name(h32_a),
            1,
          ),
        ),
      ],
    },
  )
}

// ### The bond

test close_with_a_bond_at_another_script_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction {
      ..tx,
      inputs: [
        case_input(defender_won()),
        bond_input_with(other_key, bond_lovelace, bond_datum_of(case_id())),
      ],
    },
  )
}

test close_with_a_bond_of_another_case_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction {
      ..tx,
      inputs: [
        case_input(defender_won()),
        bond_input_with(bond_hash, bond_lovelace, bond_datum_of(h32_a)),
      ],
    },
  )
}

test close_with_a_bond_below_the_bond_amount_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction {
      ..tx,
      inputs: [
        case_input(defender_won()),
        bond_input_with(bond_hash, bond_lovelace - 1, bond_datum_of(case_id())),
      ],
    },
  )
}

test close_paying_the_bond_short_is_refused() fail {
  closes(
    defender_won(),
    close_tx(
      defender_won(),
      started,
      [
        tagged(
          validation_case_v1.treasury_address,
          bond_lovelace - 1,
          bond_out_ref(),
        ),
        tagged(prover_address(), case_lovelace, case_out_ref()),
      ],
      burn(),
    ),
  )
}

test close_paying_the_bond_untagged_is_refused() fail {
  closes(
    defender_won(),
    close_tx(
      defender_won(),
      started,
      [
        tagged(
          validation_case_v1.treasury_address,
          bond_lovelace,
          case_out_ref(),
        ),
        tagged(prover_address(), case_lovelace, case_out_ref()),
      ],
      burn(),
    ),
  )
}

test close_paying_the_remainder_to_another_key_is_refused() fail {
  closes(
    defender_won(),
    close_tx(
      defender_won(),
      started,
      [
        tagged(
          validation_case_v1.treasury_address,
          bond_lovelace,
          bond_out_ref(),
        ),
        tagged(
          address.from_verification_key(other_key),
          case_lovelace,
          case_out_ref(),
        ),
      ],
      burn(),
    ),
  )
}

// ### The configuration

test close_without_the_configuration_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction { ..tx, reference_inputs: [hub_reference()] },
  )
}

test close_convicted_without_the_hub_is_refused() fail {
  let tx = convicted_close()
  closes(
    undefended(),
    Transaction { ..tx, reference_inputs: [config_reference()] },
  )
}

// ## Defend

const defender = #"e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3"

const active_policy =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const active_script =
  #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1"

const retired_policy =
  #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2"

const retired_script =
  #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

const node_lovelace = 50_000_000_000

const defense_lower = 1_060_000

fn defense_upper() -> Int {
  defense_lower + validation_dispute_v1.dispute_max_validity_range_milliseconds
}

fn trace_state(
  phase: validation_trace_v1.ValidationPhase,
  verdict: validation_trace_v1.ValidationVerdict,
) -> ValidationMachineStateV1 {
  ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h32_c,
    transaction_id: h32_c,
    transaction_commitment: h32_c,
    validation_context_hash: h32_c,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h32_c,
    phase,
    program_counter: 0,
    work_root: h32_c,
    execution_cpu: 0,
    execution_memory: 0,
    verdict,
    rejection_code_hash: if verdict == validation_trace_v1.Rejected {
      h32_b
    } else {
      #"0000000000000000000000000000000000000000000000000000000000000000"
    },
  }
}

/// A defender state at S that is still running.
fn running() -> ValidationMachineStateV1 {
  trace_state(validation_trace_v1.Cek, validation_trace_v1.Pending)
}

/// A one-step trace `[initial, last]`: its root and the proof of leaf 1.
fn one_step_trace(
  last: ValidationMachineStateV1,
) -> (ByteArray, ValidationTraceProof) {
  let last_hash = validation_trace_v1.hash_machine_state(last)
  let sibling = validation_trace_v1.trace_leaf_hash(h32_c)
  (
    validation_trace_v1.trace_branch_hash(
      sibling,
      validation_trace_v1.trace_leaf_hash(last_hash),
    ),
    ValidationTraceProof {
      state_index: 1,
      state_hash: last_hash,
      siblings: [sibling],
    },
  )
}

/// A one-step case: S = 1, so a thread opens ready for the step.
fn defendable() -> CaseV1 {
  validation_case_v1.open_case(
    header_hash,
    event_key(),
    h32_a,
    1,
    h32_b,
    prover,
    h32_c,
    started,
  )
}

fn defend_with(
  state: ValidationMachineStateV1,
  defenders_proof: mpf.Proof,
) -> CaseRedeemerV1 {
  let (defender_root, defender_state_proof) = one_step_trace(state)
  Defend {
    case_output_index: 0,
    thread_output_index: 1,
    active_node_input_index: 1,
    defender_vkh: defender,
    defenders_proof,
    defender_root,
    defender_state: state,
    defender_state_proof,
  }
}

fn defend_of(state: ValidationMachineStateV1) -> CaseRedeemerV1 {
  defend_with(state, [])
}

fn node_out_ref() -> OutputReference {
  out_ref(h32_b, 3)
}

fn node_holding(
  script: ByteArray,
  policy: ByteArray,
  key: ByteArray,
  hold: Option<Int>,
  lovelace: Int,
) -> Output {
  let datum: active_operators.Datum = linked_list.Element {
    data: linked_list.Node(
      active_operators.NodeData {
        bond_unlock_time: hold,
        inactivity_strikes: 1,
      },
    ),
    link: None,
  }
  Output {
    address: address.from_script(script),
    value: assets.from_lovelace(lovelace)
      |> assets.add(
          policy,
          bytearray.concat(active_operators.node_asset_name_prefix, key),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn active_node(hold: Option<Int>) -> Output {
  node_holding(active_script, active_policy, defender, hold, node_lovelace)
}

fn node_input_of(node: Output) -> Input {
  Input { output_reference: node_out_ref(), output: node }
}

fn hold_after(case: CaseV1, old: Option<Int>) -> Int {
  validation_case_v1.bond_hold_after_defense(old, case)
}

fn thread_token() -> assets.Value {
  assets.from_asset(
    case_policy,
    validation_case_v1.thread_token_name(case_id(), defender),
    1,
  )
}

fn thread_state(case: CaseV1, state: ValidationMachineStateV1) -> CaseThreadV1 {
  let (root, proof) = one_step_trace(state)
  validation_case_v1.open_thread(
    case,
    case_id(),
    defender,
    root,
    proof.state_hash,
    defense_lower,
    defense_upper(),
  )
}

/// A thread's datum: the challenger is its fraud prover.
fn step_of(thread: CaseThreadV1) -> Data {
  let datum: Data = ct.StepDatum { fraud_prover: prover, data: Some(thread) }
  datum
}

fn thread_datum(case: CaseV1, state: ValidationMachineStateV1) -> Data {
  step_of(thread_state(case, state))
}

fn thread_output(datum: Data, lovelace: Int) -> Output {
  Output {
    address: address.from_script(config().game_v1),
    value: assets.merge(assets.from_lovelace(lovelace), thread_token()),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn case_output(case: CaseV1, lovelace: Int) -> Output {
  let datum: Data = case
  Output {
    address: address.from_script(case_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(thread_policy, thread_name, 1)
      |> assets.add(case_policy, case_id(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn defended_by(case: CaseV1) -> CaseV1 {
  expect Some(next) = validation_case_v1.defend(case, defender, [])
  next
}

fn defense_tx(
  case: CaseV1,
  old_hold: Option<Int>,
  outputs: List<Output>,
  mint: assets.Value,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [case_input(case), node_input_of(active_node(old_hold))],
    reference_inputs: [config_reference(), hub_reference()],
    outputs,
    mint,
    fee,
    validity_range: interval.between(defense_lower, defense_upper()),
    extra_signatories: [defender],
  }
}

/// The honest defense of `case` with `state` at S, by an operator whose bond
/// was held until `old_hold`.
fn defense(
  case: CaseV1,
  state: ValidationMachineStateV1,
  old_hold: Option<Int>,
) -> Transaction {
  defense_tx(
    case,
    old_hold,
    [
      case_output(defended_by(case), case_lovelace),
      thread_output(
        thread_datum(case, state),
        validation_case_v1.thread_floor_lovelace,
      ),
      active_node(Some(hold_after(case, old_hold))),
    ],
    thread_token(),
  )
}

fn case_defends(case: CaseV1, redeemer: CaseRedeemerV1, tx: Transaction) -> Bool {
  case_validator.main.spend(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    Some(case),
    redeemer,
    case_out_ref(),
    tx,
  )
}

/// The defense's three scripts: the case spend, the thread-token mint and
/// the defender's active node.
fn defends(case: CaseV1, redeemer: CaseRedeemerV1, tx: Transaction) -> Bool {
  and {
    case_defends(case, redeemer, tx),
    case_mint(redeemer, tx),
    active_operators_validator.spend.spend(
      active_policy,
      hub_policy,
      None,
      UpdateBondHoldNewDefense {
        active_operator: defender,
        active_node_input_index: 1,
        active_node_output_index: 2,
      },
      node_out_ref(),
      tx,
    ),
  }
}

/// `tx` with its outputs at `index` replaced by `output`.
fn with_output(tx: Transaction, index: Int, output: Output) -> Transaction {
  Transaction {
    ..tx,
    outputs: list.indexed_map(
      tx.outputs,
      fn(i, current) {
        if i == index {
          output
        } else {
          current
        }
      },
    ),
  }
}

fn honest_defense() -> Transaction {
  defense(defendable(), running(), None)
}

test defend_opens_a_thread_and_holds_the_bond() {
  defends(defendable(), defend_of(running()), honest_defense())
}

test defend_with_an_accepted_terminal_is_accepted() {
  let accepted = trace_state(
    validation_trace_v1.Terminal,
    validation_trace_v1.Accepted,
  )
  defends(
    defendable(),
    defend_of(accepted),
    defense(defendable(), accepted, None),
  )
}

test defend_raises_a_shorter_hold_from_another_case() {
  let old = Some(started)
  defends(
    defendable(),
    defend_of(running()),
    defense(defendable(), running(), old),
  )
}

test defend_keeps_a_longer_hold_from_another_case() {
  let old = Some(hold_after(defendable(), None) + 1)
  and {
    hold_after(defendable(), old) == hold_after(defendable(), None) + 1,
    defends(
      defendable(),
      defend_of(running()),
      defense(defendable(), running(), old),
    ),
  }
}

test defend_at_the_end_of_the_defender_clock_is_accepted() {
  let lower = started + validation_dispute_v1.dispute_side_clock_milliseconds
  let tx = honest_defense()
  let (root, proof) = one_step_trace(running())
  let datum = step_of(
    validation_case_v1.open_thread(
      defendable(),
      case_id(),
      defender,
      root,
      proof.state_hash,
      lower,
      lower + validation_dispute_v1.dispute_max_validity_range_milliseconds,
    ),
  )
  defends(
    defendable(),
    defend_of(running()),
    with_output(
      Transaction {
        ..tx,
        validity_range: interval.between(
          lower,
          lower + validation_dispute_v1.dispute_max_validity_range_milliseconds,
        ),
      },
      1,
      thread_output(datum, validation_case_v1.thread_floor_lovelace),
    ),
  )
}

test defend_after_the_defender_clock_is_refused() fail {
  let lower = started + validation_dispute_v1.dispute_side_clock_milliseconds + 1
  let tx = honest_defense()
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction {
      ..tx,
      validity_range: interval.between(
        lower,
        lower + validation_dispute_v1.dispute_max_validity_range_milliseconds,
      ),
    },
  )
}

test defend_with_a_validity_range_longer_than_a_move_is_refused() fail {
  let tx = honest_defense()
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction {
      ..tx,
      validity_range: interval.between(defense_lower, defense_upper() + 1),
    },
  )
}

test defend_topping_up_the_case_is_accepted() {
  defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      0,
      case_output(defended_by(defendable()), case_lovelace + 1),
    ),
  )
}

// ### Who may defend

test defend_unsigned_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction { ..honest_defense(), extra_signatories: [other_key] },
  )
}

test defend_without_an_active_node_is_refused() fail {
  let wallet = Output {
    address: address.from_verification_key(defender),
    value: assets.from_lovelace(node_lovelace),
    datum: NoDatum,
    reference_script: None,
  }
  let tx = honest_defense()
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction {
      ..tx,
      inputs: [case_input(defendable()), node_input_of(wallet)],
    },
  )
}

test defend_by_a_retired_operator_is_refused() fail {
  let retired = node_holding(
    retired_script,
    retired_policy,
    defender,
    None,
    node_lovelace,
  )
  let tx = honest_defense()
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      Transaction {
        ..tx,
        inputs: [case_input(defendable()), node_input_of(retired)],
      },
      2,
      node_holding(
        retired_script,
        retired_policy,
        defender,
        Some(hold_after(defendable(), None)),
        node_lovelace,
      ),
    ),
  )
}

test defend_with_the_active_node_of_another_operator_is_refused() fail {
  let other = node_holding(
    active_script,
    active_policy,
    other_key,
    None,
    node_lovelace,
  )
  let tx = honest_defense()
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      Transaction {
        ..tx,
        inputs: [case_input(defendable()), node_input_of(other)],
      },
      2,
      node_holding(
        active_script,
        active_policy,
        other_key,
        Some(hold_after(defendable(), None)),
        node_lovelace,
      ),
    ),
  )
}

test defend_a_second_time_by_the_same_operator_is_refused() fail {
  let case = defended_by(defendable())
  case_defends(
    case,
    defend_of(running()),
    Transaction {
      ..honest_defense(),
      inputs: [case_input(case), node_input_of(active_node(None))],
    },
  )
}

test defend_without_the_hub_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction { ..honest_defense(), reference_inputs: [config_reference()] },
  )
}

// ### The defense rule

test defend_with_the_challenger_terminal_is_refused() fail {
  let rejected = trace_state(
    validation_trace_v1.Terminal,
    validation_trace_v1.Rejected,
  )
  let case = CaseV1 {
    ..defendable(),
    challenger_terminal_hash: validation_trace_v1.hash_machine_state(rejected),
  }
  case_defends(case, defend_of(rejected), defense(case, rejected, None))
}

test defend_with_a_rejected_terminal_is_refused() fail {
  let rejected = trace_state(
    validation_trace_v1.Terminal,
    validation_trace_v1.Rejected,
  )
  case_defends(
    defendable(),
    defend_of(rejected),
    defense(defendable(), rejected, None),
  )
}

// ### The bond hold

test defend_with_a_shorter_hold_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      2,
      active_node(Some(hold_after(defendable(), None) - 1)),
    ),
  )
}

test defend_with_a_longer_hold_than_the_case_needs_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      2,
      active_node(Some(hold_after(defendable(), None) + 1)),
    ),
  )
}

test defend_without_the_node_continuation_is_refused() fail {
  let tx = honest_defense()
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction { ..tx, outputs: list.take(tx.outputs, 2) },
  )
}

// ### The thread

test defend_missing_the_reserve_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      thread_output(
        thread_datum(defendable(), running()),
        validation_case_v1.thread_floor_lovelace - 1,
      ),
    ),
  )
}

test defend_with_the_thread_at_another_script_is_refused() fail {
  let thread = thread_output(
    thread_datum(defendable(), running()),
    validation_case_v1.thread_floor_lovelace,
  )
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      Output { ..thread, address: address.from_script(config().boundary_v1) },
    ),
  )
}

test defend_with_a_thread_clock_left_uncharged_is_refused() fail {
  let (root, proof) = one_step_trace(running())
  let datum = step_of(
    validation_case_v1.open_thread(
      defendable(),
      case_id(),
      defender,
      root,
      proof.state_hash,
      started,
      defense_upper(),
    ),
  )
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      thread_output(datum, validation_case_v1.thread_floor_lovelace),
    ),
  )
}

test defend_with_a_thread_of_another_step_count_is_refused() fail {
  let thread = thread_state(defendable(), running())
  let datum = step_of(CaseThreadV1 { ..thread, step_count: 2, high_index: 2 })
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      thread_output(datum, validation_case_v1.thread_floor_lovelace),
    ),
  )
}

test defend_with_a_bare_thread_datum_is_refused() fail {
  let datum: Data = thread_state(defendable(), running())
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      thread_output(datum, validation_case_v1.thread_floor_lovelace),
    ),
  )
}

test defend_with_another_fraud_prover_in_the_thread_is_refused() fail {
  let datum: Data = ct.StepDatum {
    fraud_prover: other_key,
    data: Some(thread_state(defendable(), running())),
  }
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      thread_output(datum, validation_case_v1.thread_floor_lovelace),
    ),
  )
}

/// The defense with its thread header set to `header`.
fn defense_with_header(header: ThreadHeaderV1) -> Transaction {
  let thread = thread_state(defendable(), running())
  with_output(
    honest_defense(),
    1,
    thread_output(
      step_of(CaseThreadV1 { ..thread, header }),
      validation_case_v1.thread_floor_lovelace,
    ),
  )
}

/// The thread header is the case's start and the defender's key.
test defend_with_the_header_of_the_case_and_defender_is_accepted() {
  case_defends(
    defendable(),
    defend_of(running()),
    defense_with_header(
      ThreadHeaderV1 { started_at: started, defender_vkh: defender },
    ),
  )
}

test defend_with_another_header_start_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    defense_with_header(
      ThreadHeaderV1 { started_at: started + 1, defender_vkh: defender },
    ),
  )
}

test defend_with_another_header_defender_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    defense_with_header(
      ThreadHeaderV1 { started_at: started, defender_vkh: other_key },
    ),
  )
}

test defend_with_another_token_in_the_thread_is_refused() fail {
  let thread = thread_output(
    thread_datum(defendable(), running()),
    validation_case_v1.thread_floor_lovelace,
  )
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      1,
      Output {
        ..thread,
        value: assets.add(thread.value, thread_policy, thread_name, 1),
      },
    ),
  )
}

// ### The case continuation

test defend_leaving_the_case_datum_unchanged_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(honest_defense(), 0, case_output(defendable(), case_lovelace)),
  )
}

test defend_taking_lovelace_from_the_case_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      0,
      case_output(defended_by(defendable()), case_lovelace - 1),
    ),
  )
}

test defend_moving_the_case_elsewhere_is_refused() fail {
  let moved = case_output(defended_by(defendable()), case_lovelace)
  case_defends(
    defendable(),
    defend_of(running()),
    with_output(
      honest_defense(),
      0,
      Output { ..moved, address: address.from_script(config().game_v1) },
    ),
  )
}

// ### Thread tokens are minted only by Defend, one at a time

test defend_minting_no_thread_token_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction { ..honest_defense(), mint: assets.zero },
  )
}

test defend_minting_two_thread_tokens_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction {
      ..honest_defense(),
      mint: assets.add(
        thread_token(),
        case_policy,
        validation_case_v1.thread_token_name(case_id(), other_key),
        1,
      ),
    },
  )
}

test defend_minting_a_doubled_thread_token_is_refused() fail {
  case_defends(
    defendable(),
    defend_of(running()),
    Transaction {
      ..honest_defense(),
      mint: assets.merge(thread_token(), thread_token()),
    },
  )
}

test thread_token_mint_without_a_case_input_is_refused() fail {
  let tx = honest_defense()
  case_mint(
    defend_of(running()),
    Transaction { ..tx, inputs: [node_input_of(active_node(None))] },
  )
}

test thread_token_mint_beside_a_case_close_is_refused() fail {
  let tx = standing_close(defender_won(), started)
  closes(
    defender_won(),
    Transaction { ..tx, mint: assets.merge(tx.mint, thread_token()) },
  )
}

// ## The thread close

fn thread_out_ref() -> OutputReference {
  out_ref(h32_c, 7)
}

const thread_lovelace = validation_case_v1.thread_floor_lovelace

/// The fee a lone close may take from the thread: the case and the thread are
/// its only inputs.
fn close_fee() -> Int {
  if fee < env.max_dispute_move_fee_lovelace {
    fee
  } else {
    env.max_dispute_move_fee_lovelace
  }
}

/// A one-step thread, its turn ReadyForOneStep: the challenger's to move.
fn ready_thread() -> CaseThreadV1 {
  thread_state(defendable(), running())
}

/// A four-step thread, its turn AwaitingOperator: the defender's to move.
fn awaiting_defender() -> CaseThreadV1 {
  thread_state(opened(), running())
}

/// The first lower bound at which the side to move has run out of clock.
fn lapse_of(thread: CaseThreadV1) -> Int {
  when thread.turn is {
    validation_dispute_v1.AwaitingOperator { .. } ->
      thread.turn_started_at + thread.defender_clock_ms + 1
    _ -> thread.turn_started_at + thread.challenger_clock_ms + 1
  }
}

fn thread_input_at(script: ByteArray, datum: Data) -> Input {
  Input {
    output_reference: thread_out_ref(),
    output: Output {
      address: address.from_script(script),
      value: assets.merge(assets.from_lovelace(thread_lovelace), thread_token()),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn awarded_datum() -> Data {
  let datum: Data = ct.StepDatum {
    fraud_prover: prover,
    data: Some(validation_resolution_v1.winning_resolution()),
  }
  datum
}

fn thread_close_of(outcome: DisputeWinner) -> CaseRedeemerV1 {
  ThreadClose {
    case_output_index: 0,
    thread_input_index: 1,
    outcome,
    defender_vkh: defender,
  }
}

fn receipt_token() -> assets.Value {
  assets.from_asset(
    config().award_v1,
    validation_case_v1.receipt_token_name(case_id(), defender),
    1,
  )
}

fn receipt_output(case: CaseV1, outcome: DisputeWinner, lovelace: Int) -> Output {
  expect Some(receipt) = validation_case_v1.lost_defense_receipt(
    case,
    case_id(),
    defender,
    outcome,
  )
  let datum: Data = receipt
  Output {
    address: address.from_script(config().award_v1),
    value: assets.merge(assets.from_lovelace(lovelace), receipt_token()),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn thread_burn() -> assets.Value {
  assets.negate(thread_token())
}

/// A close of the thread held at `holder` into `case` with `outcome`.
fn thread_close_tx(
  case: CaseV1,
  holder: Input,
  outcome: DisputeWinner,
  lower: Int,
  outputs: List<Output>,
  mint: assets.Value,
) -> Transaction {
  let redeemer: Data = thread_close_of(outcome)
  Transaction {
    ..transaction.placeholder,
    inputs: [case_input(case), holder],
    reference_inputs: [config_reference()],
    outputs,
    mint,
    fee,
    validity_range: interval.between(
      lower,
      lower + validation_dispute_v1.dispute_max_validity_range_milliseconds,
    ),
    redeemers: [Pair(Spend(case_out_ref()), redeemer)],
  }
}

/// The honest close of `case` with `outcome`: the case continues closed, a
/// lost thread funds its receipt with `fp`, and the rest of the thread, less
/// the fee, goes to the outcome's payee.
fn honest_thread_close(
  case: CaseV1,
  holder: Input,
  outcome: DisputeWinner,
  lower: Int,
) -> Transaction {
  let case_out = case_output(
    validation_case_v1.close_thread(case, outcome),
    case_lovelace,
  )
  let payee = validation_case_v1.thread_leftover_address(
    case,
    defender,
    outcome,
  )
  when outcome is {
    DefenderWins ->
      thread_close_tx(
        case,
        holder,
        outcome,
        lower,
        [
          case_out,
          tagged(payee, thread_lovelace - close_fee(), thread_out_ref()),
        ],
        thread_burn(),
      )
    _ ->
      thread_close_tx(
        case,
        holder,
        outcome,
        lower,
        [
          case_out,
          tagged(
            payee,
            thread_lovelace - fp_lovelace() - close_fee(),
            thread_out_ref(),
          ),
          receipt_output(case, outcome, fp_lovelace()),
        ],
        assets.merge(thread_burn(), receipt_token()),
      )
  }
}

fn case_closes_thread(
  case: CaseV1,
  outcome: DisputeWinner,
  tx: Transaction,
) -> Bool {
  case_validator.main.spend(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    Some(case),
    thread_close_of(outcome),
    case_out_ref(),
    tx,
  )
}

fn timeout_spends(thread: CaseThreadV1, tx: Transaction) -> Bool {
  timeout_validator.main.spend(
    case_policy,
    expiry_hash,
    Some(ct.StepDatum { fraud_prover: prover, data: Some(thread) }),
    timeout_validator.CloseTimedOutThread {
      input_index: 1,
      case_input_index: 0,
    },
    thread_out_ref(),
    tx,
  )
}

fn award_spends(tx: Transaction) -> Bool {
  award_validator.main.spend(
    case_policy,
    hub_policy,
    expiry_hash,
    Some(awarded_datum()),
    CloseAwardedThread { input_index: 1, case_input_index: 0 },
    thread_out_ref(),
    tx,
  )
}

fn award_mints(tx: Transaction) -> Bool {
  award_validator.main.mint(
    case_policy,
    hub_policy,
    expiry_hash,
    MintReceipt { case_input_index: 0 },
    config().award_v1,
    tx,
  )
}

fn timed_out(thread: CaseThreadV1) -> Input {
  thread_input_at(config().timeout_v1, step_of(thread))
}

fn awarded() -> Input {
  thread_input_at(config().award_v1, awarded_datum())
}

fn open_case() -> CaseV1 {
  defended_by(defendable())
}

/// The defender's timed-out close with the thread at the timeout validator.
fn defender_timed_out_close() -> Transaction {
  honest_thread_close(
    open_case(),
    timed_out(ready_thread()),
    DefenderWins,
    lapse_of(ready_thread()),
  )
}

/// The challenger's close of an awarded thread.
fn awarded_close() -> Transaction {
  honest_thread_close(open_case(), awarded(), ChallengerWins, started)
}

// ### Each holder closes with its outcome

test thread_close_after_the_challenger_ran_out_of_clock_is_accepted() {
  let tx = defender_timed_out_close()
  and {
    case_closes_thread(open_case(), DefenderWins, tx),
    timeout_spends(ready_thread(), tx),
    case_mint(thread_close_of(DefenderWins), tx),
  }
}

test thread_close_after_the_defender_ran_out_of_clock_is_accepted() {
  let thread = awaiting_defender()
  let tx = honest_thread_close(
    open_case(),
    timed_out(thread),
    ChallengerWins,
    lapse_of(thread),
  )
  and {
    case_closes_thread(open_case(), ChallengerWins, tx),
    timeout_spends(thread, tx),
    case_mint(thread_close_of(ChallengerWins), tx),
    award_mints(tx),
  }
}

test thread_close_of_an_awarded_thread_is_accepted() {
  let tx = awarded_close()
  and {
    case_closes_thread(open_case(), ChallengerWins, tx),
    award_spends(tx),
    case_mint(thread_close_of(ChallengerWins), tx),
    award_mints(tx),
  }
}

test thread_close_before_the_clock_runs_out_is_refused() fail {
  let thread = ready_thread()
  timeout_spends(
    thread,
    honest_thread_close(
      open_case(),
      timed_out(thread),
      DefenderWins,
      lapse_of(thread) - 1,
    ),
  )
}

test timed_out_thread_closed_with_the_other_outcome_is_refused() fail {
  let thread = ready_thread()
  timeout_spends(
    thread,
    honest_thread_close(
      open_case(),
      timed_out(thread),
      ChallengerWins,
      lapse_of(thread),
    ),
  )
}

test awarded_thread_closed_as_a_defender_win_is_refused() fail {
  award_spends(
    honest_thread_close(open_case(), awarded(), DefenderWins, started),
  )
}

test awarded_thread_closed_as_neither_is_refused() fail {
  award_spends(
    honest_thread_close(open_case(), awarded(), NeitherClaimValid, started),
  )
}

test awarded_thread_close_under_another_case_redeemer_is_refused() fail {
  let tx = awarded_close()
  let redeemer: Data = close
  award_spends(
    Transaction { ..tx, redeemers: [Pair(Spend(case_out_ref()), redeemer)] },
  )
}

test awarded_thread_close_without_burning_its_token_is_refused() fail {
  let tx = awarded_close()
  award_spends(Transaction { ..tx, mint: receipt_token() })
}

test thread_close_with_neither_claim_valid_pays_the_treasury() {
  let tx = honest_thread_close(
    open_case(),
    awarded(),
    NeitherClaimValid,
    started,
  )
  and {
    case_closes_thread(open_case(), NeitherClaimValid, tx),
    award_mints(tx),
  }
}

// ### The case side of the close

test thread_close_of_a_case_with_no_open_thread_is_refused() fail {
  let case = CaseV1 { ..open_case(), open_threads: 0 }
  case_closes_thread(
    case,
    ChallengerWins,
    honest_thread_close(open_case(), awarded(), ChallengerWins, started),
  )
}

test thread_close_from_the_game_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    honest_thread_close(
      open_case(),
      thread_input_at(config().game_v1, step_of(ready_thread())),
      DefenderWins,
      started,
    ),
  )
}

test thread_close_from_the_boundary_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    honest_thread_close(
      open_case(),
      thread_input_at(config().boundary_v1, step_of(ready_thread())),
      DefenderWins,
      started,
    ),
  )
}

test thread_close_naming_another_defender_is_refused() fail {
  case_validator.main.spend(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    Some(open_case()),
    ThreadClose {
      case_output_index: 0,
      thread_input_index: 1,
      outcome: DefenderWins,
      defender_vkh: other_key,
    },
    case_out_ref(),
    defender_timed_out_close(),
  )
}

test thread_close_minting_beyond_the_burn_is_refused() fail {
  let tx = defender_timed_out_close()
  case_closes_thread(
    open_case(),
    DefenderWins,
    Transaction { ..tx, mint: assets.add(tx.mint, case_policy, "x", 1) },
  )
}

test thread_close_without_the_burn_is_refused() fail {
  let tx = defender_timed_out_close()
  case_closes_thread(
    open_case(),
    DefenderWins,
    Transaction { ..tx, mint: assets.zero },
  )
}

test thread_close_leaving_the_case_open_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      0,
      case_output(open_case(), case_lovelace),
    ),
  )
}

test thread_close_recording_another_outcome_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      0,
      case_output(
        validation_case_v1.close_thread(open_case(), ChallengerWins),
        case_lovelace,
      ),
    ),
  )
}

test thread_close_taking_lovelace_from_the_case_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      0,
      case_output(
        validation_case_v1.close_thread(open_case(), DefenderWins),
        case_lovelace - 1,
      ),
    ),
  )
}

test thread_close_moving_the_case_is_refused() fail {
  let moved = case_output(
    validation_case_v1.close_thread(open_case(), DefenderWins),
    case_lovelace,
  )
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      0,
      Output { ..moved, address: address.from_script(config().timeout_v1) },
    ),
  )
}

// ### The leftover

test thread_close_paying_the_leftover_to_another_key_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      1,
      tagged(
        address.from_verification_key(other_key),
        thread_lovelace - close_fee(),
        thread_out_ref(),
      ),
    ),
  )
}

test thread_close_paying_the_defender_one_lovelace_short_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      1,
      tagged(
        address.from_verification_key(defender),
        thread_lovelace - close_fee() - 1,
        thread_out_ref(),
      ),
    ),
  )
}

test thread_close_tagging_the_leftover_with_another_out_ref_is_refused() fail {
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      defender_timed_out_close(),
      1,
      tagged(
        address.from_verification_key(defender),
        thread_lovelace - close_fee(),
        case_out_ref(),
      ),
    ),
  )
}

test thread_close_paying_a_challenger_win_to_the_defender_is_refused() fail {
  case_closes_thread(
    open_case(),
    ChallengerWins,
    with_output(
      awarded_close(),
      1,
      tagged(
        address.from_verification_key(defender),
        thread_lovelace - fp_lovelace() - close_fee(),
        thread_out_ref(),
      ),
    ),
  )
}

fn wallet_input() -> Input {
  Input {
    output_reference: out_ref(h32_c, 8),
    output: Output {
      address: address.from_verification_key(other_key),
      value: assets.from_lovelace(5_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

test thread_close_taking_the_fee_beside_another_input_is_refused() fail {
  let tx = defender_timed_out_close()
  case_closes_thread(
    open_case(),
    DefenderWins,
    Transaction { ..tx, inputs: list.concat(tx.inputs, [wallet_input()]) },
  )
}

test thread_close_beside_another_input_paying_the_whole_thread_is_accepted() {
  let tx = defender_timed_out_close()
  case_closes_thread(
    open_case(),
    DefenderWins,
    with_output(
      Transaction { ..tx, inputs: list.concat(tx.inputs, [wallet_input()]) },
      1,
      tagged(
        address.from_verification_key(defender),
        thread_lovelace,
        thread_out_ref(),
      ),
    ),
  )
}

// ### The receipt

fn with_receipt_lovelace(lovelace: Int) -> Transaction {
  let tx = awarded_close()
  let paid = with_output(
    tx,
    2,
    receipt_output(open_case(), ChallengerWins, lovelace),
  )
  with_output(
    paid,
    1,
    tagged(
      prover_address(),
      thread_lovelace - lovelace - close_fee(),
      thread_out_ref(),
    ),
  )
}

test thread_close_funding_the_receipt_up_to_one_fee_more_is_accepted() {
  case_closes_thread(
    open_case(),
    ChallengerWins,
    with_receipt_lovelace(fp_lovelace() + env.max_dispute_move_fee_lovelace),
  )
}

test thread_close_funding_the_receipt_beyond_one_fee_more_is_refused() fail {
  case_closes_thread(
    open_case(),
    ChallengerWins,
    with_receipt_lovelace(fp_lovelace() + env.max_dispute_move_fee_lovelace + 1),
  )
}

test thread_close_funding_the_receipt_below_the_output_is_refused() fail {
  case_closes_thread(
    open_case(),
    ChallengerWins,
    with_receipt_lovelace(fp_lovelace() - 1),
  )
}

test thread_close_without_the_receipt_output_is_refused() fail {
  let tx = awarded_close()
  case_closes_thread(
    open_case(),
    ChallengerWins,
    Transaction { ..tx, outputs: list.take(tx.outputs, 2) },
  )
}

test thread_close_without_minting_the_receipt_is_refused() fail {
  let tx = awarded_close()
  case_closes_thread(
    open_case(),
    ChallengerWins,
    Transaction { ..tx, mint: thread_burn() },
  )
}

test thread_close_with_a_defender_win_minting_a_receipt_is_refused() fail {
  let tx = defender_timed_out_close()
  case_closes_thread(
    open_case(),
    DefenderWins,
    Transaction {
      ..tx,
      outputs: list.concat(
        tx.outputs,
        [receipt_output(open_case(), ChallengerWins, fp_lovelace())],
      ),
      mint: assets.merge(tx.mint, receipt_token()),
    },
  )
}

test thread_close_with_the_receipt_elsewhere_is_refused() fail {
  let receipt = receipt_output(open_case(), ChallengerWins, fp_lovelace())
  case_closes_thread(
    open_case(),
    ChallengerWins,
    with_output(
      awarded_close(),
      2,
      Output { ..receipt, address: address.from_script(config().timeout_v1) },
    ),
  )
}

test thread_close_with_a_receipt_of_another_outcome_is_refused() fail {
  case_closes_thread(
    open_case(),
    ChallengerWins,
    with_output(
      awarded_close(),
      2,
      receipt_output(open_case(), NeitherClaimValid, fp_lovelace()),
    ),
  )
}

test receipt_mint_without_a_case_input_is_refused() fail {
  let tx = awarded_close()
  award_mints(Transaction { ..tx, inputs: [awarded()] })
}
