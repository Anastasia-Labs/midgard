//// The case validator and its configuration, driven through the deployed
//// validators: the configuration NFT, the case NFT mint, and the case close in
//// each of its rows, with the challenger bond spent beside it.

use aiken/interval
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use cardano/script_context.{ScriptContext, Spending}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
}
use env
use fraud_proofs/validation_trace/case_config_v1 as config_validator
use fraud_proofs/validation_trace/case_v1 as case_validator
use midgard/dispute_bond_v1.{DisputeBondV1, SettleWithCase}
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/validation_case_config_v1.{CaseConfigV1, config_asset_name}
use midgard/validation_case_v1.{
  BurnThreadToken, CaseClose, CaseRedeemerV1, CaseV1, VerifySource,
}
use midgard/validation_dispute_v1

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

// ## Reference inputs

fn config() -> CaseConfigV1 {
  CaseConfigV1 {
    source_v1: source_hash,
    game_v1: #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2",
    boundary_v1: #"d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3",
    award_v1: #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4",
    timeout_v1: #"d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5",
    dispute_bond_v1: bond_hash,
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
    verify_source_mint_tx(
      opener_at(source_hash),
      assets.from_asset(case_policy, case_id(), 1),
    ),
  )
}

test case_nft_mint_with_an_opener_of_another_script_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(bond_hash),
      assets.from_asset(case_policy, case_id(), 1),
    ),
  )
}

test case_nft_mint_of_two_tokens_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(source_hash),
      assets.from_asset(case_policy, case_id(), 2),
    ),
  )
}

test case_nft_mint_of_two_names_is_refused() fail {
  case_mint(
    verify_source,
    verify_source_mint_tx(
      opener_at(source_hash),
      assets.from_asset(case_policy, case_id(), 1)
        |> assets.add(case_policy, h32_c, 1),
    ),
  )
}

test case_nft_mint_without_the_configuration_is_refused() fail {
  case_mint(
    verify_source,
    Transaction {
      ..verify_source_mint_tx(
        opener_at(source_hash),
        assets.from_asset(case_policy, case_id(), 1),
      ),
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
      ..verify_source_mint_tx(
        opener_at(source_hash),
        assets.from_asset(case_policy, case_id(), 1),
      ),
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

fn thread_holder(
  credential: address.PaymentCredential,
  value: assets.Value,
) -> Input {
  Input {
    output_reference: out_ref(h32_b, 3),
    output: Output {
      address: Address {
        payment_credential: credential,
        stake_credential: None,
      },
      value,
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn held_thread_token() -> assets.Value {
  assets.from_lovelace(2_000_000)
    |> assets.add(case_policy, case_thread_name, 1)
}

fn thread_burn_tx(holder: Input, mint: assets.Value) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [holder],
    reference_inputs: [config_reference()],
    mint,
  }
}

fn burns_thread_token_held_at(script_hash: ByteArray) -> Bool {
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    thread_burn_tx(
      thread_holder(Script(script_hash), held_thread_token()),
      assets.from_asset(case_policy, case_thread_name, -1),
    ),
  )
}

test thread_token_burns_from_each_thread_script() {
  and {
    burns_thread_token_held_at(config().game_v1),
    burns_thread_token_held_at(config().boundary_v1),
    burns_thread_token_held_at(config().timeout_v1),
    burns_thread_token_held_at(config().award_v1),
  }
}

test thread_token_burn_from_the_source_script_is_refused() fail {
  burns_thread_token_held_at(source_hash)
}

test thread_token_burn_from_the_bond_script_is_refused() fail {
  burns_thread_token_held_at(bond_hash)
}

test thread_token_burn_from_the_case_script_is_refused() fail {
  burns_thread_token_held_at(case_policy)
}

test thread_token_burn_from_a_key_is_refused() fail {
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    thread_burn_tx(
      thread_holder(VerificationKey(prover), held_thread_token()),
      assets.from_asset(case_policy, case_thread_name, -1),
    ),
  )
}

test thread_token_burn_of_a_token_the_input_does_not_hold_is_refused() fail {
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    thread_burn_tx(
      thread_holder(Script(config().game_v1), held_thread_token()),
      assets.from_asset(case_policy, h32_a, -1),
    ),
  )
}

test thread_token_burn_of_two_tokens_is_refused() fail {
  let held = held_thread_token()
    |> assets.add(case_policy, h32_a, 1)
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    thread_burn_tx(
      thread_holder(Script(config().game_v1), held),
      assets.from_asset(case_policy, case_thread_name, -1)
        |> assets.add(case_policy, h32_a, -1),
    ),
  )
}

test thread_token_burn_of_a_doubled_token_is_refused() fail {
  let held = held_thread_token()
    |> assets.add(case_policy, case_thread_name, 1)
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    thread_burn_tx(
      thread_holder(Script(config().game_v1), held),
      assets.from_asset(case_policy, case_thread_name, -2),
    ),
  )
}

test thread_token_mint_under_the_burn_redeemer_is_refused() fail {
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    thread_burn_tx(
      thread_holder(Script(config().game_v1), held_thread_token()),
      assets.from_asset(case_policy, case_thread_name, 1),
    ),
  )
}

test thread_token_burn_without_the_configuration_is_refused() fail {
  let tx = thread_burn_tx(
    thread_holder(Script(config().game_v1), held_thread_token()),
    assets.from_asset(case_policy, case_thread_name, -1),
  )
  case_mint(
    BurnThreadToken { thread_input_index: 0 },
    Transaction { ..tx, reference_inputs: [] },
  )
}

test case_spend_under_the_thread_burn_redeemer_is_refused() fail {
  case_validator.main.spend(
    hub_policy,
    config_policy,
    thread_policy,
    bond_lovelace,
    Some(undefended()),
    BurnThreadToken { thread_input_index: 0 },
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

fn bond_input_with(script: ByteArray, lovelace: Int, datum: Data) -> Input {
  Input {
    output_reference: bond_out_ref(),
    output: Output {
      address: address.from_script(script),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
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
    |> assets.add(case_policy, case_id(), -1)
}

const lock_lovelace = 1_500_000

/// The case NFT, locked at the fraud-proof address by a convicted close.
fn case_lock(lovelace: Int) -> Output {
  Output {
    address: address.from_script(fraud_proof_script),
    value: assets.from_lovelace(lovelace)
      |> assets.add(case_policy, case_id(), 1),
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
    validity_range: interval.between(lower, lower + 60_000),
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
