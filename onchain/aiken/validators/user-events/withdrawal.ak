use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  Datum as OutputDatum, DatumHash, InlineDatum, Input, Mint, NoDatum, Output,
  OutputReference, Redeemer, ScriptPurpose, Spend, Transaction, Withdraw,
}
use cardano/transaction as tx
use env
use midgard/common/types.{Proof}
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/payout
use midgard/settlement
use midgard/user_events
use midgard/user_events/withdrawal.{
  Datum, InitializePayout, Refund, SpendRedeemer,
}

fn value_is_nonnegative(value: Value) -> Bool {
  value
    |> assets.flatten
    |> list.all(
        fn(asset) {
          let (_, _, quantity) = asset
          quantity >= 0
        },
      )
}

fn validate_initial_payout_accumulator_value(
  output_value: Value,
  payout_policy_id: PolicyId,
  payout_asset_name: AssetName,
  target: ledger_state.MidgardValue,
) -> Bool {
  let initial_accumulator_value =
    output_value
      |> assets.add(payout_policy_id, payout_asset_name, -1)
  let remaining_target =
    assets.merge(
      assets.from_asset_list(target),
      assets.negate(initial_accumulator_value),
    )
  value_is_nonnegative(remaining_target)
}

fn validate_refund_output_shape(
  own_value: Value,
  withdrawal_policy_id: PolicyId,
  burn_asset_name: AssetName,
  output_value: Value,
  output_address: Address,
  output_datum: OutputDatum,
  refund_address: Address,
  refund_datum: OutputDatum,
) -> Bool {
  expect
    output_value == (
      own_value
        |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
    )
  expect output_address == refund_address
  expect output_datum == refund_datum
  True
}

fn validate_refund_branch(
  own_value: Value,
  withdrawal_policy_id: PolicyId,
  burn_asset_name: AssetName,
  output_value: Value,
  output_address: Address,
  output_datum: OutputDatum,
  withdrawal_datum: Datum,
  withdrawals_root: ledger_state.WithdrawalsRoot,
  validity_override: ledger_state.WithdrawalValidity,
  membership_proof: Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
) -> Bool {
  let user_events.OptimisticDatum {
    event: withdrawal_event,
    refund_address,
    refund_datum,
    ..
  } = withdrawal_datum

  expect validity_override != ledger_state.WithdrawalIsValid
  expect
    validate_refund_output_shape(
      own_value,
      withdrawal_policy_id,
      burn_asset_name,
      output_value,
      output_address,
      output_datum,
      refund_address,
      refund_datum,
    )

  let Pair(withdrawal_event_id, withdrawal_event_info) =
    ledger_state.unsafe_event_to_key_value_pair(withdrawal_event)
  expect coerced_withdrawal_info: ledger_state.WithdrawalInfo =
    withdrawal_event_info
  let overridden_withdrawal_info: Data =
    ledger_state.WithdrawalInfo {
      ..coerced_withdrawal_info,
      validity: validity_override,
    }

  plutarch_phas(
    withdrawals_root,
    withdrawal_event_id,
    overridden_withdrawal_info,
    membership_proof,
    redeemers,
    inclusion_proof_script_withdraw_redeemer_index,
  )
}

fn get_settlement_withdrawals_root(
  reference_inputs: List<Input>,
  settlement_policy_id: PolicyId,
  settlement_ref_input_index: Int,
) -> ledger_state.WithdrawalsRoot {
  expect Input {
    output: Output { datum: InlineDatum(settlement_datum_data), .. },
    ..
  } =
    utils.get_authentic_input_with_policy_at(
      reference_inputs,
      settlement_policy_id,
      settlement_ref_input_index,
    )
  expect Pair(0, settlement_fields) =
    builtin.un_constr_data(settlement_datum_data)
  expect [
    _deposits_root_data, withdrawals_root_data, _transactions_root_data,
    _resolution_claim_data,
  ] = settlement_fields
  builtin.un_b_data(withdrawals_root_data)
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

    let SpendRedeemer {
      input_index,
      output_index,
      hub_ref_input_index,
      settlement_ref_input_index,
      burn_redeemer_index,
      payout_mint_redeemer_index,
      membership_proof,
      inclusion_proof_script_withdraw_redeemer_index,
      purpose,
    } = redeemer

    let hub.Datum {
      withdrawal: withdrawal_policy_id,
      settlement: settlement_policy_id,
      payout: payout_policy_id,
      payout_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let withdrawals_root =
      get_settlement_withdrawals_root(
        reference_inputs,
        settlement_policy_id,
        settlement_ref_input_index,
      )

    expect Some(withdrawal_datum) = datum
    let user_events.OptimisticDatum { event: withdrawal_event, .. } =
      withdrawal_datum

    // 1. Spending a user event UTxO must also be accompanied by the burning of
    //    its NFT (and consequently deregistration of its witness staking
    //    script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(withdrawal_policy_id),
        burn_redeemer_index,
      )

    let
      input,
      output,
    <-
      singular_utxo_indexer.one_to_one(
        input_index,
        output_index,
        own_out_ref,
        inputs,
        outputs,
        // Double satisfaction prevented by the requirement for unregistration of
        // the witness staking script.
        True,
      )

    let Input { output: Output { value: own_value, .. }, .. } = input

    // 2. The produced UTxO must not have any scripts attached to it.
    expect Output {
      address: output_address,
      value: output_value,
      datum: output_datum,
      reference_script: None,
    } = output

    when purpose is {
      InitializePayout -> {
        // 3. Output value must be the same as the input value, without the
        //    withdrawal NFT, and with the payout NFT added.
        expect
          output_value == (
            own_value
              |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
              |> assets.add(payout_policy_id, burn_asset_name, 1)
          )

        expect payout.MintPayout {
          withdrawal_utxo_out_ref,
          withdrawal_input_index,
          withdrawal_spend_redeemer_index,
          hub_ref_input_index: payout_mint_hub_ref_input_index,
        }: payout.MintRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Mint(payout_policy_id),
            payout_mint_redeemer_index,
          )
        expect withdrawal_utxo_out_ref == own_out_ref
        expect withdrawal_input_index == input_index
        expect payout_mint_hub_ref_input_index == hub_ref_input_index
        expect SpendRedeemer {
          input_index: withdrawal_redeemer_input_index,
          payout_mint_redeemer_index: withdrawal_redeemer_payout_mint_index,
          hub_ref_input_index: withdrawal_redeemer_hub_ref_input_index,
          purpose: InitializePayout,
          ..
        }: SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(own_out_ref),
            withdrawal_spend_redeemer_index,
          )
        expect withdrawal_redeemer_input_index == input_index
        expect
          withdrawal_redeemer_payout_mint_index == payout_mint_redeemer_index
        expect withdrawal_redeemer_hub_ref_input_index == hub_ref_input_index

        let Pair(withdrawal_event_id, withdrawal_event_info) =
          ledger_state.unsafe_event_to_key_value_pair(withdrawal_event)

        // 5. Initiation of a payout accumulator can only happen for valid
        //    withdrawals.
        expect ledger_state.WithdrawalInfo {
          body: withdrawal_body,
          validity: ledger_state.WithdrawalIsValid,
          ..
        } = withdrawal_event_info

        // 6. The referenced settlement UTxO's withdrawal tree must contain
        //    the subject withdrawal event.
        expect
          plutarch_phas(
            withdrawals_root,
            withdrawal_event_id,
            withdrawal_event_info,
            membership_proof,
            redeemers,
            inclusion_proof_script_withdraw_redeemer_index,
          )

        let ledger_state.WithdrawalBody { l2_value, l1_address, l1_datum, .. } =
          withdrawal_body

        // 7. The initial payout accumulator value must not exceed the withdrawal
        //    target for any asset. Otherwise the exact-value conclusion path
        //    could become impossible.
        expect
          validate_initial_payout_accumulator_value(
            output_value,
            payout_policy_id,
            burn_asset_name,
            l2_value,
          )

        // 8. Assert payout datum properly carries over all the required
        //    info.
        expect InlineDatum(output_datum_data) = output_datum
        expect payout.Datum {
          l2_value: output_l2_value,
          l1_address: output_l1_address,
          l1_datum: output_l1_datum,
        }: payout.Datum = output_datum_data
        expect output_l2_value == l2_value
        expect output_l1_address == l1_address
        expect output_l1_datum == l1_datum

        // 9. Withdrawal must go to the payout contract.
        expect output_address == payout_addr

        // Done.
        True
      }
      Refund { validity_override } -> {
        // 3. Invalid withdrawals must be reproduced at the user-specified
        //    refund address and datum with the input value minus the withdrawal
        //    NFT, and must be proven in the settlement's withdrawals tree with
        //    the same invalidity provided via the redeemer.
        expect
          validate_refund_branch(
            own_value,
            withdrawal_policy_id,
            burn_asset_name,
            output_value,
            output_address,
            output_datum,
            withdrawal_datum,
            withdrawals_root,
            validity_override,
            membership_proof,
            redeemers,
            inclusion_proof_script_withdraw_redeemer_index,
          )

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

const test_hub_policy =
  #"11111111111111111111111111111111111111111111111111111111"

const test_withdrawal_policy =
  #"22222222222222222222222222222222222222222222222222222222"

const test_settlement_policy =
  #"33333333333333333333333333333333333333333333333333333333"

const test_payout_policy =
  #"44444444444444444444444444444444444444444444444444444444"

const test_reserve_policy =
  #"55555555555555555555555555555555555555555555555555555555"

const test_owner = #"66666666666666666666666666666666666666666666666666666666"

const test_asset_name = #"aa"

const test_withdrawal_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }

const test_l2_ref =
  OutputReference {
    transaction_id: #"0202020202020202020202020202020202020202020202020202020202020202",
    output_index: 0,
  }

const test_withdrawals_root =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const test_deposits_root =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const test_transactions_root =
  #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

fn test_l1_address() {
  address.from_script(test_owner)
}

fn test_hub_datum() -> hub.Datum {
  let reserve_addr = address.from_script(test_reserve_policy)
  hub.Datum {
    registered_operators: #"10101010101010101010101010101010101010101010101010101010",
    active_operators: #"11111111111111111111111111111111111111111111111111111111",
    retired_operators: #"12121212121212121212121212121212121212121212121212121212",
    scheduler: #"13131313131313131313131313131313131313131313131313131313",
    state_queue: #"14141414141414141414141414141414141414141414141414141414",
    fraud_proof_catalogue: #"15151515151515151515151515151515151515151515151515151515",
    fraud_proof: #"16161616161616161616161616161616161616161616161616161616",
    deposit: #"17171717171717171717171717171717171717171717171717171717",
    withdrawal: test_withdrawal_policy,
    tx_order: #"18181818181818181818181818181818181818181818181818181818",
    settlement: test_settlement_policy,
    payout: test_payout_policy,
    registered_operators_addr: reserve_addr,
    active_operators_addr: reserve_addr,
    retired_operators_addr: reserve_addr,
    scheduler_addr: reserve_addr,
    state_queue_addr: reserve_addr,
    fraud_proof_catalogue_addr: reserve_addr,
    fraud_proof_addr: reserve_addr,
    deposit_addr: reserve_addr,
    withdrawal_addr: address.from_script(test_withdrawal_policy),
    tx_order_addr: reserve_addr,
    settlement_addr: address.from_script(test_settlement_policy),
    reserve_addr,
    payout_addr: address.from_script(test_payout_policy),
    reserve_observer: test_reserve_policy,
  }
}

fn test_hub_ref_input() -> Input {
  let hub_datum_data: Data = test_hub_datum()
  Input {
    output_reference: OutputReference {
      transaction_id: #"0303030303030303030303030303030303030303030303030303030303030303",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(test_hub_policy),
      value: assets.from_asset(test_hub_policy, hub.asset_name, 1),
      datum: InlineDatum(hub_datum_data),
      reference_script: None,
    },
  }
}

test withdrawal_refund_hub_fixture_decodes() {
  let hub_datum_data: Data = test_hub_datum()
  expect decoded: hub.Datum = hub_datum_data
  and {
    decoded == test_hub_datum(),
    hub.get_datum(
      [test_hub_ref_input(), test_settlement_ref_input()],
      test_hub_policy,
      0,
    ) == test_hub_datum(),
  }
}

test withdrawal_refund_tx_hub_decodes() {
  let Transaction { reference_inputs, .. } =
    test_refund_tx(test_valid_refund_output())
  hub.get_datum(reference_inputs, test_hub_policy, 0) == test_hub_datum()
}

test withdrawal_refund_tx_settlement_decodes() {
  let Transaction { reference_inputs, .. } =
    test_refund_tx(test_valid_refund_output())
  settlement.get_datum(reference_inputs, test_settlement_policy, 1) == settlement.Datum {
    deposits_root: test_deposits_root,
    withdrawals_root: test_withdrawals_root,
    transactions_root: test_transactions_root,
    resolution_claim: None,
  }
}

test settlement_sdk_fixture_decodes() {
  expect Some(settlement_datum_data) =
    cbor.deserialise(
      #"d8799f58209199f6a06dd054c176b03f971663fb08eab680f330c53f03a7c2a7a4f7e2deb35820a0a6bfacf243dca6d2adbae4e8846841e74c90f17d7602cefc2375dad3ae56eb58207777777777777777777777777777777777777777777777777777777777777777d87a80ff",
    )
  expect decoded: settlement.Datum = settlement_datum_data
  decoded == settlement.Datum {
    deposits_root: #"9199f6a06dd054c176b03f971663fb08eab680f330c53f03a7c2a7a4f7e2deb3",
    withdrawals_root: #"a0a6bfacf243dca6d2adbae4e8846841e74c90f17d7602cefc2375dad3ae56eb",
    transactions_root: #"7777777777777777777777777777777777777777777777777777777777777777",
    resolution_claim: None,
  }
}

test settlement_sdk_fixture_get_datum_decodes() {
  expect Some(settlement_datum_data) =
    cbor.deserialise(
      #"d8799f58209199f6a06dd054c176b03f971663fb08eab680f330c53f03a7c2a7a4f7e2deb35820a0a6bfacf243dca6d2adbae4e8846841e74c90f17d7602cefc2375dad3ae56eb58207777777777777777777777777777777777777777777777777777777777777777d87a80ff",
    )
  let settlement_policy = #"7079358dfbe98ea1a3b7afac27d1d99d140979eaa28dac13"
  let settlement_input =
    Input {
      output_reference: OutputReference {
        transaction_id: #"00",
        output_index: 8,
      },
      output: Output {
        address: address.from_script(settlement_policy),
        value: assets.from_asset(settlement_policy, #"cc", 1),
        datum: InlineDatum(settlement_datum_data),
        reference_script: None,
      },
    }
  settlement.get_datum([settlement_input], settlement_policy, 0) == settlement.Datum {
    deposits_root: #"9199f6a06dd054c176b03f971663fb08eab680f330c53f03a7c2a7a4f7e2deb3",
    withdrawals_root: #"a0a6bfacf243dca6d2adbae4e8846841e74c90f17d7602cefc2375dad3ae56eb",
    transactions_root: #"7777777777777777777777777777777777777777777777777777777777777777",
    resolution_claim: None,
  }
}

test withdrawal_refund_redeemer_indexes_are_stable() {
  let SpendRedeemer {
    hub_ref_input_index,
    settlement_ref_input_index,
    burn_redeemer_index,
    inclusion_proof_script_withdraw_redeemer_index,
    ..
  } = test_refund_spend_redeemer(0)
  and {
    hub_ref_input_index == 0,
    settlement_ref_input_index == 1,
    burn_redeemer_index == 1,
    inclusion_proof_script_withdraw_redeemer_index == 2,
  }
}

test withdrawal_refund_event_info_fixture_decodes() {
  let user_events.OptimisticDatum { event, .. } =
    test_withdrawal_order_datum(ledger_state.WithdrawalIsValid)
  let Pair(_, info_data) = ledger_state.unsafe_event_to_key_value_pair(event)
  expect info: ledger_state.WithdrawalInfo = info_data
  info == test_withdrawal_info(ledger_state.WithdrawalIsValid)
}

test withdrawal_output_reference_cbor_matches_sdk_encoding() {
  cbor.serialise(test_withdrawal_ref) == #"d8799f5820010101010101010101010101010101010101010101010101010101010101010100ff"
}

test withdrawal_info_cbor_matches_sdk_default_encoding() {
  cbor.serialise(test_withdrawal_info(ledger_state.WithdrawalIsValid)) == #"d8799fd8799fd8799f5820020202020202020202020202020202020202020202020202020202020202020200ff581c66666666666666666666666666666666666666666666666666666666a140a1401a002dc6c0d8799fd87a9f581c66666666666666666666666666666666666666666666666666666666ffd87a80ffd87980ff9f41014102ffd87980ff"
}

test withdrawal_refund_hub_settlement_policy_matches_fixture() {
  let Transaction { reference_inputs, .. } =
    test_refund_tx(test_valid_refund_output())
  let hub.Datum { settlement: settlement_policy_id, .. } =
    hub.get_datum(reference_inputs, test_hub_policy, 0)
  settlement_policy_id == test_settlement_policy
}

fn test_settlement_ref_input() -> Input {
  let settlement_datum_data: Data =
    builtin.constr_data(
      0,
      [
        builtin.b_data(test_deposits_root),
        builtin.b_data(test_withdrawals_root),
        builtin.b_data(test_transactions_root),
        builtin.constr_data(1, []),
      ],
    )
  Input {
    output_reference: OutputReference {
      transaction_id: #"0404040404040404040404040404040404040404040404040404040404040404",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(test_settlement_policy),
      value: assets.from_asset(test_settlement_policy, "settlement", 1),
      datum: InlineDatum(settlement_datum_data),
      reference_script: None,
    },
  }
}

fn test_withdrawal_body() -> ledger_state.WithdrawalBody {
  ledger_state.WithdrawalBody {
    l2_outref: test_l2_ref,
    l2_owner: test_owner,
    l2_value: [
      Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, 3_000_000)]),
    ],
    l1_address: test_l1_address(),
    l1_datum: NoDatum,
  }
}

fn test_withdrawal_info(
  validity: ledger_state.WithdrawalValidity,
) -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: test_withdrawal_body(),
    signature: Pair(#"01", #"02"),
    validity,
  }
}

fn test_withdrawal_order_datum(
  validity: ledger_state.WithdrawalValidity,
) -> Datum {
  user_events.OptimisticDatum {
    event: ledger_state.WithdrawalEvent {
      id: test_withdrawal_ref,
      info: test_withdrawal_info(validity),
    },
    inclusion_time: 0,
    witness: #"abababababababababababababababababababababababababababab",
    refund_address: test_l1_address(),
    refund_datum: NoDatum,
  }
}

fn test_refund_spend_redeemer(output_index: Int) -> SpendRedeemer {
  SpendRedeemer {
    input_index: 0,
    output_index,
    hub_ref_input_index: 0,
    settlement_ref_input_index: 1,
    burn_redeemer_index: 1,
    payout_mint_redeemer_index: 0,
    membership_proof: [],
    inclusion_proof_script_withdraw_redeemer_index: 2,
    purpose: Refund { validity_override: ledger_state.UnpayableWithdrawalValue },
  }
}

fn test_phas_redeemer(value: Data) -> Data {
  let key_data: Data = test_withdrawal_ref
  let proof_data: Data = []
  builtin.list_data(
    [
      builtin.b_data(test_withdrawals_root),
      builtin.b_data(cbor.serialise(key_data)),
      builtin.b_data(cbor.serialise(value)),
      proof_data,
    ],
  )
}

fn test_withdrawal_input() -> Input {
  Input {
    output_reference: test_withdrawal_ref,
    output: Output {
      address: address.from_script(test_withdrawal_policy),
      value: test_withdrawal_input_value(),
      datum: InlineDatum(
        test_withdrawal_order_datum(ledger_state.WithdrawalIsValid),
      ),
      reference_script: None,
    },
  }
}

fn test_withdrawal_input_value() -> Value {
  assets.from_lovelace(3_000_000)
    |> assets.add(test_withdrawal_policy, test_asset_name, 1)
}

fn test_multi_asset_target() -> ledger_state.MidgardValue {
  [
    Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, 3_000_000)]),
    Pair(test_reserve_policy, [Pair(#"01", 1)]),
  ]
}

test initialize_payout_allows_underfunded_initial_value() {
  validate_initial_payout_accumulator_value(
    assets.from_lovelace(2_000_000)
      |> assets.add(test_payout_policy, test_asset_name, 1),
    test_payout_policy,
    test_asset_name,
    test_withdrawal_body().l2_value,
  )
}

test initialize_payout_allows_exact_multi_asset_initial_value() {
  validate_initial_payout_accumulator_value(
    assets.from_lovelace(3_000_000)
      |> assets.add(test_reserve_policy, #"01", 1)
      |> assets.add(test_payout_policy, test_asset_name, 1),
    test_payout_policy,
    test_asset_name,
    test_multi_asset_target(),
  )
}

test initialize_payout_rejects_extra_initial_asset() fail {
  validate_initial_payout_accumulator_value(
    assets.from_lovelace(3_000_000)
      |> assets.add(test_reserve_policy, #"01", 1)
      |> assets.add(test_payout_policy, test_asset_name, 1),
    test_payout_policy,
    test_asset_name,
    test_withdrawal_body().l2_value,
  )
}

test initialize_payout_rejects_multi_asset_surplus() fail {
  validate_initial_payout_accumulator_value(
    assets.from_lovelace(3_000_000)
      |> assets.add(test_reserve_policy, #"01", 2)
      |> assets.add(test_payout_policy, test_asset_name, 1),
    test_payout_policy,
    test_asset_name,
    test_multi_asset_target(),
  )
}

fn test_refund_tx(output: Output) -> Transaction {
  let spend_redeemer_data: Data = test_refund_spend_redeemer(0)
  let burn_redeemer_data: Data =
    user_events.BurnEventNFT {
      nonce_asset_name: test_asset_name,
      witness_unregistration_redeemer_index: 2,
    }
  let invalid_withdrawal_info: Data =
    test_withdrawal_info(ledger_state.UnpayableWithdrawalValue)
  Transaction {
    ..tx.placeholder,
    inputs: [test_withdrawal_input()],
    reference_inputs: [test_hub_ref_input(), test_settlement_ref_input()],
    outputs: [output],
    mint: assets.from_asset(test_withdrawal_policy, test_asset_name, -1),
    redeemers: [
      Pair(Spend(test_withdrawal_ref), spend_redeemer_data),
      Pair(Mint(test_withdrawal_policy), burn_redeemer_data),
      Pair(
        Withdraw(Script(env.plutarch_phas_validator_hash)),
        test_phas_redeemer(invalid_withdrawal_info),
      ),
    ],
  }
}

fn test_valid_refund_output() -> Output {
  Output {
    address: test_l1_address(),
    value: assets.from_lovelace(3_000_000),
    datum: NoDatum,
    reference_script: None,
  }
}

fn validate_refund_branch_for_test(
  output: Output,
  datum: Datum,
  redeemer: SpendRedeemer,
  own_out_ref: OutputReference,
  transaction: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, redeemers, .. } =
    transaction
  let SpendRedeemer {
    input_index,
    output_index,
    hub_ref_input_index,
    settlement_ref_input_index,
    burn_redeemer_index,
    membership_proof,
    inclusion_proof_script_withdraw_redeemer_index,
    purpose,
    ..
  } = redeemer
  expect hub_ref_input_index == 0
  let withdrawal_policy_id = test_withdrawal_policy
  let settlement_policy_id = test_settlement_policy
  let withdrawals_root =
    get_settlement_withdrawals_root(
      reference_inputs,
      settlement_policy_id,
      settlement_ref_input_index,
    )
  expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
    utils.get_redeemer_at(
      redeemers,
      Mint(withdrawal_policy_id),
      burn_redeemer_index,
    )
  let
    input,
    indexed_output,
  <-
    singular_utxo_indexer.one_to_one(
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
      True,
    )
  expect indexed_output == output
  let Input { output: Output { value: own_value, .. }, .. } = input
  expect Output {
    address: output_address,
    value: output_value,
    datum: output_datum,
    reference_script: None,
  } = output
  expect Refund { validity_override } = purpose
  validate_refund_branch(
    own_value,
    withdrawal_policy_id,
    burn_asset_name,
    output_value,
    output_address,
    output_datum,
    datum,
    withdrawals_root,
    validity_override,
    membership_proof,
    redeemers,
    inclusion_proof_script_withdraw_redeemer_index,
  )
}

test invalid_withdrawal_refund_branch_accepts_recorded_target() {
  let output = test_valid_refund_output()
  validate_refund_branch_for_test(
    output,
    test_withdrawal_order_datum(ledger_state.WithdrawalIsValid),
    test_refund_spend_redeemer(0),
    test_withdrawal_ref,
    test_refund_tx(output),
  )
}

test invalid_withdrawal_refund_branch_rejects_wrong_address() fail {
  let bad_output =
    Output {
      ..test_valid_refund_output(),
      address: address.from_script(test_reserve_policy),
    }
  validate_refund_branch_for_test(
    bad_output,
    test_withdrawal_order_datum(ledger_state.WithdrawalIsValid),
    test_refund_spend_redeemer(0),
    test_withdrawal_ref,
    test_refund_tx(bad_output),
  )
}

test invalid_withdrawal_refund_branch_rejects_wrong_datum() fail {
  let bad_output =
    Output {
      ..test_valid_refund_output(),
      datum: DatumHash(
        #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
      ),
    }
  validate_refund_branch_for_test(
    bad_output,
    test_withdrawal_order_datum(ledger_state.WithdrawalIsValid),
    test_refund_spend_redeemer(0),
    test_withdrawal_ref,
    test_refund_tx(bad_output),
  )
}

test invalid_withdrawal_refund_branch_rejects_wrong_value() fail {
  let bad_output =
    Output {
      ..test_valid_refund_output(),
      value: assets.from_lovelace(2_999_999),
    }
  validate_refund_branch_for_test(
    bad_output,
    test_withdrawal_order_datum(ledger_state.WithdrawalIsValid),
    test_refund_spend_redeemer(0),
    test_withdrawal_ref,
    test_refund_tx(bad_output),
  )
}

test invalid_withdrawal_refund_branch_rejects_valid_withdrawal_marker() fail {
  let output = test_valid_refund_output()
  let redeemer =
    SpendRedeemer {
      ..test_refund_spend_redeemer(0),
      purpose: Refund { validity_override: ledger_state.WithdrawalIsValid },
    }
  validate_refund_branch_for_test(
    output,
    test_withdrawal_order_datum(ledger_state.WithdrawalIsValid),
    redeemer,
    test_withdrawal_ref,
    test_refund_tx(output),
  )
}

test invalid_withdrawal_refund_reproduces_recorded_target() {
  validate_refund_output_shape(
    test_withdrawal_input_value(),
    test_withdrawal_policy,
    test_asset_name,
    assets.from_lovelace(3_000_000),
    test_l1_address(),
    NoDatum,
    test_l1_address(),
    NoDatum,
  )
}

test invalid_withdrawal_refund_rejects_wrong_address() fail {
  validate_refund_output_shape(
    test_withdrawal_input_value(),
    test_withdrawal_policy,
    test_asset_name,
    assets.from_lovelace(3_000_000),
    address.from_script(test_reserve_policy),
    NoDatum,
    test_l1_address(),
    NoDatum,
  )
}

test invalid_withdrawal_refund_rejects_wrong_datum() fail {
  validate_refund_output_shape(
    test_withdrawal_input_value(),
    test_withdrawal_policy,
    test_asset_name,
    assets.from_lovelace(3_000_000),
    test_l1_address(),
    DatumHash(
      #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
    ),
    test_l1_address(),
    NoDatum,
  )
}

test invalid_withdrawal_refund_rejects_wrong_value() fail {
  validate_refund_output_shape(
    test_withdrawal_input_value(),
    test_withdrawal_policy,
    test_asset_name,
    assets.from_lovelace(2_999_999),
    test_l1_address(),
    NoDatum,
    test_l1_address(),
    NoDatum,
  )
}

validator mint(hub_oracle: ScriptHash) {
  mint(
    redeemer: user_events.MintRedeemer,
    own_policy: PolicyId,
    tx: Transaction,
  ) {
    // 1. Perform numerous validations (refer to `lib/midgard/user-events.ak`)
    //    and grant access to:
    //    - Serialized and hashed nonce based on the output reference of a
    //      specified input UTxO.
    //    - The output `Value` of the UTxO at the withdrawal address for the
    //      minting (not burning) endpoint.
    //    - The `WithdrawalInfo` as `Data`, which we are ignoring here (TODO:
    //      try remodeling the types to avoid inclusion of `WithdrawalValidity`
    //      on L1 datum).
    let
      l1_id,
      output_withdrawal_order_value,
      withdrawal_info_data,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { withdrawal_addr, .. } = hub_datum
          withdrawal_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    let output_withdrawal_order_nft_triplet =
      utils.get_single_asset_from_value_apart_from_ada(
        output_withdrawal_order_value,
      )

    // 2. The output value of the UTxO at the withdrawal address must contain
    //    only ADA and the authentication NFT.
    expect output_withdrawal_order_nft_triplet == (own_policy, l1_id, 1)

    // 3. The withdrawal event must be marked as valid. This is needed since
    //    only valid withdrawals can initialize payout accumulators.
    expect ledger_state.WithdrawalInfo {
      validity: ledger_state.WithdrawalIsValid,
      ..
    } = withdrawal_info_data

    // Done.
    True
  }

  else(_) {
    fail
  }
}
