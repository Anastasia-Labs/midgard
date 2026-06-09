use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use cardano/transaction as tx
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/payout
use midgard/reserve
use midgard/user_events/withdrawal as withdrawal_lib

fn singleton_asset_name_with_policy(
  value: Value,
  policy_id: PolicyId,
) -> AssetName {
  expect [Pair(asset_name, 1)] =
    value
      |> assets.tokens(policy_id)
      |> dict.to_pairs
  asset_name
}

fn value_without_nft(
  value: Value,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Value {
  value |> assets.add(policy_id, asset_name, -1)
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

fn value_is_positive(value: Value) -> Bool {
  value
    |> assets.flatten
    |> list.any(
        fn(asset) {
          let (_, _, quantity) = asset
          quantity > 0
        },
      )
}

fn no_change_for_still_needed_assets(change: Value, still_needed: Value) -> Bool {
  change
    |> assets.flatten
    |> list.all(
        fn(asset) {
          let (policy_id, asset_name, quantity) = asset
          if quantity > 0 {
            assets.quantity_of(still_needed, policy_id, asset_name) == 0
          } else {
            True
          }
        },
      )
}

fn reserve_change_value(
  outputs: List<Output>,
  reserve_addr: Address,
  reserve_change_output_index: Option<Int>,
) -> Value {
  when reserve_change_output_index is {
    None -> assets.zero
    Some(output_index) -> {
      expect Some(Output {
        address,
        value,
        datum: NoDatum,
        reference_script: None,
      }) = outputs |> list.at(output_index)
      expect address == reserve_addr
      expect !assets.is_zero(value)
      value
    }
  }
}

fn count_inputs_with_nft(
  inputs: List<Input>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Int {
  inputs
    |> list.count(
        fn(input) {
          assets.quantity_of(input.output.value, policy_id, asset_name) == 1
        },
      )
}

fn count_outputs_with_nft(
  outputs: List<Output>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Int {
  outputs
    |> list.count(
        fn(output) {
          assets.quantity_of(output.value, policy_id, asset_name) == 1
        },
      )
}

fn count_inputs_with_policy_id(inputs: List<Input>, policy_id: PolicyId) -> Int {
  inputs
    |> list.count(
        fn(input) {
          utils.quantity_of_policy_id(input.output.value, policy_id) != 0
        },
      )
}

fn count_outputs_with_policy_id(
  outputs: List<Output>,
  policy_id: PolicyId,
) -> Int {
  outputs
    |> list.count(
        fn(output) { utils.quantity_of_policy_id(output.value, policy_id) != 0 },
      )
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<payout.Datum>,
    redeemer: payout.SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, mint, .. } =
      tx

    expect Some(payout_datum) = datum
    let payout_datum_data: Data = payout_datum
    let target_value = assets.from_asset_list(payout_datum.l2_value)

    when redeemer is {
      payout.AddFunds {
        payout_input_index,
        payout_output_index,
        reserve_input_index,
        reserve_change_output_index,
        hub_ref_input_index,
        reserve_spend_redeemer_index,
        payout_spend_redeemer_index,
      } -> {
        let hub.Datum {
          payout: payout_policy_id,
          payout_addr,
          reserve_addr,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        expect Some(Input {
          output_reference: payout_out_ref,
          output: Output {
            address: payout_input_address,
            value: payout_input_value,
            datum: InlineDatum(input_datum_data),
            ..
          },
        }) = inputs |> list.at(payout_input_index)
        expect payout_out_ref == own_out_ref
        expect payout_input_address == payout_addr
        expect input_datum_data == payout_datum_data

        let payout_asset_name =
          singleton_asset_name_with_policy(payout_input_value, payout_policy_id)
        expect
          count_inputs_with_nft(inputs, payout_policy_id, payout_asset_name) == 1
        expect count_inputs_with_policy_id(inputs, payout_policy_id) == 1

        expect Some(Input {
          output_reference: reserve_out_ref,
          output: Output {
            address: reserve_input_address,
            value: reserve_input_value,
            datum: NoDatum,
            reference_script: None,
          },
        }) = inputs |> list.at(reserve_input_index)
        expect reserve_input_address == reserve_addr
        expect
          list.count(inputs, fn(input) { input.output.address == reserve_addr }) == 1

        expect reserve.Spend {
          reserve_input_index: reserve_redeemer_reserve_input_index,
          payout_input_index: reserve_redeemer_payout_input_index,
          payout_spend_redeemer_index: reserve_redeemer_payout_spend_redeemer_index,
          hub_ref_input_index: reserve_redeemer_hub_ref_input_index,
        }: reserve.SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(reserve_out_ref),
            reserve_spend_redeemer_index,
          )
        expect reserve_redeemer_reserve_input_index == reserve_input_index
        expect reserve_redeemer_payout_input_index == payout_input_index
        expect
          reserve_redeemer_payout_spend_redeemer_index == payout_spend_redeemer_index
        expect reserve_redeemer_hub_ref_input_index == hub_ref_input_index
        expect payout.AddFunds {
          reserve_input_index: self_reserve_input_index,
          ..
        }: payout.SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(own_out_ref),
            payout_spend_redeemer_index,
          )
        expect self_reserve_input_index == reserve_input_index

        expect Some(Output {
          address: payout_output_address,
          value: payout_output_value,
          datum: InlineDatum(output_datum_data),
          reference_script: None,
        }) = outputs |> list.at(payout_output_index)
        expect payout_output_address == payout_addr
        expect output_datum_data == payout_datum_data
        expect
          singleton_asset_name_with_policy(
            payout_output_value,
            payout_policy_id,
          ) == payout_asset_name
        expect
          count_outputs_with_nft(outputs, payout_policy_id, payout_asset_name) == 1
        expect count_outputs_with_policy_id(outputs, payout_policy_id) == 1

        let current_value =
          value_without_nft(
            payout_input_value,
            payout_policy_id,
            payout_asset_name,
          )
        let payout_output_without_nft =
          value_without_nft(
            payout_output_value,
            payout_policy_id,
            payout_asset_name,
          )
        let collected_value =
          assets.merge(payout_output_without_nft, assets.negate(current_value))
        let change_value =
          reserve_change_value(
            outputs,
            reserve_addr,
            reserve_change_output_index,
          )
        let remaining_needed =
          assets.merge(target_value, assets.negate(payout_output_without_nft))

        expect
          value_is_nonnegative(
            assets.merge(target_value, assets.negate(current_value)),
          )
        expect value_is_nonnegative(remaining_needed)
        expect value_is_nonnegative(collected_value)
        expect value_is_positive(collected_value)
        expect
          assets.merge(collected_value, change_value) == reserve_input_value
        expect no_change_for_still_needed_assets(change_value, remaining_needed)
        expect mint == assets.zero

        True
      }

      payout.ConcludeWithdrawal {
        payout_input_index,
        l1_output_index,
        burn_redeemer_index,
        hub_ref_input_index,
      } -> {
        let hub.Datum { payout: payout_policy_id, payout_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        expect Some(Input {
          output_reference: payout_out_ref,
          output: Output {
            address: payout_input_address,
            value: payout_input_value,
            datum: InlineDatum(input_datum_data),
            ..
          },
        }) = inputs |> list.at(payout_input_index)
        expect payout_out_ref == own_out_ref
        expect payout_input_address == payout_addr
        expect input_datum_data == payout_datum_data

        let payout_asset_name =
          singleton_asset_name_with_policy(payout_input_value, payout_policy_id)
        expect count_inputs_with_policy_id(inputs, payout_policy_id) == 1
        let payout_input_without_nft =
          value_without_nft(
            payout_input_value,
            payout_policy_id,
            payout_asset_name,
          )
        expect payout_input_without_nft == target_value

        expect Some(Output {
          address: l1_output_address,
          value: l1_output_value,
          datum: l1_output_datum,
          reference_script: None,
        }) = outputs |> list.at(l1_output_index)
        expect l1_output_address == payout_datum.l1_address
        expect l1_output_datum == payout_datum.l1_datum
        expect l1_output_value == target_value
        expect
          count_outputs_with_nft(outputs, payout_policy_id, payout_asset_name) == 0
        expect count_outputs_with_policy_id(outputs, payout_policy_id) == 0
        expect
          !list.any(
            outputs,
            fn(output) {
              and {
                output.address == payout_addr,
                output.datum == InlineDatum(payout_datum_data),
              }
            },
          )

        expect payout.BurnPayout {
          payout_input_index: burn_payout_input_index,
          payout_asset_name: burn_payout_asset_name,
          payout_spend_redeemer_index: burn_payout_spend_redeemer_index,
          hub_ref_input_index: burn_hub_ref_input_index,
        }: payout.MintRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Mint(payout_policy_id),
            burn_redeemer_index,
          )
        expect burn_payout_input_index == payout_input_index
        expect burn_payout_asset_name == payout_asset_name
        expect burn_hub_ref_input_index == hub_ref_input_index
        expect payout.ConcludeWithdrawal {
          payout_input_index: self_payout_input_index,
          l1_output_index: self_l1_output_index,
          burn_redeemer_index: self_burn_redeemer_index,
          hub_ref_input_index: self_hub_ref_input_index,
        }: payout.SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(own_out_ref),
            burn_payout_spend_redeemer_index,
          )
        expect self_payout_input_index == payout_input_index
        expect self_l1_output_index == l1_output_index
        expect self_burn_redeemer_index == burn_redeemer_index
        expect self_hub_ref_input_index == hub_ref_input_index

        True
      }
    }
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(redeemer: payout.MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction { inputs, reference_inputs, redeemers, mint, .. } = tx

    when redeemer is {
      payout.MintPayout {
        withdrawal_utxo_out_ref,
        withdrawal_input_index,
        withdrawal_spend_redeemer_index,
        hub_ref_input_index,
      } -> {
        let hub.Datum {
          payout: payout_policy_id,
          withdrawal: withdrawal_policy_id,
          withdrawal_addr,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)
        expect own_policy == payout_policy_id

        expect [Pair(payout_asset_name, 1)] =
          mint
            |> assets.tokens(own_policy)
            |> dict.to_pairs
        expect [Pair(withdrawal_asset_name, -1)] =
          mint
            |> assets.tokens(withdrawal_policy_id)
            |> dict.to_pairs
        expect payout_asset_name == withdrawal_asset_name
        expect
          mint == assets.merge(
            assets.from_asset(own_policy, payout_asset_name, 1),
            assets.from_asset(withdrawal_policy_id, withdrawal_asset_name, -1),
          )

        expect Some(Input {
          output_reference,
          output: Output {
            address: withdrawal_input_address,
            value: withdrawal_input_value,
            ..
          },
        }) = inputs |> list.at(withdrawal_input_index)
        expect output_reference == withdrawal_utxo_out_ref
        expect withdrawal_input_address == withdrawal_addr
        expect
          assets.quantity_of(
            withdrawal_input_value,
            withdrawal_policy_id,
            withdrawal_asset_name,
          ) == 1

        expect withdrawal_lib.SpendRedeemer {
          purpose: withdrawal_lib.InitializePayout,
          ..
        }: withdrawal_lib.SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(withdrawal_utxo_out_ref),
            withdrawal_spend_redeemer_index,
          )

        True
      }

      payout.BurnPayout {
        payout_input_index,
        payout_asset_name,
        payout_spend_redeemer_index,
        hub_ref_input_index,
      } -> {
        let hub.Datum { payout: payout_policy_id, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)
        expect own_policy == payout_policy_id
        expect mint == assets.from_asset(own_policy, payout_asset_name, -1)

        expect Some(Input { output_reference, output: Output { value, .. } }) =
          inputs |> list.at(payout_input_index)
        expect assets.quantity_of(value, own_policy, payout_asset_name) == 1

        expect payout.ConcludeWithdrawal {
          payout_input_index: spend_payout_input_index,
          hub_ref_input_index: spend_hub_ref_input_index,
          ..
        }: payout.SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(output_reference),
            payout_spend_redeemer_index,
          )
        expect spend_payout_input_index == payout_input_index
        expect spend_hub_ref_input_index == hub_ref_input_index

        True
      }
    }
  }

  else(_) {
    fail
  }
}

const hub_policy: PolicyId =
  #"11111111111111111111111111111111111111111111111111111111"

const payout_policy: PolicyId =
  #"22222222222222222222222222222222222222222222222222222222"

const reserve_policy: PolicyId =
  #"33333333333333333333333333333333333333333333333333333333"

const withdrawal_policy: PolicyId =
  #"52525252525252525252525252525252525252525252525252525252"

const payout_asset: AssetName = #"aa"

const other_payout_asset: AssetName = #"bb"

const token_policy: PolicyId =
  #"77777777777777777777777777777777777777777777777777777777"

const token_asset: AssetName = #"01"

const unused_token_policy: PolicyId =
  #"88888888888888888888888888888888888888888888888888888888"

const unused_token_asset: AssetName = #"02"

const reserve_ref: OutputReference =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }

const payout_ref: OutputReference =
  OutputReference {
    transaction_id: #"0202020202020202020202020202020202020202020202020202020202020202",
    output_index: 0,
  }

const other_payout_ref: OutputReference =
  OutputReference {
    transaction_id: #"0909090909090909090909090909090909090909090909090909090909090909",
    output_index: 0,
  }

const other_reserve_ref: OutputReference =
  OutputReference {
    transaction_id: #"0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a",
    output_index: 0,
  }

const withdrawal_ref: OutputReference =
  OutputReference {
    transaction_id: #"0404040404040404040404040404040404040404040404040404040404040404",
    output_index: 0,
  }

fn ada(quantity: Int) -> Value {
  assets.from_lovelace(quantity)
}

fn with_payout_nft(value: Value) -> Value {
  value |> assets.add(payout_policy, payout_asset, 1)
}

fn value_to_pairs(value: Value) -> Pairs<PolicyId, Pairs<AssetName, Int>> {
  value
    |> assets.to_dict
    |> dict.to_pairs
    |> list.map(
        fn(policy_tokens) {
          Pair(policy_tokens.1st, policy_tokens.2nd |> dict.to_pairs)
        },
      )
}

fn payout_value_pairs(quantity: Int) -> Pairs<PolicyId, Pairs<AssetName, Int>> {
  value_to_pairs(assets.from_lovelace(quantity))
}

fn test_hub_datum() -> hub.Datum {
  let reserve_addr = address.from_script(reserve_policy)
  let payout_addr = address.from_script(payout_policy)
  hub.Datum {
    registered_operators: #"44444444444444444444444444444444444444444444444444444444",
    active_operators: #"45454545454545454545454545454545454545454545454545454545",
    retired_operators: #"46464646464646464646464646464646464646464646464646464646",
    scheduler: #"47474747474747474747474747474747474747474747474747474747",
    state_queue: #"48484848484848484848484848484848484848484848484848484848",
    fraud_proof_catalogue: #"49494949494949494949494949494949494949494949494949494949",
    fraud_proof: #"50505050505050505050505050505050505050505050505050505050",
    deposit: #"51515151515151515151515151515151515151515151515151515151",
    withdrawal: withdrawal_policy,
    tx_order: #"53535353535353535353535353535353535353535353535353535353",
    settlement: #"54545454545454545454545454545454545454545454545454545454",
    payout: payout_policy,
    registered_operators_addr: reserve_addr,
    active_operators_addr: reserve_addr,
    retired_operators_addr: reserve_addr,
    scheduler_addr: reserve_addr,
    state_queue_addr: reserve_addr,
    fraud_proof_catalogue_addr: reserve_addr,
    fraud_proof_addr: reserve_addr,
    deposit_addr: reserve_addr,
    withdrawal_addr: reserve_addr,
    tx_order_addr: reserve_addr,
    settlement_addr: reserve_addr,
    reserve_addr,
    payout_addr,
    reserve_observer: reserve_policy,
  }
}

fn payout_datum() -> payout.Datum {
  payout.Datum {
    l2_value: payout_value_pairs(7),
    l1_address: address.from_script(
      #"66666666666666666666666666666666666666666666666666666666",
    ),
    l1_datum: NoDatum,
  }
}

fn payout_datum_data() -> Data {
  let datum: Data = payout_datum()
  datum
}

fn payout_datum_to_data(datum: payout.Datum) -> Data {
  let datum_data: Data = datum
  datum_data
}

fn hub_ref_input() -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"0303030303030303030303030303030303030303030303030303030303030303",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(hub_policy),
      value: assets.from_asset(hub_policy, hub.asset_name, 1),
      datum: InlineDatum(test_hub_datum()),
      reference_script: None,
    },
  }
}

fn add_funds_tx(
  payout_output_datum: Data,
  reserve_change_address: Address,
  mint: Value,
) -> Transaction {
  let payout_redeemer: Data =
    payout.AddFunds {
      payout_input_index: 1,
      payout_output_index: 0,
      reserve_input_index: 0,
      reserve_change_output_index: Some(1),
      reserve_spend_redeemer_index: 0,
      payout_spend_redeemer_index: 1,
      hub_ref_input_index: 0,
    }
  let reserve_redeemer: Data =
    reserve.Spend {
      reserve_input_index: 0,
      payout_input_index: 1,
      payout_spend_redeemer_index: 1,
      hub_ref_input_index: 0,
    }

  Transaction {
    ..tx.placeholder,
    inputs: [
      Input {
        output_reference: reserve_ref,
        output: Output {
          address: address.from_script(reserve_policy),
          value: ada(8),
          datum: NoDatum,
          reference_script: None,
        },
      },
      Input {
        output_reference: payout_ref,
        output: Output {
          address: address.from_script(payout_policy),
          value: with_payout_nft(ada(2)),
          datum: InlineDatum(payout_datum()),
          reference_script: None,
        },
      },
    ],
    reference_inputs: [hub_ref_input()],
    outputs: [
      Output {
        address: address.from_script(payout_policy),
        value: with_payout_nft(ada(7)),
        datum: InlineDatum(payout_output_datum),
        reference_script: None,
      },
      Output {
        address: reserve_change_address,
        value: ada(3),
        datum: NoDatum,
        reference_script: None,
      },
    ],
    redeemers: [
      Pair(Spend(reserve_ref), reserve_redeemer),
      Pair(Spend(payout_ref), payout_redeemer),
    ],
    mint,
  }
}

fn custom_add_funds_redeemer(
  reserve_change_output_index: Option<Int>,
) -> payout.SpendRedeemer {
  payout.AddFunds {
    payout_input_index: 1,
    payout_output_index: 0,
    reserve_input_index: 0,
    reserve_change_output_index,
    reserve_spend_redeemer_index: 0,
    payout_spend_redeemer_index: 1,
    hub_ref_input_index: 0,
  }
}

fn custom_reserve_redeemer() -> reserve.SpendRedeemer {
  reserve.Spend {
    reserve_input_index: 0,
    payout_input_index: 1,
    payout_spend_redeemer_index: 1,
    hub_ref_input_index: 0,
  }
}

fn payout_input_with(value: Value, datum: payout.Datum) -> Input {
  Input {
    output_reference: payout_ref,
    output: Output {
      address: address.from_script(payout_policy),
      value: value |> assets.add(payout_policy, payout_asset, 1),
      datum: InlineDatum(payout_datum_to_data(datum)),
      reference_script: None,
    },
  }
}

fn reserve_input_with(value: Value) -> Input {
  Input {
    output_reference: reserve_ref,
    output: Output {
      address: address.from_script(reserve_policy),
      value,
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn payout_output_with(value: Value, datum: payout.Datum) -> Output {
  Output {
    address: address.from_script(payout_policy),
    value: value |> assets.add(payout_policy, payout_asset, 1),
    datum: InlineDatum(payout_datum_to_data(datum)),
    reference_script: None,
  }
}

fn reserve_change_output(value: Value) -> Output {
  Output {
    address: address.from_script(reserve_policy),
    value,
    datum: NoDatum,
    reference_script: None,
  }
}

fn extra_payout_input() -> Input {
  Input {
    output_reference: other_payout_ref,
    output: Output {
      address: address.from_script(payout_policy),
      value: ada(2) |> assets.add(payout_policy, other_payout_asset, 1),
      datum: InlineDatum(payout_datum_data()),
      reference_script: None,
    },
  }
}

fn extra_reserve_input() -> Input {
  Input {
    output_reference: other_reserve_ref,
    output: Output {
      address: address.from_script(reserve_policy),
      value: ada(1),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn extra_payout_output() -> Output {
  Output {
    address: address.from_script(payout_policy),
    value: ada(2) |> assets.add(payout_policy, other_payout_asset, 1),
    datum: InlineDatum(payout_datum_data()),
    reference_script: None,
  }
}

fn custom_add_funds_tx(
  datum: payout.Datum,
  current_value: Value,
  reserve_value: Value,
  payout_output_value_without_nft: Value,
  reserve_change: Option<Value>,
  extra_inputs: List<Input>,
  extra_outputs: List<Output>,
) -> Transaction {
  let payout_redeemer_data: Data =
    custom_add_funds_redeemer(
      when reserve_change is {
        None -> None
        Some(_) -> Some(1)
      },
    )
  let reserve_redeemer_data: Data = custom_reserve_redeemer()
  Transaction {
    ..tx.placeholder,
    inputs: [
      reserve_input_with(reserve_value),
      payout_input_with(current_value, datum),
      ..extra_inputs
    ],
    reference_inputs: [hub_ref_input()],
    outputs: [
      payout_output_with(payout_output_value_without_nft, datum),
      ..when reserve_change is {
        None -> extra_outputs
        Some(change_value) ->
          [reserve_change_output(change_value), ..extra_outputs]
      }
    ],
    redeemers: [
      Pair(Spend(reserve_ref), reserve_redeemer_data),
      Pair(Spend(payout_ref), payout_redeemer_data),
    ],
  }
}

fn multi_asset_value(lovelace: Int, token_quantity: Int) -> Value {
  ada(lovelace) |> assets.add(token_policy, token_asset, token_quantity)
}

fn multi_asset_payout_datum() -> payout.Datum {
  payout.Datum {
    l2_value: value_to_pairs(multi_asset_value(7, 4)),
    l1_address: payout_datum().l1_address,
    l1_datum: NoDatum,
  }
}

test payout_add_funds_accepts_exact_collection() {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout.AddFunds {
      payout_input_index: 1,
      payout_output_index: 0,
      reserve_input_index: 0,
      reserve_change_output_index: Some(1),
      reserve_spend_redeemer_index: 0,
      payout_spend_redeemer_index: 1,
      hub_ref_input_index: 0,
    },
    payout_ref,
    add_funds_tx(
      payout_datum_data(),
      address.from_script(reserve_policy),
      assets.zero,
    ),
  )
}

test payout_add_funds_rejects_datum_mutation() fail {
  let mutated_datum: Data =
    payout.Datum {
      l2_value: payout_value_pairs(8),
      l1_address: address.from_script(
        #"66666666666666666666666666666666666666666666666666666666",
      ),
      l1_datum: NoDatum,
    }
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout.AddFunds {
      payout_input_index: 1,
      payout_output_index: 0,
      reserve_input_index: 0,
      reserve_change_output_index: Some(1),
      reserve_spend_redeemer_index: 0,
      payout_spend_redeemer_index: 1,
      hub_ref_input_index: 0,
    },
    payout_ref,
    add_funds_tx(
      mutated_datum,
      address.from_script(reserve_policy),
      assets.zero,
    ),
  )
}

test payout_add_funds_rejects_wrong_reserve_change_address() fail {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout.AddFunds {
      payout_input_index: 1,
      payout_output_index: 0,
      reserve_input_index: 0,
      reserve_change_output_index: Some(1),
      reserve_spend_redeemer_index: 0,
      payout_spend_redeemer_index: 1,
      hub_ref_input_index: 0,
    },
    payout_ref,
    add_funds_tx(
      payout_datum_data(),
      address.from_script(
        #"77777777777777777777777777777777777777777777777777777777",
      ),
      assets.zero,
    ),
  )
}

test payout_add_funds_rejects_unrelated_mint() fail {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout.AddFunds {
      payout_input_index: 1,
      payout_output_index: 0,
      reserve_input_index: 0,
      reserve_change_output_index: Some(1),
      reserve_spend_redeemer_index: 0,
      payout_spend_redeemer_index: 1,
      hub_ref_input_index: 0,
    },
    payout_ref,
    add_funds_tx(
      payout_datum_data(),
      address.from_script(reserve_policy),
      assets.from_asset(
        #"88888888888888888888888888888888888888888888888888888888",
        #"01",
        1,
      ),
    ),
  )
}

test payout_add_funds_accepts_exact_multi_asset_collection() {
  let datum = multi_asset_payout_datum()
  spend.spend(
    hub_policy,
    Some(datum),
    custom_add_funds_redeemer(Some(1)),
    payout_ref,
    custom_add_funds_tx(
      datum,
      multi_asset_value(2, 1),
      multi_asset_value(8, 10),
      multi_asset_value(7, 4),
      Some(multi_asset_value(3, 7)),
      [],
      [],
    ),
  )
}

test payout_add_funds_accepts_deterministic_partial_then_final_collection() {
  let datum = multi_asset_payout_datum()
  let first_collection =
    custom_add_funds_tx(
      datum,
      multi_asset_value(2, 1),
      multi_asset_value(2, 1),
      multi_asset_value(4, 2),
      None,
      [],
      [],
    )
  let final_collection =
    custom_add_funds_tx(
      datum,
      multi_asset_value(4, 2),
      multi_asset_value(8, 10),
      multi_asset_value(7, 4),
      Some(multi_asset_value(5, 8)),
      [],
      [],
    )

  and {
    spend.spend(
      hub_policy,
      Some(datum),
      custom_add_funds_redeemer(None),
      payout_ref,
      first_collection,
    ),
    spend.spend(
      hub_policy,
      Some(datum),
      custom_add_funds_redeemer(Some(1)),
      payout_ref,
      final_collection,
    ),
  }
}

test payout_add_funds_rejects_no_positive_needed_asset_contribution() fail {
  let datum = multi_asset_payout_datum()
  let unrelated_value =
    assets.from_asset(unused_token_policy, unused_token_asset, 1)
  spend.spend(
    hub_policy,
    Some(datum),
    custom_add_funds_redeemer(Some(1)),
    payout_ref,
    custom_add_funds_tx(
      datum,
      multi_asset_value(2, 1),
      unrelated_value,
      multi_asset_value(2, 1),
      Some(unrelated_value),
      [],
      [],
    ),
  )
}

test payout_add_funds_rejects_overfunding_above_target() fail {
  let datum = multi_asset_payout_datum()
  spend.spend(
    hub_policy,
    Some(datum),
    custom_add_funds_redeemer(Some(1)),
    payout_ref,
    custom_add_funds_tx(
      datum,
      multi_asset_value(2, 1),
      multi_asset_value(8, 10),
      multi_asset_value(8, 4),
      Some(multi_asset_value(2, 7)),
      [],
      [],
    ),
  )
}

test payout_add_funds_rejects_second_payout_policy_input() fail {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    custom_add_funds_redeemer(Some(1)),
    payout_ref,
    custom_add_funds_tx(
      payout_datum(),
      ada(2),
      ada(8),
      ada(7),
      Some(ada(3)),
      [extra_payout_input()],
      [],
    ),
  )
}

test payout_add_funds_rejects_second_reserve_input() fail {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    custom_add_funds_redeemer(Some(1)),
    payout_ref,
    custom_add_funds_tx(
      payout_datum(),
      ada(2),
      ada(8),
      ada(7),
      Some(ada(3)),
      [extra_reserve_input()],
      [],
    ),
  )
}

test payout_add_funds_rejects_second_payout_policy_output() fail {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    custom_add_funds_redeemer(Some(1)),
    payout_ref,
    custom_add_funds_tx(
      payout_datum(),
      ada(2),
      ada(8),
      ada(7),
      Some(ada(3)),
      [],
      [extra_payout_output()],
    ),
  )
}

fn payout_conclusion_spend_redeemer() -> payout.SpendRedeemer {
  payout.ConcludeWithdrawal {
    payout_input_index: 0,
    l1_output_index: 0,
    burn_redeemer_index: 1,
    hub_ref_input_index: 0,
  }
}

fn payout_burn_redeemer() -> payout.MintRedeemer {
  payout.BurnPayout {
    payout_input_index: 0,
    payout_asset_name: payout_asset,
    payout_spend_redeemer_index: 0,
    hub_ref_input_index: 0,
  }
}

fn payout_destination_output(lovelace: Int) -> Output {
  Output {
    address: payout_datum().l1_address,
    value: ada(lovelace),
    datum: NoDatum,
    reference_script: None,
  }
}

fn payout_conclusion_tx(
  payout_lovelace: Int,
  output: Output,
  extra_outputs: List<Output>,
  minted_value: Value,
) -> Transaction {
  let spend_redeemer_data: Data = payout_conclusion_spend_redeemer()
  let burn_redeemer_data: Data = payout_burn_redeemer()
  Transaction {
    ..tx.placeholder,
    inputs: [
      Input {
        output_reference: payout_ref,
        output: Output {
          address: address.from_script(payout_policy),
          value: with_payout_nft(ada(payout_lovelace)),
          datum: InlineDatum(payout_datum_data()),
          reference_script: None,
        },
      },
    ],
    reference_inputs: [hub_ref_input()],
    outputs: [output, ..extra_outputs],
    redeemers: [
      Pair(Spend(payout_ref), spend_redeemer_data),
      Pair(Mint(payout_policy), burn_redeemer_data),
    ],
    mint: minted_value,
  }
}

fn valid_payout_conclusion_tx() -> Transaction {
  payout_conclusion_tx(
    7,
    payout_destination_output(7),
    [],
    assets.from_asset(payout_policy, payout_asset, -1),
  )
}

test payout_conclude_accepts_exact_burn_and_destination() {
  and {
    spend.spend(
      hub_policy,
      Some(payout_datum()),
      payout_conclusion_spend_redeemer(),
      payout_ref,
      valid_payout_conclusion_tx(),
    ),
    mint.mint(
      hub_policy,
      payout_burn_redeemer(),
      payout_policy,
      valid_payout_conclusion_tx(),
    ),
  }
}

test payout_conclude_rejects_wrong_destination_address() fail {
  let bad_output =
    Output {
      ..payout_destination_output(7),
      address: address.from_script(
        #"99999999999999999999999999999999999999999999999999999999",
      ),
    }
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout_conclusion_spend_redeemer(),
    payout_ref,
    payout_conclusion_tx(
      7,
      bad_output,
      [],
      assets.from_asset(payout_policy, payout_asset, -1),
    ),
  )
}

test payout_conclude_rejects_surplus_accumulator_value() fail {
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout_conclusion_spend_redeemer(),
    payout_ref,
    payout_conclusion_tx(
      8,
      payout_destination_output(7),
      [],
      assets.from_asset(payout_policy, payout_asset, -1),
    ),
  )
}

test payout_conclude_rejects_continuing_payout_output() fail {
  let continuation =
    Output {
      address: address.from_script(payout_policy),
      value: ada(2),
      datum: InlineDatum(payout_datum_data()),
      reference_script: None,
    }
  spend.spend(
    hub_policy,
    Some(payout_datum()),
    payout_conclusion_spend_redeemer(),
    payout_ref,
    payout_conclusion_tx(
      7,
      payout_destination_output(7),
      [continuation],
      assets.from_asset(payout_policy, payout_asset, -1),
    ),
  )
}

test payout_burn_rejects_unrelated_mint() fail {
  mint.mint(
    hub_policy,
    payout_burn_redeemer(),
    payout_policy,
    payout_conclusion_tx(
      7,
      payout_destination_output(7),
      [],
      assets.merge(
        assets.from_asset(payout_policy, payout_asset, -1),
        assets.from_asset(
          #"88888888888888888888888888888888888888888888888888888888",
          #"01",
          1,
        ),
      ),
    ),
  )
}

fn withdrawal_spend_redeemer_data() -> Data {
  let redeemer: Data =
    withdrawal_lib.SpendRedeemer {
      input_index: 0,
      output_index: 0,
      hub_ref_input_index: 0,
      settlement_ref_input_index: 0,
      burn_redeemer_index: 0,
      payout_mint_redeemer_index: 1,
      membership_proof: [],
      inclusion_proof_script_withdraw_redeemer_index: 0,
      purpose: withdrawal_lib.InitializePayout,
    }
  redeemer
}

fn mint_payout_tx(minted_value: Value) -> Transaction {
  let mint_redeemer: Data =
    payout.MintPayout {
      withdrawal_utxo_out_ref: withdrawal_ref,
      withdrawal_input_index: 0,
      withdrawal_spend_redeemer_index: 0,
      hub_ref_input_index: 0,
    }
  Transaction {
    ..tx.placeholder,
    inputs: [
      Input {
        output_reference: withdrawal_ref,
        output: Output {
          address: address.from_script(reserve_policy),
          value: ada(2) |> assets.add(withdrawal_policy, payout_asset, 1),
          datum: InlineDatum(#""),
          reference_script: None,
        },
      },
    ],
    reference_inputs: [hub_ref_input()],
    redeemers: [
      Pair(Spend(withdrawal_ref), withdrawal_spend_redeemer_data()),
      Pair(Mint(payout_policy), mint_redeemer),
    ],
    mint: minted_value,
  }
}

test payout_mint_accepts_matching_withdrawal_burn() {
  mint.mint(
    hub_policy,
    payout.MintPayout {
      withdrawal_utxo_out_ref: withdrawal_ref,
      withdrawal_input_index: 0,
      withdrawal_spend_redeemer_index: 0,
      hub_ref_input_index: 0,
    },
    payout_policy,
    mint_payout_tx(
      assets.merge(
        assets.from_asset(payout_policy, payout_asset, 1),
        assets.from_asset(withdrawal_policy, payout_asset, -1),
      ),
    ),
  )
}

test payout_mint_rejects_asset_name_mismatch() fail {
  mint.mint(
    hub_policy,
    payout.MintPayout {
      withdrawal_utxo_out_ref: withdrawal_ref,
      withdrawal_input_index: 0,
      withdrawal_spend_redeemer_index: 0,
      hub_ref_input_index: 0,
    },
    payout_policy,
    mint_payout_tx(
      assets.merge(
        assets.from_asset(payout_policy, #"bb", 1),
        assets.from_asset(withdrawal_policy, payout_asset, -1),
      ),
    ),
  )
}
