use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
}
use cardano/transaction as tx
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/payout
use midgard/reserve

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Data>,
    redeemer: reserve.SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, reference_inputs, redeemers, mint, .. } = tx

    expect datum == None

    let reserve.Spend {
      reserve_input_index,
      payout_input_index,
      payout_spend_redeemer_index,
      hub_ref_input_index,
    } = redeemer

    let hub.Datum { payout: payout_policy_id, payout_addr, reserve_addr, .. } =
      hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    expect Some(Input {
      output_reference: reserve_out_ref,
      output: Output {
        address: reserve_input_address,
        datum: NoDatum,
        reference_script: None,
        ..
      },
    }) = inputs |> list.at(reserve_input_index)
    expect reserve_out_ref == own_out_ref
    expect reserve_input_address == reserve_addr

    expect Some(Input {
      output_reference: payout_out_ref,
      output: Output {
        address: payout_input_address,
        value: payout_input_value,
        datum: InlineDatum(_),
        ..
      },
    }) = inputs |> list.at(payout_input_index)
    expect payout_input_address == payout_addr
    expect
      utils.quantity_of_policy_id(payout_input_value, payout_policy_id) == 1

    expect payout.AddFunds {
      reserve_input_index: payout_redeemer_reserve_input_index,
      payout_input_index: payout_redeemer_payout_input_index,
      reserve_spend_redeemer_index,
      hub_ref_input_index: payout_redeemer_hub_ref_input_index,
      payout_spend_redeemer_index: payout_redeemer_payout_spend_redeemer_index,
      ..
    }: payout.SpendRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Spend(payout_out_ref),
        payout_spend_redeemer_index,
      )

    expect payout_redeemer_reserve_input_index == reserve_input_index
    expect payout_redeemer_payout_input_index == payout_input_index
    expect payout_redeemer_hub_ref_input_index == hub_ref_input_index
    expect
      payout_redeemer_payout_spend_redeemer_index == payout_spend_redeemer_index

    expect reserve.Spend { reserve_input_index: self_reserve_input_index, .. }: reserve.SpendRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Spend(own_out_ref),
        reserve_spend_redeemer_index,
      )
    expect self_reserve_input_index == reserve_input_index

    expect mint == assets.zero

    True
  }

  else(_) {
    fail
  }
}

validator withdraw {
  else(_) {
    // `reserve_observer` is intentionally deferred. Keep this fail-closed until
    // the observer protocol is specified and reviewed.
    fail
  }
}

const hub_policy = #"11111111111111111111111111111111111111111111111111111111"

const payout_policy =
  #"22222222222222222222222222222222222222222222222222222222"

const reserve_policy =
  #"33333333333333333333333333333333333333333333333333333333"

const payout_asset = #"aa"

const reserve_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }

const payout_ref =
  OutputReference {
    transaction_id: #"0202020202020202020202020202020202020202020202020202020202020202",
    output_index: 0,
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
    withdrawal: #"52525252525252525252525252525252525252525252525252525252",
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

fn payout_datum_data() -> Data {
  let datum: Data =
    payout.Datum {
      l2_value: [Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, 7)])],
      l1_address: address.from_script(
        #"66666666666666666666666666666666666666666666666666666666",
      ),
      l1_datum: NoDatum,
    }
  datum
}

fn reserve_redeemer() -> reserve.SpendRedeemer {
  reserve.Spend {
    reserve_input_index: 0,
    payout_input_index: 1,
    payout_spend_redeemer_index: 1,
    hub_ref_input_index: 0,
  }
}

fn add_funds_redeemer(reserve_input_index: Int) -> payout.SpendRedeemer {
  payout.AddFunds {
    payout_input_index: 1,
    payout_output_index: 0,
    reserve_input_index,
    reserve_change_output_index: Some(1),
    reserve_spend_redeemer_index: 0,
    payout_spend_redeemer_index: 1,
    hub_ref_input_index: 0,
  }
}

fn reserve_test_tx(payout_redeemer: payout.SpendRedeemer) -> Transaction {
  let reserve_redeemer_data: Data = reserve_redeemer()
  let payout_redeemer_data: Data = payout_redeemer
  Transaction {
    ..tx.placeholder,
    inputs: [
      Input {
        output_reference: reserve_ref,
        output: Output {
          address: address.from_script(reserve_policy),
          value: assets.from_lovelace(8),
          datum: NoDatum,
          reference_script: None,
        },
      },
      Input {
        output_reference: payout_ref,
        output: Output {
          address: address.from_script(payout_policy),
          value: assets.from_lovelace(2)
            |> assets.add(payout_policy, payout_asset, 1),
          datum: InlineDatum(payout_datum_data()),
          reference_script: None,
        },
      },
    ],
    reference_inputs: [hub_ref_input()],
    redeemers: [
      Pair(Spend(reserve_ref), reserve_redeemer_data),
      Pair(Spend(payout_ref), payout_redeemer_data),
    ],
  }
}

test reserve_spend_accepts_matching_payout_add_funds() {
  let redeemer = reserve_redeemer()
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    reserve_test_tx(add_funds_redeemer(0)),
  )
}

test reserve_spend_rejects_wrong_payout_branch() fail {
  let redeemer = reserve_redeemer()
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    reserve_test_tx(
      payout.ConcludeWithdrawal {
        payout_input_index: 1,
        l1_output_index: 0,
        burn_redeemer_index: 0,
        hub_ref_input_index: 0,
      },
    ),
  )
}

test reserve_spend_rejects_payout_redeemer_pointing_at_different_reserve() fail {
  let redeemer = reserve_redeemer()
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    reserve_test_tx(add_funds_redeemer(1)),
  )
}

test reserve_spend_rejects_missing_payout_input() fail {
  let redeemer = reserve_redeemer()
  let base = reserve_test_tx(add_funds_redeemer(0))
  let Transaction { inputs, .. } = base
  expect Some(reserve_input) = inputs |> list.at(0)
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    Transaction { ..base, inputs: [reserve_input] },
  )
}

test reserve_spend_rejects_wrong_payout_redeemer_index() fail {
  let redeemer =
    reserve.Spend { ..reserve_redeemer(), payout_spend_redeemer_index: 0 }
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    reserve_test_tx(add_funds_redeemer(0)),
  )
}

test reserve_spend_rejects_unrelated_mint() fail {
  let redeemer = reserve_redeemer()
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    Transaction {
      ..reserve_test_tx(add_funds_redeemer(0)),
      mint: assets.from_asset(
        #"88888888888888888888888888888888888888888888888888888888",
        #"01",
        1,
      ),
    },
  )
}

test reserve_spend_rejects_reserve_input_with_datum() fail {
  let redeemer = reserve_redeemer()
  let base = reserve_test_tx(add_funds_redeemer(0))
  let bad_reserve_input =
    Input {
      output_reference: reserve_ref,
      output: Output {
        address: address.from_script(reserve_policy),
        value: assets.from_lovelace(8),
        datum: InlineDatum(#""),
        reference_script: None,
      },
    }
  let Transaction { inputs, .. } = base
  expect Some(payout_input) = inputs |> list.at(1)
  spend.spend(
    hub_policy,
    None,
    redeemer,
    reserve_ref,
    Transaction { ..base, inputs: [bad_reserve_input, payout_input] },
  )
}
