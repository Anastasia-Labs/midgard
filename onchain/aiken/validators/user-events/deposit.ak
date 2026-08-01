use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval
use aiken_design_patterns/parameter_validation
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{RegisterCredential}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Publish,
  Transaction,
}
use cardano/transaction as tx
use env
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/deposit.{Datum, SpendRedeemer}
use midgard/user_events/witness as event_witness

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

    let input_index = redeemer.input_index
    let output_index = redeemer.output_index
    let hub_ref_input_index = redeemer.hub_ref_input_index
    let settlement_ref_input_index = redeemer.settlement_ref_input_index
    let mint_redeemer_index = redeemer.mint_redeemer_index
    let membership_proof = redeemer.membership_proof
    let inclusion_proof_script_withdraw_redeemer_index = redeemer.inclusion_proof_script_withdraw_redeemer_index

    let hub_datum = hub.get_datum(
      reference_inputs,
      hub_oracle,
      hub_ref_input_index,
    )
    let deposit_policy_id = hub_datum.deposit
    let settlement_policy_id = hub_datum.settlement
    let reserve_addr = hub_datum.reserve_addr

    let settlement.Datum { deposits_root, .. } = reference_inputs
      |> settlement.get_datum(settlement_policy_id, settlement_ref_input_index)

    expect Some(deposit_datum) = datum
    let Datum { event: deposit_event, .. } = deposit_datum

    // 1. Spending a deposit UTxO must also be accompanied by the burning of its
    //    NFT (and consequently deregistration of its witness staking script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. } = utils.get_redeemer_at(
      redeemers,
      Mint(deposit_policy_id),
      mint_redeemer_index,
    )

    let input, output <- singular_utxo_indexer.one_to_one(
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
      // Double satisfaction prevented by the requirement for unregistration of
      // the witness staking script.
      True,
    )
    //
    let Input { output: Output { value: own_value, .. }, .. } = input

    // 2. The produced UTxO must not have any scripts attached to it.
    expect None = output.reference_script
    let output_address = output.address
    let output_value = output.value
    let output_datum = output.datum

    // 3. Output value must be the same as the input value, except for the
    //    deposit NFT.
    //    TODO: Reserve contract might end up requiring an NFT.
    //    TODO: Fees must be deduced whenever we have a clear plan for them.
    expect
      output_value == (
        own_value |> assets.add(deposit_policy_id, burn_asset_name, -1)
      )

    let Pair(deposit_event_id, deposit_event_info) = ledger_state.unsafe_event_to_key_value_pair(
      deposit_event,
    )
    expect deposit_id: ledger_state.DepositId = deposit_event_id
    expect deposit_info: ledger_state.DepositInfo = deposit_event_info

    // 4. The referenced settlement utxo's deposit tree must contain the
    //    subject deposit event.
    expect
      settlement.valid_counted_membership(
        expected_domain: transition_trace.DepositsRootDomain,
        expected_root: deposits_root,
        witness: membership_proof,
        key: deposit_id,
        value: deposit_info,
        redeemers: redeemers,
        inclusion_proof_script_withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )

    // 5. Assert no datum is attached to the produced UTxO.
    //    TODO: This might change depending on implementation of reserve.
    expect output_datum == NoDatum

    // 6. Deposit must go to the reserve.
    expect output_address == reserve_addr

    // Done.
    True
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(
    redeemer: user_events.MintRedeemer,
    own_policy: PolicyId,
    tx: Transaction,
  ) {
    // 1. Perform numerous validations and grant access to:
    //    - Serialized and hashed nonce based on the output reference of a
    //      specified input UTxO
    //    - The output value of the UTxO at the deposit address
    user_events.validate_mint(
      hub_oracle,
      fn(hub_datum) {
        let hub.Datum { deposit_addr, .. } = hub_datum
        deposit_addr
      },
      redeemer,
      own_policy,
      tx,
      fn(l2_id, output_deposit_value, _deposit_info) {
        // 2. Going through all the tokens of the produced UTxO and counting its
        //    non-NFT assets, while also validating that it only contains one
        //    valid authentication NFT.
        let output_deposit_nft_is_valid, total_token_count <- list.foldl2(
          output_deposit_value |> assets.to_dict |> dict.to_pairs,
          False,
          0,
          fn(policy_tokens_pair, state_nft_found, token_count_so_far, return) {
            if !state_nft_found && policy_tokens_pair.1st == own_policy {
              expect [output_deposit_nft_tn_qty] = policy_tokens_pair.2nd
                |> dict.to_pairs
              expect output_deposit_nft_tn_qty == Pair(l2_id, 1)
              return(True, token_count_so_far)
            } else {
              let policy_token_count = dict.size(policy_tokens_pair.2nd)
              return(state_nft_found, token_count_so_far + policy_token_count)
            }
          },
        )

        and {
          // 3. Deposit NFT must be included in the produced UTxO.
          output_deposit_nft_is_valid,
          // 4. The number of non-NFT assets in the deposit UTxO, including
          //    ADA, must not exceed the relevant protocol parameter.
          total_token_count <= env.max_tokens_allowed_in_deposits,
        }
      },
    )
  }

  else(_) {
    fail
  }
}

const test_hub_oracle =
  #"11111111111111111111111111111111111111111111111111111111"

const test_deposit_policy =
  #"22222222222222222222222222222222222222222222222222222222"

const test_asset_policy_a =
  #"33333333333333333333333333333333333333333333333333333333"

const test_asset_policy_b =
  #"44444444444444444444444444444444444444444444444444444444"

const test_auxiliary_policy =
  #"55555555555555555555555555555555555555555555555555555555"

const test_nonce_ref =
  OutputReference {
    transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    output_index: 0,
  }

const test_valid_to = 2_000

fn test_hub_datum() -> hub.Datum {
  let auxiliary_address = address.from_script(test_auxiliary_policy)
  hub.Datum {
    registered_operators: test_auxiliary_policy,
    active_operators: test_auxiliary_policy,
    retired_operators: test_auxiliary_policy,
    scheduler: test_auxiliary_policy,
    state_queue: test_auxiliary_policy,
    fraud_proof_catalogue: test_auxiliary_policy,
    fraud_proof: test_auxiliary_policy,
    deposit: test_deposit_policy,
    withdrawal: test_auxiliary_policy,
    tx_order: test_auxiliary_policy,
    settlement: test_auxiliary_policy,
    payout: test_auxiliary_policy,
    registered_operators_addr: auxiliary_address,
    active_operators_addr: auxiliary_address,
    retired_operators_addr: auxiliary_address,
    scheduler_addr: auxiliary_address,
    state_queue_addr: auxiliary_address,
    fraud_proof_catalogue_addr: auxiliary_address,
    fraud_proof_addr: auxiliary_address,
    deposit_addr: address.from_script(test_deposit_policy),
    withdrawal_addr: auxiliary_address,
    tx_order_addr: auxiliary_address,
    settlement_addr: auxiliary_address,
    reserve_addr: auxiliary_address,
    payout_addr: auxiliary_address,
    reserve_observer: test_auxiliary_policy,
  }
}

fn test_hub_reference_input() -> Input {
  let hub_datum_data: Data = test_hub_datum()
  Input {
    output_reference: OutputReference {
      transaction_id: #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(test_hub_oracle),
      value: assets.from_asset(test_hub_oracle, hub.asset_name, 1),
      datum: InlineDatum(hub_datum_data),
      reference_script: None,
    },
  }
}

fn test_nonce_input() -> Input {
  Input {
    output_reference: test_nonce_ref,
    output: Output {
      address: address.from_script(test_auxiliary_policy),
      value: assets.from_lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn test_deposit_value(include_eleventh_non_nft_asset: Bool) {
  let ten_non_nft_assets = assets.from_lovelace(3_000_000)
    |> assets.add(test_asset_policy_a, #"01", 1)
    |> assets.add(test_asset_policy_a, #"02", 1)
    |> assets.add(test_asset_policy_a, #"03", 1)
    |> assets.add(test_asset_policy_a, #"04", 1)
    |> assets.add(test_asset_policy_a, #"05", 1)
    |> assets.add(test_asset_policy_b, #"11", 1)
    |> assets.add(test_asset_policy_b, #"12", 1)
    |> assets.add(test_asset_policy_b, #"13", 1)
    |> assets.add(test_asset_policy_b, #"14", 1)

  let non_nft_assets = if include_eleventh_non_nft_asset {
    ten_non_nft_assets |> assets.add(test_asset_policy_b, #"15", 1)
  } else {
    ten_non_nft_assets
  }

  non_nft_assets
    |> assets.add(
        test_deposit_policy,
        user_events.out_ref_to_nonce(test_nonce_ref),
        1,
      )
}

fn test_deposit_datum(inclusion_time: Int) -> Datum {
  let nonce = user_events.out_ref_to_nonce(test_nonce_ref)
  Datum {
    event: ledger_state.DepositEvent {
      id: test_nonce_ref,
      info: ledger_state.DepositInfo {
        l2_address: address.from_script(test_auxiliary_policy),
        l2_network_id: 0,
        l2_datum: None,
      },
    },
    inclusion_time,
    witness: parameter_validation.apply_prehashed_param(
      version: env.plutus_version,
      prefix: env.user_events_witness_script_prefix,
      param: nonce,
    ),
  }
}

fn test_authenticate_redeemer() -> user_events.MintRedeemer {
  user_events.AuthenticateEvent {
    nonce_input_index: 0,
    event_output_index: 0,
    hub_ref_input_index: 0,
    witness_registration_redeemer_index: 0,
  }
}

fn test_mint_transaction(
  inclusion_time: Int,
  include_eleventh_non_nft_asset: Bool,
) -> Transaction {
  let nonce = user_events.out_ref_to_nonce(test_nonce_ref)
  let witness_script_hash = parameter_validation.apply_prehashed_param(
    version: env.plutus_version,
    prefix: env.user_events_witness_script_prefix,
    param: nonce,
  )
  let witness_certificate = RegisterCredential(
    Script(witness_script_hash),
    Never,
  )
  let witness_redeemer_data: Data = event_witness.MintOrBurn {
    target_policy: test_deposit_policy,
  }
  let deposit_datum_data: Data = test_deposit_datum(inclusion_time)

  Transaction {
    ..tx.placeholder,
    inputs: [test_nonce_input()],
    reference_inputs: [test_hub_reference_input()],
    outputs: [
      Output {
        address: address.from_script(test_deposit_policy),
        value: test_deposit_value(include_eleventh_non_nft_asset),
        datum: InlineDatum(deposit_datum_data),
        reference_script: None,
      },
    ],
    mint: assets.from_asset(test_deposit_policy, nonce, 1),
    validity_range: interval.between(1_000, test_valid_to),
    redeemers: [
      Pair(
        Publish { at: 0, certificate: witness_certificate },
        witness_redeemer_data,
      ),
    ],
  }
}

fn test_deposit_mint(
  inclusion_time: Int,
  include_eleventh_non_nft_asset: Bool,
) -> Bool {
  mint.mint(
    test_hub_oracle,
    test_authenticate_redeemer(),
    test_deposit_policy,
    test_mint_transaction(inclusion_time, include_eleventh_non_nft_asset),
  )
}

test deposit_mint_accepts_exactly_ten_non_nft_assets_across_ada_and_multiple_policies() {
  test_deposit_mint(test_valid_to + env.event_wait_duration, False)
}

test deposit_mint_rejects_eleven_non_nft_assets_across_ada_and_multiple_policies() fail {
  test_deposit_mint(test_valid_to + env.event_wait_duration, True)
}

test deposit_mint_rejects_inclusion_time_minus_one() fail {
  test_deposit_mint(test_valid_to + env.event_wait_duration - 1, False)
}

test deposit_mint_rejects_inclusion_time_plus_one() fail {
  test_deposit_mint(test_valid_to + env.event_wait_duration + 1, False)
}
