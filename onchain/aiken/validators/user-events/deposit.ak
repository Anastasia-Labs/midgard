use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Transaction,
}
use env
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement
use midgard/user_events
use midgard/user_events/deposit.{Datum, SpendRedeemer}

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
      mint_redeemer_index,
      membership_proof,
      inclusion_proof_script_withdraw_redeemer_index,
    } = redeemer

    let hub.Datum {
      deposit: deposit_policy_id,
      settlement: settlement_policy_id,
      reserve_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let settlement.Datum { deposits_root, .. } =
      reference_inputs
        |> settlement.get_datum(
            settlement_policy_id,
            settlement_ref_input_index,
          )

    expect Some(deposit_datum) = datum
    let Datum { event: deposit_event, .. } = deposit_datum

    // 1. Spending a deposit UTxO must also be accompanied by the burning of its
    //    NFT (and consequently deregistration of its witness staking script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. } =
      utils.get_redeemer_at(
        redeemers,
        Mint(deposit_policy_id),
        mint_redeemer_index,
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
    //
    let Input { output: Output { value: own_value, .. }, .. } = input

    // 2. The produced UTxO must not have any scripts attached to it.
    expect Output {
      address: output_address,
      value: output_value,
      datum: output_datum,
      reference_script: None,
    } = output

    // 3. Output value must be the same as the input value, except for the
    //    deposit NFT.
    //    TODO: Reserve contract might end up requiring an NFT.
    //    TODO: Fees must be deduced whenever we have a clear plan for them.
    expect
      output_value == (
        own_value |> assets.add(deposit_policy_id, burn_asset_name, -1)
      )

    let Pair(deposit_event_id, deposit_event_info) =
      ledger_state.unsafe_event_to_key_value_pair(deposit_event)

    // 4. The referenced settlement utxo's deposit tree must contain the
    //    subject deposit event.
    expect
      plutarch_phas(
        deposits_root,
        deposit_event_id,
        deposit_event_info,
        membership_proof,
        redeemers,
        inclusion_proof_script_withdraw_redeemer_index,
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
    let
      l2_id,
      output_deposit_value,
      _,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { deposit_addr, .. } = hub_datum
          deposit_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    // 2. Going through all the tokens of the produced UTxO and counting its
    //    tokens, while also validating that it only contains one valid
    //    authentication NFT.
    let
      output_deposit_nft_is_valid,
      total_token_count,
    <-
      list.foldl2(
        output_deposit_value |> assets.to_dict |> dict.to_pairs,
        False,
        0,
        fn(policy_tokens_pair, state_nft_found, token_count_so_far, return) {
          if !state_nft_found && policy_tokens_pair.1st == own_policy {
            expect [output_deposit_nft_tn_qty] =
              policy_tokens_pair.2nd |> dict.to_pairs
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
      // 4. Total token count in the deposit UTxO (including ADA) must not
      //    exceed the relevant protocol parameter.
      total_token_count <= env.max_tokens_allowed_in_deposits,
    }
  }

  else(_) {
    fail
  }
}
