use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets.{Lovelace, PolicyId}
use cardano/script_context.{ScriptInfo}
use cardano/transaction.{Redeemer, ScriptPurpose}
use env
use midgard/common/types.{ValuePairs}
use midgard/common/utils
use midgard/ledger_state
use midgard/user_events
use midgard/user_events/deposit

/// TODO: Bringing this from `config` is probably much better.
const deposit_policy_id: PolicyId =
  #"00000000000000000000000000000000000000000000000000000000"

fn wrapper(
  l1_observer: ScriptHash,
  l2_observer: ScriptHash,
  script_ctx_data: Data,
  validator_function: fn(ScriptHash, ScriptHash, Data) -> Bool,
) -> Bool {
  validator_function(l1_observer, l2_observer, script_ctx_data)
}

validator main(outer_l1_observer: ScriptHash, outer_l2_observer: ScriptHash) {
  else(outer_script_ctx_data: Data) {
    let
      l1_observer,
      l2_observer,
      script_ctx_data,
    <- wrapper(outer_l1_observer, outer_l2_observer, outer_script_ctx_data)
    let Pair(layer_index, ctx_data_list) =
      builtin.un_constr_data(script_ctx_data)

    expect [tx_data, redeemer_data, script_info_data] = ctx_data_list

    // At this point we only care about the minting purpose. `Minting`
    // constructor for `ScriptInfo` is equivalent to `Constr 0 [own_policy]`.
    let Pair(script_info_index, script_info_data_list) =
      builtin.un_constr_data(script_info_data)
    let own_policy_data = builtin.head_list(script_info_data_list)

    if layer_index == 0 && script_info_index == 0 {
      expect [
        inputs_data, ref_inputs_data, outputs_data, fee_data, mint_data,
        certs_data, withdrawals_data, validitity_range_data, sigs_data,
        redeemers_data, datums_data, tx_id_data, votes_data,
        proposal_procedures_data, current_treasury_amount_data,
        treasury_donation_data,
      ] = builtin.unconstr_fields(tx_data)

      let Pair(redeemeer_index, redeemer_data_list) =
        builtin.un_constr_data(redeemer_data)
      let must_burn = redeemeer_index == env.mct_deposit_redeemer_index
      let must_mint = redeemeer_index == env.mct_withdrawal_redeemer_index

      if must_burn {
        // The presumed redeemer data is structured as:
        // ```ak
        // @constr<env.mct_deposit_redeemer_index>
        // pub type RedeemerForDeposit {
        //   deposit_input_index_data: Int,
        //   bridged_tokens_index: Int,
        //   mint_field_value_index: Int,
        // }
        // ```
        // TODO: Define the datatype above when the decorators are supported.
        expect [
          deposit_input_index_data, bridged_tokens_index_data,
          mint_field_value_index_data,
          ..
        ] = redeemer_data_list

        expect Some(deposit_utxo_input_data) =
          builtin.un_list_data(inputs_data)
            |> list.at(builtin.un_i_data(deposit_input_index_data))
        expect [_deposit_input_out_ref_data,
          deposit_input_ouput_data, ..] =
          builtin.unconstr_fields(deposit_utxo_input_data)
        expect [
          _deposit_input_address_data, _deposit_input_value_data,
          deposit_input_cardano_datum_data,
          ..
        ] = builtin.unconstr_fields(deposit_input_ouput_data)

        // Unwrapping datum data from `Option<Data>`
        let deposit_input_datum_data =
          builtin.unconstr_fields(deposit_input_cardano_datum_data)
            |> builtin.head_list

        // Using `builtin` functions might yield a more optimized UPLC, but this
        // is more maintainable.
        expect user_events.Datum {
          event: ledger_state.DepositEvent {
            info: ledger_state.DepositInfo {
              bridged_tokens: deposit_input_bridged_tokens,
              ..
            },
            ..
          },
          ..
        }: deposit.Datum = deposit_input_datum_data

        // Find own policy ID and associated tokens at the given index in the
        // declared list of MCTs.
        expect Some(mct_own_policy_tokens_pair_data) =
          deposit_input_bridged_tokens
            |> list.at(builtin.un_i_data(bridged_tokens_index_data))

        expect [mct_own_policy_data,
          mct_own_tokens_data, ..] =
          builtin.un_list_data(mct_own_policy_tokens_pair_data)

        expect mct_own_policy_data == own_policy_data

        // Find own policy ID and associated tokens at the given index in mint
        // field of the transaction.
        expect Some(mint_own_policy_tokens_pair_data) =
          builtin.un_list_data(mint_data)
            |> list.at(builtin.un_i_data(mint_field_value_index_data))

        expect [mint_own_policy_data,
          mint_own_tokens_data, ..] =
          builtin.un_list_data(mint_own_policy_tokens_pair_data)

        expect mint_own_policy_data == own_policy_data

        // Negate the quantities in `mint_own_tokens_data`.
        let negated_mint_own_tokens_data: Data =
          mint_own_tokens_data
            |> builtin.un_list_data
            |> list.map(
                fn(token_quantity_pair_data: Data) -> Data {
                  expect [token_asset_name_data,
                    token_quantity_data, ..] =
                    builtin.un_list_data(token_quantity_pair_data)
                  let token_quantity = builtin.un_i_data(token_quantity_data)
                  [token_asset_name_data, builtin.i_data(-token_quantity)]
                },
              )
        expect mct_own_tokens_data == negated_mint_own_tokens_data

        todo
      } else if must_mint {
        todo
      } else {
        // 1. Coerce withdrawals into withdrawal pairs.
        expect withdrawals: Pairs<Credential, Lovelace> = withdrawals_data
        let target_credential = Script(l1_observer)
        // 2. Check if the L1 observer script's credential is in the
        //    withdrawals.
        withdrawals
          |> list.any(
              fn(cred_lovelace_pair: Pair<Credential, Lovelace>) -> Bool {
                cred_lovelace_pair.1st == target_credential
              },
            )
      }
    } else if layer_index == 1 {
      todo
    } else {
      fail
    }
  }
}
