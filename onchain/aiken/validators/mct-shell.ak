use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets.{Lovelace, PolicyId}
use cardano/script_context.{Minting, ScriptInfo}
use cardano/transaction.{Mint, Redeemer, ScriptPurpose}
use env
use midgard/common/types.{ValuePairs}
use midgard/common/utils
use midgard/user_events

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

    if layer_index == 0 {
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
        expect [deposit_utxo_input_index_data,
          mint_field_value_index_data, ..] = redeemer_data_list

        expect Some(deposit_utxo_input_data) =
          builtin.un_list_data(inputs_data)
            |> list.at(builtin.un_i_data(deposit_utxo_input_index_data))
        expect [deposit_input_out_ref_data,
          deposit_input_ouput_data, ..] =
          builtin.unconstr_fields(deposit_utxo_input_data)
        expect [
          _deposit_input_address_data, deposit_input_value_data,
          deposit_input_datum_data,
          ..
        ] = builtin.unconstr_fields(deposit_input_ouput_data)
        expect deposit_input_value_pairs: ValuePairs = deposit_input_value_data

        expect Some(mct_policy_mint_data) =
          builtin.un_list_data(mint_data)
            |> list.at(builtin.un_i_data(mint_field_value_index_data))
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
