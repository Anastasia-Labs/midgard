use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets.{Lovelace}
use cardano/script_context.{Minting, ScriptInfo}
use env

validator main(l1_observer: ScriptHash, l2_observer: ScriptHash) {
  else(script_ctx_data: Data) {
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

      if must_burn || must_mint {
        expect [] = redeemer_data_list
        expect Minting(own_policy): ScriptInfo = script_info_data
        todo
      } else {
        // 1. Coerce withdrawals into withdrawal pairs.
        expect withdrawals: Pairs<Credential, Lovelace> = withdrawals_data
        let target_credential = Script(l1_observer)
        // 2. Check if the L1 observer script's credential is in the
        //    withdrawals.
        withdrawals
          |> list.any(fn(cred_lovelace_pair: Pair<Credential, Lovelace>) -> Bool {
                cred_lovelace_pair.1st == target_credential
          })
      }
    } else if layer_index == 1 {
      todo
    } else {
      fail
    }
  }
}
