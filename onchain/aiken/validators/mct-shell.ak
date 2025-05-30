use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/script_context.{Minting, ScriptInfo}
use cardano/transaction.{Transaction}
use env

validator main(l1_observer: ScriptHash, l2_observer: ScriptHash) {
  else(script_ctx_data: Data) {
    let Pair(layer_index, ctx_data_list) = builtin.un_constr_data(script_ctx_data)

    expect [tx_data, redeemer_data, script_info_data] = ctx_data_list

    if layer_index == 0 {
      let Pair(redeemeer_index, redeemer_data_list) = builtin.un_constr_data(redeemer_data)
      let must_burn = redeemeer_index == env.mct_deposit_redeemer_index
      let must_mint = redeemeer_index == env.mct_withdrawal_redeemer_index
      if must_burn || must_mint {
        expect [] = redeemer_data_list
        expect Minting(own_policy): ScriptInfo = script_info_data
        todo
      } else {
        todo
      }
    } else if layer_index == 1 {
      todo
    } else {
      fail
    }
  }
}
