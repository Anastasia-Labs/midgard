use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{OutputReference, Transaction}
use midgard/reserve.{MergeUTxOs, SatisfyWithdrawal, SpendRedeemer}

validator spend(
  hub_oracle: ScriptHash,
  plutarch_phas_validator_hash: ScriptHash,
  merge_logic_script_hash: ScriptHash,
) {
  spend(
    _datum: Option<Data>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      MergeUTxOs -> {
        // 1. Essentially expecting a single withdrawal present in the
        //    transaction to keep this check as minimal as possible.
        let Pair(credentials, _) = tx.withdrawals |> builtin.head_list

        // 2. The single withdrawal must be validated by the merge logic script.
        credentials == Script(merge_logic_script_hash)
      }
      SatisfyWithdrawal -> todo
    }
  }

  else(_) {
    False
  }
}
