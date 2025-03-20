use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{NoDatum, Output, OutputReference, Transaction}
use midgard/hub_oracle as hub
use midgard/reserve.{MergeRedeemer,
  MergeUTxOs, FundWithdrawal, SpendRedeemer}

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
      MergeUTxOs { withdrawal_index } -> {
        // Fetch credentials of specified withdrawal pair.
        expect Some(Pair(credentials, _)) =
          tx.withdrawals |> list.at(withdrawal_index)

        // 1. Fetched credential must be equals to the merge logic's.
        credentials == Script(merge_logic_script_hash)
      }
      FundWithdrawal { hub_ref_input_index, withdrawal_event_ref_input_index } -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        let hub.Datum {
          withdrawal: withdrawal_policy_id,
          withdrawal_addr,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        todo
      }
    }
  }

  else(_) {
    False
  }
}

validator merge(hub_oracle: ScriptHash) {
  withdraw(redeemer: MergeRedeemer, _account: Credential, tx: Transaction) {
    let Transaction { inputs, outputs, reference_inputs, .. } = tx

    let MergeRedeemer { hub_ref_input_index, output_index } = redeemer

    let hub.Datum { reserve_addr, .. } =
      hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let total_input_lovelace =
      inputs
        |> list.foldr(
            0,
            fn(input, acc) {
              let Output { address, value, .. } = input.output
              if address == reserve_addr {
                acc + ( value |> assets.lovelace_of )
              } else {
                acc
              }
            },
          )

    // 1. The specified input must not have any datum or script attached to it.
    expect Some(Output {
      address: output_address,
      value: output_value,
      datum: NoDatum,
      reference_script: None,
    }) = outputs |> list.at(output_index)

    and {
      // 2. Specified output must be produced at `reserve_addr`.
      output_address == reserve_addr,
      // 3. Specified output must contain all the input ADA.
      output_value == assets.from_lovelace(total_input_lovelace),
    }
  }

  publish(_redeemer: Data, certificate: Certificate, _self: Transaction) {
    when certificate is {
      // TODO?
      RegisterCredential { .. } |
      UnregisterCredential { .. } -> True
      _ -> False
    }
  }

  else(_) {
    fail
  }
}
