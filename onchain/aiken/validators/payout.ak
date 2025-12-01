use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets
use cardano/transaction.{Input, Output, OutputReference, Transaction}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/payout.{AddFunds, ConcludeWithdrawal, Datum, SpendRedeemer}

// 3.7.3
// Payout spending validator
// The spending validator of payout addr is responsible for collecting funds from the Midgard reserve
// until a complete payout can be sent to its destination.

// Collect Reserve Funds. Collect funds from the Midgard reserve. Conditions:
// 1. Let payout input be the transaction input being spent. Let payout datum be its datum.
// 2. Let reserve input be the sole transaction input from reserve addr.
// 3. Let payout output be a transaction output.
// 4. payout input and payout output must each hold exactly one payout token and it must
// match between them.
// 5. Let value diff be the sum of values of payout input and reserve input, minus the
// value of payout.
// 6. value diff must not be negative.
// 7. If value diff is positive:
// • The transaction must send a change output to reserve addr with a value that
// matches or exceeds value diff.
// 8. The transaction must not mint or burn any tokens.

// Complete Payout. When the payout accumulator contains sufficient funds, complete the payout
// to the destination address and datum. Conditions:
// 1. Let payout input be the transaction input being spent. Let payout datum be its datum.
// 2. payout input must hold exactly one token of payout, which must be the only payout
// burned in the transaction.
// 3. Let payout output be a transaction output.
// 4. payout output.address must match payout datum.l1 address.
// 5. payout output.datum must match payout datum.l1 datum.
// 6. payout output.value must match or exceed payout datum.l2 value, excluding the
// burned token.
// 7. The transaction must not mint or burn any other tokens.

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, mint, .. } = tx
    expect Some(payout_datum) = datum

    // let Datum { l2_value, l1_address, l1_datum } = payout_datum
    when redeemer is {
      AddFunds {
        input_index,
        output_index,
        reserve_input_index,
        hub_ref_input_index,
      } -> {
        // Extract hub datum for policy and reserve address
        let hub.Datum { payout: payout_policy_id, reserve_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // Check payout input/output with payout token
        singular_utxo_indexer.spend(
          fn(_in_ix, payout_input, _redeemer, _out_ix, payout_output) {
            // Let payout input be the transaction input being spent
            let Input { output: Output { value: payout_input_value, .. }, .. } =
              payout_input
            //  Let payout datum be its datum.
            let Datum { l2_value, l1_address, l1_datum } = payout_datum
            // 1. Payout input must hold exactly one payout token and it must
            // match between them.
            let (input_policy_id, input_asset_name, input_qty) =
              utils.get_single_asset_from_value_apart_from_ada(
                payout_input_value,
              )
            expect input_policy_id == payout_policy_id
            expect input_asset_name == "payout"
            expect input_qty == 1

            // 2. Let reserve_input be the sole transaction input from reserve_addr
            expect Some(reserve_input) = list.at(inputs, reserve_input_index)
            let Input {
              output: Output {
                address: supposed_reserve_address,
                value: reserve_value,
                ..
              },
              ..
            } = reserve_input
            expect supposed_reserve_address == reserve_addr
            let reserve_inputs_count =
              list.filter(
                inputs,
                fn(input) { input.output.address == reserve_addr },
              )
                |> list.length
            expect reserve_inputs_count == 1
            // Let payout output be a transaction output.
            // 3. The produced payout output must not have any reference script
            //    attached.
            expect Output {
              address: _payout_output_address,
              value: payout_output_value,
              datum: _payout_output_datum,
              reference_script: None,
            } = payout_output

            // 4. The payout output must have exactly one payout token with the same
            //    asset name.
            let (output_policy_id, output_asset_name, output_qty) =
              utils.get_single_asset_from_value_apart_from_ada(
                payout_output_value,
              )
            expect output_policy_id == payout_policy_id
            expect output_asset_name == "payout"
            expect output_qty == 1
            // Total input value(payout + reserve)
            let total_input_value =
              assets.merge(payout_input_value, reserve_value)
            let total_input_value_ada = assets.lovelace_of(total_input_value)
            let payout_output_value_ada =
              assets.lovelace_of(payout_output_value)

            // 5. Check inputs >= output (no negative diff)
            expect total_input_value_ada >= payout_output_value_ada
            // Calculate required change (if any)
            let required_change =
              total_input_value_ada - payout_output_value_ada
            // 6. If change > 0, must send to reserve_addr
            when required_change > 0 is {
              True -> {
                let change_output_opt =
                  list.find(outputs, fn(o) { o.address == reserve_addr })
                expect Some(change_output) = change_output_opt
                let change_output_value_ada =
                  assets.lovelace_of(change_output.value)
                expect change_output_value_ada >= required_change
              }
              False -> {
                expect True
              }
            }
            // 7. The transaction must not mint or burn any tokens.
            expect mint == assets.zero
            True
          },
          0,
          input_index,
          output_index,
          own_out_ref,
          tx,
        )
      }
      ConcludeWithdrawal -> todo
    }
  }

  else(_) {
    fail
  }
}
// 3.7.2
// Payout minting policy
// The payout minting policy ensures that only a single payout
// accumulator can be initialized or
// payout can be completed per transaction. Redeemers:
// Mint. Mint a single payout token. Conditions:
// 1. The transaction must mint exactly one token of payout.
// 2. The transaction must burn exactly one token of withdrawal.
// 3. The minted and burned tokens must match on token name.
// 4. The transaction must not mint or burn any other tokens.
// Burn. Burn a single payout token. Conditions:
// 1. The transaction must burn exactly one token of payout.
// 2. The transaction must not mint or burn any other tokens.
// validator mint {
//   mint() {
//     todo
//   }

//   else(_) {
//     fail
//   }
// }
