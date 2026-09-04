use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose, Spend,
}
use midgard/fraud_proofs/min_ada/step_02.{Datum, SpendRedeemer}

pub const tx_role = "V1FpMinAdaS02TxYield"

pub const utxo_role = "V1FpMinAdaS02UtxoYield"

/// Resolve the one min-Ada step-02 spend delegated to this rewarding purpose.
/// Requiring a singleton prevents one withdrawal from discharging multiple
/// otherwise-compatible computation threads in the same transaction.
pub fn unique_dispatch(
  dispatcher_script_hash: ScriptHash,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Datum, SpendRedeemer, OutputReference) {
  expect
      [
        Input {
          output_reference,
          output: Output { datum: InlineDatum(datum_data), .. },
        },
      ]
    = inputs
    |> list.filter(
        fn(input) {
          input.output.address.payment_credential == Script(
            dispatcher_script_hash,
          )
        },
      )
  expect [Pair(_, redeemer_data)] = redeemers
    |> list.filter(fn(pair) { pair.1st == Spend(output_reference) })
  expect datum: Datum = datum_data
  expect redeemer: SpendRedeemer = redeemer_data
  (datum, redeemer, output_reference)
}
