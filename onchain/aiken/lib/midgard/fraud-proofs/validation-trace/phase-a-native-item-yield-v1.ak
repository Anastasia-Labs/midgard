use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose, Spend,
}
use midgard/computation_thread as ct
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_semantic_v1.{Datum}

pub const native_role = "V1VtPhaseANativeItemNativeYield"

pub const foreign_role = "V1VtPhaseANativeItemForeignYield"

pub type ActionV1 {
  VerifyItem {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    field_index: Int,
    item_index: Int,
    carriage: FieldCarriageV1,
    yield_to_ref_input_index: Int,
    yield_kind: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

/// One rewarding invocation discharges exactly one dispatcher input.
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
