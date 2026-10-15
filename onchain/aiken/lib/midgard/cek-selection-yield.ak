use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose, Spend,
}
use midgard/computation_thread as ct
use midgard/script_decode_v1.{Decoded, Refused, ScriptDecodeRecordV1}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_semantic_v1.{Datum}

pub const authenticate_role = "V1VtCekSelAuthYield"

pub const successor_role = "V1VtCekSelSuccYield"

pub type Action {
  VerifyExecutionSelection {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
    yield_ref_input_indices: List<Int>,
    /// Reference input holding the script's decode record, or -1 when the
    /// selection reads no program.
    record_reference_input_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

pub fn selection_language(auxiliary: Data) -> Int {
  expect Pair(11, [_, language_data, ..]) = builtin.un_constr_data(auxiliary)
  expect language: Int = language_data
  language
}

/// A rewarding invocation discharges exactly one matching thread spend.
pub fn unique_dispatch(
  dispatcher_script_hash: ScriptHash,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Datum, Action, OutputReference) {
  expect
      [
        Input {
          output_reference,
          output: Output { datum: InlineDatum(raw_datum), .. },
        },
      ]
    = list.filter(
    inputs,
    fn(input) {
      input.output.address.payment_credential == Script(dispatcher_script_hash)
    },
  )
  expect [Pair(_, raw_redeemer)] = list.filter(
    redeemers,
    fn(pair) { pair.1st == Spend(output_reference) },
  )
  expect datum: Datum = raw_datum
  expect redeemer: SpendRedeemer = raw_redeemer
  expect ct.Continue(action) = redeemer
  (datum, action, output_reference)
}

/// The decode record at a reference input. The record token of the decode
/// family is named by the script hash the record is for, and the family only
/// mints it onto an unspendable output carrying that record, so holding the
/// token named by the record's own hash authenticates the record.
pub fn decode_record_at(
  reference_inputs: List<Input>,
  index: Int,
  script_decode_policy_id: PolicyId,
) -> ScriptDecodeRecordV1 {
  expect
      Some(Input {
        output: Output { datum: InlineDatum(raw_record), value, .. },
        ..
      })
    = list.at(reference_inputs, index)
  expect record: ScriptDecodeRecordV1 = raw_record
  let script_hash = when record is {
    Decoded { script_hash, .. } -> script_hash
    Refused { script_hash, .. } -> script_hash
  }
  expect assets.quantity_of(value, script_decode_policy_id, script_hash) == 1
  record
}
