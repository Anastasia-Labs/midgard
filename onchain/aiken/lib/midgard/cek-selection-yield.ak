use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose, Spend,
}
use midgard/cek_proof_v1.{CekMaterialPartitionFacts}
use midgard/computation_thread as ct
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_resolver_v1.{
  CekMaterialRouteV1, DirectCekMaterial, MinimumMultiOutputCekMaterial,
  SinglePublicationCekMaterial,
}
use midgard/validation_semantic_v1.{Datum}

pub const authenticate_role = "V1VtCekSelAuthYield"

pub const successor_role = "V1VtCekSelSuccYield"

pub const material_program_role = "V1VtCekSelMatProgYield"

pub const material_data_role = "V1VtCekSelMatDataYield"

pub type EnvelopeFacts {
  term_root: ByteArray,
  node_count: Int,
  material_byte_length: Int,
}

pub type Action {
  VerifyExecutionSelection {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
    material_route: CekMaterialRouteV1,
    yield_ref_input_indices: List<Int>,
    envelope_facts: EnvelopeFacts,
    material_facts: CekMaterialPartitionFacts,
    begin_traversal: Bool,
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

pub fn route_envelope(route: CekMaterialRouteV1) -> ByteArray {
  when route is {
    DirectCekMaterial { envelope_cbor, .. } -> envelope_cbor
    SinglePublicationCekMaterial { envelope_cbor, .. } -> envelope_cbor
    MinimumMultiOutputCekMaterial { envelope_cbor, .. } -> envelope_cbor
    _ -> fail
  }
}
