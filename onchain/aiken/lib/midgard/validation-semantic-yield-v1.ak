//// Shared handshake for semantic resolvers that delegate one arm to a
//// rewarding validator. Exactly one matching dispatcher may spend per yield.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, Spend, Transaction}
use midgard/state_queue_yield
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_resolution_v1.{PreparedValidationResolutionStateV1}
use midgard/validation_semantic_v1.{Datum}

pub fn require_semantic_yield_v1(
  tx: Transaction,
  reference_script_auth_policy_id: PolicyId,
  role: AssetName,
  yield_ref_input_index: Int,
) -> ScriptHash {
  state_queue_yield.require_authenticated_zero_yield(
    tx.reference_inputs,
    tx.withdrawals,
    tx.redeemers,
    reference_script_auth_policy_id,
    role,
    yield_ref_input_index,
  )
}

pub fn unique_semantic_dispatch_v1(
  dispatcher_script_hashes: List<ScriptHash>,
  tx: Transaction,
) -> (
  PreparedValidationResolutionStateV1,
  ValidationOneStepWitnessV1,
  List<Data>,
) {
  expect
      [
        Input {
          output_reference,
          output: Output { datum: InlineDatum(datum_data), .. },
        },
      ]
    = tx.inputs
    |> list.filter(
        fn(input) {
          list.any(
            dispatcher_script_hashes,
            fn(hash) { input.output.address.payment_credential == Script(hash) },
          )
        },
      )
  expect [Pair(_, redeemer_data)] = tx.redeemers
    |> list.filter(fn(pair) { pair.1st == Spend(output_reference) })
  expect datum: Datum = datum_data
  expect Some(state) = datum.data
  expect Pair(1, [action]) = builtin.un_constr_data(redeemer_data)
  expect Pair(0, [_,
        _, transition_data, ..extra]) = builtin.un_constr_data(action)
  expect transition: ValidationOneStepWitnessV1 = transition_data
  (state, transition, extra)
}
