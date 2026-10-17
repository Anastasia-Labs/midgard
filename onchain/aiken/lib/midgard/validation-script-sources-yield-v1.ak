use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ValidationMachineStateV1}

pub fn middle_role(index: Int) -> ByteArray {
  when index is {
    0 -> "V1VtSsStage2AdvanceYield"
    1 -> "V1VtSsStage3ReplayYield"
    2 -> "V1VtSsStage3FinishYield"
    3 -> "V1VtSsStage4BeginYield"
    4 -> "V1VtSsStage4FinishYield"
    5 -> "V1VtSsStage6BeginPolicyYield"
    6 -> "V1VtSsStage6FoldAssetYield"
    7 -> "V1VtSsStage6FinishYield"
    _ -> fail
  }
}

pub fn middle_stage(index: Int) -> Int {
  when index is {
    0 -> 2
    1 | 2 -> 3
    3 | 4 -> 4
    5 | 6 | 7 -> 6
    _ -> fail
  }
}

pub fn middle_dispatch(
  hashes: List<ScriptHash>,
  tx: Transaction,
  role: Int,
  auxiliary_tag: Int,
) -> (ValidationMachineStateV1, ValidationOneStepWitnessV1, List<Data>) {
  let (state, transition, extra) = validation_semantic_yield_v1.unique_semantic_dispatch_v1(
    hashes,
    tx,
  )
  expect [auxiliary, role_data, _] = extra
  expect builtin.un_i_data(role_data) == role
  expect Pair(tag, fields) = builtin.un_constr_data(auxiliary)
  expect tag == auxiliary_tag
  (state.resolution.pre_state, transition, fields)
}

pub const observer_item_role = "V1VtSsS07ObserverItemYield"

pub const observer_bound_role = "V1VtSsS07ObserverBoundYield"
