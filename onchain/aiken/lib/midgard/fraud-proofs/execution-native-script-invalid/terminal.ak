//// Terminal decision for `executionNativeScriptInvalid` (arm 38
//// `ExecutionNativeScriptFalse`, phase 9). A leaf that records the arm at a
//// coordinate convicts unless the native script executed there evaluates
//// false. Any other leaf, accepted ones included, convicts on a native
//// script that evaluates false at a position its recorded reason does not
//// precede.

use midgard/computation_thread as ct
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  Evaluated, FactV1, FinalV1, NoExecution,
}
use midgard/fraud_proofs/first_fault_v1.{
  ExecutionFault, ExecutionFaultPositionV1, NativeScriptEvaluation,
  PurposeCoordinateV1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ExecutionNativeScriptFalse}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<FinalV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

pub fn coordinate_v1(fact: FactV1) -> PurposeCoordinateV1 {
  when fact is {
    NoExecution { coordinate } -> coordinate
    Evaluated { coordinate, .. } -> coordinate
  }
}

pub fn convicts_v1(state: FinalV1) -> Bool {
  let subject = state.bound.subject
  let coordinate = coordinate_v1(state.fact)
  let falsified = state.fact == Evaluated { coordinate, satisfied: False }
  let names_coordinate = when subject.rejection_reason is {
    Some(ExecutionNativeScriptFalse { purpose_kind, purpose_index }) ->
      PurposeCoordinateV1 { kind: purpose_kind, index: purpose_index } == coordinate
    _ -> False
  }
  if names_coordinate {
    substrate.terminal_contradiction_v1(subject, falsified)
  } else {
    falsified && !first_fault_v1.leaf_admits_later_fault_v1(
      subject,
      state.bound.network_order_context,
      ExecutionFault(
        ExecutionFaultPositionV1 {
          coordinate,
          subcheck: NativeScriptEvaluation,
        },
      ),
    )
  }
}
