//// Terminal decision for `executionSourceScriptDecoding`: arms 35
//// `ExecutionNativeScriptMalformed`, 36 `ExecutionNativeScriptNodeLimit` and
//// 37 `ExecutionNativeScriptDepthLimit`, phase 9.
////
//// The thread reaches this step through the shared direct prefix: step-01
//// opens the verdict subject from its leaf under the header, and the event
//// door binds that subject to its event step. No transaction byte is decoded
//// here.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/bind_state.{EventSelectedV1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  ExecutionNativeScriptDepthLimit, ExecutionNativeScriptMalformed,
  ExecutionNativeScriptNodeLimit,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<EventSelectedV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// A leaf convicts exactly when it rejects its transaction with one of these
/// three arms. The transaction field and output byte caps keep every native
/// script below the node and depth limits, and malformed native bytes are
/// refused by earlier phases, so a correct leaf never records them. Any future
/// limit change that makes these arms reachable by a correct leaf must bring
/// back a decoding proof. A leaf that accepts its transaction or records
/// another reason never convicts here.
pub fn convicts_v1(state: EventSelectedV1) -> Bool {
  when state.subject.rejection_reason is {
    Some(ExecutionNativeScriptMalformed { .. }) |
    Some(ExecutionNativeScriptNodeLimit { .. }) |
    Some(ExecutionNativeScriptDepthLimit { .. }) ->
      substrate.terminal_contradiction_v1(state.subject, False)
    _ -> False
  }
}
