use aiken/cbor
use aiken/crypto.{blake2b_256}
use midgard/cek_context_step.{NativeFacts}
use midgard/computation_thread as ct
use midgard/validation_machine/machine_types.{
  CekContextControlV1, ValidationOneStepWitnessV1,
}

pub type ContextBound {
  prepared: Data,
  control: Data,
  auxiliary_hash: ByteArray,
  transaction_id: ByteArray,
}

pub type ContextStaged {
  bound: ContextBound,
  context: CekContextControlV1,
  native: NativeFacts,
  execution_cursor: Int,
}

pub type ContextSuccessor {
  ContinueContext { next: CekContextControlV1 }
  StartExecution {
    initial_state_hash: ByteArray,
    execution_cpu_limit: Int,
    execution_memory_limit: Int,
  }
}

pub type ContextVerified {
  staged: Data,
  successor: ContextSuccessor,
}

pub type ContextAction {
  VerifyContext {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
}

pub type ContextRedeemer =
  ct.StepRedeemer<ContextAction>

pub type BoundDatum =
  ct.StepDatum<ContextBound>

pub type StagedDatum =
  ct.StepDatum<ContextStaged>

pub type VerifiedDatum =
  ct.StepDatum<ContextVerified>

pub fn auxiliary_is_bound(bound: ContextBound, auxiliary: Data) -> Bool {
  blake2b_256(cbor.serialise(auxiliary)) == bound.auxiliary_hash
}
