use aiken/crypto.{ScriptHash}
use midgard/fraud_proofs/direct_script_derivation/source.{ResolvedWitnessV1}
use midgard/fraud_proofs/direct_script_derivation/source_index.{AdmissionV1}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{
  PendingV1, ResolvedInputsV1, StateV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type StartArgs {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type ResolveAction {
  ResolveScan {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint: ByteArray,
    budget: Int,
  }
  Advance { input_index: Int, output_index: Int }
}

pub type ResolvedStateV1 {
  resume_script_hash: ScriptHash,
  resolved: ResolvedInputsV1,
}

pub type AuthenticateArgs {
  input_index: Int,
  output_index: Int,
  witnesses: List<ResolvedWitnessV1>,
}

pub type PendingStateV1 {
  resume_script_hash: ScriptHash,
  pending: PendingV1,
}

pub type ResumeStateV1 {
  resume_script_hash: ScriptHash,
  state: StateV1,
}

pub type MapArgs {
  input_index: Int,
  output_index: Int,
  admissions: List<Option<AdmissionV1>>,
}

pub type GrammarArgs {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  budget: Int,
}

pub type InputFinishArgs {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  next_opening: FieldOpeningV1,
}

pub type WitnessAction {
  WitnessScan {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint: ByteArray,
    budget: Int,
  }
  Finish { input_index: Int, output_index: Int }
}

pub type FinishArgs {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
}

pub fn resolve_indices(action: ResolveAction) -> Pair<Int, Int> {
  when action is {
    ResolveScan { input_index, output_index, .. } ->
      Pair(input_index, output_index)
    Advance { input_index, output_index } -> Pair(input_index, output_index)
  }
}

pub fn witness_indices(action: WitnessAction) -> Pair<Int, Int> {
  when action is {
    WitnessScan { input_index, output_index, .. } ->
      Pair(input_index, output_index)
    Finish { input_index, output_index } -> Pair(input_index, output_index)
  }
}
