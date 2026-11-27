//// Thread types of the all-purposes walk validators. Each validator walks one
//// purpose field and hands over to the next field's validator once its field
//// is exhausted.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan.{
  ExecutionWitnessV1, PendingSpendsV1, SpendWitnessV1, StateV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type SpendTakeArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  count: Int,
}

pub type SpendResolveArgsV1 {
  input_index: Int,
  output_index: Int,
  witnesses: List<SpendWitnessV1>,
}

/// A variable field opens once its predecessor is exhausted, then certifies
/// its grammar in bounded batches before any item is read.
pub type GrammarActionV1 {
  Open { input_index: Int, output_index: Int, opening: FieldOpeningV1 }
  Certify {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    budget: Int,
  }
}

pub type ExecutionArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<ExecutionWitnessV1>,
}

/// Opens a fixed-stride field, which walks as soon as it opens.
pub type OpenArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type ReceiveArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<Option<ExecutionWitnessV1>>,
}

pub type Datum =
  ct.StepDatum<StateV1>

pub type PendingDatum =
  ct.StepDatum<PendingSpendsV1>

pub type SpendTakeRedeemer =
  ct.StepRedeemer<SpendTakeArgsV1>

pub type SpendResolveRedeemer =
  ct.StepRedeemer<SpendResolveArgsV1>

pub type GrammarRedeemer =
  ct.StepRedeemer<GrammarActionV1>

pub type ExecutionRedeemer =
  ct.StepRedeemer<ExecutionArgsV1>

pub type OpenRedeemer =
  ct.StepRedeemer<OpenArgsV1>

pub type ReceiveRedeemer =
  ct.StepRedeemer<ReceiveArgsV1>

pub fn grammar_indices_v1(action: GrammarActionV1) -> Pair<Int, Int> {
  when action is {
    Open { input_index, output_index, .. } -> Pair(input_index, output_index)
    Certify { input_index, output_index, .. } -> Pair(input_index, output_index)
  }
}
