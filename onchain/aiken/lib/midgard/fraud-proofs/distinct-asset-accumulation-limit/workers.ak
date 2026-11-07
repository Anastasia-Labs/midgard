//// Thread types of the `distinctAssetAccumulationLimit` walk validators.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers.{
  GrammarActionV1,
}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{
  InputWitnessV1, PendingInputsV1, RankV1, WalkV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type EntryArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type ResolveArgsV1 {
  input_index: Int,
  output_index: Int,
  witnesses: List<InputWitnessV1>,
}

pub type RankArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  rank: Int,
}

/// Outputs take a work budget; mint takes a policy count; the spend field
/// takes an item count.
pub type FieldArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
}

pub type EntryDatum =
  ct.StepDatum<BoundV1>

pub type Datum =
  ct.StepDatum<WalkV1>

pub type PendingDatum =
  ct.StepDatum<PendingInputsV1>

pub type RankDatum =
  ct.StepDatum<RankV1>

pub type EntryRedeemer =
  ct.StepRedeemer<EntryArgsV1>

pub type ResolveRedeemer =
  ct.StepRedeemer<ResolveArgsV1>

pub type RankRedeemer =
  ct.StepRedeemer<RankArgsV1>

pub type FieldRedeemer =
  ct.StepRedeemer<FieldArgsV1>

/// A variable field opens once its predecessor is exhausted, then certifies
/// its grammar in bounded batches before any item is read.
pub type GrammarRedeemer =
  ct.StepRedeemer<GrammarActionV1>
