//// Thread states of `executionNativeScriptInvalid` (arm 38
//// `ExecutionNativeScriptFalse`, phase 9) after the shared direct source
//// walk. Every fact names one purpose coordinate of the bound transaction:
//// either no native execution exists there, or its native script was
//// evaluated over the transaction's address-witness signers and validity
//// interval.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/validation_merkle_v1.{FrontierPeak}

pub type FactV1 {
  /// No native execution at the coordinate: no such purpose, a key-locked
  /// spend, no source, or a source of another language.
  NoExecution { coordinate: PurposeCoordinateV1 }
  /// The native script of the execution at the coordinate, evaluated.
  Evaluated { coordinate: PurposeCoordinateV1, satisfied: Bool }
}

pub type FinalV1 {
  bound: BoundV1,
  fact: FactV1,
}

/// A paged native evaluation. `signer_count` and `signer_peaks` are the
/// compact frontier of the address-witness signers (`blake2b_256` of each key
/// hash, in field order), complete before the first evaluation page. `cursor`
/// is 0 until the first page reads the versioned-script header (a payload
/// never starts at 0), and is then the absolute offset of the next token in
/// the committed source item. `stack_root` commits the open container frames,
/// top first, through each frame's `tail`; it is empty exactly when
/// `stack_depth` is 0. `pending` is a child result not yet applied to its
/// parent.
pub type EvalV1 {
  bound: BoundV1,
  coordinate: PurposeCoordinateV1,
  item_commitment: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_count: Int,
  signer_peaks: List<FrontierPeak>,
  total_length: Int,
  cursor: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  pending: Option<Bool>,
}

/// Ranks the raw spend item of a key-locked input among the spent outrefs.
pub type SpendRankV1 {
  cursor: cursor.CursorV1,
  subject: ByteArray,
  rank: Int,
  complete: Bool,
}

pub type FinalDatum =
  ct.StepDatum<FinalV1>
