//// Shared custody boundary for direct purpose/source proofs.
//// Step 01 alone authenticates the source and thread-bound header. Step 02
//// authenticates the subject's event pre-root before any ledger output opens.

use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_state.{HeaderV1}

pub type SourceBoundV1 {
  subject: VerdictSubjectV1,
  header: HeaderV1,
  compact_cbor: ByteArray,
}

pub type BoundV1 {
  subject: VerdictSubjectV1,
  compact_cbor: ByteArray,
  event_pre_root: ByteArray,
}

/// Internal direct-proof coordinate. Global rejection-reason constructors
/// remain the single ABI authority; this record does not change those arms.
pub type PurposeCoordinateV1 {
  kind: Int,
  index: Int,
}

pub type PurposeV1 {
  coordinate: PurposeCoordinateV1,
  script_hash: ByteArray,
  subject: ByteArray,
}
