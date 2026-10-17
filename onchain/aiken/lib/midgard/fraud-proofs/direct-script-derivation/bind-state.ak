use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_state.{HeaderV1}

/// Written by the physical event door only. The index is carried authority,
/// not an independently supplied hint in the transition door's redeemer.
pub type EventSelectedV1 {
  subject: VerdictSubjectV1,
  header: HeaderV1,
  compact_cbor: ByteArray,
  step_index: Int,
}
