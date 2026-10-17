use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/proof_thread_substrate_v1.{
  VerdictSubjectV1, encode_verdict_subject_v1,
}

pub const source_pending_forced: Int = 1

pub type BindStateV1 {
  BoundAccepted { subject: VerdictSubjectV1, witness_set_hash: ByteArray }
  PendingForced { direction: Int }
}

pub type DecisionStateV1 {
  subject: VerdictSubjectV1,
  script_integrity_hash: ByteArray,
  contains_non_native_script: Bool,
  has_redeemers: Bool,
}

pub type SubjectStateV1 {
  subject: VerdictSubjectV1,
  witness_set_hash: ByteArray,
}

pub const staged_batch_limit: Int = 32

pub const direct_field_item_limit: Int = 64

pub type StagedPhaseV1 {
  ScriptGrammar { checkpoint_hash: ByteArray }
  ScriptScan { checkpoint_hash: ByteArray, contains_non_native_script: Bool }
  ScriptComplete { contains_non_native_script: Bool }
  RedeemerGrammar {
    checkpoint_hash: ByteArray,
    contains_non_native_script: Bool,
  }
}

pub type StagedStateV1 {
  subject: VerdictSubjectV1,
  witness_set_hash: ByteArray,
  script_integrity_hash: ByteArray,
  phase: StagedPhaseV1,
}

pub fn encode_decision_state_v1(state: DecisionStateV1) -> ByteArray {
  expect bytearray.length(state.script_integrity_hash) == 32
  encode_definite_array_header(4)
    |> bytearray.concat(
        encode_definite_bytes(encode_verdict_subject_v1(state.subject)),
      )
    |> bytearray.concat(encode_definite_bytes(state.script_integrity_hash))
    |> bytearray.concat(cbor.serialise(state.contains_non_native_script))
    |> bytearray.concat(cbor.serialise(state.has_redeemers))
}
