use aiken/crypto.{VerificationKeyHash}
use midgard/ledger_state
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_trace_v1

pub type PendingValidationClaimV1 {
  challenged_header_hash: ByteArray,
  challenged_header: ledger_state.HeaderV1,
  claim: validation_claim_v1.ValidationClaimWitnessV1,
  challenger_descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  open_time_upper: Int,
}

pub type ValidationGameStateV1 {
  challenged_header_hash: ByteArray,
  operator_vkey: VerificationKeyHash,
  dispute: validation_dispute_v1.ValidationDisputeV1,
}
