use aiken_design_patterns/validity_range_normalization.{
  Always, ClosedRange, InvalidRange,
}
use midgard/fraud_proofs/invalid_range/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  RejectionReasonV1, ValidityIntervalExcludesBlockSlot,
  ValidityIntervalMalformed,
}

const tx_id =
  #"1111111111111111111111111111111111111111111111111111111111111111"

fn forced(reason: RejectionReasonV1) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 1,
    source_kind: 1,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(reason),
  }
}

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

test accepted_malformed_closes() {
  rule.terminal_contradiction_v1(accepted(), InvalidRange, 10)
}

test accepted_excluded_closes() {
  rule.terminal_contradiction_v1(
    accepted(),
    ClosedRange { lower: 11, upper: 12 },
    10,
  )
}

test malformed_rejection_is_wrong_when_interval_is_well_formed() {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalMalformed),
    ClosedRange { lower: 10, upper: 10 },
    10,
  )
}

test malformed_rejection_is_honest_for_malformed_interval() fail {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalMalformed),
    InvalidRange,
    10,
  )
}

test exclusion_rejection_is_wrong_at_lower_boundary() {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalExcludesBlockSlot),
    ClosedRange { lower: 10, upper: 12 },
    10,
  )
}

test exclusion_rejection_is_wrong_at_upper_boundary() {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalExcludesBlockSlot),
    ClosedRange { lower: 8, upper: 10 },
    10,
  )
}

test exclusion_rejection_is_wrong_for_unbounded_interval() {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalExcludesBlockSlot),
    Always,
    10,
  )
}

test exclusion_rejection_is_honest_below_range() fail {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalExcludesBlockSlot),
    ClosedRange { lower: 11, upper: 12 },
    10,
  )
}

test exclusion_reason_does_not_alias_malformed() {
  rule.terminal_contradiction_v1(
    forced(ValidityIntervalExcludesBlockSlot),
    InvalidRange,
    10,
  )
}
