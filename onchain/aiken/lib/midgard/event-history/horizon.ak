//// Event-history temporal predicates. Inputs named authenticated must come
//// from canonical queue/settlement or list authentication at their call site.
//// These predicates do not turn an off-chain timestamp into L1 authority.

use midgard/ledger_state.{block_maturity_duration_v1}

pub fn eligible(inclusion_time: Int, start_time: Int, end_time: Int) -> Bool {
  and {
    start_time >= 0,
    start_time < end_time,
    start_time < inclusion_time,
    inclusion_time <= end_time,
  }
}

/// Capture after the entire accused interval and before its earliest merge.
/// The finite inclusive bounds must be derived from the actual transaction.
pub fn capture_permitted(header_end: Int, lower: Int, upper: Int) -> Bool {
  and {
    header_end >= 0,
    header_end < lower,
    lower <= upper,
    upper < header_end + block_maturity_duration_v1,
  }
}

/// All event-dependent proof stages, including terminal issuance, must finish
/// before earliest merge. No proof thread extends this deadline or locks merge.
pub fn completion_permitted(header_end: Int, upper: Int) -> Bool {
  and {
    header_end >= 0,
    upper > header_end,
    upper < header_end + block_maturity_duration_v1,
  }
}

/// Finalized interval authority is required in addition to authorized
/// settlement membership/payout/refund. A later settlement reusing an ID does
/// not establish that its actual eligibility interval has finalized.
pub fn retirement_permitted(
  inclusion_time: Int,
  authenticated_finalized_end: Int,
) -> Bool {
  and {
    inclusion_time > 0,
    inclusion_time <= authenticated_finalized_end,
  }
}

/// Proposed bounded observation protection. Every node mutation must require
/// its actual lower bound >= the previous protection bound and set every
/// produced node's protection to its upper bound + the deployment duration.
/// Thus an adversarial replacement creates a fresh capture opportunity. The
/// duration is not fixed until deployed-parameter fit and timing measurements.
pub fn mutation_permitted(protected_until: Int, lower: Int, upper: Int) -> Bool {
  and {
    protected_until >= 0,
    protected_until <= lower,
    lower <= upper,
  }
}

pub fn protected_capture(protected_until: Int, lower: Int, upper: Int) -> Bool {
  and {
    lower >= 0,
    lower <= upper,
    upper < protected_until,
  }
}
