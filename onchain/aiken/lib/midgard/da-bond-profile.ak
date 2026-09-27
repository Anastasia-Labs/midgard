//// Relations every deployment profile's pooled DA committee bond must satisfy.
//// `demo/scripts/deployment-profiles.mjs` (`validateProfile`) enforces the
//// same relations before it renders a profile into `env/*.ak`, and
//// `da-bond-profile.test.ak` applies them to the constants of whichever env
//// the check runs under.

use aiken/math.{max}

/// A block attested at the last moment can still be challenged, and an open
/// and a merge of the same block are never both valid.
pub fn challenge_window_is_ordered_v1(
  attestation_timeout: Int,
  challenge_window: Int,
  block_maturity: Int,
) -> Bool {
  and {
    attestation_timeout < challenge_window,
    challenge_window <= block_maturity,
  }
}

/// The withdrawal delay covers the challenge path of every block the pool is
/// still liable for: the last applied block ends at most one validity range
/// after BeginWithdraw, its open lands inside the challenge window, the
/// response deadline is one full response window later, and the timeout gets
/// the slash grace.
pub fn withdraw_delay_covers_challenge_v1(
  withdraw_delay: Int,
  max_validity_range: Int,
  challenge_window: Int,
  full_response_window: Int,
  slash_grace: Int,
) -> Bool {
  withdraw_delay >= max_validity_range + challenge_window + full_response_window + slash_grace
}

/// The enforced, stronger form. An unavailable block is timed out only once it
/// is the queue head, and its predecessors merge no earlier than its end time
/// plus block maturity, so the slash grace starts at the later of the response
/// deadline and maturity. It implies `withdraw_delay_covers_challenge_v1`.
pub fn withdraw_delay_covers_head_removal_v1(
  withdraw_delay: Int,
  max_validity_range: Int,
  challenge_window: Int,
  full_response_window: Int,
  block_maturity: Int,
  slash_grace: Int,
) -> Bool {
  withdraw_delay >= max_validity_range + max(
    challenge_window + full_response_window,
    block_maturity,
  ) + slash_grace
}

/// The penalty is burned as fee and the rest of the bond rewards the
/// challenger, so both shares are positive.
pub fn slash_penalty_is_bounded_v1(penalty: Int, da_bond: Int) -> Bool {
  and {
    0 < penalty,
    penalty < da_bond,
  }
}

/// A zero top-up would let anyone churn the pool's outref for free, a zero
/// floor leaves the pool below min-UTxO, and a zero record amount leaves the
/// challenge record below min-UTxO.
pub fn pool_amounts_are_positive_v1(
  min_top_up: Int,
  pool_floor: Int,
  challenge_record: Int,
) -> Bool {
  and {
    min_top_up > 0,
    pool_floor > 0,
    challenge_record > 0,
  }
}
