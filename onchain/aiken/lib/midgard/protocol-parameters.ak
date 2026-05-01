use midgard/common/types.{PosixTimeDuration}

// TODO: set realistic parameter values
// TODO: what about parameter values for testing?

pub const shift_duration: PosixTimeDuration = 60 * 60 * 1000

pub const registration_duration: PosixTimeDuration = 30

pub const maturity_duration: PosixTimeDuration = 30

pub const slashing_penalty: Int = 0

pub const fraud_prover_reward: Int = 0

pub const required_bond: Int = slashing_penalty + fraud_prover_reward

/// TODO: This value is currently designed to be paid as tx fee. This means if a
///       partially slashed operator is also proven to be fraudulent, the
///       portion paid as fee must be at least:
///       ```
///       slashing_penalty - inactivity_slashing_penalty
///       ```
///       Note that if `inactivity_slashing_penalty` is larger than
///       `slashing_penalty`, fraud provers will be rewarded a smaller amount.
pub const inactivity_slashing_penalty: Int = 0

/// Max inactivity of operators after the earliest user event slated for
/// inclusion.
pub const user_events_negligence_timeout: PosixTimeDuration = 5 * 60 * 1000

/// Max inactivity of operators regardless of having any transactions and/or
/// user events to include.
pub const max_inactivity_between_block_commitments: PosixTimeDuration =
  10 * 6 * 1000

/// Grace period of inactivity for newly appointed operators.
pub const new_shift_inactivity_grace_period: PosixTimeDuration = 5 * 60 * 1000

pub const max_inactivity_strikes: Int = 5

pub const max_validity_range_length: PosixTimeDuration = 8 * 60 * 1000
