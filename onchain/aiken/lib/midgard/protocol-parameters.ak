use midgard/common/types.{PosixTimeDuration}

// TODO: set realistic parameter values
// TODO: what about parameter values for testing?

pub const shift_duration: PosixTimeDuration = 30

pub const registration_duration: PosixTimeDuration = 30

pub const maturity_duration: PosixTimeDuration = 30

pub const slashing_penalty: Int = 0

pub const fraud_prover_reward: Int = 0

pub const required_bond: Int = slashing_penalty + fraud_prover_reward

/// 5 minutes in milliseconds.
pub const operator_inactivity_timeout: PosixTimeDuration = 5 * 60 * 1000
