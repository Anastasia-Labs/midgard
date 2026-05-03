use aiken/builtin
use aiken/crypto.{ScriptHash}
use midgard/common/types.{Empty, PosixTimeDuration}

// Including ADA.
pub const max_tokens_allowed_in_deposits: Int = 10

pub const event_wait_duration: Int = 60_000

pub const plutarch_phas_validator_hash: ScriptHash = #""

pub const plutarch_pexcludes_validator_hash: ScriptHash = #""

pub const plutarch_pdelete_validator_hash: ScriptHash = #""

pub const shift_duration: PosixTimeDuration = 60 * 60 * 1000

pub const registration_duration: PosixTimeDuration = 30

pub const maturity_duration: PosixTimeDuration = 30

pub const slashing_penalty: Int = 0

pub const fraud_prover_reward: Int = 0

pub const required_bond: Int = slashing_penalty + fraud_prover_reward

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

/// 5 minutes in milliseconds.
///
/// > [!NOTE]
/// > This value must be set carefully, as it determines the maximum amount of
/// > time that no blocks can be submitted after advancing the scheduler by
/// > operators whose shifts have not ended. TODO.
pub const max_validity_range_length: PosixTimeDuration = 5 * 60 * 1000
