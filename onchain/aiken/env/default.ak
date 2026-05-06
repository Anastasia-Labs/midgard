use aiken/builtin
use aiken/crypto.{ScriptHash}
use midgard/common/types.{Empty, PosixTimeDuration}

pub const plutus_version: Int = 3

// Including ADA.
pub const max_tokens_allowed_in_deposits: Int = 10

pub const event_wait_duration: Int = 60_000

pub const shift_duration: PosixTimeDuration = 30

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

pub const max_validity_range_length: PosixTimeDuration = 5 * 60 * 1000

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

pub const plutarch_phas_validator_hash: ScriptHash = #""

pub const plutarch_pexcludes_validator_hash: ScriptHash = #""

pub const plutarch_pdelete_validator_hash: ScriptHash = #""

pub const user_events_witness_script_prefix: ByteArray =
  #"5902d30101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc38012400314a0809901319199119801001000912cc00400600713233225980099b910160028acc004cdc780b0014400600c80ba26600a00a603800880b8dd7180b0009bad30170013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc00400600713233225980099b910080028acc004cdc78040014400600c80c226600a00a603a00880c0dd7180b8009bab301800130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c96600266ebc00401e2b3001300930133754003132598009805980a1baa0018acc004cdd7980b980a9baa00230173015375400314a316404d16404c602c602e005164049164048602a002660066eb0c004c048dd50051bad301430123754019159800980418089baa0058992cc004c020c048dd5000c56600266ebcc054c04cdd5000980a98099baa0068a518b20228b20223014330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0000a40028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c01225820"
