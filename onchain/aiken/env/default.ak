use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use midgard/common/types.{Empty, H32, PosixTime, PosixTimeDuration}

pub const posix_time_none: PosixTime = -1

pub const plutus_version: Int = 3

// Including ADA.
pub const max_tokens_allowed_in_deposits: Int = 10

pub const event_wait_duration: Int = 60_000

pub const block_maturity_duration_v1: PosixTimeDuration =
  7 * 24 * 60 * 60 * 1000

pub const validation_dispute_response_window_milliseconds: Int = 300_000

pub const shift_duration: PosixTimeDuration = 30

pub const registration_duration: PosixTimeDuration = 30

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

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_list_hash: H32<List<Data>> = [] |> cbor.serialise |> blake2b_256

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

pub const plutarch_phas_validator_hash: ScriptHash =
  #"1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

pub const plutarch_pexcludes_validator_hash: ScriptHash =
  #"a9ec251d6476217b1abccd5f035dec1272a4b04f640f503fca9e734d"

pub const plutarch_pdelete_validator_hash: ScriptHash = #""

/// Merkelized verifier of published-chunk MPF proof carriage (issue #545).
/// Compiled from `validators/mpf-chunked-verify.ak` in this very project,
/// so this constant is the blueprint hash of
/// `mpf_chunked_verify.verify.withdraw`
/// and is re-pinned whenever that validator changes.
pub const mpf_chunked_verify_validator_hash: ScriptHash =
  #"cb5a7ec4def35ce3ec75c40919992e1b4e8839b4f6b6a2d3b06e7469"

pub const user_events_witness_script_prefix: ByteArray =
  #"5902ce0101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc38012400314a0809901319199119801001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c96600266ebc00401e2b3001300930133754003132598009805980a1baa0018acc004cdd7980b980a9baa00230173015375400314a316404d16404c602c602e005164049164048602a002660066eb0c004c048dd50051bad301430123754019159800980418089baa0058992cc004c020c048dd5000c56600266ebcc054c04cdd5000980a98099baa0068a518b20228b20223014330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0000a40028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c01225820"
