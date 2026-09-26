use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use midgard/common/types.{Empty, H32, PosixTime}

pub const posix_time_none: PosixTime = -1

pub const plutus_version: Int = 3

pub const max_tokens_allowed_in_deposits: Int = 10

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_list_hash: H32<List<Data>> = [] |> cbor.serialise |> blake2b_256

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

pub const plutarch_phas_validator_hash: ScriptHash =
  #"1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

pub const plutarch_pexcludes_validator_hash: ScriptHash =
  #"074ca4d6263082ec32a5715e4685981188fdab23aeb8f2c6d536843e"

/// Merkelized verifier of published-chunk MPF proof carriage (issue #545).
/// Compiled from `validators/mpf-chunked-verify.ak` in this very project,
/// so this constant is the blueprint hash of
/// `mpf_chunked_verify.verify.withdraw`
/// and is re-pinned whenever that validator changes.
pub const mpf_chunked_verify_validator_hash: ScriptHash =
  #"1dbd8f5ad1314ebf9e4bcbf814fb7f44eea574ed20b3a5b42c7e0767"

pub const user_events_witness_script_prefix: ByteArray =
  #"5902e20101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc3a400200514a0809901319199119801001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c966002003168992cc004cdd78008044566002601460286ea8006264b3001300c3015375400315980099baf3018301637540046030602c6ea800629462c80a22c80a0c05c00a2c809a2c8098c058009015180b000998019bac3001301237540146eb4c050c048dd50064566002601060226ea8016264b30010018b44c966002601260266ea80062b30013375e602c60286ea8004c058c050dd5003c528c59012459012180a800a028330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0240020028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c1225820"

pub const block_maturity_duration_v1: Int = 300_000

pub const response_window_milliseconds: Int = 60_000

pub const shift_duration: Int = 600_000

pub const registration_duration: Int = 30_000

pub const event_wait_duration: Int = 60_000

pub const user_events_negligence_timeout: Int = 300_000

pub const max_inactivity_between_block_commitments: Int = 60_000

pub const new_shift_inactivity_grace_period: Int = 300_000

pub const max_validity_range_length: Int = 480_000

pub const da_attestation_timeout_v1: Int = 240_000

pub const small_response_window_ms_v1: Int = 60_000

pub const full_response_window_ms_v1: Int = 120_000

pub const max_bisection_rounds: Int = 32

pub const max_inactivity_strikes: Int = 5

pub const coins_per_utxo_byte: Int = 4_310

pub const required_bond: Int = 900_000_000

pub const slashing_penalty: Int = 500_000_000

pub const inactivity_slashing_penalty: Int = 100_000_000

pub const fraud_prover_reward: Int = 400_000_000

pub const prover_collateral_floor: Int = 5_000_000
