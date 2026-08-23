use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use midgard/common/types.{Empty, H32, PosixTime, PosixTimeDuration}

pub const posix_time_none: PosixTime = -1

pub const plutus_version: Int = 3

pub const max_tokens_allowed_in_deposits: Int = 10

pub const event_wait_duration: Int = 60_000

pub const shift_duration: PosixTimeDuration = 60 * 60 * 1000

pub const registration_duration: PosixTimeDuration = 30

pub const slashing_penalty: Int = 0

pub const fraud_prover_reward: Int = 0

pub const required_bond: Int = slashing_penalty + fraud_prover_reward

pub const inactivity_slashing_penalty: Int = 0

pub const user_events_negligence_timeout: PosixTimeDuration = 5 * 60 * 1000

pub const max_inactivity_between_block_commitments: PosixTimeDuration =
  10 * 6 * 1000

pub const new_shift_inactivity_grace_period: PosixTimeDuration = 5 * 60 * 1000

pub const max_inactivity_strikes: Int = 5

pub const max_validity_range_length: PosixTimeDuration = 8 * 60 * 1000

/// C49/C70 (#618, #627). The target network's `coins_per_utxo_byte` protocol
/// parameter: the rate the minimum-Ada floor
/// `min_ada_lovelace_v1(coins_per_utxo_byte, serialized_output_bytes)`
/// (lib/midgard/validation-machine-v1.ak) is instantiated at when the
/// ValueAndMint output-descriptor scan convicts an under-funded output with
/// `E_MIN_ADA`.
///
/// Provenance: the C70 target-parameter snapshot, preprod epoch 303, pinned as
/// `PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.coinsPerUtxoByte == 4_310n`
/// (demo/midgard-validation/tests/helpers/ordered-collection-boundary-v1.ts).
/// The TypeScript mirror of this very constant is
/// `MIDGARD_CONSENSUS_LIMITS_V1.coinsPerUtxoByte`
/// (demo/midgard-core/src/consensus-profile-v1.ts); the two are held equal, and
/// held equal to the snapshot, by
/// demo/midgard-validation/tests/min-ada-twin-cross-check-v1.test.ts, which
/// reads both production pins as source text.
///
/// A COMPILED DEPLOYMENT CONSTANT, NOT A DECLARED FIELD (owner ruling
/// 2026-08-23 on #627, option B). The rate that convicts a block is not the
/// block producer's to declare, so it is neither a `HeaderV1` field nor a
/// `ValidationContextV1` element: it is compiled into the validators and is
/// bound, with the rest of the compiled constant set, by the validator
/// script hashes themselves (a rate change moves every validator that reads
/// it, as the #617 regeneration records). Changing the rate is therefore a
/// redeploy event, exactly as for `max_distinct_asset_count`
/// (lib/midgard/ledger-output-v1.ak). It lives in `env/` rather than beside
/// `max_distinct_asset_count` because it is a per-network protocol parameter of
/// the target chain rather than a Midgard-internal structural bound; it is
/// pinned identically in `env/default.ak` and `env/testnet.ak` because both
/// deployments target the same C70 snapshot.
pub const coins_per_utxo_byte: Int = 4_310

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_list_hash: H32<List<Data>> = [] |> cbor.serialise |> blake2b_256

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

pub const plutarch_phas_validator_hash: ScriptHash =
  #"1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

pub const plutarch_pexcludes_validator_hash: ScriptHash =
  #"03adaadf3154dafde48eea40030cecf5690b07c495f4c74029e4ab6a"

/// Merkelized verifier of published-chunk MPF proof carriage (issue #545).
/// Compiled from `validators/mpf-chunked-verify.ak` in this very project,
/// so this constant is the blueprint hash of
/// `mpf_chunked_verify.verify.withdraw`
/// and is re-pinned whenever that validator changes.
pub const mpf_chunked_verify_validator_hash: ScriptHash =
  #"dfd0e01fe351bd1d6f75a1ba728d06fb8b11d56bc3bf9ee98e025040"

pub const user_events_witness_script_prefix: ByteArray =
  #"5902e20101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc3a400200514a0809901319199119801001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c966002003168992cc004cdd78008044566002601460286ea8006264b3001300c3015375400315980099baf3018301637540046030602c6ea800629462c80a22c80a0c05c00a2c809a2c8098c058009015180b000998019bac3001301237540146eb4c050c048dd50064566002601060226ea8016264b30010018b44c966002601260266ea80062b30013375e602c60286ea8004c058c050dd5003c528c59012459012180a800a028330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0240020028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c1225820"
