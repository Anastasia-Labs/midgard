use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{Empty, PosixTime, PosixTimeDuration}

// *** //
// env //
// *** //

// Including ADA.
pub const max_tokens_allowed_in_deposits: Int = 10

pub const event_wait_duration: Int = 60_000

pub const l2_outbox_address: Address =
  Address {
    payment_credential: Script(
      #"00000000000000000000000000000000000000000000000000000000",
    ),
    stake_credential: None,
  }

pub const l2_outbox_policy_id: PolicyId =
  #"00000000000000000000000000000000000000000000000000000000"

pub const l2_outbox_asset_name: AssetName = #""

pub const plutarch_phas_validator_hash: ScriptHash =
  #"46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d"

pub const plutarch_pexcludes_validator_hash: ScriptHash =
  #"daa1fd7f054b1d4736eb9fa610b423b09ef81683eb1201f0f1f2591d"

pub const plutarch_pdelete_validator_hash: ScriptHash = #""

// ********* //
// constants //
// ********* //

pub const posix_time_none: PosixTime = -1

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

pub const plutus_version: Int = 3

// ******************* //
// protocol_parameters //
// ******************* //

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

// ******* //
// witness //
// ******* //

pub const user_events_witness_script_prefix: ByteArray =
  #"59030601010033232323232323223225333004323232323253330093370e900318051baa00113233223232323253330103003001132325333015301800200416375c602c00260246ea802454ccc040c0100044c8c94ccc054c06000801058dd6980b00098091baa009153330103370e900200089919299980a980c0010020b1bad3016001301237540122c60206ea80204c8c8c8c94ccc048c0140044c8c8c94ccc054c0200044c02400854ccc054c0240044cdc3801240022940c054dd500499299980a1804180a9baa0011480004dd6980c980b1baa001325333014300830153754002298103d87a80001323300100137566034602e6ea8008894ccc064004530103d87a8000132333222533301a337220300062a66603466e3c06000c4cdd2a40006603c6ea00092f5c02980103d87a8000133006006001375c60300026eb4c064004c074008c06c004c8cc004004dd5980c980d180d180d180d180b1baa00e22533301800114c103d87a800013233322253330193372200e0062a66603266e3c01c00c4cdd2a40006603a6e980092f5c02980103d87a8000133006006001375c602e0026eacc060004c070008c068004dd7180b980a1baa00b153330123006001132323253330153375e0040142a66602a6010602c6ea80084c94ccc058c028c05cdd5001099baf001301b301837540042c6034602e6ea80085858c064c068008c060004cc00cdd61801180a1baa00c375a602e60286ea802c4c94ccc04cc01cc050dd500409919299980a9804180b1baa00113375e6034602e6ea800400858c064cc014dd61802180b1baa00e0023018301537540102c6eb4c05cc050dd500598091baa00a23016301730173017301730170013001001222533301133712002900008010a99980a0010a5eb804ccc00c00cc05c008cdc0000a40026e1d2000370e90011bad300f001300f3010001300b37540022c601a601c006601800460160046016002600c6ea800452613656375c002ae6955ceaab9e5573eae815d0aba24c1225820"
