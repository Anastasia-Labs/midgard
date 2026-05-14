use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use midgard/common/types.{H32}

// Including ADA.
pub const max_tokens_allowed_in_deposits: Int = 10

pub const event_wait_duration: Int = 60_000

pub const empty_list_hash: H32<List<Data>> =
  [] |> cbor.serialise |> blake2b_256

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
