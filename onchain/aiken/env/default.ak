use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}

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

pub const plutarch_phas_validator_hash: ScriptHash = #""

pub const plutarch_pexcludes_validator_hash: ScriptHash = #""

pub const plutarch_pdelete_validator_hash: ScriptHash = #""
