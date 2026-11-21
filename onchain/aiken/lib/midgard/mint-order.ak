//// The checks every mint-field item (§5.6) must pass, shared by the validation
//// machine's stage-6 mint fold (validation-machine/script-sources-middle-stages)
//// and the tx-order mint's forced-transaction admission
//// (user-events/tx-order-v1), so the two cannot drift: an order the mint
//// admits never stalls the machine's mint fold.

use aiken/primitive/bytearray
use midgard/ledger_output_v1.{asset_key_precedes}

/// The length of a policy id.
pub const policy_id_bytes: Int = 28

/// The longest asset name.
pub const max_asset_name_bytes: Int = 32

/// A policy entry `[policy_id, {asset_name => quantity}]`: a 28-byte policy id
/// strictly after the previous policy's (byte order), with at least one asset.
/// `first` says there is no previous policy.
pub fn mint_policy_is_valid(
  first: Bool,
  previous_policy: ByteArray,
  policy_id: ByteArray,
  asset_count: Int,
) -> Bool {
  and {
    bytearray.length(policy_id) == policy_id_bytes,
    asset_count > 0,
    or {
      first,
      asset_key_precedes(previous_policy, policy_id),
    },
  }
}

/// One asset of a policy entry: a name of at most 32 bytes strictly after the
/// previous name in the entry (byte order), and a non-zero quantity. `first`
/// says there is no previous name.
pub fn mint_asset_is_valid(
  first: Bool,
  previous_asset: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
) -> Bool {
  and {
    bytearray.length(asset_name) <= max_asset_name_bytes,
    quantity != 0,
    or {
      first,
      asset_key_precedes(previous_asset, asset_name),
    },
  }
}
