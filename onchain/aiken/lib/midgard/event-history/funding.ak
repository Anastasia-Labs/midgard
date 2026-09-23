//// Explicit structural funding and bounded filler refunds. These helpers
//// complement authenticated role/mutation checks; they cannot authorize a
//// spend on their own. Deposit projection uses only the returned actual Value,
//// never the funding of a promoted filler or a separate data publication.

use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{Datum, InlineDatum, NoDatum, Output}
use env
use midgard/event_history/ordering.{Link}
use midgard/ledger_state.{WithdrawalBody}
use midgard/payout

/// Conservative canonical Cardano output envelope: 160 ledger entry bytes,
/// then 96 bytes for a Shelley address (including pointer addresses), map keys,
/// array/tag/bytes framing and datum-option framing. Plutus Value serialization
/// upper-bounds the corresponding ledger Value. Reference scripts are forbidden.
/// The existing compiled coins_per_utxo_byte pin is part of deployment identity.
pub fn minimum_output_lovelace(value: Value, datum: Datum, added_nft: Bool) -> Int {
  let nft_bytes = if added_nft { 80 } else { 0 }
  env.coins_per_utxo_byte * (
    160 + 96 + bytearray.length(cbor.serialise(value)) + bytearray.length(cbor.serialise(datum)) + nft_bytes
  )
}

/// Fund the widest possible successor and timestamp now. Later pointer-only
/// continuations preserve the entire original Value and immutable facts.
pub fn node_is_funded(value: Value, node: Link<Data>) -> Bool {
  let worst: Data = Link {
    ..node,
    next: Some(#"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"),
    protected_until: ordering.max_protection_time,
  }
  assets.lovelace_of(value) >= minimum_output_lovelace(value, InlineDatum(worst), False)
}

pub fn deposit_is_funded(original_assets: Value) -> Bool {
  assets.lovelace_of(original_assets) >= minimum_output_lovelace(original_assets, NoDatum, False)
}

/// The locked ADA starts the payout accumulator, so it must fit both possible
/// retirement outputs and must not exceed the requested payout. The complete
/// target must also fit the accumulator before its concluding L1 payment.
pub fn withdrawal_is_funded(
  locked: Value,
  body: WithdrawalBody,
  refund_datum: Datum,
) -> Bool {
  let payout_data: Data = payout.Datum {
    l2_value: body.l2_value,
    l1_address: body.l1_address,
    l1_datum: body.l1_datum,
  }
  let target = assets.from_asset_list(body.l2_value)
  let locked_ada = assets.lovelace_of(locked)
  let target_ada = assets.lovelace_of(target)
  and {
    list.all(assets.flatten(target), fn(asset) {
      let (_, _, quantity) = asset
      quantity >= 0
    }),
    locked_ada <= target_ada,
    locked_ada >= minimum_output_lovelace(locked, refund_datum, False),
    locked_ada >= minimum_output_lovelace(locked, InlineDatum(payout_data), True),
    target_ada >= minimum_output_lovelace(target, InlineDatum(payout_data), True),
  }
}

/// A filler carries only ADA and its structural NFT. Restricting the refund to
/// a plain key address and no datum makes its encoded size independent of its
/// creator's preferences. Promotion requires payment, never this key's signer.
pub fn valid_filler_value(
  value: Value,
  policy: PolicyId,
  key: ByteArray,
) -> Bool {
  let funding = assets.lovelace_of(value)
  let expected = assets.from_lovelace(funding) |> assets.add(policy, key, 1)
  and {
    funding > 0,
    value == expected,
  }
}

pub fn pays_filler_refund(
  filler_value: Value,
  policy: PolicyId,
  key: ByteArray,
  refund_key: VerificationKeyHash,
  refund: Output,
) -> Bool {
  and {
    bytearray.length(refund_key) == 28,
    valid_filler_value(filler_value, policy, key),
    refund.address == address.from_verification_key(refund_key),
    refund.datum == NoDatum,
    refund.reference_script == None,
    refund.value == assets.from_lovelace(assets.lovelace_of(filler_value)),
  }
}

/// The role validator fixes structural ADA at admission and preserves it in
/// every continuation. Ledger Values are nonnegative; removing the unique
/// structural token and nonnegative funding leaves exactly the original assets.
/// The event-specific admission rules additionally check the resulting assets
/// (including the existing deposit asset-count and L2 output constraints).
pub fn order_assets(
  value: Value,
  policy: PolicyId,
  key: ByteArray,
  structural_lovelace: Int,
) -> Value {
  let tokens = value |> assets.tokens(policy) |> dict.to_pairs
  expect tokens == [Pair(key, 1)]
  expect structural_lovelace >= 0
  expect structural_lovelace <= assets.lovelace_of(value)
  value
    |> assets.add(policy, key, -1)
    |> assets.add(#"", #"", -structural_lovelace)
}
