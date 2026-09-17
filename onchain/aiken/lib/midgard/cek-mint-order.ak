use aiken/primitive/bytearray
use midgard/validation_machine/machine_types.{CekContextControlV1, CekMintHead}
use midgard/validation_machine/shared.{prepend_mint_asset_summary}

/// Strict descending execution order over independently authenticated source
/// indices. The head opening is checked against the accumulated map commitment.
pub fn order_is_valid(
  context: CekContextControlV1,
  policy: ByteArray,
  name: ByteArray,
  previous: Option<CekMintHead>,
) -> Bool {
  if context.current_mint_policy == #"" {
    previous == None && context.mint_cursor == 0
  } else if policy == context.current_mint_policy {
    expect Some(CekMintHead { asset_name, quantity, tail }) = previous
    and {
      bytearray.compare(name, asset_name) == Less,
      prepend_mint_asset_summary(asset_name, quantity, tail) == context.current_mint_assets,
    }
  } else {
    previous == None && bytearray.compare(policy, context.current_mint_policy) == Less
  }
}
