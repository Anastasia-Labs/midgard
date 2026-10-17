//// Narrow first-fault interface consumed by the direct-script-derivation
//// library. The declarations are byte-for-byte copies of the frozen shared
//// first-fault API (network-order context and purpose coordinate), so the
//// Plutus Data encoding is identical. When the shared module carries these
//// declarations, every importer switches to it and this module is deleted.

use midgard/fraud_proofs/native_tx/types.{TxCompactView}
use midgard/ledger_state.{HeaderV1}

pub type NetworkOrderContextV1 {
  body_network_mismatch: Bool,
}

/// Both projections must already be authenticated under the same source/header.
pub fn network_order_context_v1(
  compact: TxCompactView,
  header: HeaderV1,
) -> NetworkOrderContextV1 {
  NetworkOrderContextV1 {
    body_network_mismatch: compact.body.network_id != 255 && compact.body.network_id != header.expected_network_id,
  }
}

pub type PurposeCoordinateV1 {
  kind: Int,
  index: Int,
}
