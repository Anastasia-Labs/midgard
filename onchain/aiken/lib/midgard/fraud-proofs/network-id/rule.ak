//// ## `network-id` — normative rule (GOAL_SPEC.md Q35)
////
//// **Violation `transaction-network`.** An accepted native-V1 transaction
//// explicitly carries a transaction-body network id other than the network
//// configured by the deployed proof family. The canonical value `255` means
//// that the optional Cardano field is absent and is not, by itself, a fault.
////
//// **Violation `output-network`.** An accepted native-V1 transaction creates
//// an output whose canonical Midgard address network id differs from that
//// configured network.
////
//// The expected id is a family parameter and is restricted to the canonical
//// Midgard networks `0` (testnet) and `1` (mainnet). Step 01 authenticates the
//// compact transaction and its accepted validity code. Step 02 either uses
//// the compact body's native network field or opens body field 2 (`outputs`)
//// through the flat field commitment before applying these predicates.

use aiken/collection/list

/// Canonical native-V1 spelling of an absent optional Cardano network-id body
/// field. This is not an address network id.
pub const native_network_id_none: Int = 255

/// The only network ids accepted as the deployment parameter. Observed output
/// addresses deliberately remain decodable for foreign network nibbles so a
/// committed mismatch can be convicted instead of becoming unprovable.
pub fn is_supported_network_id_v1(network_id: Int) -> Bool {
  or {
    network_id == 0,
    network_id == 1,
  }
}

/// Decisive `transaction-network` predicate. Absence is allowed by Cardano;
/// an explicit foreign id is not.
pub fn is_transaction_network_violation_v1(
  committed_network_id: Int,
  expected_network_id: Int,
) -> Bool {
  and {
    committed_network_id != native_network_id_none,
    committed_network_id != expected_network_id,
  }
}

/// Decisive `output-network` predicate over a canonically decoded output.
pub fn is_output_network_violation_v1(
  output_network_id: Int,
  expected_network_id: Int,
) -> Bool {
  output_network_id != expected_network_id
}

/// Complete predicate needed to contradict the transaction-global forced
/// `NetworkIdMismatch` reason.  The proof must establish that the optional
/// transaction network and every output address agree with the deployment.
pub fn is_any_network_violation_v1(
  committed_network_id: Int,
  output_network_ids: List<Int>,
  expected_network_id: Int,
) -> Bool {
  or {
    is_transaction_network_violation_v1(
      committed_network_id,
      expected_network_id,
    ),
    output_network_ids |> list.any(fn(id) { id != expected_network_id }),
  }
}
