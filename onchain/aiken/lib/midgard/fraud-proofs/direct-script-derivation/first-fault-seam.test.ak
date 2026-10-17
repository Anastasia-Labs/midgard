use aiken/cbor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}

// The same encodings are pinned off-chain in
// demo/midgard-sdk/tests/direct-script-derivation-first-fault-seam.test.ts.
test purpose_coordinate_encodes_as_off_chain() {
  cbor.serialise(PurposeCoordinateV1 { kind: 3, index: 7 }) == #"d8799f0307ff"
}

test network_order_context_encodes_as_off_chain() {
  cbor.serialise(NetworkOrderContextV1 { body_network_mismatch: True }) == #"d8799fd87a80ff"
}
