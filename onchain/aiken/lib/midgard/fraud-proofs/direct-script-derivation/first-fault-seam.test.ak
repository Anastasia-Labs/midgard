use aiken/cbor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
} as seam
use midgard/rejection_reason_v1 as reason

// The same encodings are pinned off-chain in
// demo/midgard-sdk/tests/direct-script-derivation-first-fault-seam.test.ts.
test purpose_coordinate_encodes_as_off_chain() {
  cbor.serialise(PurposeCoordinateV1 { kind: 3, index: 7 }) == #"d8799f0307ff"
}

test network_order_context_encodes_as_off_chain() {
  cbor.serialise(NetworkOrderContextV1 { body_network_mismatch: True }) == #"d8799fd87a80ff"
}

fn script_item(kind: Int) -> reason.RejectionReasonV1 {
  reason.PlutusScriptUndecodable { item_kind: kind, item_index: 0, class: 1 }
}

fn phase(r: reason.RejectionReasonV1) -> Int {
  seam.phase_of_reason_v1(
    r,
    NetworkOrderContextV1 { body_network_mismatch: False },
  )
}

// A witness script item (kind 0) is decoded with the native witness scripts
// and an output reference script item (kind 1) with the output scripts.
test witness_script_item_shares_the_native_witness_script_phase() {
  phase(script_item(0)) == phase(
    reason.WitnessNativeScriptMalformed { script_index: 0 },
  )
}

test output_reference_script_item_shares_the_output_script_phase() {
  phase(script_item(1)) == phase(
    reason.OutputReferenceScriptMalformed { output_index: 0 },
  )
}

test script_item_of_unknown_kind_has_no_phase() fail {
  phase(script_item(2)) >= 0
}

test script_item_of_negative_kind_has_no_phase() fail {
  phase(script_item(-1)) >= 0
}
