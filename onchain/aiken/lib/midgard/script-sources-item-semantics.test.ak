use aiken/primitive/bytearray
use midgard/bounded_item_v1 as bounded
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_item_semantics as semantics

fn initial(bytes: ByteArray) -> item.RedeemerItemProofControlV1 {
  item.initial_control_v1(
    item.mode_data,
    0,
    1,
    bytearray.length(bytes),
    bounded.from_bytes(8, 0, bytes),
    -1,
    -1,
  )
}

fn execution(
  control: item.RedeemerItemProofControlV1,
  action: item.RedeemerItemProofActionV1,
  bytes: ByteArray,
) -> normalized.OuterExecution {
  let attestation: Data = 0
  normalized.OuterExecution {
    output: normalized.ExecutionOutput {
      deployment_id: #"",
      family: 9,
      executor_script_hash: #"",
      settlement_script_hash: #"",
      attestation,
    },
    current: control,
    next: control,
    action,
    source_bytes: Some(bytes),
  }
}

test invalid_header_proves_exact_canonical_invalid_result() {
  let bytes = #"ff"
  semantics.invalid_header(
    execution(initial(bytes), item.RedeemerItemOpenHeader, bytes),
  )
}

test invalid_header_refuses_an_honest_header() {
  let bytes = #"8400004100820a14"
  !semantics.invalid_header(
    execution(initial(bytes), item.RedeemerItemOpenHeader, bytes),
  )
}

test invalid_tail_proves_exact_canonical_invalid_result() {
  let bytes = #"8400004100810a"
  expect item.RedeemerItemProofAdvanced { control } = item.prevalidated_open_header(
    initial(bytes),
    bytes,
  )
  semantics.invalid_tail(execution(control, item.RedeemerItemOpenTail, #"810a"))
}

test invalid_tail_refuses_an_honest_tail() {
  let bytes = #"8400004100820a14"
  expect item.RedeemerItemProofAdvanced { control } = item.prevalidated_open_header(
    initial(bytes),
    bytes,
  )
  !semantics.invalid_tail(
    execution(control, item.RedeemerItemOpenTail, #"820a14"),
  )
}
