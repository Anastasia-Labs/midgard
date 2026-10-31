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

fn initial_descriptor(bytes: ByteArray) -> item.RedeemerItemProofControlV1 {
  item.initial_control_v1(
    item.mode_descriptor,
    0,
    1,
    bytearray.length(bytes),
    bounded.from_bytes(8, 0, bytes),
    -1,
    -1,
  )
}

fn stepped(
  current: item.RedeemerItemProofControlV1,
  next: item.RedeemerItemProofControlV1,
  action: item.RedeemerItemProofActionV1,
  bytes: ByteArray,
) -> normalized.OuterExecution {
  normalized.OuterExecution { ..execution(current, action, bytes), next }
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

// The CEK context binds descriptor-mode selection items into the shared
// chain (context stage 0), so the open executors accept the full step_v1
// header/tail domain: descriptor steps advance by the same canonical machine.
test open_header_proves_the_descriptor_mode_canonical_advance() {
  let bytes = #"8400004100820a14"
  let current = initial_descriptor(bytes)
  expect item.RedeemerItemProofAdvanced { control: next } = item.prevalidated_open_header(
    current,
    bytes,
  )
  semantics.open_header(
    stepped(current, next, item.RedeemerItemOpenHeader, bytes),
  )
}

test open_header_refuses_a_forged_descriptor_mode_successor() {
  let bytes = #"8400004100820a14"
  let current = initial_descriptor(bytes)
  !semantics.open_header(
    stepped(current, current, item.RedeemerItemOpenHeader, bytes),
  )
}

test open_tail_proves_the_descriptor_mode_terminal_advance() {
  let bytes = #"8400004100820a14"
  expect item.RedeemerItemProofAdvanced { control } = item.prevalidated_open_header(
    initial_descriptor(bytes),
    bytes,
  )
  expect item.RedeemerItemProofAdvanced { control: next } = item.prevalidated_open_tail(
    control,
    #"820a14",
  )
  expect next.stage == item.stage_terminal
  expect next.traversal == None
  semantics.open_tail(
    stepped(control, next, item.RedeemerItemOpenTail, #"820a14"),
  )
}

test open_tail_refuses_a_data_shaped_successor_for_a_descriptor_item() {
  let bytes = #"8400004100820a14"
  expect item.RedeemerItemProofAdvanced { control } = item.prevalidated_open_header(
    initial_descriptor(bytes),
    bytes,
  )
  // Forge the mode_data tail successor shape (stage_data + opened traversal)
  // for a descriptor item: the canonical machine pins stage_terminal/None.
  expect item.RedeemerItemProofAdvanced { control: data_next } = item.prevalidated_open_tail(
    item.RedeemerItemProofControlV1 { ..control, mode: item.mode_data },
    #"820a14",
  )
  let forged = item.RedeemerItemProofControlV1 {
    ..data_next,
    mode: item.mode_descriptor,
  }
  !semantics.open_tail(
    stepped(control, forged, item.RedeemerItemOpenTail, #"820a14"),
  )
}

// Negative polarity of the widened mode selector itself: `mode_descriptor ||
// mode_data` is the FULL legal domain (`redeemer_item_proof_v1` defines only
// those two), so widening the executors did not drop a semantic requirement —
// `outer_fields_are_well_formed` inside `prevalidated_open_header` /
// `prevalidated_open_tail` pins the same `mode == 0 || mode == 1` domain, and
// the canonical successor equality pins the mode-dependent shape. These two
// tests keep the selector non-vacuous: an out-of-domain mode is refused at
// both executors. The selector is an `expect`, so its refusal traps rather
// than returning `False` — hence `fail`. Everything else in each fixture is
// the genuine canonical pair, so the mode is the only thing that can refuse
// (and the canonical machine would refuse the same control a second time:
// `outer_fields_are_well_formed` rejects `mode == 2`).
test open_header_refuses_an_out_of_domain_mode() fail {
  let bytes = #"8400004100820a14"
  let current = initial_descriptor(bytes)
  let forged = item.RedeemerItemProofControlV1 { ..current, mode: 2 }
  expect item.RedeemerItemProofAdvanced { control: next } = item.prevalidated_open_header(
    current,
    bytes,
  )
  semantics.open_header(
    stepped(forged, next, item.RedeemerItemOpenHeader, bytes),
  )
}

test open_tail_refuses_an_out_of_domain_mode() fail {
  let bytes = #"8400004100820a14"
  expect item.RedeemerItemProofAdvanced { control } = item.prevalidated_open_header(
    initial_descriptor(bytes),
    bytes,
  )
  expect item.RedeemerItemProofAdvanced { control: next } = item.prevalidated_open_tail(
    control,
    #"820a14",
  )
  let forged = item.RedeemerItemProofControlV1 { ..control, mode: 2 }
  semantics.open_tail(
    stepped(forged, next, item.RedeemerItemOpenTail, #"820a14"),
  )
}

test open_header_still_proves_the_data_mode_canonical_advance() {
  let bytes = #"8400004100820a14"
  let current = initial(bytes)
  expect item.RedeemerItemProofAdvanced { control: next } = item.prevalidated_open_header(
    current,
    bytes,
  )
  semantics.open_header(
    stepped(current, next, item.RedeemerItemOpenHeader, bytes),
  )
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
