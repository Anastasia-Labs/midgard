use aiken/primitive/bytearray
use midgard/fraud_proofs/mint_authorization/mint_scan as scan
use midgard/native_tx_field_access_v1.{encode_field_preimage}

const policy = #"42424242424242424242424242424242424242424242424242424242"

fn field(assets: ByteArray) -> ByteArray {
  encode_field_preimage(
    [#"82581c" |> bytearray.concat(policy) |> bytearray.concat(assets)],
  )
}

test selected_policy_resumes_between_assets_and_matches_burn_semantics() {
  let bytes = field(#"a241410142414220")
  let opened = scan.advance(scan.initial(bytes, 0), bytes, 0, 1)
  let first = scan.advance(opened, bytes, 0, 1)
  let last = scan.advance(first, bytes, 0, 1)
  and {
    opened.remaining_assets == 2,
    first.remaining_assets == 1,
    !scan.complete(first, bytearray.length(bytes)),
    scan.complete(last, bytearray.length(bytes)),
    last.policy_id == policy,
  }
}

test trailing_field_bytes_never_complete() {
  let bytes = field(#"a14001") |> bytearray.concat(#"00")
  !scan.complete(
    scan.advance(scan.initial(bytes, 0), bytes, 0, 32),
    bytearray.length(bytes),
  )
}

test missing_declared_field_item_never_completes() fail {
  let bytes = #"82" |> bytearray.concat(bytearray.drop(field(#"a14001"), 1))
  scan.advance(scan.initial(bytes, 0), bytes, 0, 32).selected_complete
}

test zero_quantity_refused() fail {
  let bytes = field(#"a14000")
  scan.advance(scan.initial(bytes, 0), bytes, 0, 32).selected_complete
}

test out_of_order_names_refused() fail {
  let bytes = field(#"a242414201414101")
  scan.advance(scan.initial(bytes, 0), bytes, 0, 32).selected_complete
}

test duplicate_names_refused() fail {
  let bytes = field(#"a2414101414101")
  scan.advance(scan.initial(bytes, 0), bytes, 0, 32).selected_complete
}

test trailing_selected_item_bytes_refused() fail {
  let bytes = field(#"a1400100")
  scan.advance(scan.initial(bytes, 0), bytes, 0, 32).selected_complete
}

test empty_assets_refused() fail {
  let bytes = field(#"a0")
  scan.advance(scan.initial(bytes, 0), bytes, 0, 32).selected_complete
}

test impossible_coordinate_refused() fail {
  scan.initial(field(#"a14001"), 1).selected_complete
}
