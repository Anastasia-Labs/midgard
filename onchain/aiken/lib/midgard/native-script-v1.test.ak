use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/native_script_v1

const signer = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const sig_script = #"8200" |> bytearray.concat(encode_definite_bytes(signer))

fn nest_all(script: ByteArray, count: Int) -> ByteArray {
  if count <= 0 {
    script
  } else {
    nest_all(#"820181" |> bytearray.concat(script), count - 1)
  }
}

fn repeat_after_children(count: Int) -> ByteArray {
  if count <= 0 {
    #""
  } else {
    #"820400" |> bytearray.concat(repeat_after_children(count - 1))
  }
}

fn any_after_scripts(count: Int) -> ByteArray {
  #"8202"
    |> bytearray.concat(encode_definite_array_header(count))
    |> bytearray.concat(repeat_after_children(count))
}

fn is_canonical_with_validity(
  script: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  witness_signers: List<ByteArray>,
  expected_valid: Bool,
) -> Bool {
  when
    native_script_v1.check_native_script(
      script,
      validity_interval_start,
      validity_interval_end,
      witness_signers,
    )
  is {
    Some(checked) -> and {
        checked.canonical_cbor == script,
        checked.valid == expected_valid,
        checked.node_count == 1,
        checked.depth == 1,
      }
    None -> False
  }
}

test native_script_v1_reconstructs_and_evaluates_a_signature_script() {
  when native_script_v1.check_native_script(sig_script, -1, -1, [signer]) is {
    Some(checked) -> and {
        checked.canonical_cbor == sig_script,
        checked.valid,
        checked.node_count == 1,
        checked.depth == 1,
      }
    None -> False
  }
}

test native_script_v1_pins_every_cardano_tag_and_arity() {
  and {
    is_canonical_with_validity(sig_script, -1, -1, [signer], True),
    is_canonical_with_validity(#"820180", -1, -1, [], True),
    is_canonical_with_validity(#"820280", -1, -1, [], False),
    is_canonical_with_validity(#"83030080", -1, -1, [], True),
    is_canonical_with_validity(#"820400", 0, -1, [], True),
    is_canonical_with_validity(#"820500", -1, 0, [], True),
    native_script_v1.check_native_script(#"820600", -1, -1, []) == None,
  }
}

test native_script_v1_rejects_a_non_canonical_indefinite_script() {
  let non_canonical =
    #"9f00"
      |> bytearray.concat(encode_definite_bytes(signer))
      |> bytearray.concat(#"ff")
  native_script_v1.check_native_script(non_canonical, -1, -1, [signer]) == None
}

test native_script_v1_applies_validity_bounds_and_at_least() {
  // [3, 2, [[0, signer], [4, 100], [5, 200]]]
  let script =
    #"83030283"
      |> bytearray.concat(sig_script)
      |> bytearray.concat(#"82041864")
      |> bytearray.concat(#"820518c8")
  when native_script_v1.check_native_script(script, 100, 200, [signer]) is {
    Some(checked) -> and {
        checked.valid,
        checked.node_count == 4,
        checked.depth == 2,
      }
    None -> False
  }
}

test native_script_v1_accepts_depth_16_and_rejects_depth_17() {
  and {
    native_script_v1.check_native_script(
      nest_all(sig_script, 15),
      -1,
      -1,
      [signer],
    ) != None,
    native_script_v1.check_native_script(
      nest_all(sig_script, 16),
      -1,
      -1,
      [signer],
    ) == None,
  }
}

test native_script_v1_rejects_more_than_32_nodes() {
  and {
    when
      native_script_v1.check_native_script(any_after_scripts(31), 0, -1, [])
    is {
      Some(checked) -> checked.node_count == 32
      None -> False
    },
    native_script_v1.check_native_script(any_after_scripts(32), 0, -1, []) == None,
  }
}
