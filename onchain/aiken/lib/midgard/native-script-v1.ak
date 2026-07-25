use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

/// Midgard V1 bounds recursive native scripts independently of the outer byte
/// bound. Without these limits a tiny, deeply nested script can exceed the L1
/// one-step envelope even though its serialized program is below 2 KiB.
pub const max_native_script_depth: Int = 16

pub const max_native_script_node_count: Int = 32

pub type NativeScriptCheck {
  canonical_cbor: ByteArray,
  valid: Bool,
  node_count: Int,
  depth: Int,
}

type NativeChildrenCheck {
  canonical_children: ByteArray,
  all_valid: Bool,
  any_valid: Bool,
  valid_count: Int,
  node_count: Int,
  max_depth: Int,
}

fn data_is_list(data: Data) -> Bool {
  builtin.choose_data(data, False, False, True, False, False)
}

fn data_is_int(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, True, False)
}

fn data_is_bytes(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, False, True)
}

fn list_data(data: Data) -> Option<List<Data>> {
  if data_is_list(data) {
    Some(builtin.un_list_data(data))
  } else {
    None
  }
}

fn int_data(data: Data) -> Option<Int> {
  if data_is_int(data) {
    Some(builtin.un_i_data(data))
  } else {
    None
  }
}

fn bytes_data(data: Data) -> Option<ByteArray> {
  if data_is_bytes(data) {
    Some(builtin.un_b_data(data))
  } else {
    None
  }
}

fn larger(left: Int, right: Int) -> Int {
  if left >= right {
    left
  } else {
    right
  }
}

fn check_children(
  children: List<Data>,
  validity_interval_start: Int,
  validity_interval_end: Int,
  witness_signers: List<ByteArray>,
) -> Option<NativeChildrenCheck> {
  when children is {
    [] ->
      Some(
        NativeChildrenCheck {
          canonical_children: #"",
          all_valid: True,
          any_valid: False,
          valid_count: 0,
          node_count: 0,
          max_depth: 0,
        },
      )
    [child, ..rest] ->
      when
        check_native_script_data(
          child,
          validity_interval_start,
          validity_interval_end,
          witness_signers,
        )
      is {
        None -> None
        Some(checked_child) ->
          when
            check_children(
              rest,
              validity_interval_start,
              validity_interval_end,
              witness_signers,
            )
          is {
            None -> None
            Some(checked_rest) ->
              Some(
                NativeChildrenCheck {
                  canonical_children: checked_child.canonical_cbor
                    |> bytearray.concat(checked_rest.canonical_children),
                  all_valid: and {
                    checked_child.valid,
                    checked_rest.all_valid,
                  },
                  any_valid: or {
                    checked_child.valid,
                    checked_rest.any_valid,
                  },
                  valid_count: checked_rest.valid_count + if checked_child.valid {
                    1
                  } else {
                    0
                  },
                  node_count: checked_child.node_count + checked_rest.node_count,
                  max_depth: larger(checked_child.depth, checked_rest.max_depth),
                },
              )
          }
      }
  }
}

fn check_native_script_data(
  data: Data,
  validity_interval_start: Int,
  validity_interval_end: Int,
  witness_signers: List<ByteArray>,
) -> Option<NativeScriptCheck> {
  when list_data(data) is {
    None -> None
    Some(fields) ->
      when fields is {
        [tag_data, argument_data] ->
          when int_data(tag_data) is {
            None -> None
            Some(tag) ->
              if tag == 0 {
                when bytes_data(argument_data) is {
                  Some(key_hash) ->
                    if bytearray.length(key_hash) == 28 {
                      Some(
                        NativeScriptCheck {
                          canonical_cbor: encode_definite_array_header(2)
                            |> bytearray.concat(cbor.serialise(0))
                            |> bytearray.concat(encode_definite_bytes(key_hash)),
                          valid: list.has(witness_signers, key_hash),
                          node_count: 1,
                          depth: 1,
                        },
                      )
                    } else {
                      None
                    }
                  None -> None
                }
              } else if or {
                tag == 1,
                tag == 2,
              } {
                when list_data(argument_data) is {
                  None -> None
                  Some(children) ->
                    when
                      check_children(
                        children,
                        validity_interval_start,
                        validity_interval_end,
                        witness_signers,
                      )
                    is {
                      None -> None
                      Some(checked) ->
                        Some(
                          NativeScriptCheck {
                            canonical_cbor: encode_definite_array_header(2)
                              |> bytearray.concat(cbor.serialise(tag))
                              |> bytearray.concat(
                                  encode_definite_array_header(
                                    list.length(children),
                                  ),
                                )
                              |> bytearray.concat(checked.canonical_children),
                            valid: if tag == 1 {
                              checked.all_valid
                            } else {
                              checked.any_valid
                            },
                            node_count: checked.node_count + 1,
                            depth: checked.max_depth + 1,
                          },
                        )
                    }
                }
              } else if or {
                tag == 4,
                tag == 5,
              } {
                when int_data(argument_data) is {
                  Some(slot) ->
                    if slot >= 0 {
                      Some(
                        NativeScriptCheck {
                          canonical_cbor: encode_definite_array_header(2)
                            |> bytearray.concat(cbor.serialise(tag))
                            |> bytearray.concat(cbor.serialise(slot)),
                          valid: if tag == 4 {
                            and {
                              validity_interval_start >= 0,
                              validity_interval_start >= slot,
                            }
                          } else {
                            and {
                              validity_interval_end >= 0,
                              validity_interval_end <= slot,
                            }
                          },
                          node_count: 1,
                          depth: 1,
                        },
                      )
                    } else {
                      None
                    }
                  None -> None
                }
              } else {
                None
              }
          }
        [tag_data, required_data, children_data] ->
          when int_data(tag_data) is {
            Some(3) ->
              when int_data(required_data) is {
                Some(required) ->
                  when list_data(children_data) is {
                    Some(children) ->
                      if required >= 0 {
                        when
                          check_children(
                            children,
                            validity_interval_start,
                            validity_interval_end,
                            witness_signers,
                          )
                        is {
                          None -> None
                          Some(checked) ->
                            Some(
                              NativeScriptCheck {
                                canonical_cbor: encode_definite_array_header(3)
                                  |> bytearray.concat(cbor.serialise(3))
                                  |> bytearray.concat(cbor.serialise(required))
                                  |> bytearray.concat(
                                      encode_definite_array_header(
                                        list.length(children),
                                      ),
                                    )
                                  |> bytearray.concat(
                                      checked.canonical_children,
                                    ),
                                valid: checked.valid_count >= required,
                                node_count: checked.node_count + 1,
                                depth: checked.max_depth + 1,
                              },
                            )
                        }
                      } else {
                        None
                      }
                    None -> None
                  }
                None -> None
              }
            _ -> None
          }
        _ -> None
      }
  }
}

/// Returns `None` for malformed or non-canonical scripts. Complexity is
/// returned separately so canonical admission can derive the exact depth or
/// node-count rejection code.
pub fn inspect_native_script(
  script_cbor: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  witness_signers: List<ByteArray>,
) -> Option<NativeScriptCheck> {
  when cbor.deserialise(script_cbor) is {
    None -> None
    Some(data) ->
      when
        check_native_script_data(
          data,
          validity_interval_start,
          validity_interval_end,
          witness_signers,
        )
      is {
        None -> None
        Some(checked) ->
          if checked.canonical_cbor == script_cbor {
            Some(checked)
          } else {
            None
          }
      }
  }
}

/// Returns `None` for malformed, non-canonical, or proof-unbounded scripts.
/// The caller can therefore derive a rejecting terminal state without relying
/// on an `expect` failure.
pub fn check_native_script(
  script_cbor: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  witness_signers: List<ByteArray>,
) -> Option<NativeScriptCheck> {
  when
    inspect_native_script(
      script_cbor,
      validity_interval_start,
      validity_interval_end,
      witness_signers,
    )
  is {
    None -> None
    Some(checked) ->
      if and {
        checked.depth <= max_native_script_depth,
        checked.node_count <= max_native_script_node_count,
      } {
        Some(checked)
      } else {
        None
      }
  }
}
