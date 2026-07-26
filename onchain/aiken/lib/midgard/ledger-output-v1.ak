use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardCredential, MidgardTxOutput, MidgardV1Script,
  MidgardValue, MidgardVersionedScript, NativeCardanoScript, PlutusV3Script,
  PubKeyCredential, ScriptCredential,
}

/// A ledger membership one-step reserves 12 KiB for the transition states,
/// MPF path, redeemer framing, and Cardano transaction overhead. The revealed
/// output is therefore strictly below the remaining 4 KiB. Values and datums
/// have no separate arbitrary cap: they share this concrete output-preimage
/// envelope with the address and optional reference script.
pub const max_output_canonical_cbor_bytes: Int = 4095

pub const max_distinct_asset_count: Int = 128

type ParsedAssets {
  assets: List<Pair<ByteArray, Int>>,
  count: Int,
}

fn data_is_map(data: Data) -> Bool {
  builtin.choose_data(data, False, True, False, False, False)
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

fn map_data(data: Data) -> Option<List<Pair<Data, Data>>> {
  if data_is_map(data) {
    Some(builtin.un_map_data(data))
  } else {
    None
  }
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

fn key_is(data: Data, expected: Int) -> Bool {
  when int_data(data) is {
    Some(actual) -> actual == expected
    None -> False
  }
}

fn canonical_bytes_key_precedes(left: ByteArray, right: ByteArray) -> Bool {
  let left_length = bytearray.length(left)
  let right_length = bytearray.length(right)
  if left_length < right_length {
    True
  } else if left_length > right_length {
    False
  } else {
    bytearray.compare(left, right) == Less
  }
}

fn append_assets(
  left: List<Pair<ByteArray, Int>>,
  right: List<Pair<ByteArray, Int>>,
) -> List<Pair<ByteArray, Int>> {
  when left is {
    [] -> right
    [item, ..rest] -> [item, ..append_assets(rest, right)]
  }
}

fn parse_asset_quantities(
  policy_id: ByteArray,
  entries: List<Pair<Data, Data>>,
  previous_name: Option<ByteArray>,
  remaining: Int,
) -> Option<ParsedAssets> {
  when entries is {
    [] -> Some(ParsedAssets { assets: [], count: 0 })
    [Pair(name_data, quantity_data), ..rest] ->
      if remaining <= 0 {
        None
      } else {
        when bytes_data(name_data) is {
          None -> None
          Some(name) ->
            when int_data(quantity_data) is {
              None -> None
              Some(quantity) -> {
                let ordered = when previous_name is {
                  None -> True
                  Some(previous) -> canonical_bytes_key_precedes(previous, name)
                }
                if and {
                  bytearray.length(name) <= 32,
                  quantity > 0,
                  ordered,
                } {
                  when
                    parse_asset_quantities(
                      policy_id,
                      rest,
                      Some(name),
                      remaining - 1,
                    )
                  is {
                    None -> None
                    Some(parsed_rest) ->
                      Some(
                        ParsedAssets {
                          assets: [
                            Pair(policy_id |> bytearray.concat(name), quantity),
                            ..parsed_rest.assets
                          ],
                          count: parsed_rest.count + 1,
                        },
                      )
                  }
                } else {
                  None
                }
              }
            }
        }
      }
  }
}

fn parse_policy_assets(
  entries: List<Pair<Data, Data>>,
  previous_policy: Option<ByteArray>,
  remaining: Int,
) -> Option<ParsedAssets> {
  when entries is {
    [] -> Some(ParsedAssets { assets: [], count: 0 })
    [Pair(policy_data, quantities_data), ..rest] ->
      when bytes_data(policy_data) is {
        None -> None
        Some(policy_id) ->
          when map_data(quantities_data) is {
            None -> None
            Some(quantity_entries) -> {
              let ordered = when previous_policy is {
                None -> True
                Some(previous) -> bytearray.compare(previous, policy_id) == Less
              }
              if and {
                bytearray.length(policy_id) == 28,
                ordered,
                !list.is_empty(quantity_entries),
              } {
                when
                  parse_asset_quantities(
                    policy_id,
                    quantity_entries,
                    None,
                    remaining,
                  )
                is {
                  None -> None
                  Some(parsed_policy) ->
                    when
                      parse_policy_assets(
                        rest,
                        Some(policy_id),
                        remaining - parsed_policy.count,
                      )
                    is {
                      None -> None
                      Some(parsed_rest) ->
                        Some(
                          ParsedAssets {
                            assets: append_assets(
                              parsed_policy.assets,
                              parsed_rest.assets,
                            ),
                            count: parsed_policy.count + parsed_rest.count,
                          },
                        )
                    }
                }
              } else {
                None
              }
            }
          }
      }
  }
}

fn parse_value(data: Data) -> Option<MidgardValue> {
  when list_data(data) is {
    Some([lovelace_data, assets_data]) ->
      when int_data(lovelace_data) is {
        Some(lovelace) ->
          when map_data(assets_data) is {
            Some(asset_entries) ->
              if lovelace >= 0 {
                when
                  parse_policy_assets(
                    asset_entries,
                    None,
                    max_distinct_asset_count,
                  )
                is {
                  Some(parsed) ->
                    Some(MidgardValue { lovelace, assets: parsed.assets })
                  None -> None
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
}

fn parse_address(data: Data) -> Option<MidgardAddress> {
  when bytes_data(data) is {
    None -> None
    Some(bytes) -> {
      let length = bytearray.length(bytes)
      if or {
        length == 29,
        length == 57,
      } {
        let header = bytearray.at(bytes, 0)
        let address_type = header / 16
        let network_nibble = header - address_type * 16
        let protected = network_nibble >= 8
        let network_id = if protected {
          network_nibble - 8
        } else {
          network_nibble
        }
        let payment_is_script = or {
          address_type == 1,
          address_type == 3,
          address_type == 7,
        }
        let payment_hash = bytearray.slice(bytes, 1, 28)
        let payment_credential: MidgardCredential = if payment_is_script {
          ScriptCredential(payment_hash)
        } else {
          PubKeyCredential(payment_hash)
        }
        if and {
          or {
            network_id == 0,
            network_id == 1,
          },
          if length == 29 {
            or {
              address_type == 6,
              address_type == 7,
            }
          } else {
            address_type >= 0 && address_type <= 3
          },
        } {
          let stake_credential = if length == 57 {
            let stake_hash = bytearray.slice(bytes, 29, 56)
            let stake_is_script = or {
              address_type == 2,
              address_type == 3,
            }
            Some(
              if stake_is_script {
                ScriptCredential(stake_hash)
              } else {
                PubKeyCredential(stake_hash)
              },
            )
          } else {
            None
          }
          Some(
            MidgardAddress {
              protected,
              network_id,
              payment_credential,
              stake_credential,
            },
          )
        } else {
          None
        }
      } else {
        None
      }
    }
  }
}

/// Canonical address decoder shared by the compact ledger-output descriptor.
/// It is deliberately independent of the whole-output proof envelope.
pub fn decode_canonical_address_bytes(
  address_bytes: ByteArray,
) -> Option<MidgardAddress> {
  parse_address(builtin.b_data(address_bytes))
}

fn parse_script_ref(data: Data) -> Option<MidgardVersionedScript> {
  when list_data(data) is {
    Some([tag_data, script_bytes_data]) ->
      when int_data(tag_data) is {
        None -> None
        Some(tag) ->
          when bytes_data(script_bytes_data) is {
            None -> None
            Some(script_bytes) ->
              if tag == 0 {
                Some(
                  MidgardVersionedScript {
                    language: NativeCardanoScript,
                    script_bytes,
                  },
                )
              } else if tag == 3 {
                Some(
                  MidgardVersionedScript {
                    language: PlutusV3Script,
                    script_bytes,
                  },
                )
              } else if tag == 128 {
                Some(
                  MidgardVersionedScript {
                    language: MidgardV1Script,
                    script_bytes,
                  },
                )
              } else {
                None
              }
          }
      }
    _ -> None
  }
}

fn datum_is_canonical(datum_cbor: ByteArray) -> Bool {
  when cbor.deserialise(datum_cbor) is {
    None -> False
    Some(datum) -> cbor.serialise(datum) == datum_cbor
  }
}

fn parse_required_fields(
  address_data: Data,
  value_data: Data,
  datum_cbor: Option<ByteArray>,
  script_ref: Option<MidgardVersionedScript>,
) -> Option<MidgardTxOutput> {
  when parse_address(address_data) is {
    None -> None
    Some(address) ->
      when parse_value(value_data) is {
        None -> None
        Some(value) ->
          Some(MidgardTxOutput { address, value, datum_cbor, script_ref })
      }
  }
}

fn parse_output_data(data: Data) -> Option<MidgardTxOutput> {
  when map_data(data) is {
    None -> None
    Some(entries) ->
      when entries is {
        [Pair(key_0, address_data), Pair(key_1, value_data)] ->
          if and {
            key_is(key_0, 0),
            key_is(key_1, 1),
          } {
            parse_required_fields(address_data, value_data, None, None)
          } else {
            None
          }
        [
          Pair(key_0, address_data), Pair(key_1, value_data),
          Pair(key_2, extra_data),
        ] ->
          if and {
            key_is(key_0, 0),
            key_is(key_1, 1),
          } {
            if key_is(key_2, 2) {
              when bytes_data(extra_data) is {
                Some(datum_cbor) ->
                  if datum_is_canonical(datum_cbor) {
                    parse_required_fields(
                      address_data,
                      value_data,
                      Some(datum_cbor),
                      None,
                    )
                  } else {
                    None
                  }
                None -> None
              }
            } else if key_is(key_2, 3) {
              when parse_script_ref(extra_data) is {
                Some(script_ref) ->
                  parse_required_fields(
                    address_data,
                    value_data,
                    None,
                    Some(script_ref),
                  )
                None -> None
              }
            } else {
              None
            }
          } else {
            None
          }
        [
          Pair(key_0, address_data), Pair(key_1, value_data),
          Pair(key_2, datum_data), Pair(key_3, script_ref_data),
        ] ->
          if and {
            key_is(key_0, 0),
            key_is(key_1, 1),
            key_is(key_2, 2),
            key_is(key_3, 3),
          } {
            when bytes_data(datum_data) is {
              None -> None
              Some(datum_cbor) ->
                when parse_script_ref(script_ref_data) is {
                  None -> None
                  Some(script_ref) ->
                    if datum_is_canonical(datum_cbor) {
                      parse_required_fields(
                        address_data,
                        value_data,
                        Some(datum_cbor),
                        Some(script_ref),
                      )
                    } else {
                      None
                    }
                }
            }
          } else {
            None
          }
        _ -> None
      }
  }
}

/// Total, canonical V1 decoder for ledger values supplied by an MPF
/// witness. Every malformed or unsupported representation returns `None`.
pub fn decode_canonical_output(
  output_cbor: ByteArray,
) -> Option<MidgardTxOutput> {
  if bytearray.length(output_cbor) > max_output_canonical_cbor_bytes {
    None
  } else {
    when cbor.deserialise(output_cbor) is {
      None -> None
      Some(data) ->
        when parse_output_data(data) is {
          None -> None
          Some(output) ->
            if encode_midgard_tx_output(output) == output_cbor {
              Some(output)
            } else {
              None
            }
        }
    }
  }
}
