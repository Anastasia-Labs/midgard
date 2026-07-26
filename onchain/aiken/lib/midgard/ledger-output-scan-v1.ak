use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const stage_required_fields: Int = 0

pub const stage_value_header: Int = 1

pub const stage_policy_header: Int = 2

pub const stage_asset: Int = 3

pub const stage_optional_field: Int = 4

pub const stage_datum_payload: Int = 5

pub const stage_reference_script_payload: Int = 6

pub const stage_terminal: Int = 7

pub type LedgerOutputScanControlV1 {
  version: Int,
  stage: Int,
  cursor: Int,
  map_entry_count: Int,
  optional_field_count: Int,
  address: ByteArray,
  lovelace: Int,
  cardano_value_size: Int,
  policy_remaining: Int,
  asset_remaining: Int,
  policy_asset_cursor: Int,
  previous_policy: ByteArray,
  current_policy: ByteArray,
  previous_asset_name: ByteArray,
  asset_count: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
  datum_offset: Int,
  datum_length: Int,
  payload_remaining: Int,
  reference_script_language: Int,
  reference_script_offset: Int,
  reference_script_length: Int,
}

pub fn initial_control_v1() -> LedgerOutputScanControlV1 {
  LedgerOutputScanControlV1 {
    version,
    stage: stage_required_fields,
    cursor: 0,
    map_entry_count: 0,
    optional_field_count: 0,
    address: #"",
    lovelace: 0,
    cardano_value_size: 0,
    policy_remaining: 0,
    asset_remaining: 0,
    policy_asset_cursor: 0,
    previous_policy: #"",
    current_policy: #"",
    previous_asset_name: #"",
    asset_count: 0,
    asset_peaks: validation_merkle_v1.empty_frontier(),
    datum_offset: -1,
    datum_length: 0,
    payload_remaining: 0,
    reference_script_language: -1,
    reference_script_offset: -1,
    reference_script_length: 0,
  }
}

fn decode_peak_items(
  items: List<Data>,
) -> List<validation_merkle_v1.FrontierPeak> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect [height_data, hash_data] = builtin.un_list_data(item)
      [
        validation_merkle_v1.FrontierPeak {
          height: builtin.un_i_data(height_data),
          hash: builtin.un_b_data(hash_data),
        },
        ..decode_peak_items(rest)
      ]
    }
  }
}

pub fn control_is_well_formed(
  control: LedgerOutputScanControlV1,
) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_required_fields,
    control.stage <= stage_terminal,
    control.cursor >= 0,
    or {
      control.map_entry_count == 0,
      control.map_entry_count >= 2 && control.map_entry_count <= 4,
    },
    control.optional_field_count >= 0,
    control.optional_field_count <= 2,
    or {
      control.address == #"",
      ledger_output_v1.decode_canonical_address_bytes(control.address) != None,
    },
    control.lovelace >= 0,
    control.cardano_value_size >= 0,
    control.policy_remaining >= 0,
    control.asset_remaining >= 0,
    control.policy_asset_cursor >= 0,
    or {
      control.previous_policy == #"",
      bytearray.length(control.previous_policy) == 28,
    },
    or {
      control.current_policy == #"",
      bytearray.length(control.current_policy) == 28,
    },
    bytearray.length(control.previous_asset_name) <= 32,
    validation_merkle_v1.frontier_is_well_formed(
      control.asset_count,
      control.asset_peaks,
    ),
    control.datum_offset >= -1,
    control.datum_length >= 0,
    if control.datum_offset == -1 {
      control.datum_length == 0
    } else {
      True
    },
    control.payload_remaining >= 0,
    or {
      control.reference_script_language == -1,
      control.reference_script_language == 0,
      control.reference_script_language == 3,
      control.reference_script_language == 128,
    },
    control.reference_script_offset >= -1,
    control.reference_script_length >= 0,
    if control.reference_script_language == -1 {
      and {
        control.reference_script_offset == -1,
        control.reference_script_length == 0,
      }
    } else {
      control.reference_script_offset >= 0
    },
  }
}

pub fn encode_control_v1(
  control: LedgerOutputScanControlV1,
) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(22)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.cursor))
    |> bytearray.concat(cbor.serialise(control.map_entry_count))
    |> bytearray.concat(cbor.serialise(control.optional_field_count))
    |> bytearray.concat(encode_definite_bytes(control.address))
    |> bytearray.concat(cbor.serialise(control.lovelace))
    |> bytearray.concat(cbor.serialise(control.cardano_value_size))
    |> bytearray.concat(cbor.serialise(control.policy_remaining))
    |> bytearray.concat(cbor.serialise(control.asset_remaining))
    |> bytearray.concat(cbor.serialise(control.policy_asset_cursor))
    |> bytearray.concat(encode_definite_bytes(control.previous_policy))
    |> bytearray.concat(encode_definite_bytes(control.current_policy))
    |> bytearray.concat(encode_definite_bytes(control.previous_asset_name))
    |> bytearray.concat(cbor.serialise(control.asset_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.asset_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.datum_offset))
    |> bytearray.concat(cbor.serialise(control.datum_length))
    |> bytearray.concat(cbor.serialise(control.payload_remaining))
    |> bytearray.concat(cbor.serialise(control.reference_script_language))
    |> bytearray.concat(cbor.serialise(control.reference_script_offset))
    |> bytearray.concat(cbor.serialise(control.reference_script_length))
}

pub fn control_from_data_v1(
  data: Data,
) -> LedgerOutputScanControlV1 {
  expect
    [
      version_data, stage_data, cursor_data, map_entry_count_data,
      optional_field_count_data, address_data, lovelace_data,
      cardano_value_size_data, policy_remaining_data, asset_remaining_data,
      policy_asset_cursor_data, previous_policy_data, current_policy_data,
      previous_asset_name_data, asset_count_data, asset_peaks_data,
      datum_offset_data, datum_length_data, payload_remaining_data,
      reference_script_language_data, reference_script_offset_data,
      reference_script_length_data,
    ] = builtin.un_list_data(data)
  let control = LedgerOutputScanControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    cursor: builtin.un_i_data(cursor_data),
    map_entry_count: builtin.un_i_data(map_entry_count_data),
    optional_field_count: builtin.un_i_data(optional_field_count_data),
    address: builtin.un_b_data(address_data),
    lovelace: builtin.un_i_data(lovelace_data),
    cardano_value_size: builtin.un_i_data(cardano_value_size_data),
    policy_remaining: builtin.un_i_data(policy_remaining_data),
    asset_remaining: builtin.un_i_data(asset_remaining_data),
    policy_asset_cursor: builtin.un_i_data(policy_asset_cursor_data),
    previous_policy: builtin.un_b_data(previous_policy_data),
    current_policy: builtin.un_b_data(current_policy_data),
    previous_asset_name: builtin.un_b_data(previous_asset_name_data),
    asset_count: builtin.un_i_data(asset_count_data),
    asset_peaks: asset_peaks_data
      |> builtin.un_list_data
      |> decode_peak_items,
    datum_offset: builtin.un_i_data(datum_offset_data),
    datum_length: builtin.un_i_data(datum_length_data),
    payload_remaining: builtin.un_i_data(payload_remaining_data),
    reference_script_language: builtin.un_i_data(
      reference_script_language_data,
    ),
    reference_script_offset: builtin.un_i_data(
      reference_script_offset_data,
    ),
    reference_script_length: builtin.un_i_data(
      reference_script_length_data,
    ),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(
  control_cbor: ByteArray,
) -> LedgerOutputScanControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

fn absolute_offset(
  control: LedgerOutputScanControlV1,
  window_offset: Int,
  local_offset: Int,
) -> Int {
  control.cursor + local_offset - window_offset
}

fn canonical_key_at(
  window: ByteArray,
  offset: Int,
  expected: Int,
) -> Option<Int> {
  when canonical_cbor_scan_v1.head_at_v1(window, offset, 0) is {
    Some(head) ->
      if head.value == expected {
        Some(head.next_offset)
      } else {
        None
      }
    None -> None
  }
}

fn major_head_length(value: Int) -> Int {
  expect value >= 0
  if value < 24 {
    1
  } else if value <= 255 {
    2
  } else if value <= 65535 {
    3
  } else if value <= 4294967295 {
    5
  } else {
    9
  }
}

fn step_required_fields(
  control: LedgerOutputScanControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  when canonical_cbor_scan_v1.head_at_v1(window, window_offset, 5) is {
    None -> None
    Some(output_map) ->
      if output_map.value < 2 || output_map.value > 4 {
        None
      } else {
        when canonical_key_at(window, output_map.next_offset, 0) is {
          None -> None
          Some(address_offset) ->
            when
              canonical_cbor_scan_v1.bytes_at_v1(window, address_offset)
            is {
              None -> None
              Some(address) ->
                when
                  ledger_output_v1.decode_canonical_address_bytes(
                    address.value,
                  )
                is {
                  None -> None
                  Some(_) ->
                    Some(
                      LedgerOutputScanControlV1 {
                        ..control,
                        stage: stage_value_header,
                        cursor: absolute_offset(
                          control,
                          window_offset,
                          address.next_offset,
                        ),
                        map_entry_count: output_map.value,
                        address: address.value,
                      },
                    )
                }
            }
        }
      }
  }
}

fn step_value_header(
  control: LedgerOutputScanControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  when canonical_key_at(window, window_offset, 1) is {
    None -> None
    Some(value_offset) ->
      when canonical_cbor_scan_v1.head_at_v1(window, value_offset, 4) is {
        None -> None
        Some(value_array) ->
          if value_array.value != 2 {
            None
          } else {
            when
              canonical_cbor_scan_v1.head_at_v1(
                window,
                value_array.next_offset,
                0,
              )
            is {
              None -> None
              Some(lovelace) ->
                when
                  canonical_cbor_scan_v1.head_at_v1(
                    window,
                    lovelace.next_offset,
                    5,
                  )
                is {
                  None -> None
                  Some(policies) ->
                    if policies.value > validation_merkle_v1.maximum_leaf_count {
                      None
                    } else {
                      Some(
                        LedgerOutputScanControlV1 {
                          ..control,
                          stage: if policies.value == 0 {
                            stage_optional_field
                          } else {
                            stage_policy_header
                          },
                          cursor: absolute_offset(
                            control,
                            window_offset,
                            policies.next_offset,
                          ),
                          lovelace: lovelace.value,
                          cardano_value_size: if policies.value == 0 {
                            bytearray.length(cbor.serialise(lovelace.value))
                          } else {
                            1 + bytearray.length(cbor.serialise(lovelace.value)) + major_head_length(
                              policies.value,
                            )
                          },
                          policy_remaining: policies.value,
                        },
                      )
                    }
                }
            }
          }
      }
  }
}

fn step_policy_header(
  control: LedgerOutputScanControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  when canonical_cbor_scan_v1.bytes_at_v1(window, window_offset) is {
    None -> None
    Some(policy) ->
      when
        canonical_cbor_scan_v1.head_at_v1(window, policy.next_offset, 5)
      is {
        None -> None
        Some(assets) -> {
          let policy_ordered = if control.previous_policy == #"" {
            True
          } else {
            bytearray.compare(control.previous_policy, policy.value) == Less
          }
          if and {
            bytearray.length(policy.value) == 28,
            policy_ordered,
            assets.value > 0,
            assets.value <= validation_merkle_v1.maximum_leaf_count,
            control.policy_remaining > 0,
          } {
            Some(
              LedgerOutputScanControlV1 {
                ..control,
                stage: stage_asset,
                cursor: absolute_offset(
                  control,
                  window_offset,
                  assets.next_offset,
                ),
                asset_remaining: assets.value,
                policy_asset_cursor: 0,
                current_policy: policy.value,
                previous_asset_name: #"",
                cardano_value_size: control.cardano_value_size + bytearray.length(
                  encode_definite_bytes(policy.value),
                ) + major_head_length(assets.value),
              },
            )
          } else {
            None
          }
        }
      }
  }
}

fn step_asset(
  control: LedgerOutputScanControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  when canonical_cbor_scan_v1.bytes_at_v1(window, window_offset) is {
    None -> None
    Some(asset_name) ->
      when
        canonical_cbor_scan_v1.head_at_v1(
          window,
          asset_name.next_offset,
          0,
        )
      is {
        None -> None
        Some(quantity) -> {
          let name_ordered = if control.policy_asset_cursor == 0 {
            True
          } else {
            let previous_length = bytearray.length(
              control.previous_asset_name,
            )
            let current_length = bytearray.length(asset_name.value)
            if previous_length < current_length {
              True
            } else if previous_length > current_length {
              False
            } else {
              bytearray.compare(
                control.previous_asset_name,
                asset_name.value,
              ) == Less
            }
          }
          if and {
            bytearray.length(control.current_policy) == 28,
            bytearray.length(asset_name.value) <= 32,
            name_ordered,
            quantity.value > 0,
            control.asset_remaining > 0,
            control.asset_count < validation_merkle_v1.maximum_leaf_count,
          } {
            let next_asset_remaining = control.asset_remaining - 1
            let policy_complete = next_asset_remaining == 0
            let next_policy_remaining = control.policy_remaining - if policy_complete {
              1
            } else {
              0
            }
            Some(
              LedgerOutputScanControlV1 {
                ..control,
                stage: if policy_complete {
                  if next_policy_remaining == 0 {
                    stage_optional_field
                  } else {
                    stage_policy_header
                  }
                } else {
                  stage_asset
                },
                cursor: absolute_offset(
                  control,
                  window_offset,
                  quantity.next_offset,
                ),
                policy_remaining: next_policy_remaining,
                asset_remaining: next_asset_remaining,
                policy_asset_cursor: if policy_complete {
                  0
                } else {
                  control.policy_asset_cursor + 1
                },
                previous_policy: if policy_complete {
                  control.current_policy
                } else {
                  control.previous_policy
                },
                current_policy: if policy_complete {
                  #""
                } else {
                  control.current_policy
                },
                previous_asset_name: if policy_complete {
                  #""
                } else {
                  asset_name.value
                },
                asset_count: control.asset_count + 1,
                asset_peaks: validation_merkle_v1.append_leaf(
                  control.asset_count,
                  control.asset_peaks,
                  ledger_output_commitment_v1.asset_leaf_hash(
                    control.current_policy,
                    asset_name.value,
                    quantity.value,
                  ),
                ),
                cardano_value_size: control.cardano_value_size + bytearray.length(
                  encode_definite_bytes(asset_name.value),
                ) + bytearray.length(cbor.serialise(quantity.value)),
              },
            )
          } else {
            None
          }
        }
      }
  }
}

fn optional_fields_are_complete(
  control: LedgerOutputScanControlV1,
) -> Bool {
  control.optional_field_count + 2 == control.map_entry_count
}

fn reference_script_header(
  control: LedgerOutputScanControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  when canonical_key_at(window, window_offset, 3) is {
    None -> None
    Some(script_offset) ->
      when canonical_cbor_scan_v1.head_at_v1(window, script_offset, 4) is {
        None -> None
        Some(script_array) ->
          if script_array.value != 2 {
            None
          } else {
            when
              canonical_cbor_scan_v1.head_at_v1(
                window,
                script_array.next_offset,
                0,
              )
            is {
              None -> None
              Some(language) ->
                when
                  canonical_cbor_scan_v1.head_at_v1(
                    window,
                    language.next_offset,
                    2,
                  )
                is {
                  None -> None
                  Some(payload) ->
                    if or {
                      language.value == 0,
                      language.value == 3,
                      language.value == 128,
                    } {
                      Some(
                        LedgerOutputScanControlV1 {
                          ..control,
                          stage: if payload.value == 0 {
                            stage_terminal
                          } else {
                            stage_reference_script_payload
                          },
                          cursor: absolute_offset(
                            control,
                            window_offset,
                            payload.next_offset,
                          ),
                          optional_field_count: control.optional_field_count + 1,
                          payload_remaining: payload.value,
                          reference_script_language: language.value,
                          reference_script_offset: absolute_offset(
                            control,
                            window_offset,
                            payload.next_offset,
                          ),
                          reference_script_length: payload.value,
                        },
                      )
                    } else {
                      None
                    }
                }
            }
          }
      }
  }
}

fn step_optional_field(
  control: LedgerOutputScanControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  if optional_fields_are_complete(control) {
    Some(LedgerOutputScanControlV1 { ..control, stage: stage_terminal })
  } else if and {
    control.map_entry_count == 4,
    control.optional_field_count == 0,
  } {
    when canonical_key_at(window, window_offset, 2) is {
      None -> None
      Some(datum_offset) ->
        when
          canonical_cbor_scan_v1.head_at_v1(window, datum_offset, 2)
        is {
          None -> None
          Some(datum) ->
            Some(
              LedgerOutputScanControlV1 {
                ..control,
                stage: if datum.value == 0 {
                  stage_optional_field
                } else {
                  stage_datum_payload
                },
                cursor: absolute_offset(
                  control,
                  window_offset,
                  datum.next_offset,
                ),
                optional_field_count: 1,
                datum_offset: absolute_offset(
                  control,
                  window_offset,
                  datum.next_offset,
                ),
                datum_length: datum.value,
                payload_remaining: datum.value,
              },
            )
        }
    }
  } else {
    when canonical_cbor_scan_v1.head_at_v1(window, window_offset, 0) is {
      None -> None
      Some(key) ->
        if key.value == 2 {
          when
            canonical_cbor_scan_v1.head_at_v1(window, key.next_offset, 2)
          is {
            None -> None
            Some(datum) ->
              Some(
                LedgerOutputScanControlV1 {
                  ..control,
                  stage: if datum.value == 0 {
                    stage_terminal
                  } else {
                    stage_datum_payload
                  },
                  cursor: absolute_offset(
                    control,
                    window_offset,
                    datum.next_offset,
                  ),
                  optional_field_count: control.optional_field_count + 1,
                  datum_offset: absolute_offset(
                    control,
                    window_offset,
                    datum.next_offset,
                  ),
                  datum_length: datum.value,
                  payload_remaining: datum.value,
                },
              )
          }
        } else if key.value == 3 {
          reference_script_header(control, window, window_offset)
        } else {
          None
        }
    }
  }
}

fn step_payload(
  control: LedgerOutputScanControlV1,
  total_length: Int,
) -> Option<LedgerOutputScanControlV1> {
  let chunk_remaining = 4095 - control.cursor % 4095
  let output_remaining = total_length - control.cursor
  let consumed = if control.payload_remaining < chunk_remaining {
    control.payload_remaining
  } else {
    chunk_remaining
  }
  if or {
    control.payload_remaining <= 0,
    consumed <= 0,
    consumed > output_remaining,
  } {
    None
  } else {
    let next_remaining = control.payload_remaining - consumed
    Some(
      LedgerOutputScanControlV1 {
        ..control,
        stage: if next_remaining == 0 {
          stage_optional_field
        } else {
          control.stage
        },
        cursor: control.cursor + consumed,
        payload_remaining: next_remaining,
      },
    )
  }
}

/// Advances one deterministic logical token or one payload segment. The
/// caller authenticates the current 4,095-byte output chunk and, for bounded
/// tokens crossing a chunk boundary, the immediately following chunk.
pub fn step_v1(
  control: LedgerOutputScanControlV1,
  total_length: Int,
  window: ByteArray,
  window_offset: Int,
) -> Option<LedgerOutputScanControlV1> {
  if and {
    control_is_well_formed(control),
    control.cursor >= 0,
    control.cursor <= total_length,
    window_offset >= 0,
    window_offset < bytearray.length(window),
  } {
    let result = if control.stage == stage_required_fields {
      step_required_fields(control, window, window_offset)
    } else if control.stage == stage_value_header {
      step_value_header(control, window, window_offset)
    } else if control.stage == stage_policy_header {
      step_policy_header(control, window, window_offset)
    } else if control.stage == stage_asset {
      step_asset(control, window, window_offset)
    } else if control.stage == stage_optional_field {
      step_optional_field(control, window, window_offset)
    } else if or {
      control.stage == stage_datum_payload,
      control.stage == stage_reference_script_payload,
    } {
      step_payload(control, total_length)
    } else {
      None
    }
    when result is {
      None -> None
      Some(next) ->
        if and {
          control_is_well_formed(next),
          next.cursor >= control.cursor,
          next.cursor <= total_length,
          or {
            next.cursor > control.cursor,
            next.stage != control.stage,
          },
        } {
          Some(next)
        } else {
          None
        }
    }
  } else {
    None
  }
}

/// Completes the zero-byte terminal edge after the last required Value item
/// or datum/reference-script payload. This edge needs no output preimage.
pub fn finish_v1(
  control: LedgerOutputScanControlV1,
  total_length: Int,
) -> Option<LedgerOutputScanControlV1> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_optional_field,
    optional_fields_are_complete(control),
    control.cursor == total_length,
    control.payload_remaining == 0,
  } {
    Some(LedgerOutputScanControlV1 { ..control, stage: stage_terminal })
  } else {
    None
  }
}

pub fn terminal_is_exact_v1(
  control: LedgerOutputScanControlV1,
  total_length: Int,
) -> Bool {
  and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
    control.cursor == total_length,
    control.map_entry_count >= 2,
    control.map_entry_count <= 4,
    optional_fields_are_complete(control),
    bytearray.length(control.address) == 29 || bytearray.length(
      control.address,
    ) == 57,
    control.lovelace >= 0,
    control.cardano_value_size > 0,
    control.policy_remaining == 0,
    control.asset_remaining == 0,
    control.policy_asset_cursor == 0,
    control.current_policy == #"",
    control.previous_asset_name == #"",
    validation_merkle_v1.frontier_is_well_formed(
      control.asset_count,
      control.asset_peaks,
    ),
    control.payload_remaining == 0,
    if control.datum_offset == -1 {
      control.datum_length == 0
    } else {
      and {
        control.datum_offset >= 0,
        control.datum_length >= 0,
        control.datum_offset + control.datum_length <= total_length,
      }
    },
    if control.reference_script_language == -1 {
      and {
        control.reference_script_offset == -1,
        control.reference_script_length == 0,
      }
    } else {
      and {
        or {
          control.reference_script_language == 0,
          control.reference_script_language == 3,
          control.reference_script_language == 128,
        },
        control.reference_script_offset >= 0,
        control.reference_script_length >= 0,
        control.reference_script_offset + control.reference_script_length == total_length,
      }
    },
  }
}
