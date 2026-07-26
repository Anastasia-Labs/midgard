use aiken/builtin
use aiken/primitive/bytearray
use midgard/ledger_output_scan_v1

const address =
  #"6011111111111111111111111111111111111111111111111111111111"

const output_without_optional_fields =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

const non_minimal_output_map =
  #"b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

const empty_inline_datum =
  #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a00240"

const multi_asset_output =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a007a1200a1581c55555555555555555555555555555555555555555555555555555555a241ff07420000182a"

const large_datum_prefix =
  #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a002591388"

const cross_language_terminal_control_cbor =
  #"970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770"

test scans_required_fields_without_revealing_the_complete_output() {
  let initial = ledger_output_scan_v1.initial_control_v1()
  expect Some(value_control) = ledger_output_scan_v1.step_v1(
    initial,
    bytearray.length(output_without_optional_fields),
    output_without_optional_fields,
    0,
  )
  expect Some(optional_control) = ledger_output_scan_v1.step_v1(
    value_control,
    bytearray.length(output_without_optional_fields),
    output_without_optional_fields,
    value_control.cursor,
  )
  expect Some(terminal) = ledger_output_scan_v1.finish_v1(
    optional_control,
    bytearray.length(output_without_optional_fields),
  )
  and {
    value_control.address == address,
    optional_control.lovelace == 5000000,
    optional_control.cardano_value_size == 5,
    optional_control.asset_count == 0,
    ledger_output_scan_v1.terminal_is_exact_v1(
      terminal,
      bytearray.length(output_without_optional_fields),
    ),
  }
}

test derives_the_exact_cardano_value_size_for_multi_asset_output() {
  let total_length = bytearray.length(multi_asset_output)
  let initial = ledger_output_scan_v1.initial_control_v1()
  expect Some(value_control) = ledger_output_scan_v1.step_v1(
    initial,
    total_length,
    multi_asset_output,
    0,
  )
  expect Some(policy_control) = ledger_output_scan_v1.step_v1(
    value_control,
    total_length,
    multi_asset_output,
    value_control.cursor,
  )
  expect Some(first_asset_control) = ledger_output_scan_v1.step_v1(
    policy_control,
    total_length,
    multi_asset_output,
    policy_control.cursor,
  )
  expect Some(second_asset_control) = ledger_output_scan_v1.step_v1(
    first_asset_control,
    total_length,
    multi_asset_output,
    first_asset_control.cursor,
  )
  expect Some(optional_control) = ledger_output_scan_v1.step_v1(
    second_asset_control,
    total_length,
    multi_asset_output,
    second_asset_control.cursor,
  )
  expect Some(terminal) = ledger_output_scan_v1.finish_v1(
    optional_control,
    total_length,
  )
  and {
    optional_control.cardano_value_size == 46,
    optional_control.asset_count == 2,
    ledger_output_scan_v1.terminal_is_exact_v1(terminal, total_length),
  }
}

test advances_a_large_datum_only_to_authenticated_chunk_boundaries() {
  let output_cbor = bytearray.concat(
    large_datum_prefix,
    builtin.replicate_byte(5000, 90),
  )
  let total_length = bytearray.length(output_cbor)
  let initial = ledger_output_scan_v1.initial_control_v1()
  expect Some(value_control) = ledger_output_scan_v1.step_v1(
    initial,
    total_length,
    large_datum_prefix,
    0,
  )
  expect Some(optional_control) = ledger_output_scan_v1.step_v1(
    value_control,
    total_length,
    large_datum_prefix,
    value_control.cursor,
  )
  expect Some(datum_control) = ledger_output_scan_v1.step_v1(
    optional_control,
    total_length,
    large_datum_prefix,
    optional_control.cursor,
  )
  expect Some(second_chunk) = ledger_output_scan_v1.step_v1(
    datum_control,
    total_length,
    #"00",
    0,
  )
  expect Some(datum_complete) = ledger_output_scan_v1.step_v1(
    second_chunk,
    total_length,
    #"00",
    0,
  )
  expect Some(terminal) = ledger_output_scan_v1.finish_v1(
    datum_complete,
    total_length,
  )
  and {
    datum_control.datum_offset == 45,
    datum_control.datum_length == 5000,
    second_chunk.cursor == 4095,
    second_chunk.payload_remaining == 950,
    datum_complete.cursor == total_length,
    ledger_output_scan_v1.terminal_is_exact_v1(terminal, total_length),
  }
}

test rejects_trailing_bytes_at_the_terminal_edge() {
  let total_length = bytearray.length(output_without_optional_fields) + 1
  let initial = ledger_output_scan_v1.initial_control_v1()
  expect Some(value_control) = ledger_output_scan_v1.step_v1(
    initial,
    total_length,
    output_without_optional_fields,
    0,
  )
  expect Some(optional_control) = ledger_output_scan_v1.step_v1(
    value_control,
    total_length,
    output_without_optional_fields,
    value_control.cursor,
  )
  ledger_output_scan_v1.finish_v1(optional_control, total_length) == None
}

test decodes_and_reencodes_the_typescript_terminal_control() {
  let control = ledger_output_scan_v1.decode_control_v1(
    cross_language_terminal_control_cbor,
  )
  and {
    control.stage == ledger_output_scan_v1.stage_terminal,
    control.cursor == 11249,
    control.map_entry_count == 4,
    control.optional_field_count == 2,
    control.lovelace == 8000000,
    control.cardano_value_size == 46,
    control.asset_count == 2,
    control.datum_offset == 84,
    control.datum_length == 5159,
    control.reference_script_language == 3,
    control.reference_script_item_offset == 5244,
    control.reference_script_offset == 5249,
    control.reference_script_length == 6000,
    ledger_output_scan_v1.encode_control_v1(control)
      == cross_language_terminal_control_cbor,
  }
}

test rejects_non_minimal_cbor_and_malformed_control_state() {
  let initial = ledger_output_scan_v1.initial_control_v1()
  let malformed = ledger_output_scan_v1.LedgerOutputScanControlV1 {
    ..initial,
    asset_count: 1,
  }
  and {
    ledger_output_scan_v1.step_v1(
      initial,
      bytearray.length(non_minimal_output_map),
      non_minimal_output_map,
      0,
    ) == None,
    ledger_output_scan_v1.step_v1(
      malformed,
      bytearray.length(output_without_optional_fields),
      output_without_optional_fields,
      0,
    ) == None,
  }
}

test rejects_inline_datum_without_plutus_data_bytes() {
  let total_length = bytearray.length(empty_inline_datum)
  let initial = ledger_output_scan_v1.initial_control_v1()
  expect Some(value_control) = ledger_output_scan_v1.step_v1(
    initial,
    total_length,
    empty_inline_datum,
    0,
  )
  expect Some(optional_control) = ledger_output_scan_v1.step_v1(
    value_control,
    total_length,
    empty_inline_datum,
    value_control.cursor,
  )
  ledger_output_scan_v1.step_v1(
    optional_control,
    total_length,
    empty_inline_datum,
    optional_control.cursor,
  ) == None
}
