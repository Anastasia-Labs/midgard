use aiken/builtin
use aiken/primitive/bytearray
use midgard/ledger_output_scan_v1

const address = #"6011111111111111111111111111111111111111111111111111111111"

const output_without_optional_fields =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

const non_minimal_output_map =
  #"b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

const empty_inline_datum =
  #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a00240"

const multi_asset_output =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a007a1200a1581c55555555555555555555555555555555555555555555555555555555a2420000182a41ff07"

const large_datum_prefix =
  #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a002591388"

const cross_language_terminal_control_cbor =
  #"970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820de4ab123673d8a9958247e2b709508944a6f64a4db4c7208d0a871d3358804c01854191427000319147c191481191770"

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

/// The multi-asset output with its two names in length-first order, `ff`
/// before `0000`: the second asset step refuses it.
test refuses_length_first_asset_names() {
  let output = #"a200581d601111111111111111111111111111111111111111111111111111111101821a007a1200a1581c55555555555555555555555555555555555555555555555555555555a241ff07420000182a"
  let total_length = bytearray.length(output)
  let initial = ledger_output_scan_v1.initial_control_v1()
  expect Some(value_control) = ledger_output_scan_v1.step_v1(
    initial,
    total_length,
    output,
    0,
  )
  expect Some(policy_control) = ledger_output_scan_v1.step_v1(
    value_control,
    total_length,
    output,
    value_control.cursor,
  )
  expect Some(asset_control) = ledger_output_scan_v1.step_v1(
    policy_control,
    total_length,
    output,
    policy_control.cursor,
  )
  expect Some(first_asset_control) = ledger_output_scan_v1.step_v1(
    asset_control,
    total_length,
    output,
    asset_control.cursor,
  )
  ledger_output_scan_v1.step_v1(
    first_asset_control,
    total_length,
    output,
    first_asset_control.cursor,
  ) == None
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
    ledger_output_scan_v1.encode_control_v1(control) == cross_language_terminal_control_cbor,
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

test initial_control_encoding_is_pinned() {
  ledger_output_scan_v1.encode_control_v1(
    ledger_output_scan_v1.initial_control_v1(),
  ) == ledger_output_scan_v1.initial_control_cbor_v1
}

/// The TypeScript terminal control: an output of 11,249 bytes with an inline
/// datum and a PlutusV3 reference script.
fn terminal_control() -> ledger_output_scan_v1.LedgerOutputScanControlV1 {
  ledger_output_scan_v1.decode_control_v1(cross_language_terminal_control_cbor)
}

/// `terminal_is_exact_v1` leaves to `control_is_well_formed` every condition
/// that predicate decides, so each malformed terminal control below must be
/// refused through it.
test terminal_refuses_a_control_that_is_not_well_formed() {
  let control = terminal_control()
  let refused = fn(candidate) {
    !ledger_output_scan_v1.terminal_is_exact_v1(candidate, 11249)
  }
  and {
    ledger_output_scan_v1.terminal_is_exact_v1(control, 11249),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        lovelace: -1,
      },
    ),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        asset_peaks: [],
      },
    ),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        datum_offset: -2,
      },
    ),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        reference_script_language: 2,
      },
    ),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        reference_script_item_offset: -1,
      },
    ),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        reference_script_item_offset: 5249,
      },
    ),
    refused(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..control,
        reference_script_language: -1,
      },
    ),
  }
}

/// A terminal control of an output with neither datum nor reference script
/// must still record an empty datum span.
test terminal_refuses_a_datum_length_without_a_datum() {
  let total_length = bytearray.length(output_without_optional_fields)
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
  expect Some(terminal) = ledger_output_scan_v1.finish_v1(
    optional_control,
    total_length,
  )
  and {
    ledger_output_scan_v1.terminal_is_exact_v1(terminal, total_length),
    !ledger_output_scan_v1.terminal_is_exact_v1(
      ledger_output_scan_v1.LedgerOutputScanControlV1 {
        ..terminal,
        datum_length: 1,
      },
      total_length,
    ),
  }
}

/// A control is only ever written well formed, which is what lets a thread
/// read its own control back without re-checking it.
test encoding_refuses_a_malformed_control() fail {
  let malformed = ledger_output_scan_v1.LedgerOutputScanControlV1 {
    ..ledger_output_scan_v1.initial_control_v1(),
    asset_count: 1,
  }
  ledger_output_scan_v1.encode_control_v1(malformed) != #""
}

/// A control written by `encode_control_v1` reads back unchanged without the
/// checks `decode_control_v1` repeats.
test written_control_reads_back_unchanged() {
  let control = terminal_control()
  ledger_output_scan_v1.decode_written_control_v1(
    ledger_output_scan_v1.encode_control_v1(control),
  ) == control
}
