use aiken/builtin
use aiken/primitive/bytearray
use midgard/ledger_output_scan_v1 as scan

const simple =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

const assets =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a007a1200a1581c55555555555555555555555555555555555555555555555555555555a241ff07420000182a"

const datum_prefix =
  #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a002591388"

fn parity(
  control: scan.LedgerOutputScanControlV1,
  output: ByteArray,
  fuel: Int,
) -> Bool {
  expect fuel > 0
  let total = bytearray.length(output)
  when scan.finish_v1(control, total) is {
    Some(terminal) -> scan.terminal_is_exact_v1(terminal, total)
    None -> {
      let headers = scan.step_headers(control, total, output, control.cursor)
      let assets = scan.step_assets(control, total, output, control.cursor)
      let optional = scan.step_optional(control, total, output, control.cursor)
      let selected = if control.stage <= 1 {
        expect assets == None && optional == None
        headers
      } else if control.stage <= 3 {
        expect headers == None && optional == None
        assets
      } else {
        expect headers == None && assets == None
        optional
      }
      expect Some(next) = scan.step_v1(control, total, output, control.cursor)
      expect selected == Some(next)
      parity(next, output, fuel - 1)
    }
  }
}

test headers_match_and_other_groups_refuse() {
  parity(scan.initial_control_v1(), simple, 16)
}

test policy_and_asset_steps_match_and_other_groups_refuse() {
  parity(scan.initial_control_v1(), assets, 16)
}

test datum_payload_crosses_chunk_boundaries_without_changing_scan() {
  parity(
    scan.initial_control_v1(),
    bytearray.concat(datum_prefix, builtin.replicate_byte(5000, 90)),
    16,
  )
}

test malformed_control_and_window_bounds_are_still_rejected() {
  let initial = scan.initial_control_v1()
  let malformed = scan.LedgerOutputScanControlV1 { ..initial, asset_count: 1 }
  let total = bytearray.length(simple)
  and {
    scan.step_headers(malformed, total, simple, 0) == None,
    scan.step_assets(malformed, total, simple, 0) == None,
    scan.step_optional(malformed, total, simple, 0) == None,
    scan.step_headers(initial, total, simple, -1) == None,
    scan.step_headers(initial, total, simple, total) == None,
    scan.step_headers(initial, 0, simple, 0) == None,
    scan.step_headers(initial, total, #"", 0) == None,
  }
}
