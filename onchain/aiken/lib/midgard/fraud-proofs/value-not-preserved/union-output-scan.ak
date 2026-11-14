use midgard/ledger_output_scan_v1.{LedgerOutputScanControlV1} as scan
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}

/// Every logical scan step reads at most one bounded token window. Payload
/// stages skip their authenticated extent exactly as the canonical scanner.
pub fn advance(
  view: FieldViewV1,
  item_offset: Int,
  item_length: Int,
  control: LedgerOutputScanControlV1,
  budget: Int,
) -> LedgerOutputScanControlV1 {
  if budget == 0 || scan.terminal_is_exact_v1(control, item_length) {
    control
  } else {
    when scan.finish_v1(control, item_length) is {
      Some(complete) -> complete
      None -> {
        let remaining = item_length - control.cursor
        expect remaining > 0
        let size = if remaining < 132 {
          remaining
        } else {
          132
        }
        let window = field_read_range(view, item_offset + control.cursor, size)
        expect Some(next) = scan.step_v1(control, item_length, window, 0)
        advance(view, item_offset, item_length, next, budget - 1)
      }
    }
  }
}
