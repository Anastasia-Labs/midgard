use midgard/fraud_proofs/transaction_output_non_canonical/rule as output_scan
use midgard/ledger_output_scan_v1

pub fn advance(
  state: output_scan.OutputScanStateV1,
  window: ByteArray,
) -> output_scan.OutputScanStateV1 {
  advance_batch(state, window, 4)
}

fn advance_batch(
  state: output_scan.OutputScanStateV1,
  window: ByteArray,
  remaining: Int,
) -> output_scan.OutputScanStateV1 {
  let next = output_scan.advance_scan_v1(state, window)
  if and {
    remaining > 1,
    next.outcome == output_scan.outcome_scanning,
    state.control.cursor / output_scan.scan_chunk_bytes == next.control.cursor / output_scan.scan_chunk_bytes,
    state.control.stage <= ledger_output_scan_v1.stage_optional_field == (
      next.control.stage <= ledger_output_scan_v1.stage_optional_field
    ),
  } {
    advance_batch(next, window, remaining - 1)
  } else {
    next
  }
}
