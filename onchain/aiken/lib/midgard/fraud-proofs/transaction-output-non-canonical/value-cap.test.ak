//// `transactionOutputNonCanonical` at the Cardano value cap, both directions:
//// a 5,000-byte value is canonical and a 5,001-byte one is not, each spelled
//// with long asset names across the 4,095-byte chunk boundary.

use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/transaction_output_non_canonical/rule
use midgard/ledger_output_value_size_fixture_v1 as fixture
use midgard/rejection_reason_v1.{OutputNonCanonical}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"d8799f581c000000000000000000000000000000000000000000000000000000001affffffff",
    rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
  }
}

/// Advances the scan to its outcome, each step over the chunk holding the
/// cursor and, at a token stage, the chunk after it.
fn scan_until(
  state: rule.OutputScanStateV1,
  output: ByteArray,
  remaining: Int,
) -> rule.OutputScanStateV1 {
  if state.outcome != rule.outcome_scanning {
    state
  } else {
    expect remaining > 0
    let chunk_start = state.control.cursor - state.control.cursor % rule.scan_chunk_bytes
    let maximum = if state.control.stage <= 4 {
      rule.scan_window_bytes
    } else {
      rule.scan_chunk_bytes
    }
    let length = if bytearray.length(output) - chunk_start < maximum {
      bytearray.length(output) - chunk_start
    } else {
      maximum
    }
    let window = bytearray.slice(output, chunk_start, chunk_start + length - 1)
    scan_until(rule.advance_scan_v1(state, window), output, remaining - 1)
  }
}

fn scan(
  subject: substrate.VerdictSubjectV1,
  tail_quantity: Int,
) -> rule.OutputScanStateV1 {
  let output = fixture.long_name_output(tail_quantity)
  scan_until(
    rule.initial_scan_v1(rule.bind_output_v1(subject, 0), output),
    output,
    256,
  )
}

test accepted_output_value_of_5000_bytes_is_canonical() {
  let terminal = scan(accepted(), fixture.quantity_at_cap)
  and {
    terminal.outcome == rule.outcome_canonical,
    terminal.control.cardano_value_size == 5000,
  }
}

test honest_acceptance_of_a_5000_byte_value_is_not_convicted() {
  !rule.terminal_contradiction_v1(scan(accepted(), fixture.quantity_at_cap))
}

test wrongful_acceptance_of_a_5001_byte_value_is_convicted() {
  let terminal = scan(accepted(), fixture.quantity_over_cap)
  and {
    terminal.outcome == rule.outcome_non_canonical,
    rule.terminal_contradiction_v1(terminal),
  }
}

test wrongful_rejection_of_a_5000_byte_value_is_convicted() {
  let terminal = scan(rejected(), fixture.quantity_at_cap)
  and {
    terminal.outcome == rule.outcome_canonical,
    rule.terminal_contradiction_v1(terminal),
  }
}

test honest_rejection_of_a_5001_byte_value_is_not_convicted() {
  let terminal = scan(rejected(), fixture.quantity_over_cap)
  and {
    terminal.outcome == rule.outcome_non_canonical,
    !rule.terminal_contradiction_v1(terminal),
  }
}
