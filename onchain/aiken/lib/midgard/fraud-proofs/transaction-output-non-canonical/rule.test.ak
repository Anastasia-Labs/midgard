use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/transaction_output_non_canonical/rule
use midgard/rejection_reason_v1.{FieldItemWidthIllegal, OutputNonCanonical}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const canonical_output =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

const malformed_output =
  #"b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

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

fn rejected(index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"d8799f581c000000000000000000000000000000000000000000000000000000001affffffff",
    rejection_reason: Some(OutputNonCanonical { output_index: index }),
  }
}

fn scan(
  subject: substrate.VerdictSubjectV1,
  output: ByteArray,
) -> rule.OutputScanStateV1 {
  let initial = rule.initial_scan_v1(rule.bind_output_v1(subject, 0), output)
  let first = rule.advance_scan_v1(initial, output)
  if first.outcome != rule.outcome_scanning {
    first
  } else {
    let second = rule.advance_scan_v1(first, output)
    if second.outcome != rule.outcome_scanning {
      second
    } else {
      rule.advance_scan_v1(second, output)
    }
  }
}

fn scan_until(
  state: rule.OutputScanStateV1,
  output: ByteArray,
  remaining: Int,
) -> rule.OutputScanStateV1 {
  if or {
    state.outcome != rule.outcome_scanning,
    remaining <= 0,
  } {
    state
  } else {
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

test accepted_malformed_output_convicts() {
  rule.terminal_contradiction_v1(scan(accepted(), malformed_output))
}

test forced_canonical_output_convicts() {
  rule.terminal_contradiction_v1(scan(rejected(0), canonical_output))
}

test honest_accepted_canonical_output_refuses() fail {
  rule.terminal_contradiction_v1(scan(accepted(), canonical_output))
}

test honest_forced_malformed_output_refuses() fail {
  rule.terminal_contradiction_v1(scan(rejected(0), malformed_output))
}

test exact_forced_output_coordinate_is_bound() {
  rule.bind_output_v1(rejected(7), 7).output_index == 7
}

test mutated_forced_output_coordinate_refuses() fail {
  rule.bind_output_v1(rejected(7), 6).output_index == 6
}

test unrelated_reason_refuses() fail {
  let wrong = substrate.VerdictSubjectV1 {
    ..rejected(0),
    rejection_reason: Some(
      FieldItemWidthIllegal { field_index: 2, item_index: 0 },
    ),
  }
  rule.bind_output_v1(wrong, 0).output_index == 0
}

test substituted_output_bytes_refuse_checkpoint() fail {
  let initial = rule.initial_scan_v1(
    rule.bind_output_v1(accepted(), 0),
    canonical_output,
  )
  rule.advance_scan_v1(initial, malformed_output).outcome == rule.outcome_non_canonical
}

test malformed_checkpoint_refuses() fail {
  let initial = rule.initial_scan_v1(
    rule.bind_output_v1(accepted(), 0),
    canonical_output,
  )
  let forged = rule.OutputScanStateV1 {
    ..initial,
    item_length: bytearray.length(canonical_output) + 1,
  }
  rule.advance_scan_v1(forged, canonical_output).outcome == rule.outcome_scanning
}

test adjacent_over_bound_refuses_before_scan() fail {
  let half = bytearray.from_int_big_endian(0, 8192)
  let over = bytearray.concat(half, bytearray.concat(half, #"00"))
  rule.initial_scan_v1(rule.bind_output_v1(accepted(), 0), over).output_index == 0
}

test canonical_scan_reaches_unique_terminal() {
  let terminal = scan(rejected(0), canonical_output)
  and {
    terminal.outcome == rule.outcome_canonical,
    rule.state_is_well_formed_v1(terminal),
  }
}

test maximum_output_scans_with_bounded_windows() {
  let first = bytearray.from_int_big_endian(0, 8192)
  let second = bytearray.from_int_big_endian(0, 8096)
  let payload = bytearray.concat(first, second)
  let output = bytearray.concat(
    #"a3",
    bytearray.concat(
      bytearray.slice(
        canonical_output,
        1,
        bytearray.length(canonical_output) - 1,
      ),
      bytearray.concat(#"02593fa0", payload),
    ),
  )
  let initial = rule.initial_scan_v1(
    rule.bind_output_v1(rejected(0), 0),
    output,
  )
  let terminal = scan_until(initial, output, 32)
  and {
    bytearray.length(output) <= rule.max_output_bytes,
    terminal.outcome == rule.outcome_canonical,
    rule.terminal_contradiction_v1(terminal),
  }
}
