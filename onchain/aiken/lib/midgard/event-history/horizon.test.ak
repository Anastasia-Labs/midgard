use midgard/event_history/horizon
use midgard/ledger_state.{block_maturity_duration_v1}

test eligibility_is_open_at_start_closed_at_end() {
  and {
    !horizon.eligible(10, 10, 20),
    horizon.eligible(11, 10, 20),
    horizon.eligible(20, 10, 20),
    !horizon.eligible(21, 10, 20),
    !horizon.eligible(10, 10, 10),
    !horizon.eligible(15, 20, 10),
  }
}

test evidence_must_be_captured_after_accused_interval() {
  and {
    !horizon.capture_permitted(20, 20, 19, 21),
    !horizon.capture_permitted(20, 20, 20, 21),
    horizon.capture_permitted(20, 20, 21, 22),
    !horizon.capture_permitted(20, 20, 22, 21),
  }
}

test earliest_merge_is_excluded_from_capture() {
  let merge = 20 + block_maturity_duration_v1
  and {
    horizon.capture_permitted(20, 20, merge - 2, merge - 1),
    !horizon.capture_permitted(20, 20, merge - 1, merge),
  }
}

// The coverage half alone has no merge deadline; callers that use it rely on
// the terminal's block_time bound (state_queue.require_completed_fraud_witness).
test coverage_capture_reads_only_the_header_end() {
  let merge = 20 + block_maturity_duration_v1
  and {
    !horizon.captured_after_coverage(20, 19, 21),
    !horizon.captured_after_coverage(20, 20, 21),
    horizon.captured_after_coverage(20, 21, 22),
    !horizon.captured_after_coverage(20, 22, 21),
    !horizon.captured_after_coverage(-1, 21, 22),
    horizon.captured_after_coverage(20, merge - 1, merge),
    horizon.captured_after_coverage(20, merge, merge + 1),
  }
}

// A catch-up block covers (10, 20] but was committed at 1_000: coverage stays
// on the header end, the merge clock runs from the commit time.
test catch_up_capture_runs_coverage_from_end_and_clock_from_block_time() {
  let block_time = 1_000
  let merge = block_time + block_maturity_duration_v1
  and {
    !horizon.capture_permitted(20, block_time, 20, 21),
    horizon.capture_permitted(20, block_time, 21, 22),
    horizon.capture_permitted(
      20,
      block_time,
      20 + block_maturity_duration_v1,
      20 + block_maturity_duration_v1,
    ),
    horizon.capture_permitted(20, block_time, merge - 2, merge - 1),
    !horizon.capture_permitted(20, block_time, merge - 1, merge),
  }
}

test frontier_not_payment_or_elapsed_time_controls_retirement() {
  and {
    !horizon.retirement_permitted(21, 20),
    horizon.retirement_permitted(20, 20),
    horizon.retirement_permitted(19, 20),
    !horizon.retirement_permitted(0, 20),
  }
}

test retired_event_is_ineligible_in_every_later_contiguous_interval() {
  let inclusion = 15
  let frontier = 20
  and {
    horizon.retirement_permitted(inclusion, frontier),
    !horizon.eligible(inclusion, frontier, 30),
    !horizon.eligible(inclusion, 30, 40),
  }
}

test targeted_mutation_cannot_overlap_a_protected_capture() {
  and {
    horizon.protected_capture(300, 100, 299),
    !horizon.mutation_permitted(300, 299, 301),
    horizon.mutation_permitted(300, 300, 301),
    !horizon.protected_capture(300, 299, 300),
  }
}

test replacement_renews_the_observation_opportunity() {
  let duration = 200
  let attacker_upper = 310
  let replacement_protection = attacker_upper + duration
  and {
    horizon.mutation_permitted(300, 300, attacker_upper),
    horizon.protected_capture(replacement_protection, 311, 509),
    !horizon.mutation_permitted(replacement_protection, 311, 312),
    horizon.mutation_permitted(replacement_protection, 510, 511),
  }
}
