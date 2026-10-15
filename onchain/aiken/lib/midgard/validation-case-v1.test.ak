use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{nibble}
use aiken/primitive/bytearray
use cardano/address.{from_verification_key}
use cardano/transaction.{OutputReference}
use env
use midgard/ledger_state
use midgard/validation_case_v1.{
  CaseThreadV1, CaseV1, Convicted, DefenderWon, LostDefenseReceiptV1,
  NoValidClaim,
}
use midgard/validation_dispute_v1.{
  AwaitingOperator, ChallengerWins, DefenderWins, NeitherClaimValid,
  ReadyForOneStep,
}
use midgard/validation_trace_v1.{ValidationMachineStateV1, ValidationTraceProof}

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const header_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const header_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const vkh_1 = #"11111111111111111111111111111111111111111111111111111111"

const vkh_2 = #"22222222222222222222222222222222222222222222222222222222"

const vkh_prover = #"33333333333333333333333333333333333333333333333333333333"

const event_a = ledger_state.L2TransactionEventKey { tx_id: h_a }

const event_b = ledger_state.L2TransactionEventKey { tx_id: h_b }

const opener = OutputReference { transaction_id: h_c, output_index: 0 }

const started = 1_000_000

const clock = validation_dispute_v1.dispute_side_clock_milliseconds

fn case_with(step_count: Int, challenger_terminal_hash: ByteArray) -> CaseV1 {
  validation_case_v1.open_case(
    header_a,
    event_a,
    h_b,
    step_count,
    challenger_terminal_hash,
    vkh_prover,
    h_c,
    started,
  )
}

fn state(
  phase: validation_trace_v1.ValidationPhase,
  verdict: validation_trace_v1.ValidationVerdict,
) -> ValidationMachineStateV1 {
  ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: zero,
    transaction_id: zero,
    transaction_commitment: zero,
    validation_context_hash: zero,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: zero,
    phase,
    program_counter: 0,
    work_root: zero,
    execution_cpu: 0,
    execution_memory: 0,
    verdict,
    rejection_code_hash: if verdict == validation_trace_v1.Rejected {
      h_d
    } else {
      zero
    },
    ledger_delta_root: zero,
  }
}

fn initial_hash() -> ByteArray {
  validation_trace_v1.hash_machine_state(
    state(validation_trace_v1.CanonicalDecode, validation_trace_v1.Pending),
  )
}

/// A one-step trace `[initial, last]`: its root and the proof of leaf 1.
fn one_step_trace(
  last: ValidationMachineStateV1,
) -> (ByteArray, ValidationTraceProof) {
  let last_hash = validation_trace_v1.hash_machine_state(last)
  let sibling = validation_trace_v1.trace_leaf_hash(initial_hash())
  (
    validation_trace_v1.trace_branch_hash(
      sibling,
      validation_trace_v1.trace_leaf_hash(last_hash),
    ),
    ValidationTraceProof {
      state_index: 1,
      state_hash: last_hash,
      siblings: [sibling],
    },
  )
}

/// A trace of depth two whose leaves 0 and 1 are `[initial, state]`, with an
/// arbitrary right subtree: its root and the proof of leaf 1.
fn two_level_trace(
  state: ValidationMachineStateV1,
) -> (ByteArray, ValidationTraceProof) {
  let state_hash = validation_trace_v1.hash_machine_state(state)
  let sibling = validation_trace_v1.trace_leaf_hash(initial_hash())
  (
    validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_branch_hash(
        sibling,
        validation_trace_v1.trace_leaf_hash(state_hash),
      ),
      h_a,
    ),
    ValidationTraceProof {
      state_index: 1,
      state_hash,
      siblings: [sibling, h_a],
    },
  )
}

fn defense_holds(last: ValidationMachineStateV1, case: CaseV1) -> Bool {
  let (root, proof) = one_step_trace(last)
  validation_case_v1.defense_is_valid(case, root, last, proof)
}

fn rejected_terminal() -> ValidationMachineStateV1 {
  state(validation_trace_v1.Terminal, validation_trace_v1.Rejected)
}

fn accepted_terminal() -> ValidationMachineStateV1 {
  state(validation_trace_v1.Terminal, validation_trace_v1.Accepted)
}

fn pending_mid_trace() -> ValidationMachineStateV1 {
  state(validation_trace_v1.Cek, validation_trace_v1.Pending)
}

fn common_nibbles(a: ByteArray, b: ByteArray, cursor: Int) -> Int {
  if nibble(a, cursor) == nibble(b, cursor) {
    common_nibbles(a, b, cursor + 1)
  } else {
    cursor
  }
}

/// The non-membership proof of `vkh` in a trie holding only `member`.
fn beside(member: ByteArray, vkh: ByteArray) -> mpf.Proof {
  let member_path = blake2b_256(member)
  [
    mpf.Leaf {
      skip: common_nibbles(member_path, blake2b_256(vkh), 0),
      key: member_path,
      value: blake2b_256(member),
    },
  ]
}

fn defended_with(case: CaseV1, vkh: ByteArray, proof: mpf.Proof) -> CaseV1 {
  expect Some(next) = validation_case_v1.defend(case, vkh, proof)
  next
}

fn defended(case: CaseV1, vkh: ByteArray) -> CaseV1 {
  defended_with(case, vkh, [])
}

/// A case defended by `vkh_1`, then by `vkh_2`.
fn defended_twice(case: CaseV1) -> CaseV1 {
  defended_with(defended(case, vkh_1), vkh_2, beside(vkh_1, vkh_2))
}

fn thread_of(case: CaseV1) -> CaseThreadV1 {
  validation_case_v1.open_thread(case, h_d, vkh_1, h_a, h_b, started, started)
}

// ## Token names

test case_nft_name_binds_header_event_and_opener() {
  let name = validation_case_v1.case_nft_name(header_a, event_a, opener)
  and {
    bytearray.length(name) == 32,
    name == validation_case_v1.case_nft_name(header_a, event_a, opener),
    name != validation_case_v1.case_nft_name(header_b, event_a, opener),
    name != validation_case_v1.case_nft_name(header_a, event_b, opener),
    name != validation_case_v1.case_nft_name(
      header_a,
      event_a,
      OutputReference { transaction_id: h_c, output_index: 1 },
    ),
  }
}

test case_nft_name_refuses_a_block_hash_length_header_hash() fail {
  bytearray.length(validation_case_v1.case_nft_name(h_a, event_a, opener)) == 32
}

test case_nft_name_refuses_a_short_header_hash() fail {
  bytearray.length(validation_case_v1.case_nft_name(#"aa", event_a, opener)) == 32
}

test thread_token_name_is_per_case_and_operator() {
  let name = validation_case_v1.thread_token_name(h_a, vkh_1)
  and {
    bytearray.length(name) == 32,
    name != validation_case_v1.thread_token_name(h_a, vkh_2),
    name != validation_case_v1.thread_token_name(h_b, vkh_1),
    name == validation_case_v1.receipt_token_name(h_a, vkh_1),
  }
}

test thread_token_name_refuses_a_short_key_hash() fail {
  bytearray.length(validation_case_v1.thread_token_name(h_a, #"11")) == 32
}

// ## VerifySource

test open_case_starts_an_undefended_case() {
  let case = case_with(4, h_d)
  and {
    case.version == validation_case_v1.case_version,
    case.block_header_hash == header_a,
    case.event_key == event_a,
    case.challenger_root == h_b,
    case.step_count == 4,
    case.challenger_terminal_hash == h_d,
    case.bounty_claimant == vkh_prover,
    case.agreed_initial_hash == h_c,
    case.started_at == started,
    case.open_threads == 0,
    case.defenders_root == env.empty_merkle_tree_root,
    !case.defender_won,
    !case.neither_claim_valid,
  }
}

test open_case_refuses_a_zero_step_count() fail {
  case_with(0, h_d).open_threads == 0
}

test open_case_refuses_a_step_count_over_the_machine_limit() fail {
  case_with(validation_trace_v1.max_machine_step_count + 1, h_d).open_threads == 0
}

test open_case_accepts_the_machine_limit() {
  case_with(validation_trace_v1.max_machine_step_count, h_d).open_threads == 0
}

test open_case_refuses_a_short_terminal_hash() fail {
  case_with(4, #"dd").open_threads == 0
}

// ## Defend

test defense_with_a_pending_state_at_s_is_valid() {
  defense_holds(pending_mid_trace(), case_with(1, h_d))
}

test defense_with_an_accepted_terminal_is_valid() {
  defense_holds(accepted_terminal(), case_with(1, h_d))
}

test defense_with_a_rejected_terminal_is_refused() {
  !defense_holds(rejected_terminal(), case_with(1, h_d))
}

test defense_equal_to_the_challenger_terminal_is_refused() {
  let last = pending_mid_trace()
  !defense_holds(
    last,
    case_with(1, validation_trace_v1.hash_machine_state(last)),
  )
}

test defense_proven_at_another_index_is_refused() {
  let last = pending_mid_trace()
  let (root, proof) = one_step_trace(last)
  !validation_case_v1.defense_is_valid(case_with(2, h_d), root, last, proof)
}

test defense_proven_before_the_last_step_is_refused() {
  let mid = pending_mid_trace()
  let (root, proof) = two_level_trace(mid)
  and {
    validation_trace_v1.verify_trace_proof_against_root(root, 2, proof),
    !validation_case_v1.defense_is_valid(case_with(2, h_d), root, mid, proof),
  }
}

test defense_under_another_root_is_refused() {
  let last = pending_mid_trace()
  let (_, proof) = one_step_trace(last)
  !validation_case_v1.defense_is_valid(case_with(1, h_d), h_a, last, proof)
}

test defense_with_a_state_other_than_the_proven_leaf_is_refused() {
  let (root, proof) = one_step_trace(pending_mid_trace())
  !validation_case_v1.defense_is_valid(
    case_with(1, h_d),
    root,
    accepted_terminal(),
    proof,
  )
}

test trace_proof_against_root_refuses_a_wrong_depth() {
  let (root, proof) = one_step_trace(pending_mid_trace())
  !validation_trace_v1.verify_trace_proof_against_root(
    root,
    1,
    ValidationTraceProof { ..proof, siblings: [h_a, ..proof.siblings] },
  )
}

test trace_proof_against_root_is_bound_to_the_step_count_depth() {
  let (root, proof) = two_level_trace(pending_mid_trace())
  and {
    validation_trace_v1.verify_trace_proof_against_root(root, 2, proof),
    !validation_trace_v1.verify_trace_proof_against_root(root, 1, proof),
  }
}

test defend_records_the_defender_and_opens_a_thread() {
  let case = case_with(4, h_d)
  let next = defended(case, vkh_1)
  and {
    next.open_threads == 1,
    next.defenders_root != case.defenders_root,
    next == CaseV1 {
      ..case,
      open_threads: 1,
      defenders_root: next.defenders_root,
    },
  }
}

test defend_refuses_a_second_defense_by_the_same_operator() {
  let first = defended(case_with(4, h_d), vkh_1)
  validation_case_v1.defend(first, vkh_1, []) == None
}

test defend_accepts_a_second_operator_with_its_proof() {
  let case = case_with(4, h_d)
  let twice = defended_twice(case)
  and {
    twice.open_threads == 2,
    twice.defenders_root != defended(case, vkh_1).defenders_root,
  }
}

test defend_refuses_the_same_operator_with_any_proof() {
  let first = defended(case_with(4, h_d), vkh_1)
  validation_case_v1.defend(first, vkh_1, beside(vkh_1, vkh_2)) == None
}

test defend_accepts_the_same_operator_in_another_case() {
  let other = validation_case_v1.open_case(
    header_b,
    event_b,
    h_b,
    4,
    h_d,
    vkh_prover,
    h_c,
    started,
  )
  defended(other, vkh_1).open_threads == 1 && defended(case_with(4, h_d), vkh_1).open_threads == 1
}

test open_thread_starts_from_the_case() {
  let case = case_with(4, h_d)
  let thread = validation_case_v1.open_thread(
    case,
    h_d,
    vkh_1,
    h_a,
    h_b,
    started + 500,
    started + 700,
  )
  and {
    thread.version == validation_case_v1.thread_version,
    thread.case_id == h_d,
    thread.defender_vkh == vkh_1,
    thread.challenger_root == case.challenger_root,
    thread.defender_root == h_a,
    thread.step_count == 4,
    thread.low_index == 0,
    thread.high_index == 4,
    thread.agreed_low_hash == case.agreed_initial_hash,
    thread.defender_high_hash == h_b,
    thread.challenger_high_hash == case.challenger_terminal_hash,
    thread.round == 0,
    thread.turn_started_at == started + 700,
    thread.defender_clock_ms == clock - 500,
    thread.challenger_clock_ms == clock,
    thread.turn == AwaitingOperator { midpoint: 2 },
  }
}

test open_thread_of_one_step_is_ready_for_the_step() {
  thread_of(case_with(1, h_d)).turn == ReadyForOneStep
}

test open_thread_never_credits_the_defender_clock() {
  let thread = validation_case_v1.open_thread(
    case_with(4, h_d),
    h_d,
    vkh_1,
    h_a,
    h_b,
    started - 100,
    started + 100,
  )
  thread.defender_clock_ms == clock
}

test open_thread_accepts_a_defense_at_the_end_of_the_clock() {
  let thread = validation_case_v1.open_thread(
    case_with(4, h_d),
    h_d,
    vkh_1,
    h_a,
    h_b,
    started + clock,
    started + clock + 1,
  )
  thread.defender_clock_ms == 0
}

test open_thread_refuses_a_defense_after_the_clock() fail {
  let thread = validation_case_v1.open_thread(
    case_with(4, h_d),
    h_d,
    vkh_1,
    h_a,
    h_b,
    started + clock + 1,
    started + clock + 2,
  )
  thread.case_id == h_d
}

test thread_floor_covers_the_reserve_and_a_receipt() {
  and {
    validation_case_v1.thread_reserve_lovelace == validation_dispute_v1.thread_fee_reserve_lovelace,
    validation_case_v1.thread_floor_lovelace == env.fraud_proof_output_lovelace + validation_case_v1.thread_reserve_lovelace,
    validation_case_v1.case_floor_lovelace == env.fraud_proof_output_lovelace + env.max_dispute_move_fee_lovelace,
  }
}

test defense_bond_hold_is_the_case_start_plus_maturity_after_the_open_window() {
  let case = case_with(4, h_d)
  validation_case_v1.defense_bond_unlock_time(case) == case.started_at + env.block_maturity_duration_v1 - validation_dispute_v1.case_open_window_milliseconds
}

test bond_hold_after_defense_never_shortens_a_hold() {
  let case = case_with(4, h_d)
  let needed = validation_case_v1.defense_bond_unlock_time(case)
  and {
    validation_case_v1.bond_hold_after_defense(None, case) == needed,
    validation_case_v1.bond_hold_after_defense(Some(needed - 1), case) == needed,
    validation_case_v1.bond_hold_after_defense(Some(needed + 1), case) == needed + 1,
  }
}

// ## Thread close

test close_thread_counts_down_and_records_the_outcome() {
  let case = defended_twice(case_with(4, h_d))
  let won = validation_case_v1.close_thread(case, DefenderWins)
  let lost = validation_case_v1.close_thread(case, ChallengerWins)
  let neither = validation_case_v1.close_thread(case, NeitherClaimValid)
  and {
    won == CaseV1 { ..case, open_threads: 1, defender_won: True },
    lost == CaseV1 { ..case, open_threads: 1 },
    neither == CaseV1 { ..case, open_threads: 1, neither_claim_valid: True },
  }
}

test close_thread_refuses_a_case_with_no_open_thread() fail {
  let case = case_with(4, h_d)
  validation_case_v1.close_thread(case, ChallengerWins) != case
}

test lost_defense_receipt_only_for_a_lost_thread() {
  let case = defended(case_with(4, h_d), vkh_1)
  let thread = thread_of(case)
  and {
    validation_case_v1.lost_defense_receipt(case, thread, DefenderWins) == None,
    validation_case_v1.lost_defense_receipt(case, thread, ChallengerWins) == Some(
      LostDefenseReceiptV1 {
        case_id: h_d,
        defender_vkh: vkh_1,
        outcome: ChallengerWins,
        bounty_claimant: vkh_prover,
      },
    ),
    validation_case_v1.lost_defense_receipt(case, thread, NeitherClaimValid) == Some(
      LostDefenseReceiptV1 {
        case_id: h_d,
        defender_vkh: vkh_1,
        outcome: NeitherClaimValid,
        bounty_claimant: vkh_prover,
      },
    ),
  }
}

test thread_leftover_goes_to_the_winner_or_the_treasury() {
  let case = defended(case_with(4, h_d), vkh_1)
  let thread = thread_of(case)
  and {
    validation_case_v1.thread_leftover_address(case, thread, DefenderWins) == from_verification_key(
      vkh_1,
    ),
    validation_case_v1.thread_leftover_address(case, thread, ChallengerWins) == from_verification_key(
      vkh_prover,
    ),
    validation_case_v1.thread_leftover_address(case, thread, NeitherClaimValid) == validation_case_v1.treasury_address,
  }
}

// ## Case close

test case_close_waits_for_every_thread() {
  let case = defended(case_with(4, h_d), vkh_1)
  !validation_case_v1.case_can_close(
    CaseV1 { ..case, defender_won: True },
    validation_case_v1.defense_deadline(case) + 1,
  )
}

test case_close_after_a_defender_win_needs_no_wait() {
  let case = validation_case_v1.close_thread(
    defended(case_with(4, h_d), vkh_1),
    DefenderWins,
  )
  validation_case_v1.case_can_close(case, started)
}

test case_close_otherwise_waits_past_the_defense_window() {
  let case = case_with(4, h_d)
  let deadline = validation_case_v1.defense_deadline(case)
  and {
    deadline == started + clock + validation_dispute_v1.dispute_max_validity_range_milliseconds,
    !validation_case_v1.case_can_close(case, deadline),
    validation_case_v1.case_can_close(case, deadline + 1),
  }
}

/// A case with an open thread closes only once its lower bound is past
/// `started_at + D_case`, and then closes as a defender win.
test case_close_with_an_open_thread_waits_for_d_case() {
  let case = defended(case_with(4, h_d), vkh_1)
  let d_case_end = case.started_at + validation_dispute_v1.max_dispute_duration_milliseconds
  and {
    !validation_case_v1.case_can_close(case, d_case_end),
    validation_case_v1.case_can_close(case, d_case_end + 1),
    validation_case_v1.case_outcome(case) == DefenderWon,
    validation_case_v1.challenger_bond_address(case) == validation_case_v1.treasury_address,
  }
}

test case_outcome_rows() {
  let case = case_with(4, h_d)
  let two = defended_twice(case)
  let lost_both = validation_case_v1.close_thread(
    validation_case_v1.close_thread(two, ChallengerWins),
    ChallengerWins,
  )
  let lost_and_neither = validation_case_v1.close_thread(
    validation_case_v1.close_thread(two, ChallengerWins),
    NeitherClaimValid,
  )
  let neither_and_won = validation_case_v1.close_thread(
    validation_case_v1.close_thread(two, NeitherClaimValid),
    DefenderWins,
  )
  and {
    validation_case_v1.case_outcome(case) == Convicted,
    validation_case_v1.case_outcome(lost_both) == Convicted,
    validation_case_v1.case_outcome(lost_and_neither) == NoValidClaim,
    validation_case_v1.case_outcome(neither_and_won) == DefenderWon,
  }
}

test challenger_bond_is_refunded_iff_the_case_convicts() {
  let case = case_with(4, h_d)
  and {
    validation_case_v1.challenger_bond_address(case) == from_verification_key(
      vkh_prover,
    ),
    validation_case_v1.challenger_bond_address(
      CaseV1 { ..case, defender_won: True },
    ) == validation_case_v1.treasury_address,
    validation_case_v1.challenger_bond_address(
      CaseV1 { ..case, neither_claim_valid: True },
    ) == validation_case_v1.treasury_address,
  }
}
