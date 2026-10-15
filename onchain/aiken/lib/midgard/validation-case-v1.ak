//// Validation cases: the shapes and hand-offs of the case object.
////
//// A case is opened by VerifySource against one challenged block and event.
//// Each active operator may defend it once; every defense starts one
//// two-party thread between the challenger and that defender. Every thread
//// closes back into the case, and the case closes once no thread is open.
////
//// This module fixes:
//// - the case datum `CaseV1`, the thread datum `CaseThreadV1` and the
////   lost-defense receipt `LostDefenseReceiptV1`;
//// - the case redeemer `CaseRedeemerV1` (VerifySource, Defend, ThreadClose,
////   CaseClose);
//// - the token names: the case NFT, the thread token and the receipt;
//// - what each step hands to the next: `open_case` (VerifySource),
////   `defense_is_valid`, `defend` and `open_thread` (Defend), `close_thread`,
////   `lost_defense_receipt` and `thread_leftover_address` (thread close),
////   `case_can_close`, `case_outcome` and `challenger_bond_address` (case
////   close);
//// - the funding and timing constants those steps check: the per-thread
////   reserve, the case floor and the operator bond hold.
////
//// Token custody. The case NFT is minted by VerifySource into the case
//// output, stays in the case output through every Defend and thread close,
//// and is burned at the case close. A thread token is minted by Defend into
//// the thread output and burned by that thread's close.

use aiken/cbor
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{Address, Script, from_verification_key}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use env
use midgard/ledger_state.{EventKey, HeaderHash}
use midgard/mpf_proof_v1
use midgard/validation_dispute_v1.{
  AwaitingOperator, ChallengerWins, DefenderWins, DisputeTurn, DisputeWinner,
  NeitherClaimValid, ReadyForOneStep,
}
use midgard/validation_trace_v1.{
  Accepted, Terminal, ValidationMachineStateV1, ValidationTraceProof,
}

pub const case_version: Int = 1

pub const thread_version: Int = 1

/// A case id is the case NFT's asset name (`case_nft_name`).
pub type CaseId =
  ByteArray

/// The case datum, held with the case NFT at the case validator.
pub type CaseV1 {
  version: Int,
  block_header_hash: HeaderHash,
  event_key: EventKey,
  /// The challenger's trace root over states `0..step_count`.
  challenger_root: ByteArray,
  /// S: the challenger's step count, `1 <= S <= max_machine_step_count`.
  step_count: Int,
  /// The hash of the challenger's Terminal Rejected state at S.
  challenger_terminal_hash: ByteArray,
  /// The challenger's payee. It is the opener's `fraud_prover`.
  bounty_claimant: VerificationKeyHash,
  /// The hash of the derived initial state of the source
  /// (`validation_claim_v1.derive_initial_state`); every thread starts from it.
  agreed_initial_hash: ByteArray,
  /// VerifySource's validity upper bound. The defense window and the bond hold
  /// run from it.
  started_at: Int,
  open_threads: Int,
  /// MPF over the key hash of every operator that has defended this case.
  defenders_root: ByteArray,
  /// Set by the close of a thread that the defender won.
  defender_won: Bool,
  /// Set by the close of a thread that ended with neither claim valid.
  neither_claim_valid: Bool,
}

/// One thread: the bisection between the challenger and one defender.
pub type CaseThreadV1 {
  version: Int,
  case_id: CaseId,
  defender_vkh: VerificationKeyHash,
  challenger_root: ByteArray,
  defender_root: ByteArray,
  /// S, shared by both roots: every trace proof in the thread has its depth.
  step_count: Int,
  low_index: Int,
  high_index: Int,
  agreed_low_hash: ByteArray,
  defender_high_hash: ByteArray,
  challenger_high_hash: ByteArray,
  round: Int,
  turn_started_at: Int,
  defender_clock_ms: Int,
  challenger_clock_ms: Int,
  turn: DisputeTurn,
}

/// Minted by the close of a thread the defender lost; the slash of the
/// defender's operator bond burns it.
pub type LostDefenseReceiptV1 {
  case_id: CaseId,
  defender_vkh: VerificationKeyHash,
  /// `ChallengerWins` or `NeitherClaimValid`.
  outcome: DisputeWinner,
  bounty_claimant: VerificationKeyHash,
}

/// The case validator's redeemer, for both its mint and its spend purpose.
pub type CaseRedeemerV1 {
  /// Mint only. Creates the case: one case NFT, named from the opener thread
  /// input VerifySource spends, into the case output.
  VerifySource { opener_input_index: Int, case_output_index: Int }
  /// Spends the case and mints one thread token. The defender's active node is
  /// spent in the same transaction to extend its bond hold.
  Defend {
    case_output_index: Int,
    thread_output_index: Int,
    active_node_input_index: Int,
    defender_vkh: VerificationKeyHash,
    /// Non-membership of `defender_vkh` in `defenders_root`.
    defenders_proof: mpf.Proof,
    /// The defender's trace root over `0..S`.
    defender_root: ByteArray,
    /// The defender's state at S, and its leaf under `defender_root`.
    defender_state: ValidationMachineStateV1,
    defender_state_proof: ValidationTraceProof,
  }
  /// Spends the case together with one closing thread, whose token is burned.
  ThreadClose {
    case_output_index: Int,
    thread_input_index: Int,
    outcome: DisputeWinner,
  }
  /// Spends the case and its challenger bond, and burns the case NFT. On a
  /// conviction the same transaction mints the fraud-proof token.
  CaseClose {
    bond_input_index: Int,
    bond_output_index: Int,
    remainder_output_index: Int,
  }
}

/// How a closed case settles.
pub type CaseOutcomeV1 {
  /// No defense, or every thread won by the challenger: the block is
  /// convicted and the challenger bond refunded.
  Convicted
  /// A defender won a thread: the block stands, the bond is forfeited.
  DefenderWon
  /// A thread ended with neither claim valid and none was won by a defender:
  /// the block stands, the bond is forfeited.
  NoValidClaim
}

// ## Token names

/// hash(block header hash ‖ event key ‖ opener out-ref). The header hash has a
/// fixed length and both encodings delimit themselves, so the name is
/// injective in the triple; the out-ref makes it unique.
pub fn case_nft_name(
  block_header_hash: HeaderHash,
  event_key: EventKey,
  opener_out_ref: OutputReference,
) -> AssetName {
  expect bytearray.length(block_header_hash) == 32
  block_header_hash
    |> bytearray.concat(cbor.serialise(event_key))
    |> bytearray.concat(cbor.serialise(opener_out_ref))
    |> blake2b_256
}

/// hash(case_id ‖ operator_vkh). One thread per (case, operator).
pub fn thread_token_name(
  case_id: CaseId,
  operator_vkh: VerificationKeyHash,
) -> AssetName {
  expect bytearray.length(case_id) == 32
  expect bytearray.length(operator_vkh) == 28
  case_id
    |> bytearray.concat(operator_vkh)
    |> blake2b_256
}

/// hash(case_id ‖ defender_vkh). This is the thread token's name, so the
/// receipt is minted under a different policy from the thread token.
pub fn receipt_token_name(
  case_id: CaseId,
  defender_vkh: VerificationKeyHash,
) -> AssetName {
  thread_token_name(case_id, defender_vkh)
}

// ## Funding and timing

/// The fee reserve each thread carries, posted by its defender at Defend.
pub const thread_reserve_lovelace: Int =
  validation_dispute_v1.thread_fee_reserve_lovelace

/// The least a new thread holds: the reserve, plus the receipt output its
/// close may have to fund.
pub const thread_floor_lovelace: Int =
  env.fraud_proof_output_lovelace + thread_reserve_lovelace

/// The least the case output holds at VerifySource: the fraud-proof output a
/// convicted close funds, plus one move fee.
pub const case_floor_lovelace: Int =
  env.fraud_proof_output_lovelace + env.max_dispute_move_fee_lovelace

/// P: what is left of maturity after the open deadline and the longest case,
/// for the thread closes, the case close and the follow-ons.
pub const post_case_allowance_milliseconds: Int =
  env.block_maturity_duration_v1 - validation_dispute_v1.case_open_window_milliseconds - validation_dispute_v1.max_dispute_duration_milliseconds

/// The last Defend lands with its lower bound at `started_at + C` (its clock
/// charge); a close whose lower bound is past this follows every Defend.
pub fn defense_deadline(case: CaseV1) -> Int {
  case.started_at + validation_dispute_v1.dispute_side_clock_milliseconds + validation_dispute_v1.dispute_max_validity_range_milliseconds
}

/// The bond hold a defense needs: `started_at + D_case + P`.
pub fn defense_bond_unlock_time(case: CaseV1) -> Int {
  case.started_at + validation_dispute_v1.max_dispute_duration_milliseconds + post_case_allowance_milliseconds
}

/// The defender's node's bond hold after the defense: never shortened.
pub fn bond_hold_after_defense(
  old_bond_unlock_time: Option<Int>,
  case: CaseV1,
) -> Int {
  let needed = defense_bond_unlock_time(case)
  when old_bond_unlock_time is {
    None -> needed
    Some(old) ->
      if old > needed {
        old
      } else {
        needed
      }
  }
}

// ## VerifySource

/// The case VerifySource creates. VerifySource has already checked, in order:
/// the source, R-DESC, the challenger's claim (Terminal, Rejected, leaf S of
/// `challenger_root`), the derived initial state, the open guard
/// (`validation_dispute_v1.case_can_open`) and the bond. The challenger's
/// payee is the opener's `fraud_prover`.
pub fn open_case(
  block_header_hash: HeaderHash,
  event_key: EventKey,
  challenger_root: ByteArray,
  step_count: Int,
  challenger_terminal_hash: ByteArray,
  fraud_prover: VerificationKeyHash,
  agreed_initial_hash: ByteArray,
  source_time_upper: Int,
) -> CaseV1 {
  expect bytearray.length(block_header_hash) == 32
  expect bytearray.length(challenger_root) == 32
  expect step_count >= 1
  expect step_count <= validation_trace_v1.max_machine_step_count
  expect bytearray.length(challenger_terminal_hash) == 32
  expect bytearray.length(fraud_prover) == 28
  expect bytearray.length(agreed_initial_hash) == 32
  expect source_time_upper >= 0
  CaseV1 {
    version: case_version,
    block_header_hash,
    event_key,
    challenger_root,
    step_count,
    challenger_terminal_hash,
    bounty_claimant: fraud_prover,
    agreed_initial_hash,
    started_at: source_time_upper,
    open_threads: 0,
    defenders_root: env.empty_merkle_tree_root,
    defender_won: False,
    neither_claim_valid: False,
  }
}

// ## Defend

/// The defense rule: the defender's state at S, proven under its root, is not
/// the challenger's terminal state, and is not a Terminal state with any
/// verdict but Accepted.
pub fn defense_is_valid(
  case: CaseV1,
  defender_root: ByteArray,
  defender_state: ValidationMachineStateV1,
  defender_state_proof: ValidationTraceProof,
) -> Bool {
  let state_hash = validation_trace_v1.hash_machine_state(defender_state)
  and {
    defender_state_proof.state_index == case.step_count,
    defender_state_proof.state_hash == state_hash,
    validation_trace_v1.verify_trace_proof_against_root(
      defender_root,
      case.step_count,
      defender_state_proof,
    ),
    state_hash != case.challenger_terminal_hash,
    or {
      defender_state.phase != Terminal,
      defender_state.verdict == Accepted,
    },
  }
}

/// The case after one defense: the defender joins `defenders_root`, which it
/// must not already be in, and one more thread is open. `None` when the
/// non-membership proof fails.
pub fn defend(
  case: CaseV1,
  defender_vkh: VerificationKeyHash,
  defenders_proof: mpf.Proof,
) -> Option<CaseV1> {
  expect bytearray.length(defender_vkh) == 28
  when
    mpf_proof_v1.insert_root(
      case.defenders_root,
      defender_vkh,
      defender_vkh,
      defenders_proof,
    )
  is {
    None -> None
    Some(defenders_root) ->
      Some(
        CaseV1 { ..case, defenders_root, open_threads: case.open_threads + 1 },
      )
  }
}

fn first_turn(step_count: Int) -> DisputeTurn {
  if step_count == 1 {
    ReadyForOneStep
  } else {
    AwaitingOperator { midpoint: step_count / 2 }
  }
}

/// The thread a defense starts. Posting the defense is the defender's first
/// clock move: its clock is charged from the case's `started_at` to the
/// defense's lower bound, and must cover it. The challenger's clock starts at
/// the defense's upper bound. The defender reveals first.
pub fn open_thread(
  case: CaseV1,
  case_id: CaseId,
  defender_vkh: VerificationKeyHash,
  defender_root: ByteArray,
  defender_high_hash: ByteArray,
  defense_time_lower: Int,
  defense_time_upper: Int,
) -> CaseThreadV1 {
  let clock = validation_dispute_v1.dispute_side_clock_milliseconds
  let elapsed = if defense_time_lower > case.started_at {
    defense_time_lower - case.started_at
  } else {
    0
  }
  expect elapsed <= clock
  CaseThreadV1 {
    version: thread_version,
    case_id,
    defender_vkh,
    challenger_root: case.challenger_root,
    defender_root,
    step_count: case.step_count,
    low_index: 0,
    high_index: case.step_count,
    agreed_low_hash: case.agreed_initial_hash,
    defender_high_hash,
    challenger_high_hash: case.challenger_terminal_hash,
    round: 0,
    turn_started_at: defense_time_upper,
    defender_clock_ms: clock - elapsed,
    challenger_clock_ms: clock,
    turn: first_turn(case.step_count),
  }
}

// ## Thread close

/// The case after one of its threads closes with `outcome`. The thread side,
/// which knows the outcome, and the case side, which reads it from
/// `ThreadClose`, both require the case output to be exactly this.
pub fn close_thread(case: CaseV1, outcome: DisputeWinner) -> CaseV1 {
  expect case.open_threads >= 1
  let open_threads = case.open_threads - 1
  when outcome is {
    DefenderWins -> CaseV1 { ..case, open_threads, defender_won: True }
    ChallengerWins -> CaseV1 { ..case, open_threads }
    NeitherClaimValid ->
      CaseV1 { ..case, open_threads, neither_claim_valid: True }
  }
}

/// The receipt a lost thread mints; `None` when the defender won.
pub fn lost_defense_receipt(
  case: CaseV1,
  thread: CaseThreadV1,
  outcome: DisputeWinner,
) -> Option<LostDefenseReceiptV1> {
  when outcome is {
    DefenderWins -> None
    _ ->
      Some(
        LostDefenseReceiptV1 {
          case_id: thread.case_id,
          defender_vkh: thread.defender_vkh,
          outcome,
          bounty_claimant: case.bounty_claimant,
        },
      )
  }
}

/// The protocol treasury: its native script, with no stake credential.
pub const treasury_address: Address =
  Address {
    payment_credential: Script(env.protocol_treasury_script_hash),
    stake_credential: None,
  }

/// Where a closed thread's leftover goes: to the defender when it won, to the
/// challenger's payee when the challenger won, and to the treasury when
/// neither claim was valid.
pub fn thread_leftover_address(
  case: CaseV1,
  thread: CaseThreadV1,
  outcome: DisputeWinner,
) -> Address {
  when outcome is {
    DefenderWins -> from_verification_key(thread.defender_vkh)
    ChallengerWins -> from_verification_key(case.bounty_claimant)
    NeitherClaimValid -> treasury_address
  }
}

// ## Case close

/// A case closes once no thread is open, and either a defender has won or the
/// defense window has passed.
pub fn case_can_close(case: CaseV1, current_time_lower: Int) -> Bool {
  and {
    case.open_threads == 0,
    or {
      case.defender_won,
      current_time_lower > defense_deadline(case),
    },
  }
}

pub fn case_outcome(case: CaseV1) -> CaseOutcomeV1 {
  if case.defender_won {
    DefenderWon
  } else if case.neither_claim_valid {
    NoValidClaim
  } else {
    Convicted
  }
}

/// The challenger bond is refunded to the challenger's payee iff the case
/// convicts, and forfeited to the treasury otherwise.
pub fn challenger_bond_address(case: CaseV1) -> Address {
  when case_outcome(case) is {
    Convicted -> from_verification_key(case.bounty_claimant)
    _ -> treasury_address
  }
}
