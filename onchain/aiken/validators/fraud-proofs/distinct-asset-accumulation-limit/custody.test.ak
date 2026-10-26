use aiken/collection/list
use cardano/transaction.{Transaction}
use fraud_proofs/distinct_asset_accumulation_limit/family_entry as entry_validator
use fraud_proofs/distinct_asset_accumulation_limit/input_rank as rank_validator
use fraud_proofs/distinct_asset_accumulation_limit/inputs as inputs_validator
use fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve as resolve_validator
use fraud_proofs/distinct_asset_accumulation_limit/mint as mint_validator
use fraud_proofs/distinct_asset_accumulation_limit/mint_grammar as mint_grammar_validator
use fraud_proofs/distinct_asset_accumulation_limit/outputs as outputs_validator
use fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar as outputs_grammar_validator
use fraud_proofs/distinct_asset_accumulation_limit/terminal as terminal_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  opened_checkpoint, policy,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as grammar
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fixture.{
  ShapeV1, SubjectV1, TxV1, derive, opened_variable, rank_of, subject_tx_of,
  tx_of,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{
  FieldDone, FieldWalked, InputsCross, InputsWalked, PendingInputsV1, RankV1,
  TerminalV1, WalkV1,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/terminal
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  EntryArgsV1, FieldArgsV1, RankArgsV1, ResolveArgsV1,
}
use midgard/fraud_proofs/field_opening_v1.{
  mint_field_index, outputs_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/first_fault_v1
use midgard/fraud_proofs/native_binding_fixture_v1 as native

const elsewhere = #"abababababababababababababababababababababababababababab"

const stranger = #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

const next = native.next_step_script_hash

const own = native.step_script_hash

const thread = native.computation_thread_policy_id

// Two spent inputs crossing at the second; one output and one mint policy
// under the bound when the inputs do not cross.
const crossing_shape =
  ShapeV1 { spends: [16_000, 385], references: [], outputs: [[1]], mint: [1] }

const honest_shape =
  ShapeV1 {
    spends: [16_000, 300],
    references: [],
    outputs: [[1], [2]],
    mint: [1, 1],
  }

fn datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: native.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

/// The continuation transaction: the successor at `next` (or at the spending
/// script itself when `same`) holds `written`, signed by `signer`.
fn moved(tx: TxV1, written: Data, same: Bool, signer: ByteArray) -> Transaction {
  let base = if same {
    native.continue_same_step_l1_tx_v1(tx.bound.subject.transaction_id, written)
  } else {
    native.continue_mid_step_l1_tx_v1(tx.bound.subject.transaction_id, written)
  }
  Transaction { ..base, extra_signatories: [signer] }
}

fn entry_state(tx: TxV1) -> WalkV1 {
  fold.initial_v1(tx.bound, tx.spend_opening, [], policy)
}

// ## Family entry: opens the spend field

fn entered(written: Data, inputs_hash: ByteArray, signer: ByteArray) -> Bool {
  let tx = tx_of(crossing_shape)
  entry_validator.main.spend(
    inputs_hash,
    elsewhere,
    thread,
    policy,
    datum(tx.bound),
    ct.Continue(
      EntryArgsV1 { input_index: 0, output_index: 0, opening: tx.spend_opening },
    ),
    native.own_out_ref(),
    moved(tx, written, False, signer),
  )
}

test entry_opens_the_spend_field() {
  let tx = tx_of(crossing_shape)
  entered(entry_state(tx), next, native.fraud_prover_key_hash)
}

test entry_refuses_a_forged_successor() {
  let tx = tx_of(crossing_shape)
  let state = entry_state(tx)
  and {
    !entered(
      WalkV1 { ..state, entry_count: 1 },
      next,
      native.fraud_prover_key_hash,
    ),
    !entered(
      WalkV1 { ..state, complete: True },
      next,
      native.fraud_prover_key_hash,
    ),
    !entered(state, elsewhere, native.fraud_prover_key_hash),
  }
}

test entry_refuses_a_stranger() fail {
  let tx = tx_of(crossing_shape)
  entered(entry_state(tx), next, stranger)
}

// ## Inputs: take a batch, then resolve it

fn taken(tx: TxV1, count: Int) -> PendingInputsV1 {
  fold.take_inputs_v1(
    entry_state(tx),
    own,
    tx.spend_opening,
    opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
    count,
    [],
    policy,
  )
}

fn took(written: Data, count: Int, signer: ByteArray) -> Bool {
  let tx = tx_of(crossing_shape)
  inputs_validator.main.spend(
    next,
    thread,
    policy,
    datum(entry_state(tx)),
    ct.Continue(
      FieldArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: tx.spend_opening,
        checkpoint_bytes: opened_checkpoint(
          tx.bound,
          spend_inputs_field_index,
          tx.spend_opening,
        ),
        budget: count,
      },
    ),
    native.own_out_ref(),
    moved(tx, written, False, signer),
  )
}

test take_records_its_own_hash_for_the_return() {
  let tx = tx_of(crossing_shape)
  took(taken(tx, 2), 2, native.fraud_prover_key_hash)
}

test take_refuses_a_forged_successor() {
  let tx = tx_of(crossing_shape)
  let pending = taken(tx, 2)
  and {
    !took(
      PendingInputsV1 { ..pending, resume_script_hash: elsewhere },
      2,
      native.fraud_prover_key_hash,
    ),
    !took(
      PendingInputsV1 { ..pending, first_ordinal: 1 },
      2,
      native.fraud_prover_key_hash,
    ),
    !took(taken(tx, 1), 2, native.fraud_prover_key_hash),
  }
}

test take_refuses_more_items_than_the_field_holds() fail {
  let tx = tx_of(crossing_shape)
  took(taken(tx, 2), 3, native.fraud_prover_key_hash)
}

test take_refuses_a_stranger() fail {
  let tx = tx_of(crossing_shape)
  took(taken(tx, 2), 2, stranger)
}

fn resolved(
  shape: ShapeV1,
  count: Int,
  written: Data,
  rank_hash: ByteArray,
  outputs_grammar_hash: ByteArray,
  same: Bool,
) -> Bool {
  let tx = tx_of(shape)
  resolve_validator.main.spend(
    rank_hash,
    outputs_grammar_hash,
    thread,
    datum(taken(tx, count)),
    ct.Continue(
      ResolveArgsV1 {
        input_index: 0,
        output_index: 0,
        witnesses: list.take(tx.witnesses, count),
      },
    ),
    native.own_out_ref(),
    moved(tx, written, same, native.fraud_prover_key_hash),
  )
}

fn resolution(shape: ShapeV1, count: Int) -> Data {
  let tx = tx_of(shape)
  when
    fold.resolve_inputs_v1(taken(tx, count), list.take(tx.witnesses, count))
  is {
    InputsCross(rank) -> {
      let written: Data = rank
      written
    }
    InputsWalked(walked) -> {
      let written: Data = walked
      written
    }
  }
}

test resolve_sends_a_crossing_to_the_rank_step() {
  and {
    resolved(
      crossing_shape,
      2,
      resolution(crossing_shape, 2),
      next,
      elsewhere,
      False,
    ),
    !resolved(
      crossing_shape,
      2,
      resolution(crossing_shape, 2),
      elsewhere,
      next,
      False,
    ),
  }
}

test resolve_sends_an_exhausted_field_to_the_output_grammar() {
  and {
    resolved(
      honest_shape,
      2,
      resolution(honest_shape, 2),
      elsewhere,
      next,
      False,
    ),
    !resolved(
      honest_shape,
      2,
      resolution(honest_shape, 2),
      next,
      elsewhere,
      False,
    ),
  }
}

test resolve_returns_to_the_taking_step() {
  // The taking step recorded its own hash; the continuation returns there.
  and {
    resolved(
      honest_shape,
      1,
      resolution(honest_shape, 1),
      elsewhere,
      elsewhere,
      True,
    ),
    !resolved(
      honest_shape,
      1,
      resolution(honest_shape, 1),
      elsewhere,
      elsewhere,
      False,
    ),
  }
}

test resolve_refuses_a_forged_successor() {
  expect InputsCross(rank) = {
    let tx = tx_of(crossing_shape)
    fold.resolve_inputs_v1(taken(tx, 2), tx.witnesses)
  }
  expect InputsWalked(walked) = {
    let tx = tx_of(honest_shape)
    fold.resolve_inputs_v1(taken(tx, 2), tx.witnesses)
  }
  and {
    !resolved(
      crossing_shape,
      2,
      RankV1 { ..rank, asset_index: 385 },
      next,
      elsewhere,
      False,
    ),
    !resolved(
      crossing_shape,
      2,
      RankV1 { ..rank, input_ordinal: 0 },
      next,
      elsewhere,
      False,
    ),
    !resolved(
      honest_shape,
      2,
      WalkV1 { ..walked, entry_count: 16_299 },
      elsewhere,
      next,
      False,
    ),
  }
}

test resolve_refuses_a_missing_witness() fail {
  let tx = tx_of(crossing_shape)
  resolve_validator.main.spend(
    next,
    elsewhere,
    thread,
    datum(taken(tx, 2)),
    ct.Continue(
      ResolveArgsV1 {
        input_index: 0,
        output_index: 0,
        witnesses: list.take(tx.witnesses, 1),
      },
    ),
    native.own_out_ref(),
    moved(
      tx,
      resolution(crossing_shape, 2),
      False,
      native.fraud_prover_key_hash,
    ),
  )
}

// ## Input rank: places the crossing input in the schedule

fn ranked_state(tx: TxV1) -> RankV1 {
  expect InputsCross(rank) = fold.resolve_inputs_v1(taken(tx, 2), tx.witnesses)
  rank
}

fn ranked(rank: Int, written: Data, terminal_hash: ByteArray) -> Bool {
  let tx = tx_of(crossing_shape)
  rank_validator.main.spend(
    terminal_hash,
    thread,
    policy,
    datum(ranked_state(tx)),
    ct.Continue(
      RankArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: tx.reference_opening,
        rank,
      },
    ),
    native.own_out_ref(),
    moved(tx, written, False, native.fraud_prover_key_hash),
  )
}

test rank_hands_the_crossing_to_the_terminal() {
  let tx = tx_of(crossing_shape)
  let state = ranked_state(tx)
  let rank = rank_of(tx, state.input_item)
  and {
    ranked(rank, derive(tx), next),
    !ranked(rank, derive(tx), elsewhere),
    !ranked(rank, TerminalV1 { ..derive(tx), crossing: None }, next),
  }
}

// ## Output and mint fields: grammar door, then the count

fn outputs_opened(tx: TxV1) -> WalkV1 {
  let tx_inputs = fold.resolve_inputs_v1(taken(tx, 2), tx.witnesses)
  expect InputsWalked(walked) = tx_inputs
  walked
}

test output_grammar_opens_then_certifies() {
  let tx = tx_of(honest_shape)
  let state = outputs_opened(tx)
  let opened = fold.open_next_v1(
    state,
    spend_inputs_field_index,
    outputs_field_index,
    tx.output_opening,
    [],
    policy,
  )
  let (_, grammar_bytes, _) = cursor.initial_with_completion_v1(
    tx.bound,
    outputs_field_index,
    tx.output_opening,
    [],
    policy,
  )
  let certified = fold.certify_v1(
    opened,
    outputs_field_index,
    tx.output_opening,
    grammar_bytes,
    1000,
    [],
    policy,
  )
  let open_step = fn(written: Data, same: Bool) {
    outputs_grammar_validator.main.spend(
      elsewhere,
      elsewhere,
      thread,
      policy,
      datum(state),
      ct.Continue(
        grammar.Open {
          input_index: 0,
          output_index: 0,
          opening: tx.output_opening,
        },
      ),
      native.own_out_ref(),
      moved(tx, written, same, native.fraud_prover_key_hash),
    )
  }
  let certify_step = fn(written: Data, outputs_hash: ByteArray) {
    outputs_grammar_validator.main.spend(
      outputs_hash,
      elsewhere,
      thread,
      policy,
      datum(opened),
      ct.Continue(
        grammar.Certify {
          input_index: 0,
          output_index: 0,
          opening: tx.output_opening,
          checkpoint_bytes: grammar_bytes,
          budget: 1000,
        },
      ),
      native.own_out_ref(),
      moved(tx, written, False, native.fraud_prover_key_hash),
    )
  }
  and {
    open_step(opened, True),
    !open_step(opened, False),
    !open_step(WalkV1 { ..opened, entry_count: 0 }, True),
    certify_step(certified, next),
    !certify_step(certified, elsewhere),
    !certify_step(WalkV1 { ..certified, complete: True }, next),
  }
}

fn outputs_walking(tx: TxV1) -> Pair<WalkV1, ByteArray> {
  opened_variable(
    outputs_opened(tx),
    spend_inputs_field_index,
    outputs_field_index,
    tx.output_opening,
  )
}

fn counted(
  budget: Int,
  written: Data,
  mint_grammar_hash: ByteArray,
  same: Bool,
) -> Bool {
  let tx = tx_of(honest_shape)
  let Pair(state, bytes) = outputs_walking(tx)
  outputs_validator.main.spend(
    mint_grammar_hash,
    elsewhere,
    thread,
    policy,
    datum(state),
    ct.Continue(
      FieldArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: tx.output_opening,
        checkpoint_bytes: bytes,
        budget,
      },
    ),
    native.own_out_ref(),
    moved(tx, written, same, native.fraud_prover_key_hash),
  )
}

fn outputs_after(budget: Int) -> WalkV1 {
  let tx = tx_of(honest_shape)
  let Pair(state, bytes) = outputs_walking(tx)
  expect FieldWalked(walked) = fold.advance_outputs_v1(
    state,
    tx.output_opening,
    bytes,
    budget,
    [],
    policy,
  )
  walked
}

test outputs_route_by_completion() {
  and {
    counted(100, outputs_after(100), next, False),
    !counted(100, outputs_after(100), elsewhere, False),
    counted(1, outputs_after(1), elsewhere, True),
    !counted(1, outputs_after(1), next, False),
    counted(2, outputs_after(2), elsewhere, True),
  }
}

test outputs_refuse_a_forged_successor() {
  let walked = outputs_after(100)
  let paused = outputs_after(1)
  and {
    !counted(
      100,
      WalkV1 { ..walked, entry_count: walked.entry_count - 1 },
      next,
      False,
    ),
    paused.resume != None,
    !counted(1, WalkV1 { ..paused, resume: None }, elsewhere, True),
    !counted(1, WalkV1 { ..paused, mint_index: 1 }, elsewhere, True),
  }
}

fn mint_walking(tx: TxV1) -> Pair<WalkV1, ByteArray> {
  let Pair(state, bytes) = outputs_walking(tx)
  expect FieldWalked(walked) = fold.advance_outputs_v1(
    state,
    tx.output_opening,
    bytes,
    100,
    [],
    policy,
  )
  opened_variable(
    walked,
    outputs_field_index,
    mint_field_index,
    tx.mint_opening,
  )
}

fn minted(
  count: Int,
  written: Data,
  terminal_hash: ByteArray,
  same: Bool,
) -> Bool {
  let tx = tx_of(honest_shape)
  let Pair(state, bytes) = mint_walking(tx)
  mint_validator.main.spend(
    terminal_hash,
    thread,
    policy,
    datum(state),
    ct.Continue(
      FieldArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: tx.mint_opening,
        checkpoint_bytes: bytes,
        budget: count,
      },
    ),
    native.own_out_ref(),
    moved(tx, written, same, native.fraud_prover_key_hash),
  )
}

fn mint_after(count: Int) -> Data {
  let tx = tx_of(honest_shape)
  let Pair(state, bytes) = mint_walking(tx)
  when
    fold.advance_mint_v1(state, tx.mint_opening, bytes, count, [], policy)
  is {
    FieldWalked(walked) -> {
      let written: Data = walked
      written
    }
    FieldDone(terminal) -> {
      let written: Data = terminal
      written
    }
  }
}

test mint_ends_the_derivation_at_the_terminal() {
  let tx = tx_of(honest_shape)
  and {
    minted(2, mint_after(2), next, False),
    minted(2, derive(tx), next, False),
    !minted(2, mint_after(2), elsewhere, False),
    !minted(
      2,
      TerminalV1 {
        bound: tx.bound,
        crossing: Some(first_fault_v1.MintAssetFault { mint_index: 0 }),
      },
      next,
      False,
    ),
    minted(1, mint_after(1), elsewhere, True),
    !minted(1, mint_after(2), elsewhere, True),
  }
}

test mint_grammar_sends_an_empty_mint_to_the_terminal() {
  let shape = ShapeV1 { ..honest_shape, mint: [] }
  let tx = tx_of(shape)
  let Pair(state, bytes) = outputs_walking(tx)
  expect FieldWalked(walked) = fold.advance_outputs_v1(
    state,
    tx.output_opening,
    bytes,
    100,
    [],
    policy,
  )
  let opened = fold.open_next_v1(
    walked,
    outputs_field_index,
    mint_field_index,
    tx.mint_opening,
    [],
    policy,
  )
  let (_, grammar_bytes, _) = cursor.initial_with_completion_v1(
    tx.bound,
    mint_field_index,
    tx.mint_opening,
    [],
    policy,
  )
  let step = fn(written: Data, terminal_hash: ByteArray) {
    mint_grammar_validator.main.spend(
      elsewhere,
      terminal_hash,
      thread,
      policy,
      datum(opened),
      ct.Continue(
        grammar.Certify {
          input_index: 0,
          output_index: 0,
          opening: tx.mint_opening,
          checkpoint_bytes: grammar_bytes,
          budget: 1000,
        },
      ),
      native.own_out_ref(),
      moved(tx, written, False, native.fraud_prover_key_hash),
    )
  }
  and {
    step(derive(tx), next),
    !step(derive(tx), elsewhere),
    !step(
      TerminalV1 {
        bound: tx.bound,
        crossing: Some(first_fault_v1.MintAssetFault { mint_index: 0 }),
      },
      next,
    ),
  }
}

// ## Terminal: finalizes the thread exactly when the verdict is contradicted

fn finalized(
  shape: ShapeV1,
  subject: SubjectV1,
  signer: List<ByteArray>,
) -> Bool {
  let tx = subject_tx_of(shape, subject)
  terminal_validator.main.spend(
    native.fraud_proof_policy_id,
    native.fraud_proof_token_address(),
    thread,
    datum(derive(tx)),
    ct.Continue(
      terminal.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    native.own_out_ref(),
    Transaction {
      ..native.finalize_step_l1_tx_v1(tx.bound.subject.transaction_id),
      extra_signatories: signer,
    },
  )
}

const accepted = SubjectV1 { source_kind: 0, rejection_reason: None }

test terminal_finalizes_a_wrongful_acceptance() {
  finalized(crossing_shape, accepted, [])
}

test terminal_refuses_an_honest_acceptance() {
  !finalized(honest_shape, accepted, [])
}

test terminal_finalization_is_open_to_any_submitter() {
  finalized(crossing_shape, accepted, [stranger])
}
