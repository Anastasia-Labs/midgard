//// Q35 network-id forced scan: the resumable §10 walk over the rejected
//// forced transaction's `outputs` field.
////
//// The thread arrives here from `forced_step`, which has already
//// authenticated the forced leaf, its `NetworkIdMismatch` verdict and the
//// leaf source, and has already refused an honest body-network mismatch. What
//// is left to prove is the output half of the negated predicate — that no
//// output address names a foreign network — and that is what cannot be done
//// in one transaction at the shapes a forced transaction can take.
////
//// The loop is §10's: `Open` (tiers 1–2) or `StartGrammar`/`ResumeGrammar`/
//// `FinishGrammar` (tier 3) derives the semantic walk's item-zero position,
//// then `Advance` folds batches of outputs, carrying a 32-byte checkpoint
//// digest in thread state and re-supplying the checkpoint bytes through the
//// redeemer. When the walk completes, the thread is handed to step 02 with
//// exactly the state that step's forced arm now takes as established.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, NativeTxAnchorV1,
  opened_certified_field_walk_from_grammar, opened_field_grammar_certification,
  opened_field_walk, outputs_field_index,
  resume_opened_field_grammar_certification, resume_opened_field_walk,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_network_id_cbor, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
}
use midgard/fraud_proofs/network_id/forced_scan.{
  ActionV1, Advance, BoundV1, Datum, FinishGrammar, Grammar, Open, Ready,
  ResumeGrammar, Scanning, SpendRedeemer, StartGrammar, State, grammar_batch,
  scan_batch,
}
use midgard/fraud_proofs/network_id/step_01.{ForcedNetworkIdMismatch}
use midgard/fraud_proofs/network_id/step_02 as step_02
use midgard/native_tx_field_access_v1.{
  encode_field_preimage, field_count_requires_certification,
}
use midgard/native_tx_machine_walk_v1.{
  FieldGrammarCheckpointV1, FieldWalkCheckpointV1, certify_field_grammar,
  encode_field_grammar_checkpoint, encode_field_walk_checkpoint,
  field_grammar_checkpoint_hash, field_grammar_is_complete,
  field_walk_checkpoint_hash, walk_fold, walk_is_complete,
}

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) ->
        execute(
          action,
          datum,
          own_out_ref,
          tx,
          step_02_validator_script_hash,
          computation_thread_token_policy_id,
          field_preimage_certificate_policy_id,
        )
    }
  }

  else(_) {
    fail
  }
}

fn execute(
  action: ActionV1,
  datum: Option<Datum>,
  own_out_ref: OutputReference,
  tx: Transaction,
  step_02_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, .. } = tx
  expect Some(step_datum) = datum
  let own_hash, _, _, input_state, output_hash, output_state <- continue(
    thread_policy,
    step_datum,
    input_index(action),
    output_index(action),
    own_out_ref,
    inputs,
    outputs,
  )
  expect Some(state) = input_state
  when action is {
    Open { opening, .. } -> {
      expect Ready { bound } = state
      let Pair(view, start) = opened_field_walk(
        opening,
        BodyAnchor { tx_id: bound.bad_tx_id },
        outputs_field_index,
        reference_inputs,
        certificate_policy,
      )
      // Tiers 1–2 only. A certified view's §5.1 count is provisional until the
      // grammar walk certifies it, so the direct opening has no authenticated
      // item count to derive a semantic position from.
      expect !field_count_requires_certification(view)
      let expected: Data = Scanning {
        bound,
        checkpoint_hash: field_walk_checkpoint_hash(start),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    StartGrammar { opening, item_budget, .. } -> {
      expect and {
          item_budget > 0,
          item_budget <= grammar_batch,
        }
      expect Ready { bound } = state
      let Pair(view, start) = opened_field_grammar_certification(
        opening,
        BodyAnchor { tx_id: bound.bad_tx_id },
        outputs_field_index,
        reference_inputs,
        certificate_policy,
      )
      let next = certify_field_grammar(view, start, item_budget)
      let expected: Data = Grammar {
        bound,
        checkpoint_hash: field_grammar_checkpoint_hash(next),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    ResumeGrammar { opening, checkpoint_bytes, item_budget, .. } -> {
      expect and {
          item_budget > 0,
          item_budget <= grammar_batch,
        }
      expect Grammar { bound, checkpoint_hash } = state
      let Pair(view, resumed) = resume_opened_field_grammar_certification(
        opening,
        BodyAnchor { tx_id: bound.bad_tx_id },
        outputs_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      expect !field_grammar_is_complete(resumed)
      let next = certify_field_grammar(view, resumed, item_budget)
      let expected: Data = Grammar {
        bound,
        checkpoint_hash: field_grammar_checkpoint_hash(next),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    FinishGrammar { opening, checkpoint_bytes, .. } -> {
      expect Grammar { bound, checkpoint_hash } = state
      let Pair(_view, start) = opened_certified_field_walk_from_grammar(
        opening,
        BodyAnchor { tx_id: bound.bad_tx_id },
        outputs_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      let expected: Data = Scanning {
        bound,
        checkpoint_hash: field_walk_checkpoint_hash(start),
      }
      and {
        output_hash == own_hash,
        output_state == expected,
      }
    }
    Advance { opening, checkpoint_bytes, item_budget, .. } -> {
      expect and {
          item_budget > 0,
          item_budget <= scan_batch,
        }
      expect Scanning { bound, checkpoint_hash } = state
      let Pair(view, resumed) = resume_opened_field_walk(
        opening,
        BodyAnchor { tx_id: bound.bad_tx_id },
        outputs_field_index,
        checkpoint_hash,
        checkpoint_bytes,
        reference_inputs,
        certificate_policy,
      )
      let Pair(folded, next) = walk_fold(
        view,
        resumed,
        item_budget,
        True,
        fn(ok, _index, item) {
          and {
            ok,
            decode_midgard_tx_output_network_id_cbor(item) == bound.expected_network_id,
          }
        },
      )
      // One foreign output means the operator's `NetworkIdMismatch` rejection
      // was honest, and the thread has nothing left to convict.
      expect folded
      if walk_is_complete(next) {
        let expected: Data = step_02.State {
          bad_tx_id: bound.bad_tx_id,
          committed_tx_network_id: bound.committed_tx_network_id,
          expected_network_id: bound.expected_network_id,
          fault: ForcedNetworkIdMismatch,
          post_utxo: None,
          forced_source_key: Some(bound.forced_source_key),
        }
        and {
          output_hash == step_02_hash,
          output_state == expected,
        }
      } else {
        let expected: Data = Scanning {
          bound,
          checkpoint_hash: field_walk_checkpoint_hash(next),
        }
        and {
          output_hash == own_hash,
          output_state == expected,
        }
      }
    }
  }
}

fn input_index(action: ActionV1) -> Int {
  when action is {
    Open { input_index, .. } -> input_index
    StartGrammar { input_index, .. } -> input_index
    ResumeGrammar { input_index, .. } -> input_index
    FinishGrammar { input_index, .. } -> input_index
    Advance { input_index, .. } -> input_index
  }
}

fn output_index(action: ActionV1) -> Int {
  when action is {
    Open { output_index, .. } -> output_index
    StartGrammar { output_index, .. } -> output_index
    ResumeGrammar { output_index, .. } -> output_index
    FinishGrammar { output_index, .. } -> output_index
    Advance { output_index, .. } -> output_index
  }
}

// ## Tests
//
// Both polarities of every arm. The accepted direction drives the real loop —
// `Open`, then `Advance` until the walk completes and the thread lands on step
// 02 carrying exactly the state that step's forced arm now takes as
// established — plus the tier-3 route through the grammar arms. The refused
// direction covers what a prover would have to be able to do for the loop to
// prove less than it claims: resume from a position nothing committed, take an
// arm out of order, over- or under-budget a batch, hand the thread to the
// wrong script, or walk past an output that names a foreign network.

/// The exact bytes `forced_step` freezes into `forced_source_key` for the
/// pinned order key of `forced-wire.test.ak`.
const forced_source_key_bytes =
  #"d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff"

/// Above the §8.3 raw-carriage bound (`chunk_bytes_k`, 15,148 bytes), which is
/// what makes tier-3 certified carriage admissible at all: a minimal output
/// encodes to 41 bytes and costs 43 with its §5.1 wrapper, so 400 of them is
/// ~17.2 kB over two chunks. It is also the shape the whole redesign exists
/// for — a single-transaction scan of this field is not affordable.
const certified_output_count: Int = 400

fn output_at_network(
  output: MidgardTxOutput,
  network_id: Int,
) -> MidgardTxOutput {
  let address = output.address
  MidgardTxOutput {
    ..output,
    address: MidgardAddress { ..address, network_id },
  }
}

fn build_outputs(
  output: MidgardTxOutput,
  index: Int,
  count: Int,
  foreign_index: Int,
) -> List<MidgardTxOutput> {
  if index == count {
    []
  } else {
    [
      output_at_network(
        output,
        if index == foreign_index {
          1
        } else {
          0
        },
      ),
      ..build_outputs(output, index + 1, count, foreign_index)
    ]
  }
}

/// `count` outputs on the deployment network, except the one at
/// `foreign_index` (use a negative index for an all-compliant field).
fn outputs_v1(count: Int, foreign_index: Int) -> List<MidgardTxOutput> {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  build_outputs(output, 0, count, foreign_index)
}

fn block_of(outputs: List<MidgardTxOutput>) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, outputs },
    },
  )
}

fn encoded_outputs(outputs: List<MidgardTxOutput>) -> List<ByteArray> {
  outputs |> list.map(encode_midgard_tx_output)
}

fn inline_opening(
  block: fixture.NativeBlockFixtureV1,
  outputs: List<MidgardTxOutput>,
) -> FieldOpeningV1 {
  fixture.inline_body_opening(block.compact_cbor, encoded_outputs(outputs))
}

fn anchor_of(block: fixture.NativeBlockFixtureV1) -> NativeTxAnchorV1 {
  BodyAnchor { tx_id: block.tx_id }
}

fn bound_of(block: fixture.NativeBlockFixtureV1) -> BoundV1 {
  BoundV1 {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id: 0,
    forced_source_key: forced_source_key_bytes,
  }
}

/// The state the completing `Advance` must write into step 02.
fn handoff_state(block: fixture.NativeBlockFixtureV1) -> step_02.State {
  step_02.State {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id: 0,
    fault: ForcedNetworkIdMismatch,
    post_utxo: None,
    forced_source_key: Some(forced_source_key_bytes),
  }
}

/// The validator's own fold step, so a checkpoint derived here costs exactly
/// what the validator's does. That is what makes the marginal per-output cost
/// readable off a pair of batch tests that differ only in `item_budget`.
fn agrees(ok: Bool, item: ByteArray) -> Bool {
  and {
    ok,
    decode_midgard_tx_output_network_id_cbor(item) == 0,
  }
}

// ### Deriving the positions the thread commits
//
// Every helper below takes an already-built opening (and, under tier 3, the
// already-built reference inputs) rather than rebuilding one, so a test pays
// for its fixture once and the batch pairs differ only where they are meant
// to.

fn walk_at(
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  taken: Int,
) -> FieldWalkCheckpointV1 {
  let Pair(view, start) = opened_field_walk(
    opening,
    anchor_of(block),
    outputs_field_index,
    reference_inputs,
    fixture.test_certificate_policy_id,
  )
  let Pair(_, next) = walk_fold(
    view,
    start,
    taken,
    True,
    fn(ok, _index, item) { agrees(ok, item) },
  )
  next
}

fn scanning_of(
  block: fixture.NativeBlockFixtureV1,
  checkpoint: FieldWalkCheckpointV1,
) -> State {
  Scanning {
    bound: bound_of(block),
    checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
  }
}

fn grammar_at(
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  taken: Int,
) -> FieldGrammarCheckpointV1 {
  let Pair(view, start) = opened_field_grammar_certification(
    opening,
    anchor_of(block),
    outputs_field_index,
    reference_inputs,
    fixture.test_certificate_policy_id,
  )
  certify_field_grammar(view, start, taken)
}

fn grammar_of(
  block: fixture.NativeBlockFixtureV1,
  checkpoint: FieldGrammarCheckpointV1,
) -> State {
  Grammar {
    bound: bound_of(block),
    checkpoint_hash: field_grammar_checkpoint_hash(checkpoint),
  }
}

/// The semantic position a terminal grammar certificate yields, advanced over
/// `taken` further outputs.
fn certified_walk_at(
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  grammar: FieldGrammarCheckpointV1,
  taken: Int,
) -> FieldWalkCheckpointV1 {
  let Pair(view, start) = opened_certified_field_walk_from_grammar(
    opening,
    anchor_of(block),
    outputs_field_index,
    field_grammar_checkpoint_hash(grammar),
    encode_field_grammar_checkpoint(grammar),
    reference_inputs,
    fixture.test_certificate_policy_id,
  )
  let Pair(_, next) = walk_fold(
    view,
    start,
    taken,
    True,
    fn(ok, _index, item) { agrees(ok, item) },
  )
  next
}

// ### L1 contexts and the call itself

fn loop_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction {
    ..fixture.continue_same_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    reference_inputs,
  }
}

fn handoff_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    reference_inputs,
  }
}

fn run_scan(
  step_02_hash: ScriptHash,
  input_state: State,
  action: ActionV1,
  tx: Transaction,
) -> Bool {
  main.spend(
    step_02_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(action),
    fixture.own_out_ref(),
    tx,
  )
}

fn open_action(opening: FieldOpeningV1) -> ActionV1 {
  Open { input_index: 0, output_index: 0, opening }
}

fn advance_action(
  opening: FieldOpeningV1,
  checkpoint: FieldWalkCheckpointV1,
  item_budget: Int,
) -> ActionV1 {
  Advance {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: encode_field_walk_checkpoint(checkpoint),
    item_budget,
  }
}

fn start_grammar_action(opening: FieldOpeningV1, item_budget: Int) -> ActionV1 {
  StartGrammar { input_index: 0, output_index: 0, opening, item_budget }
}

// ### Accepted direction

/// One output: the walk opens, and a single `Advance` completes it and hands
/// the thread to step 02.
test network_id_forced_scan_completes_a_single_output_field() {
  let outputs = outputs_v1(1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let opened: Data = scanning_of(block, start)
  let done: Data = handoff_state(block)
  and {
    run_scan(
      fixture.next_step_script_hash,
      Ready { bound: bound_of(block) },
      open_action(opening),
      loop_tx(block, opened, []),
    ),
    run_scan(
      fixture.next_step_script_hash,
      scanning_of(block, start),
      advance_action(opening, start, 1),
      handoff_tx(block, done, []),
    ),
  }
}

/// Two outputs taken one at a time: the intermediate `Advance` self-loops on
/// the own hash with the new checkpoint digest, and only the second finalises.
test network_id_forced_scan_completes_two_outputs_over_two_batches() {
  let outputs = outputs_v1(2, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let midway = walk_at(block, opening, [], 1)
  let opened: Data = scanning_of(block, start)
  let halfway: Data = scanning_of(block, midway)
  let done: Data = handoff_state(block)
  and {
    run_scan(
      fixture.next_step_script_hash,
      Ready { bound: bound_of(block) },
      open_action(opening),
      loop_tx(block, opened, []),
    ),
    run_scan(
      fixture.next_step_script_hash,
      scanning_of(block, start),
      advance_action(opening, start, 1),
      loop_tx(block, halfway, []),
    ),
    run_scan(
      fixture.next_step_script_hash,
      scanning_of(block, midway),
      advance_action(opening, midway, 1),
      handoff_tx(block, done, []),
    ),
  }
}

/// More outputs than one batch admits: a full `scan_batch` first, then the
/// remainder. This is the shape the redesign exists for.
test network_id_forced_scan_completes_more_outputs_than_one_batch() {
  let outputs = outputs_v1(scan_batch + 1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let batched = walk_at(block, opening, [], scan_batch)
  let opened: Data = scanning_of(block, start)
  let after_batch: Data = scanning_of(block, batched)
  let done: Data = handoff_state(block)
  and {
    run_scan(
      fixture.next_step_script_hash,
      Ready { bound: bound_of(block) },
      open_action(opening),
      loop_tx(block, opened, []),
    ),
    run_scan(
      fixture.next_step_script_hash,
      scanning_of(block, start),
      advance_action(opening, start, scan_batch),
      loop_tx(block, after_batch, []),
    ),
    run_scan(
      fixture.next_step_script_hash,
      scanning_of(block, batched),
      advance_action(opening, batched, 1),
      handoff_tx(block, done, []),
    ),
  }
}

/// Tier 3, over a field past the raw-carriage bound: the envelope is certified
/// in grammar batches, the terminal certificate becomes the semantic
/// item-zero position, and `Advance` scans from there.
///
/// The two `ResumeGrammar` transactions between item 256 and the terminal are
/// the same arm at the same budget over the same view, so they are telescoped
/// into the committed checkpoint `FinishGrammar` is run against rather than
/// re-asserted three more times.
test network_id_forced_scan_walks_a_certified_field_through_the_grammar() {
  let outputs = outputs_v1(certified_output_count, -1)
  let block = block_of(outputs)
  let preimage = encode_field_preimage(encoded_outputs(outputs))
  let opening = fixture.certified_body_opening(block.compact_cbor, preimage)
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    outputs_field_index,
    preimage,
  )
  let first = grammar_at(block, opening, refs, grammar_batch)
  let second = grammar_at(block, opening, refs, grammar_batch * 2)
  let terminal = grammar_at(block, opening, refs, certified_output_count)
  let started: Data = grammar_of(block, first)
  let resumed: Data = grammar_of(block, second)
  let certified_start = certified_walk_at(block, opening, refs, terminal, 0)
  let after_batch = certified_walk_at(
    block,
    opening,
    refs,
    terminal,
    scan_batch,
  )
  let scanning: Data = scanning_of(block, certified_start)
  let advanced: Data = scanning_of(block, after_batch)
  and {
    run_scan(
      fixture.next_step_script_hash,
      Ready { bound: bound_of(block) },
      start_grammar_action(opening, grammar_batch),
      loop_tx(block, started, refs),
    ),
    run_scan(
      fixture.next_step_script_hash,
      grammar_of(block, first),
      ResumeGrammar {
        input_index: 0,
        output_index: 0,
        opening,
        checkpoint_bytes: encode_field_grammar_checkpoint(first),
        item_budget: grammar_batch,
      },
      loop_tx(block, resumed, refs),
    ),
    run_scan(
      fixture.next_step_script_hash,
      grammar_of(block, terminal),
      FinishGrammar {
        input_index: 0,
        output_index: 0,
        opening,
        checkpoint_bytes: encode_field_grammar_checkpoint(terminal),
      },
      loop_tx(block, scanning, refs),
    ),
    run_scan(
      fixture.next_step_script_hash,
      scanning_of(block, certified_start),
      advance_action(opening, certified_start, scan_batch),
      loop_tx(block, advanced, refs),
    ),
  }
}

// ### Marginal cost
//
// Each pair below differs only in how many items the batch takes: 16 against
// 48, in the validator and in the checkpoint this file derives for the
// expected output state, over one and the same field. The `aiken check` budget
// difference between a pair is therefore 64 folded items, and half of that is
// what one item costs the validator.

test network_id_forced_scan_advances_a_sixteen_output_batch() {
  let outputs = outputs_v1(scan_batch, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let next: Data = scanning_of(block, walk_at(block, opening, [], 16))
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 16),
    loop_tx(block, next, []),
  )
}

test network_id_forced_scan_advances_a_forty_eight_output_batch() {
  let outputs = outputs_v1(scan_batch, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let next: Data = scanning_of(block, walk_at(block, opening, [], 48))
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 48),
    loop_tx(block, next, []),
  )
}

/// The same pair over tier-3 carriage, because a chunked read is not a whole
/// view's: the semantic batch has to be priced where the carriage is dearest.
test network_id_forced_scan_advances_a_sixteen_output_certified_batch() {
  let outputs = outputs_v1(certified_output_count, -1)
  let block = block_of(outputs)
  let preimage = encode_field_preimage(encoded_outputs(outputs))
  let opening = fixture.certified_body_opening(block.compact_cbor, preimage)
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    outputs_field_index,
    preimage,
  )
  let terminal = grammar_at(block, opening, refs, certified_output_count)
  let start = certified_walk_at(block, opening, refs, terminal, 0)
  let next: Data = scanning_of(
    block,
    certified_walk_at(block, opening, refs, terminal, 16),
  )
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 16),
    loop_tx(block, next, refs),
  )
}

test network_id_forced_scan_advances_a_forty_eight_output_certified_batch() {
  let outputs = outputs_v1(certified_output_count, -1)
  let block = block_of(outputs)
  let preimage = encode_field_preimage(encoded_outputs(outputs))
  let opening = fixture.certified_body_opening(block.compact_cbor, preimage)
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    outputs_field_index,
    preimage,
  )
  let terminal = grammar_at(block, opening, refs, certified_output_count)
  let start = certified_walk_at(block, opening, refs, terminal, 0)
  let next: Data = scanning_of(
    block,
    certified_walk_at(block, opening, refs, terminal, 48),
  )
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 48),
    loop_tx(block, next, refs),
  )
}

test network_id_forced_scan_certifies_a_sixteen_item_grammar_batch() {
  let outputs = outputs_v1(certified_output_count, -1)
  let block = block_of(outputs)
  let preimage = encode_field_preimage(encoded_outputs(outputs))
  let opening = fixture.certified_body_opening(block.compact_cbor, preimage)
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    outputs_field_index,
    preimage,
  )
  let next: Data = grammar_of(block, grammar_at(block, opening, refs, 16))
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    start_grammar_action(opening, 16),
    loop_tx(block, next, refs),
  )
}

test network_id_forced_scan_certifies_a_forty_eight_item_grammar_batch() {
  let outputs = outputs_v1(certified_output_count, -1)
  let block = block_of(outputs)
  let preimage = encode_field_preimage(encoded_outputs(outputs))
  let opening = fixture.certified_body_opening(block.compact_cbor, preimage)
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    outputs_field_index,
    preimage,
  )
  let next: Data = grammar_of(block, grammar_at(block, opening, refs, 48))
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    start_grammar_action(opening, 48),
    loop_tx(block, next, refs),
  )
}

// ### Refused direction

/// A resumed position is only as good as the digest thread state committed:
/// bytes for a different position simply do not hash to it.
test network_id_forced_scan_refuses_uncommitted_checkpoint_bytes() fail {
  let outputs = outputs_v1(2, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let next: Data = scanning_of(block, walk_at(block, opening, [], 2))
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, walk_at(block, opening, [], 1)),
    advance_action(opening, start, 1),
    loop_tx(block, next, []),
  )
}

/// `Advance` has no position to resume from until an opening arm derived one.
test network_id_forced_scan_refuses_an_advance_from_ready() fail {
  let outputs = outputs_v1(1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let done: Data = handoff_state(block)
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    advance_action(opening, walk_at(block, opening, [], 0), 1),
    handoff_tx(block, done, []),
  )
}

/// Re-opening a walk already in progress would discard everything the earlier
/// batches established.
test network_id_forced_scan_refuses_an_open_from_scanning() fail {
  let outputs = outputs_v1(2, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let opened: Data = scanning_of(block, walk_at(block, opening, [], 0))
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, walk_at(block, opening, [], 1)),
    open_action(opening),
    loop_tx(block, opened, []),
  )
}

/// The direct opening has no authenticated item count under tier 3, so a
/// certified field must go through the grammar arms.
test network_id_forced_scan_refuses_an_open_over_a_certified_opening() fail {
  let outputs = outputs_v1(certified_output_count, -1)
  let block = block_of(outputs)
  let preimage = encode_field_preimage(encoded_outputs(outputs))
  let opening = fixture.certified_body_opening(block.compact_cbor, preimage)
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    outputs_field_index,
    preimage,
  )
  let terminal = grammar_at(block, opening, refs, certified_output_count)
  let opened: Data = scanning_of(
    block,
    certified_walk_at(block, opening, refs, terminal, 0),
  )
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    open_action(opening),
    loop_tx(block, opened, refs),
  )
}

/// Honest refusal, and the reason the scan is batched at all: a foreign output
/// found in a *later* batch means the operator's rejection stands.
test network_id_forced_scan_refuses_a_foreign_output_in_a_later_batch() fail {
  let outputs = outputs_v1(3, 2)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let done: Data = handoff_state(block)
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, walk_at(block, opening, [], 2)),
    advance_action(opening, walk_at(block, opening, [], 2), 1),
    handoff_tx(block, done, []),
  )
}

test network_id_forced_scan_refuses_a_zero_item_budget() fail {
  let outputs = outputs_v1(2, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let unchanged: Data = scanning_of(block, start)
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 0),
    loop_tx(block, unchanged, []),
  )
}

test network_id_forced_scan_refuses_an_item_budget_above_the_scan_batch() fail {
  let outputs = outputs_v1(scan_batch + 1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let done: Data = handoff_state(block)
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, scan_batch + 1),
    handoff_tx(block, done, []),
  )
}

/// The grammar arms are open to any tier whose whole-view walk is
/// unaffordable, so their budget guard is exercised over a tier-1 opening.
test network_id_forced_scan_refuses_a_grammar_budget_above_the_grammar_batch() fail {
  let outputs = outputs_v1(4, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let certified: Data = grammar_of(block, grammar_at(block, opening, [], 4))
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    start_grammar_action(opening, grammar_batch + 1),
    loop_tx(block, certified, []),
  )
}

test network_id_forced_scan_refuses_a_zero_grammar_budget() fail {
  let outputs = outputs_v1(4, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let unchanged: Data = grammar_of(block, grammar_at(block, opening, [], 0))
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    start_grammar_action(opening, 0),
    loop_tx(block, unchanged, []),
  )
}

/// A completed walk goes to step 02 and nowhere else.
test network_id_forced_scan_refuses_a_handoff_to_a_wrong_successor() fail {
  let outputs = outputs_v1(1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let done: Data = handoff_state(block)
  run_scan(
    fixture.step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 1),
    handoff_tx(block, done, []),
  )
}

/// The forced key is what fixes the wrongful-rejection polarity in step 02, so
/// a handoff that drops it is refused here rather than there.
test network_id_forced_scan_refuses_a_handoff_dropping_the_forced_source_key() fail {
  let outputs = outputs_v1(1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let done: Data = step_02.State {
    ..handoff_state(block),
    forced_source_key: None,
  }
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 1),
    handoff_tx(block, done, []),
  )
}

/// A completed walk may not self-loop: the thread would sit at a state no arm
/// can leave.
test network_id_forced_scan_refuses_a_completed_walk_that_self_loops() fail {
  let outputs = outputs_v1(1, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let looped: Data = scanning_of(block, walk_at(block, opening, [], 1))
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 1),
    loop_tx(block, looped, []),
  )
}

/// An unfinished walk may not finalise: a prover cannot stop at the last
/// compliant output and claim the field is clean.
test network_id_forced_scan_refuses_an_incomplete_walk_that_finalises() fail {
  let outputs = outputs_v1(2, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let done: Data = handoff_state(block)
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 1),
    handoff_tx(block, done, []),
  )
}

/// The bound travels unchanged: a batch cannot rewrite the network the walk is
/// held to.
test network_id_forced_scan_refuses_a_rewritten_expected_network() fail {
  let outputs = outputs_v1(2, -1)
  let block = block_of(outputs)
  let opening = inline_opening(block, outputs)
  let start = walk_at(block, opening, [], 0)
  let next: Data = Scanning {
    bound: BoundV1 { ..bound_of(block), expected_network_id: 1 },
    checkpoint_hash: field_walk_checkpoint_hash(walk_at(block, opening, [], 1)),
  }
  run_scan(
    fixture.next_step_script_hash,
    scanning_of(block, start),
    advance_action(opening, start, 1),
    loop_tx(block, next, []),
  )
}

/// Substituted bytes: a fabricated field-2 preimage showing compliant
/// addresses does not hash to the committed field.
test network_id_forced_scan_refuses_a_forged_acquitting_preimage() fail {
  let honest = outputs_v1(1, 0)
  let forged = outputs_v1(1, -1)
  let block = block_of(honest)
  let honest_opening = inline_opening(block, honest)
  let opened: Data = scanning_of(block, walk_at(block, honest_opening, [], 0))
  run_scan(
    fixture.next_step_script_hash,
    Ready { bound: bound_of(block) },
    open_action(inline_opening(block, forged)),
    loop_tx(block, opened, []),
  )
}
