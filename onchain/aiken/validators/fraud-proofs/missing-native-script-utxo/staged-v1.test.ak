use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use fraud_proofs/missing_native_script_utxo/step_05 as step_05_validator
use fraud_proofs/missing_native_script_utxo/step_06 as step_06_validator
use fraud_proofs/missing_native_script_utxo/step_07 as step_07_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, WitnessFieldOpening, opened_certified_field_walk_from_grammar,
  opened_field_grammar_certification, resume_opened_field_grammar_certification,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_05 as step_05
use midgard/fraud_proofs/missing_native_script_utxo/step_06 as step_06
use midgard/fraud_proofs/missing_native_script_utxo/step_07 as step_07
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardVersionedScript,
  NativeCardanoScript, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{RawUtxo, encode_field_preimage}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, encode_field_grammar_checkpoint,
  encode_field_walk_checkpoint, field_grammar_checkpoint_hash,
  field_walk_checkpoint_hash, walk_fold,
}
use midgard/q33_max_fixture_dump as max_fixture
use midgard/script_proof_v1

const first_staged_count: Int = 65

const q33_maximum_count: Int = 224

fn scripts(count: Int) -> List<MidgardVersionedScript> {
  accumulate_scripts(count, [])
}

fn accumulate_scripts(
  remaining: Int,
  accumulated: List<MidgardVersionedScript>,
) -> List<MidgardVersionedScript> {
  if remaining <= 0 {
    accumulated
  } else {
    accumulate_scripts(
      remaining - 1,
      [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"8201828200581c"
            |> bytearray.concat(fixture.fraud_prover_key_hash)
            |> bytearray.concat(#"820519")
            |> bytearray.concat(
                bytearray.from_int_big_endian(20000 + remaining, 2),
              ),
        },
        ..accumulated
      ],
    )
  }
}

fn block(count: Int) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        script_tx_wits: scripts(count),
      },
    },
  )
}

fn items(count: Int) -> List<ByteArray> {
  list.map(scripts(count), encode_midgard_versioned_script)
}

fn absent_script_hash() -> ByteArray {
  script_proof_v1.versioned_script_hash(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"8201818200581c31313131313131313131313131313131313131313131",
    },
  )
}

fn ready_state(subject: fixture.NativeBlockFixtureV1) -> step_05.State {
  step_05.State {
    expected_missing_script_hash: absent_script_hash(),
    bad_tx_id: subject.tx_id,
    bad_tx_witness_set_hash: subject.compact.witness_set_hash,
    phase: step_05.Ready,
  }
}

fn opening(subject: fixture.NativeBlockFixtureV1, count: Int) {
  let encoded_items = items(count)
  if count == q33_maximum_count {
    WitnessFieldOpening {
      native_tx_compact_cbor: subject.compact_cbor,
      witness_set: subject.witness_set_compact,
      carriage: RawUtxo { ref_input_index: 0 },
    }
  } else {
    fixture.inline_witness_opening(
      subject.compact_cbor,
      subject.witness_set_compact,
      encoded_items,
    )
  }
}

fn carriage_reference_inputs(subject: fixture.NativeBlockFixtureV1, count: Int) {
  if count == q33_maximum_count {
    [
      Input {
        output_reference: OutputReference {
          transaction_id: subject.tx_id,
          output_index: 31,
        },
        output: Output {
          address: address.from_verification_key(fixture.fraud_prover_key_hash),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(
            builtin.b_data(encode_field_preimage(items(count))),
          ),
          reference_script: None,
        },
      },
    ]
  } else {
    []
  }
}

fn grammar_after(subject: fixture.NativeBlockFixtureV1, count: Int, budget: Int) {
  let Pair(view, start) = opened_field_grammar_certification(
    opening(subject, count),
    WitnessAnchor {
      tx_id: subject.tx_id,
      witness_set_hash: subject.compact.witness_set_hash,
    },
    script_witnesses_field_index,
    carriage_reference_inputs(subject, count),
    fixture.test_certificate_policy_id,
  )
  certify_field_grammar(view, start, budget)
}

fn continue_tx(
  subject: fixture.NativeBlockFixtureV1,
  count: Int,
  asset_name: ByteArray,
  output_state: Data,
  output_script_hash: ByteArray,
) -> Transaction {
  let tx = fixture.continue_mid_step_l1_tx_v1(asset_name, output_state)
  expect [output] = tx.outputs
  Transaction {
    ..tx,
    outputs: [
      Output { ..output, address: address.from_script(output_script_hash) },
    ],
    reference_inputs: carriage_reference_inputs(subject, count),
  }
}

fn max_opening() {
  WitnessFieldOpening {
    native_tx_compact_cbor: max_fixture.literal_compact_cbor_v1(),
    witness_set: NativeTxWitnessSetCompact {
      addr_tx_wits_hash: max_fixture.literal_addr_hash_v1(),
      script_tx_wits_hash: max_fixture.literal_script_hash_v1(),
      redeemer_tx_wits_hash: max_fixture.literal_redeemer_hash_v1(),
    },
    carriage: RawUtxo { ref_input_index: 0 },
  }
}

fn max_reference_inputs() -> List<Input> {
  [
    Input {
      output_reference: OutputReference {
        transaction_id: max_fixture.literal_tx_id_v1(),
        output_index: 31,
      },
      output: Output {
        address: address.from_verification_key(fixture.fraud_prover_key_hash),
        value: assets.from_lovelace(2_000_000),
        datum: InlineDatum(builtin.b_data(max_fixture.literal_preimage_v1())),
        reference_script: None,
      },
    },
  ]
}

fn max_state(phase: step_05.PhaseV1) -> step_05.State {
  step_05.State {
    expected_missing_script_hash: absent_script_hash(),
    bad_tx_id: max_fixture.literal_tx_id_v1(),
    bad_tx_witness_set_hash: max_fixture.literal_overall_witness_hash,
    phase,
  }
}

fn max_continue_tx(
  output_state: Data,
  output_script_hash: ByteArray,
) -> Transaction {
  let tx = fixture.continue_mid_step_l1_tx_v1(
    max_fixture.literal_tx_id_v1(),
    output_state,
  )
  expect [output] = tx.outputs
  Transaction {
    ..tx,
    outputs: [
      Output { ..output, address: address.from_script(output_script_hash) },
    ],
    reference_inputs: max_reference_inputs(),
  }
}

fn checkpoint_bytes(packed: ByteArray, byte_length: Int) -> ByteArray {
  bytearray.take(packed, byte_length)
}

fn checkpoint_hash(packed: ByteArray, byte_length: Int) -> ByteArray {
  bytearray.drop(packed, byte_length)
}

fn max_start_grammar() -> Bool {
  let packed = max_fixture.literal_grammar_32
  let output_state: Data = max_state(
    step_05.GrammarCertification {
      checkpoint_hash: checkpoint_hash(packed, 87),
    },
  )
  step_05_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(max_state(step_05.Ready)),
      },
    ),
    ct.Continue(
      step_05.StartGrammarCertification {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: max_opening(),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    max_continue_tx(output_state, fixture.next_step_script_hash),
  )
}

fn max_resume_grammar() -> Bool {
  let input_packed = max_fixture.literal_grammar_32
  let output_packed = max_fixture.literal_grammar_64
  let input_state = max_state(
    step_05.GrammarCertification {
      checkpoint_hash: checkpoint_hash(input_packed, 87),
    },
  )
  let output_state: Data = max_state(
    step_05.GrammarCertification {
      checkpoint_hash: checkpoint_hash(output_packed, 87),
    },
  )
  step_06_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_06.ResumeGrammarCertification {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: max_opening(),
        checkpoint_bytes: checkpoint_bytes(input_packed, 87),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    max_continue_tx(output_state, fixture.step_script_hash),
  )
}

fn max_start_semantic() -> Bool {
  let grammar = max_fixture.literal_grammar_224
  let semantic = max_fixture.literal_semantic_32
  let input_state = max_state(
    step_05.GrammarCertification {
      checkpoint_hash: checkpoint_hash(grammar, 87),
    },
  )
  let output_state: Data = max_state(
    step_05.SemanticScan {
      checkpoint_hash: checkpoint_hash(semantic, 53),
      required_script_is_present: False,
    },
  )
  step_06_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_06.StartSemanticScan {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: max_opening(),
        grammar_checkpoint_bytes: checkpoint_bytes(grammar, 87),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    max_continue_tx(output_state, fixture.next_step_script_hash),
  )
}

fn max_resume_semantic() -> Bool {
  let input_packed = max_fixture.literal_semantic_32
  let output_packed = max_fixture.literal_semantic_64
  let input_state = max_state(
    step_05.SemanticScan {
      checkpoint_hash: checkpoint_hash(input_packed, 53),
      required_script_is_present: False,
    },
  )
  let output_state: Data = max_state(
    step_05.SemanticScan {
      checkpoint_hash: checkpoint_hash(output_packed, 53),
      required_script_is_present: False,
    },
  )
  step_07_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_07.ResumeSemanticScan {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: max_opening(),
        checkpoint_bytes: checkpoint_bytes(input_packed, 53),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    max_continue_tx(output_state, fixture.step_script_hash),
  )
}

fn max_finalize_semantic() -> Bool {
  let semantic = max_fixture.literal_semantic_192
  let input_state = max_state(
    step_05.SemanticScan {
      checkpoint_hash: checkpoint_hash(semantic, 53),
      required_script_is_present: False,
    },
  )
  step_07_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_07.FinalizeSemanticScan {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_tx_wits_opening: max_opening(),
        checkpoint_bytes: checkpoint_bytes(semantic, 53),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_with_reference_inputs_v1(
      max_fixture.literal_tx_id_v1(),
      max_reference_inputs(),
    ),
  )
}

fn start_grammar(count: Int, output_script_hash: ByteArray) -> Bool {
  let subject = block(count)
  let state = ready_state(subject)
  let checkpoint = grammar_after(subject, count, step_05.staged_batch_limit)
  let output_state: Data = step_05.State {
    ..state,
    phase: step_05.GrammarCertification {
      checkpoint_hash: field_grammar_checkpoint_hash(checkpoint),
    },
  }
  step_05_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      step_05.StartGrammarCertification {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: opening(subject, count),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(
      subject,
      count,
      fixture.ct_token_asset_name(subject),
      output_state,
      output_script_hash,
    ),
  )
}

test q33_staged_step_05_starts_only_above_the_direct_limit() {
  start_grammar(first_staged_count, fixture.next_step_script_hash)
}

test q33_staged_step_05_rejects_a_direct_sized_field() fail {
  start_grammar(
    step_05.direct_script_witness_limit,
    fixture.next_step_script_hash,
  )
}

test q33_staged_step_05_rejects_the_wrong_successor() fail {
  start_grammar(first_staged_count, fixture.step_script_hash)
}

/// Applied max row: the exact 224-item Cardano witness boundary enters step07
/// after one bounded 32-envelope transaction.
test q33_staged_step_05_exact_maximum_fit() {
  max_start_grammar()
}

test q33_staged_step_06_exact_maximum_grammar_resume_fit() {
  max_resume_grammar()
}

fn resume_grammar_once(
  input_phase_is_ready: Bool,
  wrong_successor: Bool,
) -> Bool {
  let count = first_staged_count
  let subject = block(count)
  let first = grammar_after(subject, count, step_05.staged_batch_limit)
  let Pair(view, resumed) = resume_opened_field_grammar_certification(
    opening(subject, count),
    WitnessAnchor {
      tx_id: subject.tx_id,
      witness_set_hash: subject.compact.witness_set_hash,
    },
    script_witnesses_field_index,
    field_grammar_checkpoint_hash(first),
    encode_field_grammar_checkpoint(first),
    carriage_reference_inputs(subject, count),
    fixture.test_certificate_policy_id,
  )
  let next = certify_field_grammar(view, resumed, step_05.staged_batch_limit)
  let base_state = ready_state(subject)
  let input_state = step_05.State {
    ..base_state,
    phase: if input_phase_is_ready {
      step_05.Ready
    } else {
      step_05.GrammarCertification {
        checkpoint_hash: field_grammar_checkpoint_hash(first),
      }
    },
  }
  let output_state: Data = step_05.State {
    ..base_state,
    phase: step_05.GrammarCertification {
      checkpoint_hash: field_grammar_checkpoint_hash(next),
    },
  }
  step_06_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_06.ResumeGrammarCertification {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: opening(subject, count),
        checkpoint_bytes: encode_field_grammar_checkpoint(first),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(
      subject,
      count,
      fixture.ct_token_asset_name(subject),
      output_state,
      if wrong_successor {
        fixture.next_step_script_hash
      } else {
        fixture.step_script_hash
      },
    ),
  )
}

test q33_staged_step_06_resumes_grammar_only_at_step_06() {
  resume_grammar_once(False, False)
}

test q33_staged_step_06_rejects_ready_phase_skip() fail {
  resume_grammar_once(True, False)
}

test q33_staged_step_06_rejects_wrong_successor_during_grammar() fail {
  resume_grammar_once(False, True)
}

fn semantic_after(
  subject: fixture.NativeBlockFixtureV1,
  count: Int,
  budget: Int,
) {
  let terminal_grammar = grammar_after(subject, count, count)
  let Pair(view, start) = opened_certified_field_walk_from_grammar(
    opening(subject, count),
    WitnessAnchor {
      tx_id: subject.tx_id,
      witness_set_hash: subject.compact.witness_set_hash,
    },
    script_witnesses_field_index,
    field_grammar_checkpoint_hash(terminal_grammar),
    encode_field_grammar_checkpoint(terminal_grammar),
    carriage_reference_inputs(subject, count),
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = walk_fold(
    view,
    start,
    budget,
    False,
    fn(found, _index, item) {
      let Pair(_offset, script_wit) = decode_midgard_versioned_script_at(
        item,
        0,
      )
      expect encode_midgard_versioned_script(script_wit) == item
      found
    },
  )
  checkpoint
}

fn start_semantic_scan(
  count: Int,
  grammar_is_terminal: Bool,
  wrong_successor: Bool,
) -> Bool {
  let subject = block(count)
  let terminal_grammar = grammar_after(subject, count, count)
  let committed_grammar = if grammar_is_terminal {
    terminal_grammar
  } else {
    grammar_after(subject, count, step_05.staged_batch_limit)
  }
  let next = semantic_after(subject, count, step_05.staged_batch_limit)
  let base_state = ready_state(subject)
  let input_state = step_05.State {
    ..base_state,
    phase: step_05.GrammarCertification {
      checkpoint_hash: field_grammar_checkpoint_hash(committed_grammar),
    },
  }
  let output_state: Data = step_05.State {
    ..base_state,
    phase: step_05.SemanticScan {
      checkpoint_hash: field_walk_checkpoint_hash(next),
      required_script_is_present: False,
    },
  }
  step_06_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_06.StartSemanticScan {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: opening(subject, count),
        grammar_checkpoint_bytes: encode_field_grammar_checkpoint(
          committed_grammar,
        ),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(
      subject,
      count,
      fixture.ct_token_asset_name(subject),
      output_state,
      if wrong_successor {
        fixture.step_script_hash
      } else {
        fixture.next_step_script_hash
      },
    ),
  )
}

test q33_staged_step_06_terminal_grammar_starts_semantic_scan() {
  start_semantic_scan(first_staged_count, True, False)
}

test q33_staged_step_06_rejects_semantic_start_before_grammar_terminal() fail {
  start_semantic_scan(first_staged_count, False, False)
}

test q33_staged_step_06_rejects_wrong_semantic_successor() fail {
  start_semantic_scan(first_staged_count, True, True)
}

/// Applied maximum row for the grammar-to-semantics boundary: the exact
/// 224-item field starts with one bounded 32-item semantic batch.
test q33_staged_step_06_exact_maximum_semantic_start_fit() {
  max_start_semantic()
}

fn resume_semantic_scan(
  count: Int,
  input_phase_is_grammar: Bool,
  wrong_successor: Bool,
) -> Bool {
  let subject = block(count)
  let first = semantic_after(subject, count, step_05.staged_batch_limit)
  let next = semantic_after(subject, count, step_05.staged_batch_limit * 2)
  let base_state = ready_state(subject)
  let input_state = step_05.State {
    ..base_state,
    phase: if input_phase_is_grammar {
      step_05.GrammarCertification {
        checkpoint_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      }
    } else {
      step_05.SemanticScan {
        checkpoint_hash: field_walk_checkpoint_hash(first),
        required_script_is_present: False,
      }
    },
  }
  let output_state: Data = step_05.State {
    ..base_state,
    phase: step_05.SemanticScan {
      checkpoint_hash: field_walk_checkpoint_hash(next),
      required_script_is_present: False,
    },
  }
  step_07_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_07.ResumeSemanticScan {
        input_index: 0,
        output_index: 0,
        script_tx_wits_opening: opening(subject, count),
        checkpoint_bytes: encode_field_walk_checkpoint(first),
        item_budget: step_05.staged_batch_limit,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(
      subject,
      count,
      fixture.ct_token_asset_name(subject),
      output_state,
      if wrong_successor {
        fixture.next_step_script_hash
      } else {
        fixture.step_script_hash
      },
    ),
  )
}

test q33_staged_step_07_resumes_only_semantic_state_at_step_07() {
  resume_semantic_scan(first_staged_count, False, False)
}

test q33_staged_step_07_rejects_grammar_phase_skip() fail {
  resume_semantic_scan(first_staged_count, True, False)
}

test q33_staged_step_07_rejects_wrong_successor() fail {
  resume_semantic_scan(first_staged_count, False, True)
}

/// Applied maximum middle-row: a 224-item semantic scan advances by exactly
/// 32 items and stays at step08.
test q33_staged_step_07_exact_maximum_resume_fit() {
  max_resume_semantic()
}

fn finalize_semantic_scan(
  count: Int,
  already_scanned: Int,
  carried_present: Bool,
) -> Bool {
  let subject = block(count)
  let checkpoint = semantic_after(subject, count, already_scanned)
  let base_state = ready_state(subject)
  let input_state = step_05.State {
    ..base_state,
    phase: step_05.SemanticScan {
      checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
      required_script_is_present: carried_present,
    },
  }
  step_07_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_07.FinalizeSemanticScan {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_tx_wits_opening: opening(subject, count),
        checkpoint_bytes: encode_field_walk_checkpoint(checkpoint),
        item_budget: count - already_scanned,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_with_reference_inputs_v1(
      fixture.ct_token_asset_name(subject),
      carriage_reference_inputs(subject, count),
    ),
  )
}

test q33_staged_step_07_finalizes_only_at_semantic_terminal() {
  finalize_semantic_scan(first_staged_count, 64, False)
}

test q33_staged_step_07_rejects_a_valid_block_accumulator() fail {
  finalize_semantic_scan(first_staged_count, 64, True)
}

/// Applied maximum terminal row: the exact 224-item witness maximum finishes
/// from its authenticated 192-item checkpoint in one bounded batch.
test q33_staged_step_07_exact_maximum_terminal_fit() {
  max_finalize_semantic()
}

fn cancel_datum(
  subject: fixture.NativeBlockFixtureV1,
) -> Option<ct.StepDatum<step_05.State>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(ready_state(subject)),
    },
  )
}

test q33_staged_step_05_preserves_prover_cancellation() {
  let subject = block(first_staged_count)
  let asset_name = fixture.ct_token_asset_name(subject)
  step_05_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    cancel_datum(subject),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
  )
}

test q33_staged_step_06_preserves_prover_cancellation() {
  let subject = block(first_staged_count)
  let asset_name = fixture.ct_token_asset_name(subject)
  step_06_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    cancel_datum(subject),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
  )
}

test q33_staged_step_07_preserves_prover_cancellation() {
  let subject = block(first_staged_count)
  let asset_name = fixture.ct_token_asset_name(subject)
  step_07_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    cancel_datum(subject),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
  )
}
