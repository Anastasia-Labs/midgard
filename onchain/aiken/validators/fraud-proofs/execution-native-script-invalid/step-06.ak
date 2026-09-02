use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/execution_native_script_invalid/step_06.{
  Args, Datum, FinalizeScriptScan, PhaseV1, ResumeScriptScan, ScriptReady,
  ScriptWalk, SignerQueryV1, SpendRedeemer, StartScriptFinalize, StartScriptScan,
  State, staged_node_batch_limit,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
}
use midgard/native_tx_script_pushdown_v1 as pushdown
use midgard/script_proof_v1
use midgard/validation_machine_v1 as validation_machine
use midgard/validation_merkle_v1

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(action) -> {
        expect Some(step_datum) = datum
        when action is {
          StartScriptScan { input_index, output_index, .. } |
          ResumeScriptScan { input_index, output_index, .. } -> {
            let Transaction { inputs, outputs, .. } = tx
            let
                own_script_hash,
                _thread_asset_name,
                _fraud_prover,
                m_input_state_data,
                output_script_hash,
                output_state_data
              <- continue(
              computation_thread_token_policy_id,
              step_datum,
              input_index,
              output_index,
              own_out_ref,
              inputs,
              outputs,
            )
            expect Some(state) = m_input_state_data
            let next = run_action(state, action)
            expect !pushdown.native_script_walk_is_complete(next)
            expect output_script_hash == own_script_hash
            let expected_output_state: Data = State {
              ..state,
              phase: ScriptWalk {
                cursor_hash: pushdown.native_script_cursor_hash(next),
              },
            }
            expect output_state_data == expected_output_state
            True
          }
          StartScriptFinalize {
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            ..
          } |
          FinalizeScriptScan {
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            ..
          } -> {
            let Transaction { inputs, outputs, redeemers, .. } = tx
            let
                _own_script_hash,
                _thread_asset_name,
                _fraud_prover,
                m_input_state_data
              <- finalize(
              computation_thread_token_policy_id,
              fraud_proof_token_policy_id,
              fraud_proof_token_address,
              step_datum,
              input_index,
              output_index,
              fraud_proof_mint_redeemer_index,
              own_out_ref,
              inputs,
              outputs,
              redeemers,
            )
            expect Some(state) = m_input_state_data
            let terminal = run_action(state, action)
            expect pushdown.native_script_walk_is_complete(terminal)
            expect Some(satisfied) = pushdown.native_script_verdict(terminal)
            expect satisfied == ( state.direction == 1 )
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_action(state: State, action: Args) {
  let script_item_cbor = script_item_of(action)
  expect blake2b_256(script_item_cbor) == state.script_item_hash
  let Pair(offset, script) = decode_midgard_versioned_script_at(
    script_item_cbor,
    0,
  )
  expect offset == bytearray.length(script_item_cbor)
  expect encode_midgard_versioned_script(script) == script_item_cbor
  expect MidgardVersionedScript { language: NativeCardanoScript, script_bytes }
    = script
  let node_budget = node_budget_of(action)
  expect node_budget > 0
  expect node_budget <= staged_node_batch_limit
  let start = when action is {
    StartScriptScan { .. } | StartScriptFinalize { .. } -> {
      expect ScriptReady = state.phase
      pushdown.open_native_script_walk(script_bytes)
    }
    ResumeScriptScan { cursor_bytes, frames, .. } |
    FinalizeScriptScan { cursor_bytes, frames, .. } -> {
      expect ScriptWalk { cursor_hash } = state.phase
      pushdown.resume_native_script_walk_from_commitment(
        cursor_hash,
        cursor_bytes,
        frames,
        script_bytes,
      )
    }
  }
  let signer_queries = signer_queries_of(action)
  pushdown.native_script_run_with_signer_verdict(
    start,
    script_bytes,
    state.validity_interval_start,
    state.validity_interval_end,
    node_budget,
    fn(signer_hash) {
      authenticated_signer_verdict(state, signer_queries, signer_hash)
    },
  )
}

fn script_item_of(action: Args) -> ByteArray {
  when action is {
    StartScriptScan { script_item_cbor, .. } -> script_item_cbor
    ResumeScriptScan { script_item_cbor, .. } -> script_item_cbor
    StartScriptFinalize { script_item_cbor, .. } -> script_item_cbor
    FinalizeScriptScan { script_item_cbor, .. } -> script_item_cbor
  }
}

fn node_budget_of(action: Args) -> Int {
  when action is {
    StartScriptScan { node_budget, .. } -> node_budget
    ResumeScriptScan { node_budget, .. } -> node_budget
    StartScriptFinalize { node_budget, .. } -> node_budget
    FinalizeScriptScan { node_budget, .. } -> node_budget
  }
}

fn signer_queries_of(action: Args) -> List<SignerQueryV1> {
  when action is {
    StartScriptScan { signer_queries, .. } -> signer_queries
    ResumeScriptScan { signer_queries, .. } -> signer_queries
    StartScriptFinalize { signer_queries, .. } -> signer_queries
    FinalizeScriptScan { signer_queries, .. } -> signer_queries
  }
}

fn authenticated_signer_verdict(
  state: State,
  queries: List<SignerQueryV1>,
  signer_hash: ByteArray,
) -> Bool {
  expect bytearray.length(signer_hash) == 28
  when queries is {
    [] -> fail
    [query, ..rest] ->
      if query.signer_hash == signer_hash {
        signer_proof_verdict(
          signer_hash,
          state.signer_count,
          state.signer_peaks,
          query.proof,
        )
      } else {
        authenticated_signer_verdict(state, rest, signer_hash)
      }
  }
}

fn signer_frontier_matches(
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  and {
    validation_merkle_v1.frontier_is_well_formed(signer_count, peaks),
    validation_merkle_v1.frontier_commitment(signer_count, peaks) == signer_frontier_commitment,
  }
}

fn signer_membership_is_valid(
  signer_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_index: Int,
  siblings: List<ByteArray>,
) -> Bool {
  and {
    bytearray.length(signer_hash) == 28,
    signer_frontier_matches(signer_count, signer_frontier_commitment, peaks),
    validation_merkle_v1.verify_membership(
      signer_count,
      peaks,
      signer_index,
      script_proof_v1.signer_leaf_hash(signer_hash),
      siblings,
    ),
  }
}

fn signer_proof_verdict(
  signer_hash: ByteArray,
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  proof: validation_machine.SignerSetProofV1,
) -> Bool {
  let commitment = validation_merkle_v1.frontier_commitment(
    signer_count,
    signer_peaks,
  )
  when proof is {
    validation_machine.SignerMembershipProof { peaks, signer_index, siblings } -> {
      expect peaks == signer_peaks
      expect
        signer_membership_is_valid(
          signer_hash,
          signer_count,
          commitment,
          peaks,
          signer_index,
          siblings,
        )
      True
    }
    validation_machine.EmptySignerSetProof { peaks } -> {
      expect signer_count == 0
      expect signer_frontier_matches(signer_count, commitment, peaks)
      False
    }
    validation_machine.SignerBelowFirstProof {
      peaks,
      first_signer_hash,
      siblings,
    } -> {
      expect bytearray.compare(signer_hash, first_signer_hash) == Less
      expect
        signer_membership_is_valid(
          first_signer_hash,
          signer_count,
          commitment,
          peaks,
          0,
          siblings,
        )
      False
    }
    validation_machine.SignerAboveLastProof {
      peaks,
      last_signer_hash,
      siblings,
    } -> {
      expect signer_count > 0
      expect bytearray.compare(last_signer_hash, signer_hash) == Less
      expect
        signer_membership_is_valid(
          last_signer_hash,
          signer_count,
          commitment,
          peaks,
          signer_count - 1,
          siblings,
        )
      False
    }
    validation_machine.SignerBetweenProof {
      peaks,
      lower_index,
      lower_signer_hash,
      lower_siblings,
      upper_signer_hash,
      upper_siblings,
    } -> {
      expect lower_index >= 0
      expect lower_index + 1 < signer_count
      expect bytearray.compare(lower_signer_hash, signer_hash) == Less
      expect bytearray.compare(signer_hash, upper_signer_hash) == Less
      expect
        signer_membership_is_valid(
          lower_signer_hash,
          signer_count,
          commitment,
          peaks,
          lower_index,
          lower_siblings,
        )
      expect
        signer_membership_is_valid(
          upper_signer_hash,
          signer_count,
          commitment,
          peaks,
          lower_index + 1,
          upper_siblings,
        )
      False
    }
    validation_machine.NoSignerSetProof -> fail
  }
}

const present_signer_hash: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

fn one_signer_peaks() -> List<validation_merkle_v1.FrontierPeak> {
  validation_merkle_v1.append_leaf(
    0,
    validation_merkle_v1.empty_frontier(),
    script_proof_v1.signer_leaf_hash(present_signer_hash),
  )
}

fn state_for(script_item_cbor: ByteArray, phase: PhaseV1) -> State {
  State {
    direction: 0,
    execution_index: 0,
    source_index: 0,
    origin_kind: 0,
    item_commitment: blake2b_256(script_item_cbor),
    bad_tx_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    script_item_hash: blake2b_256(script_item_cbor),
    validity_interval_start: -1,
    validity_interval_end: -1,
    signer_count: 1,
    signer_peaks: one_signer_peaks(),
    phase,
  }
}

fn missing_query() -> SignerQueryV1 {
  SignerQueryV1 {
    signer_hash: tf.signer_key_hash,
    proof: validation_machine.SignerAboveLastProof {
      peaks: one_signer_peaks(),
      last_signer_hash: present_signer_hash,
      siblings: [],
    },
  }
}

fn present_query() -> SignerQueryV1 {
  SignerQueryV1 {
    signer_hash: present_signer_hash,
    proof: validation_machine.SignerMembershipProof {
      peaks: one_signer_peaks(),
      signer_index: 0,
      siblings: [],
    },
  }
}

fn signature_item(key_hash: ByteArray) -> ByteArray {
  #"820058208200581c" |> bytearray.concat(key_hash)
}

fn finalize_signature(
  script_item_cbor: ByteArray,
  signer_queries: List<SignerQueryV1>,
) -> Bool {
  let state = state_for(script_item_cbor, ScriptReady)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      StartScriptFinalize {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_item_cbor,
        node_budget: 1,
        signer_queries,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(state.bad_tx_id),
  )
}

test execution_native_script_invalid_step_06_finalizes_authenticated_absence() {
  finalize_signature(tf.signature_script_item_v1(), [missing_query()])
}

test execution_native_script_invalid_step_06_rejects_a_satisfied_script() fail {
  finalize_signature(signature_item(present_signer_hash), [present_query()])
}

test execution_native_script_invalid_step_06_rejects_a_missing_signer_query() fail {
  finalize_signature(tf.signature_script_item_v1(), [])
}

test execution_native_script_invalid_step_06_rejects_a_forged_absence_boundary() fail {
  let forged = SignerQueryV1 {
    signer_hash: tf.signer_key_hash,
    proof: validation_machine.SignerAboveLastProof {
      peaks: one_signer_peaks(),
      last_signer_hash: #"22222222222222222222222222222222222222222222222222222222",
      siblings: [],
    },
  }
  finalize_signature(tf.signature_script_item_v1(), [forged])
}

fn script_payload(script_item_cbor: ByteArray) -> ByteArray {
  let Pair(_offset, script) = decode_midgard_versioned_script_at(
    script_item_cbor,
    0,
  )
  expect MidgardVersionedScript { script_bytes, .. } = script
  script_bytes
}

fn continue_script(input_state: State, action: Args, expected_walk) -> Bool {
  let output_state: Data = State {
    ..input_state,
    phase: ScriptWalk {
      cursor_hash: pushdown.native_script_cursor_hash(expected_walk),
    },
  }
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(action),
    fixture.own_out_ref(),
    fixture.continue_same_step_l1_tx_v1(input_state.bad_tx_id, output_state),
  )
}

fn start_compound_scan() {
  let item = tf.all_of_two_signatures_item_v1()
  let payload = script_payload(item)
  let walk = pushdown.native_script_run_with_signer_verdict(
    pushdown.open_native_script_walk(payload),
    payload,
    -1,
    -1,
    1,
    fn(_key_hash) { False },
  )
  let state = state_for(item, ScriptReady)
  (state, item, walk)
}

test execution_native_script_invalid_step_06_starts_a_resumable_pushdown() {
  let (state, item, walk) = start_compound_scan()
  continue_script(
    state,
    StartScriptScan {
      input_index: 0,
      output_index: 0,
      script_item_cbor: item,
      node_budget: 1,
      signer_queries: [],
    },
    walk,
  )
}

test execution_native_script_invalid_step_06_resumes_the_committed_cursor() {
  let (ready, item, first) = start_compound_scan()
  let payload = script_payload(item)
  let input_state = State {
    ..ready,
    phase: ScriptWalk { cursor_hash: pushdown.native_script_cursor_hash(first) },
  }
  let next = pushdown.native_script_run_with_signer_verdict(
    first,
    payload,
    -1,
    -1,
    1,
    fn(_key_hash) { False },
  )
  continue_script(
    input_state,
    ResumeScriptScan {
      input_index: 0,
      output_index: 0,
      script_item_cbor: item,
      cursor_bytes: pushdown.encode_native_script_cursor(first),
      frames: pushdown.native_script_walk_frames(first),
      node_budget: 1,
      signer_queries: [missing_query()],
    },
    next,
  )
}

test execution_native_script_invalid_step_06_rejects_a_mutated_cursor() fail {
  let (ready, item, first) = start_compound_scan()
  let input_state = State {
    ..ready,
    phase: ScriptWalk {
      cursor_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    },
  }
  continue_script(
    input_state,
    ResumeScriptScan {
      input_index: 0,
      output_index: 0,
      script_item_cbor: item,
      cursor_bytes: pushdown.encode_native_script_cursor(first),
      frames: pushdown.native_script_walk_frames(first),
      node_budget: 1,
      signer_queries: [missing_query()],
    },
    first,
  )
}
