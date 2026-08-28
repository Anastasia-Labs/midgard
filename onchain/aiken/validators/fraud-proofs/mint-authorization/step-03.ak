//// ## Mint-Authorization Fault Proof — Step 03 (W-C14)
////
//// The direction dispatch. Both arms read a committed witness-set field
//// through the §8.8 door anchored to the *thread-carried*
//// `bad_tx_witness_set_hash` — the anchor step-01 read off the
//// block-committed compact structure, which no redeemer can restate (the §3
//// id preimage is the body alone; see missing-signature step-04's
//// both-sides-forgery note).
////
//// **Direction A (script absent, inline half).** The machine's stage-nine
//// source surface for a mint purpose is exactly the field-6 script witnesses
//// plus the resolved reference inputs' reference scripts
//// (`replay_source_frontier`), matched by hash alone across every language.
//// This arm walks the whole committed field 6 and proves no item's
//// canonical versioned-script hash equals the claimed policy id, then routes
//// into step-04's reference-input scan for the other half.
////
//// **Direction B (script present but unsatisfied).** `script_bytes` is
//// pinned as the policy's native payload by
//// `blake2b_224(0x00 ‖ script_bytes) == policy_id`. Collision resistance
//// makes a source-membership proof unnecessary: if the machine consulted a
//// source with this hash, its bytes were these bytes (a Plutus source cannot
//// collide — the language tag byte differs inside the preimage), and the
//// machine's deterministic evaluation of them over the committed signer set
//// and validity interval is reproduced here by the machine-twin evaluator.
//// If the machine had *no* such source, acceptance was wrongful for the
//// stronger missing-witness reason — either way the conviction is sound, and
//// an honest operator can never be caught by it. A verdict other than
//// `ScriptEvaluatedV1 { satisfied: False }` refuses: a malformed or
//// limit-breaking payload is the decoding family's territory, not this
//// one's.

use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, address_witnesses_field_index, fold_opened_field,
  opened_field_walk, script_witnesses_field_index,
}
use midgard/fraud_proofs/mint_authorization/engine
use midgard/fraud_proofs/mint_authorization/step_03.{
  Args, Datum, EvaluateUnsatisfied, SpendRedeemer, State, WitnessAbsence,
}
use midgard/fraud_proofs/mint_authorization/step_04
use midgard/fraud_proofs/mint_authorization/step_05
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, decode_midgard_versioned_script_at,
  encode_midgard_address_witness, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardVersionedScript,
  NativeCardanoScript, PlutusV3Script,
}
use midgard/script_proof_v1

validator main(
  step_04_validator_script_hash: ScriptHash,
  step_05_validator_script_hash: ScriptHash,
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
      ct.Continue(WitnessAbsence {
        input_index,
        output_index,
        script_tx_wits_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        expect
            Some(State {
              policy_id,
              direction,
              bad_tx_id,
              bad_tx_witness_set_hash,
              validity_interval_start: _,
              validity_interval_end: _,
              prior_ledger_root,
            })
          = m_input_state_data
        expect direction == engine.direction_script_absent

        // 2. The whole committed field 6 is walked through the §8.8 door:
        //    no script witness of any language may hash to the claimed
        //    policy id. Re-encoding pins §6.1 canonicality (the decoder
        //    reads a prefix and tolerates non-minimal length prefixes; see
        //    missing-native-script-tx step-06).
        let script_tx_wits_walk = opened_field_walk(
          script_tx_wits_opening,
          WitnessAnchor {
            tx_id: bad_tx_id,
            witness_set_hash: bad_tx_witness_set_hash,
          },
          script_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let policy_script_is_present = fold_opened_field(
          script_tx_wits_walk,
          False,
          fn(found, _index, item) {
            let Pair(_offset, script_wit) = decode_midgard_versioned_script_at(
              item,
              0,
            )
            expect encode_midgard_versioned_script(script_wit) == item
            or {
              found,
              script_proof_v1.versioned_script_hash(script_wit) == policy_id,
            }
          },
        )
        expect policy_script_is_present == False

        // 3. The other half of the absence claim is the reference-input
        //    scan: route into step-04 at cursor 0.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          policy_id,
          bad_tx_id,
          prior_ledger_root,
          ref_cursor: 0,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
      ct.Continue(EvaluateUnsatisfied {
        input_index,
        output_index,
        script_bytes,
        addr_tx_wits_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        expect
            Some(State {
              policy_id,
              direction,
              bad_tx_id,
              bad_tx_witness_set_hash,
              validity_interval_start,
              validity_interval_end,
              prior_ledger_root: _,
            })
          = m_input_state_data
        expect direction == engine.direction_script_unsatisfied

        // 2. The supplied payload is THE policy's native script — pinned by
        //    the versioned hash (native tag byte 0x00 inside the preimage;
        //    see the module header for why no source-membership proof is
        //    needed or possible to evade).
        expect
          script_proof_v1.versioned_script_hash(
            MidgardVersionedScript {
              language: NativeCardanoScript,
              script_bytes,
            },
          ) == policy_id

        // 3. The committed signer set is the frontier the machine built:
        //    blake2b_224 of every field-7 verification key, walked through
        //    the §8.8 door under the thread-carried anchor.
        let addr_tx_wits_walk = opened_field_walk(
          addr_tx_wits_opening,
          WitnessAnchor {
            tx_id: bad_tx_id,
            witness_set_hash: bad_tx_witness_set_hash,
          },
          address_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let signer_hashes = fold_opened_field(
          addr_tx_wits_walk,
          [],
          fn(acc, _index, item) {
            [
              utils.get_verification_key_hash(
                decode_midgard_address_witness_cbor(item).verification_key,
              ),
              ..acc
            ]
          },
        )

        // 4. The machine-twin evaluator must find the policy unsatisfied.
        //    Any other verdict refuses: `satisfied: True` is an honest
        //    acceptance, and a malformed or limit-breaking payload is the
        //    decoding family's claim, not this one's.
        expect engine.ScriptEvaluatedV1 { satisfied: False } = engine.evaluate_native_script_v1(
          script_bytes,
          signer_hashes,
          validity_interval_start,
          validity_interval_end,
        )

        // 5. The claim is decided: close to the terminal step.
        expect output_script_hash == step_05_validator_script_hash
        let expected_output_state: Data = step_05.State {
          policy_id,
          direction: engine.direction_script_unsatisfied,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

/// A 28-byte policy id unrelated to any script in the fixture witness set.
const accused_policy_id: ByteArray =
  #"42424242424242424242424242424242424242424242424242424242"

fn native_policy_of(script_bytes: ByteArray) -> ByteArray {
  script_proof_v1.versioned_script_hash(
    MidgardVersionedScript { language: NativeCardanoScript, script_bytes },
  )
}

/// `sig` node for a 28-byte key hash.
fn sig_script(key_hash: ByteArray) -> ByteArray {
  #"8200581c" |> bytearray.concat(key_hash)
}

/// The key hash the fixture's single address witness actually signs with.
fn present_key() -> ByteArray {
  utils.get_verification_key_hash(fixture.sample_witness_verification_key())
}

/// A key hash no fixture witness signs with.
fn absent_key() -> ByteArray {
  tf.h28_y
}

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn state_of(
  block: fixture.NativeBlockFixtureV1,
  policy_id: ByteArray,
  direction: Int,
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> State {
  State {
    policy_id,
    direction,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    validity_interval_start,
    validity_interval_end,
    prior_ledger_root: tf.h32_c,
  }
}

fn run_arm(
  block: fixture.NativeBlockFixtureV1,
  state: State,
  args: Args,
  expected_output_state: Data,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

// ### Direction A — inline absence walk

fn run_witness_absence(
  block: fixture.NativeBlockFixtureV1,
  policy_id: ByteArray,
  direction: Int,
  carried_items: List<ByteArray>,
) -> Bool {
  let state = state_of(
    block,
    policy_id,
    direction,
    block.compact.body.validity_interval_start,
    block.compact.body.validity_interval_end,
  )
  let expected_output_state: Data = step_04.State {
    policy_id,
    bad_tx_id: block.tx_id,
    prior_ledger_root: tf.h32_c,
    ref_cursor: 0,
  }
  run_arm(
    block,
    state,
    WitnessAbsence {
      input_index: 0,
      output_index: 0,
      script_tx_wits_opening: fixture.inline_witness_opening(
        block.compact_cbor,
        block.witness_set_compact,
        carried_items,
      ),
    },
    expected_output_state,
  )
}

fn honest_field_6_items(block: fixture.NativeBlockFixtureV1) -> List<ByteArray> {
  honest_field_6_items_of(block.tx.witness_set.script_tx_wits)
}

fn honest_field_6_items_of(
  wits: List<MidgardVersionedScript>,
) -> List<ByteArray> {
  when wits is {
    [] -> []
    [wit, ..rest] ->
      [encode_midgard_versioned_script(wit), ..honest_field_6_items_of(rest)]
  }
}

/// Direction-A positive: no field-6 witness hashes to the claimed policy id,
/// so the inline half of the absence claim holds and the thread routes into
/// the reference-input scan at cursor 0.
test mint_authorization_step_03_clears_an_absent_inline_witness() {
  let block = challenged_block()
  run_witness_absence(
    block,
    accused_policy_id,
    engine.direction_script_absent,
    honest_field_6_items(block),
  )
}

/// A field-6 native witness that IS the claimed policy refuses the absence
/// arm: the machine had the source, so the accusation is false.
test mint_authorization_step_03_rejects_absence_when_the_policy_is_inline() fail {
  let block = challenged_block()
  run_witness_absence(
    block,
    native_policy_of(fixture.sample_native_script_bytes()),
    engine.direction_script_absent,
    honest_field_6_items(block),
  )
}

/// Stage nine matches sources by hash across every language: a PlutusV3
/// witness whose versioned hash equals the claimed policy id also refuses the
/// absence arm.
test mint_authorization_step_03_rejects_absence_when_a_plutus_source_matches() fail {
  let plutus_wit = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"deadbeef",
  }
  let base = fixture.valid_native_tx_v1()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [plutus_wit],
    },
  }
  let block = fixture.native_block_fixture_v1(tx)
  run_witness_absence(
    block,
    script_proof_v1.versioned_script_hash(plutus_wit),
    engine.direction_script_absent,
    honest_field_6_items(block),
  )
}

/// The absence claim is only as good as the walked preimage: a truncated
/// field-6 carriage fails the door's authenticate-once check.
test mint_authorization_step_03_rejects_a_truncated_field_6_preimage() fail {
  let block = challenged_block()
  run_witness_absence(
    block,
    native_policy_of(fixture.sample_native_script_bytes()),
    engine.direction_script_absent,
    [],
  )
}

/// The arm demands its own direction: a direction-B claim cannot take the
/// absence route.
test mint_authorization_step_03_rejects_absence_under_direction_b() fail {
  let block = challenged_block()
  run_witness_absence(
    block,
    accused_policy_id,
    engine.direction_script_unsatisfied,
    honest_field_6_items(block),
  )
}

// ### Direction B — machine-twin refutation

fn run_evaluate_with_interval(
  policy_id: ByteArray,
  direction: Int,
  script_bytes: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  carried_witness_items: List<ByteArray>,
) -> Bool {
  let block = challenged_block()
  let state = state_of(
    block,
    policy_id,
    direction,
    validity_interval_start,
    validity_interval_end,
  )
  let expected_output_state: Data = step_05.State {
    policy_id,
    direction: engine.direction_script_unsatisfied,
  }
  run_arm(
    block,
    state,
    EvaluateUnsatisfied {
      input_index: 0,
      output_index: 0,
      script_bytes,
      addr_tx_wits_opening: fixture.inline_witness_opening(
        block.compact_cbor,
        block.witness_set_compact,
        carried_witness_items,
      ),
    },
    expected_output_state,
  )
}

fn honest_field_7_items() -> List<ByteArray> {
  [encode_midgard_address_witness(fixture.sample_address_witness())]
}

fn run_evaluate(script_bytes: ByteArray) -> Bool {
  run_evaluate_with_interval(
    native_policy_of(script_bytes),
    engine.direction_script_unsatisfied,
    script_bytes,
    env.posix_time_none,
    env.posix_time_none,
    honest_field_7_items(),
  )
}

/// Direction-B positive: the policy demands a key nobody signed with, so the
/// machine-twin evaluation refutes it and the thread closes to step-05.
test mint_authorization_step_03_convicts_an_unsigned_sig_policy() {
  run_evaluate(sig_script(absent_key()))
}

/// `all` with one unmet branch is unsatisfied: conviction.
test mint_authorization_step_03_convicts_a_failed_all_policy() {
  let payload = #"820182"
    |> bytearray.concat(sig_script(present_key()))
    |> bytearray.concat(sig_script(absent_key()))
  run_evaluate(payload)
}

/// The empty `any` container is unsatisfiable by the machine's own
/// `empty_container_result_v1`: conviction.
test mint_authorization_step_03_convicts_an_empty_any_policy() {
  run_evaluate(#"820280")
}

/// A timelock the committed validity interval does not meet is unsatisfied:
/// `after 15` against `validity_interval_start = 10` convicts.
test mint_authorization_step_03_convicts_an_unmet_after_policy() {
  let payload = #"82040f"
  run_evaluate_with_interval(
    native_policy_of(payload),
    engine.direction_script_unsatisfied,
    payload,
    10,
    20,
    honest_field_7_items(),
  )
}

/// A satisfied `sig` policy refuses: the operator authorized honestly.
test mint_authorization_step_03_rejects_a_satisfied_sig_policy() fail {
  run_evaluate(sig_script(present_key()))
}

/// `any` with one met branch is satisfied: refuse.
test mint_authorization_step_03_rejects_a_satisfied_any_policy() fail {
  let payload = #"820282"
    |> bytearray.concat(sig_script(absent_key()))
    |> bytearray.concat(sig_script(present_key()))
  run_evaluate(payload)
}

/// `at_least 1` with one met branch is satisfied: refuse.
test mint_authorization_step_03_rejects_a_satisfied_at_least_policy() fail {
  let payload = #"83030182"
    |> bytearray.concat(sig_script(absent_key()))
    |> bytearray.concat(sig_script(present_key()))
  run_evaluate(payload)
}

/// A satisfied conjunction of a signature and a met `before` timelock
/// refuses (`before 30` against `validity_interval_end = 20`).
test mint_authorization_step_03_rejects_a_satisfied_all_with_before() fail {
  let payload = #"820182"
    |> bytearray.concat(sig_script(present_key()))
    |> bytearray.concat(#"8205181e")
  run_evaluate_with_interval(
    native_policy_of(payload),
    engine.direction_script_unsatisfied,
    payload,
    10,
    20,
    honest_field_7_items(),
  )
}

/// A met `after` timelock refuses (`after 5` against start 10).
test mint_authorization_step_03_rejects_a_satisfied_after_policy() fail {
  let payload = #"820405"
  run_evaluate_with_interval(
    native_policy_of(payload),
    engine.direction_script_unsatisfied,
    payload,
    10,
    20,
    honest_field_7_items(),
  )
}

/// The empty `all` container is vacuously satisfied by the machine's own
/// rule: refuse.
test mint_authorization_step_03_rejects_an_empty_all_policy() fail {
  run_evaluate(#"820180")
}

/// The decoding boundary: a payload the machine would refuse as malformed
/// (unknown tag 7) is the decoding family's claim, not an unsatisfied
/// policy — the arm refuses rather than convicts.
test mint_authorization_step_03_rejects_a_malformed_policy_payload() fail {
  run_evaluate(#"820700")
}

/// The hash pin: script bytes that do not hash to the claimed policy id are
/// not the policy, whatever they evaluate to.
test mint_authorization_step_03_rejects_a_mismatched_script_hash() fail {
  run_evaluate_with_interval(
    accused_policy_id,
    engine.direction_script_unsatisfied,
    sig_script(absent_key()),
    env.posix_time_none,
    env.posix_time_none,
    honest_field_7_items(),
  )
}

/// The signer frontier is only as good as the walked preimage: a truncated
/// field-7 carriage fails the door's authenticate-once check even though it
/// would only strengthen the conviction.
test mint_authorization_step_03_rejects_a_truncated_field_7_preimage() fail {
  run_evaluate_with_interval(
    native_policy_of(sig_script(absent_key())),
    engine.direction_script_unsatisfied,
    sig_script(absent_key()),
    env.posix_time_none,
    env.posix_time_none,
    [],
  )
}

/// The arm demands its own direction: a direction-A claim cannot take the
/// evaluation route.
test mint_authorization_step_03_rejects_evaluation_under_direction_a() fail {
  run_evaluate_with_interval(
    native_policy_of(sig_script(absent_key())),
    engine.direction_script_absent,
    sig_script(absent_key()),
    env.posix_time_none,
    env.posix_time_none,
    honest_field_7_items(),
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = challenged_block()
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          state_of(
            block,
            accused_policy_id,
            engine.direction_script_absent,
            env.posix_time_none,
            env.posix_time_none,
          ),
        ),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread.
test mint_authorization_step_03_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test mint_authorization_step_03_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
