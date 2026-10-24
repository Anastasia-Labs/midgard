use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_224, verify_ed25519_signature}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, WitnessAnchor, opened_field_walk, resume_opened_field_walk,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionWitnessSet,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  ProtectedCredentialV1, VerdictV1, WitnessScanV1, address_witnesses_field_index,
  witness_scan_batch_size,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_04.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, encode_field_walk_checkpoint,
  field_walk_checkpoint_hash, walk_fold, walk_remaining,
}
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn valid_target_fold(
  target: ByteArray,
  transaction_id: ByteArray,
) -> fn(Bool, Int, ByteArray) -> Bool {
  fn(found, _index, item) {
    let witness = decode_midgard_address_witness_cbor(item)
    or {
      found,
      and {
        blake2b_224(witness.verification_key) == target,
        verify_ed25519_signature(
          witness.verification_key,
          transaction_id,
          witness.signature,
        ),
      },
    }
  }
}

const test_tx_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const test_vkey =
  #"ea4a6c63e29c520abef5507b132ec5f9954776aebebe7b92421eea691446d22c"

const test_signature =
  #"013f9d903c1a0a90b0beea2534582e2bb694712503215236622851f5afd54ad5f2a89983965e192edd928d484bdb1e1e521ba704fbe37968371a11597363fb0d"

fn test_witness(signature: ByteArray) -> ByteArray {
  encode_midgard_address_witness(
    MidgardAddressWitness { verification_key: test_vkey, signature },
  )
}

test protected_output_valid_signature_enters_frontier() {
  valid_target_fold(blake2b_224(test_vkey), test_tx_id)(
    False,
    0,
    test_witness(test_signature),
  )
}

test protected_output_invalid_signature_is_excluded() {
  !valid_target_fold(blake2b_224(test_vkey), test_tx_id)(
    False,
    0,
    test_witness(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
  )
}

test protected_output_wrong_credential_is_excluded() {
  !valid_target_fold(
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    test_tx_id,
  )(
    False,
    0,
    test_witness(test_signature),
  )
}

validator main(
  step_05_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, opening, checkpoint_cbor }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _name, _prover, prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = prior
        let protected = state.protected
        let Pair(view, checkpoint) = resume_opened_field_walk(
          opening,
          WitnessAnchor {
            tx_id: protected.transaction_id,
            witness_set_hash: protected.witness_set_hash,
            source_kind: protected.subject.source_kind,
          },
          address_witnesses_field_index,
          state.checkpoint_hash,
          checkpoint_cbor,
          reference_inputs,
          certificate_policy,
        )
        let batch = minimum(witness_scan_batch_size, walk_remaining(checkpoint))
        let Pair(found, next_checkpoint) = walk_fold(
          view,
          checkpoint,
          batch,
          state.signer_present,
          valid_target_fold(
            protected.payment_credential,
            protected.transaction_id,
          ),
        )
        if walk_remaining(next_checkpoint) == 0 {
          let expected: Data = VerdictV1 {
            subject: protected.subject,
            signer_required: True,
            signer_present: found,
          }
          and {
            next_hash == step_05_hash,
            next_data == expected,
          }
        } else {
          let expected: Data = WitnessScanV1 {
            ..state,
            checkpoint_hash: field_walk_checkpoint_hash(next_checkpoint),
            signer_present: found,
          }
          and {
            next_hash == own_hash,
            next_data == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

// ### Scan selectors
//
// Field 7 of the fixture block holds `count` copies of the fixture's sample
// witness, whose signature does not verify: no scan over it ever admits a
// signer, so the only honest terminal is `signer_present = False` after the
// last position. Every real signature the walk admits is exercised by the
// Lucid lifecycle, where the transaction id is signable, and by the fold
// selectors above, where the message is a fixed vector.

const test_payment_credential =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// One witness past the last exact batch, so the walk loops once and
/// terminates on a one-item suffix.
const looping_witness_count: Int = witness_scan_batch_size + 1

fn witness_items(count: Int) -> List<ByteArray> {
  list.repeat(
    encode_midgard_address_witness(fixture.sample_address_witness()),
    count,
  )
}

fn block_with_witnesses(count: Int) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        addr_tx_wits: list.repeat(fixture.sample_address_witness(), count),
      },
    },
  )
}

fn subject_of(
  block: fixture.NativeBlockFixtureV1,
  direction: Int,
) -> substrate.VerdictSubjectV1 {
  if direction == substrate.direction_wrongful_acceptance {
    substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    }
  } else {
    substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction,
      source_kind: substrate.source_kind_forced,
      transaction_id: block.tx_id,
      source_key: #"81",
      rejection_reason: Some(ProtectedOutputSignerMissing { output_index: 0 }),
    }
  }
}

fn protected_of(
  block: fixture.NativeBlockFixtureV1,
  direction: Int,
) -> ProtectedCredentialV1 {
  ProtectedCredentialV1 {
    subject: subject_of(block, direction),
    transaction_id: block.tx_id,
    witness_set_hash: block.compact.witness_set_hash,
    output_index: 0,
    payment_credential: test_payment_credential,
  }
}

fn inline_opening(
  block: fixture.NativeBlockFixtureV1,
  count: Int,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    witness_items(count),
  )
}

/// The position after `taken` items, derived exactly as step 03 and this
/// validator derive it.
fn walk_at(
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  taken: Int,
) -> FieldWalkCheckpointV1 {
  let Pair(view, start) = opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: block.tx_id,
      witness_set_hash: block.compact.witness_set_hash,
      source_kind: 0,
    },
    address_witnesses_field_index,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = walk_fold(
    view,
    start,
    taken,
    False,
    fn(found, _index, _item) { found },
  )
  checkpoint
}

fn scan_of(
  block: fixture.NativeBlockFixtureV1,
  direction: Int,
  checkpoint: FieldWalkCheckpointV1,
) -> WitnessScanV1 {
  WitnessScanV1 {
    protected: protected_of(block, direction),
    checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
    signer_present: False,
  }
}

fn verdict_of(
  block: fixture.NativeBlockFixtureV1,
  direction: Int,
  signer_present: Bool,
) -> Data {
  let value: Data = VerdictV1 {
    subject: subject_of(block, direction),
    signer_required: True,
    signer_present,
  }
  value
}

fn run_step_04(
  scan: WitnessScanV1,
  opening: FieldOpeningV1,
  checkpoint_cbor: ByteArray,
  tx: Transaction,
) -> Bool {
  let opening = if scan.protected.subject.source_kind == 1 {
    forced_fixture.from_native_opening(opening)
  } else {
    opening
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(scan),
      },
    ),
    ct.Continue(
      Args { input_index: 0, output_index: 0, opening, checkpoint_cbor },
    ),
    fixture.own_out_ref(),
    tx,
  )
}

fn loop_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
) -> Transaction {
  Transaction {
    ..fixture.continue_same_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    reference_inputs: [],
  }
}

fn handoff_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    reference_inputs: [],
  }
}

const accepted: Int = substrate.direction_wrongful_acceptance

const forced: Int = substrate.direction_wrongful_rejection

/// A single invalid witness exhausts the field: the frontier stays empty and
/// the thread terminates at step 05 with `signer_present = False`.
test protected_output_step_04_exhausts_an_invalid_witness_to_signer_absent() {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// The scan is direction-blind: a forced wrongful-rejection subject walks the
/// same field to the same terminal, and only step 05 reads the polarity.
test protected_output_step_04_exhausts_a_forced_subject_to_signer_absent() {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, forced, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, forced, False)),
  )
}

/// An empty witness field terminates on its first transaction: nothing to
/// scan, nothing admitted.
test protected_output_step_04_terminates_an_empty_witness_field() {
  let block = block_with_witnesses(0)
  let opening = inline_opening(block, 0)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// One witness past a full batch: the first transaction consumes exactly one
/// batch and self-loops with only the next checkpoint digest in state.
test protected_output_step_04_loops_after_a_full_batch() {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  let next = walk_at(block, opening, witness_scan_batch_size)
  expect walk_remaining(next) == 1
  let looped: Data = scan_of(block, accepted, next)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    loop_tx(block, looped),
  )
}

/// The resumed suffix terminates from the committed batch boundary.
test protected_output_step_04_terminates_the_resumed_suffix() {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let resumed = walk_at(block, opening, witness_scan_batch_size)
  run_step_04(
    scan_of(block, accepted, resumed),
    opening,
    encode_field_walk_checkpoint(resumed),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// Checkpoint: bytes for a position the thread never committed do not hash to
/// its digest, so a skipped batch cannot be resumed past.
test protected_output_step_04_refuses_a_substituted_checkpoint_position() fail {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  let skipped = walk_at(block, opening, witness_scan_batch_size)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(skipped),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// Malformed checkpoint: truncated bytes are refused before any position is
/// derived.
test protected_output_step_04_refuses_malformed_checkpoint_bytes() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  let bytes = encode_field_walk_checkpoint(start)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    bytearray.take(bytes, bytearray.length(bytes) - 1),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// A checkpoint taken over another transaction's field is refused at resume
/// even though it hashes to a well-formed digest.
test protected_output_step_04_refuses_a_replayed_checkpoint() fail {
  let block = block_with_witnesses(1)
  let other = block_with_witnesses(2)
  let opening = inline_opening(block, 1)
  let foreign = walk_at(other, inline_opening(other, 2), 0)
  run_step_04(
    scan_of(block, accepted, foreign),
    opening,
    encode_field_walk_checkpoint(foreign),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// A batch that leaves positions unvisited may not claim the terminal verdict.
test protected_output_step_04_refuses_a_premature_terminal() fail {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// The frontier's verdict is computed on chain: an exhausted field of invalid
/// witnesses cannot be closed as `signer_present = True`, in either direction.
test protected_output_step_04_refuses_a_claimed_present_signer() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, forced, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, forced, True)),
  )
}

/// The terminal verdict carries the subject the scan carried: a verdict for
/// another direction is not this thread's successor.
test protected_output_step_04_refuses_a_substituted_terminal_subject() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, forced, False)),
  )
}

/// Witness-set anchor: an opening whose witness set is not the anchored one
/// cannot resume the walk.
test protected_output_step_04_refuses_a_substituted_witness_set() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  let forged = fixture.inline_witness_opening(
    block.compact_cbor,
    fixture.empty_witness_set_compact(),
    [],
  )
  run_step_04(
    scan_of(block, accepted, start),
    forged,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, accepted, False)),
  )
}

/// A non-terminal batch must self-loop; handing the scan state to step 05 is
/// the wrong successor.
test protected_output_step_04_refuses_a_wrong_successor_on_loop() fail {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  let looped: Data = scan_of(
    block,
    accepted,
    walk_at(block, opening, witness_scan_batch_size),
  )
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, looped),
  )
}

/// The terminal verdict may only continue at step 05.
test protected_output_step_04_refuses_a_wrong_successor_on_terminal() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, accepted, start),
    opening,
    encode_field_walk_checkpoint(start),
    loop_tx(block, verdict_of(block, accepted, False)),
  )
}
