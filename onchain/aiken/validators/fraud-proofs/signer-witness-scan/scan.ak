use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_224, verify_ed25519_signature}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, WitnessAnchor, address_witnesses_field_index,
  opened_field_walk, resume_opened_field_walk, script_witnesses_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionWitnessSet,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/signer_witness_scan_v1/scan.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/signer_witness_scan_v1/state.{
  ScanV1, VerdictV1, witness_scan_batch_size,
}
use midgard/native_tx_field_access_v1.{encode_field_preimage}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, encode_field_walk_checkpoint,
  field_walk_checkpoint_hash, walk_fold, walk_remaining,
}
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

validator main(
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
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
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        witnesses_opening,
        checkpoint_cbor,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(scan) = state
        let transaction_id = scan.subject.transaction_id
        let Pair(view, checkpoint) = open_or_resume_witness_walk(
          scan,
          witnesses_opening,
          checkpoint_cbor,
          reference_inputs,
          certificate_policy,
        )
        let remaining = walk_remaining(checkpoint)
        let budget = if remaining > witness_scan_batch_size {
          witness_scan_batch_size
        } else {
          remaining
        }
        let Pair(signer_present, next_checkpoint) = walk_fold(
          view,
          checkpoint,
          budget,
          False,
          signer_fold(scan.payment_credential, transaction_id),
        )
        if signer_present || walk_remaining(next_checkpoint) == 0 {
          let expected: Data = VerdictV1 {
            subject: scan.subject,
            signer_required: True,
            signer_present,
          }
          and {
            next_hash == terminal_hash,
            next_state == expected,
          }
        } else {
          let expected: Data = ScanV1 {
            ..scan,
            checkpoint_hash: field_walk_checkpoint_hash(next_checkpoint),
          }
          and {
            next_hash == own_hash,
            next_state == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

/// The opening turn (empty `checkpoint_hash`) derives item zero from the
/// authenticated opening and takes no checkpoint bytes; every later turn
/// resumes from the bytes the committed digest names.
fn open_or_resume_witness_walk(
  scan: ScanV1,
  opening: FieldOpeningV1,
  checkpoint_cbor: Option<ByteArray>,
  reference_inputs: List<Input>,
  certificate_policy: PolicyId,
) {
  let anchor = WitnessAnchor {
    tx_id: scan.subject.transaction_id,
    witness_set_hash: scan.witness_set_hash,
    source_kind: scan.subject.source_kind,
  }
  if scan.checkpoint_hash == #"" {
    expect None = checkpoint_cbor
    opened_field_walk(
      opening,
      anchor,
      address_witnesses_field_index,
      reference_inputs,
      certificate_policy,
    )
  } else {
    expect Some(checkpoint_bytes) = checkpoint_cbor
    resume_opened_field_walk(
      opening,
      anchor,
      address_witnesses_field_index,
      scan.checkpoint_hash,
      checkpoint_bytes,
      reference_inputs,
      certificate_policy,
    )
  }
}

/// A witness counts when its key hashes to the target credential and its
/// signature verifies over the transaction id. The key hash is checked first,
/// so only a matching key pays for an Ed25519 check; once a signer is found
/// the rest of the batch is not decoded.
fn signer_fold(
  target: ByteArray,
  transaction_id: ByteArray,
) -> fn(Bool, Int, ByteArray) -> Bool {
  fn(found, _index, item) {
    if found {
      True
    } else {
      let witness = decode_midgard_address_witness_cbor(item)
      and {
        blake2b_224(witness.verification_key) == target,
        verify_ed25519_signature(
          witness.verification_key,
          transaction_id,
          witness.signature,
        ),
      }
    }
  }
}

// ### Scan selectors
//
// Every fixture block is `valid_native_tx_v1` with field 7 replaced, so all of
// them share one transaction id (the id covers the body only). The signer
// witness below is a real signature over that id, made offline with a fixed
// test key; the fixture's sample witness never verifies.

const signer_vkey =
  #"8ca63145907a2e6c5fb3dc0791c0189b8252da3e4143704b3b2ca4e526f25dd1"

const signer_signature =
  #"260cf7799612b77f5029c6567b7086a52b794ebff385d74aa3a582e7de5ff1aa0477999830cf6b857339f5c8df8ba04dac76210dbc2de35874d556b36a0d3909"

const other_credential =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// The consensus maximum: `floor((32,768 - 3) / 103)` address witnesses.
const max_witness_count: Int = 318

/// One witness past the aggregate field bound; 319 witnesses encode to 32,860
/// bytes.
const adjacent_over_bound_witness_count: Int = 319

/// One witness past the last exact batch, so the walk loops once and
/// terminates on a one-item suffix.
const looping_witness_count: Int = witness_scan_batch_size + 1

const accepted: Int = substrate.direction_wrongful_acceptance

const forced: Int = substrate.direction_wrongful_rejection

fn signer_witness() -> MidgardAddressWitness {
  MidgardAddressWitness {
    verification_key: signer_vkey,
    signature: signer_signature,
  }
}

fn signer_credential() -> ByteArray {
  blake2b_224(signer_vkey)
}

/// The credential every sample witness's key hashes to: with it, each item
/// passes the key-hash check and costs an Ed25519 check that fails.
fn sample_credential() -> ByteArray {
  blake2b_224(fixture.sample_witness_verification_key())
}

fn invalid_witnesses(count: Int) -> List<MidgardAddressWitness> {
  list.repeat(fixture.sample_address_witness(), count)
}

fn block_with(
  witnesses: List<MidgardAddressWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        addr_tx_wits: witnesses,
      },
    },
  )
}

fn items_of(witnesses: List<MidgardAddressWitness>) -> List<ByteArray> {
  list.map(witnesses, encode_midgard_address_witness)
}

fn inline_opening(
  block: fixture.NativeBlockFixtureV1,
  witnesses: List<MidgardAddressWitness>,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    items_of(witnesses),
  )
}

fn certified_opening(
  block: fixture.NativeBlockFixtureV1,
  witnesses: List<MidgardAddressWitness>,
) -> FieldOpeningV1 {
  fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    encode_field_preimage(items_of(witnesses)),
  )
}

fn certificate_of(
  block: fixture.NativeBlockFixtureV1,
  witnesses: List<MidgardAddressWitness>,
  field_index: Int,
) -> List<Input> {
  fixture.certified_reference_inputs_v1(
    block.tx_id,
    field_index,
    encode_field_preimage(items_of(witnesses)),
  )
}

fn subject_of(
  block: fixture.NativeBlockFixtureV1,
  direction: Int,
) -> substrate.VerdictSubjectV1 {
  if direction == accepted {
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

/// The position after `taken` items, derived as the opening turn derives it.
fn walk_at(
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  taken: Int,
) -> FieldWalkCheckpointV1 {
  let Pair(view, start) = opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: block.tx_id,
      witness_set_hash: block.compact.witness_set_hash,
      source_kind: substrate.source_kind_accepted,
    },
    address_witnesses_field_index,
    reference_inputs,
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
  credential: ByteArray,
  checkpoint_hash: ByteArray,
) -> ScanV1 {
  ScanV1 {
    subject: subject_of(block, direction),
    witness_set_hash: block.compact.witness_set_hash,
    payment_credential: credential,
    checkpoint_hash,
  }
}

fn opening_scan(
  block: fixture.NativeBlockFixtureV1,
  credential: ByteArray,
) -> ScanV1 {
  scan_of(block, accepted, credential, #"")
}

fn resumed_scan(
  block: fixture.NativeBlockFixtureV1,
  credential: ByteArray,
  checkpoint: FieldWalkCheckpointV1,
) -> ScanV1 {
  scan_of(block, accepted, credential, field_walk_checkpoint_hash(checkpoint))
}

fn looped(scan: ScanV1, checkpoint: FieldWalkCheckpointV1) -> Data {
  let value: Data = ScanV1 {
    ..scan,
    checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
  }
  value
}

fn verdict_of(scan: ScanV1, signer_present: Bool) -> Data {
  let value: Data = VerdictV1 {
    subject: scan.subject,
    signer_required: True,
    signer_present,
  }
  value
}

fn run_scan(
  scan: ScanV1,
  opening: FieldOpeningV1,
  checkpoint_cbor: Option<ByteArray>,
  tx: Transaction,
) -> Bool {
  let opening = if scan.subject.source_kind == substrate.source_kind_forced {
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
      Args {
        input_index: 0,
        output_index: 0,
        witnesses_opening: opening,
        checkpoint_cbor,
      },
    ),
    fixture.own_out_ref(),
    tx,
  )
}

/// The scan continues at its own address.
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

/// The verdict continues at the terminal.
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

fn checkpoint_bytes(checkpoint: FieldWalkCheckpointV1) -> Option<ByteArray> {
  Some(encode_field_walk_checkpoint(checkpoint))
}

// #### Open

/// The opening turn of a one-witness field with no signer exhausts it at once.
test scan_opens_and_exhausts_a_field_without_the_signer() {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// An empty witness field terminates on the opening turn.
test scan_opens_and_exhausts_an_empty_field() {
  let block = block_with([])
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, []),
    None,
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// One witness past a full batch: the opening turn consumes one batch and
/// loops with only the next checkpoint digest added to state.
test scan_opens_and_loops_after_a_full_batch() {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let next = walk_at(block, opening, [], witness_scan_batch_size)
  expect walk_remaining(next) == 1
  let scan = opening_scan(block, other_credential)
  run_scan(scan, opening, None, loop_tx(block, looped(scan, next), []))
}

/// A certified field opens through its own field-7 certificate.
test scan_opens_a_certified_field() {
  let witnesses = invalid_witnesses(160)
  let block = block_with(witnesses)
  let opening = certified_opening(block, witnesses)
  let refs = certificate_of(block, witnesses, address_witnesses_field_index)
  let next = walk_at(block, opening, refs, witness_scan_batch_size)
  let scan = opening_scan(block, other_credential)
  run_scan(scan, opening, None, loop_tx(block, looped(scan, next), refs))
}

/// The scan is direction-blind: a forced subject walks the same field to the
/// same verdict, and only the terminal reads the polarity.
test scan_exhausts_a_forced_subject() {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = scan_of(block, forced, other_credential, #"")
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

// #### Resume

/// The resumed suffix terminates from the committed batch boundary.
test scan_resumes_and_exhausts_the_suffix() {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let resumed = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = resumed_scan(block, other_credential, resumed)
  run_scan(
    scan,
    opening,
    checkpoint_bytes(resumed),
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

// #### Early exit

/// A valid signer in the first batch ends the scan on that turn, with items
/// still unvisited.
test scan_exits_early_on_a_valid_signer() {
  let witnesses = list.concat(
    invalid_witnesses(2),
    [signer_witness(), ..invalid_witnesses(witness_scan_batch_size)],
  )
  let block = block_with(witnesses)
  let scan = opening_scan(block, signer_credential())
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, True), []),
  )
}

/// A valid signer in a resumed batch ends the scan on that turn.
test scan_exits_early_on_a_resumed_valid_signer() {
  let witnesses = list.concat(
    invalid_witnesses(witness_scan_batch_size),
    [signer_witness(), ..invalid_witnesses(witness_scan_batch_size)],
  )
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let resumed = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = resumed_scan(block, signer_credential(), resumed)
  run_scan(
    scan,
    opening,
    checkpoint_bytes(resumed),
    handoff_tx(block, verdict_of(scan, True), []),
  )
}

/// Once a signer is found the scan may not keep walking.
test scan_refuses_to_loop_past_a_valid_signer() fail {
  let witnesses = list.concat(
    invalid_witnesses(2),
    [signer_witness(), ..invalid_witnesses(witness_scan_batch_size)],
  )
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let next = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = opening_scan(block, signer_credential())
  run_scan(scan, opening, None, loop_tx(block, looped(scan, next), []))
}

/// The signer's key with a signature over another message does not count.
test scan_refuses_a_present_signer_with_a_foreign_signature() fail {
  let witnesses = [
    MidgardAddressWitness {
      verification_key: signer_vkey,
      signature: fixture.sample_witness_signature(),
    },
  ]
  let block = block_with(witnesses)
  let scan = opening_scan(block, signer_credential())
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, True), []),
  )
}

/// A valid signature by a key that is not the credential's does not count.
test scan_exhausts_a_valid_signature_by_another_key() {
  let witnesses = [signer_witness()]
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

// #### Exhausted with no match

/// The verdict is computed on chain: an exhausted field without the signer
/// cannot be closed as present.
test scan_refuses_a_claimed_present_signer() fail {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, True), []),
  )
}

/// A batch that leaves positions unvisited may not claim the absent verdict.
test scan_refuses_a_premature_terminal() fail {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// The verdict carries the subject the scan carried.
test scan_refuses_a_substituted_verdict_subject() fail {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(
      block,
      verdict_of(scan_of(block, forced, other_credential, #""), False),
      [],
    ),
  )
}

// #### Wrong checkpoint

/// Bytes for a position the thread never committed do not hash to its digest,
/// so a batch cannot be skipped.
test scan_refuses_a_substituted_checkpoint_position() fail {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let start = walk_at(block, opening, [], 0)
  let skipped = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = resumed_scan(block, other_credential, start)
  run_scan(
    scan,
    opening,
    checkpoint_bytes(skipped),
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// Truncated checkpoint bytes are refused before any position is derived.
test scan_refuses_malformed_checkpoint_bytes() fail {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let resumed = walk_at(block, opening, [], witness_scan_batch_size)
  let bytes = encode_field_walk_checkpoint(resumed)
  let scan = resumed_scan(block, other_credential, resumed)
  run_scan(
    scan,
    opening,
    Some(bytearray.take(bytes, bytearray.length(bytes) - 1)),
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// A checkpoint over another transaction's field is refused at resume.
test scan_refuses_a_replayed_checkpoint() fail {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let other_witnesses = invalid_witnesses(2)
  let other = block_with(other_witnesses)
  let foreign = walk_at(other, inline_opening(other, other_witnesses), [], 0)
  let scan = resumed_scan(block, other_credential, foreign)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    checkpoint_bytes(foreign),
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// The opening turn takes no checkpoint bytes: a prover cannot start the
/// walk at a position of its choosing.
test scan_refuses_checkpoint_bytes_on_the_opening_turn() fail {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let skipped = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    opening,
    checkpoint_bytes(skipped),
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// A resume must name the committed position.
test scan_refuses_a_resume_without_checkpoint_bytes() fail {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let resumed = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = resumed_scan(block, other_credential, resumed)
  run_scan(scan, opening, None, handoff_tx(block, verdict_of(scan, False), []))
}

// #### Wrong anchor or certificate slot

/// An opening whose witness set is not the anchored one cannot open the walk.
test scan_refuses_a_substituted_witness_set() fail {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    fixture.inline_witness_opening(
      block.compact_cbor,
      fixture.empty_witness_set_compact(),
      [],
    ),
    None,
    handoff_tx(block, verdict_of(scan, False), []),
  )
}

/// Field 6's certificate cannot open field 7, even for the same transaction.
test scan_refuses_a_foreign_field_certificate() fail {
  let witnesses = invalid_witnesses(160)
  let block = block_with(witnesses)
  let opening = certified_opening(block, witnesses)
  let honest = certificate_of(block, witnesses, address_witnesses_field_index)
  let foreign = certificate_of(block, witnesses, script_witnesses_field_index)
  let next = walk_at(block, opening, honest, witness_scan_batch_size)
  let scan = opening_scan(block, other_credential)
  run_scan(scan, opening, None, loop_tx(block, looped(scan, next), foreign))
}

/// A certificate over bytes the transaction never committed is refused.
test scan_refuses_a_certificate_over_a_substituted_preimage() fail {
  let witnesses = invalid_witnesses(160)
  let block = block_with(witnesses)
  let opening = certified_opening(block, witnesses)
  let honest = certificate_of(block, witnesses, address_witnesses_field_index)
  let foreign = certificate_of(
    block,
    invalid_witnesses(159),
    address_witnesses_field_index,
  )
  let next = walk_at(block, opening, honest, witness_scan_batch_size)
  let scan = opening_scan(block, other_credential)
  run_scan(scan, opening, None, loop_tx(block, looped(scan, next), foreign))
}

// #### Foreign successor

/// A non-terminal batch must loop; handing the scan state to the terminal is
/// the wrong successor.
test scan_refuses_a_terminal_successor_on_loop() fail {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let next = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = opening_scan(block, other_credential)
  run_scan(scan, opening, None, handoff_tx(block, looped(scan, next), []))
}

/// The verdict may only continue at the terminal.
test scan_refuses_a_looping_successor_on_terminal() fail {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = opening_scan(block, other_credential)
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    loop_tx(block, verdict_of(scan, False), []),
  )
}

// #### All keys match, no signature verifies

/// Every item passes the key-hash check and fails Ed25519: the batch is
/// visited in full and loops, never claiming a signer.
test scan_loops_over_a_batch_of_matching_keys_with_bad_signatures() {
  let witnesses = invalid_witnesses(looping_witness_count)
  let block = block_with(witnesses)
  let opening = inline_opening(block, witnesses)
  let next = walk_at(block, opening, [], witness_scan_batch_size)
  let scan = opening_scan(block, sample_credential())
  run_scan(scan, opening, None, loop_tx(block, looped(scan, next), []))
}

/// The same shape cannot be closed as a present signer.
test scan_refuses_a_present_signer_over_matching_keys_with_bad_signatures() fail {
  let witnesses = invalid_witnesses(1)
  let block = block_with(witnesses)
  let scan = opening_scan(block, sample_credential())
  run_scan(
    scan,
    inline_opening(block, witnesses),
    None,
    handoff_tx(block, verdict_of(scan, True), []),
  )
}

// #### Consensus maximum (318 witnesses, certified)
//
// These are the budgeted turns: the opening turn and a full resumed batch at
// the maximum field, with keys that never match (honest) and with every key
// matching and every signature failing (adversarial), plus the final turn
// that finds a signer in the last position. Each `_control` test builds the
// same fixture without running the validator, so a reading minus its control
// is the validator's own cost.

type MaxTurn {
  scan: ScanV1,
  opening: FieldOpeningV1,
  checkpoint_cbor: Option<ByteArray>,
  tx: Transaction,
}

/// The middle full batch of the maximum field.
const max_resume_position: Int =
  max_witness_count / witness_scan_batch_size / 2 * witness_scan_batch_size

/// The last batch boundary of the maximum field.
const max_final_position: Int =
  ( max_witness_count - 1 ) / witness_scan_batch_size * witness_scan_batch_size

fn max_turn(
  witnesses: List<MidgardAddressWitness>,
  credential: ByteArray,
  taken: Int,
  signer_present: Option<Bool>,
) -> MaxTurn {
  let block = block_with(witnesses)
  let opening = certified_opening(block, witnesses)
  let refs = certificate_of(block, witnesses, address_witnesses_field_index)
  let from = walk_at(block, opening, refs, taken)
  let scan = if taken == 0 {
    opening_scan(block, credential)
  } else {
    resumed_scan(block, credential, from)
  }
  let tx = when signer_present is {
    Some(present) -> handoff_tx(block, verdict_of(scan, present), refs)
    None -> {
      let next = walk_at(block, opening, refs, taken + witness_scan_batch_size)
      loop_tx(block, looped(scan, next), refs)
    }
  }
  MaxTurn {
    scan,
    opening,
    checkpoint_cbor: if taken == 0 {
      None
    } else {
      checkpoint_bytes(from)
    },
    tx,
  }
}

fn measured(turn: MaxTurn) -> Bool {
  run_scan(turn.scan, turn.opening, turn.checkpoint_cbor, turn.tx)
}

fn control(turn: MaxTurn) -> Bool {
  and {
    bytearray.length(turn.scan.payment_credential) == 28,
    list.length(turn.tx.outputs) > 0,
    list.length(turn.tx.reference_inputs) > 0,
  }
}

fn honest_max_turn(taken: Int) -> MaxTurn {
  max_turn(invalid_witnesses(max_witness_count), other_credential, taken, None)
}

fn adversarial_max_turn(taken: Int) -> MaxTurn {
  max_turn(
    invalid_witnesses(max_witness_count),
    sample_credential(),
    taken,
    None,
  )
}

fn final_max_turn() -> MaxTurn {
  max_turn(
    list.concat(invalid_witnesses(max_witness_count - 1), [signer_witness()]),
    signer_credential(),
    max_final_position,
    Some(True),
  )
}

test scan_max_open_turn_honest() {
  measured(honest_max_turn(0))
}

test scan_max_open_turn_honest_control() {
  control(honest_max_turn(0))
}

test scan_max_resume_turn_honest() {
  measured(honest_max_turn(max_resume_position))
}

test scan_max_resume_turn_honest_control() {
  control(honest_max_turn(max_resume_position))
}

test scan_max_open_turn_adversarial() {
  measured(adversarial_max_turn(0))
}

test scan_max_open_turn_adversarial_control() {
  control(adversarial_max_turn(0))
}

test scan_max_resume_turn_adversarial() {
  measured(adversarial_max_turn(max_resume_position))
}

test scan_max_resume_turn_adversarial_control() {
  control(adversarial_max_turn(max_resume_position))
}

/// The final turn at the maximum finds the signer in the last position.
test scan_max_final_turn_finds_the_last_signer() {
  measured(final_max_turn())
}

test scan_max_final_turn_finds_the_last_signer_control() {
  control(final_max_turn())
}

/// One witness past the bound: the certified view over it is refused at the
/// door, so no walk position over such a field can exist. (An inline opening
/// of that size cannot fit an L1 transaction at all.)
test scan_refuses_an_adjacent_over_bound_witness_field() fail {
  let witnesses = invalid_witnesses(adjacent_over_bound_witness_count)
  let block = block_with(witnesses)
  let Pair(_, _) = opened_field_walk(
    certified_opening(block, witnesses),
    WitnessAnchor {
      tx_id: block.tx_id,
      witness_set_hash: block.compact.witness_set_hash,
      source_kind: substrate.source_kind_accepted,
    },
    address_witnesses_field_index,
    certificate_of(block, witnesses, address_witnesses_field_index),
    fixture.test_certificate_policy_id,
  )
  True
}
