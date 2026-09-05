use aiken/collection/list
use aiken/crypto.{ScriptHash, verify_ed25519_signature}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, WitnessAnchor, address_witnesses_field_index,
  opened_field_walk, resume_opened_field_walk,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionWitnessSet,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/spend_input_signer_missing/rule.{
  AuthenticatedCredentialV1, VerdictV1, WitnessScanV1, witness_scan_batch_size,
}
use midgard/fraud_proofs/spend_input_signer_missing/step_04.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{encode_field_preimage}
use midgard/native_tx_machine_walk_v1.{
  FieldWalkCheckpointV1, encode_field_walk_checkpoint,
  field_walk_checkpoint_hash, walk_fold, walk_remaining,
}

validator main(
  step_05_hash: ScriptHash,
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
        )
        expect Some(scan) = state
        let Pair(view, checkpoint) = resume_opened_field_walk(
          witnesses_opening,
          WitnessAnchor {
            tx_id: scan.authenticated.transaction_id,
            witness_set_hash: scan.authenticated.witness_set_hash,
          },
          address_witnesses_field_index,
          scan.checkpoint_hash,
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
          valid_signer_fold(
            scan.authenticated.transaction_id,
            scan.authenticated.payment_credential,
          ),
        )
        let terminal = signer_present || walk_remaining(next_checkpoint) == 0
        let expected_hash = if terminal {
          step_05_hash
        } else {
          own_hash
        }
        let expected_state: Data = if terminal {
          let value: Data = VerdictV1 {
            subject: scan.authenticated.subject,
            signer_missing: !signer_present,
          }
          value
        } else {
          let value: Data = WitnessScanV1 {
            ..scan,
            checkpoint_hash: field_walk_checkpoint_hash(next_checkpoint),
          }
          value
        }
        and {
          next_hash == expected_hash,
          next_state == expected_state,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn valid_signer_fold(
  transaction_id: ByteArray,
  payment_credential: ByteArray,
) -> fn(Bool, Int, ByteArray) -> Bool {
  fn(found, _index, item) {
    let witness = decode_midgard_address_witness_cbor(item)
    or {
      found,
      and {
        verify_ed25519_signature(
          witness.verification_key,
          transaction_id,
          witness.signature,
        ),
        utils.get_verification_key_hash(witness.verification_key) == payment_credential,
      },
    }
  }
}

// ### Frontier admission selectors
//
// The decisive predicate, item by item: a witness enters the signer frontier
// only when its Ed25519 signature verifies over the bound transaction id and
// its key hashes to the authenticated payment credential.

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

test valid_signature_enters_the_signer_frontier() {
  let fold = valid_signer_fold(
    test_tx_id,
    utils.get_verification_key_hash(test_vkey),
  )
  fold(False, 0, test_witness(test_signature))
}

test invalid_signature_never_enters_the_signer_frontier() {
  let fold = valid_signer_fold(
    test_tx_id,
    utils.get_verification_key_hash(test_vkey),
  )
  !fold(
    False,
    0,
    test_witness(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
  )
}

test valid_signature_from_wrong_credential_does_not_match() {
  let fold = valid_signer_fold(
    test_tx_id,
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  )
  !fold(False, 0, test_witness(test_signature))
}

// ### Scan selectors
//
// Field 7 of the fixture block holds `count` copies of the fixture's sample
// witness, whose signature does not verify: no scan over it ever admits a
// signer, so the only honest terminal is `signer_missing = True` after the
// last position. Every real signature the walk admits is exercised by the
// Lucid lifecycle, where the transaction id is signable.

const test_payment_credential =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// One witness past the last exact 16-item batch, so the walk loops once and
/// terminates on a one-item suffix.
const looping_witness_count: Int = 17

/// One witness past the aggregate field bound: `floor((32,768 - 3) / 103)` is
/// 318, and 319 witnesses encode to 32,860 bytes.
const adjacent_over_bound_witness_count: Int = 319

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

fn authenticated_of(
  block: fixture.NativeBlockFixtureV1,
) -> AuthenticatedCredentialV1 {
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: block.tx_id,
    source_key: #"",
    rejection_reason: None,
  }
  AuthenticatedCredentialV1 {
    subject,
    transaction_id: block.tx_id,
    witness_set_hash: block.compact.witness_set_hash,
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
  let authenticated = authenticated_of(block)
  let Pair(view, start) = opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: authenticated.transaction_id,
      witness_set_hash: authenticated.witness_set_hash,
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
  checkpoint: FieldWalkCheckpointV1,
) -> WitnessScanV1 {
  WitnessScanV1 {
    authenticated: authenticated_of(block),
    checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
  }
}

fn verdict_of(block: fixture.NativeBlockFixtureV1, signer_missing: Bool) -> Data {
  let value: Data = VerdictV1 {
    subject: authenticated_of(block).subject,
    signer_missing,
  }
  value
}

fn run_step_04(
  scan: WitnessScanV1,
  opening: FieldOpeningV1,
  checkpoint_cbor: ByteArray,
  tx: Transaction,
) -> Bool {
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

/// A single invalid witness exhausts the field: the frontier stays empty and
/// the thread terminates at step 05 with `signer_missing = True`.
test step_04_exhausts_an_invalid_witness_to_signer_missing() {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, True)),
  )
}

/// An empty witness field terminates on its first transaction: nothing to
/// scan, nothing admitted.
test step_04_terminates_an_empty_witness_field_as_signer_missing() {
  let block = block_with_witnesses(0)
  let opening = inline_opening(block, 0)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, True)),
  )
}

/// Seventeen witnesses: the first transaction consumes exactly one batch and
/// self-loops with only the next checkpoint digest in state.
test step_04_loops_after_a_full_batch() {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  let next = walk_at(block, opening, witness_scan_batch_size)
  expect walk_remaining(next) == 1
  let looped: Data = scan_of(block, next)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    loop_tx(block, looped),
  )
}

/// The resumed suffix terminates from the committed batch boundary.
test step_04_terminates_the_resumed_suffix() {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let resumed = walk_at(block, opening, witness_scan_batch_size)
  run_step_04(
    scan_of(block, resumed),
    opening,
    encode_field_walk_checkpoint(resumed),
    handoff_tx(block, verdict_of(block, True)),
  )
}

/// Bytes for a position the thread never committed do not hash to its digest.
test step_04_refuses_a_substituted_checkpoint_position() fail {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  let skipped = walk_at(block, opening, witness_scan_batch_size)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(skipped),
    handoff_tx(block, verdict_of(block, True)),
  )
}

/// Truncated checkpoint bytes are refused before any position is derived.
test step_04_refuses_malformed_checkpoint_bytes() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  let bytes = encode_field_walk_checkpoint(start)
  run_step_04(
    scan_of(block, start),
    opening,
    bytearray.take(bytes, bytearray.length(bytes) - 1),
    handoff_tx(block, verdict_of(block, True)),
  )
}

/// A batch that leaves positions unvisited may not claim the terminal verdict.
test step_04_refuses_a_premature_terminal() fail {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, True)),
  )
}

/// The frontier's verdict is computed on chain: an exhausted field cannot be
/// closed as `signer_missing = False`.
test step_04_refuses_a_claimed_present_signer() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, verdict_of(block, False)),
  )
}

/// A non-terminal batch must self-loop; handing the scan state to step 05 is
/// the wrong successor.
test step_04_refuses_a_wrong_successor_on_loop() fail {
  let block = block_with_witnesses(looping_witness_count)
  let opening = inline_opening(block, looping_witness_count)
  let start = walk_at(block, opening, 0)
  let looped: Data = scan_of(
    block,
    walk_at(block, opening, witness_scan_batch_size),
  )
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    handoff_tx(block, looped),
  )
}

/// The terminal verdict may only continue at step 05.
test step_04_refuses_a_wrong_successor_on_terminal() fail {
  let block = block_with_witnesses(1)
  let opening = inline_opening(block, 1)
  let start = walk_at(block, opening, 0)
  run_step_04(
    scan_of(block, start),
    opening,
    encode_field_walk_checkpoint(start),
    loop_tx(block, verdict_of(block, True)),
  )
}

/// The aggregate field bound is the family's adjacent consensus bound: a
/// 319-witness field is 32,860 bytes, which only tier-3 carriage could bring
/// to the door, and the door refuses the certified view over it. No
/// checkpoint over that field can exist. (An inline opening of that size is
/// not bounded here because it cannot fit an L1 transaction at all.)
test step_04_refuses_an_adjacent_over_bound_witness_field() fail {
  let block = block_with_witnesses(adjacent_over_bound_witness_count)
  let preimage = encode_field_preimage(
    witness_items(adjacent_over_bound_witness_count),
  )
  let opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    preimage,
  )
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    address_witnesses_field_index,
    preimage,
  )
  let authenticated = authenticated_of(block)
  let Pair(_, _) = opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: authenticated.transaction_id,
      witness_set_hash: authenticated.witness_set_hash,
    },
    address_witnesses_field_index,
    refs,
    fixture.test_certificate_policy_id,
  )
  True
}
