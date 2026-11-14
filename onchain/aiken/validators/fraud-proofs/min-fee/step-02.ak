//// ## Min-Fee Fraud Proof
////
//// Concludes the proof that the transaction carried through step-01 pays a
//// fee below the network minimum the disputed block's own header declares:
//// `fee < min_fee_a * canonical_tx_size + min_fee_b` (the validation
//// machine's `FeeBelowMinimum` rule, shared through
//// `compact.min_fee_lovelace_v1`).
////
//// The canonical size is the machine's `ledger_tx_size_v1` over
//// the thread-anchored compact structure and the nine §5.1 field-preimage
//// byte lengths, each authenticated through the §8.8 door: understating a
//// length would only *shrink* the claimed minimum, and overstating one
//// requires a preimage the committed field hashes refuse.

use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/field_opening_v1.{
  address_witnesses_field_index, mint_field_index, outputs_field_index,
  redeemers_field_index, reference_inputs_field_index,
  required_observers_field_index, required_signers_field_index,
  script_witnesses_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/min_fee/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  ledger_tx_size_v1, min_fee_lovelace_v1, validation_source_kind_v1,
  verify_native_tx_compact_cbor_v1, verify_tx_compact_cbor_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  ForcedTxCompact, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, Inline, authenticated_committed_preimage,
}
use midgard/rejection_reason_v1.{
  FeeBelowMinimum, NetworkIdMismatch, RejectionReasonV1,
}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        native_tx_compact_cbor,
        witness_set,
        field_carriages,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        expect
            Some(State {
              subject,
              bad_tx,
              bad_tx_body_fee,
              bad_tx_id,
              min_fee_a,
              min_fee_b,
            })
          = m_input_state_data

        expect substrate.subject_is_canonical_v1(subject)
        expect subject.transaction_id == bad_tx_id
        expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
          Some(
            substrate.bind_exact_rejection_reason_v1(subject, FeeBelowMinimum),
          )
        } else {
          None
        }

        // 2. All nine field-preimage lengths are read through the §8.8 door
        //    as committed bytes, in both directions. The fee formula reads
        //    each field's byte length only, never an item, so the step
        //    authenticates the bytes against their commitment and does not
        //    walk the field's grammar: its cost no longer grows with a
        //    field's item count. The witness half is bound to the
        //    thread-carried `bad_tx.witness_set_hash` before any witness
        //    field is read (§3's transaction id commits the body alone).
        //
        //    The acceptance direction reads the same lengths and nothing
        //    more: no grammar walk and no aggregate re-check over the
        //    fields. `ledger_tx_size_v1` reads the nine lengths and the
        //    compact body's scalars only; `authenticated_committed_preimage`
        //    returns the committed preimage or fails on its §4 commitment,
        //    so the commitment fixes each length; and each field's grammar
        //    is the subject of its own families (`committed-field-shape`,
        //    `canonical-decodability`, `field-item-width-illegal`), so this
        //    step does not repeat it.
        expect
            [
              spend_inputs_carriage, reference_inputs_carriage, outputs_carriage,
              required_observers_carriage, required_signers_carriage,
              mint_carriage, script_witnesses_carriage,
              address_witnesses_carriage, redeemers_carriage,
            ]
          = field_carriages

        let verified = verify_tx_compact_cbor_v1(
          validation_source_kind_v1(subject.source_kind),
          bad_tx_id,
          native_tx_compact_cbor,
        )
        expect verified.tx_compact.witness_set_hash == bad_tx.witness_set_hash
        let field_length = fn(field_index, carriage) {
          bytearray.length(
            authenticated_committed_preimage(
              verified,
              witness_set,
              field_index,
              carriage,
              reference_inputs,
              field_preimage_certificate_policy_id,
            ),
          )
        }
        let field_lengths = NativeTxFieldPreimageLengthsV1 {
          spend_inputs: field_length(
            spend_inputs_field_index,
            spend_inputs_carriage,
          ),
          reference_inputs: field_length(
            reference_inputs_field_index,
            reference_inputs_carriage,
          ),
          outputs: field_length(outputs_field_index, outputs_carriage),
          required_observers: field_length(
            required_observers_field_index,
            required_observers_carriage,
          ),
          required_signers: field_length(
            required_signers_field_index,
            required_signers_carriage,
          ),
          mint: field_length(mint_field_index, mint_carriage),
          address_witnesses: field_length(
            address_witnesses_field_index,
            address_witnesses_carriage,
          ),
          script_witnesses: field_length(
            script_witnesses_field_index,
            script_witnesses_carriage,
          ),
          redeemers: field_length(redeemers_field_index, redeemers_carriage),
        }

        // 3. The committed fee must fall below the header's declared minimum
        //    over the canonical full-transaction size — the validation
        //    machine's own formula, byte for byte. A fee exactly at the
        //    minimum satisfies the ledger rule and refuses here.
        let canonical_tx_size = ledger_tx_size_v1(bad_tx.body, field_lengths)
        expect
          substrate.terminal_contradiction_v1(
            subject,
            bad_tx_body_fee < min_fee_lovelace_v1(
              min_fee_a,
              min_fee_b,
              canonical_tx_size,
            ),
          )

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

/// The canonical full-transaction size of the fixture's committed
/// transaction — what the header's `min_fee_a` multiplies.
fn fixture_tx_size(block: fixture.NativeBlockFixtureV1) -> Int {
  ledger_tx_size_v1(
    block.compact.body,
    fixture.field_preimage_lengths_v1(block.tx),
  )
}

fn state_for(
  block: fixture.NativeBlockFixtureV1,
  min_fee_a: Int,
  min_fee_b: Int,
) -> State {
  State {
    subject: substrate.open_subject_v1(
      fixture.ct_token_asset_name(block),
      substrate.AcceptedSource {
        header: block.header,
        tx_inclusion: verify_native_tx_compact_cbor_v1(
          block.tx_id,
          block.compact_cbor,
        ),
      },
    ),
    bad_tx: ForcedTxCompact {
      body: block.compact.body,
      witness_set_hash: block.compact.witness_set_hash,
    },
    bad_tx_body_fee: block.compact.body.fee,
    bad_tx_id: block.tx_id,
    min_fee_a,
    min_fee_b,
  }
}

/// Runs the step with the thread anchored to `anchored_block` while the
/// redeemer's compact bytes, witness set and carriages come from the
/// prover-chosen side. The two sides are the same fixture in every selector
/// but the both-sides forgery, whose whole point is that they disagree.
fn run_finalize_against(
  anchored_block: fixture.NativeBlockFixtureV1,
  min_fee_a: Int,
  min_fee_b: Int,
  opened_compact_cbor: ByteArray,
  witness_set: NativeTxWitnessSetCompact,
  field_carriages: List<FieldCarriageV1>,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(anchored_block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_for(anchored_block, min_fee_a, min_fee_b)),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        native_tx_compact_cbor: opened_compact_cbor,
        witness_set,
        field_carriages,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn run_finalize(
  block: fixture.NativeBlockFixtureV1,
  min_fee_a: Int,
  min_fee_b: Int,
) -> Bool {
  run_finalize_against(
    block,
    min_fee_a,
    min_fee_b,
    block.compact_cbor,
    block.witness_set_compact,
    fixture.inline_field_carriages_v1(block.tx),
  )
}

/// Positive selector, constant term: the committed fee (1,000,000) falls one
/// lovelace short of a flat `min_fee_b` minimum, so the proof finalizes.
test min_fee_step_02_accepts_a_fee_below_the_flat_minimum() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(block, 0, block.compact.body.fee + 1)
}

/// Positive selector, size term: with `min_fee_a = 1` the minimum exceeds the
/// committed fee by exactly one lovelace only if the canonical size the step
/// derives through the door agrees with the producer's — the conviction is a
/// statement about the size computation, not just the comparison.
test min_fee_step_02_accepts_a_fee_below_the_sized_minimum() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(block, 1, block.compact.body.fee - fixture_tx_size(block) + 1)
}

/// **The boundary the ledger rule names**: a fee exactly at the minimum
/// satisfies `fee >= min_fee`, so the challenge must refuse. One lovelace of
/// slack in either direction here is a slashing proof against an honest
/// operator.
test min_fee_step_02_rejects_a_fee_exactly_at_the_minimum() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(block, 0, block.compact.body.fee)
}

/// The same boundary through the size term: `min_fee_a * size + min_fee_b`
/// lands exactly on the committed fee.
test min_fee_step_02_rejects_a_fee_exactly_at_the_sized_minimum() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(block, 1, block.compact.body.fee - fixture_tx_size(block))
}

/// Valid-block negative selector: a fee comfortably above the minimum cannot
/// be challenged.
test min_fee_step_02_rejects_a_fee_above_the_minimum() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(block, 0, block.compact.body.fee - 1)
}

/// A prover cannot inflate the canonical size by carrying a padded field
/// preimage: with the schedule at the exact refusal boundary, one extra item
/// in the outputs carriage would tip the minimum over the fee — but the
/// door's authenticate-once hash check refuses the preimage before it is
/// priced.
test min_fee_step_02_rejects_an_inflated_body_field_preimage() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let tx = block.tx
  let padded_tx = MidgardTransaction {
    ..tx,
    body: MidgardTransactionBody {
      ..tx.body,
      outputs: [fixture.sample_native_output(), ..tx.body.outputs],
    },
  }
  run_finalize_against(
    block,
    1,
    block.compact.body.fee - fixture_tx_size(block),
    block.compact_cbor,
    block.witness_set_compact,
    fixture.inline_field_carriages_v1(padded_tx),
  )
}

/// **Both sides forged — the witness-half slashing forgery.** The prover
/// keeps the genuine body (so the compact bytes still re-derive to the
/// anchored transaction id), swaps in a fattened witness set, and carries
/// preimages matching it — inflating the three witness-field lengths until
/// the honest fee falls below the claimed minimum. The thread-anchored
/// `witness_set_hash`, written by step-01 off the block-committed structure,
/// is what refuses it.
test min_fee_step_02_rejects_a_forged_witness_set_inflation() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let fattened_witness_set = MidgardTransactionWitnessSet {
    ..block.tx.witness_set,
    addr_tx_wits: [
      fixture.sample_address_witness(),
      fixture.sample_address_witness(),
      fixture.sample_address_witness(),
      fixture.sample_address_witness(),
    ],
  }
  let forged = fixture.forged_witness_set_block_v1(
    block,
    fixture.witness_set_compact_v1(fattened_witness_set),
  )
  let forged_tx = MidgardTransaction {
    ..block.tx,
    witness_set: fattened_witness_set,
  }
  // The thread state stays the honest block's; the redeemer carries the
  // internally consistent forged side — compact bytes whose trailing
  // `witness_set_hash` matches the fattened witness set, which matches its
  // own carriages. Only the thread-anchored hash disagrees.
  run_finalize_against(
    block,
    1,
    block.compact.body.fee - fixture_tx_size(block),
    forged.compact_cbor,
    forged.witness_set_compact,
    fixture.inline_field_carriages_v1(forged_tx),
  )
}

fn run_forced_boundary(min_fee_b: Int, reason: RejectionReasonV1) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let base = state_for(block, 0, min_fee_b)
  let state = State {
    ..base,
    subject: substrate.VerdictSubjectV1 {
      ..base.subject,
      direction: substrate.direction_wrongful_rejection,
      source_kind: substrate.source_kind_forced,
      source_key: #"01",
      rejection_reason: Some(reason),
    },
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        native_tx_compact_cbor: forced_fixture.from_native_compact(
          block.compact_cbor,
        ),
        witness_set: block.witness_set_compact,
        field_carriages: fixture.inline_field_carriages_v1(block.tx),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

test min_fee_step_02_proves_wrongful_rejection_at_equality() {
  run_forced_boundary(fixture.valid_native_tx_v1().body.fee, FeeBelowMinimum)
}

test min_fee_step_02_proves_wrongful_rejection_above_minimum() {
  run_forced_boundary(
    fixture.valid_native_tx_v1().body.fee - 1,
    FeeBelowMinimum,
  )
}

test min_fee_step_02_refuses_honest_forced_rejection() fail {
  run_forced_boundary(
    fixture.valid_native_tx_v1().body.fee + 1,
    FeeBelowMinimum,
  )
}

test min_fee_step_02_refuses_foreign_rejection_reason() fail {
  run_forced_boundary(fixture.valid_native_tx_v1().body.fee, NetworkIdMismatch)
}

// ## Both directions at a large field
//
// 761 outputs fill field 2 under tier 3. The fee formula reads the field's
// byte length only, so neither direction's cost grows with its item count.

const large_count = 761

fn large_outputs_preimage() -> ByteArray {
  grammar.field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 41),
    large_count,
    0,
    fixture.repeated_bytes_v1(#"a1", 41),
  )
}

fn large_block() -> fixture.NativeBlockFixtureV1 {
  fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    outputs_field_index,
    large_outputs_preimage(),
  )
}

/// The canonical size of the large block's transaction: the fixture's own
/// lengths with field 2's replaced.
fn large_tx_size(block: fixture.NativeBlockFixtureV1) -> Int {
  ledger_tx_size_v1(
    block.compact.body,
    NativeTxFieldPreimageLengthsV1 {
      ..fixture.field_preimage_lengths_v1(block.tx),
      outputs: bytearray.length(large_outputs_preimage()),
    },
  )
}

type LargeCase {
  datum: Option<Datum>,
  redeemer: SpendRedeemer,
  tx: Transaction,
}

/// Step 02 over the large block with the header's schedule at
/// `min_fee_a = 1` and `min_fee_b = fee - size + offset`, so the minimum is
/// the committed fee plus `offset`. `forced` disputes a forced rejection
/// citing `FeeBelowMinimum`; otherwise an accepted transaction. `inline`
/// carries field 2 in the redeemer (tier 1); otherwise it is certified in
/// chunks (tier 3).
fn large_case(forced: Bool, inline: Bool, offset: Int) -> LargeCase {
  let block = large_block()
  let preimage = large_outputs_preimage()
  let base = state_for(
    block,
    1,
    block.compact.body.fee - large_tx_size(block) + offset,
  )
  let state = if forced {
    State {
      ..base,
      subject: substrate.VerdictSubjectV1 {
        ..base.subject,
        direction: substrate.direction_wrongful_rejection,
        source_kind: substrate.source_kind_forced,
        source_key: #"01",
        rejection_reason: Some(FeeBelowMinimum),
      },
    }
  } else {
    base
  }
  expect [spend_inputs,
        reference_inputs,
        _, ..rest] = fixture.inline_field_carriages_v1(block.tx)
  LargeCase {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    redeemer: ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        native_tx_compact_cbor: if forced {
          forced_fixture.from_native_compact(block.compact_cbor)
        } else {
          block.compact_cbor
        },
        witness_set: block.witness_set_compact,
        field_carriages: [
          spend_inputs,
          reference_inputs,
          if inline {
            Inline { preimage }
          } else {
            fixture.certified_carriage_v1(preimage)
          },
          ..rest
        ],
      },
    ),
    tx: Transaction {
      ..fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
      reference_inputs: if inline {
        []
      } else {
        fixture.certified_reference_inputs_v1(
          block.tx_id,
          outputs_field_index,
          preimage,
        )
      },
    },
  }
}

fn run_large(case: LargeCase) -> Bool {
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

test min_fee_step_02_fixture_only_at_a_large_field() {
  large_case(False, False, 1).datum != None
}

/// Wrongful acceptance at the large certified field: the minimum is one
/// lovelace above the committed fee.
test min_fee_step_02_accepts_a_fee_below_the_minimum_at_a_large_field_cost() {
  run_large(large_case(False, False, 1))
}

test min_fee_step_02_rejects_a_fee_at_the_minimum_at_a_large_field() fail {
  run_large(large_case(False, False, 0))
}

test min_fee_step_02_forced_fixture_only_at_a_large_field() {
  large_case(True, False, 0).datum != None
}

/// Wrongful rejection at the large certified field: the committed fee meets
/// the minimum exactly.
test min_fee_step_02_proves_wrongful_rejection_at_a_large_field_cost() {
  run_large(large_case(True, False, 0))
}

test min_fee_step_02_refuses_honest_forced_rejection_at_a_large_field() fail {
  run_large(large_case(True, False, 1))
}

test min_fee_step_02_fixture_only_at_a_large_inline_field() {
  large_case(False, True, 1).datum != None
}

/// The same large field carried inline: the step reads its byte length
/// only, so the cost no longer grows with the field's item count.
test min_fee_step_02_accepts_a_fee_below_the_minimum_at_a_large_inline_field_cost() {
  run_large(large_case(False, True, 1))
}

test min_fee_step_02_rejects_a_fee_at_the_minimum_at_a_large_inline_field() fail {
  run_large(large_case(False, True, 0))
}

test min_fee_step_02_forced_fixture_only_at_a_large_inline_field() {
  large_case(True, True, 0).datum != None
}

test min_fee_step_02_proves_wrongful_rejection_at_a_large_inline_field_cost() {
  run_large(large_case(True, True, 0))
}

test min_fee_step_02_refuses_honest_forced_rejection_at_a_large_inline_field() fail {
  run_large(large_case(True, True, 1))
}
