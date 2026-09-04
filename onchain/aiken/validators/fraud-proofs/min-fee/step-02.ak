//// ## Min-Fee Fraud Proof
////
//// Concludes the proof that the transaction carried through step-01 pays a
//// fee below the network minimum the disputed block's own header declares:
//// `fee < min_fee_a * canonical_tx_size + min_fee_b` (the validation
//// machine's `reject_min_fee` rule, shared through
//// `compact.min_fee_lovelace_v1`).
////
//// The canonical size is the machine's `native_tx_canonical_size_v1` over
//// the thread-anchored compact structure and the nine §5.1 field-preimage
//// byte lengths, each authenticated through the §8.8 door: understating a
//// length would only *shrink* the claimed minimum, and overstating one
//// requires a preimage the committed field hashes refuse.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, WitnessAnchor, WitnessTxOpening,
  address_witnesses_field_index, anchored_field_view, anchored_native_tx,
  mint_field_index, outputs_field_index, redeemers_field_index,
  reference_inputs_field_index, required_observers_field_index,
  required_signers_field_index, script_witnesses_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/min_fee/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  min_fee_lovelace_v1, native_tx_canonical_size_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{FieldCarriageV1, field_total_length}

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
              bad_tx,
              bad_tx_body_fee,
              bad_tx_id,
              min_fee_a,
              min_fee_b,
            })
          = m_input_state_data

        // 2. All nine field-preimage lengths are read through the §8.8 door,
        //    one anchored handle per §2.5 half. The witness half's anchor is
        //    the thread-carried `bad_tx.witness_set_hash` — §3's transaction
        //    id commits the body alone, so without it a prover could pair the
        //    genuine body with an invented witness set and inflate the three
        //    witness-field lengths until any fee falls below the "minimum"
        //    (the same forgery `missing-signature` step-04 documents).
        expect
            [
              spend_inputs_carriage, reference_inputs_carriage, outputs_carriage,
              required_observers_carriage, required_signers_carriage,
              mint_carriage, script_witnesses_carriage,
              address_witnesses_carriage, redeemers_carriage,
            ]
          = field_carriages

        let body_handle = anchored_native_tx(
          BodyTxOpening { native_tx_compact_cbor },
          BodyAnchor { tx_id: bad_tx_id },
        )
        let witness_handle = anchored_native_tx(
          WitnessTxOpening { native_tx_compact_cbor, witness_set },
          WitnessAnchor {
            tx_id: bad_tx_id,
            witness_set_hash: bad_tx.witness_set_hash,
          },
        )
        let body_field_length = fn(field_index: Int, carriage) {
          field_total_length(
            anchored_field_view(
              body_handle,
              field_index,
              carriage,
              reference_inputs,
              field_preimage_certificate_policy_id,
            ),
          )
        }
        let witness_field_length = fn(field_index: Int, carriage) {
          field_total_length(
            anchored_field_view(
              witness_handle,
              field_index,
              carriage,
              reference_inputs,
              field_preimage_certificate_policy_id,
            ),
          )
        }
        let field_lengths = NativeTxFieldPreimageLengthsV1 {
          spend_inputs: body_field_length(
            spend_inputs_field_index,
            spend_inputs_carriage,
          ),
          reference_inputs: body_field_length(
            reference_inputs_field_index,
            reference_inputs_carriage,
          ),
          outputs: body_field_length(outputs_field_index, outputs_carriage),
          required_observers: body_field_length(
            required_observers_field_index,
            required_observers_carriage,
          ),
          required_signers: body_field_length(
            required_signers_field_index,
            required_signers_carriage,
          ),
          mint: body_field_length(mint_field_index, mint_carriage),
          address_witnesses: witness_field_length(
            address_witnesses_field_index,
            address_witnesses_carriage,
          ),
          script_witnesses: witness_field_length(
            script_witnesses_field_index,
            script_witnesses_carriage,
          ),
          redeemers: witness_field_length(
            redeemers_field_index,
            redeemers_carriage,
          ),
        }

        // 3. The committed fee must fall below the header's declared minimum
        //    over the canonical full-transaction size — the validation
        //    machine's own formula, byte for byte. A fee exactly at the
        //    minimum satisfies the ledger rule and refuses here.
        let canonical_tx_size = native_tx_canonical_size_v1(
          bad_tx,
          field_lengths,
        )
        expect
          bad_tx_body_fee < min_fee_lovelace_v1(
            min_fee_a,
            min_fee_b,
            canonical_tx_size,
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
  native_tx_canonical_size_v1(
    block.compact,
    fixture.field_preimage_lengths_v1(block.tx),
  )
}

fn state_for(
  block: fixture.NativeBlockFixtureV1,
  min_fee_a: Int,
  min_fee_b: Int,
) -> State {
  State {
    bad_tx: block.compact,
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
