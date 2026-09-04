//// ## Input-Set Uniqueness Fraud Proof — Step 02
////
//// Concludes the proof that the accepted transaction carried through step-01
//// violates intra-transaction input-set uniqueness/disjointness, in one of
//// three sub-variants over §2.5 fields 0 and 1:
////
//// - `DuplicateSpendInputs`: field 0 names one out-ref twice;
//// - `DuplicateReferenceInputs`: field 1 names one out-ref twice;
//// - `SpendReferenceOverlap`: fields 0 and 1 both name one out-ref.
////
//// Every verdict is a byte equality of two door-authenticated §5.3 items.
//// The redeemer names **item indices**, never byte offsets: `field_item_at`
//// derives each position arithmetically from the fixed stride and validates
//// the item's canonical wrapper, and §5.3's fixed index spelling means one
//// out-ref has exactly one 38-byte item form — so payload equality is out-ref
//// equality, and a prover cannot aim two indices at overlapping or
//// non-canonical byte ranges.

use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, anchored_field_view, anchored_native_tx,
  opened_field_view, reference_inputs_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/input_set_uniqueness/step_02.{
  Args, Datum, DuplicateReferenceInputs, DuplicateSpendInputs, SpendRedeemer,
  SpendReferenceOverlap, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/native_tx_field_access_v1 as field_access

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
      ct.Continue(DuplicateSpendInputs {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        first_index,
        second_index,
        spend_inputs_opening,
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

        expect Some(State { bad_tx_id }) = m_input_state_data

        // 2. Two distinct positions of the disputed transaction's field 0
        //    must decode to one out-ref. `first_index < second_index` is the
        //    canonical spelling of distinctness (it refuses i == j and the
        //    mirrored claim in one comparison), and both positions are
        //    derived by the door from §5.3's fixed stride — out-of-range
        //    indices refuse inside `field_item_at`.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect first_index < second_index
        expect
          field_access.field_item_at(spend_inputs_view, first_index) == field_access.field_item_at(
            spend_inputs_view,
            second_index,
          )

        // Done.
        True
      }
      ct.Continue(DuplicateReferenceInputs {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        first_index,
        second_index,
        reference_inputs_opening,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. As above.
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

        expect Some(State { bad_tx_id }) = m_input_state_data

        // 2. Same verdict shape over field 1: two distinct positions, one
        //    out-ref.
        let reference_inputs_view = opened_field_view(
          reference_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          reference_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect first_index < second_index
        expect
          field_access.field_item_at(reference_inputs_view, first_index) == field_access.field_item_at(
            reference_inputs_view,
            second_index,
          )

        // Done.
        True
      }
      ct.Continue(SpendReferenceOverlap {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        spend_index,
        reference_index,
        native_tx_compact_cbor,
        spend_inputs_carriage,
        reference_inputs_carriage,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. As above.
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

        expect Some(State { bad_tx_id }) = m_input_state_data

        // 2. Two fields of one transaction, so the anchor is paid once and
        //    each field is authenticated per-opening off the same handle
        //    (§7.1). `BodyTxOpening` is a call-site literal here, never a
        //    redeemer type.
        let anchored = anchored_native_tx(
          BodyTxOpening { native_tx_compact_cbor },
          BodyAnchor { tx_id: bad_tx_id },
        )
        let spend_inputs_view = anchored_field_view(
          anchored,
          spend_inputs_field_index,
          spend_inputs_carriage,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let reference_inputs_view = anchored_field_view(
          anchored,
          reference_inputs_field_index,
          reference_inputs_carriage,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. One position per field, one out-ref. The two indices index
        //    *different lists*, so no relation between them is asserted —
        //    equal indices are fine, and nothing convicts unless the two
        //    items' out-refs are byte-equal. Fields 0 and 1 share §5.3's item
        //    encoding, so cross-field payload equality is out-ref equality.
        expect
          field_access.field_item_at(spend_inputs_view, spend_index) == field_access.field_item_at(
            reference_inputs_view,
            reference_index,
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

const dup_outref_tx_id =
  #"1111111111111111111111111111111111111111111111111111111111111111"

fn outref(output_index: Int) -> MidgardTxInput {
  MidgardTxInput { tx_id: dup_outref_tx_id, output_index }
}

fn with_input_sets(
  inputs: List<MidgardTxInput>,
  ref_inputs: List<MidgardTxInput>,
) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs,
      reference_inputs: ref_inputs,
    },
  }
}

fn run_finalize(block: fixture.NativeBlockFixtureV1, args: Args) -> Bool {
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { bad_tx_id: block.tx_id }),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn duplicate_spend_args(
  block: fixture.NativeBlockFixtureV1,
  first_index: Int,
  second_index: Int,
) -> Args {
  DuplicateSpendInputs {
    input_index: 0,
    output_index: 0,
    fraud_proof_mint_redeemer_index: 0,
    first_index,
    second_index,
    spend_inputs_opening: fixture.inline_body_opening(
      block.compact_cbor,
      block.tx.body.inputs |> list.map(encode_midgard_tx_input),
    ),
  }
}

fn duplicate_reference_args(
  block: fixture.NativeBlockFixtureV1,
  first_index: Int,
  second_index: Int,
) -> Args {
  DuplicateReferenceInputs {
    input_index: 0,
    output_index: 0,
    fraud_proof_mint_redeemer_index: 0,
    first_index,
    second_index,
    reference_inputs_opening: fixture.inline_body_opening(
      block.compact_cbor,
      block.tx.body.reference_inputs |> list.map(encode_midgard_tx_input),
    ),
  }
}

fn overlap_args(
  block: fixture.NativeBlockFixtureV1,
  spend_index: Int,
  reference_index: Int,
) -> Args {
  SpendReferenceOverlap {
    input_index: 0,
    output_index: 0,
    fraud_proof_mint_redeemer_index: 0,
    spend_index,
    reference_index,
    native_tx_compact_cbor: block.compact_cbor,
    spend_inputs_carriage: field_access.Inline {
      preimage: field_access.encode_field_preimage(
        block.tx.body.inputs |> list.map(encode_midgard_tx_input),
      ),
    },
    reference_inputs_carriage: field_access.Inline {
      preimage: field_access.encode_field_preimage(
        block.tx.body.reference_inputs |> list.map(encode_midgard_tx_input),
      ),
    },
  }
}

/// Positive selector: field 0 names one out-ref at positions 0 and 2 — a
/// non-adjacent duplicate, since a committed-but-invalid transaction owes no
/// canonical ordering — and the proof finalizes.
test input_set_uniqueness_step_02_convicts_a_non_adjacent_spend_duplicate() {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(7), outref(3), outref(7)], [outref(9)]),
  )
  run_finalize(block, duplicate_spend_args(block, 0, 2))
}

/// Positive selector: the adjacent, two-item edge — positions 0 and 1 of a
/// two-item field 0, i.e. first and last at once.
test input_set_uniqueness_step_02_convicts_an_adjacent_spend_duplicate() {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(5), outref(5)], [outref(9)]),
  )
  run_finalize(block, duplicate_spend_args(block, 0, 1))
}

/// Positive selector: field 1 names one out-ref twice.
test input_set_uniqueness_step_02_convicts_a_reference_duplicate() {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(4)], [outref(9), outref(2), outref(9)]),
  )
  run_finalize(block, duplicate_reference_args(block, 0, 2))
}

/// Positive selector: fields 0 and 1 both name one out-ref. The equal numeric
/// indices are incidental — what convicts is the byte-equal out-ref.
test input_set_uniqueness_step_02_convicts_a_spend_reference_overlap() {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(4), outref(6)], [outref(8), outref(6)]),
  )
  run_finalize(block, overlap_args(block, 1, 1))
}

/// Valid-input negative selector: two distinct out-refs at the named
/// positions refuse at the equality check.
test input_set_uniqueness_step_02_rejects_distinct_spend_inputs() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(1), outref(2)], [outref(9)]),
  )
  run_finalize(block, duplicate_spend_args(block, 0, 1))
}

/// `i == j` is not a duplicate: one position always equals itself, so the
/// ordering guard refuses it before any byte is read.
test input_set_uniqueness_step_02_rejects_a_self_comparison() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(7), outref(3), outref(7)], [outref(9)]),
  )
  run_finalize(block, duplicate_spend_args(block, 2, 2))
}

/// The mirrored claim is refused by the same guard: distinctness is spelled
/// `first < second`, canonically.
test input_set_uniqueness_step_02_rejects_a_reversed_index_pair() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(7), outref(3), outref(7)], [outref(9)]),
  )
  run_finalize(block, duplicate_spend_args(block, 2, 0))
}

/// An index at the item count refuses inside the door's `field_item_at`
/// (positions are derived from §5.3's fixed stride, never prover-supplied).
test input_set_uniqueness_step_02_rejects_an_out_of_range_index() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(7), outref(3), outref(7)], [outref(9)]),
  )
  run_finalize(block, duplicate_spend_args(block, 0, 3))
}

/// Same numeric index in two different lists is NOT the overlap fault: the
/// out-refs at spend position 0 and reference position 0 differ here, so the
/// equality refuses even though the indices coincide.
test input_set_uniqueness_step_02_rejects_an_overlap_claim_on_disjoint_sets() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(4), outref(6)], [outref(8), outref(2)]),
  )
  run_finalize(block, overlap_args(block, 0, 0))
}

/// The duplicate must live in the preimage the *disputed* transaction
/// committed: a prover who fabricates a duplicated-items preimage for a
/// unique-input transaction fails the door's authenticate-once hash check,
/// not merely the equality.
test input_set_uniqueness_step_02_rejects_a_fabricated_duplicate_preimage() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(1), outref(2)], [outref(9)]),
  )
  run_finalize(
    block,
    DuplicateSpendInputs {
      input_index: 0,
      output_index: 0,
      fraud_proof_mint_redeemer_index: 0,
      first_index: 0,
      second_index: 1,
      spend_inputs_opening: fixture.inline_body_opening(
        block.compact_cbor,
        [outref(1), outref(1)] |> list.map(encode_midgard_tx_input),
      ),
    },
  )
}

/// Honest no-op guard, step-02 face: this validator only ever sees a thread
/// step-01 admitted, and step-01 refuses any leaf whose validity scalar is
/// not `TxIsValid`. The step-02-local property worth pinning is that the
/// verdict is bound to the anchored transaction: a claim against a different
/// transaction's id cannot open the door at all.
test input_set_uniqueness_step_02_rejects_a_foreign_anchor() fail {
  let block = fixture.native_block_fixture_v1(
    with_input_sets([outref(7), outref(3), outref(7)], [outref(9)]),
  )
  let foreign = fixture.native_block_fixture_v1(
    with_input_sets([outref(1), outref(2)], [outref(9)]),
  )
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        // The thread anchored the *foreign* (unique-input) transaction; the
        // prover's opening carries the duplicated one's bytes.
        data: Some(State { bad_tx_id: foreign.tx_id }),
      },
    ),
    ct.Continue(duplicate_spend_args(block, 0, 2)),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}
