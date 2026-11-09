use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/field_grammar_certificate_v1.{FieldGrammarCertificateV1}
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyFieldOpening, opened_certified_field_item_count,
  outputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput}
use midgard/fraud_proofs/reference_input_no_idx/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/native_tx_field_access_v1.{Inline}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  field_preimage_certificate_policy_id: PolicyId,
  field_grammar_certificate_policy_id: PolicyId,
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
        outputs_opening,
        grammar_ref_input_index,
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

        expect Some(State { producing_tx_id, bad_reference_input_output_index })
          = m_input_state_data

        // 2. Field 2's preimage is authenticated through the §8.8 door against
        //    the commitment extracted positionally from the compact structures
        //    `producing_tx_id` binds, and its item count is read from it.
        //
        // 3. The challenged output index must be at or beyond the producing
        //    transaction's output count.
        //
        //    This rule consumes a *count*, which is why it reads it from the
        //    door rather than from a list it reproduced. Both routes answer
        //    only with an authenticated count. Walking the field answers where
        //    the whole preimage is walked (tiers 1–2) and refuses for a
        //    variable-width field under tier 3 (§5.2/§8.6); the field grammar
        //    certificate's count is bound to the same field hash and length,
        //    so the certified count also serves a chunked field.
        expect
          bad_reference_input_output_index >= opened_certified_field_item_count(
            outputs_opening,
            BodyAnchor { tx_id: producing_tx_id, source_kind: 0 },
            outputs_field_index,
            grammar_ref_input_index,
            reference_inputs,
            field_preimage_certificate_policy_id,
            field_grammar_certificate_policy_id,
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

fn run_finalize_with(
  bad_reference_input_output_index: Int,
  carried_outputs: List<MidgardTxOutput>,
) -> Bool {
  let producing_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let state = State {
    producing_tx_id: producing_block.tx_id,
    bad_reference_input_output_index,
  }
  let asset_name = fixture.ct_token_asset_name(producing_block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    grammar.test_grammar_certificate_policy_id,
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
        outputs_opening: fixture.inline_body_opening(
          producing_block.compact_cbor,
          carried_outputs |> list.map(encode_midgard_tx_output),
        ),
        grammar_ref_input_index: None,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn run_finalize(bad_reference_input_output_index: Int) -> Bool {
  let producing_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  run_finalize_with(
    bad_reference_input_output_index,
    producing_block.tx.body.outputs,
  )
}

/// The authenticated field-2 preimage of the producing transaction proves
/// index 7 does not exist (the producing transaction has one output).
test reference_input_no_idx_step_04_accepts_out_of_range_index() {
  run_finalize(7)
}

/// Valid-block negative selector: the referenced output index exists in the
/// producing transaction, so the challenge cannot finalize.
test reference_input_no_idx_step_04_rejects_valid_block_challenge() fail {
  run_finalize(0)
}

/// The non-existence verdict rests on the door's authenticated item count, so a
/// truncated preimage — the obvious way to make any index look out of range —
/// fails the authenticate-once hash check before the count is read.
test reference_input_no_idx_step_04_rejects_a_truncated_outputs_preimage() fail {
  run_finalize_with(0, [])
}

// ### Field 2 through the field grammar certificate
//
// 761 outputs fill the producing transaction's field 2 under tier 3, where
// walking it cannot answer the output count.

const certified_count = 761

fn certified_preimage() -> ByteArray {
  grammar.field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 41),
    certified_count,
    0,
    fixture.repeated_bytes_v1(#"a1", 41),
  )
}

fn certified_block() -> fixture.NativeBlockFixtureV1 {
  fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    outputs_field_index,
    certified_preimage(),
  )
}

fn honest_certificate(
  tx_id: ByteArray,
  preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  grammar.complete_field_grammar_certificate_v1(
    tx_id,
    outputs_field_index,
    preimage,
    certified_count,
  )
}

type CertifiedCase {
  datum: Option<Datum>,
  redeemer: SpendRedeemer,
  tx: Transaction,
}

/// Step 04 challenging output `index` of the tier-3 producing transaction,
/// its count read by `grammar_ref_input_index` against `certificate`.
fn certified_case(
  index: Int,
  grammar_ref_input_index: Option<Int>,
  certificate: fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1,
) -> CertifiedCase {
  let block = certified_block()
  let preimage = certified_preimage()
  CertifiedCase {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            producing_tx_id: block.tx_id,
            bad_reference_input_output_index: index,
          },
        ),
      },
    ),
    redeemer: ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        outputs_opening: fixture.certified_body_opening(
          block.compact_cbor,
          preimage,
        ),
        grammar_ref_input_index,
      },
    ),
    tx: Transaction {
      ..fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
      reference_inputs: grammar.certified_grammar_reference_inputs_v1(
        block.tx_id,
        outputs_field_index,
        preimage,
        certificate(block.tx_id, preimage),
      ),
    },
  }
}

fn certified_route(
  index: Int,
  certificate: fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1,
) -> CertifiedCase {
  certified_case(
    index,
    Some(grammar.grammar_ref_input_index_v1(certified_preimage())),
    certificate,
  )
}

/// The dearest count a submitter walks: `grammar.walked_count_limit` outputs
/// under tier 1, the challenged index one past the last. A walk counts a
/// field it carries inline or in one raw UTxO, but not a chunked one.
fn walked_limit_case() -> CertifiedCase {
  let preimage = grammar.walked_count_limit_inline_preimage_v1(
    fixture.repeated_bytes_v1(#"a1", 41),
  )
  let block = fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    outputs_field_index,
    preimage,
  )
  CertifiedCase {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            producing_tx_id: block.tx_id,
            bad_reference_input_output_index: grammar.walked_count_limit,
          },
        ),
      },
    ),
    redeemer: ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        outputs_opening: BodyFieldOpening {
          native_tx_compact_cbor: block.compact_cbor,
          carriage: Inline { preimage },
        },
        grammar_ref_input_index: None,
      },
    ),
    tx: fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  }
}

fn run_certified(case: CertifiedCase) -> Bool {
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    grammar.test_grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

test reference_input_no_idx_step_04_certified_route_fixture_only_at_the_tier3_maximum() {
  certified_route(certified_count, honest_certificate).datum != None
}

/// The certified count also serves a chunked field: the index one past the
/// last output is out of range.
test reference_input_no_idx_step_04_certified_route_at_the_tier3_maximum_cost() {
  run_certified(certified_route(certified_count, honest_certificate))
}

/// The same challenge by the walk: under tier 3 the walk cannot answer field
/// 2's count, so only the certified route finalizes it.
test reference_input_no_idx_step_04_walked_route_cannot_count_a_chunked_field() fail {
  run_certified(certified_case(certified_count, None, honest_certificate))
}

test reference_input_no_idx_step_04_certified_route_rejects_the_last_output() fail {
  run_certified(certified_route(certified_count - 1, honest_certificate))
}

test reference_input_no_idx_step_04_certified_route_refuses_another_fields_certificate() fail {
  run_certified(
    certified_route(
      certified_count,
      fn(tx_id, preimage) {
        grammar.complete_field_grammar_certificate_v1(
          tx_id,
          5,
          preimage,
          certified_count,
        )
      },
    ),
  )
}

test reference_input_no_idx_step_04_certified_route_refuses_another_transactions_certificate() fail {
  run_certified(
    certified_route(
      certified_count,
      fn(_tx_id, preimage) {
        honest_certificate(
          #"9999999999999999999999999999999999999999999999999999999999999999",
          preimage,
        )
      },
    ),
  )
}

/// A certificate over a longer field 2 records more outputs; it binds
/// another field hash and length.
test reference_input_no_idx_step_04_certified_route_refuses_another_preimages_certificate() fail {
  run_certified(
    certified_route(
      certified_count + 1,
      fn(tx_id, _preimage) {
        grammar.complete_field_grammar_certificate_v1(
          tx_id,
          outputs_field_index,
          grammar.field_preimage_with_item_v1(
            fixture.repeated_bytes_v1(#"a1", 41),
            certified_count + 1,
            0,
            fixture.repeated_bytes_v1(#"a1", 41),
          ),
          certified_count + 1,
        )
      },
    ),
  )
}

test reference_input_no_idx_step_04_certified_route_refuses_an_in_progress_certificate() fail {
  run_certified(
    certified_route(
      certified_count,
      fn(tx_id, preimage) {
        grammar.in_progress_field_grammar_certificate_v1(
          tx_id,
          outputs_field_index,
          preimage,
          certified_count,
        )
      },
    ),
  )
}

test reference_input_no_idx_step_04_walked_route_fixture_only_at_the_walked_limit() {
  walked_limit_case().datum != None
}

test reference_input_no_idx_step_04_walked_route_at_the_walked_limit_cost() {
  run_certified(walked_limit_case())
}
