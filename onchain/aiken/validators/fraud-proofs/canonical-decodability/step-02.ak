//// ## Canonical-Decodability Fraud Proof — step 02 (`docs/spec/midgard-tx.md` §12.7)
////
//// Concludes the proof that the field carried through step 01 was committed to
//// bytes that are not a §5.1 envelope.
////
//// It reads no preimage and opens no door. Everything it adjudicates was
//// derived by step 01 from bytes the §4 hash check had already tied to the
//// block's own commitment, which is what makes this step's whole verdict a
//// bounds check and a comparison.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{
  is_canonical_decodability_violation_v1, verdict_code_count,
  verdict_grammatical, verdict_missing_array_header, verdict_missing_item_header,
  verdict_trailing_bytes,
}
use midgard/fraud_proofs/canonical_decodability/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
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
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

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

        expect Some(State { bad_tx_id: _, field_index, verdict }) = m_input_state_data

        // 2. The accused field's committed preimage must have failed §5.1.
        //    The bounds inside the predicate are not decoration: the state is a
        //    wire form written in another transaction, so a field index outside
        //    §2.5's nine or a code no walk produces is a state step 01 could not
        //    have written, and admitting it would let one fault finalize under
        //    two spellings (§12.1).
        expect is_canonical_decodability_violation_v1(field_index, verdict)

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors

const outputs_field_index: Int = 2

fn finalize_datum(state: State) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
}

fn run_finalize(state: State) -> Bool {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(finalize_datum(state)),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn state_for(field_index: Int, verdict: Int) -> State {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  State { bad_tx_id: block.tx_id, field_index, verdict }
}

/// Positive selector: the miscounted envelope step 01 forwards is convicted.
test canonical_decodability_step_02_convicts_a_miscounted_field() {
  run_finalize(state_for(outputs_field_index, verdict_trailing_bytes))
}

/// Positive selector: an empty committed preimage — shorter than §5.1's
/// shortest admissible form — is convicted on its own code.
test canonical_decodability_step_02_convicts_an_empty_committed_preimage() {
  run_finalize(state_for(outputs_field_index, verdict_missing_array_header))
}

/// Positive selector at a **witness-set** slot: the fault kind spans all nine of
/// §2.5's fields, not the body's six.
test canonical_decodability_step_02_convicts_a_witness_set_field() {
  run_finalize(state_for(6, verdict_missing_item_header))
}

/// Valid-block negative selector: a field whose committed preimage *is* a §5.1
/// envelope forwards `verdict_grammatical`, and the challenge cannot finalize.
/// This is the whole of the family's honest-operator safety: step 01 binds such
/// a block quite happily, and step 02 refuses it.
test canonical_decodability_step_02_rejects_a_grammatical_field() fail {
  run_finalize(state_for(outputs_field_index, verdict_grammatical))
}

/// A field index outside §2.5's nine is a state no step 01 could have written.
test canonical_decodability_step_02_rejects_an_out_of_range_field_index() fail {
  run_finalize(state_for(9, verdict_trailing_bytes))
}

/// A negative field index likewise — the bound is a refusal at both ends, never
/// a clamp (§7.3).
test canonical_decodability_step_02_rejects_a_negative_field_index() fail {
  run_finalize(state_for(-1, verdict_trailing_bytes))
}

/// A verdict code no walk produces is a state no step 01 could have written,
/// and admitting it would let one fault finalize under many spellings.
test canonical_decodability_step_02_rejects_an_unknown_verdict_code() fail {
  run_finalize(state_for(outputs_field_index, verdict_code_count))
}

/// The predicate itself: `verdict_grammatical` is never a violation, every
/// other in-range code always is, and both bounds refuse.
test canonical_decodability_rule_predicate_is_exact() {
  and {
    !is_canonical_decodability_violation_v1(0, verdict_grammatical),
    !is_canonical_decodability_violation_v1(8, verdict_grammatical),
    is_canonical_decodability_violation_v1(0, verdict_missing_array_header),
    is_canonical_decodability_violation_v1(8, verdict_code_count - 1),
  }
}
