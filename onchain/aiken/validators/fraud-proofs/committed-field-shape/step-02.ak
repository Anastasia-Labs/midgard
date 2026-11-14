//// ## Committed-Field-Shape Fraud Proof — step 02 (`docs/spec/midgard-tx.md` §12.8)
////
//// Concludes the proof that the field carried through step 01 was committed to
//// a §5.1 envelope its own slot refuses — §7.4's stride arithmetic or §5.4's
//// per-field byte bound.
////
//// It reads no preimage and opens no door. Everything it adjudicates was
//// derived by step 01 from bytes the §4 hash check had already tied to the
//// block's own commitment, which is what makes this step's whole verdict a
//// bounds check and a comparison.
////
//// **It is also where the boundary against §12.7 is enforced.** A committed
//// preimage that is not a §5.1 envelope reaches step 01 quite happily and
//// forwards `shape_verdict_not_an_envelope`; this step refuses it, because that
//// fault is §12.7's and one committed field must not finalize under two fault
//// kinds.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/committed_field_shape/rule.{
  is_committed_field_shape_violation_v1, shape_verdict_admissible,
  shape_verdict_code_count, shape_verdict_field_byte_bound,
  shape_verdict_not_an_envelope, shape_verdict_wrong_stride,
}
use midgard/fraud_proofs/committed_field_shape/step_02.{
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

        // 2. The accused field's committed preimage must have been an envelope
        //    slot `field_index` refuses. The two non-convicting codes are
        //    refused for different reasons and both matter: `admissible` is the
        //    honest block, and `not_an_envelope` is §12.7's fault, which must
        //    not finalize twice. The bounds inside the predicate are §12.1's
        //    one-spelling rule applied to a state that crosses a transaction
        //    boundary.
        expect is_committed_field_shape_violation_v1(field_index, verdict)

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

/// §2.5 slot 0 (spend inputs) — fixed stride 40.
const spend_inputs_field_index: Int = 0

/// §2.5 slot 2 (outputs) — variable width.
const outputs_field_index: Int = 2

/// §2.5 slot 7 (address witnesses) — fixed stride 103, in the witness-set half.
const address_witnesses_field_index: Int = 7

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

/// Positive selector: the §7.4 stride shape step 01 forwards is convicted.
test committed_field_shape_step_02_convicts_a_wrong_stride_field() {
  run_finalize(state_for(spend_inputs_field_index, shape_verdict_wrong_stride))
}

/// Positive selector at a fixed-stride **witness-set** slot: the fault kind
/// spans both halves of §2.5.
test committed_field_shape_step_02_convicts_a_wrong_stride_witness_field() {
  run_finalize(
    state_for(address_witnesses_field_index, shape_verdict_wrong_stride),
  )
}

/// Positive selector: the §5.4 byte-bound shape, at a variable-width slot where
/// it is the only rule that can have been left.
test committed_field_shape_step_02_convicts_an_oversize_field() {
  run_finalize(state_for(outputs_field_index, shape_verdict_field_byte_bound))
}

/// Valid-block negative selector: a field whose committed preimage is the shape
/// its slot admits forwards `shape_verdict_admissible`, and the challenge cannot
/// finalize. This is the whole of the family's honest-operator safety: step 01
/// binds such a block quite happily, and step 02 refuses it.
test committed_field_shape_step_02_rejects_an_admissible_field() fail {
  run_finalize(state_for(spend_inputs_field_index, shape_verdict_admissible))
}

/// **The disjointness refusal, and it is this family's most load-bearing
/// negative.** A committed non-envelope is §12.7's `canonical-decodability`
/// fault. Step 01 forwards `shape_verdict_not_an_envelope` for it and this step
/// refuses to finalize, so no committed field is faultable under both kinds and
/// §12.1's one-spelling rule survives the split into two sections.
test committed_field_shape_step_02_rejects_a_non_envelope() fail {
  run_finalize(state_for(outputs_field_index, shape_verdict_not_an_envelope))
}

/// A field index outside §2.5's nine is a state no step 01 could have written.
test committed_field_shape_step_02_rejects_an_out_of_range_field_index() fail {
  run_finalize(state_for(9, shape_verdict_wrong_stride))
}

/// A negative field index likewise — the bound is a refusal at both ends, never
/// a clamp (§7.3).
test committed_field_shape_step_02_rejects_a_negative_field_index() fail {
  run_finalize(state_for(-1, shape_verdict_wrong_stride))
}

/// A verdict code no verdict produces is a state no step 01 could have written,
/// and admitting it would let one fault finalize under many spellings.
test committed_field_shape_step_02_rejects_an_unknown_verdict_code() fail {
  run_finalize(state_for(outputs_field_index, shape_verdict_code_count))
}

/// The predicate itself: both convicting codes always convict, both
/// non-convicting codes never do, and both field bounds refuse.
test committed_field_shape_rule_predicate_is_exact() {
  and {
    !is_committed_field_shape_violation_v1(0, shape_verdict_admissible),
    !is_committed_field_shape_violation_v1(8, shape_verdict_admissible),
    !is_committed_field_shape_violation_v1(0, shape_verdict_not_an_envelope),
    !is_committed_field_shape_violation_v1(8, shape_verdict_not_an_envelope),
    is_committed_field_shape_violation_v1(0, shape_verdict_field_byte_bound),
    is_committed_field_shape_violation_v1(8, shape_verdict_field_byte_bound),
    is_committed_field_shape_violation_v1(0, shape_verdict_wrong_stride),
    is_committed_field_shape_violation_v1(8, shape_verdict_wrong_stride),
    shape_verdict_code_count == 4,
  }
}
