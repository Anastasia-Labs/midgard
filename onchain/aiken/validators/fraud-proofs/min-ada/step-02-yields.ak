use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, outputs_field_index}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/min_ada/step_01.{MinAdaTx}
use midgard/fraud_proofs/min_ada/step_02.{Args, State, YieldRedeemer}
use midgard/fraud_proofs/min_ada/step_03 as step_03
use midgard/fraud_proofs/min_ada/yield.{unique_dispatch}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/transaction_output_non_canonical/rule as scan_rule
use midgard/native_tx_machine_walk_v1 as walk

validator tx(
  dispatcher_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(
    _yield_redeemer: YieldRedeemer,
    _credential: Credential,
    transaction: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = transaction
    let (step_datum, spend_redeemer, _own_out_ref) = unique_dispatch(
      dispatcher_script_hash,
      inputs,
      redeemers,
    )
    expect
        ct.Continue(Args {
          input_index: _,
          output_index,
          yield_to_ref_input_index: _,
          outputs_opening: opening,
          grammar_checkpoint_bytes,
          walk_checkpoint_bytes,
        })
      = spend_redeemer
    expect Some(state) = step_datum.data
    let MinAdaTx { output_index: bad_output_index } = state.fault
    let anchor = BodyAnchor {
      tx_id: state.bad_tx_id,
      source_kind: state.source_kind,
    }
    expect Some(next_output) = outputs |> list.at(output_index)
    expect InlineDatum(next_datum_data) = next_output.datum
    expect next_datum: ct.StepDatum<Data> = next_datum_data
    expect Some(output_state) = next_datum.data
    if !state.grammar_complete {
      expect state.walk_checkpoint_hash == #""
      let Pair(view, prior) = if state.grammar_checkpoint_hash == #"" {
        expect grammar_checkpoint_bytes == #""
        opening.opened_field_grammar_certification(
          opening,
          anchor,
          outputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
      } else {
        opening.resume_opened_field_grammar_certification(
          opening,
          anchor,
          outputs_field_index,
          state.grammar_checkpoint_hash,
          grammar_checkpoint_bytes,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
      }
      let next = walk.certify_field_grammar(view, prior, 32)
      let expected: Data = State {
        ..state,
        grammar_checkpoint_hash: walk.field_grammar_checkpoint_hash(next),
        grammar_complete: walk.field_grammar_is_complete(next),
      }
      expect
        next_output.address.payment_credential == Script(dispatcher_script_hash)
      output_state == expected
    } else {
      let Pair(view, prior) = if state.walk_checkpoint_hash == #"" {
        expect walk_checkpoint_bytes == #""
        opening.opened_certified_field_walk_from_grammar(
          opening,
          anchor,
          outputs_field_index,
          state.grammar_checkpoint_hash,
          grammar_checkpoint_bytes,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
      } else {
        opening.resume_opened_field_walk(
          opening,
          anchor,
          outputs_field_index,
          state.walk_checkpoint_hash,
          walk_checkpoint_bytes,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
      }
      let remaining = bad_output_index - walk.walk_next_item_index(prior)
      expect remaining >= 0
      if remaining >= 32 {
        let next = walk.walk_skip(view, prior, 32)
        let expected: Data = State {
          ..state,
          walk_checkpoint_hash: walk.field_walk_checkpoint_hash(next),
        }
        expect
          next_output.address.payment_credential == Script(
            dispatcher_script_hash,
          )
        output_state == expected
      } else {
        let Pair(output_cbor, _) = walk.walk_next(
          view,
          walk.walk_skip(view, prior, remaining),
        )
        let expected: Data = step_03.MinAdaTxScan {
          direction: state.direction,
          scan: scan_rule.initial_scan_v1(
            scan_rule.BoundOutputV1 {
              subject: substrate.VerdictSubjectV1 {
                version: 1,
                direction: 0,
                // This reused output-grammar scanner carries no verdict claim.
                // Its neutral subject stays canonical; the authenticated forced
                // direction remains in the surrounding MinAdaTxScan state.
                source_kind: 0,
                transaction_id: state.bad_tx_id,
                source_key: #"",
                rejection_reason: None,
              },
              output_index: bad_output_index,
            },
            output_cbor,
          ),
        }
        output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
