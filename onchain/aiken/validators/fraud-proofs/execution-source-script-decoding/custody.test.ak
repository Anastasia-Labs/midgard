use cardano/transaction.{Transaction}
use fraud_proofs/execution_source_script_decoding/terminal as terminal_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/bind_state.{EventSelectedV1}
use midgard/fraud_proofs/execution_source_script_decoding/terminal
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1.{
  ExecutionNativeScriptDepthLimit, ExecutionNativeScriptFalse,
  ExecutionNativeScriptMalformed, ExecutionNativeScriptNodeLimit,
  RejectionReasonV1,
}

const stranger = #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

fn state(rejection_reason: Option<RejectionReasonV1>) -> EventSelectedV1 {
  let block = fixture.challenged_block_v1()
  let rejected = rejection_reason != None
  EventSelectedV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: if rejected {
        1
      } else {
        0
      },
      source_kind: 1,
      transaction_id: block.tx_id,
      source_key: #"00",
      rejection_reason,
    },
    header: block.header,
    compact_cbor: block.compact_cbor,
    step_index: 0,
  }
}

fn finalized(
  rejection_reason: Option<RejectionReasonV1>,
  mint_redeemer_index: Int,
  signer: List<ByteArray>,
) -> Bool {
  let selected = state(rejection_reason)
  terminal_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(selected),
      },
    ),
    ct.Continue(
      terminal.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: mint_redeemer_index,
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.finalize_step_l1_tx_v1(selected.subject.transaction_id),
      extra_signatories: signer,
    },
  )
}

test terminal_finalizes_each_recorded_arm() {
  and {
    finalized(
      Some(ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 0 }),
      0,
      [],
    ),
    finalized(
      Some(ExecutionNativeScriptNodeLimit { purpose_kind: 1, purpose_index: 3 }),
      0,
      [],
    ),
    finalized(
      Some(
        ExecutionNativeScriptDepthLimit { purpose_kind: 3, purpose_index: 9 },
      ),
      0,
      [],
    ),
  }
}

test terminal_finalization_is_open_to_any_submitter() {
  finalized(
    Some(ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 0 }),
    0,
    [stranger],
  )
}

test terminal_refuses_an_acceptance() {
  !finalized(None, 0, [])
}

test terminal_refuses_another_recorded_arm() {
  !finalized(
    Some(ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index: 0 }),
    0,
    [],
  )
}

test terminal_refuses_a_missing_fraud_proof_mint() fail {
  finalized(
    Some(ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 0 }),
    1,
    [],
  )
}
