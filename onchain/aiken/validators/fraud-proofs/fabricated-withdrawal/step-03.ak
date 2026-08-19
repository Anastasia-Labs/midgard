//// ## Fabricated Withdrawal Fraud Proof — step-03 (`GOAL_SPEC.md` §9.1 / Q40)
////
//// Classifies the fault. Step-02's verdict about L1 is authenticated, and this
//// step turns it into one of the two named `FabricatedWithdrawal` faults — or
//// refuses, when the verdict does not support a fault at all.
////
//// It reads no reference input. The authentic event's content arrives as the
//// preimage of the 32-byte commitment step-02 retained, which is what lets the
//// dispute continue after the event NFT is burned at settlement or refund, and
//// keeps the thread's L1 footprint independent of the withdrawer's chosen
//// `l2_value`/`l1_datum` sizes.
////
//// One commitment inequality covers all three ways a withdrawal's content can be
//// fabricated — a diverted `body`, a forged `signature`, an overridden `validity`
//// — because a `WithdrawalInfo` is exactly that triple; the positive selector
//// drives all three.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/fraud_proofs/fabricated_withdrawal/step_02
use midgard/fraud_proofs/fabricated_withdrawal/step_03.{
  Args, AuthenticContentOpeningV1, Datum, NoAuthenticContent, RetainedEventDatum,
  SpendRedeemer, State, open_authentic_withdrawal_content_v1,
}
use midgard/fraud_proofs/fabricated_withdrawal/step_04
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/user_events/withdrawal

validator main(
  step_04_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, authentic_content }) -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect Some(input_state) = m_input_state_data

        // 2. Classify the fault. The opening must pair with the verdict, its
        //    bytes must be the ones step-02 committed to, and the authentic
        //    content must genuinely differ from the committed content.
        let fault = open_authentic_withdrawal_content_v1(
          input_state,
          authentic_content,
        )

        // 3. The classified fault, the challenged header's identity and window,
        //    and the committed withdrawal identity must be passed to the next
        //    step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          challenged_header_hash: input_state.challenged_header_hash,
          header_start_time: input_state.header_start_time,
          header_end_time: input_state.header_end_time,
          committed_withdrawal_id: input_state.committed_withdrawal_id,
          fault,
        }
        expect output_state_data == expected_output_state

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
//
// Each selector drives the real `main.spend` handler on the exact thread state
// step-02's positive selector produces, so the family's handoff bytes stay
// continuous from the authenticated header onwards.

fn authentic_event_datum() -> withdrawal.Datum {
  step_02.authentic_withdrawal_datum_v1(
    step_01.authentic_withdrawal_id_v1(),
    step_01.authentic_withdrawal_info_v1(),
    step_02.authentic_inclusion_time_v1,
  )
}

fn state_from(
  block: step_01.FabricatedWithdrawalBlockV1,
  verdict: step_02.WithdrawalEvidenceVerdictV1,
) -> State {
  step_03.fixture_state_v1(step_02.fixture_state_v1(block), verdict)
}

fn run_step_03(
  input_state: State,
  authentic_content: AuthenticContentOpeningV1,
  expected_fault: step_04.FabricatedWithdrawalFaultV1,
) -> Bool {
  let asset_name = step_01.computation_thread_token_asset_name_v1(
    input_state.challenged_header_hash,
  )
  let expected_output_state: Data = step_03.fixture_next_state_v1(
    input_state,
    expected_fault,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(Args { input_index: 0, output_index: 0, authentic_content }),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// The fault a header that committed `committed_info` must be convicted of, given
/// the authentic event's genuine `WithdrawalInfo`.
fn content_mismatch_fault(
  committed_info: ledger_state.WithdrawalInfo,
) -> step_04.FabricatedWithdrawalFaultV1 {
  step_04.MismatchedWithdrawalContent {
    committed_withdrawal_info_hash: utils.serialise_and_hash_32(committed_info),
    authentic_withdrawal_info_hash: utils.serialise_and_hash_32(
      step_01.authentic_withdrawal_info_v1(),
    ),
    event_inclusion_time: step_02.authentic_inclusion_time_v1,
  }
}

fn run_content_mismatch(
  block: step_01.FabricatedWithdrawalBlockV1,
  opened_event_datum: withdrawal.Datum,
  expected_fault: step_04.FabricatedWithdrawalFaultV1,
) -> Bool {
  let opened_event_datum_data: Data = opened_event_datum
  run_step_03(
    state_from(block, step_02.observed_verdict_v1(authentic_event_datum())),
    RetainedEventDatum { event_datum: opened_event_datum_data },
    expected_fault,
  )
}

/// A challenged block committing the authentic withdrawal identity with
/// `committed_info` as its content.
fn block_committing(
  committed_info: ledger_state.WithdrawalInfo,
) -> step_01.FabricatedWithdrawalBlockV1 {
  step_01.fabricated_withdrawal_block_v1(
    step_01.authentic_withdrawal_id_v1(),
    committed_info,
  )
}

fn run_fidelity_variant(committed_info: ledger_state.WithdrawalInfo) -> Bool {
  run_content_mismatch(
    block_committing(committed_info),
    authentic_event_datum(),
    content_mismatch_fault(committed_info),
  )
}

/// Positive selector: both `FabricatedWithdrawal` faults are classified from
/// authenticated material, and the content arm catches a fabrication of *any* of
/// the three `WithdrawalInfo` fields.
///
/// - a diverted `body` (the payout redirected to another L1 address),
/// - a forged `signature` (an authorisation the owner never produced),
/// - an overridden `validity` (the verdict `transition_trace/proof`'s
///   `ValidWithdrawalTransition` arm reads out of the committed leaf),
///
/// each re-open the exact event datum step-02 committed to and show the committed
/// `WithdrawalInfo` commitment differs from the authentic one; the
/// nonexistent-identity arm carries forward the absence verdict with nothing left
/// to open.
test accepts_withdrawal_content_mismatch() {
  and {
    run_fidelity_variant(step_01.diverted_withdrawal_info_v1()),
    run_fidelity_variant(step_01.forged_signature_withdrawal_info_v1()),
    run_fidelity_variant(step_01.overridden_validity_withdrawal_info_v1()),
    run_step_03(
      state_from(
        step_01.fabricated_identity_block_v1(),
        step_02.WithdrawalIdentityAbsent,
      ),
      NoAuthenticContent,
      step_04.NonexistentWithdrawalIdentity,
    ),
  }
}

/// Valid-block negative selector: a header that commits exactly the authentic
/// event's `WithdrawalInfo` — same body, same signature, same validity — has
/// fabricated nothing, and no fault can be classified from it. This is the whole
/// family's valid-block guard on the content arm: the committed commitment and the
/// authentic one are equal, so there is no mismatch to name.
test rejects_exact_info_match() fail {
  run_content_mismatch(
    step_01.authentic_withdrawal_block_v1(),
    authentic_event_datum(),
    content_mismatch_fault(step_01.authentic_withdrawal_info_v1()),
  )
}

/// The opened preimage must be the bytes step-02 authenticated. Here the prover
/// opens an event datum carrying the *diverted* withdrawal body instead of the
/// authentic one — the single edit that would make its own fabrication look like
/// the genuine order — and the hash equality refuses it. Without this check the
/// "authentic content" would be whatever the prover chose to supply, and every
/// content-mismatch conviction would be forgeable in either direction.
test rejects_wrong_body_bytes() fail {
  let tampered = step_02.authentic_withdrawal_datum_v1(
    step_01.authentic_withdrawal_id_v1(),
    step_01.diverted_withdrawal_info_v1(),
    step_02.authentic_inclusion_time_v1,
  )
  run_content_mismatch(
    block_committing(step_01.diverted_withdrawal_info_v1()),
    tampered,
    content_mismatch_fault(step_01.diverted_withdrawal_info_v1()),
  )
}

/// The opening must pair with the verdict it inherited. A prover who found the
/// authentic event may not switch to the `NoAuthenticContent` opening: that would
/// convert a content dispute into a *non-existence* conviction, a strictly stronger
/// claim than the evidence supports, and it would let a real withdrawal order be
/// erased rather than corrected.
test rejects_forged_handoff() fail {
  run_step_03(
    state_from(
      step_01.mismatched_content_block_v1(),
      step_02.observed_verdict_v1(authentic_event_datum()),
    ),
    NoAuthenticContent,
    step_04.NonexistentWithdrawalIdentity,
  )
}
