//// ## Fabricated Deposit Fraud Proof — step-02 (`GOAL_SPEC.md` §9.1 / Q39)
////
//// The only step that reads L1. It authenticates the prover's witness about the
//// committed deposit identity and forwards an authenticated verdict, so no later
//// step ever has to trust a claim about the chain.
////
//// Either the committed `DepositId` is still an unspent output — which proves no
//// deposit event with that identity was ever authenticated, because
//// `midgard/user_events`'s `authenticate_new_event` can only mint an event NFT
//// by spending that very output reference — or the authentic event UTxO is
//// exhibited under the hub-registered deposit policy and its datum is committed
//// to by hash.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/fabricated_deposit/step_01
use midgard/fraud_proofs/fabricated_deposit/step_02.{
  AbsentDepositIdentity, Args, Datum, PresentDepositEvent, SpendRedeemer, State,
  verify_deposit_evidence_v1,
}
use midgard/fraud_proofs/fabricated_deposit/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/user_events
use midgard/user_events/deposit

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, evidence }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

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

        expect
            Some(State {
              challenged_header_hash,
              header_start_time,
              header_end_time,
              committed_deposit_id,
              committed_deposit_info_hash,
            })
          = m_input_state_data

        // 2. Authenticate what L1 holds about the committed identity. Every arm
        //    of this either produces an authenticated verdict or fails; the
        //    deposit event policy comes from the hub oracle and the event NFT's
        //    asset name is re-derived from the committed identity, so no
        //    prover-chosen token can stand in for the authentic event.
        let verdict = verify_deposit_evidence_v1(
          hub_oracle,
          committed_deposit_id,
          evidence,
          reference_inputs,
        )

        // 3. The verdict, and everything already authenticated, must be passed
        //    to the next step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          challenged_header_hash,
          header_start_time,
          header_end_time,
          committed_deposit_id,
          committed_deposit_info_hash,
          verdict,
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
// Both L1 witnesses are exercised against the exact thread state step-01's own
// positive selector produces (`step_02.fixture_state_v1`), so the handoff bytes
// are continuous across the family rather than restated per step.

fn run_step_02(
  input_state: State,
  evidence: step_02.DepositEvidenceV1,
  verdict: step_02.DepositEvidenceVerdictV1,
  reference_inputs: List<Input>,
) -> Bool {
  let asset_name = step_01.computation_thread_token_asset_name_v1(
    input_state.challenged_header_hash,
  )
  let expected_output_state: Data = step_03.State {
    challenged_header_hash: input_state.challenged_header_hash,
    header_start_time: input_state.header_start_time,
    header_end_time: input_state.header_end_time,
    committed_deposit_id: input_state.committed_deposit_id,
    committed_deposit_info_hash: input_state.committed_deposit_info_hash,
    verdict,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(Args { input_index: 0, output_index: 0, evidence }),
    fixture.own_out_ref(),
    step_01.continue_step_l1_tx_v1(
      asset_name,
      expected_output_state,
      reference_inputs,
    ),
  )
}

// ### The nonexistent-identity witness

fn absent_identity_state() -> State {
  step_02.fixture_state_v1(step_01.fabricated_identity_block_v1())
}

fn run_absent_identity(witnessed_out_ref: ledger_state.DepositId) -> Bool {
  run_step_02(
    absent_identity_state(),
    AbsentDepositIdentity { unspent_ref_input_index: 0 },
    step_02.DepositIdentityAbsent,
    [step_01.unspent_output_reference_input_v1(witnessed_out_ref)],
  )
}

// ### The present-event witness

fn present_event_state() -> State {
  step_02.fixture_state_v1(step_01.mismatched_content_block_v1())
}

fn authentic_event_datum() -> deposit.DepositDatum {
  step_02.authentic_deposit_datum_v1(
    step_01.authentic_deposit_id_v1(),
    step_01.authentic_deposit_info_v1(),
    step_02.authentic_inclusion_time_v1,
  )
}

fn run_present_event(event_reference_input: Input) -> Bool {
  run_step_02(
    present_event_state(),
    PresentDepositEvent { hub_ref_input_index: 0, event_ref_input_index: 1 },
    step_02.observed_verdict_v1(authentic_event_datum()),
    [fixture.hub_reference_input(), event_reference_input],
  )
}

fn authentic_event_reference_input(event_nft_policy: PolicyId) -> Input {
  step_02.deposit_event_reference_input_v1(
    event_nft_policy,
    user_events.out_ref_to_nonce(step_01.authentic_deposit_id_v1()),
    authentic_event_datum(),
  )
}

/// Positive selector: both admissible L1 witnesses authenticate. The
/// still-unspent output at the committed `DepositId` yields
/// `DepositIdentityAbsent`; the authentic event UTxO under the hub-registered
/// deposit policy yields `DepositEventObserved` with a commitment to the exact
/// event datum bytes and the event's inclusion time.
test accepts_authenticated_l1_deposit_evidence() {
  and {
    run_absent_identity(step_01.fabricated_deposit_id_v1()),
    run_present_event(
      authentic_event_reference_input(step_02.fixture_deposit_event_policy_v1()),
    ),
  }
}

/// The event NFT must be minted under the policy the hub oracle registers.
/// Anyone can mint a token with the right asset name under their own policy, so
/// without this the "authentic event" would be whatever the prover minted.
test rejects_wrong_nft_policy() fail {
  run_present_event(
    authentic_event_reference_input(
      #"1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e",
    ),
  )
}

/// Valid-block negative selector: the absence witness must be the live output at
/// **exactly** the committed `DepositId`. On a valid block that output is spent —
/// the spend is what authenticated the event — so a prover attacking a valid
/// block can only offer some other still-unspent output, and this is where that
/// substitution is refused.
test rejects_wrong_event_outref() fail {
  run_absent_identity(step_01.foreign_deposit_id_v1())
}

/// A reference input carrying a well-formed `DepositDatum` but no event NFT at
/// all is not evidence: any user can build that output. Untrusted material must
/// never reach the verdict.
test rejects_untrusted_evidence() fail {
  run_present_event(
    step_02.untrusted_deposit_datum_reference_input_v1(authentic_event_datum()),
  )
}
