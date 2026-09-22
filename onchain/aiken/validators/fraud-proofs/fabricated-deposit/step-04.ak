//// ## Fabricated Deposit Fraud Proof — step-04 (`GOAL_SPEC.md` §9.1 / Q39)
////
//// Finalizes the thread into a permanent fraud-proof token, and is therefore the
//// last place the family can refuse. It re-checks the two things a permanent
//// conviction must not get wrong: that the fault is filed against the header the
//// thread token names, and that the carried fault is really a fault.

use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/fabricated_deposit/step_01
use midgard/fraud_proofs/fabricated_deposit/step_02
use midgard/fraud_proofs/fabricated_deposit/step_03
use midgard/fraud_proofs/fabricated_deposit/step_04.{
  Args, Datum, SpendRedeemer, fabricated_deposit_fault_is_established_v1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/user_events/deposit

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
            computation_thread_token_asset_name,
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

        expect Some(state) = m_input_state_data

        // 2. The fraud-proof token inherits the thread token's asset name, so
        //    that name is the permanent record of what was proven and against
        //    which block. Both halves are re-checked here: the category prefix
        //    (this family's rule) and the 28-byte suffix (the challenged header).
        expect
          step_01.fraud_category_is_fabricated_deposit_v1(
            computation_thread_token_asset_name,
          )
        expect
          state.challenged_header_hash == step_01.challenged_header_hash_of_v1(
            computation_thread_token_asset_name,
          )

        // 3. The carried fault must really be a fabricated-deposit fault.
        expect fabricated_deposit_fault_is_established_v1(state)

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
// step-03's positive selector produces, so the finalized conviction is measured
// against the same bytes the earlier steps authenticated.

fn authentic_event_datum() -> deposit.DepositDatum {
  step_02.authentic_deposit_datum_v1(
    step_01.authentic_deposit_id_v1(),
    step_01.authentic_deposit_info_v1(),
    step_02.authentic_inclusion_time_v1,
  )
}

fn state_from(
  block: step_01.FabricatedDepositBlockV1,
  verdict: step_02.DepositEvidenceVerdictV1,
  fault: step_04.FabricatedDepositFaultV1,
) -> step_04.State {
  step_03.fixture_next_state_v1(
    step_03.fixture_state_v1(step_02.fixture_state_v1(block), verdict),
    fault,
  )
}

fn mismatch_state(
  block: step_01.FabricatedDepositBlockV1,
  committed_info: ledger_state.DepositInfo,
  event_inclusion_time: PosixTime,
) -> step_04.State {
  state_from(
    block,
    step_02.observed_verdict_v1(authentic_event_datum()),
    step_04.MismatchedDepositContent {
      committed_deposit_info_hash: utils.serialise_and_hash_32(committed_info),
      authentic_deposit_info_hash: utils.serialise_and_hash_32(
        step_01.authentic_deposit_info_v1(),
      ),
      event_inclusion_time,
    },
  )
}

fn nonexistent_identity_state() -> step_04.State {
  state_from(
    step_01.fabricated_identity_block_v1(),
    step_02.DepositIdentityAbsent,
    step_04.NonexistentDepositIdentity,
  )
}

fn run_step_04(state: step_04.State, thread_asset_name: AssetName) -> Bool {
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
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
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(thread_asset_name),
  )
}

fn run_own_thread(state: step_04.State) -> Bool {
  run_step_04(
    state,
    step_01.computation_thread_token_asset_name_v1(state.challenged_header_hash),
  )
}

/// Positive selector: both `FabricatedDeposit` faults finalize into a permanent
/// fraud-proof token under this family's category. The content-mismatch arm
/// carries two different content commitments and an event that was due for the
/// challenged block; the nonexistent-identity arm carries the absence verdict
/// step-02 authenticated.
test accepts_exact_fabricated_deposit_fault() {
  and {
    run_own_thread(
      mismatch_state(
        step_01.mismatched_content_block_v1(),
        step_01.diverted_deposit_info_v1(),
        step_02.authentic_inclusion_time_v1,
      ),
    ),
    run_own_thread(nonexistent_identity_state()),
  }
}

/// Valid-block negative selector: a block that committed exactly the authentic
/// deposit is not convictable. Even if such a thread somehow reached the final
/// step, the "mismatch" it carries names the same content commitment twice, and
/// no permanent token is minted for it.
test rejects_authentic_valid_deposit() fail {
  run_own_thread(
    mismatch_state(
      step_01.authentic_deposit_block_v1(),
      step_01.authentic_deposit_info_v1(),
      step_02.authentic_inclusion_time_v1,
    ),
  )
}

/// The authentic event must have been due for the challenged block:
/// `start_time < inclusion_time <= end_time`, the window
/// `midgard/fraud_proofs/transition_trace/proof` applies to a timed L1 event.
/// An event that became due after the block ended is not evidence that *this*
/// block fabricated a deposit — whether it should have been included at all is
/// the inclusion-window family's rule, and convicting on it here would let one
/// fault be proven twice under two categories.
test rejects_stale_evidence() fail {
  run_own_thread(
    mismatch_state(
      step_01.mismatched_content_block_v1(),
      step_01.diverted_deposit_info_v1(),
      stale_inclusion_time,
    ),
  )
}

/// The fault must be filed against the header the thread token names. The minted
/// fraud-proof token inherits that asset name, so a thread whose state names a
/// different block would publish its conviction against the wrong header — here
/// the valid block's, which is exactly the substitution that would matter.
test rejects_wrong_source_identity() fail {
  run_step_04(
    mismatch_state(
      step_01.mismatched_content_block_v1(),
      step_01.diverted_deposit_info_v1(),
      step_02.authentic_inclusion_time_v1,
    ),
    step_01.block_thread_asset_name_v1(step_01.authentic_deposit_block_v1()),
  )
}

/// One millisecond past the challenged header's `end_time` of 20.
const stale_inclusion_time: PosixTime = 21
