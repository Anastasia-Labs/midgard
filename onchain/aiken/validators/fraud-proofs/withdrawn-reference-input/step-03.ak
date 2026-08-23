use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{NoDatum, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/withdrawn_reference_input/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/ledger_state.{WithdrawalBody, WithdrawalInfo, WithdrawalIsValid}
use midgard/transition_trace

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
        withdrawal_membership,
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

        expect
            Some(State {
              missing_reference_input,
              blocks_withdrawals_root,
              blocks_withdrawal_count,
            })
          = m_input_state_data

        // 2. The provided withdrawal event must be valid, and it must spend
        //    the reference input that was referenced in the bad transaction.
        expect
            WithdrawalInfo {
              body: WithdrawalBody { l2_outref, .. },
              validity: WithdrawalIsValid,
              ..
            }
          = withdrawal_membership.value
        expect l2_outref.transaction_id == missing_reference_input.tx_id
        expect l2_outref.output_index == missing_reference_input.output_index

        // 3. The withdrawal event must be present in the block's counted
        //    withdrawals root (`WithdrawalsRootDomain` unwrap plus MPF
        //    membership over the canonically serialised key/value).
        expect
          transition_trace.verify_root_membership_with_bytes(
            withdrawal_membership,
            transition_trace.WithdrawalsRootDomain,
            blocks_withdrawals_root,
            blocks_withdrawal_count,
            cbor.serialise(withdrawal_membership.key),
            cbor.serialise(withdrawal_membership.value),
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

fn withdrawal_event_id() -> OutputReference {
  OutputReference {
    transaction_id: #"4242424242424242424242424242424242424242424242424242424242424242",
    output_index: 0,
  }
}

fn withdrawal_info_for(l2_outref: OutputReference) -> WithdrawalInfo {
  WithdrawalInfo {
    body: WithdrawalBody {
      l2_outref,
      l2_owner: fixture.fraud_prover_key_hash,
      l2_value: [],
      l1_address: fixture.fraud_proof_token_address(),
      l1_datum: NoDatum,
    },
    signature: Pair(
      fixture.sample_witness_verification_key(),
      fixture.sample_witness_signature(),
    ),
    validity: WithdrawalIsValid,
  }
}

fn run_finalize(withdrawn_outref: OutputReference) -> Bool {
  let base_tx = fixture.valid_native_tx_v1()
  expect [referenced_input] = base_tx.body.reference_inputs
  let withdrawal_id = withdrawal_event_id()
  let withdrawal_info = withdrawal_info_for(withdrawn_outref)
  let key_bytes = cbor.serialise(withdrawal_id)
  let value_bytes = cbor.serialise(withdrawal_info)
  let withdrawals_phas_root = mpf.root(
    mpf.insert(mpf.empty, key_bytes, value_bytes, []),
  )
  let block = fixture.native_block_fixture_with_withdrawals_v1(
    base_tx,
    withdrawals_phas_root,
    1,
  )
  let state = State {
    missing_reference_input: referenced_input,
    blocks_withdrawals_root: block.header.withdrawals_root,
    blocks_withdrawal_count: 1,
  }
  // The claimed withdrawal always targets the referenced input; whether it is
  // genuinely committed by the withdrawals root depends on `withdrawn_outref`.
  let claimed_info = withdrawal_info_for(
    OutputReference {
      transaction_id: referenced_input.tx_id,
      output_index: referenced_input.output_index,
    },
  )
  let membership = transition_trace.RootMembershipProof {
    domain: transition_trace.WithdrawalsRootDomain,
    root: block.header.withdrawals_root,
    phas_root: withdrawals_phas_root,
    count: 1,
    key: withdrawal_id,
    value: claimed_info,
    proof: [],
  }
  let asset_name = fixture.ct_token_asset_name(block)
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
        withdrawal_membership: membership,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// The withdrawal spending the referenced input is genuinely committed under
/// the block's counted withdrawals root, finalizing the proof.
test withdrawn_reference_input_step_03_accepts_committed_withdrawal() {
  let base_tx = fixture.valid_native_tx_v1()
  expect [referenced_input] = base_tx.body.reference_inputs
  run_finalize(
    OutputReference {
      transaction_id: referenced_input.tx_id,
      output_index: referenced_input.output_index,
    },
  )
}

/// Valid-block negative selector: the block's withdrawals root commits a
/// withdrawal of a different UTxO, so a claimed withdrawal of the referenced
/// input cannot verify and the challenge cannot finalize.
test withdrawn_reference_input_step_03_rejects_valid_block_challenge() fail {
  run_finalize(
    OutputReference {
      transaction_id: #"9999999999999999999999999999999999999999999999999999999999999999",
      output_index: 3,
    },
  )
}
