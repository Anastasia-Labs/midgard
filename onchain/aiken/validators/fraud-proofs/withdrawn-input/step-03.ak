use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{NoDatum, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/withdrawn_input/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/ledger_state.{
  SpentWithdrawalUtxo, WithdrawalBody, WithdrawalInfo, WithdrawalIsValid,
  WithdrawalValidity,
}
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
              withdrawn_input,
              blocks_withdrawals_root,
              blocks_withdrawal_count,
            })
          = m_input_state_data

        // 2. The provided withdrawal event must be valid, and it must spend
        //    the input that the bad transaction also spends. An invalid
        //    withdrawal (any `validity` other than `WithdrawalIsValid`) never
        //    consumed the L2 UTxO, so it convicts nothing.
        expect
            WithdrawalInfo {
              body: WithdrawalBody { l2_outref, .. },
              validity: WithdrawalIsValid,
              ..
            }
          = withdrawal_membership.value
        expect l2_outref.transaction_id == withdrawn_input.tx_id
        expect l2_outref.output_index == withdrawn_input.output_index

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

fn withdrawal_info_for(
  l2_outref: OutputReference,
  validity: WithdrawalValidity,
) -> WithdrawalInfo {
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
    validity,
  }
}

/// Runs the finalization step against a block whose counted withdrawals root
/// commits exactly one withdrawal leaf: a withdrawal of `withdrawn_outref`
/// carrying `committed_validity`. The claimed membership always targets the
/// disputed transaction's spend input with the same validity, so whether it
/// verifies depends on `withdrawn_outref`, and whether it convicts depends on
/// `committed_validity`.
fn run_finalize(
  withdrawn_outref: OutputReference,
  committed_validity: WithdrawalValidity,
) -> Bool {
  let base_tx = fixture.valid_native_tx_v1()
  expect [spent_input] = base_tx.body.inputs
  let withdrawal_id = withdrawal_event_id()
  let withdrawal_info = withdrawal_info_for(
    withdrawn_outref,
    committed_validity,
  )
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
    withdrawn_input: spent_input,
    blocks_withdrawals_root: block.header.withdrawals_root,
    blocks_withdrawal_count: 1,
  }
  // The claimed withdrawal always targets the spent input; whether it is
  // genuinely committed by the withdrawals root depends on `withdrawn_outref`
  // and `committed_validity`.
  let claimed_info = withdrawal_info_for(
    OutputReference {
      transaction_id: spent_input.tx_id,
      output_index: spent_input.output_index,
    },
    committed_validity,
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

fn spent_input_outref() -> OutputReference {
  let base_tx = fixture.valid_native_tx_v1()
  expect [spent_input] = base_tx.body.inputs
  OutputReference {
    transaction_id: spent_input.tx_id,
    output_index: spent_input.output_index,
  }
}

/// The valid withdrawal spending the disputed transaction's input is genuinely
/// committed under the block's counted withdrawals root, finalizing the proof.
test withdrawn_input_step_03_accepts_committed_withdrawal() {
  run_finalize(spent_input_outref(), WithdrawalIsValid)
}

/// Valid-block negative selector: the block's withdrawals root commits a
/// withdrawal of a different UTxO, so a claimed withdrawal of the spent input
/// cannot verify and the challenge cannot finalize.
test withdrawn_input_step_03_rejects_valid_block_challenge() fail {
  run_finalize(
    OutputReference {
      transaction_id: #"9999999999999999999999999999999999999999999999999999999999999999",
      output_index: 3,
    },
    WithdrawalIsValid,
  )
}

/// Invalid-withdrawal negative selector: the committed leaf targets the spent
/// input but the operator marked it invalid (`SpentWithdrawalUtxo`), so the
/// withdrawal never consumed the L2 UTxO. The membership witness verifies,
/// yet the validity gate refuses to convict.
test withdrawn_input_step_03_rejects_invalid_withdrawal_evidence() fail {
  run_finalize(
    spent_input_outref(),
    SpentWithdrawalUtxo {
      l2_tx_id: #"7777777777777777777777777777777777777777777777777777777777777777",
    },
  )
}
