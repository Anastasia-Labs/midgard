use aiken/builtin
use aiken/collection/list
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  OutputReference, Redeemer, ScriptPurpose, Transaction, Withdraw,
}
use env
use midgard/common/types.{Proof}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/no_input/step_04.{Args, Datum, SpendRedeemer, State}

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
        non_membership_proof_in_txs,
        non_membership_proof_script_redeemer_index,
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

        expect Some(State { missing_input_tx_id, blocks_transactions_root }) = m_input_state_data

        // 2. The missing input must not have been produced in the same block.
        //    The transactions MPF is keyed by the raw 32-byte native tx id, so
        //    the non-membership key is the tx id bytes directly.
        expect
          utils.plutarch_pexcludes_raw(
            merkle_root: blocks_transactions_root,
            key_bytes: missing_input_tx_id,
            non_membership_proof: non_membership_proof_in_txs,
            redeemers: redeemers,
            withdraw_redeemer_index: non_membership_proof_script_redeemer_index,
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

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Step-04 closes the non-existence argument: the input was absent from the
// block's initial ledger (step-03) and no transaction of the same block produced
// it. The delegated `pexcludes` claim is bound to the authenticated raw
// transactions root and to the raw 32-byte native transaction id.

/// The block whose header authenticated the challenged transaction at step-01;
/// its authenticated raw transactions root is what step-03 threads forward.
fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn transactions_root() -> ByteArray {
  challenged_block().transactions_phas_root
}

const missing_input_tx_id =
  #"3131313131313131313131313131313131313131313131313131313131313131"

fn pexcludes_withdraw_redeemer_entry(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
) -> Pair<ScriptPurpose, Redeemer> {
  let non_membership_proof: Proof = []
  let proof_data: Data = non_membership_proof
  let payload: List<Data> = [
    builtin.b_data(merkle_root),
    builtin.b_data(key_bytes),
    proof_data,
  ]
  Pair(
    Withdraw(Script(env.plutarch_pexcludes_validator_hash)),
    builtin.list_data(payload),
  )
}

fn run_finalize(claimed_root: ByteArray, claimed_key: ByteArray) -> Bool {
  let asset_name = fixture.ct_token_asset_name(challenged_block())
  let base = fixture.finalize_step_l1_tx_v1(asset_name)
  let l1_tx = Transaction {
    ..base,
    redeemers: list.concat(
      base.redeemers,
      [pexcludes_withdraw_redeemer_entry(claimed_root, claimed_key)],
    ),
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            missing_input_tx_id,
            blocks_transactions_root: transactions_root(),
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        non_membership_proof_in_txs: [],
        non_membership_proof_script_redeemer_index: 1,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the producing transaction of the challenged input is
/// excluded from the block's authenticated raw transactions root, so the
/// non-existence proof finalizes into a permanent fraud-proof token.
test no_input_step_04_excludes_the_producing_transaction() {
  run_finalize(transactions_root(), missing_input_tx_id)
}

/// Valid-block negative selector: the non-membership claim must be made against
/// the exact transactions root threaded forward from the authenticated header.
/// A prover who obtains a `pexcludes` claim against an unrelated root — the only
/// root under which a genuinely committed producing transaction can look absent
/// — is rejected here.
test no_input_step_04_rejects_valid_block_challenge() fail {
  let unrelated_root = #"4040404040404040404040404040404040404040404040404040404040404040"
  run_finalize(unrelated_root, missing_input_tx_id)
}
