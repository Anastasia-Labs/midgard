use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{nibble}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, OutputReference, Redeemer, ScriptPurpose, Transaction,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/common.{
  NonMembershipCarriage, PublishedChunkNonMembership,
  RedeemerCarriedNonMembership, cancel, finalize, verify_non_membership_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/no_reference_input/step_04.{
  Args, Datum, SpendRedeemer, State,
}

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
        non_membership_in_txs,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

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
              missing_reference_input_tx_id,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. The missing reference input must not have been produced in the
        //    same block. The transactions MPF is keyed by the raw 32-byte
        //    native tx id, so the non-membership key is the tx id bytes
        //    directly.
        expect
          verify_non_membership_carried(
            non_membership_in_txs,
            blocks_transactions_root,
            missing_reference_input_tx_id,
            reference_inputs,
            redeemers,
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

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

const missing_reference_input_tx_id = fixture.absent_native_tx_id

fn run_finalize_with(
  blocks_transactions_root: ByteArray,
  carriage: NonMembershipCarriage,
  extra_redeemers: Pairs<ScriptPurpose, Redeemer>,
  reference_inputs: List<Input>,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(challenged_block())
  let base = fixture.finalize_step_l1_tx_v1(asset_name)
  let l1_tx = Transaction {
    ..base,
    reference_inputs,
    redeemers: list.concat(base.redeemers, extra_redeemers),
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State { missing_reference_input_tx_id, blocks_transactions_root },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        non_membership_in_txs: carriage,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

fn run_finalize_direct(
  root: ByteArray,
  claimed_root: ByteArray,
  claimed_key: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  run_finalize_with(
    root,
    RedeemerCarriedNonMembership {
      non_membership_proof: proof,
      non_membership_proof_script_redeemer_index: 1,
    },
    [
      fixture.pexcludes_withdraw_redeemer_entry(
        claimed_root,
        claimed_key,
        proof,
      ),
    ],
    [],
  )
}

fn absence_claim(
  root: ByteArray,
  indices: List<Int>,
) -> chunked_inclusion_v1.ChunkedProofClaim {
  chunked_inclusion_v1.ChunkedProofClaim {
    mode: chunked_inclusion_v1.NonMembership,
    merkle_root: root,
    key_bytes: missing_reference_input_tx_id,
    value_hash: chunked_inclusion_v1.absent_value_hash,
    ordered_chunk_reference_input_indices: indices,
  }
}

fn run_finalize_published_with_claim(
  root: ByteArray,
  carriage_indices: List<Int>,
  claim_indices: List<Int>,
  proof: mpf.Proof,
) -> Bool {
  run_finalize_with(
    root,
    PublishedChunkNonMembership(
      PublishedProofCarriage {
        ordered_chunk_reference_input_indices: carriage_indices,
      },
    ),
    [
      fixture.chunked_verify_withdraw_redeemer_entry(
        absence_claim(root, claim_indices),
      ),
    ],
    fixture.proof_chunk_reference_inputs(proof),
  )
}

test no_reference_input_step_04_accepts_fitting_direct_non_membership() {
  let block = challenged_block()
  and {
    nibble(blake2b_256(missing_reference_input_tx_id), 0) != nibble(
      blake2b_256(block.tx_id),
      0,
    ),
    run_finalize_direct(
      block.transactions_phas_root,
      block.transactions_phas_root,
      missing_reference_input_tx_id,
      fixture.transactions_phas_absence_witness(block),
    ),
  }
}

test no_reference_input_step_04_accepts_maximum_published_non_membership() {
  let proof = fixture.adversarial_non_membership_steps(63)
  let root = fixture.adversarial_non_membership_root(
    missing_reference_input_tx_id,
    proof,
  )
  run_finalize_published_with_claim(root, [0, 1, 2, 3], [0, 1, 2, 3], proof)
}

test no_reference_input_step_04_rejects_reordered_published_claim() fail {
  let proof = fixture.adversarial_non_membership_steps(63)
  let root = fixture.adversarial_non_membership_root(
    missing_reference_input_tx_id,
    proof,
  )
  run_finalize_published_with_claim(root, [0, 1, 2, 3], [0, 2, 1, 3], proof)
}

test no_reference_input_step_04_rejects_direct_claim_for_another_key() fail {
  let block = challenged_block()
  run_finalize_direct(
    block.transactions_phas_root,
    block.transactions_phas_root,
    #"00",
    fixture.transactions_phas_absence_witness(block),
  )
}
