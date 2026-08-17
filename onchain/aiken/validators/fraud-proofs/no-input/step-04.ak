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
        non_membership_in_txs,
        fraud_proof_mint_redeemer_index,
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

        expect Some(State { missing_input_tx_id, blocks_transactions_root }) = m_input_state_data

        // 2. The missing input must not have been produced in the same block.
        //    The transactions MPF is keyed by the raw 32-byte native tx id, so
        //    the non-membership key is the tx id bytes directly.
        //    The prover chooses the carriage: redeemer-carried, or published
        //    beforehand as chunks this transaction references (issue #545).
        expect
          verify_non_membership_carried(
            non_membership_in_txs,
            blocks_transactions_root,
            missing_input_tx_id,
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

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Step-04 closes the non-existence argument: the input was absent from the
// block's initial ledger (step-03) and no transaction of the same block produced
// it. The delegated `pexcludes` claim is bound to the authenticated raw
// transactions root and to the raw 32-byte native transaction id.
//
// The carried claims are witness-faithful (#582): the fixture block's raw
// transactions root is a genuine single-leaf MPF, so the positive selector
// carries the real absence witness that leaf reconstructs, and the selectors
// in `validators/pexcludes.ak` execute the very same claim bytes through the
// merkelized `exclusion` validator.

/// The block whose header authenticated the challenged transaction at step-01;
/// its authenticated raw transactions root is what step-03 threads forward.
fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn transactions_root() -> ByteArray {
  challenged_block().transactions_phas_root
}

const missing_input_tx_id = fixture.absent_native_tx_id

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
        data: Some(State { missing_input_tx_id, blocks_transactions_root }),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        non_membership_in_txs: carriage,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

fn run_finalize(
  claimed_root: ByteArray,
  claimed_key: ByteArray,
  non_membership_proof: mpf.Proof,
) -> Bool {
  run_finalize_with(
    transactions_root(),
    RedeemerCarriedNonMembership {
      non_membership_proof,
      non_membership_proof_script_redeemer_index: 1,
    },
    [
      fixture.pexcludes_withdraw_redeemer_entry(
        claimed_root,
        claimed_key,
        non_membership_proof,
      ),
    ],
    [],
  )
}

// ## Published-chunk carriage (issue #545)

/// The absence claim a published-chunk step must find in the merkelized
/// verifier's redeemer.
fn absence_claim(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  indices: List<Int>,
) -> chunked_inclusion_v1.ChunkedProofClaim {
  chunked_inclusion_v1.ChunkedProofClaim {
    mode: chunked_inclusion_v1.NonMembership,
    merkle_root,
    key_bytes,
    value_hash: chunked_inclusion_v1.absent_value_hash,
    ordered_chunk_reference_input_indices: indices,
  }
}

fn run_finalize_published_with_claim(
  blocks_transactions_root: ByteArray,
  indices: List<Int>,
  claim_indices: List<Int>,
  witness: mpf.Proof,
) -> Bool {
  run_finalize_with(
    blocks_transactions_root,
    PublishedChunkNonMembership(
      PublishedProofCarriage { ordered_chunk_reference_input_indices: indices },
    ),
    [
      fixture.chunked_verify_withdraw_redeemer_entry(
        absence_claim(
          blocks_transactions_root,
          missing_input_tx_id,
          claim_indices,
        ),
      ),
    ],
    fixture.proof_chunk_reference_inputs(witness),
  )
}

fn run_finalize_published(
  blocks_transactions_root: ByteArray,
  indices: List<Int>,
  witness: mpf.Proof,
) -> Bool {
  run_finalize_published_with_claim(
    blocks_transactions_root,
    indices,
    indices,
    witness,
  )
}

/// Positive selector: the producing transaction of the challenged input is
/// excluded from the block's authenticated raw transactions root, so the
/// non-existence proof finalizes into a permanent fraud-proof token. The
/// carried witness is the block's real committed leaf, so the claim is one
/// the merkelized `exclusion` validator itself accepts.
test no_input_step_04_excludes_the_producing_transaction() {
  let block = challenged_block()
  and {
    // The absent id's path really diverges from the committed leaf's at the
    // first nibble, which is what the single-leaf witness attests.
    nibble(blake2b_256(missing_input_tx_id), 0) != nibble(
      blake2b_256(block.tx_id),
      0,
    ),
    run_finalize(
      block.transactions_phas_root,
      missing_input_tx_id,
      fixture.transactions_phas_absence_witness(block),
    ),
  }
}

/// Valid-block negative selector: the non-membership claim must be made against
/// the exact transactions root threaded forward from the authenticated header.
/// A prover who obtains a `pexcludes` claim against an unrelated root — the only
/// root under which a genuinely committed producing transaction can look absent
/// — is rejected here.
test no_input_step_04_rejects_valid_block_challenge() fail {
  let unrelated_root = #"4040404040404040404040404040404040404040404040404040404040404040"
  run_finalize(
    unrelated_root,
    missing_input_tx_id,
    fixture.transactions_phas_absence_witness(challenged_block()),
  )
}

/// Positive selector, published-chunk carriage: the same absence claim
/// finalizes with the witness in a referenced publication UTxO.
test no_input_step_04_excludes_the_producing_transaction_through_published_chunks() {
  let (root, witness) = fixture.absence_witness()
  and {
    nibble(blake2b_256(missing_input_tx_id), 0) != nibble(
      fixture.absence_witness_leaf_path(),
      0,
    ),
    run_finalize_published(root, [0], witness),
  }
}

/// The step is bound to the exact claim its redeemer named.
test no_input_step_04_rejects_a_claim_the_redeemer_did_not_name() fail {
  let (root, witness) = fixture.absence_witness()
  run_finalize_published_with_claim(root, [0], [0, 0], witness)
}
