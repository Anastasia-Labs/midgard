use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{nibble}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, OutputReference, Redeemer, ScriptPurpose, Transaction,
}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/common.{
  NonMembershipCarriage, PublishedChunkNonMembership,
  RedeemerCarriedNonMembership, cancel, continue, verify_non_membership_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_tx_types
use midgard/fraud_proofs/no_reference_input/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/no_reference_input/step_04

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
      ct.Continue(Args { input_index, output_index, non_membership_in_ledger }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

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
              missing_reference_input,
              blocks_prev_utxos_root,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. Target reference input must be absent from the verified block's
        //    prev ledger. The ledger MPF is keyed by the node's
        //    `CML.TransactionInput` CBOR (a definite 2-element array), so the
        //    non-membership key must be `encode_midgard_tx_input`, NOT
        //    `cbor.serialise` of an aiken `OutputReference` (which serialises
        //    as a constructor).
        expect
          verify_non_membership_carried(
            non_membership_in_ledger,
            blocks_prev_utxos_root,
            encode_midgard_tx_input(missing_reference_input),
            reference_inputs,
            redeemers,
          )

        // 3. Reference input's transaction ID, plus the block's transactions
        //    root, must be passed to the next step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          missing_reference_input_tx_id: missing_reference_input.tx_id,
          blocks_transactions_root,
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

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn transactions_root() -> ByteArray {
  challenged_block().transactions_phas_root
}

fn challenged_reference_input() -> MidgardTxInput {
  fixture.absent_midgard_tx_input()
}

fn run_step_03_with(
  missing_reference_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  carriage: NonMembershipCarriage,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  reference_inputs: List<Input>,
) -> Bool {
  let expected_output_state: Data = step_04.State {
    missing_reference_input_tx_id: missing_reference_input.tx_id,
    blocks_transactions_root: transactions_root(),
  }
  let asset_name = fixture.ct_token_asset_name(challenged_block())
  let l1_tx = Transaction {
    ..transaction.placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [fixture.next_step_output(asset_name, expected_output_state)],
    reference_inputs,
    redeemers,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            missing_reference_input,
            blocks_prev_utxos_root,
            blocks_transactions_root: transactions_root(),
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        non_membership_in_ledger: carriage,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

fn run_step_03_direct(
  missing_reference_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  claimed_root: ByteArray,
  claimed_key: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  run_step_03_with(
    missing_reference_input,
    blocks_prev_utxos_root,
    RedeemerCarriedNonMembership {
      non_membership_proof: proof,
      non_membership_proof_script_redeemer_index: 0,
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
  key: ByteArray,
  indices: List<Int>,
) -> chunked_inclusion_v1.ChunkedProofClaim {
  chunked_inclusion_v1.ChunkedProofClaim {
    mode: chunked_inclusion_v1.NonMembership,
    merkle_root: root,
    key_bytes: key,
    value_hash: chunked_inclusion_v1.absent_value_hash,
    ordered_chunk_reference_input_indices: indices,
  }
}

fn run_step_03_published_with_claim(
  missing_reference_input: MidgardTxInput,
  root: ByteArray,
  carriage_indices: List<Int>,
  claim_indices: List<Int>,
  proof: mpf.Proof,
) -> Bool {
  run_step_03_with(
    missing_reference_input,
    root,
    PublishedChunkNonMembership(
      PublishedProofCarriage {
        ordered_chunk_reference_input_indices: carriage_indices,
      },
    ),
    [
      fixture.chunked_verify_withdraw_redeemer_entry(
        absence_claim(
          root,
          encode_midgard_tx_input(missing_reference_input),
          claim_indices,
        ),
      ),
    ],
    fixture.proof_chunk_reference_inputs(proof),
  )
}

test no_reference_input_step_03_accepts_fitting_direct_non_membership() {
  let missing_reference_input = challenged_reference_input()
  let (root, proof) = fixture.absence_witness()
  and {
    nibble(blake2b_256(encode_midgard_tx_input(missing_reference_input)), 0) != nibble(
      fixture.absence_witness_leaf_path(),
      0,
    ),
    run_step_03_direct(
      missing_reference_input,
      root,
      root,
      encode_midgard_tx_input(missing_reference_input),
      proof,
    ),
  }
}

test no_reference_input_step_03_accepts_genesis_empty_direct_non_membership() {
  let missing_reference_input = challenged_reference_input()
  run_step_03_direct(
    missing_reference_input,
    env.empty_merkle_tree_root,
    env.empty_merkle_tree_root,
    encode_midgard_tx_input(missing_reference_input),
    [],
  )
}

test no_reference_input_step_03_accepts_maximum_published_non_membership() {
  let missing_reference_input = challenged_reference_input()
  let proof = fixture.adversarial_non_membership_steps(63)
  let root = fixture.adversarial_non_membership_root(
    encode_midgard_tx_input(missing_reference_input),
    proof,
  )
  run_step_03_published_with_claim(
    missing_reference_input,
    root,
    [0, 1, 2, 3],
    [0, 1, 2, 3],
    proof,
  )
}

test no_reference_input_step_03_rejects_reordered_published_claim() fail {
  let missing_reference_input = challenged_reference_input()
  let proof = fixture.adversarial_non_membership_steps(63)
  let root = fixture.adversarial_non_membership_root(
    encode_midgard_tx_input(missing_reference_input),
    proof,
  )
  run_step_03_published_with_claim(
    missing_reference_input,
    root,
    [0, 1, 2, 3],
    [0, 2, 1, 3],
    proof,
  )
}

test no_reference_input_step_03_rejects_direct_claim_for_another_key() fail {
  let missing_reference_input = challenged_reference_input()
  run_step_03_direct(
    missing_reference_input,
    env.empty_merkle_tree_root,
    env.empty_merkle_tree_root,
    #"00",
    [],
  )
}
