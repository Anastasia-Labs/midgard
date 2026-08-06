use aiken/builtin
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, OutputReference, Redeemer, ScriptPurpose, Transaction, Withdraw,
}
use env
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/common.{
  NonMembershipCarriage, PublishedChunkNonMembership,
  RedeemerCarriedNonMembership, cancel, continue, verify_non_membership_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_tx_types
use midgard/fraud_proofs/no_input/step_03.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/no_input/step_04

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
              missing_input,
              blocks_prev_utxos_root,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. Target input must be absent from the verified block's prev ledger.
        //    The ledger MPF is keyed by the node's `CML.TransactionInput` CBOR
        //    (a definite 2-element array), so the non-membership key must be
        //    `encode_midgard_tx_input`, NOT `cbor.serialise` of an aiken
        //    `OutputReference` (which serialises as a constructor).
        //    The prover chooses the carriage: the proof may ride in this
        //    transaction's redeemers or have been published beforehand as
        //    chunks this transaction merely references (issue #545).
        expect
          verify_non_membership_carried(
            non_membership_in_ledger,
            blocks_prev_utxos_root,
            encode_midgard_tx_input(missing_input),
            reference_inputs,
            redeemers,
          )

        // 3. Referenced transaction ID, plus the block's transactions root,
        //    must be passed to the next step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          missing_input_tx_id: missing_input.tx_id,
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

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Step-03 is the first of the two non-existence checks: the challenged input
// must be absent from the block's initial ledger (`prev_utxos_root`). The
// delegated `pexcludes` claim is bound to the exact authenticated root and to
// the canonical `MidgardTxInput` CBOR key of the forwarded input, so neither the
// root nor the key can be swapped for a convenient one.

/// The block whose header authenticated the challenged transaction at step-01.
/// Both roots this step consumes are the block's real values, so the delegated
/// non-membership claim is bound to genuine roots rather than placeholders.
fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn prev_utxos_root() -> ByteArray {
  challenged_block().header.prev_utxos_root
}

fn transactions_root() -> ByteArray {
  challenged_block().transactions_phas_root
}

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

fn challenged_input() -> MidgardTxInput {
  MidgardTxInput {
    tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
    output_index: 3,
  }
}

fn run_step_03_with(
  missing_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  carriage: NonMembershipCarriage,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  reference_inputs: List<Input>,
) -> Bool {
  let expected_output_state: Data = step_04.State {
    missing_input_tx_id: missing_input.tx_id,
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
            missing_input,
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

fn run_step_03(
  missing_input: MidgardTxInput,
  claimed_root: ByteArray,
  claimed_key: ByteArray,
) -> Bool {
  run_step_03_with(
    missing_input,
    prev_utxos_root(),
    RedeemerCarriedNonMembership {
      non_membership_proof: [],
      non_membership_proof_script_redeemer_index: 0,
    },
    [pexcludes_withdraw_redeemer_entry(claimed_root, claimed_key)],
    [],
  )
}

// ## Published-chunk carriage (issue #545)
//
// On the chunked route the absence witness is verified here rather than
// delegated, so these selectors build a real one: a canonical divergent
// terminal leaf, and the initial-ledger root that leaf reconstructs.

const absence_witness_value_hash =
  #"6666666666666666666666666666666666666666666666666666666666666666"

fn absence_witness_leaf_path() -> ByteArray {
  blake2b_256(#"a15e17")
}

/// The canonical absence witness for any key whose path diverges from
/// `absence_witness_leaf_path` at the first nibble, and the root it proves.
fn absence_witness() -> (ByteArray, mpf.Proof) {
  let leaf_path = absence_witness_leaf_path()
  (
    combine(suffix(leaf_path, 0), absence_witness_value_hash),
    [mpf.Leaf { skip: 0, key: leaf_path, value: absence_witness_value_hash }],
  )
}

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

fn run_step_03_published_with_claim(
  missing_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  indices: List<Int>,
  claim_indices: List<Int>,
  witness: mpf.Proof,
) -> Bool {
  run_step_03_with(
    missing_input,
    blocks_prev_utxos_root,
    PublishedChunkNonMembership(
      PublishedProofCarriage { ordered_chunk_reference_input_indices: indices },
    ),
    [
      fixture.chunked_verify_withdraw_redeemer_entry(
        absence_claim(
          blocks_prev_utxos_root,
          encode_midgard_tx_input(missing_input),
          claim_indices,
        ),
      ),
    ],
    fixture.proof_chunk_reference_inputs(witness),
  )
}

fn run_step_03_published(
  missing_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  indices: List<Int>,
  witness: mpf.Proof,
) -> Bool {
  run_step_03_published_with_claim(
    missing_input,
    blocks_prev_utxos_root,
    indices,
    indices,
    witness,
  )
}

/// Positive selector: the delegated non-membership claim is made against the
/// authenticated `prev_utxos_root` under the canonical `MidgardTxInput` CBOR key
/// of the forwarded input, so the transaction id reaches step-04.
test no_input_step_03_excludes_the_input_from_the_initial_ledger() {
  let missing_input = challenged_input()
  run_step_03(
    missing_input,
    prev_utxos_root(),
    encode_midgard_tx_input(missing_input),
  )
}

/// Valid-block negative selector: in a valid block the spent input really is in
/// the initial ledger, so the only non-membership claim a prover can obtain is
/// for some other key. Binding the claim to a key that is not the forwarded
/// input's canonical CBOR is rejected here.
test no_input_step_03_rejects_a_non_membership_claim_for_another_key() fail {
  let missing_input = challenged_input()
  let other_key = encode_midgard_tx_input(
    MidgardTxInput { ..missing_input, output_index: 4 },
  )
  run_step_03(missing_input, prev_utxos_root(), other_key)
}

/// Positive selector, published-chunk carriage: the very same absence claim,
/// with the witness living in a referenced publication UTxO and only its index
/// reaching the step redeemer. Nothing else about the step changes.
test no_input_step_03_excludes_the_input_through_published_chunks() {
  let missing_input = challenged_input()
  let (root, witness) = absence_witness()
  and {
    // The witness really is a divergence at the first nibble of the
    // forwarded input's canonical key.
    nibble(blake2b_256(encode_midgard_tx_input(missing_input)), 0) != nibble(
      absence_witness_leaf_path(),
      0,
    ),
    run_step_03_published(missing_input, root, [0], witness),
  }
}

/// The step is bound to the exact claim its redeemer named: a verifier invoked
/// on a different chunk order proves something this step did not ask for.
test no_input_step_03_rejects_a_claim_the_redeemer_did_not_name() fail {
  let (root, witness) = absence_witness()
  run_step_03_published_with_claim(challenged_input(), root, [0], [], witness)
}

/// A published-chunk step that never invokes the merkelized verifier proves
/// nothing at all.
test no_input_step_03_rejects_a_missing_verifier_invocation() fail {
  let (root, witness) = absence_witness()
  run_step_03_with(
    challenged_input(),
    root,
    PublishedChunkNonMembership(
      PublishedProofCarriage { ordered_chunk_reference_input_indices: [0] },
    ),
    [],
    fixture.proof_chunk_reference_inputs(witness),
  )
}
