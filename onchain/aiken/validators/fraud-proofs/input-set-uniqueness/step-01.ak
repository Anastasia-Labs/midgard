//// ## Input-Set Uniqueness Fraud Proof — Step 01
////
//// Binds an operator-ACCEPTED committed transaction whose intra-transaction
//// input sets step-02 will show to violate uniqueness/disjointness — the
//// single-party conversion of the validation machine's InputSets rule
//// (`reject_duplicate_input`, validation-machine-v1.ak).
////
//// The bound leaf must be an **acceptance**: §2.4.3(d), a leaf of the counted
//// `transactions_root` is an acceptance verdict only when its own embedded
//// validity scalar claims `TxIsValid`. An invalid transaction the operator
//// honestly recorded as a no-op therefore never enters this thread, whatever
//// its input sets look like.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/input_set_uniqueness/rule
use midgard/fraud_proofs/input_set_uniqueness/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/input_set_uniqueness/step_02
use midgard/fraud_proofs/input_set_uniqueness/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

validator main(
  step_02_validator_script_hash: ScriptHash,
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
      ct.Continue(Args{source: AcceptedSource { inclusion }}) -> {
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          inclusion,
          own_out_ref,
          tx,
        )

        expect None = m_input_state_data

        // 2. §2.4.3(d): a leaf of the counted `transactions_root` is an
        //    acceptance verdict only when its own embedded validity scalar
        //    claims `TxIsValid`. The scalar is read off the exact compact
        //    bytes the counted root committed — the leaf this callback just
        //    re-checked — so the value is committed by the *root*, not merely
        //    the §3 anchor. An honestly-rejected no-op leaf refuses here.
        expect bad_tx_view.tx_compact.validity_code == 0

        // 3. The UTxO must be produced at next step's script hash carrying
        //    the bad transaction's §2.5 anchor. Step-02 opens fields 0/1
        //    through the door against that anchor and concludes on item byte
        //    equality, so nothing else needs to be threaded forward.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State { bad_tx_id }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
      ct.Continue(Args{source: ForcedSource {
        input_index,
        output_index,
        header,
        membership,
      }}) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_name, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect None = input_state
        let subject = substrate.bind_forced_subject_to_thread_v1(
          thread_name,
          header,
          membership,
          substrate.direction_wrongful_rejection,
        )
        let bound = rule.bind_forced_duplicate_reason_v1(subject)
        let expected: Data = step_03.State { bound }
        and {
          output_hash == step_03_validator_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

const dup_outref_tx_id =
  #"1111111111111111111111111111111111111111111111111111111111111111"

fn outref(output_index: Int) -> MidgardTxInput {
  MidgardTxInput { tx_id: dup_outref_tx_id, output_index }
}

/// A transaction naming one out-ref twice in field 0 — non-adjacent, since a
/// committed (accepted-but-invalid) transaction owes no ordering.
fn duplicate_spend_bad_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [outref(7), outref(3), outref(7)],
    },
  }
}

fn run_step_01(claimed: fixture.NativeBlockFixtureV1) -> Bool {
  let expected_output_state: Data = step_02.State { bad_tx_id: claimed.tx_id }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed,
    expected_output_state,
    claimed.transactions_phas_root,
    claimed.tx_id,
    claimed.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.native_inclusion_carriage_v1(claimed),
        },
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive binding selector: an accepted native-V1 transaction with a
/// duplicated spend input, genuinely committed under the block's counted
/// `transactions_root`, passes the first step and forwards its §2.5 anchor.
test input_set_uniqueness_step_01_binds_an_accepted_duplicate_input_tx() {
  run_step_01(fixture.native_block_fixture_v1(duplicate_spend_bad_tx()))
}

/// §2.4.3(d) edge — the family's acceptance guard: a committed leaf whose own
/// validity scalar claims `TxIsInvalid` was honestly recorded as a no-op, and
/// never convicts here no matter how degenerate its input sets are.
test input_set_uniqueness_step_01_rejects_an_honestly_rejected_leaf() fail {
  let no_op_tx = native_types.MidgardTransaction {
    ..duplicate_spend_bad_tx(),
    validity: native_types.TxIsInvalid,
  }
  run_step_01(fixture.native_block_fixture_v1(no_op_tx))
}

/// Valid-block negative selector: a raw transactions root that is not the one
/// committed by the header's counted `transactions_root` cannot bind, so
/// fabricated evidence against a valid block is rejected at step one.
test input_set_uniqueness_step_01_rejects_a_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(duplicate_spend_bad_tx())
  // The prover claims the forged bad tx under the valid block's header by
  // supplying the forged raw root; the counted-root unwrap must reject it.
  run_step_01(
    fixture.NativeBlockFixtureV1 {
      ..forged_block,
      header: valid_block.header,
      header_hash: valid_block.header_hash,
    },
  )
}

// ## Published-chunk carriage (issue #545)

const adversarial_branch_levels: Int = 22

const adversarial_chunk_count: Int = 2

/// Positive selector: the subject is bound through a 22-level membership
/// proof carried by published chunks; the step's own semantics — the
/// acceptance guard included — are unchanged.
test input_set_uniqueness_step_01_binds_a_published_chunk_membership_proof() {
  let base = fixture.native_block_fixture_v1(duplicate_spend_bad_tx())
  let steps = fixture.adversarial_branch_steps(adversarial_branch_levels)
  let block = fixture.deep_transactions_root_block_fixture_v1(
    base,
    fixture.adversarial_membership_root(base.tx_id, base.source_cbor, steps),
    1_000_000,
  )
  let expected_output_state: Data = step_02.State { bad_tx_id: block.tx_id }
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.published_chunk_inclusion_carriage_v1(
            block,
            adversarial_chunk_count,
          ),
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_chunked_l1_tx_v1(
      block,
      expected_output_state,
      fixture.proof_chunk_reference_inputs(steps),
      fixture.chunked_membership_claim_v1(block, adversarial_chunk_count),
    ),
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread: the NFT burns under the prover's
/// signature.
test input_set_uniqueness_step_01_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test input_set_uniqueness_step_01_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
