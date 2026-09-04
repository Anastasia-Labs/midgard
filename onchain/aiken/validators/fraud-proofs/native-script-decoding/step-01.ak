//// ## Native-Script Decoding Fault Proof — Step 01 (#635, #633)
////
//// Binds the thread's verdict subject:
////
//// - `BindNormalTransaction` (direction A only): the accepted transaction is
////   bound through the header's counted `transactions_root`. A normal leaf
////   IS an acceptance verdict, so the leaf's embedded validity scalar must
////   claim `TxIsValid` — the §2.4.3(d) predicate.
//// - `RecordForcedSource` (either direction): only the direction is recorded
////   here; step-02 opens the forced leaf and its explicit operator verdict
////   under the header this thread's NFT names.
////
//// The acyclic step chain is 01 → 02 → 03 → (03 self-loop)* → 04, with
//// `ct.Cancel` available at every step.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine
use midgard/fraud_proofs/native_script_decoding/step_01.{
  BindNormalTransaction, Datum, RecordForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/native_tx/types as native_types

validator main(
  step_02_validator_script_hash: ScriptHash,
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
      ct.Continue(BindNormalTransaction { carriage }) -> {
        // 1. Generic validations for passing the provided transaction to the
        //    next step must pass.
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
          carriage,
          own_out_ref,
          tx,
        )

        expect None = m_input_state_data

        // 2. §2.4.3(d): a leaf of the counted `transactions_root` is an
        //    acceptance verdict only when its own embedded validity scalar
        //    claims `TxIsValid`. Direction A's accusation is against exactly
        //    that acceptance.
        expect bad_tx_view.tx_compact.validity_code == 0

        // 3. The thread advances to step-02 carrying the bound subject.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = engine.BindStateV1 {
          direction: engine.direction_wrongful_acceptance,
          source_kind: engine.source_kind_normal,
          verified_tx_id: bad_tx_id,
        }
        expect output_state_data == expected_output_state

        True
      }
      ct.Continue(RecordForcedSource { direction, input_index, output_index }) -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
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

        expect None = m_input_state_data

        // 2. The direction is fixed for the thread's whole life here; the
        //    forced leaf itself — with the verdict this direction disputes —
        //    is opened at step-02 under the thread NFT's header.
        expect or {
            direction == engine.direction_wrongful_acceptance,
            direction == engine.direction_wrongful_rejection,
          }

        // 3. The thread advances to step-02 with the sentinel subject id.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = engine.BindStateV1 {
          direction,
          source_kind: engine.source_kind_forced,
          verified_tx_id: #"",
        }
        expect output_state_data == expected_output_state

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
// Step-01 binds the verdict subject. The normal arm's leaf-is-acceptance
// predicate is the wrongful-acceptance family's entry: an operator who never
// accepted the transaction never committed such a leaf.

fn first_step_datum() -> Datum {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None }
}

fn run_step_01_normal(claimed: fixture.NativeBlockFixtureV1) -> Bool {
  let expected_output_state: Data = engine.BindStateV1 {
    direction: engine.direction_wrongful_acceptance,
    source_kind: engine.source_kind_normal,
    verified_tx_id: claimed.tx_id,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed,
    expected_output_state,
    claimed.transactions_phas_root,
    claimed.tx_id,
    claimed.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(first_step_datum()),
    ct.Continue(
      BindNormalTransaction {
        carriage: fixture.native_inclusion_carriage_v1(claimed),
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

fn run_step_01_forced(direction: Int) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  let expected_output_state: Data = engine.BindStateV1 {
    direction,
    source_kind: engine.source_kind_forced,
    verified_tx_id: #"",
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(first_step_datum()),
    ct.Continue(
      RecordForcedSource { direction, input_index: 0, output_index: 0 },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Positive selector, direction A over a normal subject: the accepted
/// transaction is genuinely committed under the counted `transactions_root`
/// with an accepting validity scalar, so its verified id reaches step-02.
test decoding_step_01_binds_an_accepted_normal_transaction() {
  run_step_01_normal(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
}

/// Positive selector: the forced arm records either direction and forwards
/// the sentinel subject for step-02 to bind the leaf.
test decoding_step_01_records_a_forced_source_for_either_direction() {
  and {
    run_step_01_forced(engine.direction_wrongful_acceptance),
    run_step_01_forced(engine.direction_wrongful_rejection),
  }
}

/// A direction outside {0, 1} never enters the thread.
test decoding_step_01_rejects_an_unknown_direction() fail {
  run_step_01_forced(2)
}

/// §2.4.3(d) edge: a committed leaf whose own validity scalar claims
/// `TxIsInvalid` is not an acceptance, so direction A cannot bind it as one.
test decoding_step_01_rejects_a_normal_leaf_with_an_invalid_scalar() fail {
  let invalid_tx = native_types.MidgardTransaction {
    ..fixture.valid_native_tx_v1(),
    validity: native_types.TxIsInvalid,
  }
  run_step_01_normal(fixture.native_block_fixture_v1(invalid_tx))
}

/// The subject must be committed: a raw transactions root that is not the
/// header's counted `transactions_root` cannot bind.
test decoding_step_01_rejects_a_forged_transactions_root() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_step_01_normal(
    fixture.NativeBlockFixtureV1 {
      ..block,
      transactions_phas_root: #"4040404040404040404040404040404040404040404040404040404040404040",
    },
  )
}

// ## Published-chunk carriage (issue #545)

const adversarial_branch_levels: Int = 22

const adversarial_chunk_count: Int = 2

/// Positive selector: the subject is bound through a 22-level membership
/// proof carried by published chunks; the leaf-is-acceptance predicate is
/// unchanged.
test decoding_step_01_binds_a_published_chunk_membership_proof() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let steps = fixture.adversarial_branch_steps(adversarial_branch_levels)
  let block = fixture.deep_transactions_root_block_fixture_v1(
    base,
    fixture.adversarial_membership_root(base.tx_id, base.source_cbor, steps),
    1_000_000,
  )
  let expected_output_state: Data = engine.BindStateV1 {
    direction: engine.direction_wrongful_acceptance,
    source_kind: engine.source_kind_normal,
    verified_tx_id: block.tx_id,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(first_step_datum()),
    ct.Continue(
      BindNormalTransaction {
        carriage: fixture.published_chunk_inclusion_carriage_v1(
          block,
          adversarial_chunk_count,
        ),
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
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(first_step_datum()),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread: the NFT burns under the prover's
/// signature.
test decoding_step_01_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test decoding_step_01_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
