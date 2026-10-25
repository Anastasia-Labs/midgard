use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/min_fee/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/min_fee/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/compact.{
  decode_forced_tx_compact_v1, verify_forced_tx_compact_cbor_v1,
  verify_native_tx_compact_cbor_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  ForcedTxCompact, MidgardTransaction, TxIsInvalid,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  FeeBelowMinimum, ForcedTxInvalid, ForcedTxValid, InputNotFound,
  OperatorVerdictV1,
}

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
      ct.Continue(Args{source: AcceptedSource { inclusion: tx_inclusion_args }}) -> {
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
            _own_script_hash,
            _,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data,
            header,
            bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        // 2. The UTxO must be produced at next step's script hash with the
        //    verified native compact transaction, its inline body fee, the
        //    verified transaction id, and the disputed header's own fee
        //    schedule.
        //
        //    Every one of these is read off evidence this step just
        //    authenticated — the compact structure and its id from the
        //    counted `transactions_root` leaf, `min_fee_a` / `min_fee_b`
        //    from the state-queue header the thread's asset name binds —
        //    which is what entitles step-02 to treat its state as anchored.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          subject: substrate.open_accepted_subject_v1(bad_tx_view),
          bad_tx: ForcedTxCompact {
            body: bad_tx_view.tx_compact.body,
            witness_set_hash: bad_tx_view.tx_compact.witness_set_hash,
          },
          bad_tx_body_fee: bad_tx_view.tx_compact.body.fee,
          bad_tx_id,
          min_fee_a: header.min_fee_a,
          min_fee_b: header.min_fee_b,
        }
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
        expect Some(step_datum) = datum
        let _, thread_name, _, _, output_script_hash, output_state_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        let Pair(subject, _) = substrate.open_forced_subject_v1(
          thread_name,
          header,
          membership,
        )
        // The opener built this subject from the authenticated leaf, so it is
        // canonical by construction and a reason is present exactly on a
        // rejecting leaf. A rejection must name this family's reason.
        expect
          when subject.rejection_reason is {
            None -> True
            Some(reason) -> reason == FeeBelowMinimum
          }
        // The opener also verified that these compact bytes hash to the
        // leaf's transaction id, so they are only decoded here.
        let leaf = membership.value
        let bad_tx = decode_forced_tx_compact_v1(
          leaf.submitted_source.compact_cbor,
        )
        let expected: Data = step_02.State {
          subject,
          bad_tx,
          bad_tx_body_fee: bad_tx.body.fee,
          bad_tx_id: leaf.tx_id,
          min_fee_a: header.min_fee_a,
          min_fee_b: header.min_fee_b,
        }
        and {
          output_script_hash == step_02_validator_script_hash,
          output_state_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn expected_state_for(block: fixture.NativeBlockFixtureV1) -> Data {
  let state: Data = step_02.State {
    subject: substrate.open_subject_v1(
      fixture.ct_token_asset_name(block),
      substrate.AcceptedSource {
        header: block.header,
        tx_inclusion: verify_native_tx_compact_cbor_v1(
          block.tx_id,
          block.compact_cbor,
        ),
      },
    ),
    bad_tx: ForcedTxCompact {
      body: block.compact.body,
      witness_set_hash: block.compact.witness_set_hash,
    },
    bad_tx_body_fee: block.compact.body.fee,
    bad_tx_id: block.tx_id,
    min_fee_a: block.header.min_fee_a,
    min_fee_b: block.header.min_fee_b,
  }
  state
}

fn run_continue(block: fixture.NativeBlockFixtureV1, output_state: Data) -> Bool {
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.native_inclusion_carriage_v1(block),
        },
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive binding selector: a native-V1 transaction genuinely committed
/// under the block's counted `transactions_root` passes the first step and
/// forwards its verified compact form, inline fee, transaction id, and the
/// header's fee schedule.
test min_fee_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_continue(block, expected_state_for(block))
}

/// The forwarded schedule is the committed header's, not the redeemer's: a
/// non-zero `min_fee_a` / `min_fee_b` pair travels because the header hash the
/// thread's asset name binds re-derives over those very fields.
test min_fee_step_01_forwards_the_header_fee_schedule() {
  let block = fixture.min_fee_params_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    44,
    155_381,
  )
  run_continue(block, expected_state_for(block))
}

/// Valid-block negative selector: a raw transactions root that does not
/// re-commit to the valid header's counted `transactions_root` cannot bind
/// fabricated fee evidence.
test min_fee_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..valid_block,
    transactions_phas_root: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
  }
  run_continue(claimed_block, expected_state_for(claimed_block))
}

/// The state equality is what stops a prover from writing a harsher fee
/// schedule than the header's into thread state: an output claiming
/// `min_fee_b` one lovelace above the committed header's is refused here, not
/// left for step-02 to trust.
test min_fee_step_01_rejects_a_forged_fee_schedule_in_state() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged_state: Data = step_02.State {
    subject: substrate.open_subject_v1(
      fixture.ct_token_asset_name(block),
      substrate.AcceptedSource {
        header: block.header,
        tx_inclusion: verify_native_tx_compact_cbor_v1(
          block.tx_id,
          block.compact_cbor,
        ),
      },
    ),
    bad_tx: ForcedTxCompact {
      body: block.compact.body,
      witness_set_hash: block.compact.witness_set_hash,
    },
    bad_tx_body_fee: block.compact.body.fee,
    bad_tx_id: block.tx_id,
    min_fee_a: block.header.min_fee_a,
    min_fee_b: block.header.min_fee_b + 1,
  }
  run_continue(block, forged_state)
}

// ## Forced source

fn forced_transaction(verdict: OperatorVerdictV1) -> MidgardTransaction {
  when verdict is {
    ForcedTxValid -> fixture.valid_native_tx_v1()
    ForcedTxInvalid { .. } ->
      MidgardTransaction {
        ..fixture.valid_native_tx_v1(),
        validity: TxIsInvalid,
      }
  }
}

/// Runs the forced arm over a one-leaf forced root whose header carries a
/// non-zero fee schedule. `written_verdict` is the verdict the forced leaf
/// carries; the expected state is built from the same leaf.
fn run_forced_continue(written_verdict: OperatorVerdictV1) -> Bool {
  let leaf = tf.forced_leaf_v1(
    forced_transaction(written_verdict),
    written_verdict,
  )
  let membership = tf.forced_leaf_membership_v1(tf.forced_order_key_v1(), leaf)
  let header = ledger_state.HeaderV1 {
    ..fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
    forced_transactions_root: tf.forced_counted_root_v1(membership.phas_root, 1),
    forced_transaction_count: 1,
    min_fee_a: 44,
    min_fee_b: 155_381,
  }
  let thread_name = tf.header_asset_name_v1(header)
  let verified = verify_forced_tx_compact_cbor_v1(
    leaf.tx_id,
    leaf.submitted_source.compact_cbor,
  )
  let expected_state: Data = step_02.State {
    subject: substrate.open_subject_v1(
      thread_name,
      substrate.ForcedSource { header, membership },
    ),
    bad_tx: ForcedTxCompact {
      body: verified.tx_compact.body,
      witness_set_hash: verified.tx_compact.witness_set_hash,
    },
    bad_tx_body_fee: verified.tx_compact.body.fee,
    bad_tx_id: leaf.tx_id,
    min_fee_a: 44,
    min_fee_b: 155_381,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: ForcedSource {
          input_index: 0,
          output_index: 0,
          header,
          membership,
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(thread_name, expected_state),
  )
}

/// A forced leaf the operator accepted opens a wrongful-acceptance thread and
/// forwards the leaf's own compact transaction, fee, id and the header's fee
/// schedule.
test min_fee_step_01_binds_a_forced_acceptance() {
  run_forced_continue(ForcedTxValid)
}

/// A forced leaf rejected as `FeeBelowMinimum` opens a wrongful-rejection
/// thread for this family.
test min_fee_step_01_binds_a_forced_fee_below_minimum_rejection() {
  run_forced_continue(ForcedTxInvalid { reason: FeeBelowMinimum })
}

/// A forced leaf rejected for any other reason is not this family's dispute:
/// the leaf's exact reason must be `FeeBelowMinimum`.
test min_fee_step_01_rejects_a_forced_rejection_for_another_reason() fail {
  run_forced_continue(
    ForcedTxInvalid { reason: InputNotFound { source_kind: 0, input_index: 0 } },
  )
}
