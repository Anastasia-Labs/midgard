//// Q35 network-id step 02: decisive check and permanent proof-token mint.

use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_cbor, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
}
use midgard/fraud_proofs/network_id/rule.{
  is_output_network_violation_v1, is_transaction_network_violation_v1,
}
use midgard/fraud_proofs/network_id/step_01.{
  ForcedNetworkIdMismatch, NetworkIdFaultV1, OutputNetwork, TransactionNetwork,
}
use midgard/fraud_proofs/network_id/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
        outputs_opening,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
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
        expect Some(state) = m_input_state_data

        when state.fault is {
          TransactionNetwork -> {
            // Cardano permits the body field to be absent. Canonical native
            // V1 spells absence as 255; only an explicitly different id is a
            // TRANSACTION-NETWORK violation.
            expect None = outputs_opening
            is_transaction_network_violation_v1(
              state.committed_tx_network_id,
              state.expected_network_id,
            )
          }
          OutputNetwork { output_index: bad_output_index } -> {
            expect Some(opening) = outputs_opening
            // Step 01 binds a normal transaction without a forced root key;
            // the forced door binds an accepted forced one with it.
            let source_kind = when state.forced_source_key is {
              None -> 0
              Some(_) -> 1
            }
            let outputs_view = opened_field_view(
              opening,
              BodyAnchor { tx_id: state.bad_tx_id, source_kind },
              outputs_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let bad_output = decode_midgard_tx_output_cbor(
              field_item_at(outputs_view, bad_output_index),
            )
            is_output_network_violation_v1(
              bad_output.address.network_id,
              state.expected_network_id,
            )
          }
          ForcedNetworkIdMismatch -> {
            // Shape only. The verdict was established before the thread got
            // here: `forced_step` decided the body half of the negated
            // predicate and refused an honest mismatch at the door, and
            // `forced_scan` walked every output of field 2 in batches and
            // refused the moment one named a foreign network. `forced_scan` is
            // the ONLY writer of a `ForcedNetworkIdMismatch` state into this
            // step — step 01's forced dispatch hands the thread to
            // `forced_step`, never here, and `forced_step` writes a
            // `forced_scan.State` at the scan's address — so there is no route
            // that reaches this arm with the scan unperformed.
            expect None = outputs_opening
            expect Some(_) = state.forced_source_key
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn output_at_network(
  output: MidgardTxOutput,
  network_id: Int,
) -> MidgardTxOutput {
  let address = output.address
  MidgardTxOutput {
    ..output,
    address: MidgardAddress { ..address, network_id },
  }
}

fn protected_output_at_network(
  output: MidgardTxOutput,
  network_id: Int,
) -> MidgardTxOutput {
  let address = output.address
  MidgardTxOutput {
    ..output,
    address: MidgardAddress { ..address, protected: True, network_id },
  }
}

fn with_outputs(outputs: List<MidgardTxOutput>) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs },
  }
}

fn repeated_output(output: MidgardTxOutput, count: Int) -> List<MidgardTxOutput> {
  if count == 0 {
    []
  } else {
    [output, ..repeated_output(output, count - 1)]
  }
}

fn state_for(
  block: fixture.NativeBlockFixtureV1,
  fault: NetworkIdFaultV1,
) -> State {
  State {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id: 0,
    fault,
    forced_source_key: None,
  }
}

fn run_finalize(
  block: fixture.NativeBlockFixtureV1,
  fault: NetworkIdFaultV1,
  carried_outputs: Option<List<MidgardTxOutput>>,
) -> Bool {
  let outputs_opening = when carried_outputs is {
    None -> None
    Some(items) ->
      Some(
        fixture.inline_body_opening(
          block.compact_cbor,
          items |> list.map(encode_midgard_tx_output),
        ),
      )
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_for(block, fault)),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        outputs_opening,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

test network_id_step_02_convicts_an_explicit_transaction_network_mismatch() {
  let base = fixture.valid_native_tx_v1()
  let bad = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 1 },
  }
  run_finalize(fixture.native_block_fixture_v1(bad), TransactionNetwork, None)
}

test network_id_step_02_rejects_an_absent_transaction_network() fail {
  let base = fixture.valid_native_tx_v1()
  let absent = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 255 },
  }
  run_finalize(
    fixture.native_block_fixture_v1(absent),
    TransactionNetwork,
    None,
  )
}

test network_id_step_02_rejects_a_matching_transaction_network() fail {
  run_finalize(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    TransactionNetwork,
    None,
  )
}

test network_id_step_02_convicts_a_wrong_output_network() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [output_at_network(output, 1)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_a_protected_wrong_network_output() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [protected_output_at_network(output, 1)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_foreign_raw_network_nibble_2() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [output_at_network(output, 2)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

test network_id_step_02_convicts_foreign_raw_network_nibble_15() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  // Midgard reserves raw nibble bit 3 as protection, so raw 15 decodes as a
  // protected address on logical foreign network 7.
  let bad_outputs = [protected_output_at_network(output, 7)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_finalize(block, OutputNetwork { output_index: 0 }, Some(bad_outputs))
}

/// Exercises a non-trivial high-cardinality opening. This is intentionally not
/// labelled the canonical maximum: Q38 owns the exact maximum-output frontier
/// and its required tiered-carriage proof-fit evidence.
test network_id_step_02_convicts_the_last_high_cardinality_output() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let honest = output_at_network(output, 0)
  let bad = output_at_network(output, 1)
  let max_outputs = list.concat(repeated_output(honest, 11), [bad])
  let block = fixture.native_block_fixture_v1(with_outputs(max_outputs))
  run_finalize(block, OutputNetwork { output_index: 11 }, Some(max_outputs))
}

test network_id_step_02_rejects_a_matching_output_network() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(block.tx.body.outputs),
  )
}

test network_id_step_02_rejects_a_protected_matching_network_output() fail {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let protected_outputs = [protected_output_at_network(output, 0)]
  let block = fixture.native_block_fixture_v1(with_outputs(protected_outputs))
  run_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(protected_outputs),
  )
}

test network_id_step_02_rejects_an_out_of_range_output_index() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(
    block,
    OutputNetwork { output_index: 12 },
    Some(block.tx.body.outputs),
  )
}

test network_id_step_02_rejects_a_forged_outputs_preimage() fail {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let block = fixture.native_block_fixture_v1(base)
  run_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some([output_at_network(output, 1)]),
  )
}

test network_id_step_02_rejects_unused_output_evidence_for_tx_claim() fail {
  let base = fixture.valid_native_tx_v1()
  let bad = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 1 },
  }
  let block = fixture.native_block_fixture_v1(bad)
  run_finalize(block, TransactionNetwork, Some(block.tx.body.outputs))
}

// ## Forced (wrongful-rejection) arm
//
// The forced polarity is the only one whose conviction is a *negation*: the
// operator's forced leaf claimed `NetworkIdMismatch`, so the thread convicts
// exactly when the complete network predicate is false — the optional body
// field agrees with the deployment and every output address does too. Neither
// half is decided here any more. `forced_step` decides the body half at the
// door, and `forced_scan` walks field 2 in batches and writes the state below
// only when every output agreed; both refuse the honest-rejection direction
// where they see it. What is left for this step is the shape that state must
// have, and the mint.

const forced_source_key_bytes =
  #"d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff"

fn with_tx_network_id(network_id: Int) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id },
  }
}

fn run_forced_finalize(
  block: fixture.NativeBlockFixtureV1,
  carried_outputs: Option<List<MidgardTxOutput>>,
  forced_source_key: Option<ByteArray>,
) -> Bool {
  let outputs_opening = when carried_outputs is {
    None -> None
    Some(items) ->
      Some(
        fixture.inline_body_opening(
          block.compact_cbor,
          items |> list.map(encode_midgard_tx_output),
        ),
      )
  }
  let state = State {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id: 0,
    fault: ForcedNetworkIdMismatch,
    forced_source_key,
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
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
        outputs_opening,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

/// Exactly the state `forced_scan`'s completing `Advance` writes: the bound it
/// carried from the door, no opening and no carriage.
test network_id_step_02_convicts_the_state_the_forced_scan_writes() {
  run_forced_finalize(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    None,
    Some(forced_source_key_bytes),
  )
}

/// The same, over a transaction whose optional body field is absent — the
/// value `forced_step` admits and freezes as `255`.
test network_id_step_02_convicts_a_forced_state_over_an_untagged_tx() {
  run_forced_finalize(
    fixture.native_block_fixture_v1(with_tx_network_id(255)),
    None,
    Some(forced_source_key_bytes),
  )
}

/// There is no field to open here any more, so an opening is evidence for a
/// check this step no longer performs.
test network_id_step_02_rejects_a_forced_claim_carrying_an_outputs_opening() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_forced_finalize(
    block,
    Some(block.tx.body.outputs),
    Some(forced_source_key_bytes),
  )
}

/// Without the authenticated forced key the state is indistinguishable from
/// an accepted-invalid route's, so the wrongful-rejection polarity is unfixed.
test network_id_step_02_rejects_a_forced_claim_without_a_source_key() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_forced_finalize(block, None, None)
}

// ## Accepted forced transactions
//
// The forced door writes an accepted forced leaf's claim with its root key;
// step 02 adjudicates it exactly as an accepted normal transaction's, opening
// the outputs under the forced source.

fn run_accepted_forced_finalize(
  block: fixture.NativeBlockFixtureV1,
  fault: NetworkIdFaultV1,
  carried_outputs: Option<List<MidgardTxOutput>>,
  source_kind: Int,
) -> Bool {
  let outputs_opening = when carried_outputs is {
    None -> None
    Some(items) -> {
      let opening = fixture.inline_body_opening(
        block.compact_cbor,
        items |> list.map(encode_midgard_tx_output),
      )
      Some(
        if source_kind == 1 {
          forced_fixture.from_native_opening(opening)
        } else {
          opening
        },
      )
    }
  }
  let state = State {
    ..state_for(block, fault),
    forced_source_key: Some(forced_source_key_bytes),
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
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
        outputs_opening,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

test network_id_step_02_convicts_an_accepted_forced_transaction_network() {
  let block = fixture.native_block_fixture_v1(with_tx_network_id(1))
  run_accepted_forced_finalize(block, TransactionNetwork, None, 1)
}

test network_id_step_02_refuses_an_honest_accepted_forced_transaction_network() fail {
  let block = fixture.native_block_fixture_v1(with_tx_network_id(0))
  run_accepted_forced_finalize(block, TransactionNetwork, None, 1)
}

test network_id_step_02_refuses_an_absent_accepted_forced_transaction_network() fail {
  let block = fixture.native_block_fixture_v1(with_tx_network_id(255))
  run_accepted_forced_finalize(block, TransactionNetwork, None, 1)
}

test network_id_step_02_convicts_an_accepted_forced_wrong_output_network() {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [output_at_network(output, 1)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_accepted_forced_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(bad_outputs),
    1,
  )
}

test network_id_step_02_refuses_an_honest_accepted_forced_output_network() fail {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let outputs = [output]
  let block = fixture.native_block_fixture_v1(with_outputs(outputs))
  run_accepted_forced_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(outputs),
    1,
  )
}

/// A forced root key fixes the forced source: the transactions-root encoding
/// of the same body does not open its outputs.
test network_id_step_02_rejects_a_normal_opening_for_a_forced_output_claim() fail {
  let base = fixture.valid_native_tx_v1()
  expect [output] = base.body.outputs
  let bad_outputs = [output_at_network(output, 1)]
  let block = fixture.native_block_fixture_v1(with_outputs(bad_outputs))
  run_accepted_forced_finalize(
    block,
    OutputNetwork { output_index: 0 },
    Some(bad_outputs),
    0,
  )
}
