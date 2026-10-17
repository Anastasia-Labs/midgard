use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ExistingPurpose, ImpossiblePurpose,
}
use midgard/fraud_proofs/missing_redeemer/claim_spend as spend
use midgard/fraud_proofs/missing_redeemer/claim_start as start
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn decision(
  block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  pre_root: ByteArray,
) -> rule.DecisionStateV1 {
  let coordinate = PurposeCoordinateV1 { kind: 2, index: 9 }
  rule.DecisionStateV1 {
    authenticated: workers.SourceSelectionV1 {
      selection: SelectionV1 {
        bound: BoundV1 {
          subject: VerdictSubjectV1 {
            version: 1,
            direction: 0,
            source_kind: 0,
            transaction_id: block.tx_id,
            source_key: #"",
            rejection_reason: reason,
          },
          compact_cbor: block.compact_cbor,
          network_order_context: NetworkOrderContextV1 {
            body_network_mismatch: False,
          },
          event_pre_root: pre_root,
        },
        source_index: source_index.StateV1 { root, count: 7 },
        coordinate,
        purpose: Some(
          PurposeV1 { coordinate, script_hash: hash, subject: hash },
        ),
      },
      source: Some(
        SourceV1 {
          field_index: 6,
          item_index: 0,
          source_key: #"00",
          language_tag: 3,
          script_hash: hash,
          total_length: 2,
          item_commitment: root,
        },
      ),
    },
    redeemer_missing: True,
  }
}

fn observed(
  block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  pre_root: ByteArray,
) -> ClaimContextV1 {
  claim.observed_context_v1(decision(block, reason, pre_root))
}

fn reason(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
}

fn input(index: Int) {
  native_types.MidgardTxInput { tx_id: root, output_index: index }
}

fn spend_fixture(key: Bool, claimed_index: Int) {
  // Raw index two has canonical spend rank one; references straddle it.
  let inputs = [input(9), input(1), input(5)]
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs,
        reference_inputs: [input(0), input(3)],
      },
    },
  )
  let summary = DataSummaryV1 { root, cbor_length: 1, memory: 1 }
  let value = descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index: 5,
      total_length: 50,
      item_commitment: root,
      address: bytearray.concat(
        if key {
          #"60"
        } else {
          #"70"
        },
        hash,
      ),
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: root,
      cardano_value_size: 5,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
  let key = ledger_outref_key(
    OutputReference { transaction_id: root, output_index: 5 },
  )
  let pre_root = mpf.root(mpf.insert(mpf.empty, key, value, []))
  let initial = observed(block, reason(0, claimed_index), pre_root)
  let opening = fixture.inline_body_opening(
    block.compact_cbor,
    list.map(inputs, encode_midgard_tx_input),
  )
  let state = spend.initial_v1(
    initial,
    2,
    opening,
    value,
    RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
    [],
    [fixture.phas_withdraw_redeemer_entry(pre_root, key, value)],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = cursor.initial_v1(
    initial.bound.bound,
    0,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  (state, opening, checkpoint, value, key)
}

fn complete_spend(key: Bool, claimed_index: Int) {
  let (state, opening, checkpoint, _, _) = spend_fixture(key, claimed_index)
  spend.advance_v1(
    state,
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: checkpoint,
      budget: 3,
    },
    [],
    fixture.test_certificate_policy_id,
  )
}

fn impossible(kind: Int, index: Int) -> claim_purpose.ClaimedPurposeFactV1 {
  ImpossiblePurpose { coordinate: PurposeCoordinateV1 { kind, index } }
}

test key_spend_is_impossible_at_completed_spends_only_rank() {
  let state = complete_spend(True, 1)
  and {
    state.rank == 1,
    state.raw_item_index == 2,
    state.complete,
    spend.finish_v1(state).purpose == impossible(0, 1),
  }
}

test script_spend_exists_at_the_same_completed_rank() {
  let state = complete_spend(False, 1)
  spend.finish_v1(state).purpose == ExistingPurpose {
    purpose: PurposeV1 {
      coordinate: PurposeCoordinateV1 { kind: 0, index: 1 },
      script_hash: hash,
      subject: state.target.subject,
    },
  }
}

// Successful returns serialize to nonempty CBOR; only an abort satisfies fail.
test raw_position_is_not_canonical_spend_rank() fail {
  cbor.serialise(spend.finish_v1(complete_spend(True, 2))) != #""
}

test incomplete_rank_cannot_prove_impossibility() fail {
  let complete = complete_spend(True, 1)
  // Keep rank and cursor valid so only semantic completion refuses.
  let state = spend.StateV1 { ..complete, complete: False }
  cbor.serialise(spend.finish_v1(state)) != #""
}

test wrong_raw_item_cannot_reuse_the_descriptor() fail {
  let (state, opening, _, value, key) = spend_fixture(True, 1)
  spend.initial_v1(
    state.context,
    1,
    opening,
    value,
    RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
    [],
    [
      fixture.phas_withdraw_redeemer_entry(
        state.cursor.bound.event_pre_root,
        key,
        value,
      ),
    ],
    fixture.test_certificate_policy_id,
  ).rank == 0
}

test wrong_membership_root_cannot_prove_a_key_credential() fail {
  let (state, opening, _, value, key) = spend_fixture(True, 1)
  spend.initial_v1(
    state.context,
    2,
    opening,
    value,
    RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
    [],
    [fixture.phas_withdraw_redeemer_entry(root, key, value)],
    fixture.test_certificate_policy_id,
  ).rank == 0
}

test completed_rank_preserves_source_receipt() fail {
  let state = complete_spend(True, 1)
  cbor.serialise(
    spend.finish_v1(
      spend.StateV1 {
        ..state,
        source_index: source_index.StateV1 { root, count: 8 },
      },
    ),
  ) != #""
}

test authenticated_spend_total_count_proves_the_boundary() {
  let (state, opening, _, _, _) = spend_fixture(True, 3)
  start.spend_out_of_range_v1(
    state.context,
    opening,
    [],
    fixture.test_certificate_policy_id,
  ).purpose == impossible(0, 3)
}

test in_range_key_needs_credential_and_rank_not_count_only() fail {
  let (state, opening, _, _, _) = spend_fixture(True, 1)
  cbor.serialise(
    start.spend_out_of_range_v1(
      state.context,
      opening,
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}
