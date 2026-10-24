use aiken/builtin
use aiken/cbor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ClaimPurposeStateV1, ExistingPurpose, ImpossiblePurpose, NotRequired,
}
use midgard/fraud_proofs/missing_redeemer/claim_start as start
use midgard/fraud_proofs/missing_redeemer/entry
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
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

fn context(kind: Int, index: Int) -> ClaimContextV1 {
  observed(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    reason(kind, index),
    root,
  )
}

fn impossible(kind: Int, index: Int) -> claim_purpose.ClaimedPurposeFactV1 {
  ImpossiblePurpose { coordinate: PurposeCoordinateV1 { kind, index } }
}

test signed_negative_index_is_preserved_in_impossible_receipt() {
  start.complete_v1(context(0, -7)).purpose == impossible(0, -7)
}

test invalid_signed_kind_is_preserved_in_impossible_receipt() {
  start.complete_v1(context(-1, 9)).purpose == impossible(-1, 9)
}

test absent_purpose_coordinate_requires_no_receipt() {
  let initial = observed(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    Some(reasons.EmptyInputs),
    root,
  )
  start.complete_v1(initial) == ClaimPurposeStateV1 {
    context: initial,
    purpose: NotRequired,
  }
}

test identical_coordinate_reuses_the_authenticated_observation() {
  start.complete_v1(context(2, 9)).purpose == ExistingPurpose {
    purpose: PurposeV1 {
      coordinate: PurposeCoordinateV1 { kind: 2, index: 9 },
      script_hash: hash,
      subject: hash,
    },
  }
}

// Successful returns serialize to nonempty CBOR; only an abort satisfies fail.
test different_valid_coordinate_requires_domain_evidence() fail {
  cbor.serialise(start.complete_v1(context(2, 0))) != #""
}

test claimed_only_context_has_no_observation_to_reuse() fail {
  let initial = context(2, 9)
  cbor.serialise(
    start.complete_v1(ClaimContextV1 { ..initial, observed: None }),
  ) != #""
}

test reused_coordinate_requires_an_actual_purpose() fail {
  let initial = context(2, 9)
  expect Some(seen) = initial.observed
  cbor.serialise(
    start.complete_v1(
      ClaimContextV1 {
        ..initial,
        observed: Some(
          rule.DecisionStateV1 {
            ..seen,
            authenticated: workers.SourceSelectionV1 {
              ..seen.authenticated,
              selection: SelectionV1 {
                ..seen.authenticated.selection,
                purpose: None,
              },
            },
          },
        ),
      },
    ),
  ) != #""
}

test finish_refuses_a_receipt_for_another_leaf_coordinate() fail {
  let initial = context(2, 0)
  cbor.serialise(start.finish_v1(initial, initial.bound, impossible(2, 1))) != #""
}

/// A receipt binds only the purpose kind the leaf's reason names, at the same
/// purpose index.
test finish_refuses_a_receipt_for_another_leaf_purpose_kind() fail {
  let initial = context(0, 7)
  cbor.serialise(start.finish_v1(initial, initial.bound, impossible(1, 7))) != #""
}

fn with_leaf_reason(reason: Option<reasons.RejectionReasonV1>) -> BoundIndexV1 {
  let bound = context(0, 7).bound
  BoundIndexV1 {
    ..bound,
    bound: BoundV1 {
      ..bound.bound,
      subject: VerdictSubjectV1 {
        ..bound.bound.subject,
        direction: 1,
        source_kind: 1,
        rejection_reason: reason,
      },
    },
  }
}

test claimed_only_route_admits_a_redeemer_missing_leaf() {
  let bound = with_leaf_reason(reason(0, 7))
  entry.claim_context_v1(bound) == ClaimContextV1 { bound, observed: None }
}

/// The claimed-only route opens only on a RedeemerMissing leaf.
test claimed_only_route_refuses_a_leaf_of_another_reason_constructor() fail {
  let bound = with_leaf_reason(
    Some(reasons.UnusedRedeemer { redeemer_index: 7 }),
  )
  entry.claim_context_v1(bound).observed == None
}

test finish_accepts_an_impossible_receipt_at_an_invalid_coordinate() {
  let initial = context(-1, 9)
  start.finish_v1(initial, initial.bound, impossible(-1, 9)).purpose == impossible(
    -1,
    9,
  )
}

test finish_refuses_an_existing_purpose_at_an_invalid_coordinate() fail {
  let initial = context(-1, 9)
  let coordinate = PurposeCoordinateV1 { kind: -1, index: 9 }
  cbor.serialise(
    start.finish_v1(
      initial,
      initial.bound,
      ExistingPurpose {
        purpose: PurposeV1 { coordinate, script_hash: hash, subject: hash },
      },
    ),
  ) != #""
}

test finish_refuses_a_not_required_fact_for_a_coordinate_leaf() fail {
  let initial = context(2, 0)
  cbor.serialise(start.finish_v1(initial, initial.bound, NotRequired)) != #""
}

fn changed_bound(which: Int) {
  let initial = context(2, 0)
  let original = initial.bound
  let changed = BoundIndexV1 {
    bound: BoundV1 {
      event_pre_root: if which == 0 {
        #""
      } else {
        original.bound.event_pre_root
      },
      compact_cbor: if which == 1 {
        #""
      } else {
        original.bound.compact_cbor
      },
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: which == 2,
      },
      subject: VerdictSubjectV1 {
        ..original.bound.subject,
        source_key: if which == 3 {
          #"01"
        } else {
          #""
        },
      },
    },
    source_index: source_index.StateV1 {
      root: if which == 4 {
        #""
      } else {
        root
      },
      count: if which == 5 {
        8
      } else {
        7
      },
    },
  }
  start.finish_v1(initial, changed, impossible(2, 0))
}

test finish_accepts_the_exact_bound() {
  changed_bound(6).purpose == impossible(2, 0)
}

test finish_refuses_another_event_root() fail {
  cbor.serialise(changed_bound(0)) != #""
}

test finish_refuses_another_compact() fail {
  cbor.serialise(changed_bound(1)) != #""
}

test finish_refuses_another_network_context() fail {
  cbor.serialise(changed_bound(2)) != #""
}

test finish_refuses_another_source_identity() fail {
  cbor.serialise(changed_bound(3)) != #""
}

test finish_refuses_another_source_map_root() fail {
  cbor.serialise(changed_bound(4)) != #""
}

test finish_refuses_another_source_map_count() fail {
  cbor.serialise(changed_bound(5)) != #""
}

fn observe(index: Int, hashes: List<ByteArray>) {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: hashes,
      },
    },
  )
  start.observe_v1(
    observed(block, reason(2, index), root),
    fixture.inline_body_opening(block.compact_cbor, hashes),
    [],
    fixture.test_certificate_policy_id,
  )
}

test empty_observer_domain_proves_impossibility() {
  observe(0, []).purpose == impossible(2, 0)
}

test observer_in_range_is_existing() {
  observe(0, [hash]).purpose == ExistingPurpose {
    purpose: PurposeV1 {
      coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
      script_hash: hash,
      subject: hash,
    },
  }
}

test observer_exact_count_is_impossible() {
  observe(1, [hash]).purpose == impossible(2, 1)
}

test observer_wrong_opening_does_not_prove_impossibility() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  cbor.serialise(
    start.observe_v1(
      observed(block, reason(2, 0), root),
      fixture.inline_body_opening(block.compact_cbor, [hash]),
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

test observer_route_refuses_the_observed_coordinate() fail {
  // The observed route's own coordinate is reused through Complete only.
  cbor.serialise(observe(9, [hash])) != #""
}

// A transaction minting under one policy at mint 0, with no redeemer and no
// required observer: its forced rejection RedeemerMissing { 1, 0 } is the
// true first fault. The observer route opens field 3 only.
fn observe_on_minting_tx(leaf_kind: Int) {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: [],
        mint: builtin.map_data(
          [
            Pair(
              builtin.b_data(hash),
              builtin.map_data([Pair(builtin.b_data(#"41"), builtin.i_data(1))]),
            ),
          ],
        ),
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: [],
      },
    },
  )
  start.observe_v1(
    observed(block, reason(leaf_kind, 0), root),
    fixture.inline_body_opening(block.compact_cbor, []),
    [],
    fixture.test_certificate_policy_id,
  )
}

test observer_route_proves_an_observer_leaf_impossible_on_a_minting_tx() {
  observe_on_minting_tx(2).purpose == impossible(2, 0)
}

// Either result passes the body, so only an abort satisfies this row.
test observer_route_refuses_a_mint_leaf() fail {
  let state = observe_on_minting_tx(1)
  state.purpose == impossible(1, 0) || state.purpose != impossible(1, 0)
}
