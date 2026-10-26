use aiken/cbor
use aiken/collection/list
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1,
  PurposeV1} as dd_types
use midgard/fraud_proofs/execution_native_script_invalid/entry
use midgard/fraud_proofs/execution_native_script_invalid/lookup.{
  NativeSource, NoNativeSource,
}
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  EvalV1, FinalV1, NoExecution,
}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{encode_forced_tx_compact_v1}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const observer_a = #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const observer_b = #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn at(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn recorded(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(
    reasons.ExecutionNativeScriptFalse {
      purpose_kind: kind,
      purpose_index: index,
    },
  )
}

/// A forced transaction with two spends, two observers and a validity
/// interval, under a rejection recording `reason`.
fn forced_bound(reason: Option<reasons.RejectionReasonV1>) -> BoundV1 {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs: spends(),
        required_observers: [observer_a, observer_b],
        validity_interval_start: 10,
        validity_interval_end: -1,
      },
    },
  )
  BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 1,
      source_kind: 1,
      transaction_id: block.tx_id,
      source_key: #"00",
      rejection_reason: reason,
    },
    compact_cbor: encode_forced_tx_compact_v1(
      native_types.ForcedTxCompact {
        body: block.compact.body,
        witness_set_hash: block.compact.witness_set_hash,
      },
    ),
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: root,
  }
}

fn spends() -> List<native_types.MidgardTxInput> {
  [
    native_types.MidgardTxInput { tx_id: root, output_index: 3 },
    native_types.MidgardTxInput { tx_id: root, output_index: 1 },
  ]
}

fn indexed(bound: BoundV1) -> BoundIndexV1 {
  BoundIndexV1 { bound, source_index: source_index.initial_v1() }
}

fn observers_opening(bound: BoundV1, items: List<ByteArray>) {
  fixture.inline_body_opening(bound.compact_cbor, items)
}

fn spends_opening(bound: BoundV1, items: List<native_types.MidgardTxInput>) {
  fixture.inline_body_opening(
    bound.compact_cbor,
    list.map(items, encode_midgard_tx_input),
  )
}

fn absent(bound: BoundV1, coordinate: PurposeCoordinateV1) -> FinalV1 {
  FinalV1 { bound, fact: NoExecution { coordinate } }
}

// ## Contradict: a recorded coordinate that names no purpose

test contradiction_names_an_invalid_coordinate() {
  let bound = forced_bound(recorded(4, 0))
  entry.contradiction_v1(indexed(bound)) == absent(bound, at(4, 0))
}

test contradiction_refuses_a_valid_coordinate() fail {
  cbor.serialise(entry.contradiction_v1(indexed(forced_bound(recorded(0, 0))))) != #""
}

test contradiction_refuses_another_reason() fail {
  let bound = forced_bound(
    Some(reasons.ScriptSourceMissing { purpose_kind: 4, purpose_index: 0 }),
  )
  cbor.serialise(entry.contradiction_v1(indexed(bound))) != #""
}

test contradiction_refuses_an_accepted_leaf() fail {
  cbor.serialise(entry.contradiction_v1(indexed(forced_bound(None)))) != #""
}

// ## Observer and spend ordinals past their field

fn observe_absent(index: Int, items: List<ByteArray>) -> FinalV1 {
  let bound = forced_bound(recorded(2, index))
  entry.observe_absent_v1(
    indexed(bound),
    observers_opening(bound, items),
    [],
    fixture.test_certificate_policy_id,
  )
}

test observer_ordinal_at_the_field_end_has_no_execution() {
  let bound = forced_bound(recorded(2, 2))
  and {
    observe_absent(2, [observer_a, observer_b]) == absent(bound, at(2, 2)),
    observe_absent(9, [observer_a, observer_b]).fact == NoExecution {
      coordinate: at(2, 9),
    },
  }
}

test observer_ordinal_inside_the_field_is_refused() fail {
  cbor.serialise(observe_absent(1, [observer_a, observer_b])) != #""
}

test observer_absence_over_a_forged_field_is_refused() fail {
  cbor.serialise(observe_absent(1, [observer_a])) != #""
}

test observer_absence_refuses_a_spend_coordinate() fail {
  let bound = forced_bound(recorded(0, 5))
  cbor.serialise(
    entry.observe_absent_v1(
      indexed(bound),
      observers_opening(bound, [observer_a, observer_b]),
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

fn spend_absent(index: Int, items: List<native_types.MidgardTxInput>) -> FinalV1 {
  let bound = forced_bound(recorded(0, index))
  entry.spend_absent_v1(
    indexed(bound),
    spends_opening(bound, items),
    [],
    fixture.test_certificate_policy_id,
  )
}

test spend_ordinal_at_the_field_end_has_no_execution() {
  spend_absent(2, spends()).fact == NoExecution { coordinate: at(0, 2) }
}

test spend_ordinal_inside_the_field_is_refused() fail {
  cbor.serialise(spend_absent(1, spends())) != #""
}

test spend_absence_over_a_forged_field_is_refused() fail {
  expect [first, ..] = spends()
  cbor.serialise(spend_absent(1, [first])) != #""
}

// ## Lookup

fn source(language: Int) -> SourceV1 {
  source_in(6, language)
}

fn source_in(field_index: Int, language: Int) -> SourceV1 {
  SourceV1 {
    field_index,
    item_index: 0,
    source_key: #"00",
    language_tag: language,
    script_hash: hash,
    total_length: 40,
    item_commitment: root,
  }
}

fn selection(entry: Option<SourceV1>, exists: Bool) -> SelectionV1 {
  let bound = forced_bound(None)
  SelectionV1 {
    bound,
    source_index: when entry is {
      None -> source_index.initial_v1()
      Some(candidate) ->
        source_index.admit_v1(
          source_index.initial_v1(),
          candidate,
          source_index.First { proof: [] },
        )
    },
    coordinate: at(1, 0),
    purpose: if exists {
      Some(PurposeV1 { coordinate: at(1, 0), script_hash: hash, subject: hash })
    } else {
      None
    },
  }
}

test resolve_names_an_ordinal_past_the_purpose_domain() {
  let state = selection(None, False)
  lookup.resolve_v1(state) == absent(state.bound, at(1, 0))
}

test resolve_refuses_a_selected_purpose() fail {
  cbor.serialise(lookup.resolve_v1(selection(None, True))) != #""
}

test lookup_starts_a_native_evaluation_under_the_interval() {
  let state = selection(Some(source(0)), True)
  lookup.lookup_v1(state, Some(source(0)), []) == NativeSource {
    state: EvalV1 {
      bound: state.bound,
      coordinate: at(1, 0),
      item_commitment: root,
      validity_interval_start: 10,
      validity_interval_end: -1,
      signer_count: 0,
      signer_peaks: [],
      total_length: 0,
      cursor: 0,
      stack_root: #"",
      stack_depth: 0,
      pending: None,
    },
  }
}

test lookup_finds_no_execution_for_other_languages_or_no_source() {
  and {
    list.all(
      [1, 2, 3, 128],
      fn(language) {
        let state = selection(Some(source(language)), True)
        lookup.lookup_v1(state, Some(source(language)), []) == NoNativeSource {
          state: absent(state.bound, at(1, 0)),
        }
      },
    ),
    lookup.lookup_v1(selection(None, True), None, []) == NoNativeSource {
      state: absent(forced_bound(None), at(1, 0)),
    },
  }
}

test lookup_cannot_hide_a_native_source() fail {
  cbor.serialise(lookup.lookup_v1(selection(Some(source(0)), True), None, [])) != #""
}

test lookup_cannot_substitute_a_source() fail {
  cbor.serialise(
    lookup.lookup_v1(selection(Some(source(3)), True), Some(source(0)), []),
  ) != #""
}

test lookup_starts_a_native_evaluation_of_a_spent_output_source() {
  let spent = source_in(0, 0)
  let state = selection(Some(spent), True)
  when lookup.lookup_v1(state, Some(spent), []) is {
    NativeSource { state: started } ->
      started.item_commitment == root && started.coordinate == at(1, 0)
    NoNativeSource { .. } -> False
  }
}
