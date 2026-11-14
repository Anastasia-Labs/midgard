use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  Arbitrary, HonestFor, IntegrityV1, ShapeV1, SubjectV1, TxV1, WitnessesV1, a,
  at, b, key_spend, protected_script_output, source_map, source_of,
  subject_tx_of, walk_tx,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/script_integrity_hash_mismatch/terminal.{convicts_v1}
use midgard/rejection_reason_v1.{
  RejectionReasonV1, ScriptIntegrityHashMismatch, UnusedRedeemer,
  ValueNotPreserved,
}

// A normal transaction executing a PlutusV3 observer and a MidgardV1 receive
// script (bitmap 3), and a forced one whose only execution is a native
// observer (bitmap 0). `HonestFor(n)` commits the hash bitmap `n` requires.

fn plutus_shape() -> ShapeV1 {
  ShapeV1 {
    spends: [key_spend],
    policies: [],
    observers: [a],
    outputs: [protected_script_output(b)],
  }
}

fn native_shape() -> ShapeV1 {
  ShapeV1 { spends: [key_spend], policies: [], observers: [a], outputs: [] }
}

fn walked(
  shape: ShapeV1,
  subject: SubjectV1,
  native: Bool,
) -> Pair<scan.StateV1, TxV1> {
  let tx = subject_tx_of(shape, subject)
  let Pair(index, sources) = if native {
    source_map([source_of(a, 0)])
  } else {
    source_map([source_of(a, 3), source_of(b, 128)])
  }
  let witnesses = if native {
    WitnessesV1 {
      spend_executions: [None],
      mint: [],
      observers: [at(sources, 0)],
      receive: [],
    }
  } else {
    WitnessesV1 {
      spend_executions: [None],
      mint: [],
      observers: [at(sources, 0)],
      receive: [Some(at(sources, 1))],
    }
  }
  Pair(walk_tx(tx, index, None, witnesses), tx)
}

fn accepted_normal(integrity: IntegrityV1) -> Bool {
  let Pair(state, tx) = walked(
    plutus_shape(),
    SubjectV1 { source_kind: 0, rejection_reason: None, integrity },
    False,
  )
  convicts_v1(state, tx.witness_set_compact_cbor)
}

fn forced(
  rejection_reason: Option<RejectionReasonV1>,
  integrity: IntegrityV1,
) -> Bool {
  let Pair(state, tx) = walked(
    native_shape(),
    SubjectV1 { source_kind: 1, rejection_reason, integrity },
    True,
  )
  convicts_v1(state, tx.witness_set_compact_cbor)
}

// ## Wrongful acceptance, normal source (A-N)

test accepted_normal_mismatch_convicts() {
  accepted_normal(Arbitrary)
}

test accepted_normal_matching_hash_does_not_convict() {
  !accepted_normal(HonestFor(3))
}

test accepted_normal_hash_missing_a_language_convicts() {
  accepted_normal(HonestFor(1)) && accepted_normal(HonestFor(2))
}

test accepted_normal_empty_language_hash_convicts() {
  accepted_normal(HonestFor(0))
}

// ## Wrongful acceptance, forced source (A-F)

test accepted_forced_mismatch_convicts() {
  forced(None, Arbitrary)
}

test accepted_forced_native_only_hash_is_the_empty_one() {
  !forced(None, HonestFor(0))
}

test accepted_forced_hash_naming_a_language_convicts() {
  forced(None, HonestFor(1))
}

// ## Wrongful rejection, forced source (R-F)

test rejection_claiming_a_mismatch_that_is_absent_convicts() {
  forced(Some(ScriptIntegrityHashMismatch), HonestFor(0))
}

test rejection_claiming_a_present_mismatch_does_not_convict() {
  !forced(Some(ScriptIntegrityHashMismatch), Arbitrary)
}

test rejection_for_a_later_reason_over_a_mismatch_convicts() {
  forced(Some(ValueNotPreserved), Arbitrary)
}

test rejection_for_a_later_reason_without_a_mismatch_does_not_convict() {
  !forced(Some(ValueNotPreserved), HonestFor(0))
}

test rejection_for_an_earlier_reason_does_not_convict() {
  !forced(Some(UnusedRedeemer { redeemer_index: 0 }), Arbitrary)
}

// ## Inputs the terminal refuses

test terminal_refuses_another_witness_set() fail {
  let Pair(state, _) = walked(
    plutus_shape(),
    SubjectV1 { source_kind: 0, rejection_reason: None, integrity: Arbitrary },
    False,
  )
  convicts_v1(
    state,
    encode_native_tx_witness_set_compact(fixture.empty_witness_set_compact()),
  )
}

test terminal_refuses_an_unfinished_walk() fail {
  let Pair(state, tx) = walked(
    plutus_shape(),
    SubjectV1 { source_kind: 0, rejection_reason: None, integrity: Arbitrary },
    False,
  )
  convicts_v1(
    scan.StateV1 { ..state, complete: False },
    tx.witness_set_compact_cbor,
  )
}
