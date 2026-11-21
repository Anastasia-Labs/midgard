use aiken/collection/list
use aiken/option
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  Arbitrary, ShapeV1, SubjectV1, WitnessesV1, at, key_spend,
  protected_script_output, source_map,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/unused_script_witness/fixture.{
  case_tx, hash_of, native_script, plutus_script, reference_source, walked,
  witness_source,
}
use midgard/fraud_proofs/unused_script_witness/terminal.{
  SelectionV1, convicts_v1, unused_v1,
}
use midgard/fraud_proofs/unused_script_witness/walk.{WalkStateV1}
use midgard/rejection_reason_v1.{
  FeeBelowMinimum, RejectionReasonV1, ScriptIntegrityHashMismatch,
  ScriptSourceMissing, UnusedRedeemer, UnusedScriptWitness, ValueNotPreserved,
}

// ## Fixtures
//
// Witness scripts: `n0` and `n1` native, `p0` Plutus V3. Each case states the
// field-6 witnesses, the purposes, and the source map's entries (one or two),
// whose order fixes each execution witness's index.

fn n0() {
  native_script(0x31)
}

fn n1() {
  native_script(0x32)
}

fn p0() {
  plutus_script(0x41, 40)
}

fn accepted(source_kind: Int) -> SubjectV1 {
  SubjectV1 { source_kind, rejection_reason: None, integrity: Arbitrary }
}

fn rejected(reason: RejectionReasonV1) -> SubjectV1 {
  SubjectV1 {
    source_kind: 1,
    rejection_reason: Some(reason),
    integrity: Arbitrary,
  }
}

fn observers(hashes: List<ByteArray>) -> ShapeV1 {
  ShapeV1 { spends: [key_spend], policies: [], observers: hashes, outputs: [] }
}

/// The case's walked state at `script_index` and the selection the prover
/// supplies for it: the map entry for the witness's hash, when there is one.
type CaseV1 {
  state: WalkStateV1,
  selection: Option<SelectionV1>,
}

fn case(
  subject: SubjectV1,
  shape: ShapeV1,
  scripts: List<native_types.MidgardVersionedScript>,
  sources: List<SourceV1>,
  pick: fn(List<scan.ExecutionWitnessV1>) -> WitnessesV1,
  script_index: Int,
) -> CaseV1 {
  let Pair(index, executions) = source_map(sources)
  let probe = when list.at(scripts, script_index) is {
    Some(script) -> hash_of(script)
    None -> hash_of(n0())
  }
  let state = walked(
    case_tx(shape, subject, scripts),
    index,
    probe,
    pick(executions),
    script_index,
  )
  let selection = list.find(
    executions,
    fn(e) { Some(e.source.script_hash) == state.witness_hash },
  )
    |> option.map(fn(e) { SelectionV1 { source: e.source, proof: e.proof } })
  CaseV1 { state, selection }
}

fn convicts(c: CaseV1) -> Bool {
  convicts_v1(c.state, c.selection)
}

fn observing(slots: List<Int>) {
  fn(executions) {
    WitnessesV1 {
      spend_executions: [None],
      mint: [],
      observers: list.map(slots, fn(i) { at(executions, i) }),
      receive: [],
    }
  }
}

fn nothing(_executions) -> WitnessesV1 {
  WitnessesV1 { spend_executions: [None], mint: [], observers: [], receive: [] }
}

/// Witnesses `[n0, n1]`; the only purpose is an observer of `n1`.
fn second_observed(subject: SubjectV1, script_index: Int) -> CaseV1 {
  case(
    subject,
    observers([hash_of(n1())]),
    [n0(), n1()],
    [witness_source(0, n0()), witness_source(1, n1())],
    observing([1]),
    script_index,
  )
}

/// Witness `[n0]`, not used by any purpose.
fn lone_unused(subject: SubjectV1) -> CaseV1 {
  case(subject, observers([]), [n0()], [witness_source(0, n0())], nothing, 0)
}

/// Witness `[n0]`, observed.
fn lone_used(subject: SubjectV1) -> CaseV1 {
  case(
    subject,
    observers([hash_of(n0())]),
    [n0()],
    [witness_source(0, n0())],
    observing([0]),
    0,
  )
}

// ## Wrongful acceptance, normal source (A-N)

test accepted_normal_unused_witness_convicts() {
  convicts(second_observed(accepted(0), 0))
}

test accepted_normal_used_witness_does_not_convict() {
  !convicts(second_observed(accepted(0), 1))
}

test accepted_normal_witness_past_the_field_does_not_convict() {
  !convicts(second_observed(accepted(0), 2))
}

test accepted_normal_negative_index_does_not_convict() {
  !convicts(second_observed(accepted(0), -1))
}

test accepted_normal_unused_plutus_witness_convicts() {
  convicts(
    case(
      accepted(0),
      observers([hash_of(n0())]),
      [n0(), p0()],
      [witness_source(0, n0()), witness_source(1, p0())],
      observing([0]),
      1,
    ),
  )
}

// ## Wrongful acceptance, forced source (A-F)

test accepted_forced_unused_native_witness_convicts() {
  convicts(lone_unused(accepted(1)))
}

test accepted_forced_used_native_witness_does_not_convict() {
  !convicts(lone_used(accepted(1)))
}

// ## Shadowed witnesses

test duplicate_witness_is_unused_even_when_its_hash_is_executed() {
  let later = case(
    accepted(0),
    observers([hash_of(n0())]),
    [n0(), n0()],
    [witness_source(0, n0())],
    observing([0]),
    1,
  )
  let earlier = case(
    accepted(0),
    observers([hash_of(n0())]),
    [n0(), n0()],
    [witness_source(0, n0())],
    observing([0]),
    0,
  )
  convicts(later) && !convicts(earlier)
}

test witness_shadowed_by_a_reference_script_is_unused() {
  convicts(
    case(
      accepted(0),
      observers([hash_of(n0())]),
      [n0()],
      [reference_source(n0())],
      observing([0]),
      0,
    ),
  )
}

// ## Wrongful rejection, forced source (R-F)

test rejection_claiming_a_used_witness_convicts() {
  convicts(lone_used(rejected(UnusedScriptWitness { script_index: 0 })))
}

test rejection_claiming_an_unused_witness_is_refused() {
  !convicts(lone_unused(rejected(UnusedScriptWitness { script_index: 0 })))
}

test rejection_claiming_a_witness_past_the_field_convicts() {
  convicts(
    second_observed(rejected(UnusedScriptWitness { script_index: 2 }), 2),
  )
}

test rejection_claiming_a_negative_witness_convicts() {
  convicts(
    second_observed(rejected(UnusedScriptWitness { script_index: -1 }), -1),
  )
}

// ## Wrongful rejection, later reason (R-F, phase 8 position)

test later_unused_witness_reason_convicts_on_an_earlier_unused_witness() {
  let c = case(
    rejected(UnusedScriptWitness { script_index: 1 }),
    observers([]),
    [n0(), n1()],
    [witness_source(0, n0()), witness_source(1, n1())],
    nothing,
    0,
  )
  convicts(c)
}

test earlier_unused_witness_reason_is_admitted() {
  let c = case(
    rejected(UnusedScriptWitness { script_index: 0 }),
    observers([]),
    [n0(), n1()],
    [witness_source(0, n0()), witness_source(1, n1())],
    nothing,
    1,
  )
  !convicts(c)
}

test unused_redeemer_reason_convicts_on_an_unused_witness() {
  convicts(lone_unused(rejected(UnusedRedeemer { redeemer_index: 0 })))
}

test later_phase_reasons_convict_on_an_unused_witness() {
  and {
    convicts(lone_unused(rejected(ScriptIntegrityHashMismatch))),
    convicts(lone_unused(rejected(ValueNotPreserved))),
  }
}

test earlier_reasons_are_admitted() {
  and {
    !convicts(lone_unused(rejected(FeeBelowMinimum))),
    !convicts(
      lone_unused(
        rejected(ScriptSourceMissing { purpose_kind: 2, purpose_index: 0 }),
      ),
    ),
  }
}

test other_reason_with_a_used_witness_is_refused() {
  !convicts(lone_used(rejected(ValueNotPreserved)))
}

// ## Scan boundaries: a witness reached by one purpose kind only

fn reached_by(shape: ShapeV1, pick) -> Bool {
  !convicts(
    case(
      accepted(0),
      shape,
      [n0(), p0()],
      [witness_source(0, n0()), witness_source(1, p0())],
      pick,
      1,
    ),
  )
}

test witness_reached_only_by_the_first_observer_is_used() {
  reached_by(observers([hash_of(p0()), hash_of(n0())]), observing([1, 0]))
}

test witness_reached_only_by_the_last_observer_is_used() {
  reached_by(observers([hash_of(n0()), hash_of(p0())]), observing([0, 1]))
}

test witness_reached_only_by_a_receive_output_is_used() {
  reached_by(
    ShapeV1 {
      spends: [key_spend],
      policies: [],
      observers: [],
      outputs: [protected_script_output(hash_of(p0()))],
    },
    fn(executions) {
      WitnessesV1 {
        spend_executions: [None],
        mint: [],
        observers: [],
        receive: [Some(at(executions, 1))],
      }
    },
  )
}

test witness_reached_only_by_the_last_receive_output_is_used() {
  reached_by(
    ShapeV1 {
      spends: [key_spend],
      policies: [],
      observers: [],
      outputs: [
        protected_script_output(hash_of(n0())),
        protected_script_output(hash_of(p0())),
      ],
    },
    fn(executions) {
      WitnessesV1 {
        spend_executions: [None],
        mint: [],
        observers: [],
        receive: [Some(at(executions, 0)), Some(at(executions, 1))],
      }
    },
  )
}

test witness_reached_only_by_a_mint_policy_is_used() {
  reached_by(
    ShapeV1 {
      spends: [key_spend],
      policies: [hash_of(p0())],
      observers: [],
      outputs: [],
    },
    fn(executions) {
      WitnessesV1 {
        spend_executions: [None],
        mint: [at(executions, 1)],
        observers: [],
        receive: [],
      }
    },
  )
}

test witness_reached_only_by_a_script_spend_is_used() {
  reached_by(
    ShapeV1 {
      spends: [key_spend, native_types.ScriptCredential(hash_of(p0()))],
      policies: [],
      observers: [],
      outputs: [],
    },
    fn(executions) {
      WitnessesV1 {
        spend_executions: [None, Some(at(executions, 1))],
        mint: [],
        observers: [],
        receive: [],
      }
    },
  )
}

test witness_reached_by_no_purpose_kind_is_unused() {
  !reached_by(
    ShapeV1 {
      spends: [key_spend],
      policies: [hash_of(n0())],
      observers: [hash_of(n0())],
      outputs: [protected_script_output(hash_of(n0()))],
    },
    fn(executions) {
      WitnessesV1 {
        spend_executions: [None],
        mint: [at(executions, 0)],
        observers: [at(executions, 0)],
        receive: [Some(at(executions, 0))],
      }
    },
  )
}

// ## Decision inputs the prover cannot bend

test selected_witness_requires_its_own_probe() fail {
  let c = second_observed(accepted(0), 0)
  let result = c.state.result
  unused_v1(
    WalkStateV1 {
      ..c.state,
      result: scan.ResultV1 {
        ..result,
        facts: scan.FactsV1 { ..result.facts, probe: Some(hash_of(n1())) },
      },
    },
    c.selection,
  )
}

test read_witness_requires_a_selection() fail {
  unused_v1(second_observed(accepted(0), 0).state, None)
}

test selection_must_be_the_map_entry_for_the_witness_hash() fail {
  let c = second_observed(accepted(0), 1)
  expect Some(selection) = c.selection
  unused_v1(
    c.state,
    Some(
      SelectionV1 {
        ..selection,
        source: SourceV1 { ..selection.source, item_index: 0 },
      },
    ),
  )
}

test selection_for_another_hash_is_refused() fail {
  let used = second_observed(accepted(0), 1)
  let other = second_observed(accepted(0), 0)
  unused_v1(used.state, other.selection)
}
