// mkCons (32) is proved as a semantic step only over a list whose element
// type the split list hop can read. The split roots hop compacts every
// argument, so the list hop reads the source's memory from its list node and
// binds the item's summary memory to its memory
// (`constant_memory_from_payload_node_v1`,
// `list_item_summary_memory_is_bound_v1`); it can do both for a Data,
// integer, bytestring, unit, boolean or `pair<data, data>` element. Over any
// other element type the executor emits the direct list-construct witness on
// the revealed values.
//
// Each evidence below is the step the TypeScript executor emits for mkCons
// over a list of the named element type. The bytes are pinned once, in
// demo/midgard-validation/tests/fixtures/cek-builtin-step-evidence-vectors.json,
// and rendered into `cek_direct_list_construct_evidence_generated`;
// `cek-executor.direct-list-construct.test.ts` checks that the executor
// still emits them. Each is proved by `verify_core_step_v1` and by the split
// chain of its arm. For each direct row, the same step as a semantic witness,
// compacted as the roots hop compacts it, is refused by the split chain.

use aiken/cbor
use aiken/collection/list
use midgard/cek_builtin_v1.{ValueWitnessV1}
use midgard/cek_cost_v1
use midgard/cek_direct_list_construct_evidence_generated.{
  mk_cons_bool_evidence, mk_cons_bytes_evidence, mk_cons_data_evidence,
  mk_cons_data_list_list_compacted_evidence, mk_cons_data_list_list_evidence,
  mk_cons_data_pair_evidence, mk_cons_integer_direct_evidence,
  mk_cons_integer_evidence, mk_cons_integer_list_compacted_evidence,
  mk_cons_integer_list_evidence, mk_cons_integer_pair_compacted_evidence,
  mk_cons_integer_pair_evidence, mk_cons_string_compacted_evidence,
  mk_cons_string_evidence, mk_cons_unit_evidence,
}
use midgard/cek_machine_v1.{
  CoreStepEvidenceV1, ExecuteBuiltinDirect, ExecuteBuiltinSemantic,
  MachineStateV1,
}
use midgard/cek_proof_v1

fn decode(evidence_cbor: ByteArray) -> CoreStepEvidenceV1 {
  expect Some(data) = cbor.deserialise(evidence_cbor)
  expect evidence: CoreStepEvidenceV1 = data
  expect cbor.serialise(evidence) == evidence_cbor
  evidence
}

fn proves(evidence: CoreStepEvidenceV1) -> Bool {
  cek_machine_v1.verify_core_step_v1(
    evidence.pre,
    evidence.post,
    evidence.witness,
  )
}

fn post_matches(
  evidence: CoreStepEvidenceV1,
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  and {
    arguments_count == cek_cost_v1.builtin_argument_count_v1(tag),
    evidence.pre.focus_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
    evidence.post == cek_machine_v1.exact_state(
      evidence.pre,
      cek_machine_v1.mode_return,
      cek_builtin_v1.result_root_v1(result),
      cek_proof_v1.empty_environment_root_v1,
      evidence.pre.continuation_root,
      0,
      budget.cpu,
      budget.memory,
    ),
  }
}

/// The semantic split chain on a mkCons step: the roots hop compacts every
/// argument and binds them to the focus, the result and budget hops bind the
/// post-state, and the list-construct hop proves the compacted result.
fn semantic_split_proves(evidence: CoreStepEvidenceV1) -> Bool {
  expect ExecuteBuiltinSemantic { tag, arguments, result, material } = evidence.witness
  let compact = list.map(arguments, cek_builtin_v1.compact_semantic_value)
  let compact_result = cek_builtin_v1.compact_semantic_value(result)
  and {
    post_matches(evidence, tag, compact, compact_result),
    cek_builtin_v1.verify_semantic_list_construct(
      tag,
      compact,
      compact_result,
      material,
    ),
  }
}

/// The direct split chain on a builtin step: the roots and budget hops bind
/// the revealed arguments and result to the states, and the structured hop
/// proves the result.
fn direct_split_proves(evidence: CoreStepEvidenceV1) -> Bool {
  expect ExecuteBuiltinDirect { tag, arguments, result } = evidence.witness
  and {
    post_matches(evidence, tag, arguments, result),
    cek_builtin_v1.verify_direct_structured_semantics(tag, arguments, result),
  }
}

fn is_semantic(evidence: CoreStepEvidenceV1) -> Bool {
  when evidence.witness is {
    ExecuteBuiltinSemantic { tag: 32, .. } -> True
    _ -> False
  }
}

fn is_direct(evidence: CoreStepEvidenceV1) -> Bool {
  when evidence.witness is {
    ExecuteBuiltinDirect { tag: 32, .. } -> True
    _ -> False
  }
}

// mkCons over a data list: the semantic witness, 524 bytes of evidence.

fn mk_cons_data() -> CoreStepEvidenceV1 {
  decode(mk_cons_data_evidence)
}

test mk_cons_data_is_emitted_as_the_semantic_witness() {
  is_semantic(mk_cons_data())
}

test mk_cons_data_proves() {
  proves(mk_cons_data())
}

test mk_cons_data_proves_on_the_split_chain() {
  semantic_split_proves(mk_cons_data())
}

// mkCons over a integer list: the semantic witness, 489 bytes of evidence.

fn mk_cons_integer() -> CoreStepEvidenceV1 {
  decode(mk_cons_integer_evidence)
}

test mk_cons_integer_is_emitted_as_the_semantic_witness() {
  is_semantic(mk_cons_integer())
}

test mk_cons_integer_proves() {
  proves(mk_cons_integer())
}

test mk_cons_integer_proves_on_the_split_chain() {
  semantic_split_proves(mk_cons_integer())
}

// The same mkCons over an integer list as the direct witness, 296 bytes of
// evidence: the revealed arguments and the result, with the semantic
// witness's states. The step verifier accepts either witness kind for the
// step.

fn mk_cons_integer_direct() -> CoreStepEvidenceV1 {
  decode(mk_cons_integer_direct_evidence)
}

fn with_another_post_state(evidence: CoreStepEvidenceV1) -> CoreStepEvidenceV1 {
  CoreStepEvidenceV1 {
    ..evidence,
    post: MachineStateV1 { ..evidence.post, cpu: evidence.post.cpu + 1 },
  }
}

test mk_cons_integer_direct_witness_has_the_semantic_witness_states() {
  let semantic = mk_cons_integer()
  let direct = mk_cons_integer_direct()
  and {
    is_direct(direct),
    direct.pre == semantic.pre,
    direct.post == semantic.post,
  }
}

test mk_cons_integer_semantic_witness_is_accepted_where_the_direct_witness_fits() {
  and {
    proves(mk_cons_integer()),
    semantic_split_proves(mk_cons_integer()),
  }
}

test mk_cons_integer_direct_witness_of_the_same_step_is_accepted() {
  and {
    proves(mk_cons_integer_direct()),
    direct_split_proves(mk_cons_integer_direct()),
  }
}

test mk_cons_integer_semantic_witness_with_another_post_state_is_refused() fail {
  proves(with_another_post_state(mk_cons_integer()))
}

test mk_cons_integer_semantic_witness_with_another_post_state_is_refused_on_the_split_chain() fail {
  semantic_split_proves(with_another_post_state(mk_cons_integer()))
}

// mkCons over a bytestring list: the semantic witness, 490 bytes of evidence.

fn mk_cons_bytes() -> CoreStepEvidenceV1 {
  decode(mk_cons_bytes_evidence)
}

test mk_cons_bytes_is_emitted_as_the_semantic_witness() {
  is_semantic(mk_cons_bytes())
}

test mk_cons_bytes_proves() {
  proves(mk_cons_bytes())
}

test mk_cons_bytes_proves_on_the_split_chain() {
  semantic_split_proves(mk_cons_bytes())
}

// mkCons over a unit list: the semantic witness, 491 bytes of evidence.

fn mk_cons_unit() -> CoreStepEvidenceV1 {
  decode(mk_cons_unit_evidence)
}

test mk_cons_unit_is_emitted_as_the_semantic_witness() {
  is_semantic(mk_cons_unit())
}

test mk_cons_unit_proves() {
  proves(mk_cons_unit())
}

test mk_cons_unit_proves_on_the_split_chain() {
  semantic_split_proves(mk_cons_unit())
}

// mkCons over a boolean list: the semantic witness, 491 bytes of evidence.

fn mk_cons_bool() -> CoreStepEvidenceV1 {
  decode(mk_cons_bool_evidence)
}

test mk_cons_bool_is_emitted_as_the_semantic_witness() {
  is_semantic(mk_cons_bool())
}

test mk_cons_bool_proves() {
  proves(mk_cons_bool())
}

test mk_cons_bool_proves_on_the_split_chain() {
  semantic_split_proves(mk_cons_bool())
}

// mkCons over a string list: the direct witness, 304 bytes of evidence.

fn mk_cons_string() -> CoreStepEvidenceV1 {
  decode(mk_cons_string_evidence)
}

test mk_cons_string_is_emitted_as_the_direct_witness() {
  is_direct(mk_cons_string())
}

test mk_cons_string_proves() {
  proves(mk_cons_string())
}

test mk_cons_string_proves_on_the_split_chain() {
  direct_split_proves(mk_cons_string())
}

// The same step as a compacted semantic witness, 524 bytes of evidence.

test mk_cons_string_as_a_compacted_semantic_witness_is_refused_on_the_split_chain() fail {
  semantic_split_proves(decode(mk_cons_string_compacted_evidence))
}

// mkCons over a pair<data, data> list: the semantic witness, 531 bytes of evidence.

fn mk_cons_data_pair() -> CoreStepEvidenceV1 {
  decode(mk_cons_data_pair_evidence)
}

test mk_cons_data_pair_is_emitted_as_the_semantic_witness() {
  is_semantic(mk_cons_data_pair())
}

test mk_cons_data_pair_proves() {
  proves(mk_cons_data_pair())
}

test mk_cons_data_pair_proves_on_the_split_chain() {
  semantic_split_proves(mk_cons_data_pair())
}

// mkCons over a pair<integer, integer> list: the direct witness, 322 bytes of evidence.

fn mk_cons_integer_pair() -> CoreStepEvidenceV1 {
  decode(mk_cons_integer_pair_evidence)
}

test mk_cons_integer_pair_is_emitted_as_the_direct_witness() {
  is_direct(mk_cons_integer_pair())
}

test mk_cons_integer_pair_proves() {
  proves(mk_cons_integer_pair())
}

test mk_cons_integer_pair_proves_on_the_split_chain() {
  direct_split_proves(mk_cons_integer_pair())
}

// The same step as a compacted semantic witness, 531 bytes of evidence.

test mk_cons_integer_pair_as_a_compacted_semantic_witness_is_refused_on_the_split_chain() fail {
  semantic_split_proves(decode(mk_cons_integer_pair_compacted_evidence))
}

// mkCons over a list<integer> list: the direct witness, 307 bytes of evidence.

fn mk_cons_integer_list() -> CoreStepEvidenceV1 {
  decode(mk_cons_integer_list_evidence)
}

test mk_cons_integer_list_is_emitted_as_the_direct_witness() {
  is_direct(mk_cons_integer_list())
}

test mk_cons_integer_list_proves() {
  proves(mk_cons_integer_list())
}

test mk_cons_integer_list_proves_on_the_split_chain() {
  direct_split_proves(mk_cons_integer_list())
}

// The same step as a compacted semantic witness, 527 bytes of evidence.

test mk_cons_integer_list_as_a_compacted_semantic_witness_is_refused_on_the_split_chain() fail {
  semantic_split_proves(decode(mk_cons_integer_list_compacted_evidence))
}

// mkCons over a list<list<data>> list: the direct witness, 318 bytes of evidence.

fn mk_cons_data_list_list() -> CoreStepEvidenceV1 {
  decode(mk_cons_data_list_list_evidence)
}

test mk_cons_data_list_list_is_emitted_as_the_direct_witness() {
  is_direct(mk_cons_data_list_list())
}

test mk_cons_data_list_list_proves() {
  proves(mk_cons_data_list_list())
}

test mk_cons_data_list_list_proves_on_the_split_chain() {
  direct_split_proves(mk_cons_data_list_list())
}

// The same step as a compacted semantic witness, 531 bytes of evidence.

test mk_cons_data_list_list_as_a_compacted_semantic_witness_is_refused_on_the_split_chain() fail {
  semantic_split_proves(decode(mk_cons_data_list_list_compacted_evidence))
}
