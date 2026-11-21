// Semantic builtin steps (tags 29 to 50 except mapData and unMapData) present
// each Data argument, each list or pair argument whose memory the verifier
// derives from its payload nodes, and each Data, list or pair result by its
// semantic summary. A summary's value root commits its type, payload root,
// payload CBOR length and memory, but not its payload's summary memory.
//
// Each evidence below is the step the TypeScript executor emits for the named
// program. The bytes are pinned once, in
// demo/midgard-validation/tests/fixtures/cek-builtin-step-evidence-vectors.json,
// and rendered into `cek_semantic_presentation_evidence_generated`;
// `cek-executor.semantic-presentation.test.ts` checks that the executor still
// emits them. It is proved by `verify_core_step_v1` and, where the split chain
// proves the builtin, by the split chain's checks on the compacted values.
// Each presented value is then moved by one word of memory or of summary
// memory, and both paths refuse it: memory through the focus or result root,
// summary memory through the arm's binding of the payload.

use aiken/cbor
use aiken/collection/list
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_constant_v1
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_machine_v1.{
  CoreStepEvidenceV1, ExecuteBuiltinSemantic, ExecuteBuiltinSemanticFailure,
  MachineStateV1,
}
use midgard/cek_proof_v1
use midgard/cek_semantic_presentation_evidence_generated.{
  choose_data_evidence, choose_list_data_evidence, constr_data_evidence,
  equals_data_true_evidence, fst_pair_string_data_evidence,
  head_list_data_evidence, head_list_data_same_head_evidence,
  head_list_empty_evidence, i_data_evidence, list_data_evidence,
  max_mk_cons_data_9200_evidence, max_snd_pair_9200_evidence,
  max_un_constr_data_9200_evidence, mk_cons_data_evidence, mk_cons_int_evidence,
  mk_nil_data_evidence, mk_pair_data_evidence, null_list_data_evidence,
  snd_pair_data_evidence, tail_list_int_evidence, un_b_data_evidence,
  un_constr_data_evidence, un_list_data_evidence,
}

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

/// The semantic hop of the builtin's group, given the compacted values.
fn semantic_hop(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
) -> Bool {
  if tag == 29 || tag == 30 {
    cek_builtin_v1.verify_semantic_pair_v1(tag, arguments, result, material)
  } else if tag == 31 || tag == 32 || tag == 35 {
    cek_builtin_v1.verify_semantic_list_construct(
      tag,
      arguments,
      result,
      material,
    )
  } else if tag == 33 || tag == 34 {
    cek_builtin_v1.verify_semantic_list_select(tag, arguments, result, material)
  } else if tag == 36 {
    cek_builtin_v1.verify_semantic_choose_data_v1(arguments, result, material)
  } else if tag == 37 || tag == 42 {
    cek_builtin_v1.verify_semantic_data_construct(
      tag,
      arguments,
      result,
      material,
    )
  } else if tag >= 47 && tag <= 51 {
    cek_builtin_v1.verify_semantic_data_misc(tag, arguments, result, material)
  } else {
    cek_builtin_v1.verify_semantic_data_scalar(tag, arguments, result, material)
  }
}

/// The split chain's checks on the step: the roots hop binds the compacted
/// arguments to the focus, the result and budget hops bind the post-state to
/// the compacted result and the arguments' budget, and the semantic hop of
/// the builtin's group proves the result.
fn split_proves(evidence: CoreStepEvidenceV1) -> Bool {
  expect ExecuteBuiltinSemantic { tag, arguments, result, material } = evidence.witness
  let compact = list.map(arguments, cek_builtin_v1.compact_semantic_value)
  let compact_result = cek_builtin_v1.compact_semantic_value(result)
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, compact)
  and {
    evidence.pre.focus_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
    evidence.post == cek_machine_v1.exact_state(
      evidence.pre,
      cek_machine_v1.mode_return,
      cek_builtin_v1.result_root_v1(compact_result),
      cek_proof_v1.empty_environment_root_v1,
      evidence.pre.continuation_root,
      0,
      budget.cpu,
      budget.memory,
    ),
    semantic_hop(tag, compact, compact_result, material),
  }
}

fn is_summary(value: ValueWitnessV1) -> Bool {
  when value is {
    SemanticConstantValue { .. } -> True
    _ -> False
  }
}

fn shifted(
  value: ValueWitnessV1,
  memory: Int,
  summary_memory: Int,
) -> ValueWitnessV1 {
  expect SemanticConstantValue { type_cbor, payload, memory: value_memory } = value
  SemanticConstantValue {
    type_cbor,
    payload: DataSummaryV1 {
      ..payload,
      memory: payload.memory + summary_memory,
    },
    memory: value_memory + memory,
  }
}

fn arguments_of(evidence: CoreStepEvidenceV1) -> List<ValueWitnessV1> {
  when evidence.witness is {
    ExecuteBuiltinSemantic { arguments, .. } -> arguments
    ExecuteBuiltinSemanticFailure { arguments, .. } -> arguments
    _ -> fail
  }
}

fn result_of(evidence: CoreStepEvidenceV1) -> ValueWitnessV1 {
  expect ExecuteBuiltinSemantic { result, .. } = evidence.witness
  result
}

/// Whether the arguments at `indexes`, and the result when `result` holds,
/// are presented by summary.
fn presents(
  evidence: CoreStepEvidenceV1,
  indexes: List<Int>,
  result: Bool,
) -> Bool {
  let arguments = arguments_of(evidence)
  and {
    list.all(
      indexes,
      fn(index) {
        expect Some(argument) = list.at(arguments, index)
        is_summary(argument)
      },
    ),
    !result || is_summary(result_of(evidence)),
  }
}

/// The step with argument `index` moved; the pre- and post-states are kept.
fn with_argument(
  evidence: CoreStepEvidenceV1,
  index: Int,
  memory: Int,
  summary_memory: Int,
) -> CoreStepEvidenceV1 {
  let move = fn(arguments: List<ValueWitnessV1>) {
    list.indexed_map(
      arguments,
      fn(at, argument) {
        if at == index {
          shifted(argument, memory, summary_memory)
        } else {
          argument
        }
      },
    )
  }
  CoreStepEvidenceV1 {
    ..evidence,
    witness: when evidence.witness is {
      ExecuteBuiltinSemantic { tag, arguments, result, material } ->
        ExecuteBuiltinSemantic {
          tag,
          arguments: move(arguments),
          result,
          material,
        }
      ExecuteBuiltinSemanticFailure { tag, arguments, material } ->
        ExecuteBuiltinSemanticFailure {
          tag,
          arguments: move(arguments),
          material,
        }
      _ -> fail
    },
  }
}

/// The step with its result moved and the post-state re-derived from it.
fn with_result(
  evidence: CoreStepEvidenceV1,
  memory: Int,
  summary_memory: Int,
) -> CoreStepEvidenceV1 {
  expect ExecuteBuiltinSemantic { tag, arguments, result, material } = evidence.witness
  let moved = shifted(result, memory, summary_memory)
  CoreStepEvidenceV1 {
    pre: evidence.pre,
    post: MachineStateV1 {
      ..evidence.post,
      focus_root: cek_builtin_v1.result_root_v1(moved),
    },
    witness: ExecuteBuiltinSemantic { tag, arguments, result: moved, material },
  }
}

fn tag_and_arguments(
  evidence: CoreStepEvidenceV1,
) -> Pair<Int, List<ValueWitnessV1>> {
  when evidence.witness is {
    ExecuteBuiltinSemantic { tag, arguments, .. } -> Pair(tag, arguments)
    ExecuteBuiltinSemanticFailure { tag, arguments, .. } -> Pair(tag, arguments)
    _ -> fail
  }
}

/// The step with argument `index`'s memory moved, the pre-state's builtin
/// value re-derived from the moved arguments, and a success's post-state
/// charged their budget. Only the arm's own memory binding can refuse it.
fn with_bound_argument(
  evidence: CoreStepEvidenceV1,
  index: Int,
  memory: Int,
) -> CoreStepEvidenceV1 {
  let moved = with_argument(evidence, index, memory, 0)
  let Pair(tag, arguments) = tag_and_arguments(moved)
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let pre = MachineStateV1 {
    ..evidence.pre,
    focus_root: cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
  }
  let post = when moved.witness is {
    ExecuteBuiltinSemantic { .. } -> {
      let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
      cek_machine_v1.exact_state(
        pre,
        cek_machine_v1.mode_return,
        evidence.post.focus_root,
        cek_proof_v1.empty_environment_root_v1,
        pre.continuation_root,
        0,
        budget.cpu,
        budget.memory,
      )
    }
    _ -> evidence.post
  }
  CoreStepEvidenceV1 { pre, post, witness: moved.witness }
}

/// The step with its result replaced and the post-state re-derived from it.
fn with_result_value(
  evidence: CoreStepEvidenceV1,
  result: ValueWitnessV1,
) -> CoreStepEvidenceV1 {
  expect ExecuteBuiltinSemantic { tag, arguments, material, .. } = evidence.witness
  CoreStepEvidenceV1 {
    pre: evidence.pre,
    post: MachineStateV1 {
      ..evidence.post,
      focus_root: cek_builtin_v1.result_root_v1(result),
    },
    witness: ExecuteBuiltinSemantic { tag, arguments, result, material },
  }
}

/// equalsData over two equal values, with the left summary memory moved and
/// the result claimed False: the summaries now differ, so only the binding
/// of each summary memory to its value's memory refuses the claim.
fn equals_claimed_false(evidence: CoreStepEvidenceV1) -> CoreStepEvidenceV1 {
  with_result_value(
    with_argument(evidence, 0, 0, 1),
    cek_builtin_v1.ConstantValue(
      cek_constant_v1.ConstantWitnessV1 {
        type_cbor: #"9f04ff",
        payload_cbor: #"d87980",
      },
    ),
  )
}

/// mkPairData with the first summary memory moved and the result built from
/// the moved summaries, as the arm builds it.
fn pair_following_moved_summary(
  evidence: CoreStepEvidenceV1,
) -> CoreStepEvidenceV1 {
  let moved = with_argument(evidence, 0, 0, 1)
  expect [first, second] = arguments_of(moved)
  expect SemanticConstantValue { type_cbor, memory, .. } = result_of(moved)
  with_result_value(
    moved,
    SemanticConstantValue {
      type_cbor,
      payload: cek_data_v1.small_constr_data_summary_v1(
        0,
        cek_data_v1.prepend_data_list_summary_v1(
          cek_builtin_v1.semantic_constant_payload_v1(first),
          cek_data_v1.prepend_data_list_summary_v1(
            cek_builtin_v1.semantic_constant_payload_v1(second),
            cek_data_v1.empty_data_list_summary_v1(),
          ),
        ),
      ),
      memory,
    },
  )
}

// fstPair over pair<string, data>: executeBuiltinSemantic 29, 602 bytes of evidence.

fn fst_pair_string_data() -> CoreStepEvidenceV1 {
  decode(fst_pair_string_data_evidence)
}

test fst_pair_string_data_proves() {
  proves(fst_pair_string_data())
}

test fst_pair_string_data_proves_on_the_split_chain() {
  split_proves(fst_pair_string_data())
}

test fst_pair_string_data_presents_by_summary() {
  presents(fst_pair_string_data(), [0], False)
}

test fst_pair_string_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(fst_pair_string_data(), 0, 1, 0))
}

test fst_pair_string_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(fst_pair_string_data(), 0, 1, 0))
}

test fst_pair_string_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(fst_pair_string_data(), 0, -1, 0))
}

test fst_pair_string_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(fst_pair_string_data(), 0, -1, 0))
}

test fst_pair_string_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(fst_pair_string_data(), 0, 0, 1))
}

test fst_pair_string_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(fst_pair_string_data(), 0, 0, 1))
}

test fst_pair_string_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(fst_pair_string_data(), 0, 0, -1))
}

test fst_pair_string_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(fst_pair_string_data(), 0, 0, -1))
}

test fst_pair_string_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(fst_pair_string_data(), 0, 1))
}

test fst_pair_string_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(fst_pair_string_data(), 0, 1))
}

test fst_pair_string_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(fst_pair_string_data(), 0, -1))
}

test fst_pair_string_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(fst_pair_string_data(), 0, -1))
}

// sndPair over pair<data, data>: executeBuiltinSemantic 30, 634 bytes of evidence.

fn snd_pair_data() -> CoreStepEvidenceV1 {
  decode(snd_pair_data_evidence)
}

test snd_pair_data_proves() {
  proves(snd_pair_data())
}

test snd_pair_data_proves_on_the_split_chain() {
  split_proves(snd_pair_data())
}

test snd_pair_data_presents_by_summary() {
  presents(snd_pair_data(), [0], True)
}

test snd_pair_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(snd_pair_data(), 0, 1, 0))
}

test snd_pair_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(snd_pair_data(), 0, 1, 0))
}

test snd_pair_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(snd_pair_data(), 0, -1, 0))
}

test snd_pair_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(snd_pair_data(), 0, -1, 0))
}

test snd_pair_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(snd_pair_data(), 0, 0, 1))
}

test snd_pair_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(snd_pair_data(), 0, 0, 1))
}

test snd_pair_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(snd_pair_data(), 0, 0, -1))
}

test snd_pair_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(snd_pair_data(), 0, 0, -1))
}

test snd_pair_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(snd_pair_data(), 0, 1))
}

test snd_pair_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(snd_pair_data(), 0, 1))
}

test snd_pair_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(snd_pair_data(), 0, -1))
}

test snd_pair_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(snd_pair_data(), 0, -1))
}

test snd_pair_data_refuses_result_memory_plus_one() fail {
  proves(with_result(snd_pair_data(), 1, 0))
}

test snd_pair_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(snd_pair_data(), 1, 0))
}

test snd_pair_data_refuses_result_memory_minus_one() fail {
  proves(with_result(snd_pair_data(), -1, 0))
}

test snd_pair_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(snd_pair_data(), -1, 0))
}

test snd_pair_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(snd_pair_data(), 0, 1))
}

test snd_pair_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(snd_pair_data(), 0, 1))
}

test snd_pair_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(snd_pair_data(), 0, -1))
}

test snd_pair_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(snd_pair_data(), 0, -1))
}

// chooseList over list<data>: executeBuiltinSemantic 31, 539 bytes of evidence.

fn choose_list_data() -> CoreStepEvidenceV1 {
  decode(choose_list_data_evidence)
}

test choose_list_data_proves() {
  proves(choose_list_data())
}

test choose_list_data_proves_on_the_split_chain() {
  split_proves(choose_list_data())
}

test choose_list_data_presents_by_summary() {
  presents(choose_list_data(), [0], False)
}

test choose_list_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(choose_list_data(), 0, 1, 0))
}

test choose_list_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_list_data(), 0, 1, 0))
}

test choose_list_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(choose_list_data(), 0, -1, 0))
}

test choose_list_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_list_data(), 0, -1, 0))
}

test choose_list_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(choose_list_data(), 0, 0, 1))
}

test choose_list_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_list_data(), 0, 0, 1))
}

test choose_list_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(choose_list_data(), 0, 0, -1))
}

test choose_list_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_list_data(), 0, 0, -1))
}

test choose_list_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(choose_list_data(), 0, 1))
}

test choose_list_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(choose_list_data(), 0, 1))
}

test choose_list_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(choose_list_data(), 0, -1))
}

test choose_list_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(choose_list_data(), 0, -1))
}

// mkCons of a Data item onto list<data>: executeBuiltinSemantic 32, 524 bytes of evidence.

fn mk_cons_data() -> CoreStepEvidenceV1 {
  decode(mk_cons_data_evidence)
}

test mk_cons_data_proves() {
  proves(mk_cons_data())
}

test mk_cons_data_proves_on_the_split_chain() {
  split_proves(mk_cons_data())
}

test mk_cons_data_presents_by_summary() {
  presents(mk_cons_data(), [0, 1], True)
}

test mk_cons_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(mk_cons_data(), 0, 1, 0))
}

test mk_cons_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 0, 1, 0))
}

test mk_cons_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(mk_cons_data(), 0, -1, 0))
}

test mk_cons_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 0, -1, 0))
}

test mk_cons_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(mk_cons_data(), 0, 0, 1))
}

test mk_cons_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 0, 0, 1))
}

test mk_cons_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(mk_cons_data(), 0, 0, -1))
}

test mk_cons_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 0, 0, -1))
}

test mk_cons_data_refuses_argument_1_memory_plus_one() fail {
  proves(with_argument(mk_cons_data(), 1, 1, 0))
}

test mk_cons_data_refuses_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 1, 1, 0))
}

test mk_cons_data_refuses_argument_1_memory_minus_one() fail {
  proves(with_argument(mk_cons_data(), 1, -1, 0))
}

test mk_cons_data_refuses_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 1, -1, 0))
}

test mk_cons_data_refuses_argument_1_summary_memory_plus_one() fail {
  proves(with_argument(mk_cons_data(), 1, 0, 1))
}

test mk_cons_data_refuses_argument_1_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 1, 0, 1))
}

test mk_cons_data_refuses_argument_1_summary_memory_minus_one() fail {
  proves(with_argument(mk_cons_data(), 1, 0, -1))
}

test mk_cons_data_refuses_argument_1_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_data(), 1, 0, -1))
}

test mk_cons_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(mk_cons_data(), 0, 1))
}

test mk_cons_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_cons_data(), 0, 1))
}

test mk_cons_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(mk_cons_data(), 0, -1))
}

test mk_cons_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_cons_data(), 0, -1))
}

test mk_cons_data_refuses_bound_argument_1_memory_plus_one() fail {
  proves(with_bound_argument(mk_cons_data(), 1, 1))
}

test mk_cons_data_refuses_bound_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_cons_data(), 1, 1))
}

test mk_cons_data_refuses_bound_argument_1_memory_minus_one() fail {
  proves(with_bound_argument(mk_cons_data(), 1, -1))
}

test mk_cons_data_refuses_bound_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_cons_data(), 1, -1))
}

test mk_cons_data_refuses_result_memory_plus_one() fail {
  proves(with_result(mk_cons_data(), 1, 0))
}

test mk_cons_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_data(), 1, 0))
}

test mk_cons_data_refuses_result_memory_minus_one() fail {
  proves(with_result(mk_cons_data(), -1, 0))
}

test mk_cons_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_data(), -1, 0))
}

test mk_cons_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(mk_cons_data(), 0, 1))
}

test mk_cons_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_data(), 0, 1))
}

test mk_cons_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(mk_cons_data(), 0, -1))
}

test mk_cons_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_data(), 0, -1))
}

// mkCons of a revealed integer onto list<integer>: executeBuiltinSemantic 32, 489 bytes of evidence.

fn mk_cons_int() -> CoreStepEvidenceV1 {
  decode(mk_cons_int_evidence)
}

test mk_cons_int_proves() {
  proves(mk_cons_int())
}

test mk_cons_int_proves_on_the_split_chain() {
  split_proves(mk_cons_int())
}

test mk_cons_int_presents_by_summary() {
  presents(mk_cons_int(), [1], True)
}

test mk_cons_int_refuses_argument_1_memory_plus_one() fail {
  proves(with_argument(mk_cons_int(), 1, 1, 0))
}

test mk_cons_int_refuses_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_int(), 1, 1, 0))
}

test mk_cons_int_refuses_argument_1_memory_minus_one() fail {
  proves(with_argument(mk_cons_int(), 1, -1, 0))
}

test mk_cons_int_refuses_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_int(), 1, -1, 0))
}

test mk_cons_int_refuses_argument_1_summary_memory_plus_one() fail {
  proves(with_argument(mk_cons_int(), 1, 0, 1))
}

test mk_cons_int_refuses_argument_1_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_int(), 1, 0, 1))
}

test mk_cons_int_refuses_argument_1_summary_memory_minus_one() fail {
  proves(with_argument(mk_cons_int(), 1, 0, -1))
}

test mk_cons_int_refuses_argument_1_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_cons_int(), 1, 0, -1))
}

test mk_cons_int_refuses_bound_argument_1_memory_plus_one() fail {
  proves(with_bound_argument(mk_cons_int(), 1, 1))
}

test mk_cons_int_refuses_bound_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_cons_int(), 1, 1))
}

test mk_cons_int_refuses_bound_argument_1_memory_minus_one() fail {
  proves(with_bound_argument(mk_cons_int(), 1, -1))
}

test mk_cons_int_refuses_bound_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_cons_int(), 1, -1))
}

test mk_cons_int_refuses_result_memory_plus_one() fail {
  proves(with_result(mk_cons_int(), 1, 0))
}

test mk_cons_int_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_int(), 1, 0))
}

test mk_cons_int_refuses_result_memory_minus_one() fail {
  proves(with_result(mk_cons_int(), -1, 0))
}

test mk_cons_int_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_int(), -1, 0))
}

test mk_cons_int_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(mk_cons_int(), 0, 1))
}

test mk_cons_int_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_int(), 0, 1))
}

test mk_cons_int_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(mk_cons_int(), 0, -1))
}

test mk_cons_int_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_cons_int(), 0, -1))
}

// headList over list<data>: executeBuiltinSemantic 33, 591 bytes of evidence.

fn head_list_data() -> CoreStepEvidenceV1 {
  decode(head_list_data_evidence)
}

test head_list_data_proves() {
  proves(head_list_data())
}

test head_list_data_proves_on_the_split_chain() {
  split_proves(head_list_data())
}

test head_list_data_presents_by_summary() {
  presents(head_list_data(), [0], True)
}

test head_list_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(head_list_data(), 0, 1, 0))
}

test head_list_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(head_list_data(), 0, 1, 0))
}

test head_list_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(head_list_data(), 0, -1, 0))
}

test head_list_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(head_list_data(), 0, -1, 0))
}

test head_list_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(head_list_data(), 0, 0, 1))
}

test head_list_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(head_list_data(), 0, 0, 1))
}

test head_list_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(head_list_data(), 0, 0, -1))
}

test head_list_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(head_list_data(), 0, 0, -1))
}

test head_list_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(head_list_data(), 0, 1))
}

test head_list_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(head_list_data(), 0, 1))
}

test head_list_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(head_list_data(), 0, -1))
}

test head_list_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(head_list_data(), 0, -1))
}

test head_list_data_refuses_result_memory_plus_one() fail {
  proves(with_result(head_list_data(), 1, 0))
}

test head_list_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(head_list_data(), 1, 0))
}

test head_list_data_refuses_result_memory_minus_one() fail {
  proves(with_result(head_list_data(), -1, 0))
}

test head_list_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(head_list_data(), -1, 0))
}

test head_list_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(head_list_data(), 0, 1))
}

test head_list_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(head_list_data(), 0, 1))
}

test head_list_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(head_list_data(), 0, -1))
}

test head_list_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(head_list_data(), 0, -1))
}

// tailList over list<integer>: executeBuiltinSemantic 34, 592 bytes of evidence.

fn tail_list_int() -> CoreStepEvidenceV1 {
  decode(tail_list_int_evidence)
}

test tail_list_int_proves() {
  proves(tail_list_int())
}

test tail_list_int_proves_on_the_split_chain() {
  split_proves(tail_list_int())
}

test tail_list_int_presents_by_summary() {
  presents(tail_list_int(), [0], True)
}

test tail_list_int_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(tail_list_int(), 0, 1, 0))
}

test tail_list_int_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(tail_list_int(), 0, 1, 0))
}

test tail_list_int_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(tail_list_int(), 0, -1, 0))
}

test tail_list_int_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(tail_list_int(), 0, -1, 0))
}

test tail_list_int_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(tail_list_int(), 0, 0, 1))
}

test tail_list_int_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(tail_list_int(), 0, 0, 1))
}

test tail_list_int_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(tail_list_int(), 0, 0, -1))
}

test tail_list_int_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(tail_list_int(), 0, 0, -1))
}

test tail_list_int_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(tail_list_int(), 0, 1))
}

test tail_list_int_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(tail_list_int(), 0, 1))
}

test tail_list_int_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(tail_list_int(), 0, -1))
}

test tail_list_int_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(tail_list_int(), 0, -1))
}

test tail_list_int_refuses_result_memory_plus_one() fail {
  proves(with_result(tail_list_int(), 1, 0))
}

test tail_list_int_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(tail_list_int(), 1, 0))
}

test tail_list_int_refuses_result_memory_minus_one() fail {
  proves(with_result(tail_list_int(), -1, 0))
}

test tail_list_int_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(tail_list_int(), -1, 0))
}

test tail_list_int_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(tail_list_int(), 0, 1))
}

test tail_list_int_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(tail_list_int(), 0, 1))
}

test tail_list_int_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(tail_list_int(), 0, -1))
}

test tail_list_int_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(tail_list_int(), 0, -1))
}

// nullList over list<data>: executeBuiltinSemantic 35, 441 bytes of evidence.

fn null_list_data() -> CoreStepEvidenceV1 {
  decode(null_list_data_evidence)
}

test null_list_data_proves() {
  proves(null_list_data())
}

test null_list_data_proves_on_the_split_chain() {
  split_proves(null_list_data())
}

test null_list_data_presents_by_summary() {
  presents(null_list_data(), [0], False)
}

test null_list_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(null_list_data(), 0, 1, 0))
}

test null_list_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(null_list_data(), 0, 1, 0))
}

test null_list_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(null_list_data(), 0, -1, 0))
}

test null_list_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(null_list_data(), 0, -1, 0))
}

test null_list_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(null_list_data(), 0, 0, 1))
}

test null_list_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(null_list_data(), 0, 0, 1))
}

test null_list_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(null_list_data(), 0, 0, -1))
}

test null_list_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(null_list_data(), 0, 0, -1))
}

test null_list_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(null_list_data(), 0, 1))
}

test null_list_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(null_list_data(), 0, 1))
}

test null_list_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(null_list_data(), 0, -1))
}

test null_list_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(null_list_data(), 0, -1))
}

// chooseData over a list: executeBuiltinSemantic 36, 652 bytes of evidence.

fn choose_data() -> CoreStepEvidenceV1 {
  decode(choose_data_evidence)
}

test choose_data_proves() {
  proves(choose_data())
}

test choose_data_proves_on_the_split_chain() {
  split_proves(choose_data())
}

test choose_data_presents_by_summary() {
  presents(choose_data(), [0], False)
}

test choose_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(choose_data(), 0, 1, 0))
}

test choose_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_data(), 0, 1, 0))
}

test choose_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(choose_data(), 0, -1, 0))
}

test choose_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_data(), 0, -1, 0))
}

test choose_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(choose_data(), 0, 0, 1))
}

test choose_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_data(), 0, 0, 1))
}

test choose_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(choose_data(), 0, 0, -1))
}

test choose_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(choose_data(), 0, 0, -1))
}

test choose_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(choose_data(), 0, 1))
}

test choose_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(choose_data(), 0, 1))
}

test choose_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(choose_data(), 0, -1))
}

test choose_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(choose_data(), 0, -1))
}

// constrData: executeBuiltinSemantic 37, 488 bytes of evidence.

fn constr_data() -> CoreStepEvidenceV1 {
  decode(constr_data_evidence)
}

test constr_data_proves() {
  proves(constr_data())
}

test constr_data_presents_by_summary() {
  presents(constr_data(), [1], True)
}

test constr_data_refuses_argument_1_memory_plus_one() fail {
  proves(with_argument(constr_data(), 1, 1, 0))
}

test constr_data_refuses_argument_1_memory_minus_one() fail {
  proves(with_argument(constr_data(), 1, -1, 0))
}

test constr_data_refuses_argument_1_summary_memory_plus_one() fail {
  proves(with_argument(constr_data(), 1, 0, 1))
}

test constr_data_refuses_argument_1_summary_memory_minus_one() fail {
  proves(with_argument(constr_data(), 1, 0, -1))
}

test constr_data_refuses_bound_argument_1_memory_plus_one() fail {
  proves(with_bound_argument(constr_data(), 1, 1))
}

test constr_data_refuses_bound_argument_1_memory_minus_one() fail {
  proves(with_bound_argument(constr_data(), 1, -1))
}

test constr_data_refuses_result_memory_plus_one() fail {
  proves(with_result(constr_data(), 1, 0))
}

test constr_data_refuses_result_memory_minus_one() fail {
  proves(with_result(constr_data(), -1, 0))
}

test constr_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(constr_data(), 0, 1))
}

test constr_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(constr_data(), 0, -1))
}

// listData: executeBuiltinSemantic 39, 474 bytes of evidence.

fn list_data() -> CoreStepEvidenceV1 {
  decode(list_data_evidence)
}

test list_data_proves() {
  proves(list_data())
}

test list_data_proves_on_the_split_chain() {
  split_proves(list_data())
}

test list_data_presents_by_summary() {
  presents(list_data(), [0], True)
}

test list_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(list_data(), 0, 1, 0))
}

test list_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(list_data(), 0, 1, 0))
}

test list_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(list_data(), 0, -1, 0))
}

test list_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(list_data(), 0, -1, 0))
}

test list_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(list_data(), 0, 0, 1))
}

test list_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(list_data(), 0, 0, 1))
}

test list_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(list_data(), 0, 0, -1))
}

test list_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(list_data(), 0, 0, -1))
}

test list_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(list_data(), 0, 1))
}

test list_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(list_data(), 0, 1))
}

test list_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(list_data(), 0, -1))
}

test list_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(list_data(), 0, -1))
}

test list_data_refuses_result_memory_plus_one() fail {
  proves(with_result(list_data(), 1, 0))
}

test list_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(list_data(), 1, 0))
}

test list_data_refuses_result_memory_minus_one() fail {
  proves(with_result(list_data(), -1, 0))
}

test list_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(list_data(), -1, 0))
}

test list_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(list_data(), 0, 1))
}

test list_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(list_data(), 0, 1))
}

test list_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(list_data(), 0, -1))
}

test list_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(list_data(), 0, -1))
}

// iData: executeBuiltinSemantic 40, 362 bytes of evidence.

fn i_data() -> CoreStepEvidenceV1 {
  decode(i_data_evidence)
}

test i_data_proves() {
  proves(i_data())
}

test i_data_proves_on_the_split_chain() {
  split_proves(i_data())
}

test i_data_presents_by_summary() {
  presents(i_data(), [], True)
}

test i_data_refuses_result_memory_plus_one() fail {
  proves(with_result(i_data(), 1, 0))
}

test i_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(i_data(), 1, 0))
}

test i_data_refuses_result_memory_minus_one() fail {
  proves(with_result(i_data(), -1, 0))
}

test i_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(i_data(), -1, 0))
}

test i_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(i_data(), 0, 1))
}

test i_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(i_data(), 0, 1))
}

test i_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(i_data(), 0, -1))
}

test i_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(i_data(), 0, -1))
}

// unConstrData: executeBuiltinSemantic 42, 477 bytes of evidence.

fn un_constr_data() -> CoreStepEvidenceV1 {
  decode(un_constr_data_evidence)
}

test un_constr_data_proves() {
  proves(un_constr_data())
}

test un_constr_data_proves_on_the_split_chain() {
  split_proves(un_constr_data())
}

test un_constr_data_presents_by_summary() {
  presents(un_constr_data(), [0], True)
}

test un_constr_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(un_constr_data(), 0, 1, 0))
}

test un_constr_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_constr_data(), 0, 1, 0))
}

test un_constr_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(un_constr_data(), 0, -1, 0))
}

test un_constr_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_constr_data(), 0, -1, 0))
}

test un_constr_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(un_constr_data(), 0, 0, 1))
}

test un_constr_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_constr_data(), 0, 0, 1))
}

test un_constr_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(un_constr_data(), 0, 0, -1))
}

test un_constr_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_constr_data(), 0, 0, -1))
}

test un_constr_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(un_constr_data(), 0, 1))
}

test un_constr_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(un_constr_data(), 0, 1))
}

test un_constr_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(un_constr_data(), 0, -1))
}

test un_constr_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(un_constr_data(), 0, -1))
}

test un_constr_data_refuses_result_memory_plus_one() fail {
  proves(with_result(un_constr_data(), 1, 0))
}

test un_constr_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(un_constr_data(), 1, 0))
}

test un_constr_data_refuses_result_memory_minus_one() fail {
  proves(with_result(un_constr_data(), -1, 0))
}

test un_constr_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(un_constr_data(), -1, 0))
}

test un_constr_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(un_constr_data(), 0, 1))
}

test un_constr_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(un_constr_data(), 0, 1))
}

test un_constr_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(un_constr_data(), 0, -1))
}

test un_constr_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(un_constr_data(), 0, -1))
}

// unListData: executeBuiltinSemantic 44, 474 bytes of evidence.

fn un_list_data() -> CoreStepEvidenceV1 {
  decode(un_list_data_evidence)
}

test un_list_data_proves() {
  proves(un_list_data())
}

test un_list_data_proves_on_the_split_chain() {
  split_proves(un_list_data())
}

test un_list_data_presents_by_summary() {
  presents(un_list_data(), [0], True)
}

test un_list_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(un_list_data(), 0, 1, 0))
}

test un_list_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_list_data(), 0, 1, 0))
}

test un_list_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(un_list_data(), 0, -1, 0))
}

test un_list_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_list_data(), 0, -1, 0))
}

test un_list_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(un_list_data(), 0, 0, 1))
}

test un_list_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_list_data(), 0, 0, 1))
}

test un_list_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(un_list_data(), 0, 0, -1))
}

test un_list_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_list_data(), 0, 0, -1))
}

test un_list_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(un_list_data(), 0, 1))
}

test un_list_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(un_list_data(), 0, 1))
}

test un_list_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(un_list_data(), 0, -1))
}

test un_list_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(un_list_data(), 0, -1))
}

test un_list_data_refuses_result_memory_plus_one() fail {
  proves(with_result(un_list_data(), 1, 0))
}

test un_list_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(un_list_data(), 1, 0))
}

test un_list_data_refuses_result_memory_minus_one() fail {
  proves(with_result(un_list_data(), -1, 0))
}

test un_list_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(un_list_data(), -1, 0))
}

test un_list_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(un_list_data(), 0, 1))
}

test un_list_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(un_list_data(), 0, 1))
}

test un_list_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(un_list_data(), 0, -1))
}

test un_list_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(un_list_data(), 0, -1))
}

// unBData: executeBuiltinSemantic 46, 364 bytes of evidence.

fn un_b_data() -> CoreStepEvidenceV1 {
  decode(un_b_data_evidence)
}

test un_b_data_proves() {
  proves(un_b_data())
}

test un_b_data_proves_on_the_split_chain() {
  split_proves(un_b_data())
}

test un_b_data_presents_by_summary() {
  presents(un_b_data(), [0], False)
}

test un_b_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(un_b_data(), 0, 1, 0))
}

test un_b_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_b_data(), 0, 1, 0))
}

test un_b_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(un_b_data(), 0, -1, 0))
}

test un_b_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_b_data(), 0, -1, 0))
}

test un_b_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(un_b_data(), 0, 0, 1))
}

test un_b_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_b_data(), 0, 0, 1))
}

test un_b_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(un_b_data(), 0, 0, -1))
}

test un_b_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(un_b_data(), 0, 0, -1))
}

test un_b_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(un_b_data(), 0, 1))
}

test un_b_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(un_b_data(), 0, 1))
}

test un_b_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(un_b_data(), 0, -1))
}

test un_b_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(un_b_data(), 0, -1))
}

// equalsData over equal lists: executeBuiltinSemantic 47, 369 bytes of evidence.

fn equals_data_true() -> CoreStepEvidenceV1 {
  decode(equals_data_true_evidence)
}

test equals_data_true_proves() {
  proves(equals_data_true())
}

test equals_data_true_proves_on_the_split_chain() {
  split_proves(equals_data_true())
}

test equals_data_true_presents_by_summary() {
  presents(equals_data_true(), [0, 1], False)
}

test equals_data_true_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(equals_data_true(), 0, 1, 0))
}

test equals_data_true_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 0, 1, 0))
}

test equals_data_true_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(equals_data_true(), 0, -1, 0))
}

test equals_data_true_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 0, -1, 0))
}

test equals_data_true_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(equals_data_true(), 0, 0, 1))
}

test equals_data_true_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 0, 0, 1))
}

test equals_data_true_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(equals_data_true(), 0, 0, -1))
}

test equals_data_true_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 0, 0, -1))
}

test equals_data_true_refuses_argument_1_memory_plus_one() fail {
  proves(with_argument(equals_data_true(), 1, 1, 0))
}

test equals_data_true_refuses_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 1, 1, 0))
}

test equals_data_true_refuses_argument_1_memory_minus_one() fail {
  proves(with_argument(equals_data_true(), 1, -1, 0))
}

test equals_data_true_refuses_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 1, -1, 0))
}

test equals_data_true_refuses_argument_1_summary_memory_plus_one() fail {
  proves(with_argument(equals_data_true(), 1, 0, 1))
}

test equals_data_true_refuses_argument_1_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 1, 0, 1))
}

test equals_data_true_refuses_argument_1_summary_memory_minus_one() fail {
  proves(with_argument(equals_data_true(), 1, 0, -1))
}

test equals_data_true_refuses_argument_1_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(equals_data_true(), 1, 0, -1))
}

test equals_data_true_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(equals_data_true(), 0, 1))
}

test equals_data_true_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(equals_data_true(), 0, 1))
}

test equals_data_true_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(equals_data_true(), 0, -1))
}

test equals_data_true_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(equals_data_true(), 0, -1))
}

test equals_data_true_refuses_bound_argument_1_memory_plus_one() fail {
  proves(with_bound_argument(equals_data_true(), 1, 1))
}

test equals_data_true_refuses_bound_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(equals_data_true(), 1, 1))
}

test equals_data_true_refuses_bound_argument_1_memory_minus_one() fail {
  proves(with_bound_argument(equals_data_true(), 1, -1))
}

test equals_data_true_refuses_bound_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(equals_data_true(), 1, -1))
}

test equals_data_true_refuses_false_from_a_moved_summary_memory() fail {
  proves(equals_claimed_false(equals_data_true()))
}

test equals_data_true_refuses_false_from_a_moved_summary_memory_on_the_split_chain() fail {
  split_proves(equals_claimed_false(equals_data_true()))
}

// mkPairData: executeBuiltinSemantic 48, 404 bytes of evidence.

fn mk_pair_data() -> CoreStepEvidenceV1 {
  decode(mk_pair_data_evidence)
}

test mk_pair_data_proves() {
  proves(mk_pair_data())
}

test mk_pair_data_proves_on_the_split_chain() {
  split_proves(mk_pair_data())
}

test mk_pair_data_presents_by_summary() {
  presents(mk_pair_data(), [0, 1], True)
}

test mk_pair_data_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(mk_pair_data(), 0, 1, 0))
}

test mk_pair_data_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 0, 1, 0))
}

test mk_pair_data_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(mk_pair_data(), 0, -1, 0))
}

test mk_pair_data_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 0, -1, 0))
}

test mk_pair_data_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(mk_pair_data(), 0, 0, 1))
}

test mk_pair_data_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 0, 0, 1))
}

test mk_pair_data_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(mk_pair_data(), 0, 0, -1))
}

test mk_pair_data_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 0, 0, -1))
}

test mk_pair_data_refuses_argument_1_memory_plus_one() fail {
  proves(with_argument(mk_pair_data(), 1, 1, 0))
}

test mk_pair_data_refuses_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 1, 1, 0))
}

test mk_pair_data_refuses_argument_1_memory_minus_one() fail {
  proves(with_argument(mk_pair_data(), 1, -1, 0))
}

test mk_pair_data_refuses_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 1, -1, 0))
}

test mk_pair_data_refuses_argument_1_summary_memory_plus_one() fail {
  proves(with_argument(mk_pair_data(), 1, 0, 1))
}

test mk_pair_data_refuses_argument_1_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 1, 0, 1))
}

test mk_pair_data_refuses_argument_1_summary_memory_minus_one() fail {
  proves(with_argument(mk_pair_data(), 1, 0, -1))
}

test mk_pair_data_refuses_argument_1_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(mk_pair_data(), 1, 0, -1))
}

test mk_pair_data_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(mk_pair_data(), 0, 1))
}

test mk_pair_data_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_pair_data(), 0, 1))
}

test mk_pair_data_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(mk_pair_data(), 0, -1))
}

test mk_pair_data_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_pair_data(), 0, -1))
}

test mk_pair_data_refuses_bound_argument_1_memory_plus_one() fail {
  proves(with_bound_argument(mk_pair_data(), 1, 1))
}

test mk_pair_data_refuses_bound_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_pair_data(), 1, 1))
}

test mk_pair_data_refuses_bound_argument_1_memory_minus_one() fail {
  proves(with_bound_argument(mk_pair_data(), 1, -1))
}

test mk_pair_data_refuses_bound_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(mk_pair_data(), 1, -1))
}

test mk_pair_data_refuses_a_pair_following_a_moved_summary_memory() fail {
  proves(pair_following_moved_summary(mk_pair_data()))
}

test mk_pair_data_refuses_a_pair_following_a_moved_summary_memory_on_the_split_chain() fail {
  split_proves(pair_following_moved_summary(mk_pair_data()))
}

test mk_pair_data_refuses_result_memory_plus_one() fail {
  proves(with_result(mk_pair_data(), 1, 0))
}

test mk_pair_data_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_pair_data(), 1, 0))
}

test mk_pair_data_refuses_result_memory_minus_one() fail {
  proves(with_result(mk_pair_data(), -1, 0))
}

test mk_pair_data_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_pair_data(), -1, 0))
}

test mk_pair_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(mk_pair_data(), 0, 1))
}

test mk_pair_data_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_pair_data(), 0, 1))
}

test mk_pair_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(mk_pair_data(), 0, -1))
}

test mk_pair_data_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(mk_pair_data(), 0, -1))
}

// mkNilData: executeBuiltinSemantic 49, 321 bytes of evidence.

fn mk_nil_data() -> CoreStepEvidenceV1 {
  decode(mk_nil_data_evidence)
}

test mk_nil_data_proves() {
  proves(mk_nil_data())
}

test mk_nil_data_presents_by_summary() {
  presents(mk_nil_data(), [], True)
}

test mk_nil_data_refuses_result_memory_plus_one() fail {
  proves(with_result(mk_nil_data(), 1, 0))
}

test mk_nil_data_refuses_result_memory_minus_one() fail {
  proves(with_result(mk_nil_data(), -1, 0))
}

test mk_nil_data_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(mk_nil_data(), 0, 1))
}

test mk_nil_data_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(mk_nil_data(), 0, -1))
}

// headList over another list<data> with the same head: executeBuiltinSemantic 33, 591 bytes of evidence.

fn head_list_data_same_head() -> CoreStepEvidenceV1 {
  decode(head_list_data_same_head_evidence)
}

test head_list_data_same_head_proves() {
  proves(head_list_data_same_head())
}

test head_list_data_same_head_proves_on_the_split_chain() {
  split_proves(head_list_data_same_head())
}

test head_list_data_same_head_reaches_the_post_state_of_head_list_data() {
  head_list_data_same_head().post == head_list_data().post
}

// headList over [0, 1, 2] with the witness of headList over [0, 5]: the
// same head and budget give the same post-state, so only the binding of
// the arguments to the pre-state's builtin value refuses it.
test head_list_data_refuses_the_witness_of_another_source() fail {
  proves(
    CoreStepEvidenceV1 {
      ..head_list_data(),
      witness: head_list_data_same_head().witness,
    },
  )
}

test head_list_data_refuses_the_witness_of_another_source_on_the_split_chain() fail {
  split_proves(
    CoreStepEvidenceV1 {
      ..head_list_data(),
      witness: head_list_data_same_head().witness,
    },
  )
}

// headList over an empty list (failure): executeBuiltinSemanticFailure 33, 347 bytes of evidence.

fn head_list_empty() -> CoreStepEvidenceV1 {
  decode(head_list_empty_evidence)
}

test head_list_empty_proves() {
  proves(head_list_empty())
}

test head_list_empty_presents_by_summary() {
  presents(head_list_empty(), [0], False)
}

test head_list_empty_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(head_list_empty(), 0, 1, 0))
}

test head_list_empty_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(head_list_empty(), 0, -1, 0))
}

test head_list_empty_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(head_list_empty(), 0, 0, 1))
}

test head_list_empty_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(head_list_empty(), 0, 0, -1))
}

test head_list_empty_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(head_list_empty(), 0, 1))
}

test head_list_empty_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(head_list_empty(), 0, -1))
}

// sndPair over a 9,200-item list: executeBuiltinSemantic 30, 668 bytes of evidence.

fn max_snd_pair_9200() -> CoreStepEvidenceV1 {
  decode(max_snd_pair_9200_evidence)
}

test max_snd_pair_9200_proves() {
  proves(max_snd_pair_9200())
}

test max_snd_pair_9200_proves_on_the_split_chain() {
  split_proves(max_snd_pair_9200())
}

test max_snd_pair_9200_presents_by_summary() {
  presents(max_snd_pair_9200(), [0], True)
}

test max_snd_pair_9200_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(max_snd_pair_9200(), 0, 1, 0))
}

test max_snd_pair_9200_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_snd_pair_9200(), 0, 1, 0))
}

test max_snd_pair_9200_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(max_snd_pair_9200(), 0, -1, 0))
}

test max_snd_pair_9200_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_snd_pair_9200(), 0, -1, 0))
}

test max_snd_pair_9200_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(max_snd_pair_9200(), 0, 0, 1))
}

test max_snd_pair_9200_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_snd_pair_9200(), 0, 0, 1))
}

test max_snd_pair_9200_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(max_snd_pair_9200(), 0, 0, -1))
}

test max_snd_pair_9200_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_snd_pair_9200(), 0, 0, -1))
}

test max_snd_pair_9200_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(max_snd_pair_9200(), 0, 1))
}

test max_snd_pair_9200_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_snd_pair_9200(), 0, 1))
}

test max_snd_pair_9200_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(max_snd_pair_9200(), 0, -1))
}

test max_snd_pair_9200_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_snd_pair_9200(), 0, -1))
}

test max_snd_pair_9200_refuses_result_memory_plus_one() fail {
  proves(with_result(max_snd_pair_9200(), 1, 0))
}

test max_snd_pair_9200_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(max_snd_pair_9200(), 1, 0))
}

test max_snd_pair_9200_refuses_result_memory_minus_one() fail {
  proves(with_result(max_snd_pair_9200(), -1, 0))
}

test max_snd_pair_9200_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(max_snd_pair_9200(), -1, 0))
}

test max_snd_pair_9200_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(max_snd_pair_9200(), 0, 1))
}

test max_snd_pair_9200_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(max_snd_pair_9200(), 0, 1))
}

test max_snd_pair_9200_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(max_snd_pair_9200(), 0, -1))
}

test max_snd_pair_9200_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(max_snd_pair_9200(), 0, -1))
}

// mkCons onto a 9,200-item list: executeBuiltinSemantic 32, 548 bytes of evidence.

fn max_mk_cons_data_9200() -> CoreStepEvidenceV1 {
  decode(max_mk_cons_data_9200_evidence)
}

test max_mk_cons_data_9200_proves() {
  proves(max_mk_cons_data_9200())
}

test max_mk_cons_data_9200_proves_on_the_split_chain() {
  split_proves(max_mk_cons_data_9200())
}

test max_mk_cons_data_9200_presents_by_summary() {
  presents(max_mk_cons_data_9200(), [0, 1], True)
}

test max_mk_cons_data_9200_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 0, 1, 0))
}

test max_mk_cons_data_9200_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 0, 1, 0))
}

test max_mk_cons_data_9200_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 0, -1, 0))
}

test max_mk_cons_data_9200_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 0, -1, 0))
}

test max_mk_cons_data_9200_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 0, 0, 1))
}

test max_mk_cons_data_9200_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 0, 0, 1))
}

test max_mk_cons_data_9200_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 0, 0, -1))
}

test max_mk_cons_data_9200_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 0, 0, -1))
}

test max_mk_cons_data_9200_refuses_argument_1_memory_plus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 1, 1, 0))
}

test max_mk_cons_data_9200_refuses_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 1, 1, 0))
}

test max_mk_cons_data_9200_refuses_argument_1_memory_minus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 1, -1, 0))
}

test max_mk_cons_data_9200_refuses_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 1, -1, 0))
}

test max_mk_cons_data_9200_refuses_argument_1_summary_memory_plus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 1, 0, 1))
}

test max_mk_cons_data_9200_refuses_argument_1_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 1, 0, 1))
}

test max_mk_cons_data_9200_refuses_argument_1_summary_memory_minus_one() fail {
  proves(with_argument(max_mk_cons_data_9200(), 1, 0, -1))
}

test max_mk_cons_data_9200_refuses_argument_1_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_mk_cons_data_9200(), 1, 0, -1))
}

test max_mk_cons_data_9200_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(max_mk_cons_data_9200(), 0, 1))
}

test max_mk_cons_data_9200_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_mk_cons_data_9200(), 0, 1))
}

test max_mk_cons_data_9200_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(max_mk_cons_data_9200(), 0, -1))
}

test max_mk_cons_data_9200_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_mk_cons_data_9200(), 0, -1))
}

test max_mk_cons_data_9200_refuses_bound_argument_1_memory_plus_one() fail {
  proves(with_bound_argument(max_mk_cons_data_9200(), 1, 1))
}

test max_mk_cons_data_9200_refuses_bound_argument_1_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_mk_cons_data_9200(), 1, 1))
}

test max_mk_cons_data_9200_refuses_bound_argument_1_memory_minus_one() fail {
  proves(with_bound_argument(max_mk_cons_data_9200(), 1, -1))
}

test max_mk_cons_data_9200_refuses_bound_argument_1_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_mk_cons_data_9200(), 1, -1))
}

test max_mk_cons_data_9200_refuses_result_memory_plus_one() fail {
  proves(with_result(max_mk_cons_data_9200(), 1, 0))
}

test max_mk_cons_data_9200_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(max_mk_cons_data_9200(), 1, 0))
}

test max_mk_cons_data_9200_refuses_result_memory_minus_one() fail {
  proves(with_result(max_mk_cons_data_9200(), -1, 0))
}

test max_mk_cons_data_9200_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(max_mk_cons_data_9200(), -1, 0))
}

test max_mk_cons_data_9200_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(max_mk_cons_data_9200(), 0, 1))
}

test max_mk_cons_data_9200_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(max_mk_cons_data_9200(), 0, 1))
}

test max_mk_cons_data_9200_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(max_mk_cons_data_9200(), 0, -1))
}

test max_mk_cons_data_9200_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(max_mk_cons_data_9200(), 0, -1))
}

// unConstrData of a 9,200-field constructor: executeBuiltinSemantic 42, 501 bytes of evidence.

fn max_un_constr_data_9200() -> CoreStepEvidenceV1 {
  decode(max_un_constr_data_9200_evidence)
}

test max_un_constr_data_9200_proves() {
  proves(max_un_constr_data_9200())
}

test max_un_constr_data_9200_proves_on_the_split_chain() {
  split_proves(max_un_constr_data_9200())
}

test max_un_constr_data_9200_presents_by_summary() {
  presents(max_un_constr_data_9200(), [0], True)
}

test max_un_constr_data_9200_refuses_argument_0_memory_plus_one() fail {
  proves(with_argument(max_un_constr_data_9200(), 0, 1, 0))
}

test max_un_constr_data_9200_refuses_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_un_constr_data_9200(), 0, 1, 0))
}

test max_un_constr_data_9200_refuses_argument_0_memory_minus_one() fail {
  proves(with_argument(max_un_constr_data_9200(), 0, -1, 0))
}

test max_un_constr_data_9200_refuses_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_un_constr_data_9200(), 0, -1, 0))
}

test max_un_constr_data_9200_refuses_argument_0_summary_memory_plus_one() fail {
  proves(with_argument(max_un_constr_data_9200(), 0, 0, 1))
}

test max_un_constr_data_9200_refuses_argument_0_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_un_constr_data_9200(), 0, 0, 1))
}

test max_un_constr_data_9200_refuses_argument_0_summary_memory_minus_one() fail {
  proves(with_argument(max_un_constr_data_9200(), 0, 0, -1))
}

test max_un_constr_data_9200_refuses_argument_0_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_argument(max_un_constr_data_9200(), 0, 0, -1))
}

test max_un_constr_data_9200_refuses_bound_argument_0_memory_plus_one() fail {
  proves(with_bound_argument(max_un_constr_data_9200(), 0, 1))
}

test max_un_constr_data_9200_refuses_bound_argument_0_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_un_constr_data_9200(), 0, 1))
}

test max_un_constr_data_9200_refuses_bound_argument_0_memory_minus_one() fail {
  proves(with_bound_argument(max_un_constr_data_9200(), 0, -1))
}

test max_un_constr_data_9200_refuses_bound_argument_0_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_bound_argument(max_un_constr_data_9200(), 0, -1))
}

test max_un_constr_data_9200_refuses_result_memory_plus_one() fail {
  proves(with_result(max_un_constr_data_9200(), 1, 0))
}

test max_un_constr_data_9200_refuses_result_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(max_un_constr_data_9200(), 1, 0))
}

test max_un_constr_data_9200_refuses_result_memory_minus_one() fail {
  proves(with_result(max_un_constr_data_9200(), -1, 0))
}

test max_un_constr_data_9200_refuses_result_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(max_un_constr_data_9200(), -1, 0))
}

test max_un_constr_data_9200_refuses_result_summary_memory_plus_one() fail {
  proves(with_result(max_un_constr_data_9200(), 0, 1))
}

test max_un_constr_data_9200_refuses_result_summary_memory_plus_one_on_the_split_chain() fail {
  split_proves(with_result(max_un_constr_data_9200(), 0, 1))
}

test max_un_constr_data_9200_refuses_result_summary_memory_minus_one() fail {
  proves(with_result(max_un_constr_data_9200(), 0, -1))
}

test max_un_constr_data_9200_refuses_result_summary_memory_minus_one_on_the_split_chain() fail {
  split_proves(with_result(max_un_constr_data_9200(), 0, -1))
}
