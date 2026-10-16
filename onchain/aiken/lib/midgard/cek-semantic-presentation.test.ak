// Semantic builtin steps (tags 29 to 50 except mapData and unMapData) present
// each Data argument, each list or pair argument whose memory the verifier
// derives from its payload nodes, and each Data, list or pair result by its
// semantic summary. A summary's value root commits its type, payload root,
// payload CBOR length and memory, but not its payload's summary memory.
//
// Each evidence below is the step the TypeScript executor emits for the named
// program (`cek-executor.semantic-presentation.test.ts` checks that it still
// does). It is proved by `verify_core_step_v1` and, where the split chain
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
const fst_pair_string_data_evidence =
  #"d8799fd8799f03005820c4eee94742c56e6c884e903c89fd42337fe09f52a63fa7d8afe6921f76a6d65358200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001b5e4190320ffd8799f0100582039839c551c76131ab26e960df886140119244f6cd68c054b35736bf7ccbf491358200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0003e02b190340ffd905189f181d9fd87a9f459f060208ffd8799f5820dd996d6cf7e94da04fd56b4150d0c7730213e6188777d440627fc9e93569d688090eff05ffffd8799fd8799f439f02ff43426162ffffd8799f9fd8799f000258203e11c4022f66792431e6d61281d9b7149f02532fbcbbb9fc4a3785a64fb7b0e4090effd87e9f5820509061931b91f10431e707251f1d5bb00c1498e3c9b326dfffce6ac933295264020305ffd87e9f58205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df3010205ffff9fd8799f5820007e3ec8e31e63cf9010491f228303be2357711b9b9affde9f84332b41dce31403055820742cb91a806ab46c7bbf87eb1ecd4a9a4816d035508e69b537e2393b1d39f44702050affd8799f58204381abbfd79dcadd705f63a2d900d07ce336e8362e530aed047478ba1d29a84e020558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010205ffff8080ffffff"

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
const snd_pair_data_evidence =
  #"d8799fd8799f03005820a67c10227c7ff578b9ef3a54b3f9aff604f0eb938e3d86af93efdad46e56018b58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001b5e4190320ffd8799f010058208d91d548ae3910272b6d5aa444c11f62b697664dbaa169e8d9862ff68401359858200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0003e08c190340ffd905189f181e9fd87a9f459f060808ffd8799f58208f9c774f50c9b4328c3b799d4beb67e2508bb54f8c0aee094888c20420f8be1e0917ff13ffffd87a9f439f08ffd8799f5820648263e7d66193122b23c5e8d2f950781d802f797ff26d7920a0b0b1b56a9f1d040eff0effd8799f9fd8799f000258202ac9128743701667c8a6ab6a214c0536de53934cd719f6d4ae8c43dd576df6c40917ffd87d9f5820f5d357b4c6dfca1299eb8ce945f37760114b116de45cbd171b9372453427df2f0105ffd87c9f0258204df4a1caa8fbc61c6646ec16fa08e157584c8e80413a1d31a5b72e5a6f11fb29040effff9fd8799f5820c6ea1acd5b625461ace9aa8a16dee74f36c8f859de076aace7cbf4a78ae24e1701055820150c2b3ab2850acf266e8f751224ddf92e56f78b36eb0c68d21115d1d84bb689020513ffd8799f5820648263e7d66193122b23c5e8d2f950781d802f797ff26d7920a0b0b1b56a9f1d040e58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa5601040effff8080ffffff"

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
const choose_list_data_evidence =
  #"d8799fd8799f03005820dc3e6feb659b10422b12d800d31db6e9436bf693134dfe286159f08640afd40b58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0002afe41904b0ffd8799f01005820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd458200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0004b7661904d0ffd905189f181f9fd87a9f449f0508ffd8799f58202ecae2f2991366be54858dcab1b16daad46aa34a4d3fd92c19a093a4700ccc930309ff05ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd8799f9fd87c9f015820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e7630309ffff9fd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const mk_cons_data_evidence =
  #"d8799fd8799f030058205a1f23204d814043b1972cc8049fd10622cf065813dfc92a5758bc257d46156858200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001f464190384ffd8799f01005820a0f2ce36e78e5bb688ca490a205baa41b71784aa1f83d9f1f47e6c33294026e558200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00030f0e1903a4ffd905189f18209fd87a9f439f08ffd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ff05ffd87a9f449f0508ffd8799f5820648263e7d66193122b23c5e8d2f950781d802f797ff26d7920a0b0b1b56a9f1d040eff0affffd87a9f449f0508ffd8799f5820ee8abc6ca1fe9f075236ebc8e00a0af952cddb59d6b0574f4eaddd2a03fedfde0513ff0fffd8799f9fd87c9f0258204df4a1caa8fbc61c6646ec16fa08e157584c8e80413a1d31a5b72e5a6f11fb29040effff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da201055820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e76302020affff8080ffffff"

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
const mk_cons_int_evidence =
  #"d8799fd8799f03005820db927e20144e99936715ab4a6befadb0925dd04234f635b2e41bcf27870a1fc058200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001f464190384ffd8799f01005820300599811cf408981490d131d66b1a682fb2adb60ea4447e644e5485c8b411d758200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00030f0e1903a4ffd905189f18209fd8799fd8799f439f00ff4100ffffd87a9f449f0500ffd8799f58209f30cbc22f001062f31bff94b478d6873a20ce141ba242f88290bba39b5fc208040eff02ffffd87a9f449f0500ffd8799f5820244ca35b10e43e55af22d97e802d936ffa0e2b5dea9e8ef2c4211da8942e0e010513ff03ffd8799f9fd87c9f025820981e28174fd74bbfff405b0bd187b5fed9a16841ca3ec98b92a8f24cedd9135c040effff9fd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc01055820c8ef2b88a50f28b7fc01a281863b82fc0ce09054c8f18cba60a063c3f650ffd102020affff8080ffffff"

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
const head_list_data_evidence =
  #"d8799fd8799f03005820514f2d4ab2b9fa32bf145b9c1a24f2ecaf3d80fdd5eeb6999730de07578977a958200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000177641902bcffd8799f01005820610ee0cd1abfeb1aa04e3771794640ba428786a79cd27d9665cf50f41b01e9fd58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0002bc321902dcffd905189f18219fd87a9f449f0508ffd8799f5820244ca35b10e43e55af22d97e802d936ffa0e2b5dea9e8ef2c4211da8942e0e010513ff0fffffd87a9f439f08ffd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ff05ffd8799f9fd87c9f03582020d7fdaad56a2a8ca3d7e837233b1c7bf729d5c2e55a364aa740e217c77135fa0513ffd87d9f58205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df30105ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da201055820981e28174fd74bbfff405b0bd187b5fed9a16841ca3ec98b92a8f24cedd9135c03030fffd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc01055820c8ef2b88a50f28b7fc01a281863b82fc0ce09054c8f18cba60a063c3f650ffd102020affff8080ffffff"

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
const tail_list_int_evidence =
  #"d8799fd8799f03005820728e0eb10ae662ebab001c80b2ef4acf46f65a4aa9b9f1eaf20e18598c0e1c7e58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000177641902bcffd8799f01005820ed4625b061d149c45dcfe0ef3d04bb345e7b0af5bec47e88c8017e737025f1ce58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0002b6631902dcffd905189f18229fd87a9f449f0500ffd8799f58208e04f9bcd808b64ab22a46e351243849f301404850e84f32317d1041d5f16bd5040eff02ffffd87a9f449f0500ffd8799f5820d52cbca42ea3585298a4674d24171014d878ebf3e36e53357dcd87b8090a0f460309ff01ffd8799f9fd87c9f0258204bebe880d9b88d54bcba7d99d0f192e99df39bfa322e59db2e41afd61e8fae0a040effd87d9f5820bad0e7df9497380fa6a8c422c637a39556ff827981382bbdeb7fd2072d3d26410105ffff9fd8799f58203a7732f0d7f55f340277935f91d354298613548be5f495b25d086a5d118cd7ca0105582087524b7468a1445f33af439c9a89a332e9c3f40e813d692ab77fe105096594b702020affd8799f5820df6f4f7673dac1cc9348287eaf4704f41f7b898afe82d702cd6f8781312797af010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const null_list_data_evidence =
  #"d8799fd8799f030058202812d2ec6003cfbe5c09fa214221ba3943bd1152efdefced773da5903a41dda658200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000177641902bcffd8799f01005820dfeef33425971de506ef1728b9379c568f08edbfb9d73bcf423c48677884c11158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00029a251902dcffd905189f18239fd87a9f449f0508ffd8799f5820d066fb41f1d0e3faa19bd86b0faf7f62f51a0c976d9a2f4d6894a2bd50c5ba470309ff05ffffd8799fd8799f439f04ff43d87980ffffd8799f9fd87c9f0158208a9bccabac40fbaae6aaf8309c5ab48ec8cba3c14d597979f975c9194a1a04d90309ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const choose_data_evidence =
  #"d8799fd8799f03005820e67179fbf4f7438b66e6074a3807afc8bcecfc139f4c775bd48abd9ea74e9af558200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0003e8641906a4ffd8799f01005820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd458200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0005590b1906c4ffd905189f18249fd87a9f439f08ffd8799f5820d066fb41f1d0e3faa19bd86b0faf7f62f51a0c976d9a2f4d6894a2bd50c5ba470309ff09ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffffd87b9f5820cb693b241e549307cd1cfa9a07b39ef073b4de207de70dcefac3269de34b8fd4ffd8799f9fd87c9f0158208a9bccabac40fbaae6aaf8309c5ab48ec8cba3c14d597979f975c9194a1a04d90309ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const constr_data_evidence =
  #"d8799fd8799f03005820fcd35cc023986231a5c0bbb154cf3c9b12049982b80db7879fa0b4a3876be9a858200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001b5e4190320ffd8799f0100582046d195dfca7983fc916a211c763bbdfb401536aca8c81f72353ab2a2b1ef0b6458200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00020c6b190340ffd905189f18259fd8799fd8799f439f00ff4103ffffd87a9f449f0508ffd8799f5820d066fb41f1d0e3faa19bd86b0faf7f62f51a0c976d9a2f4d6894a2bd50c5ba470309ff05ffffd87a9f439f08ffd8799f582076cb3422c2cb8976388bc9d1b48d9bcef7593e7477d24ed1cff1483e51579d750509ff09ffd8799f9fd87c9f0158208a9bccabac40fbaae6aaf8309c5ab48ec8cba3c14d597979f975c9194a1a04d90309ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const list_data_evidence =
  #"d8799fd8799f03005820a135963d4c0c4232a5cee4ea9ba384dfbd9c2c6a7a2d6b0338a103d70a0877b658200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f010058200b3bd88ed06f1eb090f56871c3ee5bc9b4fbb17cd94820862760664e5ac9c3c758200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001bd20190278ffd905189f18279fd87a9f449f0508ffd8799f582042e843fb4303fc7832a7a13d6ab16eec23c5d154c4a133c7ba82d83e19e4b39c050eff0affffd87a9f439f08ffd8799f582042e843fb4303fc7832a7a13d6ab16eec23c5d154c4a133c7ba82d83e19e4b39c050eff0effd8799f9fd87c9f025820247f0b0f2f2e0a23d2a7db3b8dcb5489e8eefa81f7d632b35ad1919b3ddf0c00050effff9fd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc01055820d42bce807b2595dc2425adf42be22ae2cb63e7ea8002aeb112b33b476cf9e82702030affff8080ffffff"

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
const i_data_evidence =
  #"d8799fd8799f030058209e5c30e419b431ad2d214239369686ed0907d2bc94789cae37d4a1c08c61f08d58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f0100582006aef2b3dda0298e28d0c18fdfef7446b3b106e94a7c8f14c7a4b873aed98be458200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000174a7190278ffd905189f18289fd8799fd8799f439f00ff4105ffffffd87a9f439f08ffd8799f58203a7732f0d7f55f340277935f91d354298613548be5f495b25d086a5d118cd7ca0105ff05ffd8799f9fd87d9f5820bad0e7df9497380fa6a8c422c637a39556ff827981382bbdeb7fd2072d3d26410105ffff80809f4105ffffffff"

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
const un_constr_data_evidence =
  #"d8799fd8799f030058204648227cf14c5faa1b385646419d4ee30da931f66acd1e4e76af7ca60465018158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f0100582000be226def1ff5cbb01397c51681b8178ef9aa9bcd1b0385a79677957f02ff9b58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000198f0190278ffd905189f182a9fd87a9f439f08ffd8799f5820480e02d9a2308505e1d8b9f10d80fc126a9a870906fe2300cc7620cbfc76373c0509ff09ffffd87a9f469f06000508ffd8799f582033ee044429f6032014df7ce04247999646539186d9545087fde074b9dcc9b6210812ff06ffd8799f9fd8799f02015820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e7630509ffff9fd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const un_list_data_evidence =
  #"d8799fd8799f03005820aa14b1a9d20627bd9cf8eafd74a3619ed4cfb22bdf7bd493f89f4c44c6a630f958200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f01005820b4932e726b5cc25af1137b4e618a7592cd82717eb53883ed5130b285c84301d758200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00019e31190278ffd905189f182c9fd87a9f439f08ffd8799f5820648263e7d66193122b23c5e8d2f950781d802f797ff26d7920a0b0b1b56a9f1d040eff0effffd87a9f449f0508ffd8799f5820648263e7d66193122b23c5e8d2f950781d802f797ff26d7920a0b0b1b56a9f1d040eff0affd8799f9fd87c9f0258204df4a1caa8fbc61c6646ec16fa08e157584c8e80413a1d31a5b72e5a6f11fb29040effff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da201055820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e76302020affff8080ffffff"

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
const un_b_data_evidence =
  #"d8799fd8799f030058200afba32df96a3f312c453c19a2889e4b6abaf10517027d162bf96d35abf365b858200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f01005820b624b8b924fd0dd6ae46cfd11352cb2b0e713db62c2624762959233d27c8e86358200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00018792190278ffd905189f182e9fd87a9f439f08ffd8799f582043e7a387535fa6887938faa692cac7bba54d633f831075a0ffff43c833b2e4530205ff05ffffd8799fd8799f439f01ff4241aaffffd8799f9fd87e9f5820cff70f0c887edb6c327d98d9dbc8618a0cd2634e2bdc16c3b2c0ca83fc8d38dc010205ffff80809f41aaffffffff"

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
const equals_data_true_evidence =
  #"d8799fd8799f03005820afb780d8c7c8835c361643f18a239710196e082344cb7236262de0df6966211558200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001b5e4190320ffd8799f01005820b9d299a51cb8e6181262d67d07662451930d812d4d79348a86d64361f93a889d58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0013294f190321ffd905189f182f9fd87a9f439f08ffd8799f5820d066fb41f1d0e3faa19bd86b0faf7f62f51a0c976d9a2f4d6894a2bd50c5ba470309ff09ffd87a9f439f08ffd8799f5820d066fb41f1d0e3faa19bd86b0faf7f62f51a0c976d9a2f4d6894a2bd50c5ba470309ff09ffffd8799fd8799f439f04ff43d87a80ffffd8799f80808080ffffff"

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
const mk_pair_data_evidence =
  #"d8799fd8799f03005820456707d04a5dd25e3f0301c0ccd1cb32e2244bd696b974011d8641519962758e58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001b5e4190320ffd8799f01005820593e8d24ebf598248127cbaa814ea4986c453b792614811a131b0cf3ac88720a58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001e2fe190340ffd905189f18309fd87a9f439f08ffd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc0105ff05ffd87a9f439f08ffd8799f582052d5cced9e675f6b5371a209dffd33077548f6a90436aa72d87132158a2eff170205ff05ffffd87a9f459f060808ffd8799f58208b12fd91b873d8516eb8d567ccd15a97a2a9ea548467c436e3c5ffe7f66628e7070eff0affd8799f80808080ffffff"

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
const mk_nil_data_evidence =
  #"d8799fd8799f03005820fb2a035a872d26dccfd9283f551460573e7dba9484911f2c4f22e76efd6efc6758200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f010058207b428994842cddefb4cd715f04e6512691d48c0b818d5fc58efedda9307e808658200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001552f190278ffd905189f18319fd8799fd8799f439f03ff43d87980ffffffd87a9f449f0508ffd8799f582040789c0e0bcfa83d22dbad06c2fd2bcbcabef4fad15df776b3f25250f05119980104ff00ffd8799f80808080ffffff"

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
const head_list_data_same_head_evidence =
  #"d8799fd8799f030058201683f610baabb861bb0f2b5cd3fbd0e8055b2e72e2ac310cdf901396a82e0e0b58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000177641902bcffd8799f01005820610ee0cd1abfeb1aa04e3771794640ba428786a79cd27d9665cf50f41b01e9fd58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0002bc321902dcffd905189f18219fd87a9f449f0508ffd8799f5820579b61d4492cbc07cbc9e499290cb27d45aa9d0cc8502d756faec4824b9e0e23040eff0affffd87a9f439f08ffd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ff05ffd8799f9fd87c9f0258206ca9236331cd0205151c693aa3b4b253bbd7f2647f79966ea49b45a4ddebebf7040effd87d9f58205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df30105ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105582056f6e866c0193e534fc82a706f22845d2d83c999a787988f9fedf74f2e9b8b6002020affd8799f58203a7732f0d7f55f340277935f91d354298613548be5f495b25d086a5d118cd7ca010558208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56010105ffff8080ffffff"

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
const head_list_empty_evidence =
  #"d8799fd8799f0300582046c9e9e988d4ccd9275d5f319beb07dd4f8e57605194505cc8ea032417ddbf3a58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000177641902bcffd8799f050058204c623a62d6dedf81bb74b1cf56f0b3e8ec85ed24ffb0b821b2d796c4f85a5d3d58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b071a000177641902bcffd9051d9f18219fd87a9f449f0508ffd8799f582040789c0e0bcfa83d22dbad06c2fd2bcbcabef4fad15df776b3f25250f05119980104ff00ffffd8799f9fd87c9f0058208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560104ffff808080ffffff"

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
const max_snd_pair_9200_evidence =
  #"d8799fd8799f0300582055b703f2b0bf44f333e6ae3dce1a738599cc29d2ebbfea23610a805e74dc61ff58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001b5e4190320ffd8799f010058209f70617df2717a43c27d6263a66951b9c017bb353967f01b81c7991de4950e7d58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0003e08c190340ffd905189f181e9fd87a9f459f060808ffd8799f5820b6cd243366dc7b04a904618b7ff3eded73dfd8ab17db259cc94882ee713278ec1923f719b3bdff19b3b9ffffd87a9f439f08ffd8799f58207599aac83440aabc0dee0fa0e672fd8d8a8234e8768b912335a81d7da281ef5d1923f219b3b4ff19b3b4ffd8799f9fd8799f000258203fb0d3c14534bbe38843bfdb13852bfe7be2886aee76d51e7d0dc516903025dc1923f719b3bdffd87d9f58205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df30105ffd87c9f1923f058200ee97151bbb3b6c6a2e40c19d38c60f296740f5cc9320db030952bff1459137b1923f219b3b4ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105582001a836d9d4ab9d9687b220731401f1217963f3cb8082afad20e07b5b4aa08ed0021923f319b3b9ffd8799f58207599aac83440aabc0dee0fa0e672fd8d8a8234e8768b912335a81d7da281ef5d1923f219b3b458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56011923f219b3b4ffff8080ffffff"

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
const max_mk_cons_data_9200_evidence =
  #"d8799fd8799f030058205baeb3f3cb423910286998c6c3a391fb50707ff4755c7822bb6b89612303310e58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a0001f464190384ffd8799f01005820a89a6a9ce2face9402a1236c9c0a1685d9abaf99b092315dd0082b5710489cf458200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a00030f0e1903a4ffd905189f18209fd87a9f439f08ffd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ff05ffd87a9f449f0508ffd8799f58207599aac83440aabc0dee0fa0e672fd8d8a8234e8768b912335a81d7da281ef5d1923f219b3b4ff19b3b0ffffd87a9f449f0508ffd8799f5820f17dd4399f4fc9cbf4a4f6c056f5dd9ee35edbe551e4bac9ac20c5498a349ce01923f319b3b9ff19b3b5ffd8799f9fd87c9f1923f058200ee97151bbb3b6c6a2e40c19d38c60f296740f5cc9320db030952bff1459137b1923f219b3b4ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da201055820bdfbfb87839d15ddab11625d19a422f1643a7463e2746dceb42272598271fb461923f01923f019b3b0ffff8080ffffff"

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
const max_un_constr_data_9200_evidence =
  #"d8799fd8799f03005820b2e906d03e1865a0182030b6b64ab8b45167ed78d306a43f595758f4a85e3b0858200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000138e4190258ffd8799f0100582001e17fbb682c03e194836bc6bd4490b94914128ca6745e16649e55c348b9b1e858200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582054e25d93eed4bc75270f532273ce9f0b351568d5112669c39459309cb680f94b001a000198f0190278ffd905189f182a9fd87a9f439f08ffd8799f5820e31f9e7032ee0f69b545c231c509c13c99f15914a5af173e5017417e271af1691923f419b3b4ff19b3b4ffffd87a9f469f06000508ffd8799f5820b6cd243366dc7b04a904618b7ff3eded73dfd8ab17db259cc94882ee713278ec1923f719b3bdff19b3b1ffd8799f9fd8799f001923f058200ee97151bbb3b6c6a2e40c19d38c60f296740f5cc9320db030952bff1459137b1923f419b3b4ffff9fd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da201055820bdfbfb87839d15ddab11625d19a422f1643a7463e2746dceb42272598271fb461923f01923f019b3b0ffff8080ffffff"

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
