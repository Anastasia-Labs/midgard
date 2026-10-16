use aiken/builtin
use aiken/cbor
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1

const h_01 = #"0101010101010101010101010101010101010101010101010101010101010101"

const h_02 = #"0202020202020202020202020202020202020202020202020202020202020202"

const h_03 = #"0303030303030303030303030303030303030303030303030303030303030303"

fn state(
  mode: Int,
  focus_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
) -> cek_machine_v1.MachineStateV1 {
  cek_machine_v1.MachineStateV1 {
    mode,
    execution_index: 0,
    focus_root,
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root,
    auxiliary,
    cpu: 16100,
    memory: 200,
  }
}

/// A case frame with one branch, `h_01`, closed over the empty environment
/// and returning to the empty stack.
fn one_branch_frame() -> ByteArray {
  cek_proof_v1.hash_case_continuation_v1(
    1,
    cek_proof_v1.hash_sequence_node_v1(
      h_01,
      cek_proof_v1.empty_sequence_root_v1,
      1,
    ),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
}

fn invalid_scrutinee_step(
  value: cek_machine_v1.MachineValueWitnessV1,
  focus_root: ByteArray,
) -> Bool {
  let pre = state(cek_machine_v1.mode_return, focus_root, one_branch_frame(), 0)
  cek_machine_v1.verify_core_step_v1(
    pre,
    state(
      cek_machine_v1.mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_continuation_root_v1,
      cek_machine_v1.error_invalid_case_scrutinee,
    ),
    cek_machine_v1.ReturnCaseInvalid {
      value,
      branches_count: 1,
      branches_root: cek_proof_v1.hash_sequence_node_v1(
        h_01,
        cek_proof_v1.empty_sequence_root_v1,
        1,
      ),
      captured_environment: cek_proof_v1.empty_environment_root_v1,
      tail: cek_proof_v1.empty_continuation_root_v1,
    },
  )
}

/// Any committed constant of the given canonical type.
fn committed_constant(type_cbor: ByteArray) -> Bool {
  let type_root = cek_proof_v1.hash_blob_chunk_v1(type_cbor)
  invalid_scrutinee_step(
    cek_machine_v1.ConstantValue {
      type_root,
      payload_root: h_02,
      payload_length: 1,
      semantic_root: h_03,
      memory: 1,
    },
    cek_proof_v1.hash_constant_value_v1(type_root, h_02, 1, h_03, 1),
  )
}

// Every constant has its own case arm, so the invalid-scrutinee arm proves
// nothing about one, whichever type it has.
test invalid_scrutinee_arm_refuses_an_integer_constant() {
  !committed_constant(#"9f00ff")
}

test invalid_scrutinee_arm_refuses_a_boolean_constant() {
  !committed_constant(#"9f04ff")
}

test invalid_scrutinee_arm_refuses_a_unit_constant() {
  !committed_constant(#"9f03ff")
}

test invalid_scrutinee_arm_refuses_a_list_constant() {
  !committed_constant(#"9f0500ff")
}

test invalid_scrutinee_arm_refuses_a_pair_constant() {
  !committed_constant(#"9f060004ff")
}

test invalid_scrutinee_arm_refuses_a_bytestring_constant() {
  !committed_constant(#"9f01ff")
}

test invalid_scrutinee_arm_refuses_a_data_constant() {
  !committed_constant(#"9f08ff")
}

test invalid_scrutinee_arm_still_proves_a_delay() {
  invalid_scrutinee_step(
    cek_machine_v1.DelayValue {
      body: h_02,
      environment: cek_proof_v1.empty_environment_root_v1,
    },
    cek_proof_v1.hash_delay_value_v1(
      h_02,
      cek_proof_v1.empty_environment_root_v1,
    ),
  )
}

test invalid_scrutinee_arm_still_refuses_a_constructor() {
  !invalid_scrutinee_step(
    cek_machine_v1.ConstrValue {
      tag: 0,
      values_count: 0,
      values_root: cek_proof_v1.empty_sequence_root_v1,
    },
    cek_proof_v1.hash_constr_value_v1(0, 0, cek_proof_v1.empty_sequence_root_v1),
  )
}

// Case on a constant (`ReturnCaseConstant`).

const integer_type = #"9f00ff"

const bytes_type = #"9f01ff"

const unit_type = #"9f03ff"

const boolean_type = #"9f04ff"

const integer_list_type = #"9f0500ff"

const integer_boolean_pair_type = #"9f060004ff"

const data_type = #"9f08ff"

const data_list_type = #"9f0508ff"

/// `count` branches, each `h_01`.
fn branches(count: Int) -> ByteArray {
  if count == 0 {
    cek_proof_v1.empty_sequence_root_v1
  } else {
    cek_proof_v1.hash_sequence_node_v1(h_01, branches(count - 1), count)
  }
}

fn case_frame(count: Int) -> ByteArray {
  cek_proof_v1.hash_case_continuation_v1(
    count,
    branches(count),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
}

/// The case-select state for branch `tag` of `count`, with its values.
fn selects(
  count: Int,
  tag: Int,
  values_count: Int,
  values_root: ByteArray,
) -> cek_machine_v1.MachineStateV1 {
  cek_machine_v1.MachineStateV1 {
    ..state(
      cek_machine_v1.mode_case_select,
      branches(count),
      cek_proof_v1.hash_case_select_continuation_v1(
        cek_proof_v1.empty_environment_root_v1,
        cek_proof_v1.empty_continuation_root_v1,
        values_count,
      ),
      tag,
    ),
    environment_root: values_root,
  }
}

fn selects_branch(count: Int, tag: Int) -> cek_machine_v1.MachineStateV1 {
  selects(count, tag, 0, cek_proof_v1.empty_sequence_root_v1)
}

/// The values of a list head and tail or of a pair's components: the first
/// is applied first.
fn values(first: ByteArray, second: ByteArray) -> ByteArray {
  cek_proof_v1.hash_sequence_node_v1(
    second,
    cek_proof_v1.hash_sequence_node_v1(
      first,
      cek_proof_v1.empty_sequence_root_v1,
      1,
    ),
    2,
  )
}

fn halts(reason: Int) -> cek_machine_v1.MachineStateV1 {
  state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_continuation_root_v1,
    reason,
  )
}

fn missing() -> cek_machine_v1.MachineStateV1 {
  halts(cek_machine_v1.error_case_branch_missing)
}

fn unsupported() -> cek_machine_v1.MachineStateV1 {
  halts(cek_machine_v1.error_invalid_case_scrutinee)
}

fn no_material() -> cek_builtin_v1.SemanticBuiltinWitnessV1 {
  cek_builtin_v1.SemanticBuiltinWitnessV1 {
    data_nodes: [],
    list_nodes: [],
    pair_nodes: [],
    scalar_preimages: [],
  }
}

fn case_step(
  scrutinee: cek_builtin_v1.ValueWitnessV1,
  focus_root: ByteArray,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
  frame_count: Int,
  count: Int,
  post: cek_machine_v1.MachineStateV1,
) -> Bool {
  cek_machine_v1.verify_core_step_v1(
    state(cek_machine_v1.mode_return, focus_root, case_frame(frame_count), 0),
    post,
    cek_machine_v1.ReturnCaseConstant {
      scrutinee,
      material,
      branches_count: count,
      branches_root: branches(count),
      captured_environment: cek_proof_v1.empty_environment_root_v1,
      tail: cek_proof_v1.empty_continuation_root_v1,
    },
  )
}

fn inline(
  type_cbor: ByteArray,
  payload: Data,
) -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 {
    type_cbor,
    payload_cbor: cbor.serialise(payload),
  }
}

/// The root the machine commits for an inline constant.
fn root(type_cbor: ByteArray, payload: Data) -> ByteArray {
  cek_constant_v1.constant_root_v1(inline(type_cbor, payload))
}

/// A `case` with `count` branches on an inline constant.
fn cases(
  type_cbor: ByteArray,
  payload: Data,
  count: Int,
  post: cek_machine_v1.MachineStateV1,
) -> Bool {
  case_step(
    cek_builtin_v1.ConstantValue(inline(type_cbor, payload)),
    root(type_cbor, payload),
    no_material(),
    count,
    count,
    post,
  )
}

fn unit() -> Data {
  builtin.constr_data(0, [])
}

fn boolean(value: Bool) -> Data {
  builtin.constr_data(
    if value {
      1
    } else {
      0
    },
    [],
  )
}

fn integers(items: List<Int>) -> Data {
  when items is {
    [] -> builtin.list_data([])
    [item, ..rest] ->
      builtin.list_data(
        [builtin.i_data(item), ..builtin.un_list_data(integers(rest))],
      )
  }
}

test case_on_unit_selects_its_only_branch() {
  cases(unit_type, unit(), 1, selects_branch(1, 0))
}

test case_on_unit_refuses_a_wrong_branch() {
  !cases(unit_type, unit(), 1, selects_branch(1, 1))
}

test case_on_unit_with_two_branches_has_no_branch() {
  cases(unit_type, unit(), 2, missing())
}

test case_on_unit_with_two_branches_refuses_a_selection() {
  !cases(unit_type, unit(), 2, selects_branch(2, 0))
}

test case_on_false_with_one_branch_selects_it() {
  cases(boolean_type, boolean(False), 1, selects_branch(1, 0))
}

test case_on_false_with_two_branches_selects_the_first() {
  cases(boolean_type, boolean(False), 2, selects_branch(2, 0))
}

test case_on_true_with_two_branches_selects_the_second() {
  cases(boolean_type, boolean(True), 2, selects_branch(2, 1))
}

test case_on_true_refuses_the_first_branch() {
  !cases(boolean_type, boolean(True), 2, selects_branch(2, 0))
}

test case_on_false_refuses_the_second_branch() {
  !cases(boolean_type, boolean(False), 2, selects_branch(2, 1))
}

test case_on_true_with_one_branch_has_no_branch() {
  cases(boolean_type, boolean(True), 1, missing())
}

test case_on_true_with_one_branch_refuses_a_selection() {
  !cases(boolean_type, boolean(True), 1, selects_branch(1, 0))
}

test case_on_false_with_three_branches_has_no_branch() {
  cases(boolean_type, boolean(False), 3, missing())
}

test case_on_an_integer_selects_its_index() {
  cases(integer_type, builtin.i_data(2), 3, selects_branch(3, 2))
}

test case_on_an_integer_refuses_another_index() {
  !cases(integer_type, builtin.i_data(2), 3, selects_branch(3, 1))
}

test case_on_an_integer_in_range_refuses_a_missing_branch() {
  !cases(integer_type, builtin.i_data(2), 3, missing())
}

test case_on_an_integer_past_the_last_branch_has_no_branch() {
  cases(integer_type, builtin.i_data(3), 3, missing())
}

test case_on_a_negative_integer_has_no_branch() {
  cases(integer_type, builtin.i_data(-1), 3, missing())
}

test case_on_an_integer_without_branches_has_no_branch() {
  cases(integer_type, builtin.i_data(0), 0, missing())
}

test case_on_the_widest_one_word_integer_has_no_branch() {
  cases(integer_type, builtin.i_data(18446744073709551615), 3, missing())
}

test case_on_an_empty_list_selects_the_second_branch() {
  cases(integer_list_type, integers([]), 2, selects_branch(2, 1))
}

test case_on_an_empty_list_refuses_the_first_branch() {
  !cases(integer_list_type, integers([]), 2, selects_branch(2, 0))
}

test case_on_an_empty_list_with_one_branch_has_no_branch() {
  cases(integer_list_type, integers([]), 1, missing())
}

fn head_and_tail() -> ByteArray {
  values(
    root(integer_type, builtin.i_data(1)),
    root(integer_list_type, integers([2])),
  )
}

test case_on_a_list_applies_the_first_branch_to_head_and_tail() {
  cases(
    integer_list_type,
    integers([1, 2]),
    2,
    selects(2, 0, 2, head_and_tail()),
  )
}

test case_on_a_list_with_one_branch_applies_it_to_head_and_tail() {
  cases(
    integer_list_type,
    integers([1, 2]),
    1,
    selects(1, 0, 2, head_and_tail()),
  )
}

test case_on_a_list_refuses_tail_before_head() {
  !cases(
    integer_list_type,
    integers([1, 2]),
    2,
    selects(
      2,
      0,
      2,
      values(
        root(integer_list_type, integers([2])),
        root(integer_type, builtin.i_data(1)),
      ),
    ),
  )
}

test case_on_a_list_refuses_the_whole_list_as_its_tail() {
  !cases(
    integer_list_type,
    integers([1, 2]),
    2,
    selects(
      2,
      0,
      2,
      values(
        root(integer_type, builtin.i_data(1)),
        root(integer_list_type, integers([1, 2])),
      ),
    ),
  )
}

test case_on_a_list_refuses_the_empty_list_branch() {
  !cases(integer_list_type, integers([1, 2]), 2, selects_branch(2, 1))
}

test case_on_a_list_with_three_branches_has_no_branch() {
  cases(integer_list_type, integers([1, 2]), 3, missing())
}

fn pair() -> Data {
  builtin.constr_data(0, [builtin.i_data(5), boolean(True)])
}

test case_on_a_pair_applies_its_branch_to_both_components() {
  cases(
    integer_boolean_pair_type,
    pair(),
    1,
    selects(
      1,
      0,
      2,
      values(
        root(integer_type, builtin.i_data(5)),
        root(boolean_type, boolean(True)),
      ),
    ),
  )
}

test case_on_a_pair_refuses_the_second_component_first() {
  !cases(
    integer_boolean_pair_type,
    pair(),
    1,
    selects(
      1,
      0,
      2,
      values(
        root(boolean_type, boolean(True)),
        root(integer_type, builtin.i_data(5)),
      ),
    ),
  )
}

test case_on_a_pair_refuses_no_values() {
  !cases(integer_boolean_pair_type, pair(), 1, selects_branch(1, 0))
}

test case_on_a_pair_with_two_branches_has_no_branch() {
  cases(integer_boolean_pair_type, pair(), 2, missing())
}

test case_on_a_bytestring_is_an_invalid_scrutinee() {
  cases(bytes_type, builtin.b_data(#"00"), 1, unsupported())
}

test case_on_a_bytestring_refuses_a_missing_branch() {
  !cases(bytes_type, builtin.b_data(#"00"), 1, missing())
}

test case_on_a_bytestring_refuses_a_selection() {
  !cases(bytes_type, builtin.b_data(#"00"), 1, selects_branch(1, 0))
}

test case_on_a_string_is_an_invalid_scrutinee() {
  cases(#"9f02ff", builtin.b_data(#"61"), 1, unsupported())
}

test case_on_data_is_an_invalid_scrutinee() {
  cases(data_type, builtin.i_data(0), 2, unsupported())
}

test case_on_an_integer_refuses_an_invalid_scrutinee() {
  !cases(integer_type, builtin.i_data(5), 3, unsupported())
}

test case_on_a_constant_refuses_another_frame() {
  !case_step(
    cek_builtin_v1.ConstantValue(inline(unit_type, unit())),
    root(unit_type, unit()),
    no_material(),
    2,
    1,
    selects_branch(1, 0),
  )
}

test case_on_a_constant_refuses_another_scrutinee() {
  !case_step(
    cek_builtin_v1.ConstantValue(inline(integer_type, builtin.i_data(0))),
    root(integer_type, builtin.i_data(1)),
    no_material(),
    2,
    2,
    selects_branch(2, 0),
  )
}

// Semantic scrutinees: values too wide to reveal are read through their nodes.

fn semantic(
  type_cbor: ByteArray,
  payload: cek_data_v1.DataSummaryV1,
  memory: Int,
) -> Pair<cek_builtin_v1.ValueWitnessV1, ByteArray> {
  Pair(
    cek_builtin_v1.SemanticConstantValue { type_cbor, payload, memory },
    cek_constant_v1.semantic_constant_root_v1(type_cbor, payload, memory),
  )
}

fn semantic_cases(
  scrutinee: Pair<cek_builtin_v1.ValueWitnessV1, ByteArray>,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
  count: Int,
  post: cek_machine_v1.MachineStateV1,
) -> Bool {
  case_step(scrutinee.1st, scrutinee.2nd, material, count, count, post)
}

fn wide_integer(memory: Int) -> Pair<cek_builtin_v1.ValueWitnessV1, ByteArray> {
  semantic(
    integer_type,
    cek_data_v1.DataSummaryV1 {
      root: h_02,
      cbor_length: 9300,
      memory: 4 + memory,
    },
    memory,
  )
}

test case_on_a_committed_wide_integer_has_no_branch() {
  semantic_cases(wide_integer(2), no_material(), 3, missing())
}

test case_on_a_committed_wide_integer_refuses_a_selection() {
  !semantic_cases(wide_integer(2), no_material(), 3, selects_branch(3, 0))
}

test case_on_a_committed_one_word_integer_is_not_opened() fail {
  semantic_cases(wide_integer(1), no_material(), 3, missing())
}

/// 9000 bytes, built in two halves: one `replicateByte` is capped at 8192.
fn wide_bytes() -> ByteArray {
  builtin.append_bytearray(
    builtin.replicate_byte(4500, 42),
    builtin.replicate_byte(4500, 42),
  )
}

type WideList {
  scrutinee: Pair<cek_builtin_v1.ValueWitnessV1, ByteArray>,
  head: ByteArray,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
}

/// `[B <9000 bytes>]` as a list of data, with its head node.
fn wide_list() -> WideList {
  let bytes = wide_bytes()
  let blob = cek_data_scan_v1.bounded_blob_root_v1(bytes)
  let head = cek_data_v1.bytes_data_summary_v1(9000, blob)
  let head_node = cek_data_v1.BytesData {
    bytes_root: blob,
    bytes_length: 9000,
    cbor_length: head.cbor_length,
    memory: head.memory,
  }
  let link = cek_data_v1.DataListNodeV1 {
    head: head.root,
    head_cbor_length: head.cbor_length,
    head_memory: head.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: head.cbor_length,
    memory: head.memory,
  }
  let items = cek_data_v1.prepend_data_list_summary_v1(
    head,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  let payload = cek_data_v1.list_data_summary_v1(items)
  WideList {
    scrutinee: semantic(data_list_type, payload, items.memory),
    head: semantic(data_type, head, head.memory).2nd,
    material: cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [
        cek_data_v1.ListData {
          items_count: 1,
          items_root: items.root,
          cbor_length: payload.cbor_length,
          memory: payload.memory,
        },
        head_node,
      ],
      list_nodes: [link],
      pair_nodes: [],
      scalar_preimages: [],
    },
  }
}

test case_on_a_wide_list_applies_its_branch_to_head_and_tail() {
  let list = wide_list()
  semantic_cases(
    list.scrutinee,
    list.material,
    2,
    selects(
      2,
      0,
      2,
      values(list.head, root(data_list_type, builtin.list_data([]))),
    ),
  )
}

test case_on_a_wide_list_refuses_tail_before_head() {
  let list = wide_list()
  !semantic_cases(
    list.scrutinee,
    list.material,
    2,
    selects(
      2,
      0,
      2,
      values(root(data_list_type, builtin.list_data([])), list.head),
    ),
  )
}

test case_on_a_wide_list_refuses_the_empty_list_branch() {
  let list = wide_list()
  !semantic_cases(list.scrutinee, list.material, 2, selects_branch(2, 1))
}

test case_on_a_wide_list_with_three_branches_has_no_branch() {
  semantic_cases(wide_list().scrutinee, no_material(), 3, missing())
}

test case_on_a_wide_list_without_its_nodes_is_not_opened() fail {
  let list = wide_list()
  semantic_cases(
    list.scrutinee,
    no_material(),
    2,
    selects(
      2,
      0,
      2,
      values(list.head, root(data_list_type, builtin.list_data([]))),
    ),
  )
}

// Committed lists of scalars: whatever its length, a list in its root-equal
// committed form is read through its top node, its head node and its first
// two links, so the step's cost does not grow with the list.

fn summary_link(
  head: cek_data_v1.DataSummaryV1,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataListNodeV1 {
  cek_data_v1.DataListNodeV1 {
    head: head.root,
    head_cbor_length: head.cbor_length,
    head_memory: head.memory,
    tail: tail.root,
    length: tail.length + 1,
    payload_cbor_length: head.cbor_length + tail.payload_cbor_length,
    memory: head.memory + tail.memory,
  }
}

/// A `case` with two branches on `items` (at least two) in committed form,
/// against the head and tail roots of the same list carried inline.
fn committed_case(
  type_cbor: ByteArray,
  element_cbor: ByteArray,
  items: List<Data>,
  head_node: cek_data_v1.DataNodeV1,
) -> Bool {
  expect [head, second, ..rest] = items
  let rest_items = cek_data_v1.commit_data_list(rest)
  let second_summary = cek_data_v1.semantic_data_summary_v1(second)
  let tail_items = cek_data_v1.prepend_data_list_summary_v1(
    second_summary,
    rest_items,
  )
  let head_summary = cek_data_v1.semantic_data_summary_v1(head)
  let all_items = cek_data_v1.prepend_data_list_summary_v1(
    head_summary,
    tail_items,
  )
  let payload = cek_data_v1.list_data_summary_v1(all_items)
  let list = builtin.list_data(items)
  let scrutinee = semantic(
    type_cbor,
    payload,
    cek_constant_v1.semantic_memory_size_v1(
      cek_constant_v1.decode_constant_type_v1(type_cbor),
      list,
    ),
  )
  expect scrutinee.2nd == root(type_cbor, list)
  semantic_cases(
    scrutinee,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [
        cek_data_v1.ListData {
          items_count: all_items.length,
          items_root: all_items.root,
          cbor_length: payload.cbor_length,
          memory: payload.memory,
        },
        head_node,
      ],
      list_nodes: [
        summary_link(head_summary, tail_items),
        summary_link(second_summary, rest_items),
      ],
      pair_nodes: [],
      scalar_preimages: [],
    },
    2,
    selects(
      2,
      0,
      2,
      values(
        root(element_cbor, head),
        root(type_cbor, builtin.list_data([second, ..rest])),
      ),
    ),
  )
}

fn bytes_node(bytes: ByteArray) -> cek_data_v1.DataNodeV1 {
  let length = builtin.length_of_bytearray(bytes)
  let blob = cek_data_v1.bounded_blob_root_v1(bytes)
  let summary = cek_data_v1.bytes_data_summary_v1(length, blob)
  cek_data_v1.BytesData {
    bytes_root: blob,
    bytes_length: length,
    cbor_length: summary.cbor_length,
    memory: summary.memory,
  }
}

/// The node of a constructor without fields: a unit or a boolean.
fn tag_node(constructor: Int) -> cek_data_v1.DataNodeV1 {
  cek_data_v1.ConstrSmallData {
    constructor,
    fields_count: 0,
    fields_root: cek_data_v1.empty_data_list_root_v1(),
    cbor_length: 3,
    memory: 4,
  }
}

// One, two and three words each, so the element memory is not the item count.
test case_on_a_committed_bytestring_list_applies_its_branch_to_head_and_tail() {
  committed_case(
    #"9f0501ff",
    bytes_type,
    [
      builtin.b_data(#"00"),
      builtin.b_data(builtin.replicate_byte(9, 1)),
      builtin.b_data(builtin.replicate_byte(17, 2)),
    ],
    bytes_node(#"00"),
  )
}

test case_on_a_committed_boolean_list_applies_its_branch_to_head_and_tail() {
  committed_case(
    #"9f0504ff",
    boolean_type,
    [boolean(True), boolean(False), boolean(True)],
    tag_node(1),
  )
}

test case_on_a_committed_unit_list_applies_its_branch_to_head_and_tail() {
  committed_case(#"9f0503ff", unit_type, [unit(), unit()], tag_node(0))
}

type WidePair {
  scrutinee: Pair<cek_builtin_v1.ValueWitnessV1, ByteArray>,
  first: ByteArray,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
}

/// `(B <9000 bytes>, I 0)` as a pair of data, with both component nodes.
fn wide_pair() -> WidePair {
  let bytes = wide_bytes()
  let blob = cek_data_scan_v1.bounded_blob_root_v1(bytes)
  let first = cek_data_v1.bytes_data_summary_v1(9000, blob)
  let first_node = cek_data_v1.BytesData {
    bytes_root: blob,
    bytes_length: 9000,
    cbor_length: first.cbor_length,
    memory: first.memory,
  }
  let zero_cbor = cbor.serialise(builtin.i_data(0))
  let zero_root = cek_data_scan_v1.bounded_blob_root_v1(zero_cbor)
  let second = cek_data_v1.integer_data_summary_v1(0, zero_root)
  let second_node = cek_data_v1.IntegerData {
    cbor_root: zero_root,
    cbor_length: second.cbor_length,
    memory: second.memory,
  }
  let second_link = cek_data_v1.DataListNodeV1 {
    head: second.root,
    head_cbor_length: second.cbor_length,
    head_memory: second.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: second.cbor_length,
    memory: second.memory,
  }
  let first_link = cek_data_v1.DataListNodeV1 {
    head: first.root,
    head_cbor_length: first.cbor_length,
    head_memory: first.memory,
    tail: cek_data_v1.hash_data_list_node_v1(second_link),
    length: 2,
    payload_cbor_length: first.cbor_length + second.cbor_length,
    memory: first.memory + second.memory,
  }
  let payload = cek_data_v1.small_constr_data_summary_v1(
    0,
    cek_data_v1.prepend_data_list_summary_v1(
      first,
      cek_data_v1.prepend_data_list_summary_v1(
        second,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    ),
  )
  WidePair {
    scrutinee: semantic(#"9f060808ff", payload, first.memory + second.memory),
    first: semantic(data_type, first, first.memory).2nd,
    material: cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [
        cek_data_v1.ConstrSmallData {
          constructor: 0,
          fields_count: 2,
          fields_root: cek_data_v1.hash_data_list_node_v1(first_link),
          cbor_length: payload.cbor_length,
          memory: payload.memory,
        },
        first_node,
        second_node,
      ],
      list_nodes: [first_link, second_link],
      pair_nodes: [],
      scalar_preimages: [],
    },
  }
}

test case_on_a_wide_pair_applies_its_branch_to_both_components() {
  let pair = wide_pair()
  semantic_cases(
    pair.scrutinee,
    pair.material,
    1,
    selects(1, 0, 2, values(pair.first, root(data_type, builtin.i_data(0)))),
  )
}

test case_on_a_wide_pair_refuses_the_second_component_first() {
  let pair = wide_pair()
  !semantic_cases(
    pair.scrutinee,
    pair.material,
    1,
    selects(1, 0, 2, values(root(data_type, builtin.i_data(0)), pair.first)),
  )
}

test case_on_a_wide_pair_with_two_branches_has_no_branch() {
  semantic_cases(wide_pair().scrutinee, no_material(), 2, missing())
}
