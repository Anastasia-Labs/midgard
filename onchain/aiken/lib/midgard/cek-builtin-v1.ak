use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_constant_v1
use midgard/cek_cost_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_proof_v1

/// A direct builtin witness reveals only canonical CEK constants. Opaque
/// roots are admitted solely for polymorphic branch values which the builtin
/// selects without inspecting.
pub type ValueWitnessV1 {
  ConstantValue(cek_constant_v1.ConstantWitnessV1)
  SemanticConstantValue {
    type_cbor: ByteArray,
    payload: cek_data_v1.DataSummaryV1,
    memory: Int,
  }
  OpaqueValue(ByteArray)
  BlsMillerLoopValue(ByteArray)
}

/// A failure witness must be able to reveal every CEK runtime-value shape.
/// Untyped UPLC can apply a builtin to a closure, delay, constructor, or
/// another builtin; those type errors happen before the builtin budget is
/// charged.  Keeping this distinct from `ValueWitnessV1` prevents opaque
/// values from being accepted where successful builtin semantics inspect
/// their contents.
pub type RuntimeValueWitnessV1 {
  RuntimeConstantValue(cek_constant_v1.ConstantWitnessV1)
  RuntimeSemanticConstantValue {
    type_cbor: ByteArray,
    payload: cek_data_v1.DataSummaryV1,
    memory: Int,
  }
  RuntimeLambdaValue { body: ByteArray, environment: ByteArray }
  RuntimeDelayValue { body: ByteArray, environment: ByteArray }
  RuntimeConstrValue { tag: Int, values_count: Int, values_root: ByteArray }
  RuntimeBuiltinValue {
    tag: Int,
    forces_remaining: Int,
    arguments_count: Int,
    arguments_root: ByteArray,
  }
  RuntimeBlsMillerLoopValue { expression_root: ByteArray }
}

pub type BlsExpressionWitnessV1 {
  BlsMillerLoopExpression {
    g1: cek_constant_v1.ConstantWitnessV1,
    g2: cek_constant_v1.ConstantWitnessV1,
  }
  BlsMultiplyExpression {
    left: BlsExpressionWitnessV1,
    right: BlsExpressionWitnessV1,
  }
}

/// Local semantic material for a structured builtin transition. Each tag
/// requires an exact list shape; superfluous or missing nodes fail closed.
/// Large scalar leaves are revealed independently and remain bounded by the
/// field-preimage envelope rather than by the size of the whole constant.
pub type SemanticBuiltinWitnessV1 {
  data_nodes: List<cek_data_v1.DataNodeV1>,
  list_nodes: List<cek_data_v1.DataListNodeV1>,
  pair_nodes: List<cek_data_v1.DataPairNodeV1>,
  scalar_preimages: List<ByteArray>,
}

pub const max_direct_bls_miller_loop_leaves = 10

type ArgumentAccumulatorV1 {
  root: ByteArray,
  count: Int,
}

fn runtime_value_root_v1(value: RuntimeValueWitnessV1) -> ByteArray {
  when value is {
    RuntimeConstantValue(witness) -> cek_constant_v1.constant_root_v1(witness)
    RuntimeSemanticConstantValue { type_cbor, payload, memory } -> {
      expect cek_constant_v1.constant_type_is_known_v1(type_cbor)
      cek_constant_v1.semantic_constant_root_v1(type_cbor, payload, memory)
    }
    RuntimeLambdaValue { body, environment } ->
      cek_proof_v1.hash_lambda_value_v1(body, environment)
    RuntimeDelayValue { body, environment } ->
      cek_proof_v1.hash_delay_value_v1(body, environment)
    RuntimeConstrValue { tag, values_count, values_root } ->
      cek_proof_v1.hash_constr_value_v1(tag, values_count, values_root)
    RuntimeBuiltinValue {
      tag,
      forces_remaining,
      arguments_count,
      arguments_root,
    } ->
      cek_proof_v1.hash_builtin_value_v1(
        tag,
        forces_remaining,
        arguments_count,
        arguments_root,
      )
    RuntimeBlsMillerLoopValue { expression_root } ->
      cek_proof_v1.hash_bls_miller_loop_value_v1(expression_root)
  }
}

fn value_root_v1(value: ValueWitnessV1) -> ByteArray {
  when value is {
    ConstantValue(witness) -> cek_constant_v1.constant_root_v1(witness)
    SemanticConstantValue { type_cbor, payload, memory } -> {
      expect cek_constant_v1.constant_type_is_known_v1(type_cbor)
      cek_constant_v1.semantic_constant_root_v1(type_cbor, payload, memory)
    }
    OpaqueValue(root) -> {
      expect bytearray.length(root) == 32
      root
    }
    BlsMillerLoopValue(expression_root) ->
      cek_proof_v1.hash_bls_miller_loop_value_v1(expression_root)
  }
}

fn accumulate_arguments_v1(
  arguments: List<ValueWitnessV1>,
  accumulator: ArgumentAccumulatorV1,
) -> ArgumentAccumulatorV1 {
  when arguments is {
    [] -> accumulator
    [argument, ..rest] -> {
      let next_count = accumulator.count + 1
      accumulate_arguments_v1(
        rest,
        ArgumentAccumulatorV1 {
          root: cek_proof_v1.hash_sequence_node_v1(
            value_root_v1(argument),
            accumulator.root,
            next_count,
          ),
          count: next_count,
        },
      )
    }
  }
}

pub fn arguments_root_v1(
  arguments: List<ValueWitnessV1>,
) -> Pair<ByteArray, Int> {
  let result = accumulate_arguments_v1(
    arguments,
    ArgumentAccumulatorV1 {
      root: cek_proof_v1.empty_sequence_root_v1,
      count: 0,
    },
  )
  Pair(result.root, result.count)
}

fn accumulate_runtime_arguments_v1(
  arguments: List<RuntimeValueWitnessV1>,
  accumulator: ArgumentAccumulatorV1,
) -> ArgumentAccumulatorV1 {
  when arguments is {
    [] -> accumulator
    [argument, ..rest] -> {
      let next_count = accumulator.count + 1
      accumulate_runtime_arguments_v1(
        rest,
        ArgumentAccumulatorV1 {
          root: cek_proof_v1.hash_sequence_node_v1(
            runtime_value_root_v1(argument),
            accumulator.root,
            next_count,
          ),
          count: next_count,
        },
      )
    }
  }
}

pub fn runtime_arguments_root_v1(
  arguments: List<RuntimeValueWitnessV1>,
) -> Pair<ByteArray, Int> {
  let result = accumulate_runtime_arguments_v1(
    arguments,
    ArgumentAccumulatorV1 {
      root: cek_proof_v1.empty_sequence_root_v1,
      count: 0,
    },
  )
  Pair(result.root, result.count)
}

pub fn value_memory_size_v1(value: ValueWitnessV1) -> Int {
  when value is {
    ConstantValue(witness) -> cek_constant_v1.constant_memory_size_v1(witness)
    SemanticConstantValue { type_cbor, payload, memory } -> {
      expect cek_constant_v1.constant_type_is_known_v1(type_cbor)
      expect bytearray.length(payload.root) == 32
      expect payload.cbor_length >= 0
      expect payload.memory >= 0
      expect memory >= 0
      memory
    }
    // Polymorphic control branches are not inspected by the CEK cost model.
    OpaqueValue(root) -> {
      expect bytearray.length(root) == 32
      1
    }
    BlsMillerLoopValue(expression_root) -> {
      expect bytearray.length(expression_root) == 32
      192
    }
  }
}

fn standard_cost_sizes_v1(arguments: List<ValueWitnessV1>) -> List<Int> {
  list.map(arguments, value_memory_size_v1)
}

fn bitwise_cost_sizes_v1(arguments: List<ValueWitnessV1>) -> List<Int> {
  expect [extend, left, right] = arguments
  let left_size = value_memory_size_v1(left)
  let right_size = value_memory_size_v1(right)
  let normalized = if boolean_v1(extend) {
    if left_size > right_size {
      left_size
    } else {
      right_size
    }
  } else if left_size < right_size {
    left_size
  } else {
    right_size
  }
  [1, normalized, normalized]
}

pub fn builtin_cost_sizes_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
) -> List<Int> {
  if tag == 26 {
    expect [condition, _, _] = arguments
    let condition_value = boolean_v1(condition)
    expect or {
        condition_value,
        !condition_value,
      }
    [1, 1, 1]
  } else if tag == 27 {
    expect [unit, _] = arguments
    let Void = unit_v1(unit)
    [1, 1]
  } else if tag == 28 {
    expect [message, _] = arguments
    // The reference CEK charges an empty trace message as zero rather than
    // applying the empty-byte-string minimum used by ordinary constants.
    [bytearray.length(string_bytes_v1(message)), 1]
  } else if tag == 31 {
    expect [source, _, _] = arguments
    [value_memory_size_v1(source), 1, 1]
  } else if tag == 36 {
    expect [source, _, _, _, _, _] = arguments
    [value_memory_size_v1(source), 1, 1, 1, 1, 1]
  } else if and {
    tag >= 75,
    tag <= 77,
  } {
    bitwise_cost_sizes_v1(arguments)
  } else {
    standard_cost_sizes_v1(arguments)
  }
}

pub fn direct_builtin_budget_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
) -> cek_cost_v1.BuiltinBudgetV1 {
  cek_cost_v1.builtin_budget_v1(tag, builtin_cost_sizes_v1(tag, arguments))
}

fn constant_witness_v1(
  value: ValueWitnessV1,
) -> cek_constant_v1.ConstantWitnessV1 {
  expect ConstantValue(witness) = value
  witness
}

fn constant_type_v1(value: ValueWitnessV1) -> cek_constant_v1.ConstantTypeV1 {
  when value is {
    ConstantValue(witness) -> cek_constant_v1.constant_type_v1(witness)
    SemanticConstantValue { type_cbor, .. } ->
      cek_constant_v1.decode_constant_type_v1(type_cbor)
    OpaqueValue(_) -> fail @"expected constant value"
    BlsMillerLoopValue(_) -> fail @"expected constant value"
  }
}

fn constant_payload_v1(value: ValueWitnessV1) -> Data {
  value
    |> constant_witness_v1
    |> cek_constant_v1.constant_payload_v1
}

fn result_is_constant_v1(
  result: ValueWitnessV1,
  expected_type: cek_constant_v1.ConstantTypeV1,
  expected_payload: Data,
) -> Bool {
  when result is {
    ConstantValue(witness) -> and {
        cek_constant_v1.constant_type_v1(witness) == expected_type,
        cek_constant_v1.constant_payload_v1(witness) == expected_payload,
      }
    SemanticConstantValue { type_cbor, payload, memory } -> {
      let expected_summary = cek_data_v1.semantic_data_summary_v1(
        expected_payload,
      )
      and {
        cek_constant_v1.decode_constant_type_v1(type_cbor) == expected_type,
        payload == expected_summary,
        memory == cek_constant_v1.constant_payload_memory_size_v1(
          expected_type,
          expected_payload,
        ),
      }
    }
    OpaqueValue(_) -> False
    BlsMillerLoopValue(_) -> False
  }
}

fn result_is_selected_v1(
  result: ValueWitnessV1,
  selected: ValueWitnessV1,
) -> Bool {
  value_root_v1(result) == value_root_v1(selected)
}

type SemanticConstantPartsV1 {
  constant_type: cek_constant_v1.ConstantTypeV1,
  payload: cek_data_v1.DataSummaryV1,
  memory: Int,
}

fn semantic_constant_parts_v1(value: ValueWitnessV1) -> SemanticConstantPartsV1 {
  when value is {
    ConstantValue(witness) -> {
      let payload = cek_constant_v1.constant_payload_v1(witness)
      SemanticConstantPartsV1 {
        constant_type: cek_constant_v1.constant_type_v1(witness),
        payload: cek_data_v1.semantic_data_summary_v1(payload),
        memory: cek_constant_v1.constant_memory_size_v1(witness),
      }
    }
    SemanticConstantValue { type_cbor, payload, memory } -> {
      expect bytearray.length(payload.root) == 32
      expect payload.cbor_length >= 0
      expect payload.memory >= 0
      expect memory >= 0
      SemanticConstantPartsV1 {
        constant_type: cek_constant_v1.decode_constant_type_v1(type_cbor),
        payload,
        memory,
      }
    }
    OpaqueValue(_) -> fail @"expected semantic constant value"
    BlsMillerLoopValue(_) -> fail @"expected semantic constant value"
  }
}

pub fn semantic_constant_type_v1(
  value: ValueWitnessV1,
) -> cek_constant_v1.ConstantTypeV1 {
  semantic_constant_parts_v1(value).constant_type
}

pub fn semantic_constant_payload_v1(
  value: ValueWitnessV1,
) -> cek_data_v1.DataSummaryV1 {
  semantic_constant_parts_v1(value).payload
}

pub fn semantic_constant_memory_v1(value: ValueWitnessV1) -> Int {
  semantic_constant_parts_v1(value).memory
}

fn summary_from_node_v1(
  node: cek_data_v1.DataNodeV1,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 {
    root: cek_data_v1.hash_data_node_v1(node),
    cbor_length: cek_data_v1.data_node_cbor_length_v1(node),
    memory: cek_data_v1.data_node_memory_v1(node),
  }
}

fn top_node_matches_v1(
  summary: cek_data_v1.DataSummaryV1,
  node: cek_data_v1.DataNodeV1,
  list_summary: Option<cek_data_v1.DataListNodeV1>,
  pair_summary: Option<cek_data_v1.DataPairNodeV1>,
) -> Bool {
  and {
    summary == summary_from_node_v1(node),
    cek_data_v1.verify_data_node_v1(node, list_summary, pair_summary),
  }
}

fn result_is_semantic_constant_v1(
  result: ValueWitnessV1,
  expected_type: cek_constant_v1.ConstantTypeV1,
  expected_payload: cek_data_v1.DataSummaryV1,
  expected_memory: Int,
) -> Bool {
  let actual = semantic_constant_parts_v1(result)
  and {
    actual.constant_type == expected_type,
    actual.payload == expected_payload,
    actual.memory == expected_memory,
  }
}

fn data_constant_summary_v1(value: ValueWitnessV1) -> cek_data_v1.DataSummaryV1 {
  let parts = semantic_constant_parts_v1(value)
  expect parts.constant_type == cek_constant_v1.DataConstant
  expect parts.memory == parts.payload.memory
  parts.payload
}

fn data_sequence_from_list_node_v1(
  node: cek_data_v1.DataNodeV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect
      cek_data_v1.ListData { items_count, items_root, cbor_length: _, memory }
    = node
  cek_data_v1.DataSequenceSummaryV1 {
    root: items_root,
    length: items_count,
    payload_cbor_length: cek_data_v1.data_node_cbor_length_v1(node) - if items_count == 0 {
      1
    } else {
      2
    },
    memory: memory - 4,
  }
}

fn list_constant_sequence_v1(
  value: ValueWitnessV1,
  expected_element: cek_constant_v1.ConstantTypeV1,
  node: cek_data_v1.DataNodeV1,
  first: Option<cek_data_v1.DataListNodeV1>,
) -> cek_data_v1.DataSequenceSummaryV1 {
  let parts = semantic_constant_parts_v1(value)
  expect
    parts.constant_type == cek_constant_v1.ListConstant {
      element: expected_element,
    }
  expect top_node_matches_v1(parts.payload, node, first, None)
  let sequence = data_sequence_from_list_node_v1(node)
  expect parts.memory == sequence.memory
  sequence
}

fn canonical_integer_leaf_v1(
  summary: cek_data_v1.DataSummaryV1,
  node: cek_data_v1.DataNodeV1,
  raw_cbor: ByteArray,
) -> Int {
  expect cek_data_v1.IntegerData { cbor_root, cbor_length, memory: _ } = node
  expect bytearray.length(raw_cbor) > 0
  expect
    bytearray.length(raw_cbor) <= cek_data_scan_v1.max_revealed_data_leaf_bytes
  expect cbor_root == cek_data_scan_v1.bounded_blob_root_v1(raw_cbor)
  expect cbor_length == bytearray.length(raw_cbor)
  expect Some(data) = cbor.deserialise(raw_cbor)
  expect cbor.serialise(data) == raw_cbor
  expect builtin.choose_data(data, False, False, False, True, False)
  expect top_node_matches_v1(summary, node, None, None)
  builtin.un_i_data(data)
}

fn canonical_bytes_leaf_v1(
  summary: cek_data_v1.DataSummaryV1,
  node: cek_data_v1.DataNodeV1,
  bytes: ByteArray,
) -> ByteArray {
  expect
      cek_data_v1.BytesData {
        bytes_root,
        bytes_length,
        cbor_length: _,
        memory: _,
      }
    = node
  expect
    bytearray.length(bytes) <= cek_data_scan_v1.max_revealed_data_leaf_bytes
  expect bytes_root == cek_data_scan_v1.bounded_blob_root_v1(bytes)
  expect bytes_length == bytearray.length(bytes)
  expect top_node_matches_v1(summary, node, None, None)
  bytes
}

fn exact_list_summary_for_node_v1(
  node: cek_data_v1.DataNodeV1,
  candidates: List<cek_data_v1.DataListNodeV1>,
) -> Option<cek_data_v1.DataListNodeV1> {
  let count = when node is {
    cek_data_v1.ConstrSmallData { fields_count, .. } -> fields_count
    cek_data_v1.ConstrLargeData { fields_count, .. } -> fields_count
    cek_data_v1.ListData { items_count, .. } -> items_count
    _ -> 0
  }
  if count == 0 {
    expect candidates == []
    None
  } else {
    expect [summary] = candidates
    Some(summary)
  }
}

fn exact_pair_summary_for_node_v1(
  node: cek_data_v1.DataNodeV1,
  candidates: List<cek_data_v1.DataPairNodeV1>,
) -> Option<cek_data_v1.DataPairNodeV1> {
  let count = when node is {
    cek_data_v1.MapData { entries_count, .. } -> entries_count
    _ -> 0
  }
  if count == 0 {
    expect candidates == []
    None
  } else {
    expect [summary] = candidates
    Some(summary)
  }
}

fn verify_semantic_choose_data_v1(
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [source,
        when_constr, when_map, when_list, when_integer, when_bytes] = arguments
  expect [node] = witness.data_nodes
  expect witness.scalar_preimages == []
  let list_summary = exact_list_summary_for_node_v1(node, witness.list_nodes)
  let pair_summary = exact_pair_summary_for_node_v1(node, witness.pair_nodes)
  expect
    top_node_matches_v1(
      data_constant_summary_v1(source),
      node,
      list_summary,
      pair_summary,
    )
  let selected = when node is {
    cek_data_v1.ConstrSmallData { .. } -> when_constr
    cek_data_v1.ConstrLargeData { .. } -> when_constr
    cek_data_v1.MapData { .. } -> when_map
    cek_data_v1.ListData { .. } -> when_list
    cek_data_v1.IntegerData { .. } -> when_integer
    cek_data_v1.BytesData { .. } -> when_bytes
  }
  result_is_selected_v1(result, selected)
}

fn bool_data(value: Bool) -> Data {
  builtin.constr_data(
    if value {
      1
    } else {
      0
    },
    [],
  )
}

fn unit_data() -> Data {
  builtin.constr_data(0, [])
}

fn pair_data(first: Data, second: Data) -> Data {
  builtin.constr_data(0, [first, second])
}

fn data_is_constr(data: Data) -> Bool {
  builtin.choose_data(data, True, False, False, False, False)
}

fn data_is_map(data: Data) -> Bool {
  builtin.choose_data(data, False, True, False, False, False)
}

fn data_is_list(data: Data) -> Bool {
  builtin.choose_data(data, False, False, True, False, False)
}

fn data_is_integer(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, True, False)
}

fn data_is_bytes(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, False, True)
}

fn integer_v1(value: ValueWitnessV1) -> Int {
  expect constant_type_v1(value) == cek_constant_v1.IntegerConstant
  let payload = constant_payload_v1(value)
  expect data_is_integer(payload)
  builtin.un_i_data(payload)
}

fn bytes_v1(value: ValueWitnessV1) -> ByteArray {
  expect constant_type_v1(value) == cek_constant_v1.ByteStringConstant
  let payload = constant_payload_v1(value)
  expect data_is_bytes(payload)
  builtin.un_b_data(payload)
}

fn string_bytes_v1(value: ValueWitnessV1) -> ByteArray {
  expect constant_type_v1(value) == cek_constant_v1.StringConstant
  let payload = constant_payload_v1(value)
  expect data_is_bytes(payload)
  builtin.un_b_data(payload)
}

fn boolean_v1(value: ValueWitnessV1) -> Bool {
  expect constant_type_v1(value) == cek_constant_v1.BooleanConstant
  let payload = constant_payload_v1(value)
  expect data_is_constr(payload)
  let Pair(tag, fields) = builtin.un_constr_data(payload)
  expect list.is_empty(fields)
  expect or {
      tag == 0,
      tag == 1,
    }
  tag == 1
}

fn unit_v1(value: ValueWitnessV1) -> Void {
  expect constant_type_v1(value) == cek_constant_v1.UnitConstant
  expect constant_payload_v1(value) == unit_data()
  Void
}

fn list_payload_v1(
  value: ValueWitnessV1,
) -> Pair<cek_constant_v1.ConstantTypeV1, List<Data>> {
  expect cek_constant_v1.ListConstant { element } = constant_type_v1(value)
  let payload = constant_payload_v1(value)
  expect data_is_list(payload)
  Pair(element, builtin.un_list_data(payload))
}

fn pair_payload_v1(
  value: ValueWitnessV1,
) -> Pair<
  Pair<cek_constant_v1.ConstantTypeV1, cek_constant_v1.ConstantTypeV1>,
  Pair<Data, Data>,
> {
  expect cek_constant_v1.PairConstant { first, second } = constant_type_v1(
    value,
  )
  let payload = constant_payload_v1(value)
  expect data_is_constr(payload)
  let Pair(tag, fields) = builtin.un_constr_data(payload)
  expect tag == 0
  expect [first_payload, second_payload] = fields
  Pair(Pair(first, second), Pair(first_payload, second_payload))
}

fn data_payload_v1(value: ValueWitnessV1) -> Data {
  expect constant_type_v1(value) == cek_constant_v1.DataConstant
  constant_payload_v1(value)
}

fn g1_bytes_v1(value: ValueWitnessV1) -> ByteArray {
  expect constant_type_v1(value) == cek_constant_v1.BlsG1Constant
  let payload = constant_payload_v1(value)
  expect data_is_bytes(payload)
  let compressed = builtin.un_b_data(payload)
  expect bytearray.length(compressed) == 48
  compressed
}

fn g2_bytes_v1(value: ValueWitnessV1) -> ByteArray {
  expect constant_type_v1(value) == cek_constant_v1.BlsG2Constant
  let payload = constant_payload_v1(value)
  expect data_is_bytes(payload)
  let compressed = builtin.un_b_data(payload)
  expect bytearray.length(compressed) == 96
  compressed
}

fn semantic_pairs_v1(items: List<Data>) -> List<Pair<Data, Data>> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect data_is_constr(item)
      let Pair(tag, fields) = builtin.un_constr_data(item)
      expect tag == 0
      expect [first, second] = fields
      [Pair(first, second), ..semantic_pairs_v1(rest)]
    }
  }
}

fn semantic_integers_v1(items: List<Data>) -> List<Int> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect data_is_integer(item)
      [builtin.un_i_data(item), ..semantic_integers_v1(rest)]
    }
  }
}

fn all_bit_indices_valid_v1(indices: List<Int>, bit_length: Int) -> Bool {
  when indices is {
    [] -> True
    [index, ..rest] -> and {
        index >= 0,
        index < bit_length,
        all_bit_indices_valid_v1(rest, bit_length),
      }
  }
}

fn encode_semantic_pairs_v1(items: List<Pair<Data, Data>>) -> List<Data> {
  when items is {
    [] -> []
    [item, ..rest] ->
      [pair_data(item.1st, item.2nd), ..encode_semantic_pairs_v1(rest)]
  }
}

fn verify_integer_binary_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  expect [left_value, right_value] = arguments
  let left = integer_v1(left_value)
  let right = integer_v1(right_value)
  if tag == 0 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.add_integer(left, right)),
    )
  } else if tag == 1 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.subtract_integer(left, right)),
    )
  } else if tag == 2 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.multiply_integer(left, right)),
    )
  } else if tag == 3 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.divide_integer(left, right)),
    )
  } else if tag == 4 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.quotient_integer(left, right)),
    )
  } else if tag == 5 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.remainder_integer(left, right)),
    )
  } else if tag == 6 {
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.mod_integer(left, right)),
    )
  } else if tag == 7 {
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(builtin.equals_integer(left, right)),
    )
  } else if tag == 8 {
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(builtin.less_than_integer(left, right)),
    )
  } else {
    expect tag == 9
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(builtin.less_than_equals_integer(left, right)),
    )
  }
}

fn verify_bytes_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 10 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(builtin.append_bytearray(bytes_v1(left), bytes_v1(right))),
    )
  } else if tag == 11 {
    expect [byte, bytes] = arguments
    let byte_value = integer_v1(byte)
    expect and {
        byte_value >= 0,
        byte_value <= 255,
      }
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(builtin.cons_bytearray(byte_value, bytes_v1(bytes))),
    )
  } else if tag == 12 {
    expect [start, length, bytes] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(
        builtin.slice_bytearray(
          integer_v1(start),
          integer_v1(length),
          bytes_v1(bytes),
        ),
      ),
    )
  } else if tag == 13 {
    expect [bytes] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.length_of_bytearray(bytes_v1(bytes))),
    )
  } else if tag == 14 {
    expect [bytes, index] = arguments
    let source = bytes_v1(bytes)
    let position = integer_v1(index)
    expect and {
        position >= 0,
        position < bytearray.length(source),
      }
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.index_bytearray(source, position)),
    )
  } else if tag == 15 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(builtin.equals_bytearray(bytes_v1(left), bytes_v1(right))),
    )
  } else if tag == 16 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(builtin.less_than_bytearray(bytes_v1(left), bytes_v1(right))),
    )
  } else if tag == 17 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(
        builtin.less_than_equals_bytearray(bytes_v1(left), bytes_v1(right)),
      ),
    )
  } else {
    expect [bytes] = arguments
    let source = bytes_v1(bytes)
    let digest = if tag == 18 {
      builtin.sha2_256(source)
    } else if tag == 19 {
      builtin.sha3_256(source)
    } else {
      expect tag == 20
      builtin.blake2b_256(source)
    }
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(digest),
    )
  }
}

fn verify_strings_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 22 {
    expect [left, right] = arguments
    let appended = builtin.append_string(
      builtin.decode_utf8(string_bytes_v1(left)),
      builtin.decode_utf8(string_bytes_v1(right)),
    )
      |> builtin.encode_utf8
    result_is_constant_v1(
      result,
      cek_constant_v1.StringConstant,
      builtin.b_data(appended),
    )
  } else if tag == 23 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(
        builtin.equals_string(
          builtin.decode_utf8(string_bytes_v1(left)),
          builtin.decode_utf8(string_bytes_v1(right)),
        ),
      ),
    )
  } else if tag == 24 {
    expect [value] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(string_bytes_v1(value)),
    )
  } else {
    expect tag == 25
    expect [value] = arguments
    // A canonical String result proves both UTF-8 validity and exact decoding.
    result_is_constant_v1(
      result,
      cek_constant_v1.StringConstant,
      builtin.b_data(bytes_v1(value)),
    )
  }
}

fn verify_signature_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  expect [key_value, message_value, signature_value] = arguments
  let key = bytes_v1(key_value)
  let message = bytes_v1(message_value)
  let signature = bytes_v1(signature_value)
  let verified = if tag == 21 {
    expect and {
        bytearray.length(key) == 32,
        bytearray.length(signature) == 64,
      }
    builtin.verify_ed25519_signature(key, message, signature)
  } else if tag == 52 {
    expect and {
        bytearray.length(key) == 33,
        bytearray.length(message) == 32,
        bytearray.length(signature) == 64,
      }
    builtin.verify_ecdsa_secp256k1_signature(key, message, signature)
  } else {
    expect tag == 53
    expect and {
        bytearray.length(key) == 32,
        bytearray.length(message) == 32,
        bytearray.length(signature) == 64,
      }
    builtin.verify_schnorr_secp256k1_signature(key, message, signature)
  }
  result_is_constant_v1(
    result,
    cek_constant_v1.BooleanConstant,
    bool_data(verified),
  )
}

fn verify_control_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 26 {
    expect [condition, when_true, when_false] = arguments
    result_is_selected_v1(
      result,
      if boolean_v1(condition) {
        when_true
      } else {
        when_false
      },
    )
  } else if tag == 27 {
    expect [unit, selected] = arguments
    let Void = unit_v1(unit)
    result_is_selected_v1(result, selected)
  } else {
    expect tag == 28
    expect [message, selected] = arguments
    let message_bytes = string_bytes_v1(message)
    expect bytearray.length(message_bytes) >= 0
    result_is_selected_v1(result, selected)
  }
}

fn verify_pair_and_list_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 29 {
    expect [pair] = arguments
    let Pair(types, payloads) = pair_payload_v1(pair)
    result_is_constant_v1(result, types.1st, payloads.1st)
  } else if tag == 30 {
    expect [pair] = arguments
    let Pair(types, payloads) = pair_payload_v1(pair)
    result_is_constant_v1(result, types.2nd, payloads.2nd)
  } else if tag == 31 {
    expect [source, when_empty, when_nonempty] = arguments
    let Pair(_, items) = list_payload_v1(source)
    result_is_selected_v1(
      result,
      if list.is_empty(items) {
        when_empty
      } else {
        when_nonempty
      },
    )
  } else if tag == 32 {
    expect [item, source] = arguments
    let Pair(element_type, items) = list_payload_v1(source)
    expect constant_type_v1(item) == element_type
    result_is_constant_v1(
      result,
      cek_constant_v1.ListConstant { element: element_type },
      builtin.list_data([constant_payload_v1(item), ..items]),
    )
  } else if tag == 33 {
    expect [source] = arguments
    let Pair(element_type, items) = list_payload_v1(source)
    expect [head, ..] = items
    result_is_constant_v1(result, element_type, head)
  } else if tag == 34 {
    expect [source] = arguments
    let Pair(element_type, items) = list_payload_v1(source)
    expect [_, ..tail] = items
    result_is_constant_v1(
      result,
      cek_constant_v1.ListConstant { element: element_type },
      builtin.list_data(tail),
    )
  } else {
    expect tag == 35
    expect [source] = arguments
    let Pair(_, items) = list_payload_v1(source)
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(list.is_empty(items)),
    )
  }
}

fn verify_choose_data_v1(
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  expect [source,
        when_constr, when_map, when_list, when_integer, when_bytes] = arguments
  let selected = builtin.choose_data(
    data_payload_v1(source),
    when_constr,
    when_map,
    when_list,
    when_integer,
    when_bytes,
  )
  result_is_selected_v1(result, selected)
}

fn verify_data_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 37 {
    expect [index, fields] = arguments
    let Pair(element_type, items) = list_payload_v1(fields)
    expect element_type == cek_constant_v1.DataConstant
    result_is_constant_v1(
      result,
      cek_constant_v1.DataConstant,
      builtin.constr_data(integer_v1(index), items),
    )
  } else if tag == 38 {
    expect [items_value] = arguments
    let Pair(element_type, items) = list_payload_v1(items_value)
    expect
      element_type == cek_constant_v1.PairConstant {
        first: cek_constant_v1.DataConstant,
        second: cek_constant_v1.DataConstant,
      }
    result_is_constant_v1(
      result,
      cek_constant_v1.DataConstant,
      builtin.map_data(semantic_pairs_v1(items)),
    )
  } else if tag == 39 {
    expect [items_value] = arguments
    let Pair(element_type, items) = list_payload_v1(items_value)
    expect element_type == cek_constant_v1.DataConstant
    result_is_constant_v1(
      result,
      cek_constant_v1.DataConstant,
      builtin.list_data(items),
    )
  } else if tag == 40 {
    expect [integer] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.DataConstant,
      builtin.i_data(integer_v1(integer)),
    )
  } else if tag == 41 {
    expect [bytes] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.DataConstant,
      builtin.b_data(bytes_v1(bytes)),
    )
  } else if tag == 42 {
    expect [source] = arguments
    let data = data_payload_v1(source)
    expect data_is_constr(data)
    let Pair(index, fields) = builtin.un_constr_data(data)
    result_is_constant_v1(
      result,
      cek_constant_v1.PairConstant {
        first: cek_constant_v1.IntegerConstant,
        second: cek_constant_v1.ListConstant {
          element: cek_constant_v1.DataConstant,
        },
      },
      pair_data(builtin.i_data(index), builtin.list_data(fields)),
    )
  } else if tag == 43 {
    expect [source] = arguments
    let data = data_payload_v1(source)
    expect data_is_map(data)
    result_is_constant_v1(
      result,
      cek_constant_v1.ListConstant {
        element: cek_constant_v1.PairConstant {
          first: cek_constant_v1.DataConstant,
          second: cek_constant_v1.DataConstant,
        },
      },
      builtin.list_data(encode_semantic_pairs_v1(builtin.un_map_data(data))),
    )
  } else if tag == 44 {
    expect [source] = arguments
    let data = data_payload_v1(source)
    expect data_is_list(data)
    result_is_constant_v1(
      result,
      cek_constant_v1.ListConstant { element: cek_constant_v1.DataConstant },
      builtin.list_data(builtin.un_list_data(data)),
    )
  } else if tag == 45 {
    expect [source] = arguments
    let data = data_payload_v1(source)
    expect data_is_integer(data)
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(builtin.un_i_data(data)),
    )
  } else if tag == 46 {
    expect [source] = arguments
    let data = data_payload_v1(source)
    expect data_is_bytes(data)
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(builtin.un_b_data(data)),
    )
  } else if tag == 47 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(
        builtin.equals_data(data_payload_v1(left), data_payload_v1(right)),
      ),
    )
  } else if tag == 48 {
    expect [first, second] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.PairConstant {
        first: cek_constant_v1.DataConstant,
        second: cek_constant_v1.DataConstant,
      },
      pair_data(data_payload_v1(first), data_payload_v1(second)),
    )
  } else if tag == 49 {
    expect [unit] = arguments
    let Void = unit_v1(unit)
    result_is_constant_v1(
      result,
      cek_constant_v1.ListConstant { element: cek_constant_v1.DataConstant },
      builtin.list_data([]),
    )
  } else if tag == 50 {
    expect [unit] = arguments
    let Void = unit_v1(unit)
    result_is_constant_v1(
      result,
      cek_constant_v1.ListConstant {
        element: cek_constant_v1.PairConstant {
          first: cek_constant_v1.DataConstant,
          second: cek_constant_v1.DataConstant,
        },
      },
      builtin.list_data([]),
    )
  } else {
    expect tag == 51
    expect [source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(builtin.serialise_data(data_payload_v1(source))),
    )
  }
}

fn empty_semantic_material_v1(witness: SemanticBuiltinWitnessV1) -> Bool {
  and {
    witness.data_nodes == [],
    witness.list_nodes == [],
    witness.pair_nodes == [],
    witness.scalar_preimages == [],
  }
}

fn verify_semantic_unconstr_data_v1(
  source: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect witness.pair_nodes == []
  let source_summary = data_constant_summary_v1(source)
  let list_summary = exact_list_summary_for_node_v1(node, witness.list_nodes)
  expect top_node_matches_v1(source_summary, node, list_summary, None)
  let Pair(constructor, fields) = when node is {
    cek_data_v1.ConstrSmallData {
      constructor,
      fields_count,
      fields_root,
      cbor_length: _,
      memory: _,
    } -> {
      expect witness.scalar_preimages == []
      Pair(
        constructor,
        cek_data_v1.DataSequenceSummaryV1 {
          root: fields_root,
          length: fields_count,
          payload_cbor_length: when list_summary is {
            None -> 0
            Some(summary) -> summary.payload_cbor_length
          },
          memory: when list_summary is {
            None -> 0
            Some(summary) -> summary.memory
          },
        },
      )
    }
    cek_data_v1.ConstrLargeData {
      constructor_cbor_root,
      constructor_cbor_length,
      constructor_memory,
      fields_count,
      fields_root,
      cbor_length: _,
      memory: _,
    } -> {
      expect [raw_constructor] = witness.scalar_preimages
      expect
        bytearray.length(raw_constructor) <= cek_data_scan_v1.max_revealed_data_leaf_bytes
      expect
        constructor_cbor_root == cek_data_scan_v1.bounded_blob_root_v1(
          raw_constructor,
        )
      expect constructor_cbor_length == bytearray.length(raw_constructor)
      expect Some(data) = cbor.deserialise(raw_constructor)
      expect cbor.serialise(data) == raw_constructor
      expect builtin.choose_data(data, False, False, False, True, False)
      let constructor = builtin.un_i_data(data)
      expect constructor > 127
      expect
        constructor_memory == 4 + cek_constant_v1.integer_memory_size_v1(
          constructor,
        )
      Pair(
        constructor,
        cek_data_v1.DataSequenceSummaryV1 {
          root: fields_root,
          length: fields_count,
          payload_cbor_length: when list_summary is {
            None -> 0
            Some(summary) -> summary.payload_cbor_length
          },
          memory: when list_summary is {
            None -> 0
            Some(summary) -> summary.memory
          },
        },
      )
    }
    _ -> fail @"unConstrData requires constructor Data"
  }
  let constructor_cbor = cbor.serialise(builtin.i_data(constructor))
  let constructor_summary = cek_data_v1.integer_data_summary_v1(
    constructor,
    cek_data_scan_v1.bounded_blob_root_v1(constructor_cbor),
  )
  let fields_summary = cek_data_v1.list_data_summary_v1(fields)
  let pair_payload = cek_data_v1.small_constr_data_summary_v1(
    0,
    cek_data_v1.prepend_data_list_summary_v1(
      constructor_summary,
      cek_data_v1.prepend_data_list_summary_v1(
        fields_summary,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    ),
  )
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.PairConstant {
      first: cek_constant_v1.IntegerConstant,
      second: cek_constant_v1.ListConstant {
        element: cek_constant_v1.DataConstant,
      },
    },
    pair_payload,
    constructor_summary.memory - 4 + fields.memory,
  )
}

fn verify_semantic_unlist_data_v1(
  source: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect witness.pair_nodes == []
  expect witness.scalar_preimages == []
  let first = exact_list_summary_for_node_v1(node, witness.list_nodes)
  let summary = data_constant_summary_v1(source)
  expect top_node_matches_v1(summary, node, first, None)
  let sequence = data_sequence_from_list_node_v1(node)
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.ListConstant { element: cek_constant_v1.DataConstant },
    summary,
    sequence.memory,
  )
}

fn verify_semantic_uninteger_data_v1(
  source: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect [raw_cbor] = witness.scalar_preimages
  expect witness.list_nodes == []
  expect witness.pair_nodes == []
  let source_summary = data_constant_summary_v1(source)
  let integer = canonical_integer_leaf_v1(source_summary, node, raw_cbor)
  expect
    bytearray.length(cbor.serialise(builtin.i_data(integer))) == bytearray.length(
      raw_cbor,
    )
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.IntegerConstant,
    source_summary,
    source_summary.memory - 4,
  )
}

fn verify_semantic_unbytes_data_v1(
  source: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect [bytes] = witness.scalar_preimages
  expect witness.list_nodes == []
  expect witness.pair_nodes == []
  let source_summary = data_constant_summary_v1(source)
  let revealed_bytes = canonical_bytes_leaf_v1(source_summary, node, bytes)
  expect bytearray.length(revealed_bytes) == bytearray.length(bytes)
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.ByteStringConstant,
    source_summary,
    source_summary.memory - 4,
  )
}

fn verify_semantic_idata_v1(
  integer: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect [raw_cbor] = witness.scalar_preimages
  expect witness.list_nodes == []
  expect witness.pair_nodes == []
  let parts = semantic_constant_parts_v1(integer)
  expect parts.constant_type == cek_constant_v1.IntegerConstant
  let integer_value = canonical_integer_leaf_v1(parts.payload, node, raw_cbor)
  expect
    bytearray.length(cbor.serialise(builtin.i_data(integer_value))) == bytearray.length(
      raw_cbor,
    )
  expect parts.memory == parts.payload.memory - 4
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.DataConstant,
    parts.payload,
    parts.payload.memory,
  )
}

fn verify_semantic_bdata_v1(
  bytes_value: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect [bytes] = witness.scalar_preimages
  expect witness.list_nodes == []
  expect witness.pair_nodes == []
  let parts = semantic_constant_parts_v1(bytes_value)
  expect parts.constant_type == cek_constant_v1.ByteStringConstant
  let revealed_bytes = canonical_bytes_leaf_v1(parts.payload, node, bytes)
  expect bytearray.length(revealed_bytes) == bytearray.length(bytes)
  expect parts.memory == parts.payload.memory - 4
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.DataConstant,
    parts.payload,
    parts.payload.memory,
  )
}

fn verify_semantic_mklist_data_v1(
  items: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [node] = witness.data_nodes
  expect witness.pair_nodes == []
  expect witness.scalar_preimages == []
  let first = exact_list_summary_for_node_v1(node, witness.list_nodes)
  let sequence = list_constant_sequence_v1(
    items,
    cek_constant_v1.DataConstant,
    node,
    first,
  )
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.DataConstant,
    cek_data_v1.list_data_summary_v1(sequence),
    4 + sequence.memory,
  )
}

fn verify_semantic_mkconstr_data_v1(
  index: ValueWitnessV1,
  fields_value: ValueWitnessV1,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect witness.pair_nodes == []
  let Pair(constructor, fields_node) = when index is {
    ConstantValue(_) -> {
      expect [fields_node] = witness.data_nodes
      expect witness.scalar_preimages == []
      Pair(integer_v1(index), fields_node)
    }
    SemanticConstantValue { .. } -> {
      expect [index_node, fields_node] = witness.data_nodes
      expect [raw_index] = witness.scalar_preimages
      let parts = semantic_constant_parts_v1(index)
      expect parts.constant_type == cek_constant_v1.IntegerConstant
      let constructor = canonical_integer_leaf_v1(
        parts.payload,
        index_node,
        raw_index,
      )
      expect parts.memory == parts.payload.memory - 4
      Pair(constructor, fields_node)
    }
    _ -> fail @"constrData index must be an integer constant"
  }
  expect constructor >= 0
  let first = exact_list_summary_for_node_v1(fields_node, witness.list_nodes)
  let fields = list_constant_sequence_v1(
    fields_value,
    cek_constant_v1.DataConstant,
    fields_node,
    first,
  )
  let summary = if constructor <= 127 {
    cek_data_v1.small_constr_data_summary_v1(constructor, fields)
  } else {
    let raw_constructor = cbor.serialise(builtin.i_data(constructor))
    cek_data_v1.large_constr_data_summary_v1(
      constructor,
      cek_data_scan_v1.bounded_blob_root_v1(raw_constructor),
      fields,
    )
  }
  result_is_semantic_constant_v1(
    result,
    cek_constant_v1.DataConstant,
    summary,
    summary.memory,
  )
}

fn verify_semantic_data_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  if tag == 37 {
    expect [index, fields] = arguments
    verify_semantic_mkconstr_data_v1(index, fields, result, witness)
  } else if tag == 39 {
    expect [items] = arguments
    verify_semantic_mklist_data_v1(items, result, witness)
  } else if tag == 40 {
    expect [integer] = arguments
    verify_semantic_idata_v1(integer, result, witness)
  } else if tag == 41 {
    expect [bytes] = arguments
    verify_semantic_bdata_v1(bytes, result, witness)
  } else if tag == 42 {
    expect [source] = arguments
    verify_semantic_unconstr_data_v1(source, result, witness)
  } else if tag == 44 {
    expect [source] = arguments
    verify_semantic_unlist_data_v1(source, result, witness)
  } else if tag == 45 {
    expect [source] = arguments
    verify_semantic_uninteger_data_v1(source, result, witness)
  } else if tag == 46 {
    expect [source] = arguments
    verify_semantic_unbytes_data_v1(source, result, witness)
  } else if tag == 47 {
    expect [left, right] = arguments
    expect empty_semantic_material_v1(witness)
    result_is_semantic_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      cek_data_v1.semantic_data_summary_v1(
        bool_data(
          data_constant_summary_v1(left) == data_constant_summary_v1(right),
        ),
      ),
      1,
    )
  } else if tag == 48 {
    expect [first, second] = arguments
    expect empty_semantic_material_v1(witness)
    let first_summary = data_constant_summary_v1(first)
    let second_summary = data_constant_summary_v1(second)
    result_is_semantic_constant_v1(
      result,
      cek_constant_v1.PairConstant {
        first: cek_constant_v1.DataConstant,
        second: cek_constant_v1.DataConstant,
      },
      cek_data_v1.small_constr_data_summary_v1(
        0,
        cek_data_v1.prepend_data_list_summary_v1(
          first_summary,
          cek_data_v1.prepend_data_list_summary_v1(
            second_summary,
            cek_data_v1.empty_data_list_summary_v1(),
          ),
        ),
      ),
      first_summary.memory + second_summary.memory,
    )
  } else if tag == 49 {
    expect [unit] = arguments
    expect empty_semantic_material_v1(witness)
    let Void = unit_v1(unit)
    let empty = cek_data_v1.empty_data_list_summary_v1()
    result_is_semantic_constant_v1(
      result,
      cek_constant_v1.ListConstant { element: cek_constant_v1.DataConstant },
      cek_data_v1.list_data_summary_v1(empty),
      0,
    )
  } else if tag == 50 {
    expect [unit] = arguments
    expect empty_semantic_material_v1(witness)
    let Void = unit_v1(unit)
    let empty = cek_data_v1.empty_data_list_summary_v1()
    result_is_semantic_constant_v1(
      result,
      cek_constant_v1.ListConstant {
        element: cek_constant_v1.PairConstant {
          first: cek_constant_v1.DataConstant,
          second: cek_constant_v1.DataConstant,
        },
      },
      cek_data_v1.list_data_summary_v1(empty),
      0,
    )
  } else if tag == 51 {
    expect [source] = arguments
    expect witness.data_nodes == []
    expect witness.list_nodes == []
    expect witness.pair_nodes == []
    expect [raw_data] = witness.scalar_preimages
    expect bytearray.length(raw_data) > 0
    expect
      bytearray.length(raw_data) <= cek_data_scan_v1.max_revealed_data_leaf_bytes
    expect Some(data) = cbor.deserialise(raw_data)
    expect cbor.serialise(data) == raw_data
    expect
      cek_data_v1.semantic_data_summary_v1(data) == data_constant_summary_v1(
        source,
      )
    let payload = builtin.b_data(raw_data)
    result_is_semantic_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      cek_data_v1.semantic_data_summary_v1(payload),
      cek_constant_v1.bytearray_memory_size_v1(raw_data),
    )
  } else {
    False
  }
}

fn constant_memory_from_payload_node_v1(
  constant_type: cek_constant_v1.ConstantTypeV1,
  node: cek_data_v1.DataNodeV1,
) -> Int {
  when constant_type is {
    cek_constant_v1.IntegerConstant -> cek_data_v1.data_node_memory_v1(node) - 4
    cek_constant_v1.ByteStringConstant ->
      cek_data_v1.data_node_memory_v1(node) - 4
    cek_constant_v1.StringConstant -> cek_data_v1.data_node_memory_v1(node) - 4
    cek_constant_v1.UnitConstant -> 1
    cek_constant_v1.BooleanConstant -> 1
    cek_constant_v1.DataConstant -> cek_data_v1.data_node_memory_v1(node)
    cek_constant_v1.ListConstant { element } -> {
      expect cek_data_v1.ListData { items_count, memory, .. } = node
      if element == cek_constant_v1.DataConstant {
        memory - 4
      } else if element == cek_constant_v1.IntegerConstant {
        memory - 4 - items_count * 4
      } else if element == cek_constant_v1.PairConstant {
        first: cek_constant_v1.DataConstant,
        second: cek_constant_v1.DataConstant,
      } {
        memory - 4 - items_count * 4
      } else {
        fail @"semantic list element memory requires a recursive proof"
      }
    }
    cek_constant_v1.PairConstant { first, second } ->
      if and {
        first == cek_constant_v1.DataConstant,
        second == cek_constant_v1.DataConstant,
      } {
        cek_data_v1.data_node_memory_v1(node) - 4
      } else if and {
        first == cek_constant_v1.IntegerConstant,
        second == cek_constant_v1.ListConstant {
          element: cek_constant_v1.DataConstant,
        },
      } {
        cek_data_v1.data_node_memory_v1(node) - 12
      } else {
        fail @"semantic pair memory requires a recursive proof"
      }
    cek_constant_v1.BlsG1Constant -> 48
    cek_constant_v1.BlsG2Constant -> 96
    cek_constant_v1.BlsMillerLoopResultConstant -> 192
  }
}

fn semantic_pair_components_v1(
  source: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Pair<
  Pair<cek_constant_v1.ConstantTypeV1, cek_constant_v1.ConstantTypeV1>,
  Pair<
    Pair<cek_data_v1.DataSummaryV1, Int>,
    Pair<cek_data_v1.DataSummaryV1, Int>,
  >,
> {
  expect [payload_node, first_node, second_node] = witness.data_nodes
  expect [first_link, second_link] = witness.list_nodes
  expect witness.pair_nodes == []
  expect witness.scalar_preimages == []
  let parts = semantic_constant_parts_v1(source)
  expect cek_constant_v1.PairConstant { first, second } = parts.constant_type
  expect
    top_node_matches_v1(parts.payload, payload_node, Some(first_link), None)
  expect cek_data_v1.ConstrSmallData { constructor, fields_count, .. } = payload_node
  expect constructor == 0
  expect fields_count == 2
  expect
    cek_data_v1.verify_data_list_link_v1(
      first_link,
      first_node,
      Some(second_link),
    )
  expect cek_data_v1.verify_data_list_link_v1(second_link, second_node, None)
  let first_summary = summary_from_node_v1(first_node)
  let second_summary = summary_from_node_v1(second_node)
  let Pair(first_memory, second_memory) = when source is {
    ConstantValue(direct) -> {
      let payload = cek_constant_v1.constant_payload_v1(direct)
      let Pair(tag, fields) = builtin.un_constr_data(payload)
      expect tag == 0
      expect [first_payload, second_payload] = fields
      expect
        cek_data_v1.semantic_data_summary_v1(first_payload) == first_summary
      expect
        cek_data_v1.semantic_data_summary_v1(second_payload) == second_summary
      Pair(
        cek_constant_v1.constant_payload_memory_size_v1(first, first_payload),
        cek_constant_v1.constant_payload_memory_size_v1(second, second_payload),
      )
    }
    _ ->
      Pair(
        constant_memory_from_payload_node_v1(first, first_node),
        constant_memory_from_payload_node_v1(second, second_node),
      )
  }
  expect parts.memory == first_memory + second_memory
  Pair(
    Pair(first, second),
    Pair(Pair(first_summary, first_memory), Pair(second_summary, second_memory)),
  )
}

fn verify_semantic_pair_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  expect [source] = arguments
  let Pair(types, values) = semantic_pair_components_v1(source, witness)
  if tag == 29 {
    result_is_semantic_constant_v1(
      result,
      types.1st,
      values.1st.1st,
      values.1st.2nd,
    )
  } else {
    expect tag == 30
    result_is_semantic_constant_v1(
      result,
      types.2nd,
      values.2nd.1st,
      values.2nd.2nd,
    )
  }
}

fn semantic_list_source_v1(
  source: ValueWitnessV1,
  node: cek_data_v1.DataNodeV1,
  first: Option<cek_data_v1.DataListNodeV1>,
) -> Pair<
  cek_constant_v1.ConstantTypeV1,
  Pair<cek_data_v1.DataSequenceSummaryV1, Int>,
> {
  let parts = semantic_constant_parts_v1(source)
  expect cek_constant_v1.ListConstant { element } = parts.constant_type
  expect top_node_matches_v1(parts.payload, node, first, None)
  let sequence = data_sequence_from_list_node_v1(node)
  expect
    when source is {
      ConstantValue(_) -> True
      _ ->
        parts.memory == constant_memory_from_payload_node_v1(
          parts.constant_type,
          node,
        )
    }
  Pair(element, Pair(sequence, parts.memory))
}

fn semantic_list_head_and_tail_v1(
  source: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Pair<
  cek_constant_v1.ConstantTypeV1,
  Pair<
    Pair<cek_data_v1.DataSummaryV1, Int>,
    Pair<cek_data_v1.DataSequenceSummaryV1, Int>,
  >,
> {
  expect [source_node, head_node] = witness.data_nodes
  expect [first_link, ..remaining_links] = witness.list_nodes
  expect witness.pair_nodes == []
  expect witness.scalar_preimages == []
  let tail_link = if first_link.length == 1 {
    expect remaining_links == []
    None
  } else {
    expect [link] = remaining_links
    Some(link)
  }
  let Pair(element, source_parts) = semantic_list_source_v1(
    source,
    source_node,
    Some(first_link),
  )
  expect first_link.length == source_parts.1st.length
  expect cek_data_v1.verify_data_list_link_v1(first_link, head_node, tail_link)
  let head_summary = summary_from_node_v1(head_node)
  let head_memory = when source is {
    ConstantValue(direct) -> {
      let items = direct
        |> cek_constant_v1.constant_payload_v1
        |> builtin.un_list_data
      expect [head, ..] = items
      expect cek_data_v1.semantic_data_summary_v1(head) == head_summary
      cek_constant_v1.constant_payload_memory_size_v1(element, head)
    }
    _ -> constant_memory_from_payload_node_v1(element, head_node)
  }
  expect source_parts.2nd >= head_memory
  let tail = cek_data_v1.DataSequenceSummaryV1 {
    root: first_link.tail,
    length: first_link.length - 1,
    payload_cbor_length: first_link.payload_cbor_length - first_link.head_cbor_length,
    memory: first_link.memory - first_link.head_memory,
  }
  Pair(
    element,
    Pair(
      Pair(head_summary, head_memory),
      Pair(tail, source_parts.2nd - head_memory),
    ),
  )
}

fn verify_semantic_list_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  if tag == 31 || tag == 35 {
    expect [source, ..branches] = arguments
    expect [node] = witness.data_nodes
    expect witness.pair_nodes == []
    expect witness.scalar_preimages == []
    let first = exact_list_summary_for_node_v1(node, witness.list_nodes)
    let Pair(_, parts) = semantic_list_source_v1(source, node, first)
    if tag == 31 {
      expect [when_empty, when_nonempty] = branches
      result_is_selected_v1(
        result,
        if parts.1st.length == 0 {
          when_empty
        } else {
          when_nonempty
        },
      )
    } else {
      expect branches == []
      result_is_semantic_constant_v1(
        result,
        cek_constant_v1.BooleanConstant,
        cek_data_v1.semantic_data_summary_v1(bool_data(parts.1st.length == 0)),
        1,
      )
    }
  } else if tag == 32 {
    expect [item, source] = arguments
    expect [node] = witness.data_nodes
    expect witness.pair_nodes == []
    expect witness.scalar_preimages == []
    let first = exact_list_summary_for_node_v1(node, witness.list_nodes)
    let Pair(element, parts) = semantic_list_source_v1(source, node, first)
    let item_parts = semantic_constant_parts_v1(item)
    expect item_parts.constant_type == element
    let sequence = cek_data_v1.prepend_data_list_summary_v1(
      item_parts.payload,
      parts.1st,
    )
    result_is_semantic_constant_v1(
      result,
      cek_constant_v1.ListConstant { element },
      cek_data_v1.list_data_summary_v1(sequence),
      item_parts.memory + parts.2nd,
    )
  } else if tag == 33 || tag == 34 {
    expect [source] = arguments
    let Pair(element, values) = semantic_list_head_and_tail_v1(source, witness)
    if tag == 33 {
      result_is_semantic_constant_v1(
        result,
        element,
        values.1st.1st,
        values.1st.2nd,
      )
    } else {
      result_is_semantic_constant_v1(
        result,
        cek_constant_v1.ListConstant { element },
        cek_data_v1.list_data_summary_v1(values.2nd.1st),
        values.2nd.2nd,
      )
    }
  } else {
    False
  }
}

fn verify_v3_bytes_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 71 || tag == 72 || tag == 78 || tag == 84 || tag == 85 || tag == 86 {
    expect [source_value] = arguments
    let source = bytes_v1(source_value)
    if tag == 71 {
      result_is_constant_v1(
        result,
        cek_constant_v1.ByteStringConstant,
        builtin.b_data(builtin.keccak_256(source)),
      )
    } else if tag == 72 {
      result_is_constant_v1(
        result,
        cek_constant_v1.ByteStringConstant,
        builtin.b_data(builtin.blake2b_224(source)),
      )
    } else if tag == 78 {
      result_is_constant_v1(
        result,
        cek_constant_v1.ByteStringConstant,
        builtin.b_data(builtin.complement_bytearray(source)),
      )
    } else if tag == 84 {
      result_is_constant_v1(
        result,
        cek_constant_v1.IntegerConstant,
        builtin.i_data(builtin.count_set_bits(source)),
      )
    } else if tag == 85 {
      result_is_constant_v1(
        result,
        cek_constant_v1.IntegerConstant,
        builtin.i_data(builtin.find_first_set_bit(source)),
      )
    } else {
      expect tag == 86
      result_is_constant_v1(
        result,
        cek_constant_v1.ByteStringConstant,
        builtin.b_data(builtin.ripemd_160(source)),
      )
    }
  } else if tag == 73 {
    expect [endianness, size, value] = arguments
    let target_size = integer_v1(size)
    let integer = integer_v1(value)
    expect and {
        target_size >= 0,
        integer >= 0,
      }
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(
        builtin.integer_to_bytearray(
          boolean_v1(endianness),
          target_size,
          integer,
        ),
      ),
    )
  } else if tag == 74 {
    expect [endianness, source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.IntegerConstant,
      builtin.i_data(
        builtin.bytearray_to_integer(boolean_v1(endianness), bytes_v1(source)),
      ),
    )
  } else if tag >= 75 && tag <= 77 {
    expect [padding, left, right] = arguments
    let pad = boolean_v1(padding)
    let left_bytes = bytes_v1(left)
    let right_bytes = bytes_v1(right)
    let combined = if tag == 75 {
      builtin.and_bytearray(pad, left_bytes, right_bytes)
    } else if tag == 76 {
      builtin.or_bytearray(pad, left_bytes, right_bytes)
    } else {
      builtin.xor_bytearray(pad, left_bytes, right_bytes)
    }
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(combined),
    )
  } else if tag == 79 {
    expect [source_value, index_value] = arguments
    let source = bytes_v1(source_value)
    let index = integer_v1(index_value)
    expect and {
        index >= 0,
        index < bytearray.length(source) * 8,
      }
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(builtin.read_bit(source, index)),
    )
  } else if tag == 80 {
    expect [source_value, indices_value, bit_value] = arguments
    let source = bytes_v1(source_value)
    let Pair(element_type, index_payloads) = list_payload_v1(indices_value)
    expect element_type == cek_constant_v1.IntegerConstant
    let indices = semantic_integers_v1(index_payloads)
    expect all_bit_indices_valid_v1(indices, bytearray.length(source) * 8)
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(builtin.write_bits(source, indices, boolean_v1(bit_value))),
    )
  } else if tag == 81 {
    expect [length_value, byte_value] = arguments
    let length = integer_v1(length_value)
    let byte = integer_v1(byte_value)
    expect and {
        length >= 0,
        byte >= 0,
        byte <= 255,
      }
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(builtin.replicate_byte(length, byte)),
    )
  } else if tag == 82 {
    expect [source, offset] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(
        builtin.shift_bytearray(bytes_v1(source), integer_v1(offset)),
      ),
    )
  } else {
    expect tag == 83
    expect [source, offset] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(
        builtin.rotate_bytearray(bytes_v1(source), integer_v1(offset)),
      ),
    )
  }
}

fn verify_bls_g1_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 54 {
    expect [left_value, right_value] = arguments
    let left = g1_bytes_v1(left_value)
      |> builtin.bls12_381_g1_uncompress
    let right = g1_bytes_v1(right_value)
      |> builtin.bls12_381_g1_uncompress
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG1Constant,
      builtin.b_data(
        builtin.bls12_381_g1_add(left, right)
          |> builtin.bls12_381_g1_compress,
      ),
    )
  } else if tag == 55 {
    expect [source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG1Constant,
      builtin.b_data(
        g1_bytes_v1(source)
          |> builtin.bls12_381_g1_uncompress
          |> builtin.bls12_381_g1_neg
          |> builtin.bls12_381_g1_compress,
      ),
    )
  } else if tag == 56 {
    expect [scalar, source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG1Constant,
      builtin.b_data(
        builtin.bls12_381_g1_scalar_mul(
          integer_v1(scalar),
          g1_bytes_v1(source) |> builtin.bls12_381_g1_uncompress,
        )
          |> builtin.bls12_381_g1_compress,
      ),
    )
  } else if tag == 57 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(
        builtin.bls12_381_g1_equal(
          g1_bytes_v1(left) |> builtin.bls12_381_g1_uncompress,
          g1_bytes_v1(right) |> builtin.bls12_381_g1_uncompress,
        ),
      ),
    )
  } else if tag == 58 {
    expect [message, domain] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG1Constant,
      builtin.b_data(
        builtin.bls12_381_g1_hash_to_group(bytes_v1(message), bytes_v1(domain))
          |> builtin.bls12_381_g1_compress,
      ),
    )
  } else if tag == 59 {
    expect [source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(
        g1_bytes_v1(source)
          |> builtin.bls12_381_g1_uncompress
          |> builtin.bls12_381_g1_compress,
      ),
    )
  } else {
    expect tag == 60
    expect [source] = arguments
    let uncompressed = bytes_v1(source)
      |> builtin.bls12_381_g1_uncompress
      |> builtin.bls12_381_g1_compress
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG1Constant,
      builtin.b_data(uncompressed),
    )
  }
}

fn verify_bls_g2_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 61 {
    expect [left_value, right_value] = arguments
    let left = g2_bytes_v1(left_value)
      |> builtin.bls12_381_g2_uncompress
    let right = g2_bytes_v1(right_value)
      |> builtin.bls12_381_g2_uncompress
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG2Constant,
      builtin.b_data(
        builtin.bls12_381_g2_add(left, right)
          |> builtin.bls12_381_g2_compress,
      ),
    )
  } else if tag == 62 {
    expect [source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG2Constant,
      builtin.b_data(
        g2_bytes_v1(source)
          |> builtin.bls12_381_g2_uncompress
          |> builtin.bls12_381_g2_neg
          |> builtin.bls12_381_g2_compress,
      ),
    )
  } else if tag == 63 {
    expect [scalar, source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG2Constant,
      builtin.b_data(
        builtin.bls12_381_g2_scalar_mul(
          integer_v1(scalar),
          g2_bytes_v1(source) |> builtin.bls12_381_g2_uncompress,
        )
          |> builtin.bls12_381_g2_compress,
      ),
    )
  } else if tag == 64 {
    expect [left, right] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BooleanConstant,
      bool_data(
        builtin.bls12_381_g2_equal(
          g2_bytes_v1(left) |> builtin.bls12_381_g2_uncompress,
          g2_bytes_v1(right) |> builtin.bls12_381_g2_uncompress,
        ),
      ),
    )
  } else if tag == 65 {
    expect [message, domain] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG2Constant,
      builtin.b_data(
        builtin.bls12_381_g2_hash_to_group(bytes_v1(message), bytes_v1(domain))
          |> builtin.bls12_381_g2_compress,
      ),
    )
  } else if tag == 66 {
    expect [source] = arguments
    result_is_constant_v1(
      result,
      cek_constant_v1.ByteStringConstant,
      builtin.b_data(
        g2_bytes_v1(source)
          |> builtin.bls12_381_g2_uncompress
          |> builtin.bls12_381_g2_compress,
      ),
    )
  } else {
    expect tag == 67
    expect [source] = arguments
    let uncompressed = bytes_v1(source)
      |> builtin.bls12_381_g2_uncompress
      |> builtin.bls12_381_g2_compress
    result_is_constant_v1(
      result,
      cek_constant_v1.BlsG2Constant,
      builtin.b_data(uncompressed),
    )
  }
}

fn verify_bls_expression_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  if tag == 68 {
    expect [g1, g2] = arguments
    let g1_compressed = g1_bytes_v1(g1)
    let g2_compressed = g2_bytes_v1(g2)
    // Round-trips make invalid compressed points fail before the expression
    // commitment is admitted.
    expect
      (
        g1_compressed
          |> builtin.bls12_381_g1_uncompress
          |> builtin.bls12_381_g1_compress
      ) == g1_compressed
    expect
      (
        g2_compressed
          |> builtin.bls12_381_g2_uncompress
          |> builtin.bls12_381_g2_compress
      ) == g2_compressed
    let expression_root = cek_proof_v1.hash_bls_miller_loop_expression_v1(
      value_root_v1(g1),
      value_root_v1(g2),
    )
    expect BlsMillerLoopValue(result_expression_root) = result
    expression_root == result_expression_root
  } else {
    expect tag == 69
    expect [BlsMillerLoopValue(left), BlsMillerLoopValue(right)] = arguments
    expect BlsMillerLoopValue(result_expression_root) = result
    result_expression_root == cek_proof_v1.hash_bls_multiply_expression_v1(
      left,
      right,
    )
  }
}

fn with_evaluated_bls_expression_v1(
  expression: BlsExpressionWitnessV1,
  then: fn(ByteArray, MillerLoopResult) -> result,
) -> result {
  when expression is {
    BlsMillerLoopExpression { g1, g2 } -> {
      let g1_value = ConstantValue(g1)
      let g2_value = ConstantValue(g2)
      let g1_compressed = g1_bytes_v1(g1_value)
      let g2_compressed = g2_bytes_v1(g2_value)
      then(
        cek_proof_v1.hash_bls_miller_loop_expression_v1(
          value_root_v1(g1_value),
          value_root_v1(g2_value),
        ),
        builtin.bls12_381_miller_loop(
          builtin.bls12_381_g1_uncompress(g1_compressed),
          builtin.bls12_381_g2_uncompress(g2_compressed),
        ),
      )
    }
    BlsMultiplyExpression { left, right } ->
      with_evaluated_bls_expression_v1(
        left,
        fn(left_root, left_result) {
          with_evaluated_bls_expression_v1(
            right,
            fn(right_root, right_result) {
              then(
                cek_proof_v1.hash_bls_multiply_expression_v1(
                  left_root,
                  right_root,
                ),
                builtin.bls12_381_mul_miller_loop_result(
                  left_result,
                  right_result,
                ),
              )
            },
          )
        },
      )
  }
}

fn larger(left: Int, right: Int) -> Int {
  if left >= right {
    left
  } else {
    right
  }
}

fn bls_expression_metrics_v1(
  expression: BlsExpressionWitnessV1,
) -> Pair<Int, Int> {
  when expression is {
    BlsMillerLoopExpression { .. } -> Pair(1, 1)
    BlsMultiplyExpression { left, right } -> {
      let Pair(left_leaves, left_depth) = bls_expression_metrics_v1(left)
      let Pair(right_leaves, right_depth) = bls_expression_metrics_v1(right)
      Pair(left_leaves + right_leaves, larger(left_depth, right_depth) + 1)
    }
  }
}

fn verify_direct_bls_final_with_expected_roots_v1(
  builtin_value_root: ByteArray,
  expected_left_root: ByteArray,
  expected_right_root: ByteArray,
  enforce_expected_roots: Bool,
  left: BlsExpressionWitnessV1,
  right: BlsExpressionWitnessV1,
  result: ValueWitnessV1,
) -> Bool {
  let Pair(left_leaves, left_depth) = bls_expression_metrics_v1(left)
  let Pair(right_leaves, right_depth) = bls_expression_metrics_v1(right)
  expect and {
      left_leaves + right_leaves <= max_direct_bls_miller_loop_leaves,
      left_depth <= max_direct_bls_miller_loop_leaves,
      right_depth <= max_direct_bls_miller_loop_leaves,
    }
  with_evaluated_bls_expression_v1(
    left,
    fn(left_root, left_result) {
      with_evaluated_bls_expression_v1(
        right,
        fn(right_root, right_result) {
          let arguments = [
            BlsMillerLoopValue(left_root),
            BlsMillerLoopValue(right_root),
          ]
          let Pair(arguments_root, arguments_count) = arguments_root_v1(
            arguments,
          )
          and {
            if enforce_expected_roots {
              and {
                left_root == expected_left_root,
                right_root == expected_right_root,
              }
            } else {
              True
            },
            builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
              70,
              0,
              arguments_count,
              arguments_root,
            ),
            result_is_constant_v1(
              result,
              cek_constant_v1.BooleanConstant,
              bool_data(
                builtin.bls12_381_final_verify(left_result, right_result),
              ),
            ),
          }
        },
      )
    },
  )
}

/// Direct final verification is intentionally a separate entry point because
/// it must reveal the authenticated Miller-loop expression material.
pub fn verify_direct_bls_final_v1(
  builtin_value_root: ByteArray,
  left: BlsExpressionWitnessV1,
  right: BlsExpressionWitnessV1,
  result: ValueWitnessV1,
) -> Bool {
  verify_direct_bls_final_with_expected_roots_v1(
    builtin_value_root,
    #"",
    #"",
    False,
    left,
    right,
    result,
  )
}

/// Consensus transition entry point. Explicit roots let the caller construct
/// the pre-state without traversing and decoding each expression twice; the
/// evaluator still derives and checks both roots while executing the proof.
pub fn verify_direct_bls_final_roots_v1(
  builtin_value_root: ByteArray,
  expected_left_root: ByteArray,
  expected_right_root: ByteArray,
  left: BlsExpressionWitnessV1,
  right: BlsExpressionWitnessV1,
  result: ValueWitnessV1,
) -> Bool {
  expect and {
      bytearray.length(expected_left_root) == 32,
      bytearray.length(expected_right_root) == 32,
    }
  verify_direct_bls_final_with_expected_roots_v1(
    builtin_value_root,
    expected_left_root,
    expected_right_root,
    True,
    left,
    right,
    result,
  )
}

pub const max_direct_builtin_revealed_payload_bytes = 9215

fn revealed_constant_payload_bytes_v1(value: ValueWitnessV1) -> Int {
  when value is {
    ConstantValue(witness) -> bytearray.length(witness.payload_cbor)
    SemanticConstantValue { .. } -> 0
    OpaqueValue(_) -> 0
    BlsMillerLoopValue(_) -> 0
  }
}

fn revealed_argument_payload_bytes_v1(arguments: List<ValueWitnessV1>) -> Int {
  when arguments is {
    [] -> 0
    [argument, ..rest] ->
      revealed_constant_payload_bytes_v1(argument) + revealed_argument_payload_bytes_v1(
        rest,
      )
  }
}

/// Verifies the semantic result of one successful, direct-payload builtin
/// execution. Tags requiring partial cryptographic failure proofs, BLS opaque
/// values, or streaming large-value witnesses remain deliberately closed.
pub fn verify_direct_builtin_v1(
  tag: Int,
  builtin_value_root: ByteArray,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  expect
    revealed_argument_payload_bytes_v1(arguments) + revealed_constant_payload_bytes_v1(
      result,
    ) <= max_direct_builtin_revealed_payload_bytes
  let Pair(arguments_root, arguments_count) = arguments_root_v1(arguments)
  expect
    builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    )
  if tag >= 0 && tag <= 9 {
    verify_integer_binary_v1(tag, arguments, result)
  } else if tag >= 10 && tag <= 20 {
    verify_bytes_v1(tag, arguments, result)
  } else if tag == 21 || tag == 52 || tag == 53 {
    verify_signature_v1(tag, arguments, result)
  } else if tag >= 22 && tag <= 25 {
    verify_strings_v1(tag, arguments, result)
  } else if tag >= 26 && tag <= 28 {
    verify_control_v1(tag, arguments, result)
  } else if tag >= 29 && tag <= 35 {
    verify_pair_and_list_v1(tag, arguments, result)
  } else if tag == 36 {
    verify_choose_data_v1(arguments, result)
  } else if tag >= 37 && tag <= 51 {
    verify_data_v1(tag, arguments, result)
  } else if tag >= 54 && tag <= 60 {
    verify_bls_g1_v1(tag, arguments, result)
  } else if tag >= 61 && tag <= 67 {
    verify_bls_g2_v1(tag, arguments, result)
  } else if tag == 68 || tag == 69 {
    verify_bls_expression_v1(tag, arguments, result)
  } else if tag >= 71 && tag <= 86 {
    verify_v3_bytes_v1(tag, arguments, result)
  } else {
    False
  }
}

/// Verifies a successful builtin against content-addressed semantic constant
/// nodes. Unsupported semantic operations remain closed until they have a
/// bounded multi-step proof; callers must never fall back to the direct path.
pub fn verify_semantic_builtin_v1(
  tag: Int,
  builtin_value_root: ByteArray,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  let Pair(arguments_root, arguments_count) = arguments_root_v1(arguments)
  expect
    builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    )
  if tag == 29 || tag == 30 {
    verify_semantic_pair_v1(tag, arguments, result, witness)
  } else if tag >= 31 && tag <= 35 {
    verify_semantic_list_v1(tag, arguments, result, witness)
  } else if tag == 36 {
    verify_semantic_choose_data_v1(arguments, result, witness)
  } else if tag >= 37 && tag <= 51 {
    verify_semantic_data_v1(tag, arguments, result, witness)
  } else {
    False
  }
}

/// Verifies the zero-budget semantic failures whose result is determined by
/// one authenticated top-level node. The witness must contain exactly the
/// local topology needed to distinguish the failing variant; no whole Data
/// value is revealed.
pub fn verify_semantic_builtin_failure_v1(
  tag: Int,
  builtin_value_root: ByteArray,
  arguments: List<ValueWitnessV1>,
  witness: SemanticBuiltinWitnessV1,
) -> Bool {
  let Pair(arguments_root, arguments_count) = arguments_root_v1(arguments)
  expect
    builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    )
  expect [source] = arguments
  expect [node] = witness.data_nodes
  expect witness.scalar_preimages == []
  let list_summary = exact_list_summary_for_node_v1(node, witness.list_nodes)
  let pair_summary = exact_pair_summary_for_node_v1(node, witness.pair_nodes)
  if tag == 33 || tag == 34 {
    expect witness.pair_nodes == []
    let Pair(_, parts) = semantic_list_source_v1(source, node, list_summary)
    parts.1st.length == 0
  } else if tag >= 42 && tag <= 46 {
    let summary = data_constant_summary_v1(source)
    expect top_node_matches_v1(summary, node, list_summary, pair_summary)
    if tag == 42 {
      when node is {
        cek_data_v1.ConstrSmallData { .. } -> False
        cek_data_v1.ConstrLargeData { .. } -> False
        _ -> True
      }
    } else if tag == 43 {
      when node is {
        cek_data_v1.MapData { .. } -> False
        _ -> True
      }
    } else if tag == 44 {
      when node is {
        cek_data_v1.ListData { .. } -> False
        _ -> True
      }
    } else if tag == 45 {
      when node is {
        cek_data_v1.IntegerData { .. } -> False
        _ -> True
      }
    } else {
      expect tag == 46
      when node is {
        cek_data_v1.BytesData { .. } -> False
        _ -> True
      }
    }
  } else {
    False
  }
}

type RuntimeValueKindV1 {
  AnyRuntimeValue
  IntegerRuntimeValue
  ByteStringRuntimeValue
  StringRuntimeValue
  UnitRuntimeValue
  BooleanRuntimeValue
  ListRuntimeValue
  PairRuntimeValue
  DataRuntimeValue
  BlsG1RuntimeValue
  BlsG2RuntimeValue
  BlsMillerLoopRuntimeValue
  ListDataRuntimeValue
  ListDataPairRuntimeValue
  ListIntegerRuntimeValue
}

fn runtime_value_matches_kind_v1(
  value: RuntimeValueWitnessV1,
  kind: RuntimeValueKindV1,
) -> Bool {
  if kind == AnyRuntimeValue {
    True
  } else {
    when value is {
      RuntimeConstantValue(witness) -> {
        let constant_type = cek_constant_v1.constant_type_v1(witness)
        when kind is {
          IntegerRuntimeValue ->
            constant_type == cek_constant_v1.IntegerConstant
          ByteStringRuntimeValue ->
            constant_type == cek_constant_v1.ByteStringConstant
          StringRuntimeValue -> constant_type == cek_constant_v1.StringConstant
          UnitRuntimeValue -> constant_type == cek_constant_v1.UnitConstant
          BooleanRuntimeValue ->
            constant_type == cek_constant_v1.BooleanConstant
          ListRuntimeValue ->
            when constant_type is {
              cek_constant_v1.ListConstant { .. } -> True
              _ -> False
            }
          PairRuntimeValue ->
            when constant_type is {
              cek_constant_v1.PairConstant { .. } -> True
              _ -> False
            }
          DataRuntimeValue -> constant_type == cek_constant_v1.DataConstant
          BlsG1RuntimeValue -> constant_type == cek_constant_v1.BlsG1Constant
          BlsG2RuntimeValue -> constant_type == cek_constant_v1.BlsG2Constant
          ListDataRuntimeValue ->
            constant_type == cek_constant_v1.ListConstant {
              element: cek_constant_v1.DataConstant,
            }
          ListDataPairRuntimeValue ->
            constant_type == cek_constant_v1.ListConstant {
              element: cek_constant_v1.PairConstant {
                first: cek_constant_v1.DataConstant,
                second: cek_constant_v1.DataConstant,
              },
            }
          ListIntegerRuntimeValue ->
            constant_type == cek_constant_v1.ListConstant {
              element: cek_constant_v1.IntegerConstant,
            }
          AnyRuntimeValue -> True
          BlsMillerLoopRuntimeValue -> False
        }
      }
      RuntimeSemanticConstantValue { type_cbor, payload, memory } -> {
        expect bytearray.length(payload.root) == 32
        expect payload.cbor_length >= 0
        expect payload.memory >= 0
        expect memory >= 0
        let constant_type = cek_constant_v1.decode_constant_type_v1(type_cbor)
        when kind is {
          IntegerRuntimeValue ->
            constant_type == cek_constant_v1.IntegerConstant
          ByteStringRuntimeValue ->
            constant_type == cek_constant_v1.ByteStringConstant
          StringRuntimeValue -> constant_type == cek_constant_v1.StringConstant
          UnitRuntimeValue -> constant_type == cek_constant_v1.UnitConstant
          BooleanRuntimeValue ->
            constant_type == cek_constant_v1.BooleanConstant
          ListRuntimeValue ->
            when constant_type is {
              cek_constant_v1.ListConstant { .. } -> True
              _ -> False
            }
          PairRuntimeValue ->
            when constant_type is {
              cek_constant_v1.PairConstant { .. } -> True
              _ -> False
            }
          DataRuntimeValue -> constant_type == cek_constant_v1.DataConstant
          BlsG1RuntimeValue -> constant_type == cek_constant_v1.BlsG1Constant
          BlsG2RuntimeValue -> constant_type == cek_constant_v1.BlsG2Constant
          ListDataRuntimeValue ->
            constant_type == cek_constant_v1.ListConstant {
              element: cek_constant_v1.DataConstant,
            }
          ListDataPairRuntimeValue ->
            constant_type == cek_constant_v1.ListConstant {
              element: cek_constant_v1.PairConstant {
                first: cek_constant_v1.DataConstant,
                second: cek_constant_v1.DataConstant,
              },
            }
          ListIntegerRuntimeValue ->
            constant_type == cek_constant_v1.ListConstant {
              element: cek_constant_v1.IntegerConstant,
            }
          AnyRuntimeValue -> True
          BlsMillerLoopRuntimeValue -> False
        }
      }
      RuntimeBlsMillerLoopValue { .. } -> kind == BlsMillerLoopRuntimeValue
      _ -> False
    }
  }
}

fn runtime_arguments_match_kinds_v1(
  arguments: List<RuntimeValueWitnessV1>,
  kinds: List<RuntimeValueKindV1>,
) -> Bool {
  when Pair(arguments, kinds) is {
    Pair([], []) -> True
    Pair([argument, ..rest], [kind, ..remaining]) -> and {
        runtime_value_matches_kind_v1(argument, kind),
        runtime_arguments_match_kinds_v1(rest, remaining),
      }
    _ -> False
  }
}

fn revealed_runtime_payload_bytes_v1(
  arguments: List<RuntimeValueWitnessV1>,
) -> Int {
  when arguments is {
    [] -> 0
    [argument, ..rest] -> {
      let revealed = when argument is {
        RuntimeConstantValue(witness) -> bytearray.length(witness.payload_cbor)
        _ -> 0
      }
      revealed + revealed_runtime_payload_bytes_v1(rest)
    }
  }
}

fn mk_cons_arguments_are_well_typed_v1(
  arguments: List<RuntimeValueWitnessV1>,
) -> Bool {
  when arguments is {
    [RuntimeConstantValue(element), RuntimeConstantValue(list_value)] ->
      when cek_constant_v1.constant_type_v1(list_value) is {
        cek_constant_v1.ListConstant { element: element_type } ->
          cek_constant_v1.constant_type_v1(element) == element_type
        _ -> False
      }
    _ -> False
  }
}

fn builtin_argument_kinds_v1(tag: Int) -> List<RuntimeValueKindV1> {
  expect and {
      tag >= 0,
      tag <= cek_proof_v1.max_builtin_tag,
    }
  if tag >= 0 && tag <= 9 {
    [IntegerRuntimeValue, IntegerRuntimeValue]
  } else if tag == 10 {
    [ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 11 {
    [IntegerRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 12 {
    [IntegerRuntimeValue, IntegerRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 13 {
    [ByteStringRuntimeValue]
  } else if tag == 14 {
    [ByteStringRuntimeValue, IntegerRuntimeValue]
  } else if tag >= 15 && tag <= 17 {
    [ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag >= 18 && tag <= 20 {
    [ByteStringRuntimeValue]
  } else if tag == 21 {
    [ByteStringRuntimeValue, ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 22 || tag == 23 {
    [StringRuntimeValue, StringRuntimeValue]
  } else if tag == 24 {
    [StringRuntimeValue]
  } else if tag == 25 {
    [ByteStringRuntimeValue]
  } else if tag == 26 {
    [BooleanRuntimeValue, AnyRuntimeValue, AnyRuntimeValue]
  } else if tag == 27 {
    [UnitRuntimeValue, AnyRuntimeValue]
  } else if tag == 28 {
    [StringRuntimeValue, AnyRuntimeValue]
  } else if tag == 29 || tag == 30 {
    [PairRuntimeValue]
  } else if tag == 31 {
    [ListRuntimeValue, AnyRuntimeValue, AnyRuntimeValue]
  } else if tag == 32 {
    [AnyRuntimeValue, ListRuntimeValue]
  } else if tag >= 33 && tag <= 35 {
    [ListRuntimeValue]
  } else if tag == 36 {
    [
      DataRuntimeValue, AnyRuntimeValue, AnyRuntimeValue, AnyRuntimeValue,
      AnyRuntimeValue, AnyRuntimeValue,
    ]
  } else if tag == 37 {
    [IntegerRuntimeValue, ListDataRuntimeValue]
  } else if tag == 38 {
    [ListDataPairRuntimeValue]
  } else if tag == 39 {
    [ListDataRuntimeValue]
  } else if tag == 40 {
    [IntegerRuntimeValue]
  } else if tag == 41 {
    [ByteStringRuntimeValue]
  } else if tag >= 42 && tag <= 46 {
    [DataRuntimeValue]
  } else if tag == 47 || tag == 48 {
    [DataRuntimeValue, DataRuntimeValue]
  } else if tag == 49 || tag == 50 {
    [UnitRuntimeValue]
  } else if tag == 51 {
    [DataRuntimeValue]
  } else if tag == 52 || tag == 53 {
    [ByteStringRuntimeValue, ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 54 || tag == 57 {
    [BlsG1RuntimeValue, BlsG1RuntimeValue]
  } else if tag == 55 || tag == 59 {
    [BlsG1RuntimeValue]
  } else if tag == 56 {
    [IntegerRuntimeValue, BlsG1RuntimeValue]
  } else if tag == 58 {
    [ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 60 {
    [ByteStringRuntimeValue]
  } else if tag == 61 || tag == 64 {
    [BlsG2RuntimeValue, BlsG2RuntimeValue]
  } else if tag == 62 || tag == 66 {
    [BlsG2RuntimeValue]
  } else if tag == 63 {
    [IntegerRuntimeValue, BlsG2RuntimeValue]
  } else if tag == 65 {
    [ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 67 {
    [ByteStringRuntimeValue]
  } else if tag == 68 {
    [BlsG1RuntimeValue, BlsG2RuntimeValue]
  } else if tag == 69 || tag == 70 {
    [BlsMillerLoopRuntimeValue, BlsMillerLoopRuntimeValue]
  } else if tag == 71 || tag == 72 {
    [ByteStringRuntimeValue]
  } else if tag == 73 {
    [BooleanRuntimeValue, IntegerRuntimeValue, IntegerRuntimeValue]
  } else if tag == 74 {
    [BooleanRuntimeValue, ByteStringRuntimeValue]
  } else if tag >= 75 && tag <= 77 {
    [BooleanRuntimeValue, ByteStringRuntimeValue, ByteStringRuntimeValue]
  } else if tag == 78 {
    [ByteStringRuntimeValue]
  } else if tag == 79 {
    [ByteStringRuntimeValue, IntegerRuntimeValue]
  } else if tag == 80 {
    [ByteStringRuntimeValue, ListIntegerRuntimeValue, BooleanRuntimeValue]
  } else if tag == 81 {
    [IntegerRuntimeValue, IntegerRuntimeValue]
  } else if tag == 82 || tag == 83 {
    [ByteStringRuntimeValue, IntegerRuntimeValue]
  } else {
    expect tag >= 84 && tag <= 86
    [ByteStringRuntimeValue]
  }
}

fn builtin_arguments_are_well_typed_v1(
  tag: Int,
  arguments: List<RuntimeValueWitnessV1>,
) -> Bool {
  if tag == 32 {
    mk_cons_arguments_are_well_typed_v1(arguments)
  } else {
    runtime_arguments_match_kinds_v1(arguments, builtin_argument_kinds_v1(tag))
  }
}

/// Authenticates a CEK builtin failure caused by runtime argument types.
/// The reference CEK rejects these before consulting the builtin cost model,
/// so the corresponding transition must add zero CPU and memory.
pub fn verify_builtin_type_failure_v1(
  tag: Int,
  builtin_value_root: ByteArray,
  arguments: List<RuntimeValueWitnessV1>,
) -> Bool {
  expect
    revealed_runtime_payload_bytes_v1(arguments) <= max_direct_builtin_revealed_payload_bytes
  let Pair(arguments_root, arguments_count) = runtime_arguments_root_v1(
    arguments,
  )
  and {
    arguments_count == cek_cost_v1.builtin_argument_count_v1(tag),
    builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
    !builtin_arguments_are_well_typed_v1(tag, arguments),
  }
}

fn continuation_byte_v1(value: Int) -> Bool {
  and {
    value >= 0x80,
    value <= 0xbf,
  }
}

fn utf8_tail_v1(bytes: ByteArray, index: Int, required: Int) -> Bool {
  if required == 0 {
    is_valid_utf8_from_v1(bytes, index)
  } else {
    and {
      index < bytearray.length(bytes),
      continuation_byte_v1(bytearray.at(bytes, index)),
      utf8_tail_v1(bytes, index + 1, required - 1),
    }
  }
}

fn is_valid_utf8_from_v1(bytes: ByteArray, index: Int) -> Bool {
  let length = bytearray.length(bytes)
  if index == length {
    True
  } else if index > length {
    False
  } else {
    let first = bytearray.at(bytes, index)
    if first <= 0x7f {
      is_valid_utf8_from_v1(bytes, index + 1)
    } else if and {
      first >= 0xc2,
      first <= 0xdf,
    } {
      utf8_tail_v1(bytes, index + 1, 1)
    } else if index + 1 >= length {
      False
    } else {
      let second = bytearray.at(bytes, index + 1)
      if first == 0xe0 {
        and {
          second >= 0xa0,
          second <= 0xbf,
          utf8_tail_v1(bytes, index + 2, 1),
        }
      } else if first == 0xed {
        and {
          second >= 0x80,
          second <= 0x9f,
          utf8_tail_v1(bytes, index + 2, 1),
        }
      } else if or {
        and {
          first >= 0xe1,
          first <= 0xec,
        },
        and {
          first >= 0xee,
          first <= 0xef,
        },
      } {
        and {
          continuation_byte_v1(second),
          utf8_tail_v1(bytes, index + 2, 1),
        }
      } else if first == 0xf0 {
        and {
          second >= 0x90,
          second <= 0xbf,
          utf8_tail_v1(bytes, index + 2, 2),
        }
      } else if and {
        first >= 0xf1,
        first <= 0xf3,
      } {
        and {
          continuation_byte_v1(second),
          utf8_tail_v1(bytes, index + 2, 2),
        }
      } else if first == 0xf4 {
        and {
          second >= 0x80,
          second <= 0x8f,
          utf8_tail_v1(bytes, index + 2, 2),
        }
      } else {
        False
      }
    }
  }
}

pub fn is_valid_utf8_v1(bytes: ByteArray) -> Bool {
  is_valid_utf8_from_v1(bytes, 0)
}

fn unsigned_integer_byte_size_v1(value: Int) -> Int {
  if value == 0 {
    0
  } else if value < 256 {
    1
  } else {
    1 + unsigned_integer_byte_size_v1(value / 256)
  }
}

fn integer_to_bytes_fails_v1(arguments: List<ValueWitnessV1>) -> Bool {
  expect [endian, size_value, integer_value] = arguments
  let endian_value = boolean_v1(endian)
  expect or {
      endian_value,
      !endian_value,
    }
  let size = integer_v1(size_value)
  let integer = integer_v1(integer_value)
  let required = if integer < 0 {
    0
  } else {
    unsigned_integer_byte_size_v1(integer)
  }
  or {
    size < 0,
    size > 8192,
    integer < 0,
    required > 8192,
    and {
      size > 0,
      required > size,
    },
  }
}

fn wrong_data_variant_v1(tag: Int, arguments: List<ValueWitnessV1>) -> Bool {
  expect [source] = arguments
  let data = data_payload_v1(source)
  if tag == 42 {
    !data_is_constr(data)
  } else if tag == 43 {
    !data_is_map(data)
  } else if tag == 44 {
    !data_is_list(data)
  } else if tag == 45 {
    !data_is_integer(data)
  } else {
    expect tag == 46
    !data_is_bytes(data)
  }
}

const secp256k1_field_prime =
  115792089237316195423570985008687907853269984665640564039457584007908834671663

fn modular_power_go_v1(
  current: Int,
  remaining: Int,
  accumulator: Int,
  modulus: Int,
) -> Int {
  if remaining == 0 {
    accumulator
  } else {
    modular_power_go_v1(
      current * current % modulus,
      remaining / 2,
      if remaining % 2 == 1 {
        accumulator * current % modulus
      } else {
        accumulator
      },
      modulus,
    )
  }
}

fn modular_power_v1(base: Int, exponent: Int, modulus: Int) -> Int {
  expect and {
      exponent >= 0,
      modulus > 1,
    }
  modular_power_go_v1(base % modulus, exponent, 1, modulus)
}

fn secp256k1_x_is_on_curve_v1(x: Int) -> Bool {
  if or {
    x < 0,
    x >= secp256k1_field_prime,
  } {
    False
  } else {
    let y_squared = ( x * x * x + 7 ) % secp256k1_field_prime
    or {
      y_squared == 0,
      modular_power_v1(
        y_squared,
        ( secp256k1_field_prime - 1 ) / 2,
        secp256k1_field_prime,
      ) == 1,
    }
  }
}

fn secp256k1_public_key_is_valid_v1(tag: Int, key: ByteArray) -> Bool {
  if tag == 52 {
    and {
      bytearray.length(key) == 33,
      or {
        bytearray.at(key, 0) == 2,
        bytearray.at(key, 0) == 3,
      },
      secp256k1_x_is_on_curve_v1(
        builtin.bytearray_to_integer(True, builtin.slice_bytearray(1, 32, key)),
      ),
    }
  } else {
    expect tag == 53
    and {
      bytearray.length(key) == 32,
      secp256k1_x_is_on_curve_v1(builtin.bytearray_to_integer(True, key)),
    }
  }
}

const bls12_381_field_prime =
  4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787

fn bls_compressed_coordinate_v1(encoded: ByteArray, offset: Int) -> Int {
  let first = bytearray.at(encoded, offset)
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(
      first % 32,
      builtin.slice_bytearray(offset + 1, 47, encoded),
    ),
  )
}

fn bls_uncompress_header_is_invalid_v1(tag: Int, encoded: ByteArray) -> Bool {
  let expected_length = if tag == 60 {
    48
  } else {
    expect tag == 67
    96
  }
  expect bytearray.length(encoded) == expected_length
  let first = bytearray.at(encoded, 0)
  let compressed = first >= 128
  let infinity = first % 128 >= 64
  let sort = first % 64 >= 32
  let first_coordinate = bls_compressed_coordinate_v1(encoded, 0)
  let second_coordinate = if tag == 67 {
    builtin.bytearray_to_integer(True, builtin.slice_bytearray(48, 48, encoded))
  } else {
    0
  }
  or {
    !compressed,
    if infinity {
      or {
        sort,
        first_coordinate != 0,
        second_coordinate != 0,
      }
    } else {
      or {
        first_coordinate >= bls12_381_field_prime,
        second_coordinate >= bls12_381_field_prime,
      }
    },
  }
}

fn bls_g1_curve_encoding_is_invalid_v1(encoded: ByteArray) -> Bool {
  expect bytearray.length(encoded) == 48
  let first = bytearray.at(encoded, 0)
  let infinity = first % 128 >= 64
  if infinity {
    False
  } else {
    let x = bls_compressed_coordinate_v1(encoded, 0)
    let y_squared = ( x * x * x + 4 ) % bls12_381_field_prime
    and {
      y_squared != 0,
      modular_power_v1(
        y_squared,
        ( bls12_381_field_prime - 1 ) / 2,
        bls12_381_field_prime,
      ) != 1,
    }
  }
}

fn malformed_signature_lengths_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
) -> Bool {
  expect [key_value, message_value, signature_value] = arguments
  let key_length = bytearray.length(bytes_v1(key_value))
  let message_length = bytearray.length(bytes_v1(message_value))
  let signature_length = bytearray.length(bytes_v1(signature_value))
  if tag == 21 {
    or {
      key_length != 32,
      signature_length != 64,
    }
  } else if tag == 52 {
    if or {
      key_length != 33,
      message_length != 32,
      signature_length != 64,
    } {
      True
    } else {
      !secp256k1_public_key_is_valid_v1(tag, bytes_v1(key_value))
    }
  } else {
    expect tag == 53
    if or {
      key_length != 32,
      message_length != 32,
      signature_length != 64,
    } {
      True
    } else {
      !secp256k1_public_key_is_valid_v1(tag, bytes_v1(key_value))
    }
  }
}

fn known_builtin_failure_v1(tag: Int, arguments: List<ValueWitnessV1>) -> Bool {
  if and {
    tag >= 3,
    tag <= 6,
  } {
    expect [_, divisor] = arguments
    integer_v1(divisor) == 0
  } else if tag == 11 {
    expect [byte, _] = arguments
    let value = integer_v1(byte)
    or {
      value < 0,
      value > 255,
    }
  } else if tag == 14 {
    expect [source, index] = arguments
    let position = integer_v1(index)
    or {
      position < 0,
      position >= bytearray.length(bytes_v1(source)),
      position > 9223372036854775807,
    }
  } else if tag == 21 {
    malformed_signature_lengths_v1(tag, arguments)
  } else if tag == 25 {
    expect [source] = arguments
    !is_valid_utf8_v1(bytes_v1(source))
  } else if or {
    tag == 33,
    tag == 34,
  } {
    expect [source] = arguments
    let Pair(_, items) = list_payload_v1(source)
    list.is_empty(items)
  } else if and {
    tag >= 42,
    tag <= 46,
  } {
    wrong_data_variant_v1(tag, arguments)
  } else if or {
    tag == 52,
    tag == 53,
  } {
    malformed_signature_lengths_v1(tag, arguments)
  } else if or {
    tag == 58,
    tag == 65,
  } {
    expect [_, domain] = arguments
    bytearray.length(bytes_v1(domain)) > 255
  } else if tag == 60 {
    expect [source] = arguments
    let encoded = bytes_v1(source)
    or {
      bytearray.length(encoded) != 48,
      if bytearray.length(encoded) == 48 {
        let header_invalid = bls_uncompress_header_is_invalid_v1(tag, encoded)
        or {
          header_invalid,
          if !header_invalid {
            bls_g1_curve_encoding_is_invalid_v1(encoded)
          } else {
            False
          },
        }
      } else {
        False
      },
    }
  } else if tag == 67 {
    expect [source] = arguments
    let encoded = bytes_v1(source)
    or {
      bytearray.length(encoded) != 96,
      if bytearray.length(encoded) == 96 {
        bls_uncompress_header_is_invalid_v1(tag, encoded)
      } else {
        False
      },
    }
  } else if tag == 73 {
    integer_to_bytes_fails_v1(arguments)
  } else if tag == 79 {
    expect [source, index] = arguments
    let position = integer_v1(index)
    or {
      position < 0,
      position >= bytearray.length(bytes_v1(source)) * 8,
    }
  } else if tag == 80 {
    expect [source, indices, _] = arguments
    let Pair(_, index_data) = list_payload_v1(indices)
    !all_bit_indices_valid_v1(
      semantic_integers_v1(index_data),
      bytearray.length(bytes_v1(source)) * 8,
    )
  } else if tag == 81 {
    expect [length_value, byte_value] = arguments
    let length = integer_v1(length_value)
    let byte = integer_v1(byte_value)
    or {
      length < 0,
      length > 8192,
      byte < 0,
      byte > 255,
    }
  } else {
    False
  }
}

pub fn verify_direct_builtin_failure_v1(
  tag: Int,
  builtin_value_root: ByteArray,
  arguments: List<ValueWitnessV1>,
) -> Bool {
  expect
    revealed_argument_payload_bytes_v1(arguments) <= max_direct_builtin_revealed_payload_bytes
  let Pair(arguments_root, arguments_count) = arguments_root_v1(arguments)
  and {
    arguments_count == cek_cost_v1.builtin_argument_count_v1(tag),
    builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
    known_builtin_failure_v1(tag, arguments),
  }
}

pub fn direct_builtin_failure_budget_v1(
  tag: Int,
  arguments: List<ValueWitnessV1>,
) -> cek_cost_v1.BuiltinBudgetV1 {
  expect known_builtin_failure_v1(tag, arguments)
  let bls_uncompress_is_paid = if tag == 60 {
    expect [source] = arguments
    bytearray.length(bytes_v1(source)) == 48
  } else if tag == 67 {
    expect [source] = arguments
    bytearray.length(bytes_v1(source)) == 96
  } else {
    False
  }
  if or {
    tag == 4,
    tag == 5,
    tag == 6,
    tag == 52,
    tag == 53,
    tag == 58,
    tag == 65,
    tag == 73,
    bls_uncompress_is_paid,
  } {
    direct_builtin_budget_v1(tag, arguments)
  } else {
    cek_cost_v1.BuiltinBudgetV1 { cpu: 0, memory: 0 }
  }
}

pub fn result_root_v1(result: ValueWitnessV1) -> ByteArray {
  value_root_v1(result)
}
