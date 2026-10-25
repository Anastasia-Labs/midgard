//// Transaction-level rows for the Data scalar semantics step (arm 31, group 5,
//// `cek_core_semantic_data_scalar`): listData, iData, bData, unListData,
//// unIData and unBData. A Data leaf and the integer or byte string it holds
//// share one payload summary, so iData, bData, unIData and unBData are proved
//// from the leaf node alone, at any size, and the step must accept exactly
//// the results Cardano returns.
////
//// The rows past the direct carriage bound (9,215 bytes) also carry the
//// result on: the unBData and unIData results bind as operands of the staged
//// byte-string and integer steps, and iData and bData round-trip through
//// unIData and unBData to the same value.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_builtin_roots as builtin_roots
use fraud_proofs/validation_trace/cek_core_semantic_data_misc as data_misc
use fraud_proofs/validation_trace/cek_core_semantic_data_scalar as data_scalar
use fraud_proofs/validation_trace/cek_core_staged_bytes as staged_bytes
use fraud_proofs/validation_trace/cek_core_staged_integer as staged_integer
use midgard/cek_builtin_v1.{
  SemanticBuiltinWitnessV1, SemanticConstantValue, ValueWitnessV1,
}
use midgard/cek_constant_v1
use midgard/cek_data_v1.{
  BytesData, DataListNodeV1, DataNodeV1, DataPairNodeV1, DataSummaryV1,
  IntegerData, ListData,
}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as chain
use midgard/cek_semantic_presentation_evidence_generated.{
  i_data_evidence, list_data_evidence, un_b_data_evidence, un_list_data_evidence,
}
use midgard/cek_staged_arm_v1.{
  BytesLeafOperand, InlineOperand, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{flat_root_of_pieces_v1, flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{
  encode_integer_cbor_bulk_v1, parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, integer_leaf_from_cbor_v1, magnitude_words_v1,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, boolean_value, bytes_type_cbor, bytes_value, compact,
  flip_last_byte, inline_constant, integer_type_cbor, integer_value, material,
  no_staging, pattern, semantic_bytes, semantic_integer_cbor, semantic_pieces,
  stage, staged, staged_step, string_type_cbor, text,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## The step

const data_type_cbor = #"9f08ff"

const ct_asset = "cek-core-data-scalar"

fn bound_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness_material: SemanticBuiltinWitnessV1,
  focus_root: ByteArray,
  progress: Int,
  facts: Data,
  next_facts: Data,
) -> StagedStepV1 {
  let arguments_data: Data = arguments
  let result_data: Data = result
  let material_data: Data = witness_material
  let witness = builtin.constr_data(
    31,
    [builtin.i_data(tag), arguments_data, result_data, material_data],
  )
  let state = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root,
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let bound = chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: state,
    post: state,
    witness_hash: blake2b_256(cbor.serialise(witness)),
    arm: 31,
    group: 5,
    progress,
    facts,
  }
  let next: Data = chain.CoreBound {
    ..bound,
    progress: progress + 1,
    facts: next_facts,
  }
  StagedStepV1 {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      chain.VerifyArm { input_index: 0, output_index: 0, witness },
    ),
    transaction: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(ct_asset, next),
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

/// The semantics hop of builtin `tag`, with the compacted values the roots
/// hop commits.
fn scalar_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness_material: SemanticBuiltinWitnessV1,
) -> StagedStepV1 {
  let facts: Data = chain.SemanticRoots {
    roots: chain.BuiltinRoots {
      arguments_root: #"",
      arguments_count: list.length(arguments),
      result_root: #"",
      builtin_root: #"",
    },
    arguments: list.map(arguments, compact),
    result: compact(result),
  }
  bound_step(tag, arguments, result, witness_material, #"", 3, facts, facts)
}

fn verifies(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  witness_material: SemanticBuiltinWitnessV1,
) -> Bool {
  let step = scalar_step(tag, arguments, result, witness_material)
  data_scalar.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// The roots hop of builtin `tag` over `arguments`, for a pre-state whose
/// focus is the builtin over `committed`.
fn roots_verify(
  tag: Int,
  committed: List<ValueWitnessV1>,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> Bool {
  let Pair(committed_root, committed_count) = cek_builtin_v1.arguments_root_v1(
    list.map(committed, compact),
  )
  let focus_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    committed_count,
    committed_root,
  )
  let compacted = list.map(arguments, compact)
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    compacted,
  )
  let next_facts: Data = chain.SemanticArguments {
    arguments_root,
    arguments_count,
    builtin_root: cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
    arguments: compacted,
  }
  let step = bound_step(
    tag,
    arguments,
    result,
    leaf(empty_list_node()),
    focus_root,
    0,
    builtin.constr_data(0, []),
    next_facts,
  )
  builtin_roots.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn staged_spend(integer: Bool, step: StagedStepV1) -> Bool {
  if integer {
    staged_integer.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      step.datum,
      step.redeemer,
      fixture.own_out_ref(),
      step.transaction,
    )
  } else {
    staged_bytes.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      step.datum,
      step.redeemer,
      fixture.own_out_ref(),
      step.transaction,
    )
  }
}

// ## Leaves and values

fn summary_of(node: DataNodeV1) -> DataSummaryV1 {
  DataSummaryV1 {
    root: cek_data_v1.hash_data_node_v1(node),
    cbor_length: cek_data_v1.data_node_cbor_length_v1(node),
    memory: cek_data_v1.data_node_memory_v1(node),
  }
}

/// The Data value whose payload is `node`.
fn data_of(node: DataNodeV1) -> ValueWitnessV1 {
  let payload = summary_of(node)
  SemanticConstantValue {
    type_cbor: data_type_cbor,
    payload,
    memory: payload.memory,
  }
}

/// The B leaf of the byte string given as `pieces`.
fn bytes_node(pieces: List<ByteArray>) -> DataNodeV1 {
  let length = list.foldl(
    pieces,
    0,
    fn(piece, total) { total + builtin.length_of_bytearray(piece) },
  )
  BytesData {
    bytes_root: flat_root_of_pieces_v1(pieces),
    bytes_length: length,
    cbor_length: cek_data_v1.bytes_data_cbor_length_v1(length),
    memory: 4 + cek_data_v1.bytes_memory_size_v1(length),
  }
}

/// The I leaf of the integer whose CBOR is `integer_cbor`.
fn integer_node(integer_cbor: ByteArray) -> DataNodeV1 {
  IntegerData {
    cbor_root: flat_root_v1(integer_cbor),
    cbor_length: builtin.length_of_bytearray(integer_cbor),
    memory: 4 + magnitude_words_v1(parse_integer_cbor_bulk_v1(integer_cbor)),
  }
}

fn empty_list_node() -> DataNodeV1 {
  ListData {
    items_count: 0,
    items_root: cek_data_v1.empty_data_list_root_v1(),
    cbor_length: 1,
    memory: 4,
  }
}

/// The material naming only the leaf.
fn leaf(node: DataNodeV1) -> SemanticBuiltinWitnessV1 {
  SemanticBuiltinWitnessV1 {
    data_nodes: [node],
    list_nodes: [],
    pair_nodes: [],
    scalar_preimages: [],
  }
}

fn with_memory(value: ValueWitnessV1, delta: Int) -> ValueWitnessV1 {
  expect SemanticConstantValue { type_cbor, payload, memory } = value
  SemanticConstantValue { type_cbor, payload, memory: memory + delta }
}

fn with_type(value: ValueWitnessV1, type_cbor: ByteArray) -> ValueWitnessV1 {
  expect SemanticConstantValue { payload, memory, .. } = value
  SemanticConstantValue { type_cbor, payload, memory }
}

/// A value of type `type_cbor` over the payload of `node`, with memory
/// `payload.memory - 4`.
fn scalar_of(node: DataNodeV1, type_cbor: ByteArray) -> ValueWitnessV1 {
  let payload = summary_of(node)
  SemanticConstantValue { type_cbor, payload, memory: payload.memory - 4 }
}

/// 12,000 bytes, past the direct carriage bound.
fn long_bytes() -> ByteArray {
  pattern(12000, 7)
}

/// A positive integer of 12,000 magnitude bytes, whose CBOR is past the
/// direct carriage bound.
fn long_integer() -> Int {
  builtin.bytearray_to_integer(True, pattern(12000, 3))
}

/// A negative integer of 12,000 magnitude bytes.
fn long_negative() -> Int {
  -1 - builtin.bytearray_to_integer(True, pattern(12000, 5))
}

// ## unBData

test accepts_un_b_data_of_a_12000_byte_leaf() {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(46, [data_of(node)], semantic_bytes(bytes, False), leaf(node))
}

test accepts_un_b_data_of_a_70000_byte_leaf() {
  let pieces = [pattern(65536, 1), pattern(4464, 2)]
  let node = bytes_node(pieces)
  verifies(46, [data_of(node)], semantic_pieces(pieces, False), leaf(node))
}

test accepts_un_b_data_of_small_leaves() {
  let check = fn(bytes) {
    let node = bytes_node([bytes])
    verifies(
      46,
      [inline_constant(data_type_cbor, builtin.b_data(bytes))],
      bytes_value(bytes, False),
      leaf(node),
    )
  }
  and {
    check(#""),
    check(#"00"),
    check(pattern(64, 1)),
    check(pattern(65, 1)),
    check(pattern(4000, 1)),
  }
}

test the_leaf_and_the_byte_string_share_one_summary() {
  let bytes = long_bytes()
  expect SemanticConstantValue { payload, memory, .. } = semantic_bytes(
    bytes,
    False,
  )
  and {
    payload == summary_of(bytes_node([bytes])),
    memory == payload.memory - 4,
  }
}

test refuses_an_un_b_data_result_of_other_bytes() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(flip_last_byte(bytes), False),
    leaf(node),
  )
}

test refuses_an_un_b_data_result_one_word_heavier() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    with_memory(semantic_bytes(bytes, False), 1),
    leaf(node),
  )
}

test refuses_an_un_b_data_result_one_word_lighter() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    with_memory(semantic_bytes(bytes, False), -1),
    leaf(node),
  )
}

test refuses_an_un_b_data_result_typed_as_a_string() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    with_type(semantic_bytes(bytes, False), string_type_cbor),
    leaf(node),
  )
}

test refuses_an_un_b_data_result_typed_as_data() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(46, [data_of(node)], data_of(node), leaf(node))
}

test refuses_un_b_data_given_the_leaf_of_other_bytes() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(bytes, False),
    leaf(bytes_node([flip_last_byte(bytes)])),
  )
}

test refuses_un_b_data_with_a_revealed_scalar() fail {
  let bytes = pattern(100, 1)
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(bytes, False),
    SemanticBuiltinWitnessV1 { ..leaf(node), scalar_preimages: [bytes] },
  )
}

test refuses_un_b_data_with_a_second_data_node() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(bytes, False),
    SemanticBuiltinWitnessV1 { ..leaf(node), data_nodes: [node, node] },
  )
}

test refuses_un_b_data_with_no_data_node() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(bytes, False),
    SemanticBuiltinWitnessV1 { ..leaf(node), data_nodes: [] },
  )
}

fn some_list_node(node: DataNodeV1) -> DataListNodeV1 {
  let summary = summary_of(node)
  DataListNodeV1 {
    head: summary.root,
    head_cbor_length: summary.cbor_length,
    head_memory: summary.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: summary.cbor_length,
    memory: summary.memory,
  }
}

fn some_pair_node(node: DataNodeV1) -> DataPairNodeV1 {
  let summary = summary_of(node)
  DataPairNodeV1 {
    key: summary.root,
    key_cbor_length: summary.cbor_length,
    key_memory: summary.memory,
    value: summary.root,
    value_cbor_length: summary.cbor_length,
    value_memory: summary.memory,
    tail: cek_data_v1.empty_data_pair_root_v1(),
    length: 1,
    payload_cbor_length: 2 * summary.cbor_length,
    memory: 2 * summary.memory,
  }
}

test refuses_un_b_data_with_a_list_node() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(bytes, False),
    SemanticBuiltinWitnessV1 {
      ..leaf(node),
      list_nodes: [some_list_node(node)],
    },
  )
}

test refuses_un_b_data_with_a_pair_node() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [data_of(node)],
    semantic_bytes(bytes, False),
    SemanticBuiltinWitnessV1 {
      ..leaf(node),
      pair_nodes: [some_pair_node(node)],
    },
  )
}

test refuses_un_b_data_over_an_integer_leaf() fail {
  let node = integer_node(cbor.serialise(long_integer()))
  verifies(46, [data_of(node)], scalar_of(node, bytes_type_cbor), leaf(node))
}

test refuses_un_b_data_over_an_empty_list() fail {
  let node = empty_list_node()
  verifies(46, [data_of(node)], scalar_of(node, bytes_type_cbor), leaf(node))
}

test refuses_un_b_data_of_a_byte_string() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [semantic_bytes(bytes, False)],
    semantic_bytes(bytes, False),
    leaf(node),
  )
}

/// A string of 36 bytes weighs 9 words, as much as the B leaf of the same
/// bytes, and its payload is that leaf: only the source's type separates it
/// from a Data value the step accepts.
test a_36_byte_string_weighs_as_much_as_the_leaf_of_its_bytes() {
  let bytes = text(36)
  let source = bytes_value(bytes, True)
  let node = bytes_node([bytes])
  and {
    cek_builtin_v1.semantic_constant_type_v1(source) == cek_constant_v1.StringConstant,
    cek_builtin_v1.semantic_constant_memory_v1(source) == 9,
    cek_builtin_v1.semantic_constant_payload_v1(source) == summary_of(node),
    summary_of(node).memory == 9,
    cek_builtin_v1.semantic_constant_memory_v1(data_of(node)) == 9,
  }
}

test refuses_un_b_data_of_a_36_byte_string() fail {
  let bytes = text(36)
  verifies(
    46,
    [bytes_value(bytes, True)],
    bytes_value(bytes, False),
    leaf(bytes_node([bytes])),
  )
}

test refuses_un_b_data_of_a_source_one_word_heavier() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    46,
    [with_memory(data_of(node), 1)],
    semantic_bytes(bytes, False),
    leaf(node),
  )
}

// ## unIData

test accepts_un_i_data_of_a_12000_byte_positive_leaf() {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(45, [data_of(node)], semantic_integer_cbor(integer_cbor), leaf(node))
}

test accepts_un_i_data_of_a_12000_byte_negative_leaf() {
  let integer_cbor = cbor.serialise(long_negative())
  let node = integer_node(integer_cbor)
  verifies(45, [data_of(node)], semantic_integer_cbor(integer_cbor), leaf(node))
}

test accepts_un_i_data_of_small_leaves() {
  let check = fn(value) {
    let node = integer_node(cbor.serialise(value))
    verifies(
      45,
      [inline_constant(data_type_cbor, builtin.i_data(value))],
      integer_value(value),
      leaf(node),
    )
  }
  and {
    check(0),
    check(-1),
    check(23),
    check(18446744073709551615),
    check(18446744073709551616),
    check(-18446744073709551616),
    check(-18446744073709551617),
  }
}

test the_leaf_and_the_integer_share_one_summary() {
  let integer_cbor = cbor.serialise(long_negative())
  expect SemanticConstantValue { payload, memory, .. } = semantic_integer_cbor(
    integer_cbor,
  )
  and {
    builtin.length_of_bytearray(integer_cbor) > 9215,
    payload == summary_of(integer_node(integer_cbor)),
    memory == payload.memory - 4,
  }
}

test refuses_an_un_i_data_result_of_another_integer() fail {
  let node = integer_node(cbor.serialise(long_integer()))
  verifies(
    45,
    [data_of(node)],
    semantic_integer_cbor(cbor.serialise(long_integer() + 1)),
    leaf(node),
  )
}

test refuses_an_un_i_data_result_one_word_heavier() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [data_of(node)],
    with_memory(semantic_integer_cbor(integer_cbor), 1),
    leaf(node),
  )
}

test refuses_an_un_i_data_result_one_word_lighter() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [data_of(node)],
    with_memory(semantic_integer_cbor(integer_cbor), -1),
    leaf(node),
  )
}

test refuses_an_un_i_data_result_typed_as_a_byte_string() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [data_of(node)],
    with_type(semantic_integer_cbor(integer_cbor), bytes_type_cbor),
    leaf(node),
  )
}

test refuses_un_i_data_given_the_leaf_of_another_integer() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [data_of(node)],
    semantic_integer_cbor(integer_cbor),
    leaf(integer_node(cbor.serialise(long_integer() + 1))),
  )
}

test refuses_un_i_data_with_a_revealed_scalar() fail {
  let integer_cbor = cbor.serialise(18446744073709551616)
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [data_of(node)],
    semantic_integer_cbor(integer_cbor),
    SemanticBuiltinWitnessV1 { ..leaf(node), scalar_preimages: [integer_cbor] },
  )
}

test refuses_un_i_data_with_a_list_node() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [data_of(node)],
    semantic_integer_cbor(integer_cbor),
    SemanticBuiltinWitnessV1 {
      ..leaf(node),
      list_nodes: [some_list_node(node)],
    },
  )
}

test refuses_un_i_data_over_a_bytes_leaf() fail {
  let node = bytes_node([long_bytes()])
  verifies(45, [data_of(node)], scalar_of(node, integer_type_cbor), leaf(node))
}

test refuses_un_i_data_of_a_source_one_word_lighter() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    45,
    [with_memory(data_of(node), -1)],
    semantic_integer_cbor(integer_cbor),
    leaf(node),
  )
}

// ## iData

test accepts_i_data_of_a_12000_byte_integer() {
  let integer_cbor = cbor.serialise(long_negative())
  let node = integer_node(integer_cbor)
  verifies(40, [semantic_integer_cbor(integer_cbor)], data_of(node), leaf(node))
}

test accepts_i_data_of_small_integers() {
  let check = fn(value) {
    let node = integer_node(cbor.serialise(value))
    verifies(
      40,
      [integer_value(value)],
      inline_constant(data_type_cbor, builtin.i_data(value)),
      leaf(node),
    )
  }
  and {
    check(0),
    check(-24),
    check(18446744073709551616),
    check(-18446744073709551617),
  }
}

test refuses_an_i_data_result_of_another_integer() fail {
  let integer_cbor = cbor.serialise(long_integer())
  verifies(
    40,
    [semantic_integer_cbor(integer_cbor)],
    data_of(integer_node(cbor.serialise(long_integer() + 1))),
    leaf(integer_node(integer_cbor)),
  )
}

test refuses_an_i_data_result_one_word_heavier() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    40,
    [semantic_integer_cbor(integer_cbor)],
    with_memory(data_of(node), 1),
    leaf(node),
  )
}

test refuses_an_i_data_result_typed_as_an_integer() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    40,
    [semantic_integer_cbor(integer_cbor)],
    with_type(data_of(node), integer_type_cbor),
    leaf(node),
  )
}

test refuses_i_data_of_an_argument_one_word_heavier() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    40,
    [with_memory(semantic_integer_cbor(integer_cbor), 1)],
    data_of(node),
    leaf(node),
  )
}

test refuses_i_data_of_an_argument_typed_as_a_byte_string() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    40,
    [with_type(semantic_integer_cbor(integer_cbor), bytes_type_cbor)],
    data_of(node),
    leaf(node),
  )
}

test refuses_i_data_of_a_byte_string() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(40, [scalar_of(node, integer_type_cbor)], data_of(node), leaf(node))
}

test refuses_i_data_given_the_leaf_of_another_integer() fail {
  let integer_cbor = cbor.serialise(long_integer())
  verifies(
    40,
    [semantic_integer_cbor(integer_cbor)],
    data_of(integer_node(integer_cbor)),
    leaf(integer_node(cbor.serialise(long_integer() + 1))),
  )
}

test refuses_i_data_with_a_revealed_scalar() fail {
  let integer_cbor = cbor.serialise(18446744073709551616)
  let node = integer_node(integer_cbor)
  verifies(
    40,
    [semantic_integer_cbor(integer_cbor)],
    data_of(node),
    SemanticBuiltinWitnessV1 { ..leaf(node), scalar_preimages: [integer_cbor] },
  )
}

test refuses_i_data_with_a_pair_node() fail {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  verifies(
    40,
    [semantic_integer_cbor(integer_cbor)],
    data_of(node),
    SemanticBuiltinWitnessV1 {
      ..leaf(node),
      pair_nodes: [some_pair_node(node)],
    },
  )
}

// ## bData

test accepts_b_data_of_a_12000_byte_string() {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(41, [semantic_bytes(bytes, False)], data_of(node), leaf(node))
}

test accepts_b_data_of_small_byte_strings() {
  let check = fn(bytes) {
    let node = bytes_node([bytes])
    verifies(
      41,
      [bytes_value(bytes, False)],
      inline_constant(data_type_cbor, builtin.b_data(bytes)),
      leaf(node),
    )
  }
  and {
    check(#""),
    check(pattern(64, 9)),
    check(pattern(65, 9)),
    check(pattern(4000, 9)),
  }
}

test refuses_a_b_data_result_of_other_bytes() fail {
  let bytes = long_bytes()
  verifies(
    41,
    [semantic_bytes(bytes, False)],
    data_of(bytes_node([flip_last_byte(bytes)])),
    leaf(bytes_node([bytes])),
  )
}

test refuses_a_b_data_result_one_word_lighter() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    41,
    [semantic_bytes(bytes, False)],
    with_memory(data_of(node), -1),
    leaf(node),
  )
}

test refuses_b_data_of_a_string() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    41,
    [with_type(semantic_bytes(bytes, False), string_type_cbor)],
    data_of(node),
    leaf(node),
  )
}

test refuses_b_data_of_an_argument_one_word_lighter() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    41,
    [with_memory(semantic_bytes(bytes, False), -1)],
    data_of(node),
    leaf(node),
  )
}

test refuses_b_data_of_an_integer() fail {
  let node = integer_node(cbor.serialise(long_integer()))
  verifies(41, [scalar_of(node, bytes_type_cbor)], data_of(node), leaf(node))
}

test refuses_b_data_with_a_revealed_scalar() fail {
  let bytes = pattern(100, 1)
  let node = bytes_node([bytes])
  verifies(
    41,
    [semantic_bytes(bytes, False)],
    data_of(node),
    SemanticBuiltinWitnessV1 { ..leaf(node), scalar_preimages: [bytes] },
  )
}

test refuses_b_data_with_a_second_data_node() fail {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  verifies(
    41,
    [semantic_bytes(bytes, False)],
    data_of(node),
    SemanticBuiltinWitnessV1 { ..leaf(node), data_nodes: [node, node] },
  )
}

// ## Round trips and the staged steps

test i_data_then_un_i_data_returns_the_same_value() {
  let integer_cbor = cbor.serialise(long_integer())
  let node = integer_node(integer_cbor)
  let value = semantic_integer_cbor(integer_cbor)
  and {
    verifies(40, [value], data_of(node), leaf(node)),
    verifies(45, [data_of(node)], value, leaf(node)),
  }
}

test b_data_then_un_b_data_returns_the_same_value() {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  let value = semantic_bytes(bytes, False)
  and {
    verifies(41, [value], data_of(node), leaf(node)),
    verifies(46, [data_of(node)], value, leaf(node)),
  }
}

/// unBData of a 12,000-byte leaf, then appendByteString over its result and
/// a short operand through the staged byte-string step.
fn append_after_un_b_data(claimed: ByteArray) -> Bool {
  let bytes = long_bytes()
  let node = bytes_node([bytes])
  let value = semantic_bytes(bytes, False)
  let right = pattern(100, 1)
  let staging = stage([bytes])
  and {
    verifies(46, [data_of(node)], value, leaf(node)),
    staged_spend(
      False,
      staged_step(
        10,
        [value, bytes_value(right, False)],
        semantic_bytes(claimed, False),
        material(staging, [staged(0), InlineOperand { bytes: right }]),
        staging.reference_inputs,
      ),
    ),
  }
}

test the_un_b_data_result_appends_through_the_staged_step() {
  append_after_un_b_data(
    builtin.append_bytearray(long_bytes(), pattern(100, 1)),
  )
}

test refuses_a_wrong_append_after_un_b_data() fail {
  append_after_un_b_data(
    flip_last_byte(builtin.append_bytearray(long_bytes(), pattern(100, 1))),
  )
}

/// unIData of a 12,000-byte leaf, then addInteger over its result and 1
/// through the staged integer step.
fn add_after_un_i_data(claimed: Int) -> Bool {
  let integer = long_integer()
  let integer_cbor = cbor.serialise(integer)
  let node = integer_node(integer_cbor)
  let value = semantic_integer_cbor(integer_cbor)
  let staging = stage([integer_cbor])
  and {
    verifies(45, [data_of(node)], value, leaf(node)),
    staged_spend(
      True,
      staged_step(
        0,
        [value, integer_value(1)],
        integer_value(claimed),
        material(
          staging,
          [staged(0), InlineOperand { bytes: cbor.serialise(1) }],
        ),
        staging.reference_inputs,
      ),
    ),
  }
}

test the_un_i_data_result_adds_through_the_staged_step() {
  add_after_un_i_data(long_integer() + 1)
}

test refuses_a_wrong_sum_after_un_i_data() fail {
  add_after_un_i_data(long_integer() + 2)
}

// ## A staged byte-string result is equal as Data to a direct constant
//
// The staged step accepts exactly one result per builtin and operands, so the
// leaf it accepts is the leaf the arm commits. bData relabels that leaf as a
// Data value, and equalsData over it and the direct constant of the same bytes
// returns True.

/// equalsData over `left` and `right` returning True, through the Data
/// semantics step.
fn equals_data_returns_true(left: ValueWitnessV1, right: ValueWitnessV1) -> Bool {
  let step = scalar_step(
    47,
    [left, right],
    boolean_value(True),
    SemanticBuiltinWitnessV1 {
      data_nodes: [],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
  data_misc.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// bData of the byte-string `result` over the leaf of `bytes`, then equalsData
/// over that Data value and the direct constant of `bytes`.
fn equal_as_data_to_a_direct_constant(
  result: ValueWitnessV1,
  bytes: ByteArray,
) -> Bool {
  let node = bytes_node([bytes])
  and {
    verifies(41, [result], data_of(node), leaf(node)),
    equals_data_returns_true(
      data_of(node),
      inline_constant(data_type_cbor, builtin.b_data(bytes)),
    ),
  }
}

/// A result of two pieces: a staged operand and an inline one.
test a_staged_append_result_is_equal_as_data_to_a_direct_constant() {
  let left = pattern(4000, 1)
  let right = pattern(100, 2)
  let joined = builtin.append_bytearray(left, right)
  let staging = stage([left])
  let result = semantic_bytes(joined, False)
  and {
    staged_spend(
      False,
      staged_step(
        10,
        [bytes_value(left, False), bytes_value(right, False)],
        result,
        material(staging, [staged(0), InlineOperand { bytes: right }]),
        staging.reference_inputs,
      ),
    ),
    equal_as_data_to_a_direct_constant(result, joined),
  }
}

/// A result of one piece.
test a_staged_slice_result_is_equal_as_data_to_a_direct_constant() {
  let bytes = pattern(9216, 8)
  let slice = builtin.slice_bytearray(100, 4096, bytes)
  let staging = stage([bytes])
  let result = semantic_bytes(slice, False)
  and {
    staged_spend(
      False,
      staged_step(
        12,
        [integer_value(100), integer_value(4096), bytes_value(bytes, False)],
        result,
        material(
          staging,
          [
            SmallIntegerOperand { value: 100 },
            SmallIntegerOperand { value: 4096 },
            staged(0),
          ],
        ),
        staging.reference_inputs,
      ),
    ),
    equal_as_data_to_a_direct_constant(result, slice),
  }
}

/// encodeUtf8 keeps its operand's leaf.
test a_staged_encode_utf8_result_is_equal_as_data_to_a_direct_constant() {
  let string = text(4100)
  let result = semantic_bytes(string, False)
  and {
    staged_spend(
      False,
      staged_step(
        24,
        [semantic_bytes(string, True)],
        result,
        material(
          no_staging(),
          [BytesLeafOperand { root: flat_root_v1(string), length: 4100 }],
        ),
        [],
      ),
    ),
    equal_as_data_to_a_direct_constant(result, string),
  }
}

// ## An I leaf over a non-canonical spelling never reaches the step
//
// The roots hop binds the step's arguments to the pre-state's focus, which
// commits the integer as its producer committed it: the canonical leaf. An
// argument whose leaf hashes another spelling of the same integer has another
// root, so the roots hop refuses it before the semantics hop runs.

/// The integer 7 behind a one-byte head.
fn seven_long_head() -> ValueWitnessV1 {
  let spelled = #"1807"
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(flat_root_v1(spelled), 2, 1),
    memory: 1,
  }
}

/// `long_integer()` as one definite byte string past 64 bytes.
fn long_integer_definite() -> ValueWitnessV1 {
  let magnitude = pattern(12000, 3)
  let spelled = builtin.append_bytearray(#"c2592ee0", magnitude)
  let canonical = cbor.serialise(long_integer())
  expect SemanticConstantValue { memory: words, .. } = semantic_integer_cbor(
    canonical,
  )
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_v1(spelled),
      builtin.length_of_bytearray(spelled),
      words,
    ),
    memory: words,
  }
}

test the_roots_hop_accepts_the_committed_integer() {
  and {
    roots_verify(
      40,
      [integer_value(7)],
      [integer_value(7)],
      inline_constant(data_type_cbor, builtin.i_data(7)),
    ),
    roots_verify(
      40,
      [integer_value(long_integer())],
      [integer_value(long_integer())],
      data_of(integer_node(cbor.serialise(long_integer()))),
    ),
  }
}

test the_roots_hop_refuses_a_small_integer_behind_a_long_head() fail {
  roots_verify(
    40,
    [integer_value(7)],
    [seven_long_head()],
    inline_constant(data_type_cbor, builtin.i_data(7)),
  )
}

test the_roots_hop_refuses_a_long_integer_as_one_definite_string() fail {
  roots_verify(
    40,
    [integer_value(long_integer())],
    [long_integer_definite()],
    data_of(integer_node(cbor.serialise(long_integer()))),
  )
}

test a_non_canonical_spelling_has_another_value_root() {
  let canonical = cek_builtin_v1.arguments_root_v1([compact(integer_value(7))])
  let spelled = cek_builtin_v1.arguments_root_v1([seven_long_head()])
  let long_canonical = cek_builtin_v1.arguments_root_v1(
    [integer_value(long_integer())],
  )
  let long_spelled = cek_builtin_v1.arguments_root_v1([long_integer_definite()])
  and {
    canonical != spelled,
    long_canonical != long_spelled,
  }
}

// The staged integer step reads and writes only canonical CBOR, so a leaf
// over another spelling neither binds as its operand nor is its result.

/// A long integer leaf over the CBOR `spelled`, with `value`'s words.
fn spelled_leaf(spelled: ByteArray, value: Int) -> ValueWitnessV1 {
  expect SemanticConstantValue { memory: words, .. } = semantic_integer_cbor(
    cbor.serialise(value),
  )
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_v1(spelled),
      builtin.length_of_bytearray(spelled),
      words,
    ),
    memory: words,
  }
}

/// The positive integer of `magnitude` (its first byte nonzero), spelled
/// with one zero byte before it, in 64-byte segments.
fn leading_zero_spelling(magnitude: ByteArray) -> ByteArray {
  encode_integer_cbor_bulk_v1(
    CanonicalIntegerV1 {
      negative: False,
      argument: builtin.cons_bytearray(0, magnitude),
    },
  )
}

/// The magnitude of `long_integer() + 1`: its last byte is under 0xff.
fn long_integer_plus_one_magnitude() -> ByteArray {
  let magnitude = pattern(12000, 3)
  let last = builtin.index_bytearray(magnitude, 11999)
  expect last < 255
  builtin.append_bytearray(
    builtin.slice_bytearray(0, 11999, magnitude),
    builtin.cons_bytearray(last + 1, #""),
  )
}

test the_leading_zero_spellings_hold_the_values() {
  let one_more = builtin.cons_bytearray(0, long_integer_plus_one_magnitude())
  and {
    builtin.bytearray_to_integer(True, one_more) == long_integer() + 1,
    builtin.length_of_bytearray(leading_zero_spelling(pattern(12000, 3))) == builtin.length_of_bytearray(
      cbor.serialise(long_integer()),
    ) + 1,
    builtin.length_of_bytearray(leading_zero_spelling(pattern(11999, 3))) == builtin.length_of_bytearray(
      cbor.serialise(integer_one_byte_short()),
    ) + 1,
  }
}

/// addInteger over a staged operand spelled `spelled` and 1, claiming
/// `result`.
fn add_one_to(spelled: ByteArray, value: Int, result: ValueWitnessV1) -> Bool {
  let staging = stage([spelled])
  staged_spend(
    True,
    staged_step(
      0,
      [spelled_leaf(spelled, value), integer_value(1)],
      result,
      material(staging, [staged(0), InlineOperand { bytes: cbor.serialise(1) }]),
      staging.reference_inputs,
    ),
  )
}

test the_staged_step_accepts_a_canonical_long_operand() {
  let value = long_integer()
  add_one_to(cbor.serialise(value), value, integer_value(value + 1))
}

test the_staged_step_refuses_an_operand_spelled_with_a_leading_zero() fail {
  let value = long_integer()
  add_one_to(
    leading_zero_spelling(pattern(12000, 3)),
    value,
    integer_value(value + 1),
  )
}

/// A positive integer of 11,999 magnitude bytes: after one zero byte its
/// magnitude still fills the same 1,500 words.
fn integer_one_byte_short() -> Int {
  builtin.bytearray_to_integer(True, pattern(11999, 3))
}

test the_staged_step_accepts_a_canonical_operand_one_byte_short_of_a_word() {
  let value = integer_one_byte_short()
  add_one_to(cbor.serialise(value), value, integer_value(value + 1))
}

test the_staged_step_refuses_a_leading_zero_operand_of_the_same_words() fail {
  let value = integer_one_byte_short()
  add_one_to(
    leading_zero_spelling(pattern(11999, 3)),
    value,
    integer_value(value + 1),
  )
}

test the_staged_step_refuses_a_result_leaf_spelled_with_a_leading_zero() fail {
  let value = long_integer()
  add_one_to(
    cbor.serialise(value),
    value,
    spelled_leaf(
      leading_zero_spelling(long_integer_plus_one_magnitude()),
      value + 1,
    ),
  )
}

// ## The executor's steps
//
// The steps the TypeScript executor emits for listData, iData, unListData and
// unBData (cek-builtin-step-evidence-vectors.json), spent through the step.

fn executor_step_verifies(expected_tag: Int, evidence_cbor: ByteArray) -> Bool {
  expect Some(data) = cbor.deserialise(evidence_cbor)
  expect evidence: machine.CoreStepEvidenceV1 = data
  expect machine.ExecuteBuiltinSemantic { tag, arguments, result, material } = evidence.witness
  expect tag == expected_tag
  verifies(tag, arguments, result, material)
}

test accepts_the_executor_list_data_step() {
  executor_step_verifies(39, list_data_evidence)
}

test accepts_the_executor_un_list_data_step() {
  executor_step_verifies(44, un_list_data_evidence)
}

test accepts_the_executor_i_data_step() {
  executor_step_verifies(40, i_data_evidence)
}

test accepts_the_executor_un_b_data_step() {
  executor_step_verifies(46, un_b_data_evidence)
}

test refuses_a_list_data_step_with_another_result() fail {
  expect Some(data) = cbor.deserialise(list_data_evidence)
  expect evidence: machine.CoreStepEvidenceV1 = data
  expect machine.ExecuteBuiltinSemantic { tag, arguments, material, .. } = evidence.witness
  verifies(
    tag,
    arguments,
    inline_constant(data_type_cbor, builtin.list_data([builtin.i_data(1)])),
    material,
  )
}

test refuses_an_un_list_data_step_with_another_result() fail {
  expect Some(data) = cbor.deserialise(un_list_data_evidence)
  expect evidence: machine.CoreStepEvidenceV1 = data
  expect machine.ExecuteBuiltinSemantic { tag, arguments, result, material } = evidence.witness
  // The listData step's argument: another list of Data.
  expect Some(other_data) = cbor.deserialise(list_data_evidence)
  expect other: machine.CoreStepEvidenceV1 = other_data
  expect machine.ExecuteBuiltinSemantic { arguments: [items], .. } = other.witness
  expect items != result
  verifies(tag, arguments, items, material)
}
