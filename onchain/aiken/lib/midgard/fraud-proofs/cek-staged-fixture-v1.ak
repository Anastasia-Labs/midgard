//// Test-fixture support for the staged CEK core semantics arms
//// (`cek_core_staged_bytes`, `cek_core_staged_integer`): byte patterns, their
//// staging outputs, the compacted constants the roots hop commits, and the
//// arm 31 step a test spends. It is referenced only by tests and is not
//// reachable from any on-chain validator entry point.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use midgard/cek_builtin_v1.{
  ConstantValue, SemanticConstantValue, ValueWitnessV1,
}
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_data_v1
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_arm_v1.{OperandSourceV1, StagedMaterialV1, StagedOperand}
use midgard/cek_staged_blob_v1.{
  StagedRunsV1, chunk_bytes, flat_root_of_pieces_v1, flat_root_v1,
}
use midgard/cek_staged_integer_cbor_v1.{parse_integer_cbor_bulk_v1}
use midgard/cek_staged_integer_v1.{
  integer_leaf_from_cbor_v1, magnitude_words_v1,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Bytes

fn ascending(from: Int, acc: ByteArray) -> ByteArray {
  if from < 0 {
    acc
  } else {
    ascending(from - 1, builtin.cons_bytearray(from, acc))
  }
}

/// `block` repeated to `length` bytes (at most 65,536), never building a byte
/// string longer than that.
fn repeat_to(block: ByteArray, length: Int) -> ByteArray {
  let size = builtin.length_of_bytearray(block)
  if size >= length {
    builtin.slice_bytearray(0, length, block)
  } else if size + size <= 65536 {
    repeat_to(builtin.append_bytearray(block, block), length)
  } else {
    builtin.append_bytearray(
      block,
      builtin.slice_bytearray(0, length - size, block),
    )
  }
}

/// `length` bytes (at most 65,536) where byte i is (i + shift) mod 256.
pub fn pattern(length: Int, shift: Int) -> ByteArray {
  let block = ascending(255, #"")
  repeat_to(
    builtin.slice_bytearray(shift, 256, builtin.append_bytearray(block, block)),
    length,
  )
}

/// `length` bytes of ASCII text (valid UTF-8), at most 65,536.
pub fn text(length: Int) -> ByteArray {
  repeat_to("staged operand text ", length)
}

/// `bytes` with its last byte changed.
pub fn flip_last_byte(bytes: ByteArray) -> ByteArray {
  let length = builtin.length_of_bytearray(bytes)
  builtin.append_bytearray(
    builtin.slice_bytearray(0, length - 1, bytes),
    builtin.cons_bytearray(
      ( builtin.index_bytearray(bytes, length - 1) + 1 ) % 256,
      #"",
    ),
  )
}

/// The 4,095-byte chunks of `bytes`, the last one shorter.
pub fn chunks_of(bytes: ByteArray) -> List<ByteArray> {
  chunks_of_pieces([bytes], #"")
}

/// The 4,095-byte chunks of the concatenation of `pieces` after `carry`,
/// the last one shorter, never building a byte string longer than a piece.
fn chunks_of_pieces(
  pieces: List<ByteArray>,
  carry: ByteArray,
) -> List<ByteArray> {
  when pieces is {
    [] ->
      if carry == #"" {
        []
      } else {
        [carry]
      }
    [piece, ..rest] -> {
      let length = builtin.length_of_bytearray(piece)
      let need = chunk_bytes - builtin.length_of_bytearray(carry)
      if length < need {
        chunks_of_pieces(rest, builtin.append_bytearray(carry, piece))
      } else {
        [
          builtin.append_bytearray(
            carry,
            builtin.slice_bytearray(0, need, piece),
          ),
          ..chunks_of_pieces(
            [builtin.slice_bytearray(need, length - need, piece), ..rest],
            #"",
          )
        ]
      }
    }
  }
}

/// Chunks grouped into runs of up to three, one per staging output.
pub fn runs_of(chunks: List<ByteArray>) -> List<List<ByteArray>> {
  when chunks is {
    [a, b, c, ..rest] -> [[a, b, c], ..runs_of(rest)]
    [] -> []
    short -> [short]
  }
}

// ## Staging outputs

pub fn reference(seed: Int) -> OutputReference {
  OutputReference {
    transaction_id: builtin.blake2b_256(builtin.cons_bytearray(seed, #"")),
    output_index: seed,
  }
}

fn output(datum) -> Output {
  Output {
    address: address.from_verification_key(builtin.replicate_byte(28, 7)),
    value: assets.from_lovelace(2000000),
    datum,
    reference_script: None,
  }
}

pub fn run_datum(chunks: List<ByteArray>) -> Data {
  builtin.constr_data(
    0,
    [builtin.list_data(list.map(chunks, fn(chunk) { builtin.b_data(chunk) }))],
  )
}

pub fn staging_input(seed: Int, chunks: List<ByteArray>) -> Input {
  Input {
    output_reference: reference(seed),
    output: output(InlineDatum(run_datum(chunks))),
  }
}

/// A reference input that is not a staging output.
pub fn other_input(seed: Int) -> Input {
  Input { output_reference: reference(seed), output: output(NoDatum) }
}

/// The staging of some operands: the reference inputs (in ledger order, with
/// one other reference input first) and the runs material naming them.
pub type StagingV1 {
  reference_inputs: List<Input>,
  runs: StagedRunsV1,
}

fn stage_from(
  operands: List<List<ByteArray>>,
  next: Int,
) -> Pair<List<Input>, List<List<Int>>> {
  when operands is {
    [] -> Pair([], [])
    [pieces, ..rest] -> {
      let runs = runs_of(chunks_of_pieces(pieces, #""))
      let count = list.length(runs)
      let inputs = list.indexed_map(
        runs,
        fn(index, run) { staging_input(10 + next + index, run) },
      )
      let Pair(later_inputs, later_indices) = stage_from(rest, next + count)
      Pair(
        list.concat(inputs, later_inputs),
        [list.range(next, next + count - 1), ..later_indices],
      )
    }
  }
}

/// Stages each operand's bytes as runs on outputs with seeds 10, 11, ...
/// Operand k is named by index k of `runs.operands`.
pub fn stage(operands: List<ByteArray>) -> StagingV1 {
  stage_pieces(list.map(operands, fn(bytes) { [bytes] }))
}

/// `stage` of operands given as pieces of at most 65,536 bytes each, so an
/// operand may be longer than one byte string can be.
pub fn stage_pieces(operands: List<List<ByteArray>>) -> StagingV1 {
  let Pair(inputs, operand_indices) = stage_from(operands, 0)
  StagingV1 {
    reference_inputs: [other_input(1), ..inputs],
    runs: StagedRunsV1 {
      refs: list.map(inputs, fn(input) { input.output_reference }),
      operands: operand_indices,
    },
  }
}

/// A source naming staged operand `operand`.
pub fn staged(operand: Int) -> OperandSourceV1 {
  StagedOperand { operand }
}

pub fn no_staging() -> StagingV1 {
  StagingV1 {
    reference_inputs: [],
    runs: StagedRunsV1 { refs: [], operands: [] },
  }
}

// ## Constants

pub const integer_type_cbor = #"9f00ff"

pub const bytes_type_cbor = #"9f01ff"

pub const string_type_cbor = #"9f02ff"

pub const boolean_type_cbor = #"9f04ff"

/// An inline constant within the direct carriage bound, as the witness
/// carries it.
pub fn inline_constant(type_cbor: ByteArray, payload: Data) -> ValueWitnessV1 {
  ConstantValue(
    ConstantWitnessV1 { type_cbor, payload_cbor: cbor.serialise(payload) },
  )
}

/// A byte string or string as a root-only semantic constant: its leaf is the
/// flat root of its bytes and its length. The bytes are given as pieces of at
/// most 65,536 bytes, so the value may be longer than one byte string can be.
pub fn semantic_pieces(pieces: List<ByteArray>, string: Bool) -> ValueWitnessV1 {
  let length = list.foldl(
    pieces,
    0,
    fn(piece, total) { total + builtin.length_of_bytearray(piece) },
  )
  SemanticConstantValue {
    type_cbor: if string {
      string_type_cbor
    } else {
      bytes_type_cbor
    },
    payload: cek_data_v1.bytes_data_summary_v1(
      length,
      flat_root_of_pieces_v1(pieces),
    ),
    memory: if string {
      length / 4
    } else {
      cek_data_v1.bytes_memory_size_v1(length)
    },
  }
}

/// `semantic_pieces` of one byte string.
pub fn semantic_bytes(bytes: ByteArray, string: Bool) -> ValueWitnessV1 {
  semantic_pieces([bytes], string)
}

/// The byte string or string `bytes`, inline when its payload CBOR is within
/// the direct carriage bound (9,215 bytes) and root-only otherwise.
pub fn bytes_value(bytes: ByteArray, string: Bool) -> ValueWitnessV1 {
  if builtin.length_of_bytearray(cbor.serialise(bytes)) <= 9215 {
    inline_constant(
      if string {
        string_type_cbor
      } else {
        bytes_type_cbor
      },
      builtin.b_data(bytes),
    )
  } else {
    semantic_bytes(bytes, string)
  }
}

/// An integer of any size as a root-only semantic constant, from its
/// canonical CBOR.
pub fn semantic_integer_cbor(integer_cbor: ByteArray) -> ValueWitnessV1 {
  let words = magnitude_words_v1(parse_integer_cbor_bulk_v1(integer_cbor))
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_v1(integer_cbor),
      builtin.length_of_bytearray(integer_cbor),
      words,
    ),
    memory: words,
  }
}

/// The integer `value`, inline when its CBOR is within the direct carriage
/// bound and root-only otherwise.
pub fn integer_value(value: Int) -> ValueWitnessV1 {
  let integer_cbor = cbor.serialise(value)
  if builtin.length_of_bytearray(integer_cbor) <= 9215 {
    inline_constant(integer_type_cbor, builtin.i_data(value))
  } else {
    semantic_integer_cbor(integer_cbor)
  }
}

pub fn boolean_value(value: Bool) -> ValueWitnessV1 {
  inline_constant(
    boolean_type_cbor,
    builtin.constr_data(
      if value {
        1
      } else {
        0
      },
      [],
    ),
  )
}

/// A value as the roots hop commits it.
pub fn compact(value: ValueWitnessV1) -> ValueWitnessV1 {
  cek_builtin_v1.compact_semantic_value(value)
}

/// `count` zero bytes, built 8,192 at a time.
pub fn zeros(count: Int) -> ByteArray {
  if count <= 8192 {
    builtin.replicate_byte(count, 0)
  } else {
    builtin.append_bytearray(
      builtin.replicate_byte(8192, 0),
      zeros(count - 8192),
    )
  }
}

/// 2^bits.
pub fn power_of_two(bits: Int) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(
      builtin.index_bytearray(#"0102040810204080", bits % 8),
      zeros(bits / 8),
    ),
  )
}

// ## The step

/// The arm 31 step for builtin `tag` with these witness arguments and result
/// and this material, as the semantics hop receives it: the datum, the
/// redeemer and the transaction with `reference_inputs`.
pub type StagedStepV1 {
  datum: Option<chain.CoreDatum>,
  redeemer: chain.ArmRedeemer,
  transaction: Transaction,
}

const ct_asset = "cek-core-staged"

/// The material for `sources` over `staging`.
pub fn material(
  staging: StagingV1,
  sources: List<OperandSourceV1>,
) -> StagedMaterialV1 {
  StagedMaterialV1 { runs: staging.runs, sources }
}

pub fn staged_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  step_material: StagedMaterialV1,
  reference_inputs: List<Input>,
) -> StagedStepV1 {
  staged_step_at(
    tag,
    arguments,
    result,
    step_material,
    reference_inputs,
    machine.mode_builtin,
    5,
    3,
  )
}

/// `staged_step` with the bound mode, group and progress chosen.
pub fn staged_step_at(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  step_material: StagedMaterialV1,
  reference_inputs: List<Input>,
  mode: Int,
  group: Int,
  progress: Int,
) -> StagedStepV1 {
  let arguments_data: Data = arguments
  let result_data: Data = result
  let material_data: Data = step_material
  let witness = builtin.constr_data(
    31,
    [builtin.i_data(tag), arguments_data, result_data, material_data],
  )
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
  let state = machine.MachineStateV1 {
    mode,
    execution_index: 0,
    focus_root: #"",
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
    group,
    progress,
    facts,
  }
  let next: Data = chain.CoreBound { ..bound, progress: progress + 1 }
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
      reference_inputs,
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

/// The arm 31 step for builtin `tag` as the budget hop receives it: the
/// post-state charges the builtin's cost over the compacted arguments and
/// returns the committed result root.
pub fn budget_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
) -> StagedStepV1 {
  let step = staged_step_at(
    tag,
    arguments,
    result,
    material(no_staging(), []),
    [],
    machine.mode_builtin,
    5,
    2,
  )
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let budget = cek_builtin_v1.direct_builtin_budget_v1(
    tag,
    list.map(arguments, compact),
  )
  let post = machine.exact_state(
    bound.pre,
    machine.mode_return,
    #"",
    cek_proof_v1.empty_environment_root_v1,
    bound.pre.continuation_root,
    0,
    budget.cpu,
    budget.memory,
  )
  let charged = chain.CoreBound { ..bound, post }
  let next: Data = chain.CoreBound { ..charged, progress: 3 }
  StagedStepV1 {
    datum: Some(ct.StepDatum { ..datum, data: Some(charged) }),
    redeemer: step.redeemer,
    transaction: Transaction {
      ..step.transaction,
      outputs: fixture.continue_mid_step_l1_tx_v1(ct_asset, next).outputs,
    },
  }
}

/// The arm 36 step for builtin `tag` failing on these witness arguments, with
/// this material, as the staged failure hop receives it (group 7, progress 1):
/// the post-state halts on the builtin failure, charging nothing.
pub fn failure_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  step_material: StagedMaterialV1,
  reference_inputs: List<Input>,
) -> StagedStepV1 {
  charged_failure_step(tag, arguments, step_material, reference_inputs, 0, 0)
}

/// `failure_step` with a post-state charging `cpu` and `memory`.
pub fn charged_failure_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  step_material: StagedMaterialV1,
  reference_inputs: List<Input>,
  cpu: Int,
  memory: Int,
) -> StagedStepV1 {
  let material_data: Data = step_material
  failure_step_charging(
    tag,
    arguments,
    material_data,
    reference_inputs,
    36,
    7,
    1,
    cpu,
    memory,
  )
}

/// `failure_step` with the witness constructor, group and progress chosen and
/// the material given as Data.
pub fn failure_step_at(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  material_data: Data,
  reference_inputs: List<Input>,
  arm: Int,
  group: Int,
  progress: Int,
) -> StagedStepV1 {
  failure_step_charging(
    tag,
    arguments,
    material_data,
    reference_inputs,
    arm,
    group,
    progress,
    0,
    0,
  )
}

fn failure_step_charging(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  material_data: Data,
  reference_inputs: List<Input>,
  arm: Int,
  group: Int,
  progress: Int,
  cpu: Int,
  memory: Int,
) -> StagedStepV1 {
  let arguments_data: Data = arguments
  let witness = builtin.constr_data(
    arm,
    [builtin.i_data(tag), arguments_data, material_data],
  )
  let state = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: #"",
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let bound = chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: state,
    post: machine.exact_state(
      state,
      machine.mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      machine.error_builtin_failure,
      cpu,
      memory,
    ),
    witness_hash: blake2b_256(cbor.serialise(witness)),
    arm,
    group,
    progress,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = chain.CoreBound { ..bound, progress: progress + 1 }
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
      reference_inputs,
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}
