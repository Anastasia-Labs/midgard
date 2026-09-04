use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes, slice_len,
}
use midgard/ledger_state.{CekProgramMaterialDatumV1}

const term_node_domain = #"4d69646761726443656b5465726d4e6f64655631"

const value_node_domain = #"4d69646761726443656b56616c75654e6f64655631"

const sequence_node_domain = #"4d69646761726443656b53657175656e63654e6f64655631"

const environment_node_domain =
  #"4d69646761726443656b456e7669726f6e6d656e744e6f64655631"

const continuation_node_domain =
  #"4d69646761726443656b436f6e74696e756174696f6e4e6f64655631"

const blob_chunk_domain = #"4d69646761726443656b426c6f624368756e6b5631"

const blob_branch_domain = #"4d69646761726443656b426c6f624272616e63685631"

const machine_state_domain = #"4d69646761726443656b4d616368696e6553746174655631"

const program_envelope_domain =
  #"4d69646761726443656b50726f6772616d456e76656c6f70655631"

const bls_expression_node_domain =
  #"4d69646761726443656b426c7345787072657373696f6e5631"

pub const max_blob_chunk_bytes = 4095

pub const max_bounded_blob_bytes = 9215

pub const max_builtin_tag = 86

pub const program_envelope_version: Int = 1

/// Exact maximum of `[1, [1, 1, 0], h32, uint32, uint32]`.
pub const max_program_envelope_cbor_bytes = 50

/// `floor((64 MiB - 447 fixed V1 DA bytes) / 42 minimum tuple bytes)`.
pub const max_program_node_count = 1597819

/// Structural DA upper bound after the exact 447-byte fixed V1 framing.
pub const max_program_material_byte_length = 67108417

pub type ProgramEnvelopeV1 {
  term_root: ByteArray,
  node_count: Int,
  material_byte_length: Int,
}

pub type ProgramTermMaterialV1 {
  VariableTerm { index: Int }
  UnaryTerm { tag: Int, child: ByteArray }
  ApplicationTerm { function: ByteArray, argument: ByteArray }
  ConstantTerm { value: ByteArray }
  ErrorTerm
  BuiltinTerm { tag: Int }
  ConstrTerm { tag: Int, count: Int, sequence: ByteArray }
  CaseTerm { scrutinee: ByteArray, count: Int, sequence: ByteArray }
}

pub type ProgramValueMaterialV1 {
  ConstantValue {
    type_root: ByteArray,
    payload_root: ByteArray,
    payload_length: Int,
    semantic_root: ByteArray,
    memory: Int,
  }
}

pub type ProgramSequenceMaterialV1 {
  head: ByteArray,
  tail: ByteArray,
  length: Int,
}

pub type ProgramBlobMaterialV1 {
  BlobChunk { bytes: ByteArray }
  BlobBranch { left: ByteArray, right: ByteArray, byte_length: Int }
}

type ProgramMaterialTaskV1 {
  kind: Int,
  root: ByteArray,
  expected_length: Int,
}

type ProgramMaterialTraversalV1 {
  seen: List<ByteArray>,
  node_count: Int,
  material_byte_length: Int,
}

pub const empty_sequence_root_v1 =
  sequence_node_domain
    |> bytearray.concat(#"8100")
    |> blake2b_256

pub const empty_environment_root_v1 =
  environment_node_domain
    |> bytearray.concat(#"8100")
    |> blake2b_256

pub const empty_continuation_root_v1 =
  continuation_node_domain
    |> bytearray.concat(#"8100")
    |> blake2b_256

fn expect_hash(hash: ByteArray) -> ByteArray {
  expect bytearray.length(hash) == 32
  hash
}

fn expect_uint32(value: Int) -> Int {
  expect value >= 0
  expect value <= 4294967295
  value
}

fn expect_uint64(value: Int) -> Int {
  expect value >= 0
  expect value <= 18446744073709551615
  value
}

fn hash_term_preimage(preimage: ByteArray) -> ByteArray {
  term_node_domain
    |> bytearray.concat(preimage)
    |> blake2b_256
}

pub fn hash_variable_term_v1(index: Int) -> ByteArray {
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(0))
      |> bytearray.concat(cbor.serialise(expect_uint32(index))),
  )
}

pub fn hash_delay_term_v1(body: ByteArray) -> ByteArray {
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(1))
      |> bytearray.concat(encode_definite_bytes(expect_hash(body))),
  )
}

pub fn hash_lambda_term_v1(body: ByteArray) -> ByteArray {
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(2))
      |> bytearray.concat(encode_definite_bytes(expect_hash(body))),
  )
}

pub fn hash_application_term_v1(
  function: ByteArray,
  argument: ByteArray,
) -> ByteArray {
  hash_term_preimage(
    #"83"
      |> bytearray.concat(cbor.serialise(3))
      |> bytearray.concat(encode_definite_bytes(expect_hash(function)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(argument))),
  )
}

pub fn hash_constant_term_v1(value: ByteArray) -> ByteArray {
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(4))
      |> bytearray.concat(encode_definite_bytes(expect_hash(value))),
  )
}

/// Runtime-only constant term for the validation-machine-authenticated script
/// context. Canonical source programs never admit tag 10.
pub fn hash_context_constant_term_v1(value: ByteArray) -> ByteArray {
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(10))
      |> bytearray.concat(encode_definite_bytes(expect_hash(value))),
  )
}

pub fn hash_force_term_v1(term: ByteArray) -> ByteArray {
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(5))
      |> bytearray.concat(encode_definite_bytes(expect_hash(term))),
  )
}

pub fn hash_error_term_v1() -> ByteArray {
  hash_term_preimage(
    #"81"
      |> bytearray.concat(cbor.serialise(6)),
  )
}

pub fn hash_builtin_term_v1(tag: Int) -> ByteArray {
  expect tag >= 0
  expect tag <= max_builtin_tag
  hash_term_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(7))
      |> bytearray.concat(cbor.serialise(tag)),
  )
}

pub fn hash_constr_term_v1(
  tag: Int,
  terms_count: Int,
  terms_root: ByteArray,
) -> ByteArray {
  hash_term_preimage(
    #"84"
      |> bytearray.concat(cbor.serialise(8))
      |> bytearray.concat(cbor.serialise(expect_uint64(tag)))
      |> bytearray.concat(cbor.serialise(expect_uint32(terms_count)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(terms_root))),
  )
}

pub fn hash_case_term_v1(
  scrutinee: ByteArray,
  branches_count: Int,
  branches_root: ByteArray,
) -> ByteArray {
  hash_term_preimage(
    #"84"
      |> bytearray.concat(cbor.serialise(9))
      |> bytearray.concat(encode_definite_bytes(expect_hash(scrutinee)))
      |> bytearray.concat(cbor.serialise(expect_uint32(branches_count)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(branches_root))),
  )
}

fn hash_value_preimage(preimage: ByteArray) -> ByteArray {
  value_node_domain
    |> bytearray.concat(preimage)
    |> blake2b_256
}

pub fn hash_constant_value_v1(
  type_root: ByteArray,
  payload_root: ByteArray,
  payload_length: Int,
  semantic_root: ByteArray,
  memory: Int,
) -> ByteArray {
  hash_value_preimage(
    #"86"
      |> bytearray.concat(cbor.serialise(0))
      |> bytearray.concat(encode_definite_bytes(expect_hash(type_root)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(payload_root)))
      |> bytearray.concat(cbor.serialise(expect_uint64(payload_length)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(semantic_root)))
      |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
  )
}

pub fn hash_lambda_value_v1(
  body: ByteArray,
  environment: ByteArray,
) -> ByteArray {
  hash_value_preimage(
    #"83"
      |> bytearray.concat(cbor.serialise(1))
      |> bytearray.concat(encode_definite_bytes(expect_hash(body)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(environment))),
  )
}

pub fn hash_delay_value_v1(body: ByteArray, environment: ByteArray) -> ByteArray {
  hash_value_preimage(
    #"83"
      |> bytearray.concat(cbor.serialise(2))
      |> bytearray.concat(encode_definite_bytes(expect_hash(body)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(environment))),
  )
}

pub fn hash_constr_value_v1(
  tag: Int,
  values_count: Int,
  values_root: ByteArray,
) -> ByteArray {
  hash_value_preimage(
    #"84"
      |> bytearray.concat(cbor.serialise(3))
      |> bytearray.concat(cbor.serialise(expect_uint64(tag)))
      |> bytearray.concat(cbor.serialise(expect_uint32(values_count)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(values_root))),
  )
}

pub fn hash_builtin_value_v1(
  tag: Int,
  forces_remaining: Int,
  arguments_count: Int,
  arguments_root: ByteArray,
) -> ByteArray {
  expect tag >= 0
  expect tag <= max_builtin_tag
  hash_value_preimage(
    #"85"
      |> bytearray.concat(cbor.serialise(4))
      |> bytearray.concat(cbor.serialise(tag))
      |> bytearray.concat(cbor.serialise(expect_uint32(forces_remaining)))
      |> bytearray.concat(cbor.serialise(expect_uint32(arguments_count)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(arguments_root))),
  )
}

pub fn hash_bls_miller_loop_value_v1(expression_root: ByteArray) -> ByteArray {
  hash_value_preimage(
    #"82"
      |> bytearray.concat(cbor.serialise(5))
      |> bytearray.concat(encode_definite_bytes(expect_hash(expression_root))),
  )
}

pub fn hash_bls_miller_loop_expression_v1(
  g1_value: ByteArray,
  g2_value: ByteArray,
) -> ByteArray {
  bls_expression_node_domain
    |> bytearray.concat(#"83")
    |> bytearray.concat(cbor.serialise(0))
    |> bytearray.concat(encode_definite_bytes(expect_hash(g1_value)))
    |> bytearray.concat(encode_definite_bytes(expect_hash(g2_value)))
    |> blake2b_256
}

pub fn hash_bls_multiply_expression_v1(
  left: ByteArray,
  right: ByteArray,
) -> ByteArray {
  bls_expression_node_domain
    |> bytearray.concat(#"83")
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(encode_definite_bytes(expect_hash(left)))
    |> bytearray.concat(encode_definite_bytes(expect_hash(right)))
    |> blake2b_256
}

pub fn hash_sequence_node_v1(
  head: ByteArray,
  tail: ByteArray,
  length: Int,
) -> ByteArray {
  expect length > 0
  sequence_node_domain
    |> bytearray.concat(
        #"84"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(encode_definite_bytes(expect_hash(head)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail)))
          |> bytearray.concat(cbor.serialise(expect_uint32(length))),
      )
    |> blake2b_256
}

pub fn hash_environment_node_v1(
  value: ByteArray,
  tail: ByteArray,
  length: Int,
) -> ByteArray {
  expect length > 0
  environment_node_domain
    |> bytearray.concat(
        #"84"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(encode_definite_bytes(expect_hash(value)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail)))
          |> bytearray.concat(cbor.serialise(expect_uint32(length))),
      )
    |> blake2b_256
}

pub fn hash_force_continuation_v1(tail: ByteArray) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"83"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(0))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail))),
      )
    |> blake2b_256
}

pub fn hash_apply_argument_continuation_v1(
  argument: ByteArray,
  environment: ByteArray,
  tail: ByteArray,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"85"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(encode_definite_bytes(expect_hash(argument)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(environment)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail))),
      )
    |> blake2b_256
}

pub fn hash_apply_function_continuation_v1(
  function_value: ByteArray,
  tail: ByteArray,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"84"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(2))
          |> bytearray.concat(
              encode_definite_bytes(expect_hash(function_value)),
            )
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail))),
      )
    |> blake2b_256
}

pub fn hash_constr_continuation_v1(
  tag: Int,
  remaining_terms_count: Int,
  remaining_terms_root: ByteArray,
  values_count: Int,
  values_root: ByteArray,
  environment: ByteArray,
  tail: ByteArray,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        encode_definite_array_header(9)
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(3))
          |> bytearray.concat(cbor.serialise(expect_uint64(tag)))
          |> bytearray.concat(
              cbor.serialise(expect_uint32(remaining_terms_count)),
            )
          |> bytearray.concat(
              encode_definite_bytes(expect_hash(remaining_terms_root)),
            )
          |> bytearray.concat(cbor.serialise(expect_uint32(values_count)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(values_root)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(environment)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail))),
      )
    |> blake2b_256
}

pub fn hash_case_continuation_v1(
  branches_count: Int,
  branches_root: ByteArray,
  environment: ByteArray,
  tail: ByteArray,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"86"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(4))
          |> bytearray.concat(cbor.serialise(expect_uint32(branches_count)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(branches_root)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(environment)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail))),
      )
    |> blake2b_256
}

pub fn hash_apply_value_continuation_v1(
  value: ByteArray,
  tail: ByteArray,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"84"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(5))
          |> bytearray.concat(encode_definite_bytes(expect_hash(value)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail))),
      )
    |> blake2b_256
}

pub fn hash_case_select_continuation_v1(
  environment: ByteArray,
  tail: ByteArray,
  values_count: Int,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"85"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(6))
          |> bytearray.concat(encode_definite_bytes(expect_hash(environment)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(tail)))
          |> bytearray.concat(cbor.serialise(expect_uint32(values_count))),
      )
    |> blake2b_256
}

pub fn hash_case_apply_continuation_v1(
  environment: ByteArray,
  built_continuation: ByteArray,
) -> ByteArray {
  continuation_node_domain
    |> bytearray.concat(
        #"84"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(7))
          |> bytearray.concat(encode_definite_bytes(expect_hash(environment)))
          |> bytearray.concat(
              encode_definite_bytes(expect_hash(built_continuation)),
            ),
      )
    |> blake2b_256
}

pub fn hash_blob_chunk_v1(chunk: ByteArray) -> ByteArray {
  expect bytearray.length(chunk) <= max_blob_chunk_bytes
  blob_chunk_domain
    |> bytearray.concat(encode_definite_bytes(chunk))
    |> blake2b_256
}

pub fn hash_blob_branch_v1(
  left: ByteArray,
  right: ByteArray,
  byte_length: Int,
) -> ByteArray {
  blob_branch_domain
    |> bytearray.concat(
        #"83"
          |> bytearray.concat(encode_definite_bytes(expect_hash(left)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(right)))
          |> bytearray.concat(cbor.serialise(expect_uint64(byte_length))),
      )
    |> blake2b_256
}

pub fn bounded_blob_root_v1(bytes: ByteArray) -> ByteArray {
  let length = bytearray.length(bytes)
  expect length <= max_bounded_blob_bytes
  if length <= max_blob_chunk_bytes {
    hash_blob_chunk_v1(bytes)
  } else {
    let first = slice_len(bytes, 0, max_blob_chunk_bytes)
    let remaining = length - max_blob_chunk_bytes
    let second_length = if remaining <= max_blob_chunk_bytes {
      remaining
    } else {
      max_blob_chunk_bytes
    }
    let second = slice_len(bytes, max_blob_chunk_bytes, second_length)
    let left = hash_blob_branch_v1(
      hash_blob_chunk_v1(first),
      hash_blob_chunk_v1(second),
      max_blob_chunk_bytes + second_length,
    )
    if remaining <= max_blob_chunk_bytes {
      left
    } else {
      let third_length = remaining - second_length
      let third = slice_len(
        bytes,
        max_blob_chunk_bytes + second_length,
        third_length,
      )
      hash_blob_branch_v1(left, hash_blob_chunk_v1(third), length)
    }
  }
}

pub fn hash_machine_state_v1(
  mode: Int,
  execution_index: Int,
  focus_root: ByteArray,
  environment_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
  cpu: Int,
  memory: Int,
) -> ByteArray {
  expect mode >= 0
  expect mode <= 8
  machine_state_domain
    |> bytearray.concat(
        encode_definite_array_header(9)
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(mode))
          |> bytearray.concat(cbor.serialise(expect_uint32(execution_index)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(focus_root)))
          |> bytearray.concat(
              encode_definite_bytes(expect_hash(environment_root)),
            )
          |> bytearray.concat(
              encode_definite_bytes(expect_hash(continuation_root)),
            )
          |> bytearray.concat(cbor.serialise(expect_uint64(auxiliary)))
          |> bytearray.concat(cbor.serialise(expect_uint64(cpu)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
    |> blake2b_256
}

pub fn encode_program_envelope_v1(
  version_major: Int,
  version_minor: Int,
  version_patch: Int,
  term_root: ByteArray,
  node_count: Int,
  material_byte_length: Int,
) -> ByteArray {
  #"85"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(
        #"83"
          |> bytearray.concat(cbor.serialise(expect_uint32(version_major)))
          |> bytearray.concat(cbor.serialise(expect_uint32(version_minor)))
          |> bytearray.concat(cbor.serialise(expect_uint32(version_patch))),
      )
    |> bytearray.concat(encode_definite_bytes(expect_hash(term_root)))
    |> bytearray.concat(cbor.serialise(expect_uint32(node_count)))
    |> bytearray.concat(cbor.serialise(expect_uint64(material_byte_length)))
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

fn list_data(data: Data) -> Option<List<Data>> {
  if data_is_list(data) {
    Some(builtin.un_list_data(data))
  } else {
    None
  }
}

fn data_hash(data: Data) -> Option<ByteArray> {
  if data_is_bytes(data) {
    let value = builtin.un_b_data(data)
    if bytearray.length(value) == 32 {
      Some(value)
    } else {
      None
    }
  } else {
    None
  }
}

fn encode_program_term_material_v1(term: ProgramTermMaterialV1) -> ByteArray {
  when term is {
    VariableTerm { index } ->
      #"82"
        |> bytearray.concat(cbor.serialise(0))
        |> bytearray.concat(cbor.serialise(index))
    UnaryTerm { tag, child } ->
      #"82"
        |> bytearray.concat(cbor.serialise(tag))
        |> bytearray.concat(encode_definite_bytes(child))
    ApplicationTerm { function, argument } ->
      #"83"
        |> bytearray.concat(cbor.serialise(3))
        |> bytearray.concat(encode_definite_bytes(function))
        |> bytearray.concat(encode_definite_bytes(argument))
    ConstantTerm { value } ->
      #"82"
        |> bytearray.concat(cbor.serialise(4))
        |> bytearray.concat(encode_definite_bytes(value))
    ErrorTerm -> #"81" |> bytearray.concat(cbor.serialise(6))
    BuiltinTerm { tag } ->
      #"82"
        |> bytearray.concat(cbor.serialise(7))
        |> bytearray.concat(cbor.serialise(tag))
    ConstrTerm { tag, count, sequence } ->
      #"84"
        |> bytearray.concat(cbor.serialise(8))
        |> bytearray.concat(cbor.serialise(tag))
        |> bytearray.concat(cbor.serialise(count))
        |> bytearray.concat(encode_definite_bytes(sequence))
    CaseTerm { scrutinee, count, sequence } ->
      #"84"
        |> bytearray.concat(cbor.serialise(9))
        |> bytearray.concat(encode_definite_bytes(scrutinee))
        |> bytearray.concat(cbor.serialise(count))
        |> bytearray.concat(encode_definite_bytes(sequence))
  }
}

fn encode_program_value_material_v1(value: ProgramValueMaterialV1) -> ByteArray {
  when value is {
    ConstantValue {
      type_root,
      payload_root,
      payload_length,
      semantic_root,
      memory,
    } ->
      #"86"
        |> bytearray.concat(cbor.serialise(0))
        |> bytearray.concat(encode_definite_bytes(type_root))
        |> bytearray.concat(encode_definite_bytes(payload_root))
        |> bytearray.concat(cbor.serialise(payload_length))
        |> bytearray.concat(encode_definite_bytes(semantic_root))
        |> bytearray.concat(cbor.serialise(memory))
  }
}

fn encode_program_sequence_material_v1(
  value: ProgramSequenceMaterialV1,
) -> ByteArray {
  #"84"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(encode_definite_bytes(value.head))
    |> bytearray.concat(encode_definite_bytes(value.tail))
    |> bytearray.concat(cbor.serialise(value.length))
}

fn encode_program_blob_material_v1(value: ProgramBlobMaterialV1) -> ByteArray {
  when value is {
    BlobChunk { bytes } -> encode_definite_bytes(bytes)
    BlobBranch { left, right, byte_length } ->
      #"83"
        |> bytearray.concat(encode_definite_bytes(left))
        |> bytearray.concat(encode_definite_bytes(right))
        |> bytearray.concat(cbor.serialise(byte_length))
  }
}

/// Strictly decodes a source-program term. Runtime-only tag 10 is deliberately
/// absent: it may appear in machine evidence but never in a published program.
pub fn inspect_program_term_material_v1(
  preimage: ByteArray,
) -> Option<ProgramTermMaterialV1> {
  when cbor.deserialise(preimage) is {
    Some(data) ->
      when list_data(data) is {
        Some([tag_data, index_data]) ->
          if data_is_integer(tag_data) && data_is_integer(index_data) {
            let tag = builtin.un_i_data(tag_data)
            let index = builtin.un_i_data(index_data)
            if tag == 0 && index >= 0 && index <= 4294967295 && encode_program_term_material_v1(
              VariableTerm { index },
            ) == preimage {
              Some(VariableTerm { index })
            } else if tag == 7 && index >= 0 && index <= max_builtin_tag && encode_program_term_material_v1(
              BuiltinTerm { tag: index },
            ) == preimage {
              Some(BuiltinTerm { tag: index })
            } else {
              None
            }
          } else if data_is_integer(tag_data) {
            let tag = builtin.un_i_data(tag_data)
            when data_hash(index_data) is {
              Some(child) ->
                if ( tag == 1 || tag == 2 || tag == 5 ) && encode_program_term_material_v1(
                  UnaryTerm { tag, child },
                ) == preimage {
                  Some(UnaryTerm { tag, child })
                } else if tag == 4 && encode_program_term_material_v1(
                  ConstantTerm { value: child },
                ) == preimage {
                  Some(ConstantTerm { value: child })
                } else {
                  None
                }
              None -> None
            }
          } else {
            None
          }
        Some([tag_data]) ->
          if data_is_integer(tag_data) && builtin.un_i_data(tag_data) == 6 && encode_program_term_material_v1(
            ErrorTerm,
          ) == preimage {
            Some(ErrorTerm)
          } else {
            None
          }
        Some([tag_data, left_data, right_data]) ->
          if data_is_integer(tag_data) {
            let tag = builtin.un_i_data(tag_data)
            when data_hash(left_data) is {
              Some(function) ->
                when data_hash(right_data) is {
                  Some(argument) ->
                    if tag == 3 && encode_program_term_material_v1(
                      ApplicationTerm { function, argument },
                    ) == preimage {
                      Some(ApplicationTerm { function, argument })
                    } else {
                      None
                    }
                  None -> None
                }
              None -> None
            }
          } else {
            None
          }
        Some([tag_data, first_data, count_data, sequence_data]) ->
          if data_is_integer(tag_data) && data_is_integer(count_data) {
            let tag = builtin.un_i_data(tag_data)
            let count = builtin.un_i_data(count_data)
            when data_hash(sequence_data) is {
              Some(sequence) ->
                if count >= 0 && count <= 4294967295 {
                  if tag == 8 && data_is_integer(first_data) {
                    let constr_tag = builtin.un_i_data(first_data)
                    if constr_tag >= 0 && constr_tag <= 18446744073709551615 && encode_program_term_material_v1(
                      ConstrTerm { tag: constr_tag, count, sequence },
                    ) == preimage {
                      Some(ConstrTerm { tag: constr_tag, count, sequence })
                    } else {
                      None
                    }
                  } else if tag == 9 {
                    when data_hash(first_data) is {
                      Some(scrutinee) ->
                        if encode_program_term_material_v1(
                          CaseTerm { scrutinee, count, sequence },
                        ) == preimage {
                          Some(CaseTerm { scrutinee, count, sequence })
                        } else {
                          None
                        }
                      None -> None
                    }
                  } else {
                    None
                  }
                } else {
                  None
                }
              None -> None
            }
          } else {
            None
          }
        _ -> None
      }
    _ -> None
  }
}

pub fn program_term_child_roots_v1(
  term: ProgramTermMaterialV1,
) -> List<ByteArray> {
  when term is {
    VariableTerm { .. } -> []
    UnaryTerm { child, .. } -> [child]
    ApplicationTerm { function, argument } -> [function, argument]
    ConstantTerm { value } -> [value]
    ErrorTerm -> []
    BuiltinTerm { .. } -> []
    ConstrTerm { sequence, .. } -> [sequence]
    CaseTerm { scrutinee, sequence, .. } -> [scrutinee, sequence]
  }
}

pub fn inspect_program_value_material_v1(
  preimage: ByteArray,
) -> Option<ProgramValueMaterialV1> {
  when cbor.deserialise(preimage) is {
    Some(data) ->
      when list_data(data) is {
        Some([
          tag_data, type_root_data, payload_root_data, payload_length_data,
          semantic_root_data, memory_data,
        ]) ->
          if and {
            data_is_integer(tag_data),
            builtin.un_i_data(tag_data) == 0,
            data_is_integer(payload_length_data),
            data_is_integer(memory_data),
          } {
            when data_hash(type_root_data) is {
              Some(type_root) ->
                when data_hash(payload_root_data) is {
                  Some(payload_root) ->
                    when data_hash(semantic_root_data) is {
                      Some(semantic_root) -> {
                        let payload_length = builtin.un_i_data(
                          payload_length_data,
                        )
                        let memory = builtin.un_i_data(memory_data)
                        if and {
                          payload_length >= 0,
                          payload_length <= 18446744073709551615,
                          memory >= 0,
                          memory <= 18446744073709551615,
                          encode_program_value_material_v1(
                            ConstantValue {
                              type_root,
                              payload_root,
                              payload_length,
                              semantic_root,
                              memory,
                            },
                          ) == preimage,
                        } {
                          Some(
                            ConstantValue {
                              type_root,
                              payload_root,
                              payload_length,
                              semantic_root,
                              memory,
                            },
                          )
                        } else {
                          None
                        }
                      }
                      None -> None
                    }
                  None -> None
                }
              None -> None
            }
          } else {
            None
          }
        _ -> None
      }
    _ -> None
  }
}

pub fn inspect_program_sequence_material_v1(
  preimage: ByteArray,
) -> Option<ProgramSequenceMaterialV1> {
  when cbor.deserialise(preimage) is {
    Some(data) ->
      when list_data(data) is {
        Some([tag_data, head_data, tail_data, length_data]) ->
          if and {
            data_is_integer(tag_data),
            builtin.un_i_data(tag_data) == 1,
            data_is_integer(length_data),
          } {
            when data_hash(head_data) is {
              Some(head) ->
                when data_hash(tail_data) is {
                  Some(tail) -> {
                    let length = builtin.un_i_data(length_data)
                    if length > 0 && length <= 4294967295 && encode_program_sequence_material_v1(
                      ProgramSequenceMaterialV1 { head, tail, length },
                    ) == preimage {
                      Some(ProgramSequenceMaterialV1 { head, tail, length })
                    } else {
                      None
                    }
                  }
                  None -> None
                }
              None -> None
            }
          } else {
            None
          }
        _ -> None
      }
    _ -> None
  }
}

pub fn inspect_program_blob_material_v1(
  kind: Int,
  preimage: ByteArray,
) -> Option<ProgramBlobMaterialV1> {
  if kind == 3 {
    when cbor.deserialise(preimage) is {
      Some(data) ->
        if data_is_bytes(data) {
          let bytes = builtin.un_b_data(data)
          if bytearray.length(bytes) <= max_blob_chunk_bytes && encode_program_blob_material_v1(
            BlobChunk { bytes },
          ) == preimage {
            Some(BlobChunk { bytes })
          } else {
            None
          }
        } else {
          None
        }
      _ -> None
    }
  } else if kind == 4 {
    when cbor.deserialise(preimage) is {
      Some(data) ->
        when list_data(data) is {
          Some([left_data, right_data, length_data]) ->
            if data_is_integer(length_data) {
              when data_hash(left_data) is {
                Some(left) ->
                  when data_hash(right_data) is {
                    Some(right) -> {
                      let byte_length = builtin.un_i_data(length_data)
                      if byte_length > 0 && byte_length <= 18446744073709551615 && encode_program_blob_material_v1(
                        BlobBranch { left, right, byte_length },
                      ) == preimage {
                        Some(BlobBranch { left, right, byte_length })
                      } else {
                        None
                      }
                    }
                    None -> None
                  }
                None -> None
              }
            } else {
              None
            }
          _ -> None
        }
      _ -> None
    }
  } else {
    None
  }
}

fn encode_program_material_entry_v1(
  entry: CekProgramMaterialDatumV1,
) -> ByteArray {
  #"82"
    |> bytearray.concat(encode_definite_bytes(expect_hash(entry.root)))
    |> bytearray.concat(
        #"83"
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(cbor.serialise(entry.kind))
          |> bytearray.concat(encode_definite_bytes(entry.preimage)),
      )
}

fn encode_program_material_entries_v1(
  entries: List<CekProgramMaterialDatumV1>,
) -> ByteArray {
  when entries is {
    [] -> #""
    [entry, ..rest] ->
      encode_program_material_entry_v1(entry)
        |> bytearray.concat(encode_program_material_entries_v1(rest))
  }
}

fn encode_complete_program_material_sidecar_v1(
  entries: List<CekProgramMaterialDatumV1>,
) -> ByteArray {
  #"82"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(encode_definite_array_header(list.length(entries)))
    |> bytearray.concat(encode_program_material_entries_v1(entries))
}

fn inspect_program_material_entry_data_v1(
  data: Data,
) -> Option<CekProgramMaterialDatumV1> {
  when list_data(data) is {
    Some([root_data, value_data]) ->
      when data_hash(root_data) is {
        Some(root) ->
          when list_data(value_data) is {
            Some([version_data, kind_data, preimage_data]) ->
              if and {
                data_is_integer(version_data),
                builtin.un_i_data(version_data) == 1,
                data_is_integer(kind_data),
                data_is_bytes(preimage_data),
              } {
                let kind = builtin.un_i_data(kind_data)
                let preimage = builtin.un_b_data(preimage_data)
                if kind >= 0 && kind <= 7 && bytearray.length(preimage) > 0 {
                  Some(CekProgramMaterialDatumV1 { kind, root, preimage })
                } else {
                  None
                }
              } else {
                None
              }
            _ -> None
          }
        None -> None
      }
    _ -> None
  }
}

fn inspect_program_material_entries_data_v1(
  entries: List<Data>,
) -> Option<List<CekProgramMaterialDatumV1>> {
  when entries is {
    [] -> Some([])
    [entry_data, ..rest] ->
      when inspect_program_material_entry_data_v1(entry_data) is {
        Some(entry) ->
          when inspect_program_material_entries_data_v1(rest) is {
            Some(decoded) -> Some([entry, ..decoded])
            None -> None
          }
        None -> None
      }
  }
}

fn strictly_sorted_material_roots_v1(
  entries: List<CekProgramMaterialDatumV1>,
  previous: Option<ByteArray>,
) -> Bool {
  when entries is {
    [] -> True
    [entry, ..rest] ->
      when previous is {
        Some(root) ->
          bytearray.compare(root, entry.root) == Less && strictly_sorted_material_roots_v1(
            rest,
            Some(entry.root),
          )
        None -> strictly_sorted_material_roots_v1(rest, Some(entry.root))
      }
  }
}

pub fn inspect_complete_program_material_sidecar_v1(
  sidecar_cbor: ByteArray,
) -> Option<List<CekProgramMaterialDatumV1>> {
  when cbor.deserialise(sidecar_cbor) is {
    Some(data) ->
      when list_data(data) is {
        Some([version_data, entries_data]) ->
          if data_is_integer(version_data) && builtin.un_i_data(version_data) == 1 {
            when list_data(entries_data) is {
              Some(raw_entries) ->
                when inspect_program_material_entries_data_v1(raw_entries) is {
                  Some(entries) ->
                    if and {
                      strictly_sorted_material_roots_v1(entries, None),
                      encode_complete_program_material_sidecar_v1(entries) == sidecar_cbor,
                    } {
                      Some(entries)
                    } else {
                      None
                    }
                  None -> None
                }
              None -> None
            }
          } else {
            None
          }
        _ -> None
      }
    _ -> None
  }
}

fn program_material_root_matches_v1(entry: CekProgramMaterialDatumV1) -> Bool {
  if bytearray.length(entry.root) != 32 {
    False
  } else if entry.kind == 0 {
    when inspect_program_term_material_v1(entry.preimage) is {
      Some(_) -> hash_term_preimage(entry.preimage) == entry.root
      None -> False
    }
  } else if entry.kind == 1 {
    when inspect_program_value_material_v1(entry.preimage) is {
      Some(_) -> hash_value_preimage(entry.preimage) == entry.root
      None -> False
    }
  } else if entry.kind == 2 {
    when inspect_program_sequence_material_v1(entry.preimage) is {
      Some(_) ->
        (
          sequence_node_domain
            |> bytearray.concat(entry.preimage)
            |> blake2b_256
        ) == entry.root
      None -> False
    }
  } else if entry.kind == 3 || entry.kind == 4 {
    when inspect_program_blob_material_v1(entry.kind, entry.preimage) is {
      Some(_) ->
        (
          if entry.kind == 3 {
            blob_chunk_domain
          } else {
            blob_branch_domain
          }
            |> bytearray.concat(entry.preimage)
            |> blake2b_256
        ) == entry.root
      None -> False
    }
  } else if entry.kind == 5 {
    when cek_data_v1.inspect_data_node_preimage_v1(entry.preimage) is {
      Some(node) -> cek_data_v1.hash_data_node_v1(node) == entry.root
      None -> False
    }
  } else if entry.kind == 6 {
    when cek_data_v1.inspect_data_list_node_preimage_v1(entry.preimage) is {
      Some(node) -> cek_data_v1.hash_data_list_node_v1(node) == entry.root
      None -> False
    }
  } else if entry.kind == 7 {
    when cek_data_v1.inspect_data_pair_node_preimage_v1(entry.preimage) is {
      Some(node) -> cek_data_v1.hash_data_pair_node_v1(node) == entry.root
      None -> False
    }
  } else {
    False
  }
}

fn find_program_material_entry_v1(
  root: ByteArray,
  entries: List<CekProgramMaterialDatumV1>,
) -> Option<CekProgramMaterialDatumV1> {
  when entries is {
    [] -> None
    [entry, ..rest] ->
      if entry.root == root {
        Some(entry)
      } else {
        find_program_material_entry_v1(root, rest)
      }
  }
}

fn material_task_v1(
  kind: Int,
  root: ByteArray,
  expected_length: Int,
) -> ProgramMaterialTaskV1 {
  ProgramMaterialTaskV1 { kind, root, expected_length }
}

fn source_program_material_children_v1(
  task: ProgramMaterialTaskV1,
  entry: CekProgramMaterialDatumV1,
) -> Option<List<ProgramMaterialTaskV1>> {
  if task.kind == 0 && entry.kind == 0 {
    when inspect_program_term_material_v1(entry.preimage) is {
      Some(VariableTerm { .. }) -> Some([])
      Some(UnaryTerm { child, .. }) -> Some([material_task_v1(0, child, -1)])
      Some(ApplicationTerm { function, argument }) ->
        Some(
          [material_task_v1(0, function, -1), material_task_v1(0, argument, -1)],
        )
      Some(ConstantTerm { value }) -> Some([material_task_v1(1, value, -1)])
      Some(ErrorTerm) -> Some([])
      Some(BuiltinTerm { .. }) -> Some([])
      Some(ConstrTerm { count, sequence, .. }) ->
        if count == 0 {
          if sequence == empty_sequence_root_v1 {
            Some([])
          } else {
            None
          }
        } else {
          Some([material_task_v1(2, sequence, count)])
        }
      Some(CaseTerm { scrutinee, count, sequence }) ->
        if count == 0 {
          if sequence == empty_sequence_root_v1 {
            Some([material_task_v1(0, scrutinee, -1)])
          } else {
            None
          }
        } else {
          Some(
            [
              material_task_v1(0, scrutinee, -1),
              material_task_v1(2, sequence, count),
            ],
          )
        }
      None -> None
    }
  } else if task.kind == 1 && entry.kind == 1 {
    when inspect_program_value_material_v1(entry.preimage) is {
      Some(value) ->
        if value.payload_root == value.semantic_root {
          Some(
            [
              material_task_v1(3, value.type_root, -1),
              material_task_v1(5, value.semantic_root, -1),
            ],
          )
        } else {
          None
        }
      None -> None
    }
  } else if task.kind == 2 && entry.kind == 2 {
    when inspect_program_sequence_material_v1(entry.preimage) is {
      Some(sequence) ->
        if task.expected_length == -1 || sequence.length == task.expected_length {
          if sequence.length == 1 {
            if sequence.tail == empty_sequence_root_v1 {
              Some([material_task_v1(0, sequence.head, -1)])
            } else {
              None
            }
          } else {
            Some(
              [
                material_task_v1(0, sequence.head, -1),
                material_task_v1(2, sequence.tail, sequence.length - 1),
              ],
            )
          }
        } else {
          None
        }
      None -> None
    }
  } else if task.kind == 5 && entry.kind == 5 {
    when cek_data_v1.inspect_data_node_preimage_v1(entry.preimage) is {
      Some(cek_data_v1.ConstrSmallData { fields_count, fields_root, .. }) ->
        if fields_count == 0 {
          if fields_root == cek_data_v1.empty_data_list_root_v1() {
            Some([])
          } else {
            None
          }
        } else {
          Some([material_task_v1(6, fields_root, fields_count)])
        }
      Some(cek_data_v1.ConstrLargeData {
        constructor_cbor_root,
        fields_count,
        fields_root,
        ..
      }) ->
        if fields_count == 0 {
          if fields_root == cek_data_v1.empty_data_list_root_v1() {
            Some([material_task_v1(3, constructor_cbor_root, -1)])
          } else {
            None
          }
        } else {
          Some(
            [
              material_task_v1(3, constructor_cbor_root, -1),
              material_task_v1(6, fields_root, fields_count),
            ],
          )
        }
      Some(cek_data_v1.MapData { entries_count, entries_root, .. }) ->
        if entries_count == 0 {
          if entries_root == cek_data_v1.empty_data_pair_root_v1() {
            Some([])
          } else {
            None
          }
        } else {
          Some([material_task_v1(7, entries_root, entries_count)])
        }
      Some(cek_data_v1.ListData { items_count, items_root, .. }) ->
        if items_count == 0 {
          if items_root == cek_data_v1.empty_data_list_root_v1() {
            Some([])
          } else {
            None
          }
        } else {
          Some([material_task_v1(6, items_root, items_count)])
        }
      Some(cek_data_v1.IntegerData { cbor_root, .. }) ->
        Some([material_task_v1(3, cbor_root, -1)])
      Some(cek_data_v1.BytesData { bytes_root, .. }) ->
        Some([material_task_v1(3, bytes_root, -1)])
      None -> None
    }
  } else if task.kind == 6 && entry.kind == 6 {
    when cek_data_v1.inspect_data_list_node_preimage_v1(entry.preimage) is {
      Some(node) ->
        if task.expected_length == -1 || node.length == task.expected_length {
          if node.length == 1 {
            if node.tail == cek_data_v1.empty_data_list_root_v1() {
              Some([material_task_v1(5, node.head, -1)])
            } else {
              None
            }
          } else {
            Some(
              [
                material_task_v1(5, node.head, -1),
                material_task_v1(6, node.tail, node.length - 1),
              ],
            )
          }
        } else {
          None
        }
      None -> None
    }
  } else if task.kind == 7 && entry.kind == 7 {
    when cek_data_v1.inspect_data_pair_node_preimage_v1(entry.preimage) is {
      Some(node) ->
        if task.expected_length == -1 || node.length == task.expected_length {
          if node.length == 1 {
            if node.tail == cek_data_v1.empty_data_pair_root_v1() {
              Some(
                [
                  material_task_v1(5, node.key, -1),
                  material_task_v1(5, node.value, -1),
                ],
              )
            } else {
              None
            }
          } else {
            Some(
              [
                material_task_v1(5, node.key, -1),
                material_task_v1(5, node.value, -1),
                material_task_v1(7, node.tail, node.length - 1),
              ],
            )
          }
        } else {
          None
        }
      None -> None
    }
  } else if task.kind == 3 && ( entry.kind == 3 || entry.kind == 4 ) {
    when inspect_program_blob_material_v1(entry.kind, entry.preimage) is {
      Some(BlobChunk { .. }) -> Some([])
      Some(BlobBranch { left, right, .. }) ->
        Some([material_task_v1(3, left, -1), material_task_v1(3, right, -1)])
      None -> None
    }
  } else {
    None
  }
}

fn walk_complete_program_material_v1(
  tasks: List<ProgramMaterialTaskV1>,
  entries: List<CekProgramMaterialDatumV1>,
  traversal: ProgramMaterialTraversalV1,
) -> Option<ProgramMaterialTraversalV1> {
  when tasks is {
    [] -> Some(traversal)
    [task, ..rest] ->
      when find_program_material_entry_v1(task.root, entries) is {
        Some(entry) ->
          when source_program_material_children_v1(task, entry) is {
            Some(children) ->
              if list.has(traversal.seen, task.root) {
                walk_complete_program_material_v1(rest, entries, traversal)
              } else {
                let next = ProgramMaterialTraversalV1 {
                  seen: [task.root, ..traversal.seen],
                  node_count: traversal.node_count + 1,
                  material_byte_length: traversal.material_byte_length + bytearray.length(
                    entry.preimage,
                  ),
                }
                walk_complete_program_material_v1(
                  list.concat(children, rest),
                  entries,
                  next,
                )
              }
            None -> None
          }
        None -> None
      }
  }
}

pub fn verify_complete_program_material_entries_v1(
  envelope_cbor: ByteArray,
  entries: List<CekProgramMaterialDatumV1>,
) -> Bool {
  when inspect_program_envelope_v1(envelope_cbor) is {
    Some(envelope) ->
      if and {
        strictly_sorted_material_roots_v1(entries, None),
        list.all(entries, program_material_root_matches_v1),
      } {
        when
          walk_complete_program_material_v1(
            [material_task_v1(0, envelope.term_root, -1)],
            entries,
            ProgramMaterialTraversalV1 {
              seen: [],
              node_count: 0,
              material_byte_length: 0,
            },
          )
        is {
          Some(traversal) -> and {
              traversal.node_count == envelope.node_count,
              traversal.material_byte_length == envelope.material_byte_length,
              list.length(traversal.seen) == list.length(entries),
            }
          None -> False
        }
      } else {
        False
      }
    None -> False
  }
}

pub fn verify_complete_program_material_v1(
  envelope_cbor: ByteArray,
  sidecar_cbor: ByteArray,
) -> Bool {
  when inspect_complete_program_material_sidecar_v1(sidecar_cbor) is {
    Some(entries) ->
      verify_complete_program_material_entries_v1(envelope_cbor, entries)
    None -> False
  }
}

/// Parses the exact V1 script payload. Raw Flat/CBOR programs are SDK
/// inputs only; PlutusV3 and MidgardV1 witnesses/reference scripts carry this
/// canonical envelope so L1 can bind the executable term root directly.
pub fn inspect_program_envelope_v1(
  envelope_cbor: ByteArray,
) -> Option<ProgramEnvelopeV1> {
  if bytearray.length(envelope_cbor) > max_program_envelope_cbor_bytes {
    None
  } else {
    when cbor.deserialise(envelope_cbor) is {
      None -> None
      Some(data) ->
        when list_data(data) is {
          Some([
            envelope_version_data, uplc_version_data, term_root_data,
            node_count_data, material_byte_length_data,
          ]) ->
            when list_data(uplc_version_data) is {
              Some([major_data, minor_data, patch_data]) ->
                if and {
                  data_is_integer(envelope_version_data),
                  data_is_integer(major_data),
                  data_is_integer(minor_data),
                  data_is_integer(patch_data),
                  data_is_bytes(term_root_data),
                  data_is_integer(node_count_data),
                  data_is_integer(material_byte_length_data),
                } {
                  let envelope_version = builtin.un_i_data(
                    envelope_version_data,
                  )
                  let major = builtin.un_i_data(major_data)
                  let minor = builtin.un_i_data(minor_data)
                  let patch = builtin.un_i_data(patch_data)
                  let term_root = builtin.un_b_data(term_root_data)
                  let node_count = builtin.un_i_data(node_count_data)
                  let material_byte_length = builtin.un_i_data(
                    material_byte_length_data,
                  )
                  if and {
                    envelope_version == program_envelope_version,
                    major == 1,
                    minor == 1,
                    patch == 0,
                    bytearray.length(term_root) == 32,
                    node_count > 0,
                    node_count <= max_program_node_count,
                    material_byte_length > 0,
                    material_byte_length <= max_program_material_byte_length,
                    encode_program_envelope_v1(
                      major,
                      minor,
                      patch,
                      term_root,
                      node_count,
                      material_byte_length,
                    ) == envelope_cbor,
                  } {
                    Some(
                      ProgramEnvelopeV1 {
                        term_root,
                        node_count,
                        material_byte_length,
                      },
                    )
                  } else {
                    None
                  }
                } else {
                  None
                }
              _ -> None
            }
          _ -> None
        }
    }
  }
}

pub fn decode_program_envelope_v1(envelope_cbor: ByteArray) -> ProgramEnvelopeV1 {
  expect Some(envelope) = inspect_program_envelope_v1(envelope_cbor)
  envelope
}

pub fn hash_program_envelope_v1(
  version_major: Int,
  version_minor: Int,
  version_patch: Int,
  term_root: ByteArray,
  node_count: Int,
  material_byte_length: Int,
) -> ByteArray {
  program_envelope_domain
    |> bytearray.concat(
        encode_program_envelope_v1(
          version_major,
          version_minor,
          version_patch,
          term_root,
          node_count,
          material_byte_length,
        ),
      )
    |> blake2b_256
}
