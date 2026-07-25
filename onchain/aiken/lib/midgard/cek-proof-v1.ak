use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

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

pub const max_builtin_tag = 86

/// Exact maximum of `[3, [1, 1, 0], h32, uint32, uint32]`.
pub const max_program_envelope_cbor_bytes = 50

/// `floor((64 MiB - 442 fixed V1 DA bytes) / 42 minimum tuple bytes)`.
pub const max_program_node_count = 1597819

/// Structural DA upper bound after the exact 442-byte fixed V1 framing.
pub const max_program_material_byte_length = 67108422

pub type ProgramEnvelopeV1 {
  term_root: ByteArray,
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
    |> bytearray.concat(cbor.serialise(3))
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
                    envelope_version == 1,
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
