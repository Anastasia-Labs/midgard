use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/cek_proof_v1

/// Maximum canonical constant payload that can be revealed in one bounded
/// builtin proof. Multi-value steps separately enforce the same aggregate
/// envelope; larger structured values use semantic nodes.
pub const max_direct_payload_bytes = 9215

const max_type_cbor_bytes = 64

pub type ConstantTypeV1 {
  IntegerConstant
  ByteStringConstant
  StringConstant
  UnitConstant
  BooleanConstant
  ListConstant { element: ConstantTypeV1 }
  PairConstant { first: ConstantTypeV1, second: ConstantTypeV1 }
  DataConstant
  BlsG1Constant
  BlsG2Constant
  BlsMillerLoopResultConstant
}

pub type ConstantWitnessV1 {
  type_cbor: ByteArray,
  payload_cbor: ByteArray,
}

fn decode_type_items(items: List<Data>) -> Pair<ConstantTypeV1, List<Data>> {
  expect [tag_data, ..rest] = items
  let tag = builtin.un_i_data(tag_data)
  if tag == 0 {
    Pair(IntegerConstant, rest)
  } else if tag == 1 {
    Pair(ByteStringConstant, rest)
  } else if tag == 2 {
    Pair(StringConstant, rest)
  } else if tag == 3 {
    Pair(UnitConstant, rest)
  } else if tag == 4 {
    Pair(BooleanConstant, rest)
  } else if tag == 5 {
    let Pair(element, remaining) = decode_type_items(rest)
    Pair(ListConstant { element }, remaining)
  } else if tag == 6 {
    let Pair(first, after_first) = decode_type_items(rest)
    let Pair(second, remaining) = decode_type_items(after_first)
    Pair(PairConstant { first, second }, remaining)
  } else if tag == 8 {
    Pair(DataConstant, rest)
  } else if tag == 9 {
    Pair(BlsG1Constant, rest)
  } else if tag == 10 {
    Pair(BlsG2Constant, rest)
  } else {
    expect tag == 11
    Pair(BlsMillerLoopResultConstant, rest)
  }
}

pub fn decode_constant_type_v1(type_cbor: ByteArray) -> ConstantTypeV1 {
  expect bytearray.length(type_cbor) <= max_type_cbor_bytes
  expect Some(type_data) = cbor.deserialise(type_cbor)
  expect cbor.serialise(type_data) == type_cbor
  let items = builtin.un_list_data(type_data)
  let Pair(constant_type, remaining) = decode_type_items(items)
  expect list.is_empty(remaining)
  constant_type
}

pub fn constant_type_is_known_v1(type_cbor: ByteArray) -> Bool {
  when decode_constant_type_v1(type_cbor) is {
    IntegerConstant -> True
    ByteStringConstant -> True
    StringConstant -> True
    UnitConstant -> True
    BooleanConstant -> True
    ListConstant { .. } -> True
    PairConstant { .. } -> True
    DataConstant -> True
    BlsG1Constant -> True
    BlsG2Constant -> True
    BlsMillerLoopResultConstant -> True
  }
}

pub fn decode_constant_payload_v1(payload_cbor: ByteArray) -> Data {
  expect bytearray.length(payload_cbor) <= max_direct_payload_bytes
  // stdlib's streaming CBOR decoder rejects a top-level zero-length byte
  // string because no cursor remains after its header. Preserve the canonical
  // ledger encoding explicitly instead of excluding valid empty constants.
  if payload_cbor == #"40" {
    builtin.b_data(#"")
  } else {
    expect Some(payload) = cbor.deserialise(payload_cbor)
    expect cbor.serialise(payload) == payload_cbor
    payload
  }
}

fn is_data_constr(data: Data) -> Bool {
  builtin.choose_data(data, True, False, False, False, False)
}

fn is_data_list(data: Data) -> Bool {
  builtin.choose_data(data, False, False, True, False, False)
}

fn is_data_integer(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, True, False)
}

fn is_data_bytes(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, False, True)
}

fn payload_matches_type(constant_type: ConstantTypeV1, payload: Data) -> Bool {
  when constant_type is {
    IntegerConstant -> is_data_integer(payload)
    ByteStringConstant -> is_data_bytes(payload)
    StringConstant ->
      if is_data_bytes(payload) {
        let bytes = builtin.un_b_data(payload)
        ( bytes |> builtin.decode_utf8 |> builtin.encode_utf8 ) == bytes
      } else {
        False
      }
    UnitConstant ->
      if is_data_constr(payload) {
        let Pair(tag, fields) = builtin.un_constr_data(payload)
        and {
          tag == 0,
          list.is_empty(fields),
        }
      } else {
        False
      }
    BooleanConstant ->
      if is_data_constr(payload) {
        let Pair(tag, fields) = builtin.un_constr_data(payload)
        and {
          or {
            tag == 0,
            tag == 1,
          },
          list.is_empty(fields),
        }
      } else {
        False
      }
    ListConstant { element } ->
      if is_data_list(payload) {
        builtin.un_list_data(payload)
          |> list.all(fn(item) { payload_matches_type(element, item) })
      } else {
        False
      }
    PairConstant { first, second } ->
      if is_data_constr(payload) {
        let Pair(tag, fields) = builtin.un_constr_data(payload)
        when fields is {
          [first_value, second_value] -> and {
              tag == 0,
              payload_matches_type(first, first_value),
              payload_matches_type(second, second_value),
            }
          _ -> False
        }
      } else {
        False
      }
    DataConstant -> True
    BlsG1Constant ->
      if is_data_bytes(payload) {
        bytearray.length(builtin.un_b_data(payload)) == 48
      } else {
        False
      }
    BlsG2Constant ->
      if is_data_bytes(payload) {
        bytearray.length(builtin.un_b_data(payload)) == 96
      } else {
        False
      }
    // Miller-loop results are opaque UPLC values and never decode from Data.
    BlsMillerLoopResultConstant -> False
  }
}

pub fn verify_constant_witness_v1(
  root: ByteArray,
  witness: ConstantWitnessV1,
) -> Bool {
  root == constant_root_v1(witness)
}

pub fn constant_root_v1(witness: ConstantWitnessV1) -> ByteArray {
  let constant_type = decode_constant_type_v1(witness.type_cbor)
  let payload = decode_constant_payload_v1(witness.payload_cbor)
  expect payload_matches_type(constant_type, payload)
  let semantic = cek_data_v1.semantic_data_summary_v1(payload)
  cek_proof_v1.hash_constant_value_v1(
    cek_proof_v1.hash_blob_chunk_v1(witness.type_cbor),
    semantic.root,
    semantic.cbor_length,
    semantic.root,
    semantic_memory_size_v1(constant_type, payload),
  )
}

/// Commits a canonical constant from a content-addressed semantic payload.
/// Direct constants use the same rule, so a value has one consensus root
/// regardless of whether a proof reveals its whole payload or only the nodes
/// touched by a builtin.
pub fn semantic_constant_root_v1(
  type_cbor: ByteArray,
  summary: cek_data_v1.DataSummaryV1,
  memory: Int,
) -> ByteArray {
  expect bytearray.length(type_cbor) <= max_type_cbor_bytes
  expect constant_type_is_known_v1(type_cbor)
  expect bytearray.length(summary.root) == 32
  expect summary.cbor_length >= 0
  expect summary.memory >= 0
  expect memory >= 0
  cek_proof_v1.hash_constant_value_v1(
    cek_proof_v1.hash_blob_chunk_v1(type_cbor),
    summary.root,
    summary.cbor_length,
    summary.root,
    memory,
  )
}

/// Data memory is exactly the semantic tree memory, so the context constant
/// can be seeded without revealing its entire canonical CBOR payload.
pub fn semantic_data_constant_root_v1(
  summary: cek_data_v1.DataSummaryV1,
) -> ByteArray {
  semantic_constant_root_v1(#"9f08ff", summary, summary.memory)
}

pub fn constant_type_v1(witness: ConstantWitnessV1) -> ConstantTypeV1 {
  let constant_type = decode_constant_type_v1(witness.type_cbor)
  expect
    payload_matches_type(
      constant_type,
      decode_constant_payload_v1(witness.payload_cbor),
    )
  constant_type
}

pub fn constant_payload_v1(witness: ConstantWitnessV1) -> Data {
  let payload = decode_constant_payload_v1(witness.payload_cbor)
  expect
    payload_matches_type(decode_constant_type_v1(witness.type_cbor), payload)
  payload
}

fn unsigned_byte_size_v1(value: Int) -> Int {
  if value < 256 {
    1
  } else {
    1 + unsigned_byte_size_v1(value / 256)
  }
}

pub fn integer_memory_size_v1(value: Int) -> Int {
  unsigned_byte_size_v1(
    if value < 0 {
      ( -value - 1 ) * 2
    } else {
      value * 2
    },
  )
}

pub fn bytearray_memory_size_v1(value: ByteArray) -> Int {
  let length = bytearray.length(value)
  if length == 0 {
    1
  } else {
    length
  }
}

fn data_items_memory_size_v1(items: List<Data>) -> Int {
  when items is {
    [] -> 0
    [item, ..rest] ->
      data_memory_size_v1(item) + data_items_memory_size_v1(rest)
  }
}

fn data_pairs_memory_size_v1(items: List<Pair<Data, Data>>) -> Int {
  when items is {
    [] -> 0
    [item, ..rest] ->
      data_memory_size_v1(item.1st) + data_memory_size_v1(item.2nd) + data_pairs_memory_size_v1(
        rest,
      )
  }
}

/// Plutus Data charges four words per node, plus the signed integer or
/// byte-string size for leaf payloads.
pub fn data_memory_size_v1(value: Data) -> Int {
  builtin.choose_data(
    value,
    4 + data_items_memory_size_v1(builtin.un_constr_data(value).2nd),
    4 + data_pairs_memory_size_v1(builtin.un_map_data(value)),
    4 + data_items_memory_size_v1(builtin.un_list_data(value)),
    4 + integer_memory_size_v1(builtin.un_i_data(value)),
    4 + bytearray_memory_size_v1(builtin.un_b_data(value)),
  )
}

fn semantic_items_memory_size_v1(
  constant_type: ConstantTypeV1,
  items: List<Data>,
) -> Int {
  when items is {
    [] -> 0
    [item, ..rest] ->
      semantic_memory_size_v1(constant_type, item) + semantic_items_memory_size_v1(
        constant_type,
        rest,
      )
  }
}

fn semantic_memory_size_v1(constant_type: ConstantTypeV1, payload: Data) -> Int {
  when constant_type is {
    IntegerConstant -> integer_memory_size_v1(builtin.un_i_data(payload))
    ByteStringConstant -> bytearray_memory_size_v1(builtin.un_b_data(payload))
    StringConstant -> bytearray_memory_size_v1(builtin.un_b_data(payload))
    UnitConstant -> 1
    BooleanConstant -> 1
    ListConstant { element } ->
      semantic_items_memory_size_v1(element, builtin.un_list_data(payload))
    PairConstant { first, second } -> {
      let Pair(tag, fields) = builtin.un_constr_data(payload)
      expect tag == 0
      expect [first_payload, second_payload] = fields
      semantic_memory_size_v1(first, first_payload) + semantic_memory_size_v1(
        second,
        second_payload,
      )
    }
    DataConstant -> data_memory_size_v1(payload)
    BlsG1Constant -> 48
    BlsG2Constant -> 96
    BlsMillerLoopResultConstant -> 192
  }
}

pub fn constant_payload_memory_size_v1(
  constant_type: ConstantTypeV1,
  payload: Data,
) -> Int {
  expect payload_matches_type(constant_type, payload)
  semantic_memory_size_v1(constant_type, payload)
}

pub fn constant_memory_size_v1(witness: ConstantWitnessV1) -> Int {
  let constant_type = constant_type_v1(witness)
  semantic_memory_size_v1(constant_type, constant_payload_v1(witness))
}
