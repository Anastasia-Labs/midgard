use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

const data_node_domain = #"4d69646761726443656b446174614e6f64655631"

const data_list_node_domain =
  #"4d69646761726443656b446174614c6973744e6f64655631"

const data_pair_node_domain =
  #"4d69646761726443656b44617461506169724e6f64655631"

const uint32_max = 4294967295

const uint64_max = 18446744073709551615

pub type DataNodeV1 {
  ConstrSmallData {
    constructor: Int,
    fields_count: Int,
    fields_root: ByteArray,
    cbor_length: Int,
    memory: Int,
  }
  ConstrLargeData {
    constructor_cbor_root: ByteArray,
    constructor_cbor_length: Int,
    constructor_memory: Int,
    fields_count: Int,
    fields_root: ByteArray,
    cbor_length: Int,
    memory: Int,
  }
  MapData {
    entries_count: Int,
    entries_root: ByteArray,
    cbor_length: Int,
    memory: Int,
  }
  ListData {
    items_count: Int,
    items_root: ByteArray,
    cbor_length: Int,
    memory: Int,
  }
  IntegerData { cbor_root: ByteArray, cbor_length: Int, memory: Int }
  BytesData {
    bytes_root: ByteArray,
    bytes_length: Int,
    cbor_length: Int,
    memory: Int,
  }
}

pub type DataListNodeV1 {
  head: ByteArray,
  head_cbor_length: Int,
  head_memory: Int,
  tail: ByteArray,
  length: Int,
  payload_cbor_length: Int,
  memory: Int,
}

pub type DataPairNodeV1 {
  key: ByteArray,
  key_cbor_length: Int,
  key_memory: Int,
  value: ByteArray,
  value_cbor_length: Int,
  value_memory: Int,
  tail: ByteArray,
  length: Int,
  payload_cbor_length: Int,
  memory: Int,
}

pub type DataSummaryV1 {
  root: ByteArray,
  cbor_length: Int,
  memory: Int,
}

pub type DataSequenceSummaryV1 {
  root: ByteArray,
  length: Int,
  payload_cbor_length: Int,
  memory: Int,
}

fn expect_hash(value: ByteArray) -> ByteArray {
  expect bytearray.length(value) == 32
  value
}

fn expect_uint32(value: Int) -> Int {
  expect value >= 0 && value <= uint32_max
  value
}

fn expect_uint64(value: Int) -> Int {
  expect value >= 0 && value <= uint64_max
  value
}

fn hash_data_node_preimage(preimage: ByteArray) -> ByteArray {
  data_node_domain
    |> bytearray.concat(preimage)
    |> builtin.blake2b_256
}

fn hash_data_list_preimage(preimage: ByteArray) -> ByteArray {
  data_list_node_domain
    |> bytearray.concat(preimage)
    |> builtin.blake2b_256
}

fn hash_data_pair_preimage(preimage: ByteArray) -> ByteArray {
  data_pair_node_domain
    |> bytearray.concat(preimage)
    |> builtin.blake2b_256
}

pub fn hash_data_node_v1(node: DataNodeV1) -> ByteArray {
  when node is {
    ConstrSmallData {
      constructor,
      fields_count,
      fields_root,
      cbor_length,
      memory,
    } -> {
      expect constructor >= 0 && constructor <= 127
      hash_data_node_preimage(
        encode_definite_array_header(6)
          |> bytearray.concat(cbor.serialise(0))
          |> bytearray.concat(cbor.serialise(constructor))
          |> bytearray.concat(cbor.serialise(expect_uint32(fields_count)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(fields_root)))
          |> bytearray.concat(cbor.serialise(expect_uint64(cbor_length)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
    }
    ConstrLargeData {
      constructor_cbor_root,
      constructor_cbor_length,
      constructor_memory,
      fields_count,
      fields_root,
      cbor_length,
      memory,
    } ->
      hash_data_node_preimage(
        encode_definite_array_header(8)
          |> bytearray.concat(cbor.serialise(1))
          |> bytearray.concat(
              encode_definite_bytes(expect_hash(constructor_cbor_root)),
            )
          |> bytearray.concat(
              cbor.serialise(expect_uint32(constructor_cbor_length)),
            )
          |> bytearray.concat(cbor.serialise(expect_uint64(constructor_memory)))
          |> bytearray.concat(cbor.serialise(expect_uint32(fields_count)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(fields_root)))
          |> bytearray.concat(cbor.serialise(expect_uint64(cbor_length)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
    MapData { entries_count, entries_root, cbor_length, memory } ->
      hash_data_node_preimage(
        encode_definite_array_header(5)
          |> bytearray.concat(cbor.serialise(2))
          |> bytearray.concat(cbor.serialise(expect_uint32(entries_count)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(entries_root)))
          |> bytearray.concat(cbor.serialise(expect_uint64(cbor_length)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
    ListData { items_count, items_root, cbor_length, memory } ->
      hash_data_node_preimage(
        encode_definite_array_header(5)
          |> bytearray.concat(cbor.serialise(3))
          |> bytearray.concat(cbor.serialise(expect_uint32(items_count)))
          |> bytearray.concat(encode_definite_bytes(expect_hash(items_root)))
          |> bytearray.concat(cbor.serialise(expect_uint64(cbor_length)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
    IntegerData { cbor_root, cbor_length, memory } ->
      hash_data_node_preimage(
        encode_definite_array_header(4)
          |> bytearray.concat(cbor.serialise(4))
          |> bytearray.concat(encode_definite_bytes(expect_hash(cbor_root)))
          |> bytearray.concat(cbor.serialise(expect_uint32(cbor_length)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
    BytesData { bytes_root, bytes_length, cbor_length, memory } ->
      hash_data_node_preimage(
        encode_definite_array_header(5)
          |> bytearray.concat(cbor.serialise(5))
          |> bytearray.concat(encode_definite_bytes(expect_hash(bytes_root)))
          |> bytearray.concat(cbor.serialise(expect_uint32(bytes_length)))
          |> bytearray.concat(cbor.serialise(expect_uint32(cbor_length)))
          |> bytearray.concat(cbor.serialise(expect_uint64(memory))),
      )
  }
}

pub fn hash_data_list_node_v1(node: DataListNodeV1) -> ByteArray {
  hash_data_list_preimage(
    encode_definite_array_header(7)
      |> bytearray.concat(encode_definite_bytes(expect_hash(node.head)))
      |> bytearray.concat(cbor.serialise(expect_uint32(node.head_cbor_length)))
      |> bytearray.concat(cbor.serialise(expect_uint64(node.head_memory)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(node.tail)))
      |> bytearray.concat(cbor.serialise(expect_uint32(node.length)))
      |> bytearray.concat(
          cbor.serialise(expect_uint64(node.payload_cbor_length)),
        )
      |> bytearray.concat(cbor.serialise(expect_uint64(node.memory))),
  )
}

pub fn hash_data_pair_node_v1(node: DataPairNodeV1) -> ByteArray {
  hash_data_pair_preimage(
    encode_definite_array_header(10)
      |> bytearray.concat(encode_definite_bytes(expect_hash(node.key)))
      |> bytearray.concat(cbor.serialise(expect_uint32(node.key_cbor_length)))
      |> bytearray.concat(cbor.serialise(expect_uint64(node.key_memory)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(node.value)))
      |> bytearray.concat(cbor.serialise(expect_uint32(node.value_cbor_length)))
      |> bytearray.concat(cbor.serialise(expect_uint64(node.value_memory)))
      |> bytearray.concat(encode_definite_bytes(expect_hash(node.tail)))
      |> bytearray.concat(cbor.serialise(expect_uint32(node.length)))
      |> bytearray.concat(
          cbor.serialise(expect_uint64(node.payload_cbor_length)),
        )
      |> bytearray.concat(cbor.serialise(expect_uint64(node.memory))),
  )
}

pub fn empty_data_list_root_v1() -> ByteArray {
  hash_data_list_preimage(#"80")
}

pub fn empty_data_pair_root_v1() -> ByteArray {
  hash_data_pair_preimage(#"80")
}

pub fn data_node_cbor_length_v1(node: DataNodeV1) -> Int {
  when node is {
    ConstrSmallData { cbor_length, .. } -> cbor_length
    ConstrLargeData { cbor_length, .. } -> cbor_length
    MapData { cbor_length, .. } -> cbor_length
    ListData { cbor_length, .. } -> cbor_length
    IntegerData { cbor_length, .. } -> cbor_length
    BytesData { cbor_length, .. } -> cbor_length
  }
}

pub fn data_node_memory_v1(node: DataNodeV1) -> Int {
  when node is {
    ConstrSmallData { memory, .. } -> memory
    ConstrLargeData { memory, .. } -> memory
    MapData { memory, .. } -> memory
    ListData { memory, .. } -> memory
    IntegerData { memory, .. } -> memory
    BytesData { memory, .. } -> memory
  }
}

fn list_cbor_length(length: Int, payload_cbor_length: Int) -> Int {
  expect expect_uint32(length) >= 0
  expect expect_uint64(payload_cbor_length) >= 0
  if length == 0 {
    1
  } else {
    2 + payload_cbor_length
  }
}

fn map_cbor_length(length: Int, payload_cbor_length: Int) -> Int {
  expect expect_uint32(length) >= 0
  expect expect_uint64(payload_cbor_length) >= 0
  let header_length = if length < 24 {
    1
  } else if length <= 255 {
    2
  } else if length <= 65535 {
    3
  } else {
    5
  }
  header_length + payload_cbor_length
}

fn bytes_header_length(length: Int) -> Int {
  expect expect_uint32(length) >= 0
  if length < 24 {
    1
  } else if length <= 255 {
    2
  } else if length <= 65535 {
    3
  } else {
    5
  }
}

pub fn bytes_data_cbor_length_v1(bytes_length: Int) -> Int {
  expect expect_uint32(bytes_length) >= 0
  if bytes_length <= 64 {
    bytes_header_length(bytes_length) + bytes_length
  } else {
    let full_chunks = bytes_length / 64
    let remainder = bytes_length % 64
    2 + full_chunks * 66 + if remainder == 0 {
      0
    } else {
      bytes_header_length(remainder) + remainder
    }
  }
}

fn empty_or_list_summary_matches(
  root: ByteArray,
  length: Int,
  summary: Option<DataListNodeV1>,
) -> Bool {
  if length == 0 {
    and {
      root == empty_data_list_root_v1(),
      summary == None,
    }
  } else {
    expect Some(exact) = summary
    and {
      root == hash_data_list_node_v1(exact),
      exact.length == length,
    }
  }
}

fn empty_or_pair_summary_matches(
  root: ByteArray,
  length: Int,
  summary: Option<DataPairNodeV1>,
) -> Bool {
  if length == 0 {
    and {
      root == empty_data_pair_root_v1(),
      summary == None,
    }
  } else {
    expect Some(exact) = summary
    and {
      root == hash_data_pair_node_v1(exact),
      exact.length == length,
    }
  }
}

fn list_payload_length(summary: Option<DataListNodeV1>) -> Int {
  when summary is {
    None -> 0
    Some(exact) -> exact.payload_cbor_length
  }
}

fn list_memory(summary: Option<DataListNodeV1>) -> Int {
  when summary is {
    None -> 0
    Some(exact) -> exact.memory
  }
}

fn pair_payload_length(summary: Option<DataPairNodeV1>) -> Int {
  when summary is {
    None -> 0
    Some(exact) -> exact.payload_cbor_length
  }
}

fn pair_memory(summary: Option<DataPairNodeV1>) -> Int {
  when summary is {
    None -> 0
    Some(exact) -> exact.memory
  }
}

/// Verifies the local summaries of one semantic Data node. Integer canonical
/// CBOR and large-constructor blob parsing are separate streaming steps.
pub fn verify_data_node_v1(
  node: DataNodeV1,
  list_summary: Option<DataListNodeV1>,
  pair_summary: Option<DataPairNodeV1>,
) -> Bool {
  when node is {
    ConstrSmallData {
      constructor,
      fields_count,
      fields_root,
      cbor_length,
      memory,
    } -> {
      expect constructor >= 0 && constructor <= 127
      let summary_matches = empty_or_list_summary_matches(
        fields_root,
        fields_count,
        list_summary,
      )
      let fields_cbor_length = list_cbor_length(
        fields_count,
        list_payload_length(list_summary),
      )
      and {
        summary_matches,
        pair_summary == None,
        cbor_length == if constructor <= 6 {
          2 + fields_cbor_length
        } else {
          3 + fields_cbor_length
        },
        memory == 4 + list_memory(list_summary),
      }
    }
    ConstrLargeData {
      constructor_cbor_root,
      constructor_cbor_length,
      constructor_memory,
      fields_count,
      fields_root,
      cbor_length,
      memory,
    } -> {
      let summary_matches = empty_or_list_summary_matches(
        fields_root,
        fields_count,
        list_summary,
      )
      and {
        summary_matches,
        pair_summary == None,
        bytearray.length(constructor_cbor_root) == 32,
        constructor_cbor_length > 0,
        constructor_memory >= 5,
        cbor_length == 3 + constructor_cbor_length + list_cbor_length(
          fields_count,
          list_payload_length(list_summary),
        ),
        memory == 4 + list_memory(list_summary),
      }
    }
    MapData { entries_count, entries_root, cbor_length, memory } -> {
      let summary_matches = empty_or_pair_summary_matches(
        entries_root,
        entries_count,
        pair_summary,
      )
      and {
        summary_matches,
        list_summary == None,
        cbor_length == map_cbor_length(
          entries_count,
          pair_payload_length(pair_summary),
        ),
        memory == 4 + pair_memory(pair_summary),
      }
    }
    ListData { items_count, items_root, cbor_length, memory } -> {
      let summary_matches = empty_or_list_summary_matches(
        items_root,
        items_count,
        list_summary,
      )
      and {
        summary_matches,
        pair_summary == None,
        cbor_length == list_cbor_length(
          items_count,
          list_payload_length(list_summary),
        ),
        memory == 4 + list_memory(list_summary),
      }
    }
    IntegerData { cbor_root, cbor_length, memory } -> and {
        list_summary == None,
        pair_summary == None,
        bytearray.length(cbor_root) == 32,
        cbor_length > 0,
        memory >= 5,
      }
    BytesData { bytes_root, bytes_length, cbor_length, memory } -> and {
        list_summary == None,
        pair_summary == None,
        bytearray.length(bytes_root) == 32,
        bytes_length >= 0,
        cbor_length == bytes_data_cbor_length_v1(bytes_length),
        memory == 4 + if bytes_length == 0 {
          1
        } else {
          bytes_length
        },
      }
  }
}

pub fn verify_data_list_link_v1(
  node: DataListNodeV1,
  head: DataNodeV1,
  tail: Option<DataListNodeV1>,
) -> Bool {
  let tail_root = when tail is {
    None -> empty_data_list_root_v1()
    Some(exact) -> hash_data_list_node_v1(exact)
  }
  let tail_length = when tail is {
    None -> 0
    Some(exact) -> exact.length
  }
  let tail_payload = when tail is {
    None -> 0
    Some(exact) -> exact.payload_cbor_length
  }
  let tail_memory = when tail is {
    None -> 0
    Some(exact) -> exact.memory
  }
  and {
    node.head == hash_data_node_v1(head),
    node.head_cbor_length == data_node_cbor_length_v1(head),
    node.head_memory == data_node_memory_v1(head),
    node.tail == tail_root,
    node.length == tail_length + 1,
    node.payload_cbor_length == node.head_cbor_length + tail_payload,
    node.memory == node.head_memory + tail_memory,
  }
}

pub fn verify_data_pair_link_v1(
  node: DataPairNodeV1,
  key: DataNodeV1,
  value: DataNodeV1,
  tail: Option<DataPairNodeV1>,
) -> Bool {
  let tail_root = when tail is {
    None -> empty_data_pair_root_v1()
    Some(exact) -> hash_data_pair_node_v1(exact)
  }
  let tail_length = when tail is {
    None -> 0
    Some(exact) -> exact.length
  }
  let tail_payload = when tail is {
    None -> 0
    Some(exact) -> exact.payload_cbor_length
  }
  let tail_memory = when tail is {
    None -> 0
    Some(exact) -> exact.memory
  }
  and {
    node.key == hash_data_node_v1(key),
    node.key_cbor_length == data_node_cbor_length_v1(key),
    node.key_memory == data_node_memory_v1(key),
    node.value == hash_data_node_v1(value),
    node.value_cbor_length == data_node_cbor_length_v1(value),
    node.value_memory == data_node_memory_v1(value),
    node.tail == tail_root,
    node.length == tail_length + 1,
    node.payload_cbor_length == node.key_cbor_length + node.value_cbor_length + tail_payload,
    node.memory == node.key_memory + node.value_memory + tail_memory,
  }
}

fn unsigned_byte_size(value: Int) -> Int {
  if value < 256 {
    1
  } else {
    1 + unsigned_byte_size(value / 256)
  }
}

fn integer_memory_size(value: Int) -> Int {
  let doubled_magnitude = if value < 0 {
    ( -value - 1 ) * 2
  } else {
    value * 2
  }
  unsigned_byte_size(doubled_magnitude)
}

pub fn empty_data_list_summary_v1() -> DataSequenceSummaryV1 {
  DataSequenceSummaryV1 {
    root: empty_data_list_root_v1(),
    length: 0,
    payload_cbor_length: 0,
    memory: 0,
  }
}

pub fn prepend_data_list_summary_v1(
  head: DataSummaryV1,
  tail: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  let node = DataListNodeV1 {
    head: head.root,
    head_cbor_length: head.cbor_length,
    head_memory: head.memory,
    tail: tail.root,
    length: tail.length + 1,
    payload_cbor_length: head.cbor_length + tail.payload_cbor_length,
    memory: head.memory + tail.memory,
  }
  DataSequenceSummaryV1 {
    root: hash_data_list_node_v1(node),
    length: node.length,
    payload_cbor_length: node.payload_cbor_length,
    memory: node.memory,
  }
}

pub fn empty_data_pair_summary_v1() -> DataSequenceSummaryV1 {
  DataSequenceSummaryV1 {
    root: empty_data_pair_root_v1(),
    length: 0,
    payload_cbor_length: 0,
    memory: 0,
  }
}

pub fn prepend_data_pair_summary_v1(
  key: DataSummaryV1,
  value: DataSummaryV1,
  tail: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  let node = DataPairNodeV1 {
    key: key.root,
    key_cbor_length: key.cbor_length,
    key_memory: key.memory,
    value: value.root,
    value_cbor_length: value.cbor_length,
    value_memory: value.memory,
    tail: tail.root,
    length: tail.length + 1,
    payload_cbor_length: key.cbor_length + value.cbor_length + tail.payload_cbor_length,
    memory: key.memory + value.memory + tail.memory,
  }
  DataSequenceSummaryV1 {
    root: hash_data_pair_node_v1(node),
    length: node.length,
    payload_cbor_length: node.payload_cbor_length,
    memory: node.memory,
  }
}

pub fn small_constr_data_summary_v1(
  constructor: Int,
  fields: DataSequenceSummaryV1,
) -> DataSummaryV1 {
  expect constructor >= 0 && constructor <= 127
  let fields_cbor_length = list_cbor_length(
    fields.length,
    fields.payload_cbor_length,
  )
  let node = ConstrSmallData {
    constructor,
    fields_count: fields.length,
    fields_root: fields.root,
    cbor_length: if constructor <= 6 {
      2 + fields_cbor_length
    } else {
      3 + fields_cbor_length
    },
    memory: 4 + fields.memory,
  }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length: data_node_cbor_length_v1(node),
    memory: data_node_memory_v1(node),
  }
}

pub fn large_constr_data_summary_v1(
  constructor: Int,
  constructor_cbor_root: ByteArray,
  fields: DataSequenceSummaryV1,
) -> DataSummaryV1 {
  expect constructor > 127
  expect bytearray.length(constructor_cbor_root) == 32
  let constructor_cbor_length = bytearray.length(
    cbor.serialise(builtin.i_data(constructor)),
  )
  let node = ConstrLargeData {
    constructor_cbor_root,
    constructor_cbor_length,
    constructor_memory: 4 + integer_memory_size(constructor),
    fields_count: fields.length,
    fields_root: fields.root,
    cbor_length: 3 + constructor_cbor_length + list_cbor_length(
      fields.length,
      fields.payload_cbor_length,
    ),
    memory: 4 + fields.memory,
  }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length: data_node_cbor_length_v1(node),
    memory: data_node_memory_v1(node),
  }
}

pub fn list_data_summary_v1(items: DataSequenceSummaryV1) -> DataSummaryV1 {
  let node = ListData {
    items_count: items.length,
    items_root: items.root,
    cbor_length: list_cbor_length(items.length, items.payload_cbor_length),
    memory: 4 + items.memory,
  }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length: data_node_cbor_length_v1(node),
    memory: data_node_memory_v1(node),
  }
}

pub fn map_data_summary_v1(entries: DataSequenceSummaryV1) -> DataSummaryV1 {
  let node = MapData {
    entries_count: entries.length,
    entries_root: entries.root,
    cbor_length: map_cbor_length(entries.length, entries.payload_cbor_length),
    memory: 4 + entries.memory,
  }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length: data_node_cbor_length_v1(node),
    memory: data_node_memory_v1(node),
  }
}

pub fn integer_data_summary_v1(
  integer: Int,
  cbor_root: ByteArray,
) -> DataSummaryV1 {
  expect bytearray.length(cbor_root) == 32
  let cbor_length = bytearray.length(cbor.serialise(builtin.i_data(integer)))
  let node = IntegerData {
    cbor_root,
    cbor_length,
    memory: 4 + integer_memory_size(integer),
  }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length,
    memory: data_node_memory_v1(node),
  }
}

pub fn bytes_data_summary_v1(
  bytes_length: Int,
  bytes_root: ByteArray,
) -> DataSummaryV1 {
  expect bytes_length >= 0
  expect bytearray.length(bytes_root) == 32
  let node = BytesData {
    bytes_root,
    bytes_length,
    cbor_length: bytes_data_cbor_length_v1(bytes_length),
    memory: 4 + if bytes_length == 0 {
      1
    } else {
      bytes_length
    },
  }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length: data_node_cbor_length_v1(node),
    memory: data_node_memory_v1(node),
  }
}

fn commit_data_list(items: List<Data>) -> DataSequenceSummaryV1 {
  when items is {
    [] -> empty_data_list_summary_v1()
    [head, ..tail] -> {
      let head_summary = semantic_data_summary_v1(head)
      let tail_summary = commit_data_list(tail)
      prepend_data_list_summary_v1(head_summary, tail_summary)
    }
  }
}

fn commit_data_pairs(items: List<Pair<Data, Data>>) -> DataSequenceSummaryV1 {
  when items is {
    [] -> empty_data_pair_summary_v1()
    [head, ..tail] -> {
      let key = semantic_data_summary_v1(head.1st)
      let value = semantic_data_summary_v1(head.2nd)
      let tail_summary = commit_data_pairs(tail)
      prepend_data_pair_summary_v1(key, value, tail_summary)
    }
  }
}

/// Direct semantic commitment for a payload that already fits one proof.
/// Larger byte and integer leaves are committed by the streaming blob/data
/// proof instead; this helper never creates a whole-value consensus cap.
pub fn semantic_data_summary_v1(data: Data) -> DataSummaryV1 {
  let cbor_length = bytearray.length(cbor.serialise(data))
  let node = builtin.choose_data(
    data,
    {
      let Pair(constructor, fields) = builtin.un_constr_data(data)
      let summary = commit_data_list(fields)
      let memory = 4 + summary.memory
      if constructor <= 127 {
        ConstrSmallData {
          constructor,
          fields_count: summary.length,
          fields_root: summary.root,
          cbor_length,
          memory,
        }
      } else {
        let constructor_data = builtin.i_data(constructor)
        let constructor_cbor = cbor.serialise(constructor_data)
        ConstrLargeData {
          constructor_cbor_root: cek_proof_v1.hash_blob_chunk_v1(
            constructor_cbor,
          ),
          constructor_cbor_length: bytearray.length(constructor_cbor),
          constructor_memory: 4 + integer_memory_size(constructor),
          fields_count: summary.length,
          fields_root: summary.root,
          cbor_length,
          memory,
        }
      }
    },
    {
      let summary = commit_data_pairs(builtin.un_map_data(data))
      MapData {
        entries_count: summary.length,
        entries_root: summary.root,
        cbor_length,
        memory: 4 + summary.memory,
      }
    },
    {
      let summary = commit_data_list(builtin.un_list_data(data))
      ListData {
        items_count: summary.length,
        items_root: summary.root,
        cbor_length,
        memory: 4 + summary.memory,
      }
    },
    {
      let integer = builtin.un_i_data(data)
      IntegerData {
        cbor_root: cek_proof_v1.hash_blob_chunk_v1(cbor.serialise(data)),
        cbor_length,
        memory: 4 + integer_memory_size(integer),
      }
    },
    {
      let bytes = builtin.un_b_data(data)
      BytesData {
        bytes_root: cek_proof_v1.hash_blob_chunk_v1(bytes),
        bytes_length: bytearray.length(bytes),
        cbor_length,
        memory: 4 + if bytearray.length(bytes) == 0 {
          1
        } else {
          bytearray.length(bytes)
        },
      }
    },
  )
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length,
    memory: data_node_memory_v1(node),
  }
}

pub fn hash_semantic_data_v1(data: Data) -> ByteArray {
  semantic_data_summary_v1(data).root
}
