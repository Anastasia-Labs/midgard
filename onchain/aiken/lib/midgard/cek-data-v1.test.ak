use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_data_v1

test canonical_signed_integer_boundaries_match_the_evidence_constructor() {
  and {
    cbor.serialise(builtin.i_data(18_446_744_073_709_551_615)) == #"1bffffffffffffffff",
    cbor.serialise(builtin.i_data(18_446_744_073_709_551_616)) == #"c249010000000000000000",
    cbor.serialise(builtin.i_data(-18_446_744_073_709_551_616)) == #"3bffffffffffffffff",
    cbor.serialise(builtin.i_data(-18_446_744_073_709_551_617)) == #"c349010000000000000000",
  }
}

test offchain_semantic_data_root_vector() {
  let bytes = cek_data_v1.BytesData {
    bytes_root: #"27bffb36b7607480b9af8e0abd0a28042d953fb5791792e2c0e9c28015cad8f1",
    bytes_length: 65,
    cbor_length: 70,
    memory: 13,
  }
  let integer = cek_data_v1.IntegerData {
    cbor_root: #"e727db15461df0c95814206c4df3ca7397b8bdeab92da97eb2e87141dd3cd178",
    cbor_length: 1,
    memory: 5,
  }
  let last_field = cek_data_v1.DataListNodeV1 {
    head: cek_data_v1.hash_data_node_v1(bytes),
    head_cbor_length: 70,
    head_memory: 13,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: 70,
    memory: 13,
  }
  let fields = cek_data_v1.DataListNodeV1 {
    head: cek_data_v1.hash_data_node_v1(integer),
    head_cbor_length: 1,
    head_memory: 5,
    tail: cek_data_v1.hash_data_list_node_v1(last_field),
    length: 2,
    payload_cbor_length: 71,
    memory: 18,
  }
  let root = cek_data_v1.ConstrLargeData {
    constructor_cbor_root: #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
    constructor_cbor_length: 2,
    constructor_memory: 5,
    fields_count: 2,
    fields_root: cek_data_v1.hash_data_list_node_v1(fields),
    cbor_length: 78,
    memory: 22,
  }
  and {
    cek_data_v1.hash_data_node_v1(bytes) == #"bf990a87fc44de2dcf2c9ffbe5e8e33562949ce2a2669d818eb682b5a97f6df8",
    cek_data_v1.hash_data_node_v1(integer) == #"045b6d8572fcb30c6ac06398f3dda84638c1f44e43df6acce9ec954870dfdc37",
    cek_data_v1.hash_data_list_node_v1(last_field) == #"6a383e4909c43eb4d9da08bbc2f12876b8afa1062677a72e3c3dfdc6ef8fcbbb",
    cek_data_v1.hash_data_list_node_v1(fields) == #"fffd15ae30656d195b90b5efba7ee0162e5a5e95b4641f799677b0676ba0174e",
    cek_data_v1.hash_data_node_v1(root) == #"f37e29e366cb173ba3d65f8f0a0c5494b0edf067cdafd7b55557849be02b0d4b",
    cek_data_v1.verify_data_list_link_v1(last_field, bytes, None),
    cek_data_v1.verify_data_list_link_v1(fields, integer, Some(last_field)),
    cek_data_v1.verify_data_node_v1(root, Some(fields), None),
  }
}

test semantic_data_summary_drift_is_rejected() {
  let bytes = cek_data_v1.BytesData {
    bytes_root: #"27bffb36b7607480b9af8e0abd0a28042d953fb5791792e2c0e9c28015cad8f1",
    bytes_length: 65,
    cbor_length: 69,
    memory: 13,
  }
  !cek_data_v1.verify_data_node_v1(bytes, None, None)
}

test data_material_preimage_inspectors_reject_noncanonical_and_extract_edges() {
  let head_root = #"e727db15461df0c95814206c4df3ca7397b8bdeab92da97eb2e87141dd3cd178"
  let head = cek_data_v1.IntegerData {
    cbor_root: head_root,
    cbor_length: 1,
    memory: 5,
  }
  let list = cek_data_v1.DataListNodeV1 {
    head: cek_data_v1.hash_data_node_v1(head),
    head_cbor_length: 1,
    head_memory: 5,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: 1,
    memory: 5,
  }
  let pair = cek_data_v1.DataPairNodeV1 {
    key: cek_data_v1.hash_data_node_v1(head),
    key_cbor_length: 1,
    key_memory: 5,
    value: cek_data_v1.hash_data_node_v1(head),
    value_cbor_length: 1,
    value_memory: 5,
    tail: cek_data_v1.empty_data_pair_root_v1(),
    length: 1,
    payload_cbor_length: 2,
    memory: 10,
  }
  let head_preimage = cek_data_v1.encode_data_node_preimage_v1(head)
  let list_preimage = cek_data_v1.encode_data_list_node_preimage_v1(list)
  let pair_preimage = cek_data_v1.encode_data_pair_node_preimage_v1(pair)
  and {
    cek_data_v1.inspect_data_node_preimage_v1(head_preimage) == Some(head),
    cek_data_v1.inspect_data_list_node_preimage_v1(list_preimage) == Some(list),
    cek_data_v1.inspect_data_pair_node_preimage_v1(pair_preimage) == Some(pair),
    cek_data_v1.data_node_child_roots_v1(head) == [head_root],
    cek_data_v1.data_list_node_child_roots_v1(list) == [list.head, list.tail],
    cek_data_v1.data_pair_node_child_roots_v1(pair) == [
      pair.key,
      pair.value,
      pair.tail,
    ],
    cek_data_v1.inspect_data_node_preimage_v1(
      head_preimage |> bytearray.concat(#"00"),
    ) == None,
  }
}

/// A Data map is a list of pairs: duplicate keys stay separate entries, in
/// order. The root is pinned in
/// `demo/midgard-core/tests/cek-proof-duplicate-map-keys.test.ts`.
test offchain_duplicate_key_map_root_vector() {
  let data = builtin.map_data(
    [
      Pair(builtin.i_data(1), builtin.i_data(2)),
      Pair(builtin.i_data(1), builtin.i_data(3)),
    ],
  )
  let deduplicated = builtin.map_data(
    [Pair(builtin.i_data(1), builtin.i_data(2))],
  )
  let summary = cek_data_v1.semantic_data_summary_v1(data)
  and {
    summary.root == #"aef1afbc4af14bed532f136ec0e9db7d1f297752cbe935c6af79d0f49b0817e5",
    summary.cbor_length == 5,
    summary.memory == 24,
    cek_data_v1.semantic_data_summary_v1(deduplicated).root != summary.root,
  }
}

test offchain_ordered_map_root_vector() {
  let data = builtin.map_data(
    [
      Pair(builtin.b_data(#"11"), builtin.i_data(1)),
      Pair(builtin.b_data(#""), builtin.i_data(2)),
    ],
  )
  let summary = cek_data_v1.semantic_data_summary_v1(data)
  and {
    summary.root == #"96cf66f3acdee22a9661894a6fdae1deb78e5b434338a19cdc23df2d73fabb51",
    summary.cbor_length == 6,
    summary.memory == 24,
  }
}
