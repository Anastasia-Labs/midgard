use aiken/builtin
use midgard/cek_data_v1

test offchain_semantic_data_root_vector() {
  let bytes = cek_data_v1.BytesData {
    bytes_root: #"27bffb36b7607480b9af8e0abd0a28042d953fb5791792e2c0e9c28015cad8f1",
    bytes_length: 65,
    cbor_length: 70,
    memory: 69,
  }
  let integer = cek_data_v1.IntegerData {
    cbor_root: #"e727db15461df0c95814206c4df3ca7397b8bdeab92da97eb2e87141dd3cd178",
    cbor_length: 1,
    memory: 5,
  }
  let last_field = cek_data_v1.DataListNodeV1 {
    head: cek_data_v1.hash_data_node_v1(bytes),
    head_cbor_length: 70,
    head_memory: 69,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: 70,
    memory: 69,
  }
  let fields = cek_data_v1.DataListNodeV1 {
    head: cek_data_v1.hash_data_node_v1(integer),
    head_cbor_length: 1,
    head_memory: 5,
    tail: cek_data_v1.hash_data_list_node_v1(last_field),
    length: 2,
    payload_cbor_length: 71,
    memory: 74,
  }
  let root = cek_data_v1.ConstrLargeData {
    constructor_cbor_root: #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
    constructor_cbor_length: 2,
    constructor_memory: 6,
    fields_count: 2,
    fields_root: cek_data_v1.hash_data_list_node_v1(fields),
    cbor_length: 78,
    memory: 78,
  }
  and {
    cek_data_v1.hash_data_node_v1(bytes) == #"8d961cac2f0554e64621a1874febaaedcc89a40bb882f2ea869725ee57505191",
    cek_data_v1.hash_data_node_v1(integer) == #"045b6d8572fcb30c6ac06398f3dda84638c1f44e43df6acce9ec954870dfdc37",
    cek_data_v1.hash_data_list_node_v1(last_field) == #"aa85e721cfa08a22e6590847a80f8e24d890e88f7d539ba1601c5fad1cc492a9",
    cek_data_v1.hash_data_list_node_v1(fields) == #"89ba3f57e131f81546f69405455db99d51a76825a7e41aa33a806e90bea03632",
    cek_data_v1.hash_data_node_v1(root) == #"9ce9a6db13fa610a6efad613e5266cefe3740f2de1dd4a014884fa3f717d69de",
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
    memory: 69,
  }
  !cek_data_v1.verify_data_node_v1(bytes, None, None)
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
