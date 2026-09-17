use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1 as data
use midgard/cek_proof_v1 as proof
use midgard/ledger_state.{CekProgramMaterialDatumV1}

fn constant_material() {
  let type_root = proof.hash_blob_chunk_v1(#"00")
  let integer_root = proof.hash_blob_chunk_v1(#"01")
  let integer = data.IntegerData {
    cbor_root: integer_root,
    cbor_length: 1,
    memory: 1,
  }
  let data_root = data.hash_data_node_v1(integer)
  let data_preimage = data.encode_data_node_preimage_v1(integer)
  let value_root = proof.hash_constant_value_v1(
    type_root,
    data_root,
    1,
    data_root,
    1,
  )
  let value_preimage = #"86005820"
    |> bytearray.concat(type_root)
    |> bytearray.concat(#"5820")
    |> bytearray.concat(data_root)
    |> bytearray.concat(#"015820")
    |> bytearray.concat(data_root)
    |> bytearray.concat(#"01")
  let term_root = proof.hash_constant_term_v1(value_root)
  let term_preimage = #"82045820" |> bytearray.concat(value_root)
  let entries = [
    CekProgramMaterialDatumV1 {
      kind: 0,
      root: term_root,
      preimage: term_preimage,
    },
    CekProgramMaterialDatumV1 {
      kind: 1,
      root: value_root,
      preimage: value_preimage,
    },
    CekProgramMaterialDatumV1 { kind: 3, root: type_root, preimage: #"4100" },
    CekProgramMaterialDatumV1 { kind: 3, root: integer_root, preimage: #"4101" },
    CekProgramMaterialDatumV1 {
      kind: 5,
      root: data_root,
      preimage: data_preimage,
    },
  ]
    |> list.sort(fn(a, b) { bytearray.compare(a.root, b.root) })
  let facts = proof.CekMaterialPartitionFacts {
    program_node_count: 4,
    program_byte_length: bytearray.length(term_preimage) + bytearray.length(
      value_preimage,
    ) + 4,
    data_node_count: 1,
    data_byte_length: bytearray.length(data_preimage),
    data_roots: [data_root],
    data_blob_roots: [integer_root],
  }
  (term_root, entries, facts)
}

test partitions_agree_with_complete_graph_including_data_blob_edges() {
  let (root, entries, facts) = constant_material()
  let length = facts.program_byte_length + facts.data_byte_length
  let envelope = proof.encode_program_envelope_v1(1, 1, 0, root, 5, length)
  and {
    proof.verify_complete_program_material_entries_v1(envelope, entries),
    proof.verify_program_material_partition(root, entries, facts),
    proof.verify_data_material_partition(entries, facts, 5, length),
  }
}

test partition_refuses_omitted_data_blob_and_unreachable_material() {
  let (root, entries, facts) = constant_material()
  let length = facts.program_byte_length + facts.data_byte_length
  let missing_blob = proof.CekMaterialPartitionFacts {
    ..facts,
    data_blob_roots: [],
  }
  let missing_data = proof.CekMaterialPartitionFacts { ..facts, data_roots: [] }
  and {
    !proof.verify_program_material_partition(root, entries, missing_blob),
    !proof.verify_data_material_partition(entries, missing_blob, 5, length),
    !proof.verify_program_material_partition(root, entries, missing_data),
    !proof.verify_data_material_partition(entries, missing_data, 5, length),
  }
}

test partition_refuses_forged_totals_and_digest() {
  let (root, entries, facts) = constant_material()
  let length = facts.program_byte_length + facts.data_byte_length
  let wrong = proof.CekMaterialPartitionFacts {
    ..facts,
    program_byte_length: facts.program_byte_length + 1,
  }
  let forged = list.map(
    entries,
    fn(entry) {
      if entry.kind == 3 {
        CekProgramMaterialDatumV1 { ..entry, preimage: #"4102" }
      } else {
        entry
      }
    },
  )
  and {
    !proof.verify_program_material_partition(root, entries, wrong),
    !proof.verify_data_material_partition(entries, wrong, 5, length),
    !proof.verify_program_material_partition(root, forged, facts),
  }
}
