use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use midgard/cek_data_v1
use midgard/validation_machine_v1

const auxiliary_corpus_cbor =
  #"9fd87980d87a9fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffd8799f010000010041128080ffffd87b9fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffd8799f010000010041128080ffd87980ffd87c9fd8799f010000010041128080ffd87a80d87980ffd87d9fd8799f41020100000000ffffd87e9f00410358202020202020202020202020202020202020202020202020202020202020202020410480d87980ffd87f9f0141055820212121212121212121212121212121212121212121212121212121212121212180ffd905009f004106582022222222222222222222222222222222222222222222222222222222222222224107ffd905019f000058202323232323232323232323232323232323232323232323232323232323232323410980ffd905029f0000410a0058202424242424242424242424242424242424242424242424242424242424242424015820252525252525252525252525252525252525252525252525252525252525252580ffd905039f0001015820262626262626262626262626262626262626262626262626262626262626262680ffd905049f0000000058202727272727272727272727272727272727272727272727272727272727272727410b800000410c0158203030303030303030303030303030303030303030303030303030303030303030805820282828282828282828282828282828282828282828282828282828282828282880d8799f010000010041128080ffffd905059fd8799fd8799f0000582014141414141414141414141414141414141414141414141414141414141414145820151515151515151515151515151515151515151515151515151515151515151558201616161616161616161616161616161616161616161616161616161616161616000000ffd8799f0000582014141414141414141414141414141414141414141414141414141414141414145820151515151515151515151515151515151515151515151515151515151515151558201616161616161616161616161616161616161616161616161616161616161616000000ffd87f80ffffd905069f0000410e410f80ffd905079f00411080ffd905089f80005820292929292929292929292929292929292929292929292929292929292929292980ffd905099f00411141120180ffd9050a9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffff00010158202a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a8000000058202b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b411380ffd9050b9fd87a80d8799f010000000101582013131313131313131313131313131313131313131313131313131313131313130000000000000000d87a80ffd8799fd87c80d87a80d87a80ffffd9050c9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffffffd9050d9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffff004114411580ffd9050e9fd8799fd8799f40000000ffd8799f400000ffd8799f400000ffffffd9050f9fd8799fd8799f40000000ffd8799f400000ffd8799f400000ffffffd905109fd8799fd8799f400000ffd8799f400000ffd8799f400000ffffffd905119f00411658202c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c41170041184119018080d8799fd879800080ffffd905129f00411a00411b411c018080d8799fd879800080ffffd905139f00411d411e0180d8799fd879800080ffffd905149f01411f58202d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d4120ffd905159f00412180ffd905169fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffffd905179fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffffd905189f000158202e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e80ffd905199fd87980ffd9051a9f4122d87980ffd9051b9fd8799f01000001d8799f0040ffff80ffd9051c9f0041234124d8799fd8799f01000080ff00800080ffffd9051d9fd8799f010000010041128080ffd87a80ffd9051e9f0000000058202f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f412580000041260158203030303030303030303030303030303030303030303030303030303030303030805820313131313131313131313131313131313131313131313131313131313131313180d87a8080ffd9051f9f00412780ffd905209fd8799f010000010041128080ffd87a80ffff"

const h41 = #"4141414141414141414141414141414141414141414141414141414141414141"

const h42 = #"4242424242424242424242424242424242424242424242424242424242424242"

const h43 = #"4343434343434343434343434343434343434343434343434343434343434343"

const h44 = #"4444444444444444444444444444444444444444444444444444444444444444"

const h45 = #"4545454545454545454545454545454545454545454545454545454545454545"

const h46 = #"4646464646464646464646464646464646464646464646464646464646464646"

const h47 = #"4747474747474747474747474747474747474747474747474747474747474747"

const h48 = #"4848484848484848484848484848484848484848484848484848484848484848"

const h49 = #"4949494949494949494949494949494949494949494949494949494949494949"

const h4a = #"4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a"

const h4b = #"4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b"

const h4c = #"4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c"

const h4d = #"4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d"

const h4e = #"4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e"

const h4f = #"4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f"

const h50 = #"5050505050505050505050505050505050505050505050505050505050505050"

const h51 = #"5151515151515151515151515151515151515151515151515151515151515151"

const h52 = #"5252525252525252525252525252525252525252525252525252525252525252"

const h53 = #"5353535353535353535353535353535353535353535353535353535353535353"

const h54 = #"5454545454545454545454545454545454545454545454545454545454545454"

fn empty_receive_scan() -> validation_machine_v1.ReceivePurposeScanControlV1 {
  validation_machine_v1.ReceivePurposeScanControlV1 {
    source_count: 0,
    source_peaks: [],
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: [],
  }
}

fn empty_observer_scan() -> validation_machine_v1.ObserverPurposeScanControlV1 {
  validation_machine_v1.ObserverPurposeScanControlV1 {
    total_count: 0,
    seen: 0,
    previous_hash: #"",
  }
}

fn empty_mint_fold() -> validation_machine_v1.MintFoldControlV1 {
  validation_machine_v1.MintFoldControlV1 {
    policy_count: -1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: #"",
    item_length: 0,
    item_commitment: #"",
    item_cursor: 0,
    assets_remaining: 0,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 0,
    asset_peaks: [],
  }
}

fn native_control() -> validation_machine_v1.NativeScriptsControlV1 {
  validation_machine_v1.NativeScriptsControlV1 {
    compact_cbor: #"01",
    witness_set_compact_cbor: #"02",
    field_preimage_lengths_cbor: #"03",
    context_cbor: #"04",
    resolved_input_count: 0,
    resolved_inputs_accumulator: h4a,
    spend_input_count: 0,
    resolved_item_peaks: [],
    signer_count: 0,
    signer_frontier_commitment: h4b,
    source_count: 0,
    source_peaks: [],
    redeemer_count: 0,
    redeemer_peaks: [],
    purpose_count: 0,
    purpose_peaks: [],
    output_count: 0,
    output_peaks: [],
    output_descriptor_peaks: [],
    mint_count: 0,
    mint_peaks: [],
    execution_count: 0,
    execution_peaks: [],
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: h4c,
  }
}

fn nonempty_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: h51, cbor_length: 1, memory: 1 }
}

fn empty_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: #"", cbor_length: 0, memory: 0 }
}

fn nonempty_sequence() -> cek_data_v1.DataSequenceSummaryV1 {
  cek_data_v1.DataSequenceSummaryV1 {
    root: h52,
    length: 1,
    payload_cbor_length: 1,
    memory: 1,
  }
}

fn auxiliary_arity(tag: Int) -> Int {
  when tag is {
    0 -> 0
    1 -> 2
    2 -> 3
    3 -> 3
    4 -> 1
    5 -> 6
    6 -> 4
    7 -> 4
    8 -> 5
    9 -> 8
    10 -> 5
    11 -> 16
    12 -> 1
    13 -> 5
    14 -> 3
    15 -> 4
    16 -> 5
    17 -> 12
    18 -> 3
    19 -> 1
    20 -> 5
    21 -> 1
    22 -> 1
    23 -> 1
    24 -> 11
    25 -> 9
    26 -> 6
    27 -> 4
    28 -> 3
    29 -> 1
    30 -> 1
    31 -> 4
    32 -> 1
    33 -> 2
    34 -> 2
    35 -> 4
    36 -> 2
    37 -> 17
    38 -> 3
    39 -> 2
    _ -> -1
  }
}

fn auxiliary_constructor_is_exact(data: Data, expected_tag: Int) -> Bool {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  and {
    tag == expected_tag,
    list.length(fields) == auxiliary_arity(expected_tag),
  }
}

fn auxiliary_corpus_is_exact(items: List<Data>, expected_tag: Int) -> Bool {
  when items is {
    [] -> expected_tag == 40
    [item, ..rest] -> and {
        auxiliary_constructor_is_exact(item, expected_tag),
        auxiliary_corpus_is_exact(rest, expected_tag + 1),
      }
  }
}

test canonical_validation_controls_v1_typescript_abi_vectors() {
  expect Some(auxiliary_data) = cbor.deserialise(auxiliary_corpus_cbor)
  let auxiliary_witnesses = builtin.un_list_data(auxiliary_data)
  let discovery = validation_machine_v1.empty_script_discovery_control()
  let native = native_control()
  let summary = nonempty_summary()
  let empty = empty_summary()
  let sequence = nonempty_sequence()
  let redeemer_control = validation_machine_v1.CekRedeemerContextControlV1 {
    cursor: 1,
    map_items: sequence,
    active_scan_hash: h53,
    active_redeemer_leaf: h54,
    active_purpose: empty,
    current_redeemer: summary,
  }
  let final_control = validation_machine_v1.CekFinalContextControlV1 {
    tx_info: summary,
    redeemer: summary,
    script_info: summary,
  }
  let parts_control = validation_machine_v1.CekContextPartsControlV1 {
    redeemer_items: sequence,
    redeemer: summary,
    script_info: summary,
  }
  let assembly_control = validation_machine_v1.CekTxInfoAssemblyControlV1 {
    tail_fields: sequence,
    redeemer: summary,
    script_info: summary,
  }
  and {
    auxiliary_corpus_is_exact(auxiliary_witnesses, 0),
    cbor.serialise(auxiliary_data) == auxiliary_corpus_cbor,
    blake2b_256(auxiliary_corpus_cbor) == #"255f802737c82d7cb6515ca4f1bcd416bbb79877706d5f6e2d36de7123ef9b33",
    validation_machine_v1.encode_resolve_inputs_witness(
      #"01",
      #"02",
      #"03",
      #"04",
      0,
      h41,
      h42,
      0,
      h43,
      None,
      h44,
    ) == #"8b41014102410341040058204141414141414141414141414141414141414141414141414141414141414141582042424242424242424242424242424242424242424242424242424242424242420058204343434343434343434343434343434343434343434343434343434343434343410058204444444444444444444444444444444444444444444444444444444444444444",
    validation_machine_v1.encode_script_discovery_control(discovery) == #"8f000000202040402020400000008040",
    validation_machine_v1.encode_script_sources_witness(
      #"01",
      #"02",
      #"03",
      #"04",
      0,
      h45,
      0,
      h46,
      [],
      0,
      0,
      [],
      0,
      [],
      0,
      h47,
      h48,
      0,
      0,
      [],
      0,
      0,
      [],
      0,
      empty_receive_scan(),
      0,
      0,
      empty_observer_scan(),
      empty_mint_fold(),
      h49,
    ) == #"981e41014102410341040058204545454545454545454545454545454545454545454545454545454545454545005820464646464646464646464646464646464646464646464646464646464646464680000080008000582047474747474747474747474747474747474747474747474747474747474747475820484848484848484848484848484848484848484848484848484848484848484800008000008000860080004040800000830040008c20004040004000000040008058204949494949494949494949494949494949494949494949494949494949494949",
    validation_machine_v1.encode_native_scripts_control_v1(native) == #"981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c",
    validation_machine_v1.encode_script_integrity_witness_v1(native) == #"825883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c00",
    validation_machine_v1.encode_script_integrity_compact_witness_v1(native) == #"825883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c01",
    validation_machine_v1.encode_script_integrity_witness_set_witness_v1(
      native,
      h4d,
      h4e,
    ) == #"845883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c0258204d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d58204e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e",
    validation_machine_v1.encode_script_integrity_finalize_witness_v1(
      native,
      h4f,
      h50,
    ) == #"845883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c0358204f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f58205050505050505050505050505050505050505050505050505050505050505050",
    validation_machine_v1.encode_cek_redeemer_context_control_v1(
      redeemer_control,
    ) == #"8601845820525252525252525252525252525252525252525252525252525252525252525201010158205353535353535353535353535353535353535353535353535353535353535353582054545454545454545454545454545454545454545454545454545454545454548340000083582051515151515151515151515151515151515151515151515151515151515151510101",
    validation_machine_v1.hash_cek_redeemer_context_control_v1(redeemer_control) == #"3dfab23fb96dece2da964d3b0b62ef26006400b04b676b6ccfc18ac5da438c10",
    validation_machine_v1.encode_cek_final_context_control_v1(final_control) == #"83835820515151515151515151515151515151515151515151515151515151515151515101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
    validation_machine_v1.hash_cek_final_context_control_v1(final_control) == #"a4fdda392c9324034244f6b4674441a320d90d819521ccc9b62ff37c0dfdc10b",
    validation_machine_v1.encode_cek_context_parts_control_v1(parts_control) == #"8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
    validation_machine_v1.hash_cek_context_parts_control_v1(parts_control) == #"1e8e3ea65ea7e762512207ea4276022ce321332ee4c2f6bdf1b7329bd1baa962",
    validation_machine_v1.encode_cek_tx_info_assembly_control_v1(
      assembly_control,
    ) == #"8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
    validation_machine_v1.hash_cek_tx_info_assembly_control_v1(assembly_control) == #"7e848c60ea1a41e1d8d90d38c9034b78ce2c0b55e5e0cba7620bb0d3f909e674",
  }
}

test validation_auxiliary_v1_rejects_adjacent_tag_40() fail {
  expect Some(data) = cbor.deserialise(#"d9052180")
  expect auxiliary_constructor_is_exact(data, 40)
  True
}

test validation_auxiliary_v1_rejects_wrong_constructor_arity() fail {
  expect Some(data) = cbor.deserialise(#"d8799f00ff")
  expect auxiliary_constructor_is_exact(data, 0)
  True
}

test resolve_inputs_v1_rejects_malformed_accumulator() fail {
  validation_machine_v1.encode_resolve_inputs_witness(
    #"01",
    #"02",
    #"03",
    #"04",
    0,
    #"41414141414141414141414141414141414141414141414141414141414141",
    h42,
    0,
    h43,
    None,
    h44,
  ) == #""
}

test script_sources_v1_rejects_negative_stage() fail {
  validation_machine_v1.encode_script_sources_witness(
    #"01",
    #"02",
    #"03",
    #"04",
    0,
    h45,
    0,
    h46,
    [],
    -1,
    0,
    [],
    0,
    [],
    0,
    h47,
    h48,
    0,
    0,
    [],
    0,
    0,
    [],
    0,
    empty_receive_scan(),
    0,
    0,
    empty_observer_scan(),
    empty_mint_fold(),
    h49,
  ) == #""
}

test native_scripts_v1_rejects_short_integrity_hash() fail {
  validation_machine_v1.encode_script_integrity_witness_set_witness_v1(
    native_control(),
    #"00",
    h4e,
  ) == #""
}

test cek_context_v1_rejects_empty_final_summary() fail {
  let empty = empty_summary()
  validation_machine_v1.hash_cek_final_context_control_v1(
    validation_machine_v1.CekFinalContextControlV1 {
      tx_info: empty,
      redeemer: empty,
      script_info: empty,
    },
  ) == #""
}
