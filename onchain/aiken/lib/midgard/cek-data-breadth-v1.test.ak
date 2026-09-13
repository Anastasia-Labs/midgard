use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/validation_merkle_v1

fn summary(
  root: ByteArray,
  cbor_length: Int,
  memory: Int,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root, cbor_length, memory }
}

fn peak(height: Int, hash: ByteArray) -> validation_merkle_v1.FrontierPeak {
  validation_merkle_v1.FrontierPeak { height, hash }
}

fn frame(
  kind: Int,
  constructor_cbor_root: ByteArray,
  constructor_cbor_length: Int,
  constructor_memory: Int,
  expected_children: Int,
  child_peaks: List<validation_merkle_v1.FrontierPeak>,
  fold_cursor: Int,
  sequence_root: ByteArray,
  sequence_length: Int,
  payload_cbor_length: Int,
  sequence_memory: Int,
) -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind,
    constructor: 0,
    constructor_cbor_root,
    constructor_cbor_length,
    constructor_memory,
    tail: #"",
    expected_children,
    child_count: expected_children,
    child_peaks,
    fold_cursor,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      root: sequence_root,
      length: sequence_length,
      payload_cbor_length,
      memory: sequence_memory,
    },
  }
}

fn unit_summary() -> cek_data_v1.DataSummaryV1 {
  summary(
    #"0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2",
    1,
    5,
  )
}

fn broad_list_siblings() -> List<ByteArray> {
  [
    #"ab2a972c81b27a008e51e7bfc21fa624480c25a10ef4cf8b2fc4126bede787a0",
    #"d335dd87d228b2aa7b1151e9ab91c5c0699453dc85d749f99c30bf2c136c8113",
    #"df44478b5bde4ecf70e2a07efe7b6bd3e1f61fb2b1ef83e3b259ddf1e2adea2d",
    #"87f8eb1b3ae3706b7ac4ae9a3645aacc8ba9f8420b6525a4d7ca80e06ecf1cfc",
    #"5eba7fe157aea07d5009512556d5efc706a3bdb80c2078d84198d98fb5d5b6fc",
    #"01495dc87dadb536912d40c6d64d714335aa2aac0efae101bfc550bde531bb7b",
    #"728b21775172eed0ad58d1cdc095ca8cf2833dfeca327ce050cb940bc48f2ecb",
    #"188ec8994e9bf0e212b106d8c82d0660ea3e35bd3b21f2e07ca49e1c6e97bd36",
    #"103ee780ab37963f93630784f513a345a679d0dbea43345ad232255490a869ed",
    #"e9c15cef679bfd1f8fb5efbd4735a3db216ecb2718b768a7adf22c79ab6f7274",
    #"7a6a5674af79f93e6709a502a46b6ae3010cb1a59afaa957491389b9052ca2c0",
    #"4706813a35b188f4ddbf2ba007a43310918357ceb859220a510a48d6a228f254",
    #"f819c60cad2e369ea86b906a87c2af5585714c072d0b67e2be7d143492d8fe25",
  ]
}

fn replace_first_sibling(
  siblings: List<ByteArray>,
  mutated_first: ByteArray,
) -> List<ByteArray> {
  expect [_, ..rest] = siblings
  [mutated_first, ..rest]
}

fn list_frontier_agrees(
  pre_control_cbor: ByteArray,
  expected_frame_cbor: ByteArray,
  exact_frame: cek_data_frame_v1.DataFrameV1,
  child_index: Int,
  siblings: List<ByteArray>,
  expected_post_control_cbor: ByteArray,
) -> Bool {
  let child = unit_summary()
  let pre = cek_data_traverse_v1.decode_control_v1(pre_control_cbor)
  expect Some(post) = cek_data_traverse_v1.step_v1(
    pre,
    None,
    cek_data_traverse_v1.FoldList {
      frame: exact_frame,
      child_index,
      child,
      siblings,
    },
  )
  and {
    cek_data_frame_v1.encode_frame_v1(exact_frame) == expected_frame_cbor,
    cek_data_traverse_v1.encode_control_v1(post) == expected_post_control_cbor,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FoldList {
        frame: exact_frame,
        child_index: child_index - 1,
        child,
        siblings,
      },
    ) == None,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FoldList {
        frame: exact_frame,
        child_index,
        child,
        siblings: replace_first_sibling(
          siblings,
          #"aa2a972c81b27a008e51e7bfc21fa624480c25a10ef4cf8b2fc4126bede787a0",
        ),
      },
    ) == None,
  }
}

fn map_frontier_agrees(
  pre_control_cbor: ByteArray,
  expected_frame_cbor: ByteArray,
  exact_frame: cek_data_frame_v1.DataFrameV1,
  pair_index: Int,
  key: cek_data_v1.DataSummaryV1,
  value: cek_data_v1.DataSummaryV1,
  key_siblings: List<ByteArray>,
  value_siblings: List<ByteArray>,
  mutated_key_first: ByteArray,
  mutated_value_first: ByteArray,
  expected_post_control_cbor: ByteArray,
) -> Bool {
  let pre = cek_data_traverse_v1.decode_control_v1(pre_control_cbor)
  expect Some(post) = cek_data_traverse_v1.step_v1(
    pre,
    None,
    cek_data_traverse_v1.FoldMap {
      frame: exact_frame,
      pair_index,
      key,
      value,
      key_siblings,
      value_siblings,
    },
  )
  and {
    cek_data_frame_v1.encode_frame_v1(exact_frame) == expected_frame_cbor,
    cek_data_traverse_v1.encode_control_v1(post) == expected_post_control_cbor,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FoldMap {
        frame: exact_frame,
        pair_index: pair_index - 1,
        key,
        value,
        key_siblings,
        value_siblings,
      },
    ) == None,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FoldMap {
        frame: exact_frame,
        pair_index,
        key,
        value,
        key_siblings: replace_first_sibling(key_siblings, mutated_key_first),
        value_siblings,
      },
    ) == None,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FoldMap {
        frame: exact_frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings: replace_first_sibling(
          value_siblings,
          mutated_value_first,
        ),
      },
    ) == None,
  }
}

fn terminal_agrees(
  pre_control_cbor: ByteArray,
  expected_frame_cbor: ByteArray,
  exact_frame: cek_data_frame_v1.DataFrameV1,
  expected_post_control_cbor: ByteArray,
  expected_root: ByteArray,
  expected_cbor_length: Int,
  expected_memory: Int,
  mutated_sequence_root: ByteArray,
) -> Bool {
  let pre = cek_data_traverse_v1.decode_control_v1(pre_control_cbor)
  expect Some(post) = cek_data_traverse_v1.step_v1(
    pre,
    None,
    cek_data_traverse_v1.FinalizeFrame { frame: exact_frame, parent: None },
  )
  expect Some(exact_summary) = cek_data_traverse_v1.finalize_v1(post)
  let mutated_frame = cek_data_frame_v1.DataFrameV1 {
    ..exact_frame,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      ..exact_frame.sequence,
      root: mutated_sequence_root,
    },
  }
  and {
    cek_data_frame_v1.encode_frame_v1(exact_frame) == expected_frame_cbor,
    cek_data_traverse_v1.encode_control_v1(post) == expected_post_control_cbor,
    exact_summary.root == expected_root,
    exact_summary.cbor_length == expected_cbor_length,
    exact_summary.memory == expected_memory,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FinalizeFrame { frame: mutated_frame, parent: None },
    ) == None,
  }
}

test datum_constructor_frontier_agrees() {
  list_frontier_agrees(
    #"8a01061831193f2d193f2d5820b4e51a813e0db08144e69ffc377ab17348ee60c68e4a10204db19d19ac4c164fd87a80d87a80d87a80d87a80",
    #"8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193f26193f268982015820efe8f3010418beaf5549119f466aed141636fcf69a3bd24a7256a2d9b73591268202582071bcb569dc0f82be3304bbf21d970e6dfa45ba658d55a3fd26d8c6128604f9078205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191f2684582068bdcf182521f7e6dbfd0522ac74ac3f3f1cae35e2e44ffbb03b87b35354b46c191f26191f26199bbe",
    frame(
      cek_data_frame_v1.constr_large_frame,
      #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
      2,
      6,
      16166,
      [
        peak(
          1,
          #"efe8f3010418beaf5549119f466aed141636fcf69a3bd24a7256a2d9b7359126",
        ),
        peak(
          2,
          #"71bcb569dc0f82be3304bbf21d970e6dfa45ba658d55a3fd26d8c6128604f907",
        ),
        peak(
          5,
          #"14aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb",
        ),
        peak(
          8,
          #"c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc11",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      7974,
      #"68bdcf182521f7e6dbfd0522ac74ac3f3f1cae35e2e44ffbb03b87b35354b46c",
      7974,
      7974,
      39870,
    ),
    8191,
    broad_list_siblings(),
    #"8a01061831193f2d193f2d58207427b8b1fb4b4bb54bcfc605f0460e416caa00ee13314c878b7370b747e95c01d87a80d87a80d87a80d87a80",
  )
}

test datum_constructor_terminal_agrees() {
  terminal_agrees(
    #"8a01061831193f2d193f2d582026d0f372cf8e7fc0eea66dcc131c5f6e288560e31a048dc87968548a00ab7970d87a80d87a80d87a80d87a80",
    #"8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193f26193f268982015820efe8f3010418beaf5549119f466aed141636fcf69a3bd24a7256a2d9b73591268202582071bcb569dc0f82be3304bbf21d970e6dfa45ba658d55a3fd26d8c6128604f9078205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193f26845820e32b0616afebaedfeac9da563c2513bc53853b891d194fe57e2392b1a0e70a24193f26193f261a00013bbe",
    frame(
      cek_data_frame_v1.constr_large_frame,
      #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
      2,
      6,
      16166,
      [
        peak(
          1,
          #"efe8f3010418beaf5549119f466aed141636fcf69a3bd24a7256a2d9b7359126",
        ),
        peak(
          2,
          #"71bcb569dc0f82be3304bbf21d970e6dfa45ba658d55a3fd26d8c6128604f907",
        ),
        peak(
          5,
          #"14aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb",
        ),
        peak(
          8,
          #"c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc11",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      16166,
      #"e32b0616afebaedfeac9da563c2513bc53853b891d194fe57e2392b1a0e70a24",
      16166,
      16166,
      80830,
    ),
    #"8a01071831193f2d193f2d40d87a80d87a80d87a80d8799f8358207b303304d0263d23d7b6a26c2596af652b0d0909c126cb8a7da4559bc8eb6d4a193f2d1a00013bc2ff",
    #"7b303304d0263d23d7b6a26c2596af652b0d0909c126cb8a7da4559bc8eb6d4a",
    16173,
    80834,
    #"e22b0616afebaedfeac9da563c2513bc53853b891d194fe57e2392b1a0e70a24",
  )
}

test datum_list_frontier_agrees() {
  list_frontier_agrees(
    #"8a01061831193f2d193f2d582081e863eecbabad08d04e6e1c653b05dfa61ea10fb4f3ba1738271ed95f998db5d87a80d87a80d87a80d87a80",
    #"8b020040000040193f2b193f2b8a820058206ba167794ebd11ca5626c5adcbeb856de4458e5b91f6a9a3390ccb4294ee863c82015820d494f923c728c631f44776b8286db5cd8a1bf5748c40f7cb7a8059aaa2babe8d82035820d1adbec1a123cde6a98e75420abb4ab7a0e3d3f2f5aa43aab2eb977c0ea2c33b8205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191f2b8458205e52f38a5649f614cb32ceb5bbc7dce1a3c8dd2e8d9b3bab1dad0f1503cf18b8191f2b191f2b199bd7",
    frame(
      cek_data_frame_v1.list_frame,
      #"",
      0,
      0,
      16171,
      [
        peak(
          0,
          #"6ba167794ebd11ca5626c5adcbeb856de4458e5b91f6a9a3390ccb4294ee863c",
        ),
        peak(
          1,
          #"d494f923c728c631f44776b8286db5cd8a1bf5748c40f7cb7a8059aaa2babe8d",
        ),
        peak(
          3,
          #"d1adbec1a123cde6a98e75420abb4ab7a0e3d3f2f5aa43aab2eb977c0ea2c33b",
        ),
        peak(
          5,
          #"14aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb",
        ),
        peak(
          8,
          #"c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc11",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      7979,
      #"5e52f38a5649f614cb32ceb5bbc7dce1a3c8dd2e8d9b3bab1dad0f1503cf18b8",
      7979,
      7979,
      39895,
    ),
    8191,
    broad_list_siblings(),
    #"8a01061831193f2d193f2d5820d6a49cd6c3355bd06dfb354023be99abec63fb0f2ebeb19db47679ba92f0939ad87a80d87a80d87a80d87a80",
  )
}

test datum_list_terminal_agrees() {
  terminal_agrees(
    #"8a01061831193f2d193f2d5820af079bcf11e4599a3004f73daba99841c1b61c54dd59b6d30e0d614ae075f8ffd87a80d87a80d87a80d87a80",
    #"8b020040000040193f2b193f2b8a820058206ba167794ebd11ca5626c5adcbeb856de4458e5b91f6a9a3390ccb4294ee863c82015820d494f923c728c631f44776b8286db5cd8a1bf5748c40f7cb7a8059aaa2babe8d82035820d1adbec1a123cde6a98e75420abb4ab7a0e3d3f2f5aa43aab2eb977c0ea2c33b8205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193f2b84582063ddf7a530c9d366defd7da8dfec2c9058188289265fd7529b018f60b8ff809a193f2b193f2b1a00013bd7",
    frame(
      cek_data_frame_v1.list_frame,
      #"",
      0,
      0,
      16171,
      [
        peak(
          0,
          #"6ba167794ebd11ca5626c5adcbeb856de4458e5b91f6a9a3390ccb4294ee863c",
        ),
        peak(
          1,
          #"d494f923c728c631f44776b8286db5cd8a1bf5748c40f7cb7a8059aaa2babe8d",
        ),
        peak(
          3,
          #"d1adbec1a123cde6a98e75420abb4ab7a0e3d3f2f5aa43aab2eb977c0ea2c33b",
        ),
        peak(
          5,
          #"14aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb",
        ),
        peak(
          8,
          #"c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc11",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      16171,
      #"63ddf7a530c9d366defd7da8dfec2c9058188289265fd7529b018f60b8ff809a",
      16171,
      16171,
      80855,
    ),
    #"8a01071831193f2d193f2d40d87a80d87a80d87a80d8799f835820af1bd84873581d7cd6042fd677fc42508c943d337970e6bc7ef1f16e8d3f1652193f2d1a00013bdbff",
    #"af1bd84873581d7cd6042fd677fc42508c943d337970e6bc7ef1f16e8d3f1652",
    16173,
    80859,
    #"62ddf7a530c9d366defd7da8dfec2c9058188289265fd7529b018f60b8ff809a",
  )
}

test datum_map_frontier_agrees() {
  map_frontier_agrees(
    #"8a01061831193f2b193f2b5820e2b34612aada46585b509cdfbdde9662639edee29b7cb649d8ca33f64e7137ccd87a80d87a80d87a80d87a80",
    #"8b03004000004019202019202082820558202c04aeaacfa69cd0155a4de323c3c002871b2ae45004526459611075a5223a20820d5820364791a54122388cbfc3d2923152cc12ffed5a40c08657fedf959ba66dcfe79b10845820440882f7141af971b11729a306e3bb7f86c3e073d2847cafccb584749440670410184018b0",
    frame(
      cek_data_frame_v1.map_frame,
      #"",
      0,
      0,
      8224,
      [
        peak(
          5,
          #"2c04aeaacfa69cd0155a4de323c3c002871b2ae45004526459611075a5223a20",
        ),
        peak(
          13,
          #"364791a54122388cbfc3d2923152cc12ffed5a40c08657fedf959ba66dcfe79b",
        ),
      ],
      16,
      #"440882f7141af971b11729a306e3bb7f86c3e073d2847cafccb5847494406704",
      16,
      64,
      176,
    ),
    4095,
    summary(
      #"bc49f33ecd4551db7a0ca279af9ee46f50789d7ac4c7f9971914c048fa1e5c16",
      3,
      6,
    ),
    unit_summary(),
    [
      #"fbf47176a866dbbe9f0abaed5089e8f98310b409ba158e5886c2062f5db8032f",
      #"a02aa4ae212513cad8b63e01fb30e5488f9c492a3d4a811e3f9162d1f0c1a616",
      #"ef6b44bc646b33107c750304b749342525346ffddd8dce5ce1cdc1e77cc8c1e7",
      #"700a93e0057e26a4ea4dae231e16fdf824d07acee08a979d4759d9418acfceb9",
      #"58f86bac0e70c28d08cbbed4cf5496b064ffdbad4dc0cfab13a4fd65748611e5",
      #"ecbb22a3a1d75307d2fd9d90775b8b23f8db09d9b1f47a5e82f618ac51c5a9b3",
      #"70e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a58",
      #"157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a",
      #"d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181",
      #"1c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b",
      #"01df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce",
      #"31e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d",
      #"68e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314",
    ],
    [
      #"cbe38915df4c2a785be65ecce73240d5eec035faeb54c1c21190c5adf4fd203c",
      #"a02aa4ae212513cad8b63e01fb30e5488f9c492a3d4a811e3f9162d1f0c1a616",
      #"ef6b44bc646b33107c750304b749342525346ffddd8dce5ce1cdc1e77cc8c1e7",
      #"700a93e0057e26a4ea4dae231e16fdf824d07acee08a979d4759d9418acfceb9",
      #"58f86bac0e70c28d08cbbed4cf5496b064ffdbad4dc0cfab13a4fd65748611e5",
      #"ecbb22a3a1d75307d2fd9d90775b8b23f8db09d9b1f47a5e82f618ac51c5a9b3",
      #"70e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a58",
      #"157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a",
      #"d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181",
      #"1c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b",
      #"01df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce",
      #"31e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d",
      #"68e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314",
    ],
    #"faf47176a866dbbe9f0abaed5089e8f98310b409ba158e5886c2062f5db8032f",
    #"cae38915df4c2a785be65ecce73240d5eec035faeb54c1c21190c5adf4fd203c",
    #"8a01061831193f2b193f2b5820cc9b98c851406eee37c0d72c6547320a7c629efac14eeea20cf22ea7f42ae90fd87a80d87a80d87a80d87a80",
  )
}

test datum_map_terminal_agrees() {
  terminal_agrees(
    #"8a01061831193f2b193f2b5820501699a6d4f2648cec8cb75f23edaba0009816fdaeba9fb575561fdd121bed75d87a80d87a80d87a80d87a80",
    #"8b03004000004019202019202082820558202c04aeaacfa69cd0155a4de323c3c002871b2ae45004526459611075a5223a20820d5820364791a54122388cbfc3d2923152cc12ffed5a40c08657fedf959ba66dcfe79b1910108458205609775593de4a6045c86efde1bb729c30b40b56835fba9e23b2aab8188d764c191010193f2819b030",
    frame(
      cek_data_frame_v1.map_frame,
      #"",
      0,
      0,
      8224,
      [
        peak(
          5,
          #"2c04aeaacfa69cd0155a4de323c3c002871b2ae45004526459611075a5223a20",
        ),
        peak(
          13,
          #"364791a54122388cbfc3d2923152cc12ffed5a40c08657fedf959ba66dcfe79b",
        ),
      ],
      4112,
      #"5609775593de4a6045c86efde1bb729c30b40b56835fba9e23b2aab8188d764c",
      4112,
      16168,
      45104,
    ),
    #"8a01071831193f2b193f2b40d87a80d87a80d87a80d8799f83582027bf1066c2477eaae456420999f6b6f83139dbce2502cea5424a0924db643581193f2b19b034ff",
    #"27bf1066c2477eaae456420999f6b6f83139dbce2502cea5424a0924db643581",
    16171,
    45108,
    #"5709775593de4a6045c86efde1bb729c30b40b56835fba9e23b2aab8188d764c",
  )
}

test redeemer_constructor_frontier_agrees() {
  list_frontier_agrees(
    #"8a010606193e70193e70582075b4bbdd15c02fe5e282d357c2748627bf9a9c65728c9355609fa8d0a461b674d87a80d87a80d87a80d87a80",
    #"8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193e69193e698982005820a87a488e10297529b04c01a69c4634c480a41d277730fbc6824c3bf91ff16ebb82035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191e69845820db769f6bbcb7b2cf462ce920a89553ecc3b0ab0264c5bf421abaf02c8039428b191e69191e6919980d",
    frame(
      cek_data_frame_v1.constr_large_frame,
      #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
      2,
      6,
      15977,
      [
        peak(
          0,
          #"a87a488e10297529b04c01a69c4634c480a41d277730fbc6824c3bf91ff16ebb",
        ),
        peak(
          3,
          #"f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da",
        ),
        peak(
          5,
          #"d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa",
        ),
        peak(
          6,
          #"bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da3",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      7785,
      #"db769f6bbcb7b2cf462ce920a89553ecc3b0ab0264c5bf421abaf02c8039428b",
      7785,
      7785,
      38925,
    ),
    8191,
    broad_list_siblings(),
    #"8a010606193e70193e70582087f4606562f56ac4eba1adff36e216bbf013a02d9c39e3ecb45fe4dcbb625ba2d87a80d87a80d87a80d87a80",
  )
}

test redeemer_constructor_terminal_agrees() {
  terminal_agrees(
    #"8a010606193e70193e70582029201c895676ea4211866a5ab8183f6bc1fa37ce08e4a950b70d7bb095d8c9c2d87a80d87a80d87a80d87a80",
    #"8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193e69193e698982005820a87a488e10297529b04c01a69c4634c480a41d277730fbc6824c3bf91ff16ebb82035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193e6984582006de6a3a6db23a497f9a88a768d15d6f7ff63ec40cee256326ce3a4e57f7fb46193e69193e691a0001380d",
    frame(
      cek_data_frame_v1.constr_large_frame,
      #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
      2,
      6,
      15977,
      [
        peak(
          0,
          #"a87a488e10297529b04c01a69c4634c480a41d277730fbc6824c3bf91ff16ebb",
        ),
        peak(
          3,
          #"f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da",
        ),
        peak(
          5,
          #"d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa",
        ),
        peak(
          6,
          #"bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da3",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      15977,
      #"06de6a3a6db23a497f9a88a768d15d6f7ff63ec40cee256326ce3a4e57f7fb46",
      15977,
      15977,
      79885,
    ),
    #"8a010706193e70193e7040d87a80d87a80d87a80d8799f83582068202260a6d772b441b247400f69fa7799355da692120ac670cb7547b01bb745193e701a00013811ff",
    #"68202260a6d772b441b247400f69fa7799355da692120ac670cb7547b01bb745",
    15984,
    79889,
    #"07de6a3a6db23a497f9a88a768d15d6f7ff63ec40cee256326ce3a4e57f7fb46",
  )
}

test redeemer_list_frontier_agrees() {
  list_frontier_agrees(
    #"8a010606193e70193e7058200ed22209f9048bfb21061585a17b038e1a02dac8247d79f9c3981383a664ef39d87a80d87a80d87a80d87a80",
    #"8b020040000040193e6e193e6e8a82015820e3b249eefffab2fa881b2cec5ab9934a3fecb09dfd9717a97349e76fe7a002a6820258206c990dab77d0f16c66eee567a54bbbc774ff06e7fc647010bcd2e0f366d046f382035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191e6e845820a105d7dbe5bc6de2fd32a3415fa52b56c67703652b147f722d4218f6132f6ce4191e6e191e6e199826",
    frame(
      cek_data_frame_v1.list_frame,
      #"",
      0,
      0,
      15982,
      [
        peak(
          1,
          #"e3b249eefffab2fa881b2cec5ab9934a3fecb09dfd9717a97349e76fe7a002a6",
        ),
        peak(
          2,
          #"6c990dab77d0f16c66eee567a54bbbc774ff06e7fc647010bcd2e0f366d046f3",
        ),
        peak(
          3,
          #"f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da",
        ),
        peak(
          5,
          #"d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa",
        ),
        peak(
          6,
          #"bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da3",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      7790,
      #"a105d7dbe5bc6de2fd32a3415fa52b56c67703652b147f722d4218f6132f6ce4",
      7790,
      7790,
      38950,
    ),
    8191,
    broad_list_siblings(),
    #"8a010606193e70193e7058207efd673d3c799af342565968e2a6686452d94ab3ce9efdd369f0fe18bef3aef3d87a80d87a80d87a80d87a80",
  )
}

test redeemer_list_terminal_agrees() {
  terminal_agrees(
    #"8a010606193e70193e705820463b8f5c39a691bdecc4e9bd7d02b21c97c599b5d789621460a5aa611e0da11ad87a80d87a80d87a80d87a80",
    #"8b020040000040193e6e193e6e8a82015820e3b249eefffab2fa881b2cec5ab9934a3fecb09dfd9717a97349e76fe7a002a6820258206c990dab77d0f16c66eee567a54bbbc774ff06e7fc647010bcd2e0f366d046f382035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193e6e845820c98415365fccc4c818ac3e255707782ef7d850c5fa3f84f1b5d3ad007691d8b8193e6e193e6e1a00013826",
    frame(
      cek_data_frame_v1.list_frame,
      #"",
      0,
      0,
      15982,
      [
        peak(
          1,
          #"e3b249eefffab2fa881b2cec5ab9934a3fecb09dfd9717a97349e76fe7a002a6",
        ),
        peak(
          2,
          #"6c990dab77d0f16c66eee567a54bbbc774ff06e7fc647010bcd2e0f366d046f3",
        ),
        peak(
          3,
          #"f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da",
        ),
        peak(
          5,
          #"d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa",
        ),
        peak(
          6,
          #"bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da3",
        ),
        peak(
          9,
          #"84bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2",
        ),
        peak(
          10,
          #"d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d",
        ),
        peak(
          11,
          #"0bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124",
        ),
        peak(
          12,
          #"6963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f",
        ),
        peak(
          13,
          #"936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429",
        ),
      ],
      15982,
      #"c98415365fccc4c818ac3e255707782ef7d850c5fa3f84f1b5d3ad007691d8b8",
      15982,
      15982,
      79910,
    ),
    #"8a010706193e70193e7040d87a80d87a80d87a80d8799f835820e267f1612bd596a10ed967889fe81efb005fdf09c6651720f0e87715774d35d5193e701a0001382aff",
    #"e267f1612bd596a10ed967889fe81efb005fdf09c6651720f0e87715774d35d5",
    15984,
    79914,
    #"c88415365fccc4c818ac3e255707782ef7d850c5fa3f84f1b5d3ad007691d8b8",
  )
}

test redeemer_map_frontier_agrees() {
  map_frontier_agrees(
    #"8a010606193e6f193e6f58205441034e8f9e79524aa4961daa3564b41e27fe9d4e08aa3f1eff491cde3fe1efd87a80d87a80d87a80d87a80",
    #"8b030040000040191fc2191fc28882015820ef30dea6d2c17a299ef4549031e2516ea9e5a5b792a0d84cbf25d15b72a5f0bd8206582070e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a5882075820157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a82085820d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181820958201c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b820a582001df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce820b582031e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d820c582068e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d401343141907e18458202d3237b8bd77ed407f3048c41d73e35d8861e3ac1c3311f4b33c574c22f69e591907e1191f841956ab",
    frame(
      cek_data_frame_v1.map_frame,
      #"",
      0,
      0,
      8130,
      [
        peak(
          1,
          #"ef30dea6d2c17a299ef4549031e2516ea9e5a5b792a0d84cbf25d15b72a5f0bd",
        ),
        peak(
          6,
          #"70e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a58",
        ),
        peak(
          7,
          #"157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a",
        ),
        peak(
          8,
          #"d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181",
        ),
        peak(
          9,
          #"1c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b",
        ),
        peak(
          10,
          #"01df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce",
        ),
        peak(
          11,
          #"31e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d",
        ),
        peak(
          12,
          #"68e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314",
        ),
      ],
      2017,
      #"2d3237b8bd77ed407f3048c41d73e35d8861e3ac1c3311f4b33c574c22f69e59",
      2017,
      8068,
      22187,
    ),
    2047,
    summary(
      #"4f0c41d3a758682c3f345581483b1f05f8dc1cb4b7a75b7c7379dc98e1833d8a",
      3,
      6,
    ),
    unit_summary(),
    [
      #"7baae1975ecb5118c570c8dca87afd59c7b504ff383f9315669f212557c976e1",
      #"15f439dde5c4396dde95691bf3e4e1806dfc2c48ae5ef373fb39701279b873c1",
      #"9e10aa07c1263733115d5fe0fb8a7ac2e9ad6eb13c2f1b047906c7fbaae93c7f",
      #"23c71c630aca0f74959719e6ac08c5bc8f1dce9f7d85517089a1aadf8772e2f3",
      #"d57b256ebdc7916c9a661f6641f49d55756962ab05fe2d777db16174e7370334",
      #"cb50da584379e82471566e77ad56ea60a65acd218e6e65c0770ee0fb571e5f6c",
      #"a935a034cdd1f046a665efc1dbe926d1a6337b0c29038fc7f608b3e05b500283",
      #"a39fd0f277153d1a4d279fc8b13c15e4fe872077d0c348d1fd9f420dfd9a3606",
      #"76df3f162ec7bf8c10fbc88fa71dfd42759cdc7846f09e3ea2b74558afc9f7df",
      #"3aa0206c9a518a16b5ac79597cc0245f241b44d8602cd0d2805ec23cd7f23db4",
      #"83e00ebb50f88562c1ce30345925d3c3f25aaaed82623b722c02d895cac884ab",
      #"f58c85354a6e958f0f5f0cb36d9111d58f29b0aac8eb48eb36460b9abc9c93a3",
    ],
    [
      #"66b81fbe02ed0e9419f22881a3d07fd1bf07d9ea5c4c826079dd29ed6743cdf6",
      #"15f439dde5c4396dde95691bf3e4e1806dfc2c48ae5ef373fb39701279b873c1",
      #"9e10aa07c1263733115d5fe0fb8a7ac2e9ad6eb13c2f1b047906c7fbaae93c7f",
      #"23c71c630aca0f74959719e6ac08c5bc8f1dce9f7d85517089a1aadf8772e2f3",
      #"d57b256ebdc7916c9a661f6641f49d55756962ab05fe2d777db16174e7370334",
      #"cb50da584379e82471566e77ad56ea60a65acd218e6e65c0770ee0fb571e5f6c",
      #"a935a034cdd1f046a665efc1dbe926d1a6337b0c29038fc7f608b3e05b500283",
      #"a39fd0f277153d1a4d279fc8b13c15e4fe872077d0c348d1fd9f420dfd9a3606",
      #"76df3f162ec7bf8c10fbc88fa71dfd42759cdc7846f09e3ea2b74558afc9f7df",
      #"3aa0206c9a518a16b5ac79597cc0245f241b44d8602cd0d2805ec23cd7f23db4",
      #"83e00ebb50f88562c1ce30345925d3c3f25aaaed82623b722c02d895cac884ab",
      #"f58c85354a6e958f0f5f0cb36d9111d58f29b0aac8eb48eb36460b9abc9c93a3",
    ],
    #"7aaae1975ecb5118c570c8dca87afd59c7b504ff383f9315669f212557c976e1",
    #"67b81fbe02ed0e9419f22881a3d07fd1bf07d9ea5c4c826079dd29ed6743cdf6",
    #"8a010606193e6f193e6f58207e8007304fea4b6142e88a3ade6f06ebf4c5e6fc8f683897e0dd1d02319fa106d87a80d87a80d87a80d87a80",
  )
}

test redeemer_map_terminal_agrees() {
  terminal_agrees(
    #"8a010606193e6f193e6f582082b86d7d8db1d1517ef3ab50d1e5592af116a98566c8ec31df0c1840f0d9d2ecd87a80d87a80d87a80d87a80",
    #"8b030040000040191fc2191fc28882015820ef30dea6d2c17a299ef4549031e2516ea9e5a5b792a0d84cbf25d15b72a5f0bd8206582070e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a5882075820157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a82085820d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181820958201c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b820a582001df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce820b582031e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d820c582068e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314190fe1845820acf786ffedfff0505125acc7c2e8e3ca534993740205a60ec4a1771e4915ae18190fe1193e6c19ae2b",
    frame(
      cek_data_frame_v1.map_frame,
      #"",
      0,
      0,
      8130,
      [
        peak(
          1,
          #"ef30dea6d2c17a299ef4549031e2516ea9e5a5b792a0d84cbf25d15b72a5f0bd",
        ),
        peak(
          6,
          #"70e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a58",
        ),
        peak(
          7,
          #"157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a",
        ),
        peak(
          8,
          #"d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181",
        ),
        peak(
          9,
          #"1c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b",
        ),
        peak(
          10,
          #"01df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce",
        ),
        peak(
          11,
          #"31e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d",
        ),
        peak(
          12,
          #"68e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314",
        ),
      ],
      4065,
      #"acf786ffedfff0505125acc7c2e8e3ca534993740205a60ec4a1771e4915ae18",
      4065,
      15980,
      44587,
    ),
    #"8a010706193e6f193e6f40d87a80d87a80d87a80d8799f835820875b2886163d7c6869f44c94fcde51131baa99e550d7331cf25b517670f961a8193e6f19ae2fff",
    #"875b2886163d7c6869f44c94fcde51131baa99e550d7331cf25b517670f961a8",
    15983,
    44591,
    #"adf786ffedfff0505125acc7c2e8e3ca534993740205a60ec4a1771e4915ae18",
  )
}
