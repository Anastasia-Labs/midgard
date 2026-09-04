use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/validation_merkle_v1

const maximum_cardano_nested_data_pre_control =
  #"8a01061831193f2b193f2b582064c916e4a790b0d133bb36d04d9e9ecca7dc2f3c679690d2d005f84b311e9d94d87a80d87a80d87a80d87a80"

const maximum_cardano_nested_data_post_control =
  #"8a01071831193f2b193f2b40d87a80d87a80d87a80d8799f83582077156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f193f2b19bd6bff"

const maximum_cardano_nested_redeemer_pre_control =
  #"8a010600193e6e193e6e582008f6a2dc24df8fbc23b2d4255dda3ca30a2fd28eb361e9ab31bf01732c764eead87a80d87a80d87a80d87a80"

const maximum_cardano_nested_redeemer_post_control =
  #"8a010700193e6e193e6e40d87a80d87a80d87a80d8799f83582026ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20193e6e19bb34ff"

fn summary(
  root: ByteArray,
  cbor_length: Int,
  memory: Int,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root, cbor_length, memory }
}

fn sequence(
  root: ByteArray,
  length: Int,
  payload_cbor_length: Int,
  memory: Int,
) -> cek_data_v1.DataSequenceSummaryV1 {
  cek_data_v1.DataSequenceSummaryV1 {
    root,
    length,
    payload_cbor_length,
    memory,
  }
}

fn applied(
  pre_cbor: ByteArray,
  source: Option<ByteArray>,
  action: cek_data_traverse_v1.DataTraverseActionV1,
  post_cbor: ByteArray,
) -> Bool {
  let pre = cek_data_traverse_v1.decode_control_v1(pre_cbor)
  expect Some(post) = cek_data_traverse_v1.step_v1(pre, source, action)
  cek_data_traverse_v1.encode_control_v1(post) == post_cbor
}

test maximum_cardano_nested_data_applies_real_head_controls() {
  let large_pre = #"8a01001831193f2b0040d87a80d87a80d87a80d87a80"
  let large_source = #"d8668218809fa1d879809f9f9f9f"
  let large_post = #"8a01031831193f2b0340d8799f01ffd8799f86010018340200d87a80ffd87a80d87a80"
  let sequence_pre = #"8a01001831193f2b07582030d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01d87a80d87a80d87a80d87a80"
  let sequence_source = #"d879809f9f9f9f9f9f9f9f9f9f9f"
  let sequence_post = #"8a01061831193f2b0a5820df8903414ebd963ae3a2ed2ca1ec2f15b56e1017fe0701620626657b7fa92a12d87a80d87a80d87a80d87a80"
  let map_pre = #"8a01001831193f2b06582001afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fcd87a80d87a80d87a80d87a80"
  let map_source = #"a1d879809f9f9f9f9f9f9f9f9f9f"
  let map_post = #"8a01001831193f2b07582030d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01d87a80d87a80d87a80d87a80"
  let scalar_pre = #"8a01001831193f2b165820bf2bd3eb8fc61e7769e9f918c8cdcc9668c96c8be328284060e76931c62005b8d87a80d87a80d87a80d87a80"
  let scalar_source = #"410100ff9f009f0000ffffff9f9f"
  let scalar_post = #"8a01021831193f2b165820bf2bd3eb8fc61e7769e9f918c8cdcc9668c96c8be328284060e76931c62005b8d87a80d87a80d8799f86010018470200d87a80ffd87a80"
  and {
    applied(
      large_pre,
      Some(large_source),
      cek_data_traverse_v1.HeadLargeConstructor {
        constructor_cbor_length: 2,
        expected_children: 1,
      },
      large_post,
    ),
    applied(
      sequence_pre,
      Some(sequence_source),
      cek_data_traverse_v1.HeadSequence { expected_children: 0 },
      sequence_post,
    ),
    applied(map_pre, Some(map_source), cek_data_traverse_v1.HeadMap, map_post),
    applied(
      scalar_pre,
      Some(scalar_source),
      cek_data_traverse_v1.HeadScalar { item_length: 2 },
      scalar_post,
    ),
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.decode_control_v1(large_pre),
      Some(#"d8658218809fa1d879809f9f9f9f"),
      cek_data_traverse_v1.HeadLargeConstructor {
        constructor_cbor_length: 2,
        expected_children: 1,
      },
    ) == None,
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.decode_control_v1(sequence_pre),
      Some(sequence_source),
      cek_data_traverse_v1.HeadSequence { expected_children: 1 },
    ) == None,
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.decode_control_v1(map_pre),
      Some(#"9fd879809f9f9f9f9f9f9f9f9f9f"),
      cek_data_traverse_v1.HeadMap,
    ) == None,
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.decode_control_v1(scalar_pre),
      Some(scalar_source),
      cek_data_traverse_v1.HeadScalar { item_length: 0 },
    ) == None,
  }
}

fn terminal_frame(sequence_root: ByteArray) -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.constr_large_frame,
    constructor: 0,
    constructor_cbor_root: #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
    constructor_cbor_length: 2,
    constructor_memory: 6,
    tail: #"",
    expected_children: 1,
    child_count: 1,
    child_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"9f62b20d5db17ead31389c3864fb7ea3a2f68726b21ef48452c670dab47a8a66",
      },
    ],
    fold_cursor: 1,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      root: sequence_root,
      length: 1,
      payload_cbor_length: 16164,
      memory: 48487,
    },
  }
}

fn redeemer_terminal_frame(
  sequence_root: ByteArray,
) -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.constr_large_frame,
    constructor: 0,
    constructor_cbor_root: #"3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf",
    constructor_cbor_length: 2,
    constructor_memory: 6,
    tail: #"",
    expected_children: 1,
    child_count: 1,
    child_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"20de66bc0f1322c9c61884ce582d6698c9075e35c183a4264c6c7c27fbf1401b",
      },
    ],
    fold_cursor: 1,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      root: sequence_root,
      length: 1,
      payload_cbor_length: 15975,
      memory: 47920,
    },
  }
}

fn maximum_first_list_fold_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.list_frame,
    constructor: 0,
    constructor_cbor_root: #"",
    constructor_cbor_length: 0,
    constructor_memory: 0,
    tail: #"7f014d1984be15f9587409aa022afa8f6c4e1488924df30c012383e3bbf28e2c",
    expected_children: 2,
    child_count: 2,
    child_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 1,
        hash: #"1bc19900bb2341f556a93b216efdc1e85b076c82d3ef67f35f42367b491cc22d",
      },
    ],
    fold_cursor: 0,
    sequence: sequence(
      #"8c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56",
      0,
      0,
      0,
    ),
  }
}

fn maximum_map_fold_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.map_frame,
    constructor: 0,
    constructor_cbor_root: #"",
    constructor_cbor_length: 0,
    constructor_memory: 0,
    tail: #"01afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fc",
    expected_children: 2,
    child_count: 2,
    child_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 1,
        hash: #"1f137899e881e56f1776f5788ffd5cdb32aac1b7541970499657c0f354e942bc",
      },
    ],
    fold_cursor: 0,
    sequence: sequence(
      #"bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2",
      0,
      0,
      0,
    ),
  }
}

fn maximum_empty_key_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.constr_small_frame,
    constructor: 0,
    constructor_cbor_root: #"",
    constructor_cbor_length: 0,
    constructor_memory: 0,
    tail: #"30d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01",
    expected_children: 0,
    child_count: 0,
    child_peaks: [],
    fold_cursor: 0,
    sequence: sequence(
      #"8c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56",
      0,
      0,
      0,
    ),
  }
}

fn maximum_empty_map_parent() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.map_frame,
    constructor: 0,
    constructor_cbor_root: #"",
    constructor_cbor_length: 0,
    constructor_memory: 0,
    tail: #"01afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fc",
    expected_children: 2,
    child_count: 0,
    child_peaks: [],
    fold_cursor: 0,
    sequence: sequence(
      #"bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2",
      0,
      0,
      0,
    ),
  }
}

test maximum_cardano_nested_data_applies_real_fold_controls() {
  let list_pre = #"8a01061831193f2b181a58207af9fbf871cd41cd5754b69c195d71a3d671739de3f37e177e7313de792b51dcd87a80d87a80d87a80d87a80"
  let list_post = #"8a01061831193f2b181a58205ff9de6f99abc0e417a8e81850431e4fecf6fe4b0a902e85b8ec2373939440d3d87a80d87a80d87a80d87a80"
  let list_child = summary(
    #"0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2",
    1,
    5,
  )
  let list_action = cek_data_traverse_v1.FoldList {
    frame: maximum_first_list_fold_frame(),
    child_index: 1,
    child: list_child,
    siblings: [
      #"48e5cbf5c18eade239c5fdea7e7c105612da100e19047628fc75ca7175197fca",
    ],
  }
  let map_pre = #"8a01061831193f2b193f2a58204af2404008f74a4ee3eb9123aa0bed99e829e1bf31c6ad5c5ca5efceb0292d85d87a80d87a80d87a80d87a80"
  let map_post = #"8a01061831193f2b193f2a582062601ffbae712e9b1f490fc9c1b52b6e7f46560afa3c873539a2a54d45afb98fd87a80d87a80d87a80d87a80"
  let map_action = cek_data_traverse_v1.FoldMap {
    frame: maximum_map_fold_frame(),
    pair_index: 0,
    key: summary(
      #"284d708d538f4a2427de3995ea06dbb2ffadff18ebb763b56498bb66882e17f9",
      3,
      4,
    ),
    value: summary(
      #"a5423cf93544d255ed2af3806b522663bb0b3a807128a2304524935b7703e347",
      16160,
      48479,
    ),
    key_siblings: [
      #"fc2a353cbdde9156f280ae88c1eadee3d3b42a22d0396ebfa4662dc860f076c9",
    ],
    value_siblings: [
      #"731f16f5e9418e13a0d46f037c791150d0285d1f8d2c96cf39847d0a8c56030d",
    ],
  }
  let finalize_pre = #"8a01061831193f2b0a5820df8903414ebd963ae3a2ed2ca1ec2f15b56e1017fe0701620626657b7fa92a12d87a80d87a80d87a80d87a80"
  let finalize_post = #"8a01001831193f2b0a58202ce6e5a25de1f43178e311d53992e750c87a95234d34fddc2f0f6d4477f533f4d87a80d87a80d87a80d87a80"
  and {
    applied(list_pre, None, list_action, list_post),
    applied(map_pre, None, map_action, map_post),
    applied(
      finalize_pre,
      None,
      cek_data_traverse_v1.FinalizeFrame {
        frame: maximum_empty_key_frame(),
        parent: Some(maximum_empty_map_parent()),
      },
      finalize_post,
    ),
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.decode_control_v1(list_pre),
      None,
      cek_data_traverse_v1.FoldList {
        frame: maximum_first_list_fold_frame(),
        child_index: 1,
        child: list_child,
        siblings: [
          #"49e5cbf5c18eade239c5fdea7e7c105612da100e19047628fc75ca7175197fca",
        ],
      },
    ) == None,
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.decode_control_v1(map_pre),
      None,
      cek_data_traverse_v1.FoldMap {
        frame: maximum_map_fold_frame(),
        pair_index: 1,
        key: summary(
          #"284d708d538f4a2427de3995ea06dbb2ffadff18ebb763b56498bb66882e17f9",
          3,
          4,
        ),
        value: summary(
          #"a5423cf93544d255ed2af3806b522663bb0b3a807128a2304524935b7703e347",
          16160,
          48479,
        ),
        key_siblings: [
          #"fc2a353cbdde9156f280ae88c1eadee3d3b42a22d0396ebfa4662dc860f076c9",
        ],
        value_siblings: [
          #"731f16f5e9418e13a0d46f037c791150d0285d1f8d2c96cf39847d0a8c56030d",
        ],
      },
    ) == None,
  }
}

test maximum_cardano_nested_data_terminal_matches_typescript() {
  let pre = cek_data_traverse_v1.decode_control_v1(
    maximum_cardano_nested_data_pre_control,
  )
  let frame = terminal_frame(
    #"f64559d8fa739e5dec6e218602ff2ebd0d24b477421f38b1872a2274454f84c7",
  )
  expect Some(post) = cek_data_traverse_v1.step_v1(
    pre,
    None,
    cek_data_traverse_v1.FinalizeFrame { frame, parent: None },
  )
  expect Some(summary) = cek_data_traverse_v1.finalize_v1(post)
  and {
    pre.stage == cek_data_traverse_v1.stage_fold,
    cek_data_traverse_v1.encode_control_v1(post) == maximum_cardano_nested_data_post_control,
    summary.root == #"77156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f",
    summary.cbor_length == 16171,
    summary.memory == 48491,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FinalizeFrame {
        frame: terminal_frame(
          #"f74559d8fa739e5dec6e218602ff2ebd0d24b477421f38b1872a2274454f84c7",
        ),
        parent: None,
      },
    ) == None,
  }
}

test maximum_cardano_nested_redeemer_terminal_matches_typescript() {
  let pre = cek_data_traverse_v1.decode_control_v1(
    maximum_cardano_nested_redeemer_pre_control,
  )
  let frame = redeemer_terminal_frame(
    #"0f7bb776751d400f727bf81b02cd7ed66457e144209eb5f9f90e2c6500fe1496",
  )
  expect Some(post) = cek_data_traverse_v1.step_v1(
    pre,
    None,
    cek_data_traverse_v1.FinalizeFrame { frame, parent: None },
  )
  expect Some(summary) = cek_data_traverse_v1.finalize_v1(post)
  and {
    pre.stage == cek_data_traverse_v1.stage_fold,
    cek_data_traverse_v1.encode_control_v1(post) == maximum_cardano_nested_redeemer_post_control,
    summary.root == #"26ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20",
    summary.cbor_length == 15982,
    summary.memory == 47924,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FinalizeFrame {
        frame: redeemer_terminal_frame(
          #"1f7bb776751d400f727bf81b02cd7ed66457e144209eb5f9f90e2c6500fe1496",
        ),
        parent: None,
      },
    ) == None,
  }
}
