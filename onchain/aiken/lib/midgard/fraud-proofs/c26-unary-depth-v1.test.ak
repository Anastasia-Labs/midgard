use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/validation_merkle_v1

const maximum_cardano_transaction_bytes = 16_384

const accepted_unary_depth = 4_043

const accepted_unary_datum_bytes = 16_173

const adjacent_unary_depth = 4_044

const adjacent_unary_datum_bytes = 16_177

const signed_transaction_overhead_bytes = 211

const terminal_pre_control =
  #"8a010600193f2d193f2d58204b1583076e081511c0c79da0bd361e87a6da46e2bbea22cf93f153a3dbb28203d87a80d87a80d87a80d87a80"

const terminal_frame_cbor =
  #"8b000040000040010181820058200349c700d41147fa43955b7c1ee2578d2ef8f08599dd99307121859dd2ee8e860184582087f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e71201193f29193f2d"

const terminal_post_control =
  #"8a010700193f2d193f2d40d87a80d87a80d87a80d8799f835820db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723193f2d193f31ff"

const terminal_root =
  #"db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723"

fn maximum_terminal_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.DataFrameV1 {
    kind: cek_data_frame_v1.constr_small_frame,
    constructor: 0,
    constructor_cbor_root: #"",
    constructor_cbor_length: 0,
    constructor_memory: 0,
    tail: #"",
    expected_children: 1,
    child_count: 1,
    child_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"0349c700d41147fa43955b7c1ee2578d2ef8f08599dd99307121859dd2ee8e86",
      },
    ],
    fold_cursor: 1,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      root: #"87f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e712",
      length: 1,
      payload_cbor_length: 16_169,
      memory: 16_173,
    },
  }
}

test maximum_unary_depth_is_bound_by_signed_cardano_capacity() {
  let accepted_signed_bytes = accepted_unary_datum_bytes + signed_transaction_overhead_bytes
  let adjacent_signed_bytes = adjacent_unary_datum_bytes + signed_transaction_overhead_bytes
  and {
    accepted_unary_datum_bytes == accepted_unary_depth * 4 + 1,
    adjacent_unary_depth == accepted_unary_depth + 1,
    adjacent_unary_datum_bytes == accepted_unary_datum_bytes + 4,
    accepted_signed_bytes == maximum_cardano_transaction_bytes,
    adjacent_signed_bytes > maximum_cardano_transaction_bytes,
  }
}

test maximum_unary_depth_terminal_agrees_with_typescript_and_rejects_mutation() {
  let pre = cek_data_traverse_v1.decode_control_v1(terminal_pre_control)
  let frame = maximum_terminal_frame()
  expect Some(post) = cek_data_traverse_v1.step_v1(
    pre,
    None,
    cek_data_traverse_v1.FinalizeFrame { frame, parent: None },
  )
  expect Some(summary) = cek_data_traverse_v1.finalize_v1(post)
  let mutated_frame = cek_data_frame_v1.DataFrameV1 {
    ..frame,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      ..frame.sequence,
      root: #"86f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e712",
    },
  }
  and {
    cek_data_frame_v1.encode_frame_v1(frame) == terminal_frame_cbor,
    cek_data_traverse_v1.encode_control_v1(post) == terminal_post_control,
    summary.root == terminal_root,
    summary.cbor_length == accepted_unary_datum_bytes,
    summary.memory == 16_177,
    cek_data_traverse_v1.step_v1(
      pre,
      None,
      cek_data_traverse_v1.FinalizeFrame { frame: mutated_frame, parent: None },
    ) == None,
  }
}
