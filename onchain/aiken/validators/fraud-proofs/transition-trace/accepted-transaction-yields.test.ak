//// The phase-11 output step's choice between a whole replay and the staged
//// phases, read from an output's canonical bytes alone.

use aiken/primitive/bytearray
use fraud_proofs/transition_trace/accepted_transaction_yields.{
  output_step_bytes, output_step_is_whole,
}

test whole_step_replays_an_enterprise_output_with_a_one_byte_coin() {
  let item = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0"
  and {
    bytearray.length(item) == 37,
    output_step_is_whole(item),
  }
}

test whole_step_replays_an_enterprise_output_with_a_nine_byte_coin() {
  let item = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821bffffffffffffffffa0"
  and {
    bytearray.length(item) == 45,
    output_step_is_whole(item),
  }
}

test whole_step_replays_a_base_output_with_a_one_byte_coin() {
  let item = #"a200583900abababababababababababababababababababababababababababababababababababababababababababababababababababababababab018217a0"
  and {
    bytearray.length(item) == 65,
    output_step_is_whole(item),
  }
}

test whole_step_replays_a_base_output_with_a_five_byte_coin() {
  let item = #"a200583900abababababababababababababababababababababababababababababababababababababababababababababababababababababababab01821affffffffa0"
  and {
    bytearray.length(item) == 69,
    output_step_is_whole(item),
  }
}

test whole_step_replays_a_base_output_with_a_nine_byte_coin_at_the_bound() {
  let item = #"a200583900abababababababababababababababababababababababababababababababababababababababababababababababababababababababab01821bffffffffffffffffa0"
  and {
    bytearray.length(item) == 73,
    output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_one_asset() {
  let item = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c44444444444444444444444444444444444444444444444444444444a14001"
  and {
    bytearray.length(item) == 70,
    !output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_one_asset_ending_in_an_empty_map_byte() {
  let item = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c44444444444444444444444444444444444444444444444444444444a14018a0"
  and {
    bytearray.length(item) == 71,
    !output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_one_asset_at_the_bound() {
  let item = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821818a1581c44444444444444444444444444444444444444444444444444444444a1410118a0"
  and {
    bytearray.length(item) == 73,
    !output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_one_asset_past_the_bound() {
  let item = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c44444444444444444444444444444444444444444444444444444444a14001"
  and {
    bytearray.length(item) > output_step_bytes,
    !output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_a_small_inline_datum() {
  let item = #"a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0024100"
  and {
    bytearray.length(item) == 40,
    !output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_a_reference_script() {
  let item = #"a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a003820340"
  and {
    bytearray.length(item) == 41,
    !output_step_is_whole(item),
  }
}

test whole_step_stages_an_output_with_a_node_heavy_inline_datum() {
  let item = #"a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a004c4b40a00258519f40404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040ff"
  and {
    bytearray.length(item) > output_step_bytes,
    !output_step_is_whole(item),
  }
}
