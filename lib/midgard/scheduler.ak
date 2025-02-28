pub type Datum {
  operator: ByteArray,
  start_time: Int,
}

pub type MintRedeemer {
  Init
  Deinit
}

pub type SpendRedeemer {
  Advance { scheduler_output_index: Int, active_node_ref_input_index: Int }
  Rewind {
    scheduler_output_index: Int,
    active_node_ref_input_index: Int,
    active_root_node_ref_input_index: Int,
    registered_node_ref_input_index: Int,
  }
}
