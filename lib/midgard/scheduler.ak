use midgard/common/types.{PosixTime}
use midgard/protocol_parameters.{shift_duration}

pub const scheduler_asset_name = ""

pub type Datum {
  operator: ByteArray,
  start_time: PosixTime,
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

/// The end of a shift is the start of the shift, shifted forward
/// by the shift_duration parameter.
pub fn shift_end_time(start_time: PosixTime) -> PosixTime {
  start_time + shift_duration
}
