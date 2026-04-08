use aiken/collection/list
use aiken/builtin
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

/// Parses scheduler datum data using Aiken's typed expectation path.
/// Decodes scheduler datum data using an expect cast.
pub fn parse_scheduler_expect_datum(data: Data) -> Datum {
  expect datum: Datum = data 
  datum 
}
/// Parses scheduler datum data using the manual field decoder.

/// Parses scheduler datum data field by field.
pub fn parse_scheduler_datum(data: Data) -> Datum {
  let raw_datum = builtin.un_constr_data(data)
  expect raw_datum.1st == 0
  let fields = raw_datum.2nd
  let operator_data = builtin.head_list(fields)
  expect operator: ByteArray = operator_data
  let start_time_data = builtin.head_list(builtin.tail_list(fields))
  expect start_time: PosixTime = start_time_data
  expect builtin.null_list(builtin.tail_list(builtin.tail_list(fields)))
  Datum { operator, start_time }
}

/// The end of a shift is the start of the shift, shifted forward
/// by the shift_duration parameter.
pub fn shift_end_time(start_time: PosixTime) -> PosixTime {
  start_time + shift_duration
}
