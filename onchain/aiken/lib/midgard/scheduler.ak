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

// Decode scheduler datums explicitly from raw `Data`, matching the rest of the
// operator-set helper modules.
//
// The direct typed coercion path
//   expect datum: Datum = data
// has proven unreliable for the scheduler output datum on the rewind path, so
// keep this decoder explicit and structurally aligned with the on-chain datum.
pub fn parse_scheduler_expect_datum(data: Data) -> Datum {
  parse_scheduler_datum(data)
}

pub fn parse_scheduler_datum(data: Data) -> Datum {
  let raw_datum = builtin.un_constr_data(data)
  expect raw_datum.1st == 0
  let fields = raw_datum.2nd
  expect Some(operator_data) = list.at(fields, 0)
  expect operator: ByteArray = operator_data
  expect Some(start_time_data) = list.at(fields, 1)
  expect start_time: PosixTime = start_time_data
  Datum { operator, start_time }
}

test parse_scheduler_datum_accepts_canonical_data() {
  let datum = Datum {
    operator: #"aa",
    start_time: 42,
  }
  let data: Data = datum

  parse_scheduler_datum(data) == datum
}

test parse_scheduler_expect_datum_accepts_canonical_data() {
  let datum = Datum {
    operator: #"bb",
    start_time: 1337,
  }
  let data: Data = datum

  parse_scheduler_expect_datum(data) == datum
}

/// The end of a shift is the start of the shift, shifted forward
/// by the shift_duration parameter.
pub fn shift_end_time(start_time: PosixTime) -> PosixTime {
  start_time + shift_duration
}
