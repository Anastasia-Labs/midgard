use aiken/builtin
use midgard/common/types.{Empty, PosixTime, PosixTimeDuration}
use midgard/protocol_parameters.{operator_inactivity_timeout}

pub const posix_time_none: PosixTime = -1

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

/// TODO?
pub const max_validity_range_length: PosixTimeDuration =
  operator_inactivity_timeout
