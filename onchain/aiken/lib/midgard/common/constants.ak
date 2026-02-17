use aiken/builtin
use midgard/common/types.{Empty, PosixTime, PosixTimeDuration}

pub const posix_time_none: PosixTime = -1

pub const empty: Empty = ""

pub const empty_data: Data = empty |> builtin.b_data()

pub const empty_merkle_tree_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

/// 5 minutes in milliseconds.
///
/// > [!NOTE]
/// > This value must be set carefully, as it determines the maximum amount of
/// > time that no blocks can be submitted after advancing the scheduler by
/// > operators whose shifts have not ended. TODO.
pub const max_validity_range_length: PosixTimeDuration = 5 * 60 * 1000
