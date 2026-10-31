//// Test support: drives `data_serialise_walk_v1` over a value's CBOR the way
//// the serialise loop's prover does. Each hop reads a window of at most
//// `window` bytes from where the last hop stopped and runs at most `steps`
//// steps; a step the window cannot hold ends the hop early, and the next
//// window starts at that step. A summary counts only from the window that
//// ends the bytes: the loop's result is the bytes it absorbed.

use aiken/builtin
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/data_serialise_walk_v1.{
  Advanced, Entry, Finished, Frame, Refused, Root, run,
}

type HopV1 {
  Stopped { at: Int, frame: Frame, stack: List<Entry>, used: Int }
  Done { summary: DataSummaryV1 }
  Bad
}

fn hop(
  piece: ByteArray,
  at: Int,
  frame: Frame,
  stack: List<Entry>,
  size: Int,
  steps: Int,
  used: Int,
  last: Bool,
) -> HopV1 {
  if used == steps {
    Stopped { at, frame, stack, used }
  } else {
    when run(piece, at, frame, stack, size, 1) is {
      Advanced(next, next_frame, next_stack) ->
        hop(piece, next, next_frame, next_stack, size, steps, used + 1, last)
      Finished(summary) ->
        if last {
          Done(summary)
        } else {
          Bad
        }
      Refused ->
        if last {
          Bad
        } else {
          Stopped { at, frame, stack, used }
        }
    }
  }
}

fn drive(
  bytes: ByteArray,
  position: Int,
  frame: Frame,
  stack: List<Entry>,
  window: Int,
  steps: Int,
) -> Option<DataSummaryV1> {
  let total = builtin.length_of_bytearray(bytes)
  let end = if position + window < total {
    position + window
  } else {
    total
  }
  let piece = builtin.slice_bytearray(position, end - position, bytes)
  when hop(piece, 0, frame, stack, end - position, steps, 0, end == total) is {
    Stopped { at, frame: next_frame, stack: next_stack, used } ->
      if used == 0 {
        // No step fits the window: the step is refused on the rest of the
        // bytes, or the window is too small for the test.
        let rest = builtin.slice_bytearray(position, total - position, bytes)
        when run(rest, 0, frame, stack, total - position, 1) is {
          Refused -> None
          _ -> fail @"the window is smaller than a step"
        }
      } else {
        drive(bytes, position + at, next_frame, next_stack, window, steps)
      }
    Done(summary) -> Some(summary)
    Bad -> None
  }
}

/// The summary of `bytes` read in windows of at most `window` bytes and
/// `steps` steps, or `None` when the reader refuses them.
pub fn windows_v1(
  bytes: ByteArray,
  window: Int,
  steps: Int,
) -> Option<DataSummaryV1> {
  drive(bytes, 0, Root, [], window, steps)
}

/// The summary of `bytes` read whole in one window.
pub fn whole_v1(bytes: ByteArray) -> Option<DataSummaryV1> {
  let size = builtin.length_of_bytearray(bytes)
  when run(bytes, 0, Root, [], size, 4 * size + 4) is {
    Finished(summary) -> Some(summary)
    _ -> None
  }
}
