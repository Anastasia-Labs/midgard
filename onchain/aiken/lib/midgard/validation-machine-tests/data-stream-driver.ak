//// Test support: drives `canonical_plutus_data_stream_v1` over a span the way
//// a resumable proof does. The span sits at `start` inside a larger carrier;
//// each call reads a window of two `chunk`-byte chunks from the chunk holding
//// the cursor, runs at most `budget` nodes, and reveals only the top open
//// container, so the next call has to resume from the stored hash chain.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_stream_v1.{
  StreamCanonical, StreamFrameV1, StreamNonCanonical, StreamPaused,
} as stream

/// `True` when the span is canonical Data, `False` when it is not.
pub fn stream_verdict_v1(
  carrier: ByteArray,
  start: Int,
  length: Int,
  chunk: Int,
  budget: Int,
) -> Bool {
  drive(carrier, start, start + length, chunk, budget, start, [])
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn drive(
  carrier: ByteArray,
  start: Int,
  end: Int,
  chunk: Int,
  budget: Int,
  cursor: Int,
  stack: List<StreamFrameV1>,
) -> Bool {
  let carrier_length = bytearray.length(carrier)
  let base = minimum(cursor, carrier_length - 1) / chunk * chunk
  let window_end = minimum(base + 2 * chunk, carrier_length)
  let full = window_end >= end
  let bytes = carrier
    |> bytearray.slice(
        base,
        if full {
          end
        } else {
          window_end
        } - 1,
      )
  let revealed = list.take(stack, 1)
  let hidden = list.drop(stack, 1)
  let tail_root = stream.stack_root_v1(hidden, stream.empty_stack_root)
  when
    stream.run_v1(
      bytes,
      full,
      start - base,
      cursor - base,
      revealed,
      tail_root,
      budget,
    )
  is {
    StreamCanonical -> True
    StreamNonCanonical -> False
    StreamPaused { offset, frames, nodes } -> {
      expect nodes > 0
      drive(
        carrier,
        start,
        end,
        chunk,
        budget,
        base + offset,
        list.concat(frames, hidden),
      )
    }
  }
}
