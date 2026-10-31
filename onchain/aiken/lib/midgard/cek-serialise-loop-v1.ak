//// The hops of `cek_core_semantic_serialise_loop`: serialiseData (tag 51)
//// over a Data argument of any size.
////
//// The result's bytes R are staged in B1 staging outputs (runs of up to three
//// chunks; every chunk 4,095 bytes but R's last). Each hop absorbs the runs it
//// names, in order, into a flat-tree frontier of R and reads them, after the
//// unread tail the previous hop left, with the resumable canonical reader
//// `data_serialise_walk_v1`. The hop that summarises the value checks that
//// the summary is the argument's and that R's flat root and length are the
//// result's.
////
//// The tail is carried by reference: the run it lies in and its offset there.
//// A staging output is immutable, so the reference fixes the bytes.

use aiken/builtin
use aiken/collection/list
use cardano/transaction.{Input, OutputReference}
use midgard/cek_builtin_v1.{SemanticConstantValue}
use midgard/cek_data_v1
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_blob_v1.{collect_runs_v1, concat_balanced_v1}
use midgard/computation_thread as ct
use midgard/data_serialise_walk_v1.{
  Advanced, Entry, Finished, Frame, FrontierV1, Refused, Root, append_chunks,
  bytes_leaf, chain as stack_chain, empty_frontier, empty_stack_root,
  frontier_root, run,
}

/// The unread tail: the staging output it lies in and its offset in the
/// output's bytes.
pub type CarryV1 {
  run: OutputReference,
  offset: Int,
}

/// The loop's state between hops: the reader's frame, the hash chain of its
/// stack, the frontier of the bytes absorbed so far and the unread tail.
pub type SerialiseStateV1 {
  frame: Frame,
  stack_root: ByteArray,
  frontier: FrontierV1,
  carry: Option<CarryV1>,
}

pub const initial_state: SerialiseStateV1 =
  SerialiseStateV1 {
    frame: Root,
    stack_root: empty_stack_root,
    frontier: empty_frontier,
    carry: None,
  }

/// What one hop reads: the staging outputs `refs` (in ledger order), the
/// indices of the runs to read in byte order (the carried run first when
/// there is a tail), and the stack entries the hop pops below its own pushes
/// (`restore`, top first, above `base`).
pub type SerialiseHopV1 {
  refs: List<OutputReference>,
  order: List<Int>,
  restore: List<Entry>,
  base: ByteArray,
}

pub type SerialiseAction {
  SerialiseHop {
    input_index: Int,
    output_index: Int,
    witness: Data,
    end: Int,
    hop: SerialiseHopV1,
  }
}

pub type SerialiseRedeemer =
  ct.StepRedeemer<SerialiseAction>

/// The canonical CBOR of the Data constant type.
const data_type = #"9f08ff"

/// The canonical CBOR of the ByteString constant type.
const bytes_type = #"9f01ff"

/// A run's bytes from `offset`, as pieces.
fn tail_of(chunks: List<ByteArray>, offset: Int) -> List<ByteArray> {
  expect [chunk, ..rest] = chunks
  let size = builtin.length_of_bytearray(chunk)
  if offset >= size {
    tail_of(rest, offset - size)
  } else {
    [builtin.slice_bytearray(offset, size - offset, chunk), ..rest]
  }
}

fn run_at(runs: List<List<ByteArray>>, index: Int) -> List<ByteArray> {
  expect Some(chunks) = list.at(runs, index)
  chunks
}

fn ref_at(refs: List<OutputReference>, index: Int) -> OutputReference {
  expect Some(reference) = list.at(refs, index)
  reference
}

fn length_of(chunks: List<ByteArray>) -> Int {
  list.foldl(
    chunks,
    0,
    fn(chunk, total) { total + builtin.length_of_bytearray(chunk) },
  )
}

/// One hop: walk steps [cursor, end) after `from` (the cursor and state the
/// previous hop left, `None` on the first hop). Returns the state to carry on
/// with, or `None` when the value is summarised and every exit check holds.
/// Steps are bounded by 16 per word of the result: every step reads a byte
/// or pops an entry a byte-reading step pushed, so a value of n bytes takes
/// at most 2n steps, and the result has ceil(n / 8) words.
pub fn advance_serialise_v1(
  inner: Data,
  hop: SerialiseHopV1,
  reference_inputs: List<Input>,
  from: Option<Pair<Int, Data>>,
  end: Int,
) -> Option<Data> {
  expect facts: chain.SemanticRoots = inner
  expect
      SemanticConstantValue {
        type_cbor: result_type,
        payload: result_payload,
        memory: result_memory,
      }
    = facts.result
  let Pair(cursor, state) = when from is {
    None -> Pair(0, initial_state)
    Some(Pair(cursor, data)) -> {
      expect state: SerialiseStateV1 = data
      Pair(cursor, state)
    }
  }
  expect cursor < end
  expect end <= 16 * result_memory
  let runs = collect_runs_v1(reference_inputs, hop.refs)
  let Pair(tail, fresh) = when state.carry is {
    None -> Pair([], hop.order)
    Some(CarryV1 { run: carried, offset }) -> {
      expect [first, ..rest] = hop.order
      expect ref_at(hop.refs, first) == carried
      Pair(tail_of(run_at(runs, first), offset), rest)
    }
  }
  let fresh_chunks = list.flat_map(fresh, fn(index) { run_at(runs, index) })
  let frontier = append_chunks(state.frontier, fresh_chunks)
  let window = concat_balanced_v1(list.concat(tail, fresh_chunks))
  let size = builtin.length_of_bytearray(window)
  expect stack_chain(hop.restore, hop.base) == state.stack_root
  when run(window, 0, state.frame, hop.restore, size, end - cursor) is {
    Advanced(at, frame, stack) -> {
      let carry = if at == size {
        None
      } else {
        when list.last(fresh) is {
          None -> {
            expect Some(CarryV1 { run: carried, offset }) = state.carry
            Some(CarryV1 { run: carried, offset: offset + at })
          }
          Some(last) -> {
            let start = size - length_of(run_at(runs, last))
            expect at >= start
            Some(CarryV1 { run: ref_at(hop.refs, last), offset: at - start })
          }
        }
      }
      let next: Data = SerialiseStateV1 {
        frame,
        stack_root: stack_chain(stack, hop.base),
        frontier,
        carry,
      }
      Some(next)
    }
    Finished(summary) -> {
      expect [SemanticConstantValue { type_cbor, payload, memory }] = facts.arguments
      expect type_cbor == data_type
      expect payload == summary
      expect memory == summary.memory
      let length = frontier.length
      expect result_type == bytes_type
      expect
        result_payload == bytes_leaf(
          frontier_root(frontier),
          length,
          cek_data_v1.bytes_data_cbor_length_v1(length),
        )
      expect result_memory == cek_data_v1.bytes_memory_size_v1(length)
      None
    }
    Refused -> fail @"unreadable"
  }
}
