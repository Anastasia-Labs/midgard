//// Test support: an honest driver for `cek_data_traverse_v1`.
////
//// Given only a source span, it plays every step the way an honest prover
//// would: it offers each admissible action of the current stage, keeps the
//// frame preimages and child summaries the fold and finalize actions need, and
//// computes the child MMR siblings. It stops at the terminal (the span is
//// canonical Data), at a control where `unreadable_v1` holds (the span is not),
//// or at a control where neither holds (a stuck traverse, which the corpus must
//// never reach). At every control it also checks the one-successor rule: at
//// most one offered action advances, and `unreadable_v1` never holds beside a
//// successor.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_bytes_v1
use midgard/cek_data_frame_v1.{DataFrameV1}
use midgard/cek_data_integer_v1
use midgard/cek_data_traverse_v1.{
  AttachScalar, DataTraverseActionV1, DataTraverseControlV1, FinalizeFrame,
  FoldList, FoldMap, HeadLargeConstructor, HeadMap, HeadScalar, HeadSequence,
  NoAction,
}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_source_blob_v1
use midgard/validation_merkle_v1

pub type DriveVerdictV1 {
  /// The traverse reached its terminal with this summary.
  DriveAccepted { summary: DataSummaryV1 }
  /// `unreadable_v1` holds at a control of this traverse stage.
  DriveUnreadable { stage: Int }
  /// No offered action advances and `unreadable_v1` does not hold.
  DriveStuck { stage: Int }
  /// Two offered actions advance, or `unreadable_v1` holds beside a successor.
  DriveAmbiguous { stage: Int }
}

type OpenFrameV1 {
  frame: DataFrameV1,
  /// Attached children, most recent first.
  children: List<DataSummaryV1>,
}

fn window(control: DataTraverseControlV1, blob: ByteArray) -> Option<ByteArray> {
  when cek_data_traverse_v1.next_source_span_v1(control) is {
    None -> None
    Some(span) ->
      Some(
        blob
          |> bytearray.drop(span.absolute_start)
          |> bytearray.take(span.length),
      )
  }
}

fn argument_value(bytes: ByteArray) -> Int {
  let additional = bytearray.at(bytes, 0) % 32
  if additional < 24 {
    additional
  } else {
    let width = if additional == 24 {
      1
    } else if additional == 25 {
      2
    } else if additional == 26 {
      4
    } else {
      8
    }
    bytes
      |> bytearray.drop(1)
      |> bytearray.take(width)
      |> bytearray.to_int_big_endian
  }
}

fn power_at_most(count: Int, size: Int) -> Int {
  if size * 2 > count {
    size
  } else {
    power_at_most(count, size * 2)
  }
}

fn perfect_root(leaves: List<ByteArray>, size: Int) -> ByteArray {
  if size == 1 {
    expect [leaf] = leaves
    leaf
  } else {
    let half = size / 2
    validation_merkle_v1.hash_branch(
      perfect_root(list.take(leaves, half), half),
      perfect_root(list.drop(leaves, half), half),
    )
  }
}

fn perfect_siblings(
  leaves: List<ByteArray>,
  size: Int,
  index: Int,
) -> List<ByteArray> {
  if size == 1 {
    []
  } else {
    let half = size / 2
    let left = list.take(leaves, half)
    let right = list.drop(leaves, half)
    if index < half {
      list.concat(
        perfect_siblings(left, half, index),
        [perfect_root(right, half)],
      )
    } else {
      list.concat(
        perfect_siblings(right, half, index - half),
        [perfect_root(left, half)],
      )
    }
  }
}

/// The membership path of leaf `index` in the compact MMR over `leaves`: the
/// leftmost peak covers the largest power of two, then the rest recursively.
fn mmr_siblings(leaves: List<ByteArray>, index: Int) -> List<ByteArray> {
  let count = list.length(leaves)
  let size = power_at_most(count, 1)
  if index < size {
    perfect_siblings(list.take(leaves, size), size, index)
  } else {
    mmr_siblings(list.drop(leaves, size), index - size)
  }
}

fn child_leaves(children: List<DataSummaryV1>) -> List<ByteArray> {
  list.indexed_map(
    list.reverse(children),
    fn(index, child) { cek_data_frame_v1.child_leaf_hash_v1(index, child) },
  )
}

fn child_at(children: List<DataSummaryV1>, index: Int) -> DataSummaryV1 {
  expect Some(child) = list.at(list.reverse(children), index)
  child
}

fn top_frame(stack: List<OpenFrameV1>) -> Option<DataFrameV1> {
  when stack is {
    [] -> None
    [top, ..] -> Some(top.frame)
  }
}

/// The one action a fold-stage control takes: fold the next child (lists and
/// constructor fields fold from the last child back, maps by pair), or
/// finalize once every child is folded.
fn fold_action(stack: List<OpenFrameV1>) -> DataTraverseActionV1 {
  expect [top, ..rest] = stack
  let frame = top.frame
  let leaves = child_leaves(top.children)
  if frame.kind == cek_data_frame_v1.map_frame {
    let pairs = frame.expected_children / 2
    if frame.fold_cursor < pairs {
      let pair_index = pairs - frame.fold_cursor - 1
      FoldMap {
        frame,
        pair_index,
        key: child_at(top.children, pair_index * 2),
        value: child_at(top.children, pair_index * 2 + 1),
        key_siblings: mmr_siblings(leaves, pair_index * 2),
        value_siblings: mmr_siblings(leaves, pair_index * 2 + 1),
      }
    } else {
      FinalizeFrame { frame, parent: top_frame(rest) }
    }
  } else if frame.fold_cursor < frame.child_count {
    let child_index = frame.child_count - frame.fold_cursor - 1
    FoldList {
      frame,
      child_index,
      child: child_at(top.children, child_index),
      siblings: mmr_siblings(leaves, child_index),
    }
  } else {
    FinalizeFrame { frame, parent: top_frame(rest) }
  }
}

fn scalar_terminal(control: DataTraverseControlV1) -> Bool {
  if control.stage == cek_data_traverse_v1.stage_integer {
    expect Some(integer) = control.integer
    integer.stage == cek_data_integer_v1.stage_terminal
  } else if control.stage == cek_data_traverse_v1.stage_bytes {
    expect Some(bytes) = control.bytes
    bytes.stage == cek_data_bytes_v1.stage_terminal
  } else {
    False
  }
}

fn offered_actions(
  control: DataTraverseControlV1,
  stack: List<OpenFrameV1>,
) -> List<DataTraverseActionV1> {
  let heads = [HeadScalar, HeadSequence, HeadMap, HeadLargeConstructor]
  if control.stage == cek_data_traverse_v1.stage_head {
    heads
  } else if control.stage == cek_data_traverse_v1.stage_close {
    [NoAction, ..heads]
  } else if control.stage == cek_data_traverse_v1.stage_fold {
    [fold_action(stack)]
  } else if scalar_terminal(control) {
    [AttachScalar { parent: top_frame(stack) }]
  } else {
    [NoAction]
  }
}

fn scalar_summary(control: DataTraverseControlV1) -> DataSummaryV1 {
  if control.stage == cek_data_traverse_v1.stage_integer {
    expect Some(integer) = control.integer
    expect Some(summary) = cek_data_integer_v1.finalize_v1(integer)
    summary
  } else {
    expect Some(bytes) = control.bytes
    expect Some(summary) = cek_data_bytes_v1.finalize_v1(bytes)
    summary
  }
}

fn attach(stack: List<OpenFrameV1>, summary: DataSummaryV1) -> List<OpenFrameV1> {
  when stack is {
    [] -> []
    [top, ..rest] -> {
      expect Some(frame) = cek_data_frame_v1.append_child_v1(top.frame, summary)
      [OpenFrameV1 { frame, children: [summary, ..top.children] }, ..rest]
    }
  }
}

fn push(stack: List<OpenFrameV1>, frame: DataFrameV1) -> List<OpenFrameV1> {
  [OpenFrameV1 { frame, children: [] }, ..stack]
}

/// The frame the head action opened, rebuilt from the window it read.
fn opened_sequence_frame(bytes: ByteArray, tail: ByteArray) -> DataFrameV1 {
  let first = bytearray.at(bytes, 0)
  if first == 216 {
    cek_data_frame_v1.initial_small_constr_frame_v1(
      bytearray.at(bytes, 1) - 121,
      tail,
    )
  } else if first == 217 {
    cek_data_frame_v1.initial_small_constr_frame_v1(
      bytearray.at(bytes, 1) * 256 + bytearray.at(bytes, 2) - 1280 + 7,
      tail,
    )
  } else {
    cek_data_frame_v1.initial_list_frame_v1(tail)
  }
}

fn next_stack(
  control: DataTraverseControlV1,
  bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
  stack: List<OpenFrameV1>,
) -> List<OpenFrameV1> {
  when action is {
    HeadSequence -> {
      expect Some(window_bytes) = bytes
      push(stack, opened_sequence_frame(window_bytes, control.frame_root))
    }
    HeadMap -> {
      expect Some(window_bytes) = bytes
      push(
        stack,
        cek_data_frame_v1.initial_map_frame_v1(
          control.frame_root,
          argument_value(window_bytes) * 2,
        ),
      )
    }
    NoAction ->
      if control.stage == cek_data_traverse_v1.stage_large_fields {
        expect Some(integer) = control.integer
        expect Some(blob) = integer.blob
        expect Some(root) = cek_source_blob_v1.finalize_v1(blob)
        push(
          stack,
          cek_data_frame_v1.initial_large_constr_frame_v1(
            root,
            integer.source_length,
            integer.memory,
            control.frame_root,
          ),
        )
      } else {
        stack
      }
    AttachScalar { .. } -> attach(stack, scalar_summary(control))
    FoldList { frame, child_index, child, siblings } -> {
      expect [top, ..rest] = stack
      expect Some(folded) = cek_data_frame_v1.fold_list_child_v1(
        frame,
        child_index,
        child,
        siblings,
      )
      [OpenFrameV1 { ..top, frame: folded }, ..rest]
    }
    FoldMap { frame, pair_index, key, value, key_siblings, value_siblings } -> {
      expect [top, ..rest] = stack
      expect Some(folded) = cek_data_frame_v1.fold_map_pair_v1(
        frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings,
      )
      [OpenFrameV1 { ..top, frame: folded }, ..rest]
    }
    FinalizeFrame { frame, .. } -> {
      expect [_, ..rest] = stack
      expect Some(summary) = cek_data_frame_v1.finalized_summary_v1(frame)
      attach(rest, summary)
    }
    _ -> stack
  }
}

fn successors(
  control: DataTraverseControlV1,
  bytes: Option<ByteArray>,
  actions: List<DataTraverseActionV1>,
) -> List<Pair<DataTraverseActionV1, DataTraverseControlV1>> {
  list.foldr(
    actions,
    [],
    fn(action, found) {
      when cek_data_traverse_v1.step_v1(control, bytes, action) is {
        None -> found
        Some(next) -> [Pair(action, next), ..found]
      }
    },
  )
}

fn drive_loop(
  control: DataTraverseControlV1,
  blob: ByteArray,
  stack: List<OpenFrameV1>,
) -> DriveVerdictV1 {
  if control.stage == cek_data_traverse_v1.stage_terminal {
    expect Some(summary) = cek_data_traverse_v1.finalize_v1(control)
    expect stack == []
    DriveAccepted { summary }
  } else {
    let bytes = window(control, blob)
    let actions = offered_actions(control, stack)
    // A byte-reading stage claims `Unreadable` with `NoAction`; the attach and
    // finalize stages with the witness action they would advance with.
    let unreadable = list.any(
      [NoAction, ..actions],
      fn(action) { cek_data_traverse_v1.unreadable_v1(control, bytes, action) },
    )
    when successors(control, bytes, actions) is {
      [] ->
        if unreadable {
          DriveUnreadable { stage: control.stage }
        } else {
          DriveStuck { stage: control.stage }
        }
      [Pair(action, next)] ->
        if unreadable {
          DriveAmbiguous { stage: control.stage }
        } else {
          let stack = next_stack(control, bytes, action, stack)
          expect
            next.stage == cek_data_traverse_v1.stage_terminal || when stack is {
              [] -> next.frame_root == #""
              [top, ..] ->
                cek_data_frame_v1.hash_frame_v1(top.frame) == next.frame_root
            }
          drive_loop(next, blob, stack)
        }
      _ -> DriveAmbiguous { stage: control.stage }
    }
  }
}

/// Drive the traverse over `blob[source_start, source_start + source_length)`.
pub fn drive_v1(
  blob: ByteArray,
  source_start: Int,
  source_length: Int,
) -> DriveVerdictV1 {
  drive_loop(
    cek_data_traverse_v1.initial_control_v1(source_start, source_length),
    blob,
    [],
  )
}

/// Drive the traverse over all of `source`.
pub fn drive_source_v1(source: ByteArray) -> DriveVerdictV1 {
  drive_v1(source, 0, bytearray.length(source))
}
