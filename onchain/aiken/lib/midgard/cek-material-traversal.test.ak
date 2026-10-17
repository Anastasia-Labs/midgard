use aiken/primitive/bytearray
use midgard/cek_material_traversal as traversal
use midgard/cek_proof_v1 as proof
use midgard/ledger_state.{CekProgramMaterialDatumV1}

fn visit_error() {
  traversal.Visit {
    input_index: 0,
    output_index: 0,
    task: proof.ProgramMaterialTaskV1 {
      kind: 0,
      root: proof.hash_error_term_v1(),
      expected_length: -1,
    },
    tail_root: traversal.empty_stack,
    entry: CekProgramMaterialDatumV1 {
      kind: 0,
      root: proof.hash_error_term_v1(),
      preimage: #"8106",
    },
    already_seen: False,
    visited_proof: [],
    yield_reference_input_index: 0,
    next_pending_root: traversal.empty_stack,
  }
}

test material_traversal_accounts_unique_nodes_once() {
  let action = visit_error()
  let initial = traversal.initial(action.task.root, 1, 2)
  let first = traversal.advance(initial, action)
  let repeated_state = traversal.State {
    ..first,
    pending_root: initial.pending_root,
  }
  let repeated = traversal.advance(
    repeated_state,
    traversal.Visit { ..action, already_seen: True },
  )
  and {
    first.node_count == 1,
    first.byte_length == 2,
    first.pending_root == traversal.empty_stack,
    repeated == first,
  }
}

test material_traversal_refuses_forged_visited_membership() fail {
  let action = visit_error()
  expect _ = traversal.advance(
    traversal.initial(action.task.root, 1, 2),
    traversal.Visit { ..action, already_seen: True },
  )
}

test material_traversal_refuses_duplicate_as_new() fail {
  let action = visit_error()
  let initial = traversal.initial(action.task.root, 2, 4)
  let first = traversal.advance(initial, action)
  expect _ = traversal.advance(
    traversal.State { ..first, pending_root: initial.pending_root },
    action,
  )
}

test material_traversal_refuses_substituted_stack_tail() fail {
  let action = visit_error()
  expect _ = traversal.advance(
    traversal.initial(action.task.root, 1, 2),
    traversal.Visit { ..action, tail_root: #"00" },
  )
}

test material_traversal_refuses_substituted_node_root() fail {
  let action = visit_error()
  expect _ = traversal.advance(
    traversal.initial(action.task.root, 1, 2),
    traversal.Visit {
      ..action,
      entry: CekProgramMaterialDatumV1 {
        ..action.entry,
        root: proof.hash_variable_term_v1(0),
      },
    },
  )
}

test material_traversal_stack_commits_order_and_every_child() {
  let first = proof.ProgramMaterialTaskV1 {
    kind: 0,
    root: proof.hash_error_term_v1(),
    expected_length: -1,
  }
  let second = proof.ProgramMaterialTaskV1 {
    ..first,
    root: proof.hash_variable_term_v1(0),
  }
  let stack = traversal.push_children([first, second], traversal.empty_stack)
  and {
    bytearray.length(stack) == 32,
    stack != traversal.push_children([second, first], traversal.empty_stack),
    stack != traversal.push_children([first], traversal.empty_stack),
    stack != traversal.push_children([second], traversal.empty_stack),
    traversal.push_children([], traversal.empty_stack) == traversal.empty_stack,
  }
}
