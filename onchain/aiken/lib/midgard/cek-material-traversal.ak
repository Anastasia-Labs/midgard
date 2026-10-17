use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry.{Proof}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose, Spend,
}
use env
use midgard/cek_proof_v1.{ProgramMaterialTaskV1}
use midgard/computation_thread as ct
use midgard/ledger_state.{CekProgramMaterialDatumV1}
use midgard/mpf_proof_v1

pub const program_role = "V1VtCekMatProgramTask"

pub const data_role = "V1VtCekMatDataTask"

pub const empty_stack = blake2b_256("midgard/cek-material-stack/empty")

pub type State {
  pending_root: ByteArray,
  visited_root: ByteArray,
  node_count: Int,
  byte_length: Int,
  expected_node_count: Int,
  expected_byte_length: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Action {
  Visit {
    input_index: Int,
    output_index: Int,
    task: ProgramMaterialTaskV1,
    tail_root: ByteArray,
    entry: CekProgramMaterialDatumV1,
    already_seen: Bool,
    visited_proof: Proof,
    yield_reference_input_index: Int,
    next_pending_root: ByteArray,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

pub type StackNode {
  task: ProgramMaterialTaskV1,
  tail: ByteArray,
}

pub fn push(task: ProgramMaterialTaskV1, tail: ByteArray) -> ByteArray {
  let data: Data = StackNode { task, tail }
  blake2b_256(
    bytearray.concat(
      "midgard/cek-material-stack/item",
      builtin.serialise_data(data),
    ),
  )
}

pub fn push_children(
  children: List<ProgramMaterialTaskV1>,
  tail: ByteArray,
) -> ByteArray {
  list.foldr(children, tail, push)
}

pub fn initial(term_root: ByteArray, node_count: Int, byte_length: Int) -> State {
  State {
    pending_root: push(
      ProgramMaterialTaskV1 { kind: 0, root: term_root, expected_length: -1 },
      empty_stack,
    ),
    visited_root: env.empty_merkle_tree_root,
    node_count: 0,
    byte_length: 0,
    expected_node_count: node_count,
    expected_byte_length: byte_length,
  }
}

pub fn advance(state: State, action: Action) -> State {
  expect state.pending_root == push(action.task, action.tail_root)
  expect action.entry.root == action.task.root
  let visited_root = if action.already_seen {
    expect
      mpf_proof_v1.has(
        state.visited_root,
        action.task.root,
        #"01",
        action.visited_proof,
      )
    state.visited_root
  } else {
    expect Some(root) = mpf_proof_v1.insert_root(
      state.visited_root,
      action.task.root,
      #"01",
      action.visited_proof,
    )
    root
  }
  let next = State {
    ..state,
    pending_root: action.next_pending_root,
    visited_root,
    node_count: state.node_count + if action.already_seen {
      0
    } else {
      1
    },
    byte_length: state.byte_length + if action.already_seen {
      0
    } else {
      bytearray.length(action.entry.preimage)
    },
  }
  expect next.node_count <= next.expected_node_count
  expect next.byte_length <= next.expected_byte_length
  next
}

pub fn unique_dispatch(
  hash: ScriptHash,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Datum, Action) {
  expect
      [
        Input {
          output_reference,
          output: Output { datum: InlineDatum(raw), .. },
        },
      ]
    = list.filter(
    inputs,
    fn(input) { input.output.address.payment_credential == Script(hash) },
  )
  expect [Pair(_, redeemer_data)] = list.filter(
    redeemers,
    fn(pair) { pair.1st == Spend(output_reference) },
  )
  expect datum: Datum = raw
  expect redeemer: SpendRedeemer = redeemer_data
  expect ct.Continue(action) = redeemer
  (datum, action)
}
