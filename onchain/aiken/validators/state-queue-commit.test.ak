//// Full-transaction fixtures for the state-queue `CommitBlockHeader` arm
//// (`state_queue.validate_commit_block_header_v1`).
////
//// Each honest fixture is one complete, internally consistent commit
//// transaction. Every index the commit arm reads lives in the
//// `CommitBlockHeader` mint redeemer carried in `tx.redeemers`, so a fixture is
//// just a `Transaction`: `validates_commit` recovers the indices from that
//// redeemer and calls the arm directly, and `commit_is_accepted` also runs the
//// state-queue mint policy, the commit yield (rewarding) validator and the
//// state-queue spending validator over the same transaction. The
//// active-operators spending validator is not exercised here; its bond-hold
//// arithmetic needs a hub-oracle datum and has its own suites.
////
//// Three honest shapes:
//// - root anchor: the queue is empty and the spent anchor is the
////   confirmed-state root;
//// - node anchor: a one-node queue whose tail is also the head;
//// - deep queue: a two-node queue whose head is authenticated by a separate
////   reference input.
////
//// Layout shared by every shape:
//// - inputs: [0] spent anchor, [1] active-operator node;
//// - reference inputs: [0] commit yield script, [1] scheduler, [2] Idle
////   correction lock, [3] confirmed-state root (node anchors only),
////   [4] queue head (deep queue only);
//// - outputs: [0] continued anchor, [1] new block node, [2] continued
////   active-operator node, all with `reference_script: None`;
//// - redeemers: [0] state-queue mint `CommitBlockHeader`, [1] anchor spend
////   `LinkedListMutation`, [2] active-operator spend `UpdateBondHoldNewState`,
////   [3] commit yield withdrawal.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
  Withdraw,
}
use env
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/correction_lock
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  ConfirmedState, HeaderV1, genesis_confirmed_state_v1, protocol_version_v1,
}
use midgard/operator_directory/active_operators
use midgard/scheduler
use midgard/state_queue.{CommitBlockHeader, InitV1, MintRedeemer}
use midgard/state_queue_yield
use state_queue as state_queue_validator
use state_queue_yields

// ## Deployment identities

const state_queue_policy =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const state_queue_spend_script =
  #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2"

const hub_oracle_policy =
  #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3"

const correction_lock_script =
  #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4"

const active_operators_policy =
  #"a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5"

const retired_operators_policy =
  #"a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6"

const scheduler_policy =
  #"a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7"

const fraud_proof_policy =
  #"a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8"

const settlement_policy =
  #"a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9"

const da_attestation_policy =
  #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1"

const availability_policy =
  #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

const reference_script_auth_policy =
  #"b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3"

const commit_yield_script =
  #"b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4"

// ## Fixture values

/// The operator committing the new block.
const operator = #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

/// The operator that committed the blocks already queued.
const earlier_operator =
  #"c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2"

const confirmed_header_hash =
  #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1"

const confirmed_prev_header_hash =
  #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2"

const confirmed_utxo_root =
  #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1"

const head_utxo_root =
  #"e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2"

const tail_utxo_root =
  #"e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3"

const new_utxo_root =
  #"e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4"

const attested_commitment_hash =
  #"f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1"

const challenge_asset_name =
  #"44414348f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2"

/// A script hash attached to outputs to probe the `reference_script` pins.
const probe_reference_script: ScriptHash =
  #"f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3"

const inputs_transaction_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const reference_inputs_transaction_id =
  #"0202020202020202020202020202020202020202020202020202020202020202"

const element_lovelace = 2_000_000

/// Every block node in these fixtures holds exactly the state-queue node
/// lovelace floor, so each honest commit pays the new node exactly the floor.
const node_lovelace = state_queue.state_queue_node_min_lovelace_v1

/// Fixed positions of the shared layout (see the module header).
const anchor_input_index = 0

const active_operator_input_index = 1

const yield_ref_input_index = 0

const scheduler_ref_input_index = 1

const confirmed_state_ref_input_index = 3

const head_ref_input_index = 4

const continued_anchor_output_index = 0

const new_block_output_index = 1

const active_operator_output_index = 2

const commit_mint_redeemer_index = 0

const active_operator_redeemer_index = 2

// ## Primitive builders

fn state_queue_address() -> Address {
  address.from_script(state_queue_spend_script)
}

fn out_ref(transaction_id: ByteArray, output_index: Int) -> OutputReference {
  OutputReference { transaction_id, output_index }
}

fn header_hash(header: HeaderV1) -> ByteArray {
  crypto.blake2b_224(builtin.serialise_data(header))
}

fn node_asset_name(key: ByteArray) -> ByteArray {
  bytearray.concat(state_queue.block_asset_name_prefix, key)
}

/// An empty-material V1 block (no events), valid under `header_v1_is_valid`.
fn block_header(
  block_operator: ByteArray,
  prev_header_hash: ByteArray,
  prev_utxos_root: ByteArray,
  utxos_root: ByteArray,
  start_time: Int,
  end_time: Int,
) -> HeaderV1 {
  HeaderV1 {
    prev_utxos_root,
    utxos_root,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time,
    end_time,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash,
    operator_vkey: block_operator,
    protocol_version: protocol_version_v1,
  }
}

/// An ordinary (non-genesis) confirmed state ending at 1_000.
fn confirmed_state() -> ConfirmedState {
  ConfirmedState {
    header_hash: confirmed_header_hash,
    prev_header_hash: confirmed_prev_header_hash,
    utxo_root: confirmed_utxo_root,
    start_time: 0,
    end_time: 1_000,
    protocol_version: protocol_version_v1,
  }
}

fn root_output(link: Option<ByteArray>) -> Output {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Root(confirmed_state()),
    link,
  }
  Output {
    address: state_queue_address(),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(
          state_queue_policy,
          state_queue.confirmed_state_asset_name,
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn node_output(
  header: HeaderV1,
  da_attestation: availability.StateQueueStatusV1,
  link: Option<ByteArray>,
) -> Output {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode { header, da_attestation, proven_fraud: None },
    ),
    link,
  }
  Output {
    address: state_queue_address(),
    value: assets.from_lovelace(node_lovelace)
      |> assets.add(state_queue_policy, node_asset_name(header_hash(header)), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

/// Same element, relinked. The commit arm requires the continued anchor to
/// keep its address, NFT and data, and to point at the new node.
fn relink(element: Output, link: Option<ByteArray>) -> Output {
  expect InlineDatum(datum_data) = element.datum
  expect linked_list.Element { data, .. }: state_queue.Datum = datum_data
  let relinked: state_queue.Datum = linked_list.Element { data, link }
  Output { ..element, datum: InlineDatum(relinked) }
}

fn active_operator_output() -> Output {
  let datum: active_operators.Datum = linked_list.Element {
    data: linked_list.Node(
      active_operators.NodeData {
        bond_unlock_time: None,
        inactivity_strikes: 0,
      },
    ),
    link: None,
  }
  Output {
    address: address.from_script(active_operators_policy),
    value: assets.from_lovelace(element_lovelace)
      |> assets.add(
          active_operators_policy,
          bytearray.concat(active_operators.node_asset_name_prefix, operator),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn commit_yield_reference_input() -> Input {
  Input {
    output_reference: out_ref(reference_inputs_transaction_id, 0),
    output: Output {
      address: address.from_script(reference_script_auth_policy),
      value: assets.from_lovelace(element_lovelace)
        |> assets.add(
            reference_script_auth_policy,
            state_queue_yield.commit_role,
            1,
          ),
      datum: transaction.NoDatum,
      reference_script: Some(commit_yield_script),
    },
  }
}

fn scheduler_reference_input(scheduled_operator: ByteArray) -> Input {
  let datum: scheduler.SchedDatum = scheduler.ActiveOperator {
    operator: scheduled_operator,
    start_time: 0,
  }
  Input {
    output_reference: out_ref(reference_inputs_transaction_id, 1),
    output: Output {
      address: address.from_script(scheduler_policy),
      value: assets.from_lovelace(element_lovelace)
        |> assets.add(scheduler_policy, scheduler.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn idle_correction_lock_reference_input() -> Input {
  Input {
    output_reference: out_ref(reference_inputs_transaction_id, 2),
    output: Output {
      address: address.from_script(correction_lock_script),
      value: assets.from_lovelace(element_lovelace)
        |> assets.add(hub_oracle_policy, correction_lock.asset_name, 1),
      datum: InlineDatum(correction_lock.Idle),
      reference_script: None,
    },
  }
}

// ## Commit transaction assembly

/// Assemble one commit transaction in the shared layout. `anchor` is the
/// spent terminal element (the root or the tail node); `queue_references`
/// are appended after the three fixed reference inputs, at indices 3 and 4.
fn commit_transaction(
  anchor: Output,
  new_header: HeaderV1,
  queue_references: List<Output>,
  m_confirmed_state_ref_input_index: Option<Int>,
  m_head_state_queue_node_ref_input_index: Option<Int>,
) -> Transaction {
  let new_key = header_hash(new_header)
  let anchor_input = Input {
    output_reference: out_ref(inputs_transaction_id, anchor_input_index),
    output: anchor,
  }
  let active_operator_input = Input {
    output_reference: out_ref(
      inputs_transaction_id,
      active_operator_input_index,
    ),
    output: active_operator_output(),
  }
  let commit_redeemer: Data = CommitBlockHeader {
    yield_to_ref_input_index: yield_ref_input_index,
    new_block_output_index,
    continued_latest_block_output_index: continued_anchor_output_index,
    operator,
    scheduler_ref_input_index,
    active_operators_input_index: active_operator_input_index,
    active_operators_redeemer_index: active_operator_redeemer_index,
    m_confirmed_state_ref_input_index,
    m_head_state_queue_node_ref_input_index,
  }
  let anchor_spend_redeemer: Data = state_queue.LinkedListMutation
  let active_operator_redeemer: Data = active_operators.UpdateBondHoldNewState {
    active_operator: operator,
    active_node_input_index: active_operator_input_index,
    active_node_output_index: active_operator_output_index,
    hub_oracle_ref_input_index: 0,
    state_queue_redeemer_index: commit_mint_redeemer_index,
  }
  let yield_redeemer: Data = state_queue.YieldStateQueueV1
  Transaction {
    ..transaction.placeholder,
    inputs: [anchor_input, active_operator_input],
    reference_inputs: list.concat(
      [
        commit_yield_reference_input(),
        scheduler_reference_input(operator),
        idle_correction_lock_reference_input(),
      ],
      list.indexed_map(
        queue_references,
        fn(offset, output) {
          Input {
            output_reference: out_ref(
              reference_inputs_transaction_id,
              confirmed_state_ref_input_index + offset,
            ),
            output,
          }
        },
      ),
    ),
    outputs: [
      relink(anchor, Some(new_key)),
      node_output(new_header, state_queue.no_da_attestation, None),
      active_operator_output(),
    ],
    mint: assets.from_asset(state_queue_policy, node_asset_name(new_key), 1),
    withdrawals: [Pair(Script(commit_yield_script), 0)],
    // The commit binds `new_header.end_time` to the inclusive upper bound.
    validity_range: between(new_header.end_time - 500, new_header.end_time),
    extra_signatories: [operator],
    redeemers: [
      Pair(Mint(state_queue_policy), commit_redeemer),
      Pair(Spend(anchor_input.output_reference), anchor_spend_redeemer),
      Pair(
        Spend(active_operator_input.output_reference),
        active_operator_redeemer,
      ),
      Pair(Withdraw(Script(commit_yield_script)), yield_redeemer),
    ],
  }
}

/// Empty queue: the spent anchor is the confirmed-state root (link `None`).
/// The new header carries over the confirmed state
/// (`commit_block_header_carries_confirmed_state_v1`); both optional queue
/// references are `None`.
fn honest_root_anchor_commit() -> Transaction {
  let confirmed = confirmed_state()
  commit_transaction(
    root_output(None),
    block_header(
      operator,
      confirmed.header_hash,
      confirmed.utxo_root,
      new_utxo_root,
      confirmed.end_time,
      2_000,
    ),
    [],
    None,
    None,
  )
}

/// The first queued block, committed by `earlier_operator` on top of the
/// confirmed state.
fn head_header() -> HeaderV1 {
  let confirmed = confirmed_state()
  block_header(
    earlier_operator,
    confirmed.header_hash,
    confirmed.utxo_root,
    head_utxo_root,
    confirmed.end_time,
    2_000,
  )
}

/// The second queued block, on top of `head_header`.
fn tail_header() -> HeaderV1 {
  let head = head_header()
  block_header(
    earlier_operator,
    header_hash(head),
    head.utxos_root,
    tail_utxo_root,
    head.end_time,
    3_000,
  )
}

/// The new block appended after `anchor`, carrying it over exactly
/// (`commit_block_header_carries_previous_block_v1`).
fn header_after(anchor: HeaderV1) -> HeaderV1 {
  block_header(
    operator,
    header_hash(anchor),
    anchor.utxos_root,
    new_utxo_root,
    anchor.end_time,
    anchor.end_time + 1_000,
  )
}

/// One-node queue with the given head status: the spent tail is the head.
/// The root reference's link names it, so no separate head reference is
/// supplied (`m_head_state_queue_node_ref_input_index = None`).
fn node_anchor_commit_with_head_status(
  head_status: availability.StateQueueStatusV1,
) -> Transaction {
  let head = head_header()
  commit_transaction(
    node_output(head, head_status, None),
    header_after(head),
    [root_output(Some(header_hash(head)))],
    Some(confirmed_state_ref_input_index),
    None,
  )
}

/// One-node queue whose Attested head is the spent tail.
fn honest_node_anchor_commit() -> Transaction {
  node_anchor_commit_with_head_status(
    availability.Attested { commitment_hash: attested_commitment_hash },
  )
}

/// Two-node queue: root -> Attested head -> Unattested tail. The spent anchor
/// is the tail; the head is authenticated by the root's link and a separate
/// reference input. Only the head is fenced, so an Unattested tail is fine.
fn honest_deep_queue_commit() -> Transaction {
  let head = head_header()
  let tail = tail_header()
  commit_transaction(
    node_output(tail, state_queue.no_da_attestation, None),
    header_after(tail),
    [
      root_output(Some(header_hash(head))),
      node_output(
        head,
        availability.Attested { commitment_hash: attested_commitment_hash },
        Some(header_hash(tail)),
      ),
    ],
    Some(confirmed_state_ref_input_index),
    Some(head_ref_input_index),
  )
}

// ## Mutators for later negatives

/// Rewrite the output at `index` and leave every other field alone.
fn with_output_at(
  tx: Transaction,
  index: Int,
  rewrite: fn(Output) -> Output,
) -> Transaction {
  Transaction {
    ..tx,
    outputs: list.indexed_map(
      tx.outputs,
      fn(i, output) {
        if i == index {
          rewrite(output)
        } else {
          output
        }
      },
    ),
  }
}

/// Attach `script_hash` as the new block node's reference script.
fn with_new_node_reference_script(
  tx: Transaction,
  script_hash: ScriptHash,
) -> Transaction {
  with_output_at(
    tx,
    new_block_output_index,
    fn(output) { Output { ..output, reference_script: Some(script_hash) } },
  )
}

/// Attach `script_hash` as the continued anchor's (root or tail) reference
/// script.
fn with_anchor_reference_script(
  tx: Transaction,
  script_hash: ScriptHash,
) -> Transaction {
  with_output_at(
    tx,
    continued_anchor_output_index,
    fn(output) { Output { ..output, reference_script: Some(script_hash) } },
  )
}

fn without_operator_signature(tx: Transaction) -> Transaction {
  Transaction { ..tx, extra_signatories: [] }
}

/// Rewrite the state-queue mint redeemer (`CommitBlockHeader`) in place.
fn with_commit_redeemer(
  tx: Transaction,
  rewrite: fn(MintRedeemer) -> MintRedeemer,
) -> Transaction {
  Transaction {
    ..tx,
    redeemers: list.map(
      tx.redeemers,
      fn(pair) {
        if pair.1st == Mint(state_queue_policy) {
          expect redeemer: MintRedeemer = pair.2nd
          let rewritten: Data = rewrite(redeemer)
          Pair(pair.1st, rewritten)
        } else {
          pair
        }
      },
    ),
  }
}

// ## Script entry points

/// The `CommitBlockHeader` redeemer the fixture carries for the state-queue
/// mint policy.
fn commit_redeemer_of(tx: Transaction) -> MintRedeemer {
  expect redeemer: MintRedeemer = utils.get_unique_mint_redeemer(
    tx.redeemers,
    state_queue_policy,
  )
  redeemer
}

/// The commit arm itself, with the indices taken from the fixture's redeemer.
fn validates_commit(tx: Transaction) -> Bool {
  expect
      CommitBlockHeader {
        new_block_output_index,
        continued_latest_block_output_index,
        operator,
        scheduler_ref_input_index,
        active_operators_input_index,
        active_operators_redeemer_index,
        m_confirmed_state_ref_input_index,
        m_head_state_queue_node_ref_input_index,
        ..
      }
    = commit_redeemer_of(tx)
  state_queue_validator.validate_commit_block_header_v1(
    hub_oracle_policy,
    correction_lock_script,
    active_operators_policy,
    address.from_script(active_operators_policy),
    scheduler_policy,
    da_attestation_policy,
    state_queue_policy,
    tx,
    new_block_output_index,
    continued_latest_block_output_index,
    operator,
    scheduler_ref_input_index,
    active_operators_input_index,
    active_operators_redeemer_index,
    m_confirmed_state_ref_input_index,
    m_head_state_queue_node_ref_input_index,
  )
}

/// The deployed route: the commit yield (rewarding) validator, which reads
/// the unique state-queue mint redeemer from `tx.redeemers`.
fn commit_yield_accepts(tx: Transaction) -> Bool {
  state_queue_yields.commit.withdraw(
    state_queue_policy,
    hub_oracle_policy,
    correction_lock_script,
    active_operators_policy,
    address.from_script(active_operators_policy),
    scheduler_policy,
    da_attestation_policy,
    state_queue.YieldStateQueueV1,
    Script(commit_yield_script),
    tx,
  )
}

/// The state-queue mint policy, which delegates the commit to the
/// authenticated zero-valued yield withdrawal.
fn state_queue_mint_accepts(tx: Transaction) -> Bool {
  state_queue_validator.mint.mint(
    hub_oracle_policy,
    correction_lock_script,
    active_operators_policy,
    address.from_script(active_operators_policy),
    retired_operators_policy,
    scheduler_policy,
    fraud_proof_policy,
    settlement_policy,
    da_attestation_policy,
    availability_policy,
    reference_script_auth_policy,
    commit_redeemer_of(tx),
    state_queue_policy,
    tx,
  )
}

/// The state-queue spending validator on the spent anchor.
fn anchor_spend_accepts(tx: Transaction) -> Bool {
  expect Some(anchor_input) = list.at(tx.inputs, anchor_input_index)
  expect InlineDatum(anchor_datum) = anchor_input.output.datum
  state_queue_validator.spend.spend(
    state_queue_policy,
    da_attestation_policy,
    availability_policy,
    fraud_proof_policy,
    Some(anchor_datum),
    state_queue.LinkedListMutation,
    anchor_input.output_reference,
    tx,
  )
}

/// Every state-queue script the commit transaction runs accepts it.
fn commit_is_accepted(tx: Transaction) -> Bool {
  and {
    validates_commit(tx),
    commit_yield_accepts(tx),
    state_queue_mint_accepts(tx),
    anchor_spend_accepts(tx),
  }
}

// ## Honest controls

test state_queue_commit_accepts_honest_root_anchor_append() {
  commit_is_accepted(honest_root_anchor_commit())
}

test state_queue_commit_accepts_honest_node_anchor_append() {
  commit_is_accepted(honest_node_anchor_commit())
}

test state_queue_commit_accepts_honest_deep_queue_append() {
  commit_is_accepted(honest_deep_queue_commit())
}

// ## Sanity negatives (the fixtures are not vacuous)

/// Refused by the commit arm's first check,
/// `expect operator |> utils.has_signed(extra_signatories)`.
test state_queue_commit_rejects_root_anchor_without_operator_signature() fail {
  validates_commit(without_operator_signature(honest_root_anchor_commit()))
}

/// Refused by the append fence `state_queue_head_allows_append_v1`
/// (`Challenged { .. } -> False`): a challenged head halts appends.
test state_queue_commit_rejects_node_anchor_behind_challenged_head() fail {
  validates_commit(
    node_anchor_commit_with_head_status(
      availability.Challenged {
        commitment_hash: attested_commitment_hash,
        challenge_asset_name,
      },
    ),
  )
}

/// Refused by `authenticated_queue_head_for_append_v1`
/// (`expect Some(head_ref_input_index) = m_head_state_queue_node_ref_input_index`):
/// when the tail is not the root's head, the head must be referenced.
test state_queue_commit_rejects_deep_queue_without_head_reference() fail {
  validates_commit(
    with_commit_redeemer(
      honest_deep_queue_commit(),
      fn(redeemer) {
        expect
            CommitBlockHeader {
              yield_to_ref_input_index,
              new_block_output_index,
              continued_latest_block_output_index,
              operator,
              scheduler_ref_input_index,
              active_operators_input_index,
              active_operators_redeemer_index,
              m_confirmed_state_ref_input_index,
              ..
            }
          = redeemer
        CommitBlockHeader {
          yield_to_ref_input_index,
          new_block_output_index,
          continued_latest_block_output_index,
          operator,
          scheduler_ref_input_index,
          active_operators_input_index,
          active_operators_redeemer_index,
          m_confirmed_state_ref_input_index,
          m_head_state_queue_node_ref_input_index: None,
        }
      },
    ),
  )
}

// ## Fixture self-checks

/// Every output of every honest fixture carries no reference script, and each
/// reference-script mutator rewrites exactly its own output's
/// `reference_script`, leaving the rest of the transaction untouched.
test state_queue_commit_fixture_outputs_and_mutators_are_exact() {
  let fixtures = [
    honest_root_anchor_commit(),
    honest_node_anchor_commit(),
    honest_deep_queue_commit(),
  ]
  list.all(
    fixtures,
    fn(tx) {
      expect [anchor, new_node, active_operator] = tx.outputs
      let with_new_node = with_new_node_reference_script(
        tx,
        probe_reference_script,
      )
      let with_anchor = with_anchor_reference_script(tx, probe_reference_script)
      and {
        list.all(tx.outputs, fn(output) { output.reference_script == None }),
        with_new_node.outputs == [
          anchor,
          Output { ..new_node, reference_script: Some(probe_reference_script) },
          active_operator,
        ],
        with_anchor.outputs == [
          Output { ..anchor, reference_script: Some(probe_reference_script) },
          new_node,
          active_operator,
        ],
        Transaction { ..with_new_node, outputs: tx.outputs } == tx,
        Transaction { ..with_anchor, outputs: tx.outputs } == tx,
      }
    },
  )
}

// ## Reference-script pins (the state-queue element invariant)
//
// `validate_completed_fraud_record` requires the fraud node to carry no
// reference script on both sides. Every state-queue element output must
// therefore carry none, or a node could be made unmarkable and fraud in it
// unprovable. Each negative below is its honest control with one output
// carrying `Some(probe_reference_script)`; the controls are the three
// `state_queue_commit_accepts_honest_*_append` tests above.

/// Control: `state_queue_commit_accepts_honest_root_anchor_append`. Refused
/// by `expect element_output_has_no_reference_script(new_block_output)` in
/// `validate_commit_block_header_v1`.
test state_queue_commit_rejects_new_node_reference_script_on_root_anchor() fail {
  validates_commit(
    with_new_node_reference_script(
      honest_root_anchor_commit(),
      probe_reference_script,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_node_anchor_append`. Refused
/// by `expect element_output_has_no_reference_script(new_block_output)` in
/// `validate_commit_block_header_v1`.
test state_queue_commit_rejects_new_node_reference_script_on_node_anchor() fail {
  validates_commit(
    with_new_node_reference_script(
      honest_node_anchor_commit(),
      probe_reference_script,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_root_anchor_append`. Refused
/// by `expect element_output_has_no_reference_script(continued_anchor_element_output)`
/// in `validate_commit_block_header_v1`: the continued confirmed-state root.
test state_queue_commit_rejects_continued_root_anchor_reference_script() fail {
  validates_commit(
    with_anchor_reference_script(
      honest_root_anchor_commit(),
      probe_reference_script,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_deep_queue_append`. Refused
/// by `expect element_output_has_no_reference_script(continued_anchor_element_output)`
/// in `validate_commit_block_header_v1`: the continued tail node, which is
/// not the queue head.
test state_queue_commit_rejects_continued_tail_anchor_reference_script() fail {
  validates_commit(
    with_anchor_reference_script(
      honest_deep_queue_commit(),
      probe_reference_script,
    ),
  )
}

// ## State-queue node lovelace floor and header width bounds
//
// `state_queue_node_min_lovelace_v1` is an inductive floor: the commit arm
// requires the new node to hold at least it and a continued node anchor to
// keep its lovelace exactly. A continued root anchor may only gain lovelace.
// The floor only covers the largest admissible node if the committed header
// has a bounded width, so the commit arm also runs
// `commit_block_header_width_is_bounded_v1`. The honest controls are the three
// `state_queue_commit_accepts_honest_*_append` tests above, whose new node
// holds exactly the floor.

/// Add `delta` lovelace to the output at `index`.
fn with_output_lovelace_delta(
  tx: Transaction,
  index: Int,
  delta: Int,
) -> Transaction {
  with_output_at(
    tx,
    index,
    fn(output) {
      Output {
        ..output,
        value: assets.add(
          output.value,
          assets.ada_policy_id,
          assets.ada_asset_name,
          delta,
        ),
      }
    },
  )
}

/// `honest_root_anchor_commit` with its new header passed through `rewrite`.
/// The new node's key, NFT and the anchor's link follow the rewritten header.
fn root_anchor_commit_with_header(
  rewrite: fn(HeaderV1) -> HeaderV1,
) -> Transaction {
  let confirmed = confirmed_state()
  commit_transaction(
    root_output(None),
    rewrite(
      block_header(
        operator,
        confirmed.header_hash,
        confirmed.utxo_root,
        new_utxo_root,
        confirmed.end_time,
        2_000,
      ),
    ),
    [],
    None,
    None,
  )
}

/// The largest value each bounded header Int may take.
const largest_bounded_header_int =
  state_queue_validator.commit_header_int_exclusive_bound_v1 - 1

/// Two-sided pin: the SDK's `STATE_QUEUE_NODE_MIN_LOVELACE` is `5_000_000n`.
test state_queue_node_min_lovelace_is_five_ada() {
  state_queue.state_queue_node_min_lovelace_v1 == 5_000_000
}

/// The header Int bound is exactly 2^64.
test state_queue_commit_header_int_bound_is_two_pow_64() {
  state_queue_validator.commit_header_int_exclusive_bound_v1 == 18_446_744_073_709_551_616
}

/// Every honest fixture pays its new node exactly the floor, and
/// `root_anchor_commit_with_header` with no rewrite is the root-anchor control.
test state_queue_commit_fixtures_pay_exactly_the_floor() {
  let fixtures = [
    honest_root_anchor_commit(),
    honest_node_anchor_commit(),
    honest_deep_queue_commit(),
  ]
  and {
    list.all(
      fixtures,
      fn(tx) {
        expect Some(new_node) = list.at(tx.outputs, new_block_output_index)
        assets.lovelace_of(new_node.value) == state_queue.state_queue_node_min_lovelace_v1
      },
    ),
    root_anchor_commit_with_header(fn(header) { header }) == honest_root_anchor_commit(),
  }
}

/// A new node above the floor is accepted: the floor is a lower bound.
test state_queue_commit_accepts_new_node_above_the_floor() {
  commit_is_accepted(
    with_output_lovelace_delta(
      honest_node_anchor_commit(),
      new_block_output_index,
      1,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_root_anchor_append`. Refused
/// by `expect output_header_lovelace >= state_queue_node_min_lovelace_v1` in
/// `validate_commit_block_header_v1`: the new node holds the floor minus one.
test state_queue_commit_rejects_new_node_below_the_floor_on_root_anchor() fail {
  validates_commit(
    with_output_lovelace_delta(
      honest_root_anchor_commit(),
      new_block_output_index,
      -1,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_node_anchor_append`. Refused
/// by `expect output_header_lovelace >= state_queue_node_min_lovelace_v1` in
/// `validate_commit_block_header_v1`: the new node holds the floor minus one.
test state_queue_commit_rejects_new_node_below_the_floor_on_node_anchor() fail {
  validates_commit(
    with_output_lovelace_delta(
      honest_node_anchor_commit(),
      new_block_output_index,
      -1,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_root_anchor_append`. Refused
/// by the root arm `None -> anchor_lovelace_change >= 0` in
/// `validate_commit_block_header_v1`: the continued root loses one lovelace.
test state_queue_commit_rejects_root_anchor_losing_lovelace() fail {
  validates_commit(
    with_output_lovelace_delta(
      honest_root_anchor_commit(),
      continued_anchor_output_index,
      -1,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_node_anchor_append`. Refused
/// by the node arm `Some(_) -> anchor_lovelace_change == 0` in
/// `validate_commit_block_header_v1`: the continued node anchor (an Attested
/// head still inside its challenge window) loses one lovelace.
test state_queue_commit_rejects_node_anchor_losing_lovelace() fail {
  validates_commit(
    with_output_lovelace_delta(
      honest_node_anchor_commit(),
      continued_anchor_output_index,
      -1,
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_deep_queue_append`. Refused
/// by the node arm `Some(_) -> anchor_lovelace_change == 0` in
/// `validate_commit_block_header_v1`: the continued tail gains one lovelace,
/// so the node pin is exact rather than one-sided.
test state_queue_commit_rejects_tail_anchor_gaining_lovelace() fail {
  validates_commit(
    with_output_lovelace_delta(
      honest_deep_queue_commit(),
      continued_anchor_output_index,
      1,
    ),
  )
}

/// Regression (liveness): a commit that tops up the root anchor is accepted
/// by every script the commit runs. Merge and `remove_unavailable_head_v1`
/// leave the root's lovelace free, so whoever empties the queue can leave the
/// root at the ledger minimum of its unlinked shape; linking it grows the
/// datum, so the next commit must be able to add lovelace to the root or no
/// commit is ever possible again. The spent root here holds 1 ADA, the
/// continued root 3 ADA.
test state_queue_commit_accepts_root_anchor_top_up_after_a_drain() {
  let drained = with_output_lovelace_delta(
    honest_root_anchor_commit(),
    continued_anchor_output_index,
    1_000_000,
  )
  expect [anchor_input, ..other_inputs] = drained.inputs
  let drained_anchor_input = Input {
    ..anchor_input,
    output: Output {
      ..anchor_input.output,
      value: assets.add(
        anchor_input.output.value,
        assets.ada_policy_id,
        assets.ada_asset_name,
        -1_000_000,
      ),
    },
  }
  commit_is_accepted(
    Transaction { ..drained, inputs: [drained_anchor_input, ..other_inputs] },
  )
}

/// Every bounded header Int at its largest admissible value is accepted by
/// every script the commit runs.
test state_queue_commit_accepts_header_ints_at_the_largest_bounded_value() {
  commit_is_accepted(
    root_anchor_commit_with_header(
      fn(header) {
        HeaderV1 {
          ..header,
          block_slot: largest_bounded_header_int,
          min_fee_a: largest_bounded_header_int,
          min_fee_b: largest_bounded_header_int,
        }
      },
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_root_anchor_append`. Refused
/// by `bytearray.length(header.utxos_root) == 32` in
/// `commit_block_header_width_is_bounded_v1`: a 33-byte `utxos_root`.
test state_queue_commit_rejects_33_byte_utxos_root() fail {
  validates_commit(
    root_anchor_commit_with_header(
      fn(header) {
        HeaderV1 { ..header, utxos_root: bytearray.push(new_utxo_root, 0) }
      },
    ),
  )
}

/// Control: `state_queue_commit_accepts_honest_root_anchor_append`. Refused
/// by `bytearray.length(header.utxos_root) == 32` in
/// `commit_block_header_width_is_bounded_v1`: a 31-byte `utxos_root`.
test state_queue_commit_rejects_31_byte_utxos_root() fail {
  validates_commit(
    root_anchor_commit_with_header(
      fn(header) {
        HeaderV1 { ..header, utxos_root: bytearray.drop(new_utxo_root, 1) }
      },
    ),
  )
}

/// Control: `state_queue_commit_accepts_header_ints_at_the_largest_bounded_value`.
/// Refused by `header.block_slot < commit_header_int_exclusive_bound_v1` in
/// `commit_block_header_width_is_bounded_v1`: `block_slot` at the bound.
test state_queue_commit_rejects_block_slot_at_the_bound() fail {
  validates_commit(
    root_anchor_commit_with_header(
      fn(header) {
        HeaderV1 {
          ..header,
          block_slot: state_queue_validator.commit_header_int_exclusive_bound_v1,
        }
      },
    ),
  )
}

/// Control: `state_queue_commit_accepts_header_ints_at_the_largest_bounded_value`.
/// Refused by `header.min_fee_a < commit_header_int_exclusive_bound_v1` in
/// `commit_block_header_width_is_bounded_v1`: `min_fee_a` at the bound.
test state_queue_commit_rejects_min_fee_a_at_the_bound() fail {
  validates_commit(
    root_anchor_commit_with_header(
      fn(header) {
        HeaderV1 {
          ..header,
          min_fee_a: state_queue_validator.commit_header_int_exclusive_bound_v1,
        }
      },
    ),
  )
}

/// Control: `state_queue_commit_accepts_header_ints_at_the_largest_bounded_value`.
/// Refused by `header.min_fee_b < commit_header_int_exclusive_bound_v1` in
/// `commit_block_header_width_is_bounded_v1`: `min_fee_b` at the bound.
test state_queue_commit_rejects_min_fee_b_at_the_bound() fail {
  validates_commit(
    root_anchor_commit_with_header(
      fn(header) {
        HeaderV1 {
          ..header,
          min_fee_b: state_queue_validator.commit_header_int_exclusive_bound_v1,
        }
      },
    ),
  )
}

// ## InitV1: the genesis root

const init_upper_bound = 10_000

const init_root_output_index = 0

/// A minimal `InitV1` transaction: it mints the confirmed-state root, the hub
/// oracle NFT and the correction-lock singleton, and produces the genesis root
/// (at `init_root_output_index`) and the Idle correction lock.
fn honest_init_transaction() -> Transaction {
  expect Some(genesis) = genesis_confirmed_state_v1(init_upper_bound)
  let root_datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Root(genesis),
    link: None,
  }
  Transaction {
    ..transaction.placeholder,
    outputs: [
      Output {
        address: state_queue_address(),
        value: assets.from_lovelace(element_lovelace)
          |> assets.add(
              state_queue_policy,
              state_queue.confirmed_state_asset_name,
              1,
            ),
        datum: InlineDatum(root_datum),
        reference_script: None,
      },
      Output {
        address: address.from_script(correction_lock_script),
        value: assets.from_lovelace(element_lovelace)
          |> assets.add(hub_oracle_policy, correction_lock.asset_name, 1),
        datum: InlineDatum(correction_lock.Idle),
        reference_script: None,
      },
    ],
    mint: assets.from_asset(
      state_queue_policy,
      state_queue.confirmed_state_asset_name,
      1,
    )
      |> assets.add(hub_oracle_policy, hub.asset_name, 1)
      |> assets.add(hub_oracle_policy, correction_lock.asset_name, 1),
    validity_range: between(init_upper_bound - 500, init_upper_bound),
  }
}

/// The state-queue mint policy under `InitV1`.
fn init_is_accepted(tx: Transaction) -> Bool {
  state_queue_validator.mint.mint(
    hub_oracle_policy,
    correction_lock_script,
    active_operators_policy,
    address.from_script(active_operators_policy),
    retired_operators_policy,
    scheduler_policy,
    fraud_proof_policy,
    settlement_policy,
    da_attestation_policy,
    availability_policy,
    reference_script_auth_policy,
    InitV1 { output_index: init_root_output_index },
    state_queue_policy,
    tx,
  )
}

test state_queue_init_accepts_honest_genesis_root() {
  init_is_accepted(honest_init_transaction())
}

/// Control: `state_queue_init_accepts_honest_genesis_root`. Refused by
/// `expect element_output_has_no_reference_script(root_output)` in the
/// `InitV1` arm of the state-queue mint policy.
test state_queue_init_rejects_root_reference_script() fail {
  init_is_accepted(
    with_output_at(
      honest_init_transaction(),
      init_root_output_index,
      fn(output) {
        Output { ..output, reference_script: Some(probe_reference_script) }
      },
    ),
  )
}
