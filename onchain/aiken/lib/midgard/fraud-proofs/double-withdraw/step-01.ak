//// ## `double-withdraw` step-01 — first payable withdrawal leaf
//// (work item W-C3, catalogue row DOUBLE-WITHDRAW)
////
//// ### The rule this family adjudicates
////
//// A block header commits its withdrawal source set as the counted
//// `withdrawals_root`/`withdrawal_count` pair, whose leaves are
//// `(WithdrawalId, WithdrawalInfo)`. Each leaf's `body.l2_outref` names the L2
//// UTxO the withdrawal drains, and only a leaf whose `validity` is
//// `WithdrawalIsValid` pays out at settlement
//// (`validators/user-events/withdrawal.ak` pays a withdrawal order exactly when
//// the committed verdict is `WithdrawalIsValid`).
////
//// Violation identifier: **`DoubleWithdraw`** (spec
//// `technical-spec/5-ledger-rules/1-cardano-ledger-rules.tex` §DOUBLE-WITHDRAW).
//// A block is faulty when its counted `withdrawals_root` commits two leaves
//// `(id_1, info_1)` and `(id_2, info_2)` with
////
//// - `id_1 != id_2` — two distinct withdrawal events,
//// - `info_1.body.l2_outref == info_2.body.l2_outref` — draining the same L2
////   UTxO, and
//// - `info_1.validity == WithdrawalIsValid == info_2.validity` — **both**
////   payable.
////
//// The both-payable clause is a refinement over the bare spec predicate
//// (`∃ w ≠ w1: l2_outref(w) = l2_outref(w1)`), and it is what keeps honest
//// operators out of reach: every due L1 withdrawal event MUST be included in
//// the block that covers its inclusion time, so when two users (or one user
//// twice) submit withdrawal orders against the same L2 UTxO, the honest block
//// commits both leaves and tags the duplicate with a non-payable verdict
//// (`SpentWithdrawalUtxo`/`NonExistentWithdrawalUtxo`). Convicting on the bare
//// predicate would convict that honest block. Two `WithdrawalIsValid` leaves
//// over one `l2_outref`, by contrast, pay the same funds out twice — the fund
//// theft this family proves.
////
//// ### Scope boundary
////
//// - Same L1 withdrawal **event** applied in two different blocks is the
////   cross-block-duplicate-event family's rule, not this one's.
//// - Two blocks each paying the same L2 UTxO through **distinct** event ids is
////   trace territory: the second block's `ValidWithdrawalTransition` cannot
////   hold over a spent L2 UTxO, so the withdrawal-mistag/transition-trace
////   families adjudicate it. This family is same-block only.
////
//// ### Step-01's job
////
//// Authenticate the challenged header and bind the **first** payable leaf: the
//// committed `(id_1, info_1)` with `info_1.validity == WithdrawalIsValid`. The
//// forwarded state is fixed-size — the header hash, `id_1` and
//// `info_1.body.l2_outref` — so no later step's L1 footprint depends on
//// withdrawer-chosen value or datum sizes.
////
//// ### Binding
////
//// Membership is exclusively the canonical counted-root walk:
//// `transition_trace.verify_root_membership_with_bytes` under
//// `WithdrawalsRootDomain` over `cbor.serialise` of the key and of the value —
//// the same call the transition-trace family's `WithdrawalSourceMembership`
//// arm and the fabricated-withdrawal family make, and the same domain the
//// settlement spend path keys. No new binding path is introduced.
////
//// The category binding is Init's: the catalogue membership proof binds the
//// thread's first step hash at mint time, so — like `native-script-decoding`,
//// and unlike the older contingent-pin families — no category id is pinned on
//// chain. The thread token's 28-byte suffix is bound here instead, through the
//// state-queue node key it must equal.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{AssetName}
use cardano/transaction.{Input, NoDatum, Transaction, placeholder}
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/state_queue
use midgard/transition_trace

// ## Thread identity

/// The challenged header hash a computation-thread token names: its 28-byte
/// suffix after the `fraud_proof_catalogue.id_byte_count`-byte category id.
/// The category id itself is not pinned on chain — Init's catalogue membership
/// proof bound it to this family's step-01 hash at mint time.
pub fn challenged_header_hash_of_v1(
  computation_thread_token_asset_name: AssetName,
) -> ledger_state.HeaderHash {
  bytearray.drop(
    computation_thread_token_asset_name,
    fraud_proof_catalogue.id_byte_count,
  )
}

// ## Header authentication

/// Authenticates the challenged block header.
///
/// The header is never prover-supplied: it is decoded out of the state-queue
/// node reference input the thread names, and that node's own linked-list key
/// must be the 28-byte header hash the computation-thread token already
/// commits to — the same hub → state-queue → node-key chain
/// `midgard/fraud_proofs/common`'s headerless walkers and the
/// fabricated-withdrawal family use. The asset-name length check makes the
/// suffix well-formed before it is compared.
pub fn verify_challenged_header_v1(
  computation_thread_token_asset_name: AssetName,
  hub_oracle: ScriptHash,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  reference_inputs: List<Input>,
  return: fn(ledger_state.HeaderV1, ledger_state.HeaderHash) -> result,
) -> result {
  // 1. The asset name must be a category id followed by a 28-byte header
  //    hash; anything shorter would make the suffix drop below 28 bytes and
  //    the node-key comparison meaningless.
  expect
    bytearray.length(computation_thread_token_asset_name) == fraud_proof_catalogue.id_byte_count + 28

  let hub_datum = hub.get_datum(
    reference_inputs,
    hub_oracle,
    hub_ref_input_index,
  )

  // 2. The header comes from the authentic state-queue node, whose linked-list
  //    key is its own header hash.
  let header, header_hash <- state_queue.get_block_datum_v1(
    reference_inputs,
    hub_datum.state_queue,
    state_queue_node_ref_input_index,
  )

  // 3. The node must be the block the thread token names.
  expect
    header_hash == challenged_header_hash_of_v1(
      computation_thread_token_asset_name,
    )

  return(header, header_hash)
}

// ## Committed withdrawal-source membership

/// Membership witness for one `(WithdrawalId, WithdrawalInfo)` leaf of a
/// header's counted `withdrawals_root`.
pub type CommittedWithdrawalSourceProofV1 =
  transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >

/// Verifies that the challenged header commits `(membership.key,
/// membership.value)` under its counted `withdrawals_root`.
///
/// This is deliberately the *same* machinery the transition-trace and
/// fabricated-withdrawal families walk for a committed withdrawal —
/// `transition_trace.verify_root_membership_with_bytes` under
/// `WithdrawalsRootDomain`, over `cbor.serialise` of the key and of the value,
/// bound to `header.withdrawals_root` and `header.withdrawal_count`. The
/// counted-root wrapper makes `withdrawal_count` load-bearing: a witness that
/// keeps the raw MPF root but restates the count cannot bind.
pub fn verify_committed_withdrawal_membership_v1(
  header: ledger_state.HeaderV1,
  membership: CommittedWithdrawalSourceProofV1,
) -> Bool {
  transition_trace.verify_root_membership_with_bytes(
    membership,
    transition_trace.WithdrawalsRootDomain,
    header.withdrawals_root,
    header.withdrawal_count,
    cbor.serialise(membership.key),
    cbor.serialise(membership.value),
  )
}

// ## Step-01 thread types

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  /// The first committed payable withdrawal leaf this thread challenges.
  committed_withdrawal: CommittedWithdrawalSourceProofV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## Family test fixtures
//
// Test-fixture support for this family's step selectors, in the same spirit as
// `midgard/fraud_proofs/native_binding_fixture_v1` (whose block, hub,
// state-queue and thread builders these extend); not reachable from any
// on-chain validator entry point. Unlike the single-leaf families, the block
// fixture here commits a genuine **two-leaf** withdrawals MPF, with real
// `Leaf` proof steps for each member.

/// The L2 UTxO both withdrawal leaves of the fraudulent fixture drain.
pub fn shared_l2_outref_v1() -> transaction.OutputReference {
  transaction.OutputReference {
    transaction_id: #"7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e",
    output_index: 1,
  }
}

/// An unrelated L2 UTxO, for the distinct-outref honest shape.
pub fn other_l2_outref_v1() -> transaction.OutputReference {
  transaction.OutputReference {
    transaction_id: #"1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f",
    output_index: 0,
  }
}

/// First withdrawal identity — the L1 outref a real withdrawal authentication
/// transaction spent.
pub fn first_withdrawal_id_v1() -> ledger_state.WithdrawalId {
  transaction.OutputReference {
    transaction_id: #"8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b",
    output_index: 2,
  }
}

/// Second, distinct withdrawal identity.
pub fn second_withdrawal_id_v1() -> ledger_state.WithdrawalId {
  transaction.OutputReference {
    transaction_id: #"c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4",
    output_index: 1,
  }
}

/// A payable withdrawal order draining `l2_outref`.
pub fn payable_withdrawal_info_v1(
  l2_outref: transaction.OutputReference,
) -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref,
      l2_owner: #"9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c",
      l2_value: [
        Pair(
          #"4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b",
          [Pair(#"6d6964676172642d746f6b656e", 42)],
        ),
      ],
      l1_address: address.from_verification_key(
        #"2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b",
      ),
      l1_datum: NoDatum,
    },
    signature: Pair(
      #"adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad",
      #"bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebe",
    ),
    validity: ledger_state.WithdrawalIsValid,
  }
}

/// The same order with the honest duplicate verdict: the second event over an
/// already-drained L2 UTxO is included (due L1 events MUST be) but tagged
/// non-payable, so it never pays out.
pub fn honest_duplicate_withdrawal_info_v1(
  l2_outref: transaction.OutputReference,
) -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    ..payable_withdrawal_info_v1(l2_outref),
    validity: ledger_state.SpentWithdrawalUtxo {
      l2_tx_id: #"5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a",
    },
  }
}

/// A single-transaction native-V1 block whose header commits exactly two
/// withdrawal leaves under its counted `withdrawals_root`.
pub type DoubleWithdrawBlockV1 {
  block: fixture.NativeBlockFixtureV1,
  withdrawals_phas_root: ByteArray,
  first_id: ledger_state.WithdrawalId,
  first_info: ledger_state.WithdrawalInfo,
  second_id: ledger_state.WithdrawalId,
  second_info: ledger_state.WithdrawalInfo,
}

fn nibble_v1(bytes: ByteArray, index: Int) -> Int {
  if index % 2 == 0 {
    builtin.index_bytearray(bytes, index / 2) / 16
  } else {
    builtin.index_bytearray(bytes, index / 2) % 16
  }
}

/// Length of the common nibble prefix of two 32-byte MPF paths. Total because
/// two distinct `WithdrawalId`s hash to distinct paths.
fn common_nibble_prefix_v1(a: ByteArray, b: ByteArray, index: Int) -> Int {
  if nibble_v1(a, index) == nibble_v1(b, index) {
    common_nibble_prefix_v1(a, b, index + 1)
  } else {
    index
  }
}

/// The `Leaf` proof step that stands for the *other* leaf of a two-leaf trie:
/// the neighbor's full 32-byte path and its value hash, skipping the two
/// paths' common prefix.
fn two_leaf_neighbor_step_v1(
  own_path: ByteArray,
  neighbor_path: ByteArray,
  neighbor_value_bytes: ByteArray,
) -> mpf.ProofStep {
  mpf.Leaf {
    skip: common_nibble_prefix_v1(own_path, neighbor_path, 0),
    key: neighbor_path,
    value: crypto.blake2b_256(neighbor_value_bytes),
  }
}

/// Commits `(first_id, first_info)` and `(second_id, second_info)` as the two
/// leaves of the block's raw withdrawals MPF and lets the shared fixture
/// re-derive the counted `withdrawals_root`, the dependent event counts and
/// the header hash. The second insert carries the genuine one-leaf exclusion
/// proof, so `mpf.insert`'s own `excluding == root` assertion validates the
/// construction.
pub fn double_withdraw_block_v1(
  first_id: ledger_state.WithdrawalId,
  first_info: ledger_state.WithdrawalInfo,
  second_id: ledger_state.WithdrawalId,
  second_info: ledger_state.WithdrawalInfo,
) -> DoubleWithdrawBlockV1 {
  let first_key_bytes = cbor.serialise(first_id)
  let first_value_bytes = cbor.serialise(first_info)
  let second_key_bytes = cbor.serialise(second_id)
  let second_value_bytes = cbor.serialise(second_info)
  let first_path = crypto.blake2b_256(first_key_bytes)
  let second_path = crypto.blake2b_256(second_key_bytes)
  let withdrawals_phas_root = mpf.root(
    mpf.insert(
      mpf.insert(mpf.empty, first_key_bytes, first_value_bytes, []),
      second_key_bytes,
      second_value_bytes,
      [two_leaf_neighbor_step_v1(second_path, first_path, first_value_bytes)],
    ),
  )
  DoubleWithdrawBlockV1 {
    block: fixture.native_block_fixture_with_withdrawals_v1(
      fixture.valid_native_tx_v1(),
      withdrawals_phas_root,
      2,
    ),
    withdrawals_phas_root,
    first_id,
    first_info,
    second_id,
    second_info,
  }
}

/// The **fraudulent** challenged block: two distinct withdrawal identities,
/// both `WithdrawalIsValid`, both draining the same L2 UTxO — the double
/// payout this family convicts.
pub fn fraudulent_double_withdraw_block_v1() -> DoubleWithdrawBlockV1 {
  double_withdraw_block_v1(
    first_withdrawal_id_v1(),
    payable_withdrawal_info_v1(shared_l2_outref_v1()),
    second_withdrawal_id_v1(),
    payable_withdrawal_info_v1(shared_l2_outref_v1()),
  )
}

/// An **honest** challenged block with a same-outref duplicate: the second
/// event is included (as it must be) but tagged non-payable. Nothing in this
/// family may convict it.
pub fn honest_duplicate_block_v1() -> DoubleWithdrawBlockV1 {
  double_withdraw_block_v1(
    first_withdrawal_id_v1(),
    payable_withdrawal_info_v1(shared_l2_outref_v1()),
    second_withdrawal_id_v1(),
    honest_duplicate_withdrawal_info_v1(shared_l2_outref_v1()),
  )
}

/// An **honest** challenged block with two payable withdrawals over distinct
/// L2 UTxOs — the ordinary two-withdrawal block. Nothing in this family may
/// convict it.
pub fn honest_distinct_outref_block_v1() -> DoubleWithdrawBlockV1 {
  double_withdraw_block_v1(
    first_withdrawal_id_v1(),
    payable_withdrawal_info_v1(shared_l2_outref_v1()),
    second_withdrawal_id_v1(),
    payable_withdrawal_info_v1(other_l2_outref_v1()),
  )
}

fn membership_v1(
  block: DoubleWithdrawBlockV1,
  key: ledger_state.WithdrawalId,
  value: ledger_state.WithdrawalInfo,
  neighbor_key: ledger_state.WithdrawalId,
  neighbor_value: ledger_state.WithdrawalInfo,
) -> CommittedWithdrawalSourceProofV1 {
  transition_trace.RootMembershipProof {
    domain: transition_trace.WithdrawalsRootDomain,
    root: block.block.header.withdrawals_root,
    phas_root: block.withdrawals_phas_root,
    count: block.block.header.withdrawal_count,
    key,
    value,
    proof: [
      two_leaf_neighbor_step_v1(
        crypto.blake2b_256(cbor.serialise(key)),
        crypto.blake2b_256(cbor.serialise(neighbor_key)),
        cbor.serialise(neighbor_value),
      ),
    ],
  }
}

/// The membership witness of the block's first withdrawal leaf.
pub fn first_withdrawal_membership_v1(
  block: DoubleWithdrawBlockV1,
) -> CommittedWithdrawalSourceProofV1 {
  membership_v1(
    block,
    block.first_id,
    block.first_info,
    block.second_id,
    block.second_info,
  )
}

/// The membership witness of the block's second withdrawal leaf.
pub fn second_withdrawal_membership_v1(
  block: DoubleWithdrawBlockV1,
) -> CommittedWithdrawalSourceProofV1 {
  membership_v1(
    block,
    block.second_id,
    block.second_info,
    block.first_id,
    block.first_info,
  )
}

/// The thread token asset name of a double-withdraw thread against `block`,
/// under the shared fixture's stand-in category id (the family pins no
/// category on chain; Init's catalogue membership binds it at mint time).
pub fn block_thread_asset_name_v1(block: DoubleWithdrawBlockV1) -> AssetName {
  fixture.fraud_category_id
    |> builtin.append_bytearray(block.block.header_hash)
}

/// The two reference inputs the steps name, in the order their args index
/// them.
pub fn block_reference_inputs_v1(block: DoubleWithdrawBlockV1) -> List<Input> {
  [
    fixture.hub_reference_input(),
    fixture.state_queue_reference_input(block.block),
  ]
}

/// L1 transaction that continues a double-withdraw thread: the thread input,
/// the next step's output, and the caller's reference-input layout. The family
/// opens no delegated membership claim, so there is no `phas` withdraw
/// redeemer to attach.
pub fn continue_step_l1_tx_v1(
  asset_name: AssetName,
  output_state_data: Data,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [fixture.next_step_output(asset_name, output_state_data)],
    reference_inputs,
  }
}
