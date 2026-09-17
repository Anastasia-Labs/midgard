//// ## `fabricated-deposit` step-01 — committed deposit-source membership
//// (`GOAL_SPEC.md` §9.1 / Q39)
////
//// ### The rule this family adjudicates
////
//// A block header commits its deposit source set as the counted
//// `deposits_root`/`deposit_count` pair, whose leaves are
//// `(DepositId, DepositInfo)` — the L1 deposit `OutputReference` and the
//// `l2_address`/`l2_network_id`/`l2_datum` triple credited on L2
//// (`midgard/ledger_state`). The transition-trace family verifies that a
//// committed deposit leaf is a member of that root and that the projected L2
//// output follows from it, but every one of its deposit arms *starts* from the
//// committed leaf: it assumes the committed `DepositInfo` is the genuine L1
//// event's content and can only run at all when the prover can produce the
//// matching authentic event NFT. A header that commits a deposit with no
//// authentic L1 event, or with content the authentic event never carried, is
//// therefore outside its reach.
////
//// Violation identifier: **`FabricatedDeposit`**. A block is faulty when its
//// counted `deposits_root` commits a `(DepositId, DepositInfo)` leaf that is
//// not the authentic L1 deposit event pair, either because
////
//// - `NonexistentDepositIdentity` — no L1 deposit event with that `DepositId`
////   was ever authenticated, or
//// - `MismatchedDepositContent` — the authentic event exists and was due for
////   this block, but its `DepositInfo` is not the committed one.
////
//// ### Why non-existence is positively provable
////
//// `DepositId` is the `OutputReference` of the L1 output the deposit
//// authentication transaction **spent**: `user_events.authenticate_new_event`
//// mints exactly one event NFT whose asset name is
//// `out_ref_to_nonce(nonce_input_out_ref)` and requires the event datum's id to
//// equal that very spent output reference. A Cardano output is spent at most
//// once, so an output reference that is *still unspent* can never have been the
//// nonce of an authenticated deposit event. Referencing the live output at the
//// committed `DepositId` is therefore a positive, on-chain-checkable proof that
//// the committed identity is fabricated — see step-02.
////
//// ### Step-01's job
////
//// Authenticate the challenged header and prove that it really does commit the
//// leaf the rest of the family will refute. Nothing about the L1 event is read
//// here; a fabricated-deposit thread that cannot first exhibit the committed
//// leaf has nothing to adjudicate.
////
//// ### Shared with `fabricated-withdrawal` (Q40)
////
//// The category binding, header authentication and counted-root membership
//// helpers below are family-local on purpose: the deposit and withdrawal
//// families share machinery but bind different roots and datum shapes, so the
//// withdrawal family mirrors these modules against `withdrawals_root` rather
//// than editing a shared parent-owned file.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{AssetName}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use midgard/common/types.{H32}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/state_queue
use midgard/transition_trace

// ## Catalogue binding

/// Catalogue identifier of the `fabricatedDeposit` category.
///
/// A category id is the `fraud_proof_catalogue.id_byte_count`-byte big-endian
/// index of the category's position in the append-only
/// `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`
/// (`demo/midgard-sdk/src/fraud-proof/catalogue.ts`), the same derivation
/// `transition_trace`'s `#"00000004"` follows from `transitionTrace` sitting at
/// index 4. `fabricatedDeposit` appends at index 11.
///
/// The category is **not registered yet**: catalogue registration is
/// parent-owned integration work, and this constant is the single point that
/// has to move if the append-only order gains another entry before Q39 lands.
pub const fabricated_deposit_fraud_category_id: ByteArray = #"0000000b"

/// A fabricated-deposit computation-thread token's asset name is this family's
/// category id followed by the 28-byte hash of the header under challenge.
pub fn fraud_category_is_fabricated_deposit_v1(
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    bytearray.length(computation_thread_token_asset_name) == fraud_proof_catalogue.id_byte_count + 28,
    bytearray.take(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    ) == fabricated_deposit_fraud_category_id,
  }
}

/// The challenged header hash a computation-thread token names.
pub fn challenged_header_hash_of_v1(
  computation_thread_token_asset_name: AssetName,
) -> ledger_state.HeaderHash {
  bytearray.drop(
    computation_thread_token_asset_name,
    fraud_proof_catalogue.id_byte_count,
  )
}

/// The computation-thread token asset name of a fabricated-deposit thread
/// against `challenged_header_hash`.
pub fn computation_thread_token_asset_name_v1(
  challenged_header_hash: ledger_state.HeaderHash,
) -> AssetName {
  fabricated_deposit_fraud_category_id
    |> builtin.append_bytearray(challenged_header_hash)
}

// ## Header authentication

/// Authenticates the challenged block header, and hands back the hub datum the
/// later steps read the deposit event policy from.
///
/// The header is never prover-supplied. It is decoded out of the state-queue
/// node reference input the thread names, and that node's own key must be the
/// header hash the computation-thread token already commits to — the same
/// hub → state-queue → node-key chain `midgard/fraud_proofs/common`'s
/// `verify_native_tx_in_state_queue_node_with` walks before it opens a
/// transactions-root leaf. Only the root that gets opened afterwards differs,
/// because `deposits_root` is a header **event** root: it has no raw-CBOR
/// carriage seam and is not reachable through the published-chunk transactions
/// carriage of issue #545.
pub fn verify_challenged_header_v1(
  computation_thread_token_asset_name: AssetName,
  hub_oracle: ScriptHash,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  reference_inputs: List<Input>,
  return: fn(ledger_state.HeaderV1, ledger_state.HeaderHash, hub.Datum) ->
    result,
) -> result {
  // 1. The thread token must belong to this family's catalogue category. A
  //    thread minted under another category names another family's rule, and
  //    its final fraud-proof token would be filed under that category.
  expect
    fraud_category_is_fabricated_deposit_v1(computation_thread_token_asset_name)

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

  return(header, header_hash, hub_datum)
}

// ## Committed deposit-source membership

/// Membership witness for one `(DepositId, DepositInfo)` leaf of a header's
/// counted `deposits_root`.
pub type CommittedDepositSourceProofV1 =
  transition_trace.RootMembershipProof<
    ledger_state.DepositId,
    ledger_state.DepositInfo,
  >

/// Verifies that the challenged header commits `(membership.key,
/// membership.value)` under its counted `deposits_root`.
///
/// This is deliberately the *same* machinery the transition-trace family walks
/// for a committed deposit —
/// `transition_trace.verify_root_membership_with_bytes` under
/// `DepositsRootDomain`, over `cbor.serialise` of the key and of the value,
/// bound to `header.deposits_root` and `header.deposit_count` (see
/// `midgard/fraud_proofs/transition_trace/proof`'s `verify_source_membership`,
/// `DepositSourceMembership` arm, and `validators/user-events/deposit.ak`'s
/// settlement-side membership check, which keys the same domain). Inventing a
/// second membership rule here would let Q39 convict a block over a leaf the
/// transition arms and the settlement spend path do not recognise as committed.
///
/// The counted-root wrapper is what makes `deposit_count` load-bearing:
/// `verify_root_membership_with_bytes` re-derives
/// `commit_counted_root(DepositsRootDomain, phas_root, count)` and requires it
/// to equal `header.deposits_root`, so a witness that keeps the raw MPF root but
/// restates the count cannot bind. `ledger_state`'s
/// `max_deposit_count_v1`/`root_matches_count_v1` bounds are not re-asserted
/// here: they are enforced where the header is committed
/// (`header_v1_is_valid`), and a settled state-queue node cannot carry a header
/// that violates them, so a repeat of that check would be a branch no evidence
/// can reach.
pub fn verify_committed_deposit_membership_v1(
  header: ledger_state.HeaderV1,
  membership: CommittedDepositSourceProofV1,
) -> Bool {
  transition_trace.verify_root_membership_with_bytes(
    membership,
    transition_trace.DepositsRootDomain,
    header.deposits_root,
    header.deposit_count,
    cbor.serialise(membership.key),
    cbor.serialise(membership.value),
  )
}

/// Blake2b-256 hash of `cbor.serialise` of a `DepositInfo`.
///
/// The thread carries this hash rather than the `DepositInfo` itself, and that
/// is a soundness property rather than an optimisation: `DepositInfo.l2_datum`
/// is an `Option<Data>` of unbounded size, so a thread state that embedded it
/// would let the block producer choose how many bytes every later step of the
/// dispute has to pay for — a griefing lever on the fraud prover. A 32-byte
/// commitment makes each handoff a fixed size, and comparing two such hashes is
/// equivalent to comparing the `DepositInfo`s themselves.
pub type DepositInfoHash =
  H32<ledger_state.DepositInfo>

/// The committed leaf's value commitment, taken from the very bytes
/// `verify_committed_deposit_membership_v1` fed to the counted-root check.
pub fn committed_deposit_info_hash_v1(
  membership: CommittedDepositSourceProofV1,
) -> DepositInfoHash {
  utils.serialise_and_hash_32(membership.value)
}

// ## Step-01 thread types

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  /// The committed deposit leaf this thread challenges.
  committed_deposit: CommittedDepositSourceProofV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## Family test fixtures
//
// Test-fixture support for this family's step selectors, in the same spirit as
// `midgard/fraud_proofs/native_binding_fixture_v1` (whose block, hub,
// state-queue and thread builders these extend): it is not reachable from any
// on-chain validator entry point. It lives in this family's own lib modules
// rather than in the shared fixture module because Q39 and Q40 must keep
// disjoint family paths.

/// Deterministic L1 output reference standing in for a genuine deposit nonce —
/// the output a real deposit authentication transaction spent.
pub fn authentic_deposit_id_v1() -> ledger_state.DepositId {
  OutputReference {
    transaction_id: #"7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a",
    output_index: 3,
  }
}

/// A deposit identity no authentication transaction ever spent — the shape a
/// header commits when it invents a deposit out of nothing.
pub fn fabricated_deposit_id_v1() -> ledger_state.DepositId {
  OutputReference {
    transaction_id: #"5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c",
    output_index: 0,
  }
}

/// A second, unrelated output reference, used where a witness must name an
/// identity other than the committed one.
pub fn foreign_deposit_id_v1() -> ledger_state.DepositId {
  OutputReference {
    transaction_id: #"6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b",
    output_index: 1,
  }
}

/// The L2 credit an authentic deposit event carries.
pub fn authentic_deposit_info_v1() -> ledger_state.DepositInfo {
  ledger_state.DepositInfo {
    l2_address: address.from_verification_key(
      #"1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c",
    ),
    l2_network_id: 0,
    l2_datum: None,
  }
}

/// The same credit with a different `l2_address` — the shape a header commits
/// when it fabricates the *content* of a real deposit event.
pub fn diverted_deposit_info_v1() -> ledger_state.DepositInfo {
  ledger_state.DepositInfo {
    ..authentic_deposit_info_v1(),
    l2_address: address.from_verification_key(
      #"2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d",
    ),
  }
}

/// A single-transaction native-V1 block whose header additionally commits
/// exactly one deposit leaf under its counted `deposits_root`.
pub type FabricatedDepositBlockV1 {
  block: fixture.NativeBlockFixtureV1,
  deposits_phas_root: ByteArray,
  committed_deposit_id: ledger_state.DepositId,
  committed_deposit_info: ledger_state.DepositInfo,
}

/// Commits `(committed_deposit_id, committed_deposit_info)` as the single leaf
/// of the block's raw deposits MPF and re-derives the counted `deposits_root`,
/// the dependent event counts and the header hash.
pub fn fabricated_deposit_block_v1(
  committed_deposit_id: ledger_state.DepositId,
  committed_deposit_info: ledger_state.DepositInfo,
) -> FabricatedDepositBlockV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let deposits_phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(committed_deposit_id),
      cbor.serialise(committed_deposit_info),
      [],
    ),
  )
  let header = ledger_state.HeaderV1 {
    ..base.header,
    deposits_root: transition_trace.commit_counted_root(
      transition_trace.DepositsRootDomain,
      deposits_phas_root,
      1,
    ),
    deposit_count: 1,
    total_event_count: base.header.total_event_count + 1,
    transition_step_count: base.header.transition_step_count + 1,
  }
  let header_data: Data = header
  FabricatedDepositBlockV1 {
    block: fixture.NativeBlockFixtureV1 {
      ..base,
      header,
      header_hash: utils.serialise_and_hash_28(header_data),
    },
    deposits_phas_root,
    committed_deposit_id,
    committed_deposit_info,
  }
}

/// A **valid** challenged block: its header commits exactly the authentic L1
/// deposit event's identity and content. Nothing in this family may convict it.
pub fn authentic_deposit_block_v1() -> FabricatedDepositBlockV1 {
  fabricated_deposit_block_v1(
    authentic_deposit_id_v1(),
    authentic_deposit_info_v1(),
  )
}

/// The challenged block of the family's **nonexistent identity** scenario: the
/// header commits a `DepositId` no authentication transaction ever spent.
pub fn fabricated_identity_block_v1() -> FabricatedDepositBlockV1 {
  fabricated_deposit_block_v1(
    fabricated_deposit_id_v1(),
    authentic_deposit_info_v1(),
  )
}

/// The challenged block of the family's **content mismatch** scenario: the
/// header commits the authentic deposit identity with content the authentic L1
/// event never carried.
pub fn mismatched_content_block_v1() -> FabricatedDepositBlockV1 {
  fabricated_deposit_block_v1(
    authentic_deposit_id_v1(),
    diverted_deposit_info_v1(),
  )
}

/// The membership witness the block's single deposit leaf really reconstructs.
pub fn committed_deposit_membership_v1(
  block: FabricatedDepositBlockV1,
) -> CommittedDepositSourceProofV1 {
  transition_trace.RootMembershipProof {
    domain: transition_trace.DepositsRootDomain,
    root: block.block.header.deposits_root,
    phas_root: block.deposits_phas_root,
    count: block.block.header.deposit_count,
    key: block.committed_deposit_id,
    value: block.committed_deposit_info,
    proof: [],
  }
}

/// The thread token asset name of a fabricated-deposit thread against `block`.
pub fn block_thread_asset_name_v1(block: FabricatedDepositBlockV1) -> AssetName {
  computation_thread_token_asset_name_v1(block.block.header_hash)
}

/// The two reference inputs step-01 names, in the order its args index them.
pub fn block_reference_inputs_v1(block: FabricatedDepositBlockV1) -> List<Input> {
  [
    fixture.hub_reference_input(),
    fixture.state_queue_reference_input(block.block),
  ]
}

/// L1 transaction that continues a fabricated-deposit thread: the thread input,
/// the next step's output, and the caller's reference-input layout. Unlike the
/// transactions-root families this family opens no delegated membership claim,
/// so there is no `phas` withdraw redeemer to attach.
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

/// A reference input standing in for the still-unspent L1 output at `out_ref` —
/// the surviving evidence that no deposit event was ever authenticated against
/// that nonce. It carries no event NFT and no datum, exactly like the ordinary
/// wallet output a depositor would have spent.
pub fn unspent_output_reference_input_v1(out_ref: OutputReference) -> Input {
  Input {
    output_reference: out_ref,
    output: Output {
      address: address.from_verification_key(
        #"4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e",
      ),
      value: assets.from_lovelace(9_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

/// A reference input at `out_ref` carrying an inline datum, used where a witness
/// must name a live output that is not a bare wallet output.
pub fn datum_carrying_reference_input_v1(
  out_ref: OutputReference,
  datum_data: Data,
) -> Input {
  Input {
    output_reference: out_ref,
    output: Output {
      address: address.from_verification_key(
        #"4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e",
      ),
      value: assets.from_lovelace(9_000_000),
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
  }
}
