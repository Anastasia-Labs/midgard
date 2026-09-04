//// ## `fabricated-withdrawal` step-01 — committed withdrawal-source membership
//// (`GOAL_SPEC.md` §9.1 / Q40)
////
//// ### The rule this family adjudicates
////
//// A block header commits its withdrawal source set as the counted
//// `withdrawals_root`/`withdrawal_count` pair, whose leaves are
//// `(WithdrawalId, WithdrawalInfo)`: the L1 withdrawal `OutputReference` and the
//// `body`/`signature`/`validity` triple the block claims the order carried
//// (`midgard/ledger_state`). That value is wider than a deposit's — it commits
//// the withdrawal *body* (the L2 outref, owner, value and the L1 address and
//// datum the payout must go to), the *signature* that authorises it, and the
//// *validity* verdict that decides whether it pays out at all — so a header can
//// fabricate any of the three independently while keeping the other two honest.
////
//// The transition-trace family verifies that a committed withdrawal leaf is a
//// member of that root and that the projected L2 transition follows from it, but
//// every one of its withdrawal arms *starts* from the committed leaf: it assumes
//// the committed `WithdrawalInfo` is the genuine L1 order's content — its
//// `ValidWithdrawalTransition` arm even reads `source_membership.value.validity`
//// straight out of the committed leaf — and it can only run at all when the
//// prover can produce the matching authentic event NFT. A header that commits a
//// withdrawal with no authentic L1 event, or with a body, signature or validity
//// the authentic event never carried, is therefore outside its reach.
////
//// Violation identifier: **`FabricatedWithdrawal`**. A block is faulty when its
//// counted `withdrawals_root` commits a `(WithdrawalId, WithdrawalInfo)` leaf
//// that is not the authentic L1 withdrawal event pair, either because
////
//// - `NonexistentWithdrawalIdentity` — no L1 withdrawal event with that
////   `WithdrawalId` was ever authenticated, or
//// - `MismatchedWithdrawalContent` — the authentic event exists and was due for
////   this block, but its `WithdrawalInfo` is not the committed one.
////
//// ### Why non-existence is positively provable
////
//// `WithdrawalId` is the `OutputReference` of the L1 output the withdrawal
//// authentication transaction **spent**: `user_events.authenticate_new_event`
//// mints exactly one event NFT whose asset name is
//// `out_ref_to_nonce(nonce_input_out_ref)` and requires the event datum's id to
//// equal that very spent output reference. A Cardano output is spent at most
//// once, so an output reference that is *still unspent* can never have been the
//// nonce of an authenticated withdrawal event. Referencing the live output at the
//// committed `WithdrawalId` is therefore a positive, on-chain-checkable proof
//// that the committed identity is fabricated — see step-02.
////
//// ### Step-01's job
////
//// Authenticate the challenged header and prove that it really does commit the
//// leaf the rest of the family will refute. Nothing about the L1 event is read
//// here; a fabricated-withdrawal thread that cannot first exhibit the committed
//// leaf has nothing to adjudicate.
////
//// ### Mirror of `fabricated-deposit` (Q39)
////
//// The category binding, header authentication and counted-root membership
//// helpers below are family-local on purpose: the two fabricated-event families
//// share machinery but bind different roots and datum shapes, so this
//// module mirrors `midgard/fraud_proofs/fabricated_deposit/step_01` against
//// `withdrawals_root` rather than importing it or editing a shared parent-owned
//// file.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{AssetName}
use cardano/transaction.{
  Input, NoDatum, Output, OutputReference, Transaction, placeholder,
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

/// Catalogue identifier of the `fabricatedWithdrawal` category.
///
/// A category id is the `fraud_proof_catalogue.id_byte_count`-byte big-endian
/// index of the category's position in the append-only
/// `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`
/// (`demo/midgard-sdk/src/fraud-proof/catalogue.ts`), the same derivation
/// `transition_trace`'s `#"00000004"` follows from `transitionTrace` sitting at
/// index 4.
///
/// **This pin is contingent.** `fabricatedWithdrawal` appends at index 12 only if
/// `fabricatedDeposit` (Q39) takes index 11 first; neither category is registered
/// yet, because catalogue registration is parent-owned integration work. If the
/// append-only order gains any other entry before Q39 and Q40 land, or if the two
/// land in the other order, this constant and its four sibling pin sites move
/// together — they are listed in the Q40 report's race note.
pub const fabricated_withdrawal_fraud_category_id: ByteArray = #"0000000c"

/// A fabricated-withdrawal computation-thread token's asset name is this family's
/// category id followed by the 28-byte hash of the header under challenge.
pub fn fraud_category_is_fabricated_withdrawal_v1(
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    bytearray.length(computation_thread_token_asset_name) == fraud_proof_catalogue.id_byte_count + 28,
    bytearray.take(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    ) == fabricated_withdrawal_fraud_category_id,
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

/// The computation-thread token asset name of a fabricated-withdrawal thread
/// against `challenged_header_hash`.
pub fn computation_thread_token_asset_name_v1(
  challenged_header_hash: ledger_state.HeaderHash,
) -> AssetName {
  fabricated_withdrawal_fraud_category_id
    |> builtin.append_bytearray(challenged_header_hash)
}

// ## Header authentication

/// Authenticates the challenged block header, and hands back the hub datum the
/// later steps read the withdrawal event policy from.
///
/// The header is never prover-supplied. It is decoded out of the state-queue
/// node reference input the thread names, and that node's own key must be the
/// header hash the computation-thread token already commits to — the same
/// hub → state-queue → node-key chain `midgard/fraud_proofs/common`'s
/// `verify_native_tx_in_state_queue_node_with` walks before it opens a
/// transactions-root leaf. Only the root that gets opened afterwards differs,
/// because `withdrawals_root` is a header **event** root: it has no raw-CBOR
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
    fraud_category_is_fabricated_withdrawal_v1(
      computation_thread_token_asset_name,
    )

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

// ## Committed withdrawal-source membership

/// Membership witness for one `(WithdrawalId, WithdrawalInfo)` leaf of a header's
/// counted `withdrawals_root`.
pub type CommittedWithdrawalSourceProofV1 =
  transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >

/// Verifies that the challenged header commits `(membership.key,
/// membership.value)` under its counted `withdrawals_root`.
///
/// This is deliberately the *same* machinery the transition-trace family walks
/// for a committed withdrawal —
/// `transition_trace.verify_root_membership_with_bytes` under
/// `WithdrawalsRootDomain`, over `cbor.serialise` of the key and of the value,
/// bound to `header.withdrawals_root` and `header.withdrawal_count` (see
/// `midgard/fraud_proofs/transition_trace/proof`'s `verify_source_membership`
/// and its `WithdrawalSourceMembership` arm, and `validators/user-events`'
/// settlement-side membership check, which keys the same domain). Inventing a
/// second membership rule here would let Q40 convict a block over a leaf the
/// transition arms and the settlement spend path do not recognise as committed.
///
/// The counted-root wrapper is what makes `withdrawal_count` load-bearing:
/// `verify_root_membership_with_bytes` re-derives
/// `commit_counted_root(WithdrawalsRootDomain, phas_root, count)` and requires it
/// to equal `header.withdrawals_root`, so a witness that keeps the raw MPF root
/// but restates the count cannot bind. `ledger_state`'s
/// `max_withdrawal_count_v1`/`root_matches_count_v1` bounds are not re-asserted
/// here: they are enforced where the header is committed (`header_v1_is_valid`),
/// and a settled state-queue node cannot carry a header that violates them, so a
/// repeat of that check would be a branch no evidence can reach.
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

/// Blake2b-256 hash of `cbor.serialise` of a `WithdrawalInfo`.
///
/// The thread carries this hash rather than the `WithdrawalInfo` itself, and that
/// is a soundness property rather than an optimisation: the value embeds
/// `WithdrawalBody.l2_value` (a whole multi-asset value) and
/// `WithdrawalBody.l1_datum` (an `InlineDatum` of unbounded `Data`), so a thread
/// state that embedded it would let the block producer choose how many bytes
/// every later step of the dispute has to pay for — a griefing lever on the fraud
/// prover. A 32-byte commitment makes each handoff a fixed size, and comparing
/// two such hashes is equivalent to comparing the `WithdrawalInfo`s themselves,
/// which is why one comparison settles body, signature and validity fidelity at
/// once.
pub type WithdrawalInfoHash =
  H32<ledger_state.WithdrawalInfo>

/// The committed leaf's value commitment, taken from the very bytes
/// `verify_committed_withdrawal_membership_v1` fed to the counted-root check.
pub fn committed_withdrawal_info_hash_v1(
  membership: CommittedWithdrawalSourceProofV1,
) -> WithdrawalInfoHash {
  utils.serialise_and_hash_32(membership.value)
}

// ## Step-01 thread types

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  /// The committed withdrawal leaf this thread challenges.
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
// state-queue and thread builders these extend): it is not reachable from any
// on-chain validator entry point. It lives in this family's own lib modules
// rather than in the shared fixture module because Q39 and Q40 must keep
// disjoint family paths.

/// Deterministic L1 output reference standing in for a genuine withdrawal nonce —
/// the output a real withdrawal authentication transaction spent.
pub fn authentic_withdrawal_id_v1() -> ledger_state.WithdrawalId {
  OutputReference {
    transaction_id: #"8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b",
    output_index: 2,
  }
}

/// A withdrawal identity no authentication transaction ever spent — the shape a
/// header commits when it invents a withdrawal out of nothing.
pub fn fabricated_withdrawal_id_v1() -> ledger_state.WithdrawalId {
  OutputReference {
    transaction_id: #"3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a",
    output_index: 0,
  }
}

/// A second, unrelated output reference, used where a witness must name an
/// identity other than the committed one.
pub fn foreign_withdrawal_id_v1() -> ledger_state.WithdrawalId {
  OutputReference {
    transaction_id: #"c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4",
    output_index: 1,
  }
}

/// The order an authentic withdrawal event carries: the L2 UTxO being withdrawn,
/// its owner and value, the L1 destination, the owner's signature, and the
/// `WithdrawalIsValid` verdict that makes it payable.
pub fn authentic_withdrawal_info_v1() -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: OutputReference {
        transaction_id: #"7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e",
        output_index: 1,
      },
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

/// The same order paid to a different L1 address — the shape a header commits
/// when it fabricates the withdrawal *body*, redirecting a real payout.
pub fn diverted_withdrawal_info_v1() -> ledger_state.WithdrawalInfo {
  let authentic = authentic_withdrawal_info_v1()
  ledger_state.WithdrawalInfo {
    ..authentic,
    body: ledger_state.WithdrawalBody {
      ..authentic.body,
      l1_address: address.from_verification_key(
        #"5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d",
      ),
    },
  }
}

/// The same body under a signature the owner never produced — the shape a header
/// commits when it fabricates the *authorisation* of a real order.
pub fn forged_signature_withdrawal_info_v1() -> ledger_state.WithdrawalInfo {
  let authentic = authentic_withdrawal_info_v1()
  ledger_state.WithdrawalInfo {
    ..authentic,
    signature: Pair(
      #"adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad",
      #"f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0",
    ),
  }
}

/// The same body and signature under a validity verdict the authentic event
/// never carried — the shape a header commits when it fabricates the *verdict*,
/// which is the field `transition_trace/proof`'s `ValidWithdrawalTransition` arm
/// reads straight out of the committed leaf.
pub fn overridden_validity_withdrawal_info_v1() -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    ..authentic_withdrawal_info_v1(),
    validity: ledger_state.NonExistentWithdrawalUtxo,
  }
}

/// A single-transaction native-V1 block whose header additionally commits
/// exactly one withdrawal leaf under its counted `withdrawals_root`.
pub type FabricatedWithdrawalBlockV1 {
  block: fixture.NativeBlockFixtureV1,
  withdrawals_phas_root: ByteArray,
  committed_withdrawal_id: ledger_state.WithdrawalId,
  committed_withdrawal_info: ledger_state.WithdrawalInfo,
}

/// Commits `(committed_withdrawal_id, committed_withdrawal_info)` as the single
/// leaf of the block's raw withdrawals MPF and lets the shared fixture re-derive
/// the counted `withdrawals_root`, the dependent event counts and the header
/// hash.
///
/// Unlike the deposit mirror, no header override is needed:
/// `native_binding_fixture_v1.native_block_fixture_with_withdrawals_v1` already
/// takes a raw withdrawals PHAS root and a cardinality and commits them under
/// `WithdrawalsRootDomain`, so this builder feeds the shared fixture rather than
/// rewriting the header it produced.
pub fn fabricated_withdrawal_block_v1(
  committed_withdrawal_id: ledger_state.WithdrawalId,
  committed_withdrawal_info: ledger_state.WithdrawalInfo,
) -> FabricatedWithdrawalBlockV1 {
  let withdrawals_phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(committed_withdrawal_id),
      cbor.serialise(committed_withdrawal_info),
      [],
    ),
  )
  FabricatedWithdrawalBlockV1 {
    block: fixture.native_block_fixture_with_withdrawals_v1(
      fixture.valid_native_tx_v1(),
      withdrawals_phas_root,
      1,
    ),
    withdrawals_phas_root,
    committed_withdrawal_id,
    committed_withdrawal_info,
  }
}

/// A **valid** challenged block: its header commits exactly the authentic L1
/// withdrawal event's identity and content. Nothing in this family may convict
/// it.
pub fn authentic_withdrawal_block_v1() -> FabricatedWithdrawalBlockV1 {
  fabricated_withdrawal_block_v1(
    authentic_withdrawal_id_v1(),
    authentic_withdrawal_info_v1(),
  )
}

/// The challenged block of the family's **nonexistent identity** scenario: the
/// header commits a `WithdrawalId` no authentication transaction ever spent.
pub fn fabricated_identity_block_v1() -> FabricatedWithdrawalBlockV1 {
  fabricated_withdrawal_block_v1(
    fabricated_withdrawal_id_v1(),
    authentic_withdrawal_info_v1(),
  )
}

/// The challenged block of the family's **content mismatch** scenario: the header
/// commits the authentic withdrawal identity with a body the authentic L1 event
/// never carried.
pub fn mismatched_content_block_v1() -> FabricatedWithdrawalBlockV1 {
  fabricated_withdrawal_block_v1(
    authentic_withdrawal_id_v1(),
    diverted_withdrawal_info_v1(),
  )
}

/// The membership witness the block's single withdrawal leaf really
/// reconstructs.
pub fn committed_withdrawal_membership_v1(
  block: FabricatedWithdrawalBlockV1,
) -> CommittedWithdrawalSourceProofV1 {
  transition_trace.RootMembershipProof {
    domain: transition_trace.WithdrawalsRootDomain,
    root: block.block.header.withdrawals_root,
    phas_root: block.withdrawals_phas_root,
    count: block.block.header.withdrawal_count,
    key: block.committed_withdrawal_id,
    value: block.committed_withdrawal_info,
    proof: [],
  }
}

/// The thread token asset name of a fabricated-withdrawal thread against
/// `block`.
pub fn block_thread_asset_name_v1(
  block: FabricatedWithdrawalBlockV1,
) -> AssetName {
  computation_thread_token_asset_name_v1(block.block.header_hash)
}

/// The two reference inputs step-01 names, in the order its args index them.
pub fn block_reference_inputs_v1(
  block: FabricatedWithdrawalBlockV1,
) -> List<Input> {
  [
    fixture.hub_reference_input(),
    fixture.state_queue_reference_input(block.block),
  ]
}

/// L1 transaction that continues a fabricated-withdrawal thread: the thread
/// input, the next step's output, and the caller's reference-input layout. Unlike
/// the transactions-root families this family opens no delegated membership
/// claim, so there is no `phas` withdraw redeemer to attach.
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
/// the surviving evidence that no withdrawal event was ever authenticated against
/// that nonce. It carries no event NFT and no datum, exactly like the ordinary
/// wallet output a withdrawer would have spent.
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
