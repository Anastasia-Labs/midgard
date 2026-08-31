//// ## `cross-block-duplicate-event` step-01 — committed duplicate-event
//// membership (catalogue-status §6, decision gate D-S11)
////
//// ### The rule this family adjudicates
////
//// Every L1 event (a deposit, withdrawal order, or forced transaction order)
//// must be applied by
//// **exactly one** L2 block. Block event windows are contiguous and strictly
//// monotone (`timed_l1_event_is_due`: `start_time < t <= end_time`, and each
//// header's `start_time` is its parent's `end_time`), so an event's
//// `inclusion_time` is due in exactly one block's window. A second block that
//// commits the same event key under the same counted source root applies the
//// event twice — a deposit credited twice, or a withdrawal paid twice — which
//// is fund theft.
////
//// Violation identifier: **`CrossBlockDuplicateEvent`**. A live state-queue
//// block is faulty when its counted `deposits_root` (resp.
//// `withdrawals_root`) commits an event key that a **settlement node** — the
//// confirmed history — also commits under the same root domain.
////
//// ### The D-S11 reading this family takes (recorded per the open gate)
////
//// D-S11 leaves the duplicate-event adjudication rule unstated. The reading
//// implemented here is the most canonical-V1-consistent one:
////
//// 1. **Confirmed-vs-live duplicates convict the live block, key-only.** A
////    settlement node is spawned at merge, its NFT's asset name is the merged
////    header's hash, and its datum retains the merged block's four event
////    roots. Every live block's window lies strictly after every confirmed
////    block's window, so an event a settlement absorbed can never again be
////    *due* for a live block: same-key membership in both roots convicts
////    without reading the event's `inclusion_time` — which is exactly what
////    lets the proof survive the event NFT's burn (the NFT burns when the
////    confirmed block's settlement resolves the event, spec 1-deposit).
//// 2. **Live-vs-live duplicates are deliberately out of scope.** With both
////    blocks still live, identifying the culprit requires the event's
////    `inclusion_time` (a poisoned ancestor can include an event *early*,
////    while `OmittedDueL1Event` **forces** the descendant whose window covers
////    the event to include it too — convicting the descendant would convict
////    an honest, forced operator). Both-live duplicates always still have a
////    live event NFT (only a confirmed block's settlement burns it), so the
////    existing transition-trace `OutOfWindowSourceEvent` family already
////    convicts the out-of-window copy with full evidence.
//// 3. **Removed ancestors change nothing.** Settlement nodes are final and
////    unremovable, so the confirmed side of this proof cannot be retracted; a
////    removed *live* block never burned any event NFT, and post-removal
////    re-inclusion legitimacy stays with the due-window machinery (the next
////    block's event interval covers removed intervals).
////
//// Residual (recorded, not solved here): once a settlement node itself
//// resolves and its NFT burns, a much-later duplicate of an event it carried
//// has no on-chain witness left — that is coverage-matrix §7's cross-cutting
//// evidence-liveness row, a protocol-level remainder of D-S11.
////
//// ### Scope boundary
////
//// The sibling `double-withdraw` family adjudicates two *distinct* withdrawal
//// orders draining the same L2 funds. This family adjudicates the **same L1
//// event key** applied by two blocks; the two predicates share no arm.
////
//// ### Step-01's job
////
//// Authenticate the challenged header out of the state-queue node the
//// computation thread names, and prove the header really commits the event
//// leaf whose duplication step-02 adjudicates. Binding is exclusively through
//// the canonical counted-root machinery (catalogue-status §1a):
//// `transition_trace.verify_root_membership_with_bytes` under the same domain
//// the transition-trace family and the settlement spend path key.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{AssetName}
use cardano/transaction.{Input, OutputReference, Transaction, placeholder}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/rejection_reason_v1
use midgard/state_queue
use midgard/transition_trace

// ## Catalogue binding

/// Catalogue identifier of the `crossBlockDuplicateEvent` category.
///
/// The category is **not registered yet**: catalogue registration is
/// parent-owned integration work, and `#"00000016"` is the id reserved for
/// this family's pre-registration emulator wiring (the same posture
/// `native-script-decoding` holds with `#"0000000d"`). It appears in the
/// emulator catalogue only — never in the deployment manifest, whose parser
/// drops non-canonical keys.
pub const cross_block_duplicate_event_fraud_category_id: ByteArray = #"00000016"

/// A cross-block-duplicate-event computation-thread token's asset name is this
/// family's category id followed by the 28-byte hash of the header under
/// challenge.
pub fn fraud_category_is_cross_block_duplicate_event_v1(
  computation_thread_token_asset_name: AssetName,
) -> Bool {
  and {
    bytearray.length(computation_thread_token_asset_name) == fraud_proof_catalogue.id_byte_count + 28,
    bytearray.take(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    ) == cross_block_duplicate_event_fraud_category_id,
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

/// The computation-thread token asset name of a cross-block-duplicate-event
/// thread against `challenged_header_hash`.
pub fn computation_thread_token_asset_name_v1(
  challenged_header_hash: ledger_state.HeaderHash,
) -> AssetName {
  cross_block_duplicate_event_fraud_category_id
    |> builtin.append_bytearray(challenged_header_hash)
}

// ## Header authentication

/// Authenticates the challenged block header, and hands back the hub datum
/// step-02 reads the settlement policy from.
///
/// The header is never prover-supplied: it is decoded out of the state-queue
/// node reference input whose linked-list key must equal the header hash the
/// thread token already commits to — the same hub → state-queue → node-key
/// chain every counted-root family walks (catalogue-status §1a).
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
    fraud_category_is_cross_block_duplicate_event_v1(
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

// ## Committed duplicate-event membership

/// Membership witness for one `(DepositId, DepositInfo)` leaf of a counted
/// `deposits_root`.
pub type CommittedDepositSourceProofV1 =
  transition_trace.RootMembershipProof<
    ledger_state.DepositId,
    ledger_state.DepositInfo,
  >

/// Membership witness for one `(WithdrawalId, WithdrawalInfo)` leaf of a
/// counted `withdrawals_root`.
pub type CommittedWithdrawalSourceProofV1 =
  transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >

/// Membership witness for one `(TxOrderId, ForcedInclusionTxV1)` leaf of a
/// counted `forced_transactions_root`.
pub type CommittedForcedTransactionSourceProofV1 =
  transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >

/// Which counted source-root domain the duplicated event lives in.
///
pub type DuplicateEventKindV1 {
  DuplicateDepositV1
  DuplicateWithdrawalV1
  DuplicateForcedTransactionV1
}

/// The committed event leaf under challenge, in one of the two implemented
/// counted source-root domains.
///
/// The **key** is the event's L1 identity (`DepositId` and `WithdrawalId` are
/// both the `OutputReference` the authentication transaction spent, so a key
/// collision *is* an event collision). The value travels only to feed the
/// membership hash: the duplicate verdict is key-only on purpose, because a
/// second application of the same event is theft whether or not the second
/// block also restated the event's content faithfully.
pub type CommittedDuplicateEventProofV1 {
  CommittedDuplicateDepositV1 { membership: CommittedDepositSourceProofV1 }
  CommittedDuplicateWithdrawalV1 {
    membership: CommittedWithdrawalSourceProofV1,
  }
  CommittedDuplicateForcedTransactionV1 {
    membership: CommittedForcedTransactionSourceProofV1,
  }
}

/// Verifies that the challenged header commits `(membership.key,
/// membership.value)` under the counted root of the witness's own domain —
/// `deposits_root`/`deposit_count` or `withdrawals_root`/`withdrawal_count`.
///
/// This is deliberately the *same* machinery the transition-trace family walks
/// for a committed source event and the settlement spend path keys on the
/// resolution side: `transition_trace.verify_root_membership_with_bytes` over
/// `cbor.serialise` of the key and of the value. Inventing a second membership
/// rule here would let this family convict a block over a leaf no other
/// machinery recognises as committed (catalogue-status §1a).
pub fn verify_committed_event_membership_v1(
  header: ledger_state.HeaderV1,
  committed_event: CommittedDuplicateEventProofV1,
) -> Bool {
  when committed_event is {
    CommittedDuplicateDepositV1 { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.DepositsRootDomain,
        header.deposits_root,
        header.deposit_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
    CommittedDuplicateWithdrawalV1 { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.WithdrawalsRootDomain,
        header.withdrawals_root,
        header.withdrawal_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
    CommittedDuplicateForcedTransactionV1 { membership } ->
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.ForcedTransactionsV1RootDomain,
        header.forced_transactions_root,
        header.forced_transaction_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
  }
}

/// The challenged leaf's domain, as step-02 state.
pub fn committed_event_kind_v1(
  committed_event: CommittedDuplicateEventProofV1,
) -> DuplicateEventKindV1 {
  when committed_event is {
    CommittedDuplicateDepositV1 { .. } -> DuplicateDepositV1
    CommittedDuplicateWithdrawalV1 { .. } -> DuplicateWithdrawalV1
    CommittedDuplicateForcedTransactionV1 { .. } -> DuplicateForcedTransactionV1
  }
}

/// The challenged leaf's L1 event identity, as step-02 state.
pub fn committed_event_key_v1(
  committed_event: CommittedDuplicateEventProofV1,
) -> OutputReference {
  when committed_event is {
    CommittedDuplicateDepositV1 { membership } -> membership.key
    CommittedDuplicateWithdrawalV1 { membership } -> membership.key
    CommittedDuplicateForcedTransactionV1 { membership } -> membership.key
  }
}

// ## Step-01 thread types

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  /// The committed event leaf this thread challenges as a duplicate.
  committed_event: CommittedDuplicateEventProofV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## Family test fixtures
//
// Test-fixture support for this family's step selectors, extending
// `midgard/fraud_proofs/native_binding_fixture_v1`'s block, hub, state-queue
// and thread builders. Not reachable from any on-chain validator entry point.

/// Deterministic L1 output reference standing in for the duplicated event's
/// nonce — the output its authentication transaction spent.
pub fn duplicated_event_id_v1() -> OutputReference {
  OutputReference {
    transaction_id: #"8181818181818181818181818181818181818181818181818181818181818181",
    output_index: 2,
  }
}

/// A second, unrelated event identity, used where a witness must name an
/// event other than the duplicated one.
pub fn foreign_event_id_v1() -> OutputReference {
  OutputReference {
    transaction_id: #"9292929292929292929292929292929292929292929292929292929292929292",
    output_index: 0,
  }
}

/// The L2 credit the duplicated deposit event carries.
pub fn duplicated_deposit_info_v1() -> ledger_state.DepositInfo {
  ledger_state.DepositInfo {
    l2_address: address.from_verification_key(
      #"1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a",
    ),
    l2_network_id: 0,
    l2_datum: None,
  }
}

/// The payout order the duplicated withdrawal event carries.
pub fn duplicated_withdrawal_info_v1() -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: OutputReference {
        transaction_id: #"2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b",
        output_index: 1,
      },
      l2_owner: #"3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c",
      l2_value: [],
      l1_address: address.from_verification_key(
        #"4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d",
      ),
      l1_datum: transaction.NoDatum,
    },
    signature: Pair(
      #"5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e",
      #"6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f"
        |> builtin.append_bytearray(
            #"7070707070707070707070707070707070707070707070707070707070707070",
          ),
    ),
    validity: ledger_state.WithdrawalIsValid,
  }
}

/// The forced-inclusion value committed for the duplicated tx-order identity.
/// The duplicate predicate is key-only; nevertheless the exact value travels
/// through both authenticated membership openings so neither root can be
/// substituted or recomputed over a different leaf.
pub fn duplicated_forced_transaction_v1() -> ledger_state.ForcedInclusionTxV1 {
  ledger_state.ForcedInclusionTxV1 {
    tx_id: #"1111111111111111111111111111111111111111111111111111111111111111",
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor: #"80",
      witness_set_compact_cbor: #"80",
      field_preimage_lengths_cbor: #"80",
    },
    verdict: rejection_reason_v1.ForcedTxValid,
  }
}

/// A single-transaction native-V1 block whose header additionally commits
/// exactly one event leaf under the counted root of one source domain.
pub type DuplicateEventBlockV1 {
  block: fixture.NativeBlockFixtureV1,
  event_phas_root: ByteArray,
  committed_event: CommittedDuplicateEventProofV1,
}

/// Commits `(key, value)` as the single leaf of the block's raw deposits MPF
/// and re-derives the counted `deposits_root`, the dependent event counts and
/// the header hash.
pub fn deposit_event_block_v1(
  key: ledger_state.DepositId,
  value: ledger_state.DepositInfo,
) -> DuplicateEventBlockV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let event_phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(value), []),
  )
  let header = ledger_state.HeaderV1 {
    ..base.header,
    deposits_root: transition_trace.commit_counted_root(
      transition_trace.DepositsRootDomain,
      event_phas_root,
      1,
    ),
    deposit_count: 1,
    total_event_count: base.header.total_event_count + 1,
    transition_step_count: base.header.transition_step_count + 1,
  }
  let header_data: Data = header
  let block = fixture.NativeBlockFixtureV1 {
    ..base,
    header,
    header_hash: utils.serialise_and_hash_28(header_data),
  }
  DuplicateEventBlockV1 {
    block,
    event_phas_root,
    committed_event: CommittedDuplicateDepositV1 {
      membership: transition_trace.RootMembershipProof {
        domain: transition_trace.DepositsRootDomain,
        root: header.deposits_root,
        phas_root: event_phas_root,
        count: 1,
        key,
        value,
        proof: [],
      },
    },
  }
}

/// Commits `(key, value)` as the single leaf of the block's raw withdrawals
/// MPF, through the shared fixture's withdrawal-carrying block builder.
pub fn withdrawal_event_block_v1(
  key: ledger_state.WithdrawalId,
  value: ledger_state.WithdrawalInfo,
) -> DuplicateEventBlockV1 {
  let event_phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(value), []),
  )
  let block = fixture.native_block_fixture_with_withdrawals_v1(
    fixture.valid_native_tx_v1(),
    event_phas_root,
    1,
  )
  DuplicateEventBlockV1 {
    block,
    event_phas_root,
    committed_event: CommittedDuplicateWithdrawalV1 {
      membership: transition_trace.RootMembershipProof {
        domain: transition_trace.WithdrawalsRootDomain,
        root: block.header.withdrawals_root,
        phas_root: event_phas_root,
        count: 1,
        key,
        value,
        proof: [],
      },
    },
  }
}

/// Commits `(key, value)` as the single leaf of the block's raw forced-order
/// MPF and re-derives the counted root and dependent header counters.
pub fn forced_transaction_event_block_v1(
  key: ledger_state.TxOrderId,
  value: ledger_state.ForcedInclusionTxV1,
) -> DuplicateEventBlockV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let event_phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(value), []),
  )
  let header = ledger_state.HeaderV1 {
    ..base.header,
    forced_transactions_root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      event_phas_root,
      1,
    ),
    forced_transaction_count: 1,
    total_event_count: base.header.total_event_count + 1,
    transition_step_count: base.header.transition_step_count + 1,
  }
  let header_data: Data = header
  let block = fixture.NativeBlockFixtureV1 {
    ..base,
    header,
    header_hash: utils.serialise_and_hash_28(header_data),
  }
  DuplicateEventBlockV1 {
    block,
    event_phas_root,
    committed_event: CommittedDuplicateForcedTransactionV1 {
      membership: transition_trace.RootMembershipProof {
        domain: transition_trace.ForcedTransactionsV1RootDomain,
        root: header.forced_transactions_root,
        phas_root: event_phas_root,
        count: 1,
        key,
        value,
        proof: [],
      },
    },
  }
}

/// The challenged block of the family's deposit scenario: it commits the
/// duplicated deposit event a settlement node already absorbed.
pub fn duplicate_deposit_block_v1() -> DuplicateEventBlockV1 {
  deposit_event_block_v1(duplicated_event_id_v1(), duplicated_deposit_info_v1())
}

/// The challenged block of the family's withdrawal scenario.
pub fn duplicate_withdrawal_block_v1() -> DuplicateEventBlockV1 {
  withdrawal_event_block_v1(
    duplicated_event_id_v1(),
    duplicated_withdrawal_info_v1(),
  )
}

/// The challenged block of the family's forced transaction-order scenario.
pub fn duplicate_forced_transaction_block_v1() -> DuplicateEventBlockV1 {
  forced_transaction_event_block_v1(
    duplicated_event_id_v1(),
    duplicated_forced_transaction_v1(),
  )
}

/// The thread token asset name of a cross-block-duplicate-event thread against
/// `block`.
pub fn block_thread_asset_name_v1(block: DuplicateEventBlockV1) -> AssetName {
  computation_thread_token_asset_name_v1(block.block.header_hash)
}

/// The two reference inputs step-01 names, in the order its args index them.
pub fn block_reference_inputs_v1(block: DuplicateEventBlockV1) -> List<Input> {
  [
    fixture.hub_reference_input(),
    fixture.state_queue_reference_input(block.block),
  ]
}

/// L1 transaction that continues a cross-block-duplicate-event thread: the
/// thread input, the next step's output, and the caller's reference-input
/// layout. This family opens no delegated membership claim, so there is no
/// `phas` withdraw redeemer to attach.
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
