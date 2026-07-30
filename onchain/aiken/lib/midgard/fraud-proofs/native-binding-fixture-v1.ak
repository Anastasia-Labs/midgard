//// Test-fixture support for the native-V1 counted-root binding selectors of
//// the fault-proof step validators (GOAL_SPEC.md Q00).
////
//// This module is referenced exclusively by tests embedded in the
//// `validators/fraud-proofs/**` step modules. It builds a real single-tx
//// native-V1 block fixture (canonical compact CBOR, genuine MPF transactions
//// root, counted-root committed header) plus the L1 transactions that drive a
//// computation-thread `Continue` or finalize step against it. It is not
//// reachable from any on-chain validator entry point.

use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/linked_list
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Redeemer,
  ScriptPurpose, Transaction, Withdraw, placeholder,
}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/hub_oracle
use midgard/ledger_state
use midgard/state_queue
use midgard/transition_trace

// ## Deterministic identities

pub const hub_oracle_script_hash =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

pub const state_queue_policy_id =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

pub const computation_thread_policy_id =
  #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

pub const step_script_hash =
  #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

pub const next_step_script_hash =
  #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

pub const fraud_proof_policy_id =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

pub const fraud_prover_key_hash =
  #"11111111111111111111111111111111111111111111111111111111"

pub const fraud_category_id = #"00000001"

const h32_a =
  #"1010101010101010101010101010101010101010101010101010101010101010"

const h32_b =
  #"2020202020202020202020202020202020202020202020202020202020202020"

const h28_x = #"30303030303030303030303030303030303030303030303030303030"

pub fn own_out_ref() -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: 0 }
}

pub fn fraud_proof_token_address() -> Address {
  address.from_script(fraud_proof_policy_id)
}

// ## Canonical native-V1 transactions

/// A representative, fully canonical native-V1 transaction as committed by a
/// well-formed block: one spend input, one reference input, one output, a fee,
/// one required signer, and one address witness whose key hashes to the
/// required signer.
pub fn valid_native_tx_v1() -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  native_types.MidgardTransaction {
    version: native_types.native_tx_version_v1,
    validity: native_types.TxIsValid,
    body: native_types.MidgardTransactionBody {
      inputs: [native_types.MidgardTxInput { tx_id: h32_b, output_index: 0 }],
      reference_inputs: [
        native_types.MidgardTxInput { tx_id: h32_b, output_index: 1 },
      ],
      outputs: [sample_native_output()],
      fee: 1_000_000,
      validity_interval_start: env.posix_time_none,
      validity_interval_end: env.posix_time_none,
      required_observers: [],
      required_signers: [
        utils.get_verification_key_hash(sample_witness_verification_key()),
      ],
      mint: empty_mint,
      script_integrity_hash: h32_a,
      auxiliary_data_hash: h32_a,
      network_id: 0,
    },
    witness_set: native_types.MidgardTransactionWitnessSet {
      addr_tx_wits: [sample_address_witness()],
      script_tx_wits: [sample_native_script_witness()],
      redeemer_tx_wits: [],
    },
  }
}

pub fn sample_native_output() -> native_types.MidgardTxOutput {
  native_types.MidgardTxOutput {
    address: native_types.MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: native_types.PubKeyCredential(h28_x),
      stake_credential: None,
    },
    value: native_types.MidgardValue { lovelace: 5_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

pub fn sample_witness_verification_key() -> ByteArray {
  #"4040404040404040404040404040404040404040404040404040404040404040"
}

pub fn sample_witness_signature() -> ByteArray {
  #"5050505050505050505050505050505050505050505050505050505050505050"
    |> builtin.append_bytearray(
        #"6060606060606060606060606060606060606060606060606060606060606060",
      )
}

pub fn sample_address_witness() -> native_types.MidgardAddressWitness {
  native_types.MidgardAddressWitness {
    verification_key: sample_witness_verification_key(),
    signature: sample_witness_signature(),
  }
}

pub fn sample_native_script_bytes() -> ByteArray {
  #"8200581c303030303030303030303030303030303030303030303030303030"
}

pub fn sample_native_script_witness() -> native_types.MidgardVersionedScript {
  native_types.MidgardVersionedScript {
    language: native_types.NativeCardanoScript,
    script_bytes: sample_native_script_bytes(),
  }
}

// ## Single-transaction native-V1 block fixture

pub type NativeBlockFixtureV1 {
  tx: native_types.MidgardTransaction,
  tx_id: ByteArray,
  compact: native_types.NativeTxCompact,
  compact_cbor: ByteArray,
  witness_set_compact: native_types.NativeTxWitnessSetCompact,
  transactions_phas_root: ByteArray,
  header: ledger_state.HeaderV1,
  header_hash: ByteArray,
}

/// Commits `tx` as the single transaction of a native-V1 block: the raw MPF
/// transactions root holds exactly `(tx_id, compact_cbor)` and the header
/// commits it under the `TransactionsV1RootDomain` counted root.
pub fn native_block_fixture_v1(
  tx: native_types.MidgardTransaction,
) -> NativeBlockFixtureV1 {
  native_block_fixture_with_withdrawals_v1(tx, env.empty_merkle_tree_root, 0)
}

pub fn native_block_fixture_with_withdrawals_v1(
  tx: native_types.MidgardTransaction,
  withdrawals_phas_root: ByteArray,
  withdrawal_count: Int,
) -> NativeBlockFixtureV1 {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let compact_cbor = native_compact.encode_native_tx_compact_v1(compact)
  let tx_id = native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
  let witness_set_compact = native_transaction.midgard_transaction_witness_set_to_compact(
    tx.witness_set,
  )
  let transactions_phas_root = mpf.root(
    mpf.insert(mpf.empty, tx_id, compact_cbor, []),
  )
  let withdrawals_root = transition_trace.commit_counted_root(
    transition_trace.WithdrawalsRootDomain,
    withdrawals_phas_root,
    withdrawal_count,
  )
  let header = ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      transactions_phas_root,
      1,
    ),
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count,
    forced_transaction_count: 0,
    l2_transaction_count: 1,
    deposit_count: 0,
    total_event_count: withdrawal_count + 1,
    transition_step_count: withdrawal_count + 1,
    validation_trace_count: 1,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_x,
    operator_vkey: h28_x,
    protocol_version: ledger_state.protocol_version_v1,
  }
  let header_data: Data = header
  let header_hash = utils.serialise_and_hash_28(header_data)
  NativeBlockFixtureV1 {
    tx,
    tx_id,
    compact,
    compact_cbor,
    witness_set_compact,
    transactions_phas_root,
    header,
    header_hash,
  }
}

pub fn ct_token_asset_name(fixture: NativeBlockFixtureV1) -> ByteArray {
  fraud_category_id |> builtin.append_bytearray(fixture.header_hash)
}

/// The prover-side inclusion arguments matching `continue_step_l1_tx_v1`.
pub fn native_inclusion_args_v1(
  fixture: NativeBlockFixtureV1,
) -> NativeTxInclusionArgs {
  NativeTxInclusionArgs {
    input_index: 0,
    output_index: 0,
    hub_ref_input_index: 0,
    state_queue_node_ref_input_index: 1,
    native_tx_id: fixture.tx_id,
    native_tx_compact_cbor: fixture.compact_cbor,
    transactions_phas_root: fixture.transactions_phas_root,
    tx_membership_proof: [],
    inclusion_proof_script_withdraw_redeemer_index: 0,
  }
}

pub fn step_datum_without_state() -> ct.StepDatum<Data> {
  ct.StepDatum { fraud_prover: fraud_prover_key_hash, data: None }
}

// ## L1 transaction assembly

pub fn ct_token_value(asset_name: ByteArray) {
  assets.from_lovelace(2_000_000)
    |> assets.add(computation_thread_policy_id, asset_name, 1)
}

pub fn ct_thread_input(asset_name: ByteArray) -> Input {
  step_input_with_value(ct_token_value(asset_name))
}

/// Thread-step input at the step script address holding an arbitrary value
/// (used by token-coupling negatives).
pub fn step_input_with_value(value: assets.Value) -> Input {
  Input {
    output_reference: own_out_ref(),
    output: Output {
      address: address.from_script(step_script_hash),
      value,
      datum: NoDatum,
      reference_script: None,
    },
  }
}

pub fn next_step_output(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Output {
  step_output_with_value(ct_token_value(asset_name), output_state_data)
}

/// Next-step output carrying an arbitrary value with a well-formed
/// `StepDatum` (used by token-coupling negatives).
pub fn step_output_with_value(
  value: assets.Value,
  output_state_data: Data,
) -> Output {
  let output_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fraud_prover_key_hash,
    data: Some(output_state_data),
  }
  let output_datum_data: Data = output_datum
  Output {
    address: address.from_script(next_step_script_hash),
    value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  }
}

pub fn hub_reference_input() -> Input {
  hub_reference_input_with_nft_policy(hub_oracle_script_hash)
}

/// Hub-oracle reference input whose authenticating NFT is minted under an
/// arbitrary policy (identity-substitution negatives).
pub fn hub_reference_input_with_nft_policy(nft_policy: ByteArray) -> Input {
  let dummy_addr = address.from_script(h28_x)
  let hub_datum = hub_oracle.Datum {
    registered_operators: h28_x,
    active_operators: h28_x,
    retired_operators: h28_x,
    scheduler: h28_x,
    state_queue: state_queue_policy_id,
    fraud_proof_catalogue: h28_x,
    fraud_proof: fraud_proof_policy_id,
    deposit: h28_x,
    withdrawal: h28_x,
    tx_order: h28_x,
    settlement: h28_x,
    payout: h28_x,
    registered_operators_addr: dummy_addr,
    active_operators_addr: dummy_addr,
    retired_operators_addr: dummy_addr,
    scheduler_addr: dummy_addr,
    state_queue_addr: dummy_addr,
    fraud_proof_catalogue_addr: dummy_addr,
    fraud_proof_addr: fraud_proof_token_address(),
    deposit_addr: dummy_addr,
    withdrawal_addr: dummy_addr,
    tx_order_addr: dummy_addr,
    settlement_addr: dummy_addr,
    reserve_addr: dummy_addr,
    payout_addr: dummy_addr,
    reserve_observer: h28_x,
  }
  let hub_datum_data: Data = hub_datum
  Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 0 },
    output: Output {
      address: address.from_script(hub_oracle_script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(nft_policy, hub_oracle.asset_name, 1),
      datum: InlineDatum(hub_datum_data),
      reference_script: None,
    },
  }
}

pub fn state_queue_reference_input(fixture: NativeBlockFixtureV1) -> Input {
  state_queue_reference_input_with_policy(fixture, state_queue_policy_id)
}

/// State-queue node reference input whose authenticating NFT is minted under
/// an arbitrary policy (block-registry substitution negatives).
pub fn state_queue_reference_input_with_policy(
  fixture: NativeBlockFixtureV1,
  nft_policy: ByteArray,
) -> Input {
  let node = state_queue.StateQueueNode {
    header: fixture.header,
    da_attestation: state_queue.no_da_attestation,
  }
  let element: linked_list.Element<Data, state_queue.StateQueueNode> = linked_list.Element {
    data: linked_list.Node { data: node },
    link: None,
  }
  let element_data: Data = element
  Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 1 },
    output: Output {
      address: address.from_script(nft_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            nft_policy,
            state_queue.block_asset_name_prefix
              |> builtin.append_bytearray(fixture.header_hash),
            1,
          ),
      datum: InlineDatum(element_data),
      reference_script: None,
    },
  }
}

/// Deployed fault-proof catalogue identity used by init fixtures.
pub const catalogue_script_hash =
  #"77777777777777777777777777777777777777777777777777777777"

pub fn catalogue_reference_input(catalogue_root: ByteArray) -> Input {
  catalogue_reference_input_with_nft_policy(
    catalogue_root,
    catalogue_script_hash,
  )
}

/// Catalogue reference input whose authenticating NFT is minted under an
/// arbitrary policy (catalogue substitution negatives).
pub fn catalogue_reference_input_with_nft_policy(
  catalogue_root: ByteArray,
  nft_policy: ByteArray,
) -> Input {
  let catalogue_datum_data: Data = catalogue_root
  Input {
    output_reference: OutputReference { transaction_id: h32_b, output_index: 2 },
    output: Output {
      address: address.from_script(catalogue_script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(nft_policy, fraud_proof_catalogue.asset_name, 1),
      datum: InlineDatum(catalogue_datum_data),
      reference_script: None,
    },
  }
}

/// L1 transaction that cancels a computation thread: spends the thread input
/// and invokes the thread policy's `BurnForCancellation` redeemer for
/// `burn_asset_name`, optionally signed by the fraud prover.
pub fn cancel_step_l1_tx_v1(
  asset_name: ByteArray,
  burn_asset_name: ByteArray,
  signed_by_prover: Bool,
) -> Transaction {
  let mint_redeemer: Data = ct.BurnForCancellation {
    burning_token_asset_name: burn_asset_name,
  }
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    redeemers: [Pair(Mint(computation_thread_policy_id), mint_redeemer)],
    extra_signatories: if signed_by_prover {
      [fraud_prover_key_hash]
    } else {
      []
    },
  }
}

/// Withdraw redeemer entry mirroring the delegated Plutarch `phas` script
/// invocation for the given raw-root membership claim.
pub fn phas_withdraw_redeemer_entry(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> Pair<ScriptPurpose, Redeemer> {
  let membership_proof: mpf.Proof = []
  let proof_data: Data = membership_proof
  let payload: List<Data> = [
    builtin.b_data(merkle_root),
    builtin.b_data(key_bytes),
    builtin.b_data(value_bytes),
    proof_data,
  ]
  Pair(
    Withdraw(Script(env.plutarch_phas_validator_hash)),
    builtin.list_data(payload),
  )
}

/// L1 transaction that continues a first-step computation thread against the
/// fixture block: computation-thread input/output, hub-oracle and state-queue
/// reference inputs, and the delegated membership withdraw redeemer.
pub fn continue_step_l1_tx_v1(
  fixture: NativeBlockFixtureV1,
  output_state_data: Data,
  membership_root: ByteArray,
  membership_key: ByteArray,
  membership_value: ByteArray,
) -> Transaction {
  let asset_name = ct_token_asset_name(fixture)
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [next_step_output(asset_name, output_state_data)],
    reference_inputs: [
      hub_reference_input(),
      state_queue_reference_input(fixture),
    ],
    redeemers: [
      phas_withdraw_redeemer_entry(
        membership_root,
        membership_key,
        membership_value,
      ),
    ],
  }
}

/// L1 transaction that continues a mid-pipeline computation thread (no block
/// reference inputs and no delegated membership redeemer).
pub fn continue_mid_step_l1_tx_v1(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [next_step_output(asset_name, output_state_data)],
  }
}

/// L1 transaction that finalizes a computation thread into a fraud-proof NFT
/// at the always-fails fraud-proof address.
pub fn finalize_step_l1_tx_v1(asset_name: ByteArray) -> Transaction {
  let fraud_proof_datum: Data = fraud_proof.Datum {
    fraud_prover: fraud_prover_key_hash,
  }
  let mint_redeemer: Data = fraud_proof.MintRedeemer {
    computation_thread_token_asset_name: asset_name,
    computation_thread_mint_redeemer_index: 0,
  }
  Transaction {
    ..placeholder,
    inputs: [ct_thread_input(asset_name)],
    outputs: [
      Output {
        address: fraud_proof_token_address(),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(fraud_proof_policy_id, asset_name, 1),
        datum: InlineDatum(fraud_proof_datum),
        reference_script: None,
      },
    ],
    redeemers: [Pair(Mint(fraud_proof_policy_id), mint_redeemer)],
  }
}
