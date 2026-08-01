//// Midgard's ledger state types are described in the specification (§ 1).
//// Midgard uses two distinct data representations for ledger state:
////
////   - The *full* representation contains complete data structures.
////     It is stored offchain and only small parts of it may be occasionally
////     submitted onchain during fraud-proof verification.
////     The full representation sometimes includes fields that are compact
////     representations of its other variable-sized fields.
////   - The *compact* representation is a fixed-sized data structure derived from
////     the full representation by abbreviating all variable-sized fields.
////     Other than in fraud-proof scripts, all onchain scripts must use the
////     compact representation.
////
//// These types must correspond to the CBOR types defined in Midgard's
//// Codec Standard (/codec.cddl).

use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{
  ScriptHash, Signature, VerificationKey, VerificationKeyHash, blake2b_224,
}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential}
use cardano/assets.{Lovelace, PolicyId}
use cardano/transaction.{
  Datum as CardanoDatum, OutputReference, Redeemer, ScriptPurpose, ValidityRange,
}
use env
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/common/types.{H28, H32, MerkleRoot, PosixTime, ValuePairs}
use midgard/common/utils
use midgard/validation_trace_v1

// # Ledger state types

// ## Block

/// Midgard block.
/// Full representation.
pub type Block {
  header_hash: HeaderHash,
  header: HeaderV1,
  block_body: BlockBody,
}

/// Midgard block header hash.
/// Compact representation.
pub type HeaderHash =
  H28<HeaderV1>

pub type MidgardLedger =
  Dict<OutputReference, MidgardOutput>

pub type MidgardLedgerRoot =
  MerkleRoot<OutputReference, MidgardOutput>

/// Canonical proof-complete Midgard V1 block header.
/// Compact representation.
pub type HeaderV1 {
  prev_utxos_root: MidgardLedgerRoot,
  utxos_root: MidgardLedgerRoot,
  withdrawals_root: WithdrawalsRoot,
  forced_transactions_root: ForcedTransactionsRoot,
  transactions_root: MidgardTxsRoot,
  deposits_root: DepositsRoot,
  transition_trace_root: TransitionTraceRoot,
  event_to_step_root: EventToStepRoot,
  validation_traces_root: ValidationTracesRoot,
  withdrawal_count: Int,
  forced_transaction_count: Int,
  l2_transaction_count: Int,
  deposit_count: Int,
  total_event_count: Int,
  transition_step_count: Int,
  validation_trace_count: Int,
  start_time: PosixTime,
  end_time: PosixTime,
  block_slot: Int,
  expected_network_id: Int,
  min_fee_a: Int,
  min_fee_b: Int,
  prev_header_hash: HeaderHash,
  operator_vkey: VerificationKeyHash,
  protocol_version: Int,
}

pub type HeaderTransitionCommitmentsV1 {
  forced_transactions_root: ForcedTransactionsRoot,
  transition_trace_root: TransitionTraceRoot,
  event_to_step_root: EventToStepRoot,
  validation_traces_root: ValidationTracesRoot,
  withdrawal_count: Int,
  forced_transaction_count: Int,
  l2_transaction_count: Int,
  deposit_count: Int,
  total_event_count: Int,
  transition_step_count: Int,
  validation_trace_count: Int,
}

/// Midgard block body.
/// Full representation.
/// This type is not used in onchain code.
pub type BlockBody {
  utxos: MidgardLedger,
  withdrawals: Withdrawals,
  forced_transactions: ForcedTransactions,
  transactions: MidgardTxs,
  deposits: Deposits,
  transition_trace: TransitionTrace,
  event_to_step: EventToStep,
}

// ## Confirmed state

/// Midgard confirmed state.
/// Compact representation.
pub type ConfirmedState {
  header_hash: HeaderHash,
  prev_header_hash: HeaderHash,
  utxo_root: MidgardLedgerRoot,
  start_time: PosixTime,
  end_time: PosixTime,
  protocol_version: Int,
}

pub const genesis_header_hash: HeaderHash =
  #"00000000000000000000000000000000000000000000000000000000"

pub const genesis_utxo_root: MidgardLedgerRoot = env.empty_merkle_tree_root

pub const genesis_protocol_version: Int = 0

pub const protocol_version_v1: Int = 1

/// Construct the sole authenticated state-queue genesis sentinel. Protocol
/// version zero identifies only this exact all-genesis state; it is not an
/// ordinary block-header version.
pub fn genesis_confirmed_state_v1(genesis_time: Int) -> Option<ConfirmedState> {
  if genesis_time < 0 {
    None
  } else {
    Some(
      ConfirmedState {
        header_hash: genesis_header_hash,
        prev_header_hash: genesis_header_hash,
        utxo_root: genesis_utxo_root,
        start_time: genesis_time,
        end_time: genesis_time,
        protocol_version: genesis_protocol_version,
      },
    )
  }
}

/// Authenticate either the exact genesis sentinel or an ordinary confirmed V1
/// state and return the protocol version required for its next ordinary
/// header. The sentinel's zero cannot leak into any committed header.
pub fn confirmed_state_next_header_protocol_version_v1(
  confirmed_state: ConfirmedState,
) -> Option<Int> {
  let
      ConfirmedState {
        header_hash,
        prev_header_hash,
        utxo_root,
        start_time,
        end_time,
        protocol_version,
      }
    = confirmed_state
  let is_genesis = and {
    protocol_version == genesis_protocol_version,
    header_hash == genesis_header_hash,
    prev_header_hash == genesis_header_hash,
    utxo_root == genesis_utxo_root,
    start_time >= 0,
    start_time == end_time,
  }
  if is_genesis {
    Some(protocol_version_v1)
  } else if and {
    protocol_version == protocol_version_v1,
    header_hash != genesis_header_hash,
    start_time >= 0,
    start_time <= end_time,
  } {
    Some(protocol_version_v1)
  } else {
    None
  }
}

/// Seven-day maturity for the canonical V1 deployment.
pub const block_maturity_duration_v1: Int = 7 * 24 * 60 * 60 * 1000

pub const transition_step_schema_version_v1: Int = 1

// Immutable canonical V1 consensus bounds. These are deployment identity, not
// operator-configurable tuning knobs.
pub const max_withdrawal_count_v1: Int = 10_000

pub const max_forced_transaction_count_v1: Int = 10_000

pub const max_l2_transaction_count_v1: Int = 10_000

pub const max_deposit_count_v1: Int = 10_000

pub const max_total_event_count_v1: Int = 40_000

pub const max_transition_step_count_v1: Int = 40_000

pub const max_validation_trace_count_v1: Int = 20_000

fn root_matches_count_v1(root: ByteArray, count: Int) -> Bool {
  if count == 0 {
    root == env.empty_merkle_tree_root
  } else {
    root != env.empty_merkle_tree_root
  }
}

pub fn header_transition_commitments_v1(
  header: HeaderV1,
) -> HeaderTransitionCommitmentsV1 {
  HeaderTransitionCommitmentsV1 {
    forced_transactions_root: header.forced_transactions_root,
    transition_trace_root: header.transition_trace_root,
    event_to_step_root: header.event_to_step_root,
    validation_traces_root: header.validation_traces_root,
    withdrawal_count: header.withdrawal_count,
    forced_transaction_count: header.forced_transaction_count,
    l2_transaction_count: header.l2_transaction_count,
    deposit_count: header.deposit_count,
    total_event_count: header.total_event_count,
    transition_step_count: header.transition_step_count,
    validation_trace_count: header.validation_trace_count,
  }
}

pub fn header_transition_commitments_v1_are_valid(
  commitments: HeaderTransitionCommitmentsV1,
) -> Bool {
  and {
    commitments.withdrawal_count >= 0,
    commitments.withdrawal_count <= max_withdrawal_count_v1,
    commitments.forced_transaction_count >= 0,
    commitments.forced_transaction_count <= max_forced_transaction_count_v1,
    commitments.l2_transaction_count >= 0,
    commitments.l2_transaction_count <= max_l2_transaction_count_v1,
    commitments.deposit_count >= 0,
    commitments.deposit_count <= max_deposit_count_v1,
    commitments.total_event_count >= 0,
    commitments.total_event_count <= max_total_event_count_v1,
    commitments.transition_step_count >= 0,
    commitments.transition_step_count <= max_transition_step_count_v1,
    commitments.validation_trace_count >= 0,
    commitments.validation_trace_count <= max_validation_trace_count_v1,
    commitments.total_event_count == commitments.withdrawal_count + commitments.forced_transaction_count + commitments.l2_transaction_count + commitments.deposit_count,
    commitments.transition_step_count == commitments.total_event_count,
    commitments.validation_trace_count == commitments.forced_transaction_count + commitments.l2_transaction_count,
    root_matches_count_v1(
      commitments.forced_transactions_root,
      commitments.forced_transaction_count,
    ),
    root_matches_count_v1(
      commitments.transition_trace_root,
      commitments.total_event_count,
    ),
    root_matches_count_v1(
      commitments.event_to_step_root,
      commitments.total_event_count,
    ),
    root_matches_count_v1(
      commitments.validation_traces_root,
      commitments.validation_trace_count,
    ),
  }
}

pub fn header_v1_is_valid(header: HeaderV1) -> Bool {
  and {
    header.protocol_version == protocol_version_v1,
    root_matches_count_v1(header.withdrawals_root, header.withdrawal_count),
    root_matches_count_v1(header.transactions_root, header.l2_transaction_count),
    root_matches_count_v1(header.deposits_root, header.deposit_count),
    header_transition_commitments_v1_are_valid(
      header_transition_commitments_v1(header),
    ),
  }
}

// ## User events

pub fn unsafe_event_to_id_data(event_datum_data: Data) -> Data {
  expect [id_data, ..] = builtin.unconstr_fields(event_datum_data)
  id_data
}

pub fn unsafe_event_to_key_value_pair(
  event_datum_data: Data,
) -> Pair<Data, Data> {
  expect [id_data, info_data, ..] = builtin.unconstr_fields(event_datum_data)
  Pair(id_data, info_data)
}

// ### Deposit event

pub type Deposits =
  Dict<DepositId, DepositInfo>

pub type DepositsRoot =
  MerkleRoot<DepositId, DepositInfo>

pub type DepositId =
  OutputReference

pub type DepositInfo {
  l2_address: Address,
  l2_network_id: Int,
  l2_datum: Option<Data>,
}

pub type DepositEvent {
  id: DepositId,
  info: DepositInfo,
}

// ### Withdrawal event

pub type Withdrawals =
  Dict<WithdrawalId, WithdrawalInfo>

pub type WithdrawalsRoot =
  MerkleRoot<WithdrawalId, WithdrawalInfo>

pub type WithdrawalId =
  OutputReference

pub type WithdrawalBody {
  l2_outref: OutputReference,
  l2_owner: VerificationKeyHash,
  l2_value: MidgardValue,
  l1_address: Address,
  l1_datum: CardanoDatum,
}

pub type WithdrawalSignature =
  Pair<VerificationKey, Signature>

pub type WithdrawalValidity {
  WithdrawalIsValid
  NonExistentWithdrawalUtxo
  SpentWithdrawalUtxo { l2_tx_id: MidgardTxId }
  IncorrectWithdrawalOwner
  IncorrectWithdrawalValue
  IncorrectWithdrawalSignature
  TooManyTokensInWithdrawal
  UnpayableWithdrawalValue
}

pub type WithdrawalInfo {
  body: WithdrawalBody,
  signature: WithdrawalSignature,
  validity: WithdrawalValidity,
}

pub type WithdrawalEvent {
  id: WithdrawalId,
  info: WithdrawalInfo,
}

// ### Transaction order event

pub type TxOrderId =
  OutputReference

pub type NativeTxProofSourceV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
}

pub type TxFieldPreimageV1 {
  field_receipt_policy_id: PolicyId,
  tx_order_policy_id: PolicyId,
  tx_order_id: TxOrderId,
  transaction_commitment: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  proof: bounded_item_v1.ChunkProofV1,
}

pub type TxFieldReceiptV1 {
  field_receipt_policy_id: PolicyId,
  tx_order_policy_id: PolicyId,
  tx_order_id: TxOrderId,
  transaction_commitment: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_index: Int,
  field_reference: OutputReference,
  predecessor_receipt_reference: Option<OutputReference>,
  field_encoded_size: Int,
}

/// Permissionless, immutable L1 publication of one content-addressed CEK
/// program node. The root authenticates `(kind, preimage)` under the
/// canonical V1 domain; the enclosing validator deliberately has no spend path so
/// forced-transaction material remains available for later classification and
/// challenge.
pub type CekProgramMaterialDatumV1 {
  kind: Int,
  root: ByteArray,
  preimage: ByteArray,
}

pub type TxOrderPayloadV1 {
  tx_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
  terminal_receipt_reference: Option<OutputReference>,
}

pub type TxOrderEventV1 {
  id: TxOrderId,
  tx: TxOrderPayloadV1,
}

pub type ForcedTransactions =
  Dict<TxOrderId, ForcedInclusionTxV1>

pub type ForcedTransactionsRoot =
  MerkleRoot<TxOrderId, ForcedInclusionTxV1>

// ## Transaction

/// > [!NOTE]
/// > This is the hash of the compact transaction body. The transaction root
/// > maps this id to MidgardTxCompact, which carries the compact body inline.
pub type MidgardTxId =
  H32<MidgardTxBodyCompact>

pub type MidgardTxWitsHash =
  H32<MidgardTxWitnessSetCompact>

/// Midgard transaction.
pub type MidgardTx {
  body: MidgardTxBody,
  wits: MidgardTxWitnessSet,
  validity: MidgardTxValidity,
}

pub type MidgardTxs =
  Dict<MidgardTxId, MidgardTxCompact>

pub type MidgardTxsRoot =
  MerkleRoot<MidgardTxId, MidgardTxCompact>

/// Compact representation.
pub type MidgardTxCompact {
  body: MidgardTxBodyCompact,
  wits: MidgardTxWitsHash,
  validity: MidgardTxValidity,
}

pub type MidgardTxCompactWithoutValidity {
  body: MidgardTxBodyCompact,
  wits: MidgardTxWitsHash,
}

// TODO
pub type MidgardTxValidity {
  TxIsValid
  NonExistentInputUtxo
  InvalidSignature
  FailedScript
  FeeTooLow
  UnbalancedTx
}

pub type L2TransactionSourceV1 {
  tx_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
}

pub type ForcedInclusionTxV1 {
  tx_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
  operator_validity: MidgardTxValidity,
}

pub type TransitionPhase {
  Withdrawal
  ForcedTransaction
  L2Transaction
  Deposit
}

pub type EventKey {
  WithdrawalEventKey { withdrawal_id: WithdrawalId }
  ForcedTransactionEventKey { tx_order_id: TxOrderId }
  L2TransactionEventKey { tx_id: MidgardTxId }
  DepositEventKey { deposit_id: DepositId }
}

pub type EventToStepValue {
  step_index: Int,
  phase: TransitionPhase,
}

pub type EventToStep =
  Dict<EventKey, EventToStepValue>

pub type EventToStepRoot =
  MerkleRoot<EventKey, EventToStepValue>

pub type TransitionStep {
  schema_version: Int,
  step_index: Int,
  event_key: EventKey,
  phase: TransitionPhase,
  pre_utxos_root: MidgardLedgerRoot,
  post_utxos_root: MidgardLedgerRoot,
}

pub type TransitionStepV1 =
  TransitionStep

pub fn transition_step_v1_is_valid(step: TransitionStepV1) -> Bool {
  step.schema_version == transition_step_schema_version_v1
}

pub type TransitionTrace =
  Dict<Int, TransitionStep>

pub type TransitionTraceRoot =
  MerkleRoot<Int, TransitionStep>

pub type ValidationTraces =
  Dict<EventKey, validation_trace_v1.ValidationTraceDescriptorV1>

pub type ValidationTracesRoot =
  MerkleRoot<EventKey, validation_trace_v1.ValidationTraceDescriptorV1>

// ### Transaction body

/// TODO
pub type MidgardAddress =
  Credential

/// TODO
pub type MidgardValue =
  ValuePairs

pub type MidgardValueHash =
  H32<MidgardValue>

/// TODO
pub type MidgardOutput {
  address: MidgardAddress,
  value: MidgardValue,
  datum: Option<Data>,
  reference_script: Option<ScriptHash>,
}

pub type MidgardInputs =
  List<OutputReference>

pub type MidgardInputsHash =
  H32<MidgardInputs>

pub type MidgardOutputs =
  List<MidgardOutput>

pub type MidgardOutputsHash =
  H32<MidgardOutputs>

pub type Observers =
  List<ScriptHash>

pub type ObserversHash =
  H32<Observers>

pub type Signers =
  List<VerificationKeyHash>

pub type SignersHash =
  H32<Signers>

/// Midgard transaction body.
/// Full representation.
pub type MidgardTxBody {
  spend_inputs: MidgardInputs,
  reference_inputs: MidgardInputs,
  outputs: MidgardOutputs,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: Observers,
  required_signer_hashes: Signers,
  mint: MidgardValue,
  script_integrity_hash: H32<Data>,
  auxiliary_data_hash: H32<Data>,
  network_id: MidgardNetworkId,
}

/// Midgard transaction body.
/// Compact representation.
pub type MidgardTxBodyCompact {
  spend_inputs: MidgardInputsHash,
  reference_inputs: MidgardInputsHash,
  outputs: MidgardOutputsHash,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: ObserversHash,
  required_signer_hashes: SignersHash,
  mint: MidgardValueHash,
  script_integrity_hash: H32<Data>,
  auxiliary_data_hash: H32<Data>,
  network_id: MidgardNetworkId,
}

/// Midgard network ID
/// It indicates the network for which the transaction is intended.
/// Compact representation.
pub type MidgardNetworkId {
  Mainnet
  Testnet
}

// ### Transaction witnesses

pub type AddressWits =
  Pairs<VerificationKey, Signature>

pub type AddressWitsHash =
  H32<AddressWits>

pub type MidgardScriptWits =
  Pairs<ScriptHash, MidgardScript>

pub type MidgardScriptWitsHash =
  H32<MidgardScriptWits>

pub type Redeemers =
  Pairs<ScriptPurpose, Redeemer>

pub type RedeemersHash =
  H32<Redeemers>

/// Midgard transaction witnesses.
/// Full representation.
pub type MidgardTxWitnessSet {
  addr_tx_wits: AddressWits,
  script_tx_wits: MidgardScriptWits,
  redeemer_tx_wits: Redeemers,
}

/// Midgard transaction witnesses.
/// Compact representation.
pub type MidgardTxWitnessSetCompact {
  addr_tx_wits: AddressWitsHash,
  script_tx_wits: MidgardScriptWitsHash,
  redeemer_tx_wits: RedeemersHash,
}

pub fn midgard_tx_body_to_compact(body: MidgardTxBody) -> MidgardTxBodyCompact {
  let
      MidgardTxBody {
        spend_inputs,
        reference_inputs,
        outputs,
        fee,
        validity_interval,
        required_observers,
        required_signer_hashes,
        mint,
        script_integrity_hash,
        auxiliary_data_hash,
        network_id,
      }
    = body
  MidgardTxBodyCompact {
    spend_inputs: spend_inputs |> utils.serialise_and_hash_32,
    reference_inputs: reference_inputs |> utils.serialise_and_hash_32,
    outputs: outputs |> utils.serialise_and_hash_32,
    fee,
    validity_interval,
    required_observers: required_observers |> utils.serialise_and_hash_32,
    required_signer_hashes: required_signer_hashes
      |> utils.serialise_and_hash_32,
    mint: mint |> utils.serialise_and_hash_32,
    script_integrity_hash,
    auxiliary_data_hash,
    network_id,
  }
}

pub fn midgard_tx_wits_to_compact(
  wits: MidgardTxWitnessSet,
) -> MidgardTxWitnessSetCompact {
  let MidgardTxWitnessSet { addr_tx_wits, script_tx_wits, redeemer_tx_wits } = wits
  MidgardTxWitnessSetCompact {
    addr_tx_wits: addr_tx_wits |> utils.serialise_and_hash_32,
    script_tx_wits: script_tx_wits |> utils.serialise_and_hash_32,
    redeemer_tx_wits: redeemer_tx_wits |> utils.serialise_and_hash_32,
  }
}

pub fn midgard_tx_to_compact(tx: MidgardTx) -> MidgardTxCompact {
  let MidgardTx { body, wits, validity } = tx
  let body_compact = body |> midgard_tx_body_to_compact
  MidgardTxCompact {
    body: body_compact,
    wits: wits |> midgard_tx_wits_to_compact |> utils.serialise_and_hash_32,
    validity,
  }
}

pub fn midgard_full_tx_to_id(tx: MidgardTx) -> MidgardTxId {
  tx.body |> midgard_tx_body_to_compact |> utils.serialise_and_hash_32
}

/// Midgard script -- either timelock (native) or Plutus (V3).
/// Full representation.
pub type MidgardScript {
  Timelock { script: ByteArray }
  Plutus { script: ByteArray }
}

/// TODO: This assumes the script stored under `Plutus` constructor of
///       `MidgardScript` is the single CBOR encoding of the Flat encoded
///       program bytes. Also uses a hardcoded Plutus version 3.
///
///       `Timelock` scripts are simply passed to the hash function.
pub fn hash_midgard_script(midgard_script: MidgardScript) -> ScriptHash {
  when midgard_script is {
    Timelock { script } ->
      builtin.integer_to_bytearray(True, 1, 0)
        |> bytearray.concat(script)
        |> blake2b_224
    Plutus { script } ->
      builtin.integer_to_bytearray(True, 1, env.plutus_version)
        |> bytearray.concat(script)
        |> blake2b_224
  }
}
