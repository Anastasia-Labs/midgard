import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type BuildTxWithRedeemer,
  Data,
  fromText,
  LucidEvolution,
  Script,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";

import { ActiveOperatorSpendRedeemer } from "./active-operators.js";
import { scriptRewardAddress } from "./cardano-addresses.js";
import { MerkleRootSchema, OutputReferenceSchema } from "./common.js";
import { HeaderHashSchema } from "./ledger-state.js";
import { LinkedListNodeView } from "./linked-list.js";

/** A terminal history proof must protect its pending header from merge. */
export const CompletedFraudWitnessSchema = Data.Enum([
  Data.Object({
    RecordedOutput: Data.Object({ output_index: Data.Integer() }),
  }),
  Data.Object({
    PreviouslyRecorded: Data.Object({ reference_input_index: Data.Integer() }),
  }),
]);

export type CompletedFraudWitness = Data.Static<
  typeof CompletedFraudWitnessSchema
>;

export const CompletedFraudWitness = asDataType<CompletedFraudWitness>(
  CompletedFraudWitnessSchema,
);

export const STATE_QUEUE_ROOT_ASSET_NAME = fromText("MIDGARD_CONFIRMED_STATE");

/**
 * The state-queue node lovelace floor, the twin of Aiken's
 * `state_queue_node_min_lovelace_v1`. The commit arm refuses a new block node
 * holding less, and no later arm lets a queued node's lovelace fall. The
 * floor covers the ledger minimum of the largest node the commit arm admits,
 * so a node can always take the `Challenged` status at an availability Open,
 * which carries the node value exactly. Every commit builder pays it.
 */
export const STATE_QUEUE_NODE_MIN_LOVELACE = 5_000_000n;

export type ActiveOperatorSpendTxRedeemer =
  | "ListStateTransition"
  | BuildTxWithRedeemer;

export const encodeActiveOperatorSpendRedeemer = (
  redeemer: ActiveOperatorSpendTxRedeemer,
): string | BuildTxWithRedeemer =>
  typeof redeemer === "function"
    ? redeemer
    : Data.to(redeemer as never, ActiveOperatorSpendRedeemer as never);

/**
 * Mirrors `midgard/state_queue.SlashingApproach`.
 *
 * The two bond-consuming constructors carry
 * `m_fraud_prover_reward_output_index`, the output that pays the fraud prover
 * exactly `env.fraud_prover_reward` (2026-08-11 owner ruling 7, D3). It is
 * `null` exactly when that compiled reward is zero — today's placeholder
 * economics, which F04 §2.5 assigns to Q53. `OperatorAlreadySlashed` consumes
 * no bond and so cannot name a reward output at all: that is the type-level
 * half of the D4 exclusivity ruling.
 */
export const SlashingApproachSchema = Data.Enum([
  Data.Object({
    SlashActiveOperator: Data.Object({
      active_operators_redeemer_index: Data.Integer(),
      m_fraud_prover_reward_output_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    SlashRetiredOperator: Data.Object({
      retired_operators_redeemer_index: Data.Integer(),
      m_fraud_prover_reward_output_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    OperatorAlreadySlashed: Data.Object({
      active_operators_element_ref_input_index: Data.Integer(),
      retired_operators_element_ref_input_index: Data.Integer(),
    }),
  }),
]);

export type SlashingApproach = Data.Static<typeof SlashingApproachSchema>;

export const SlashingApproach = asDataType<SlashingApproach>(
  SlashingApproachSchema,
);

export const BlockRemovalApproachSchema = Data.Enum([
  Data.Object({
    RemoveLastFraudulentBlock: Data.Object({
      anchor_element_input_outref: OutputReferenceSchema,
      anchor_element_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlocksLink: Data.Object({
      fraudulent_node_input_outref: OutputReferenceSchema,
      fraudulent_node_output_index: Data.Integer(),
    }),
  }),
]);

export type BlockRemovalApproach = Data.Static<
  typeof BlockRemovalApproachSchema
>;

export const BlockRemovalApproach = asDataType<BlockRemovalApproach>(
  BlockRemovalApproachSchema,
);

export const AttestationTimeoutRemovalApproachSchema = Data.Enum([
  Data.Object({
    PruneTimedOutBlockDescendant: Data.Object({
      confirmed_state_ref_input_index: Data.Integer(),
      timed_out_node_input_outref: OutputReferenceSchema,
      timed_out_node_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveTimedOutHead: Data.Object({
      confirmed_state_input_outref: OutputReferenceSchema,
      confirmed_state_output_index: Data.Integer(),
    }),
  }),
]);

export type AttestationTimeoutRemovalApproach = Data.Static<
  typeof AttestationTimeoutRemovalApproachSchema
>;

export const AttestationTimeoutRemovalApproach =
  asDataType<AttestationTimeoutRemovalApproach>(
    AttestationTimeoutRemovalApproachSchema,
  );

export const UnattestedTimeoutRemovalApproachSchema = Data.Enum([
  Data.Object({
    PruneUnattestedBlockDescendant: Data.Object({
      predecessor_ref_input_index: Data.Integer(),
      timed_out_node_input_outref: OutputReferenceSchema,
      timed_out_node_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveLastUnattestedBlock: Data.Object({
      predecessor_input_outref: OutputReferenceSchema,
      predecessor_output_index: Data.Integer(),
    }),
  }),
]);

export type UnattestedTimeoutRemovalApproach = Data.Static<
  typeof UnattestedTimeoutRemovalApproachSchema
>;

export const UnattestedTimeoutRemovalApproach =
  asDataType<UnattestedTimeoutRemovalApproach>(
    UnattestedTimeoutRemovalApproachSchema,
  );

export const StateQueueRedeemerSchema = Data.Enum([
  Data.Object({
    InitV1: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Deinit"),
  Data.Object({
    CommitBlockHeader: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      new_block_output_index: Data.Integer(),
      continued_latest_block_output_index: Data.Integer(),
      operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      scheduler_ref_input_index: Data.Integer(),
      active_operators_input_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
      m_confirmed_state_ref_input_index: Data.Nullable(Data.Integer()),
      m_head_state_queue_node_ref_input_index: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      fraudulent_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      fraudulent_blocks_header_hash: HeaderHashSchema,
      slashing_approach: SlashingApproachSchema,
      fraud_proof_ref_input_index: Data.Integer(),
      block_removal_approach: BlockRemovalApproachSchema,
    }),
  }),
  Data.Object({
    RemoveUnattestedBlockAfterTimeout: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      timed_out_header_hash: HeaderHashSchema,
      removal_approach: UnattestedTimeoutRemovalApproachSchema,
    }),
  }),
  Data.Object({
    RemoveUnavailableBlockAfterTimeout: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      unavailable_header_hash: HeaderHashSchema,
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
      removal_approach: AttestationTimeoutRemovalApproachSchema,
    }),
  }),
  Data.Object({
    MergeToConfirmedStateV1: Data.Object({
      yield_to_ref_input_index: Data.Integer(),
      header_node_key: Data.Bytes(),
      confirmed_state_input_outref: OutputReferenceSchema,
      confirmed_state_output_index: Data.Integer(),
      m_settlement_redeemer_index: Data.Nullable(Data.Integer()),
      merged_block_withdrawals_root: MerkleRootSchema,
      merged_block_forced_transactions_root: MerkleRootSchema,
      merged_block_transactions_root: MerkleRootSchema,
      merged_block_deposits_root: MerkleRootSchema,
      merged_block_transition_trace_root: MerkleRootSchema,
      merged_block_event_to_step_root: MerkleRootSchema,
      merged_block_validation_traces_root: MerkleRootSchema,
      merged_block_withdrawal_count: Data.Integer(),
      merged_block_forced_transaction_count: Data.Integer(),
      merged_block_l2_transaction_count: Data.Integer(),
      merged_block_deposit_count: Data.Integer(),
      merged_block_total_event_count: Data.Integer(),
      merged_block_transition_step_count: Data.Integer(),
      merged_block_validation_trace_count: Data.Integer(),
    }),
  }),
]);

export type StateQueueRedeemer = Data.Static<typeof StateQueueRedeemerSchema>;

export const StateQueueRedeemer = asDataType<StateQueueRedeemer>(
  StateQueueRedeemerSchema,
);

export const StateQueueYieldRedeemerSchema = Data.Enum([
  Data.Literal("YieldStateQueueV1"),
]);

export type StateQueueYieldRedeemer = Data.Static<
  typeof StateQueueYieldRedeemerSchema
>;

export const StateQueueYieldRedeemer = asDataType<StateQueueYieldRedeemer>(
  StateQueueYieldRedeemerSchema,
);

/** Encode Aiken's sole fieldless `YieldStateQueueV1` constructor. */
export const encodeStateQueueYieldRedeemer = (): string => Data.void();

export type StateQueueYieldWitness = {
  /** Authenticated deployment output carrying the arm-specific reference script. */
  readonly referenceInput: UTxO;
  /** The same rewarding script, used to derive its network reward address. */
  readonly script: Script;
};

export const applyStateQueueZeroYield = (
  lucid: LucidEvolution,
  tx: TxBuilder,
  witness: StateQueueYieldWitness,
): TxBuilder => {
  const network = lucid.config().network;
  if (network === undefined) {
    throw new Error(
      "Cannot build a state-queue yield without a configured Lucid network",
    );
  }
  return tx.withdraw(scriptRewardAddress(network, witness.script), 0n, (() =>
    encodeStateQueueYieldRedeemer()) satisfies BuildTxWithRedeemer);
};

export const StateQueueSpendRedeemerSchema = Data.Enum([
  Data.Literal("LinkedListMutation"),
  Data.Object({
    AttachDaAttestation: Data.Object({
      state_queue_input_index: Data.Integer(),
      da_attestation_mint_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    AvailabilityStatusUpdate: Data.Object({
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      availability_mint_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RecordCompletedFraud: Data.Object({
      state_queue_input_index: Data.Integer(),
      state_queue_output_index: Data.Integer(),
      fraud_proof_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
]);

export type StateQueueSpendRedeemer = Data.Static<
  typeof StateQueueSpendRedeemerSchema
>;

export const StateQueueSpendRedeemer = asDataType<StateQueueSpendRedeemer>(
  StateQueueSpendRedeemerSchema,
);

export const STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER = Data.to(
  "LinkedListMutation" as never,
  StateQueueSpendRedeemer as never,
);

export type StateQueueUTxO = {
  utxo: UTxO;
  datum: LinkedListNodeView;
  assetName: string;
};
