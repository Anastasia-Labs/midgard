/**
 * Canonical redeemer layout helpers for commit transactions.
 * This module derives ledger-aligned indices for the state-queue and
 * active-operator redeemers used by the block-commit worker.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

type OutRefLike = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type StateQueueCommitRedeemer = {
  readonly CommitBlockHeader: {
    readonly latest_block_input_index: bigint;
    readonly new_block_output_index: bigint;
    readonly continued_latest_block_output_index: bigint;
    readonly operator: string;
    readonly scheduler_ref_input_index: bigint;
    readonly active_operators_input_index: bigint;
    readonly active_operators_redeemer_index: bigint;
  };
};

export type ActiveOperatorCommitRedeemer = {
  readonly UpdateBondHoldNewState: {
    readonly active_operator: string;
    readonly active_node_input_index: bigint;
    readonly active_node_output_index: bigint;
    readonly hub_oracle_ref_input_index: bigint;
    readonly state_queue_input_index: bigint;
    readonly state_queue_redeemer_index: bigint;
  };
};

export type StateQueueCommitLayout = {
  readonly schedulerRefInputIndex: bigint;
  readonly latestBlockInputIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueSpendRedeemerIndex: bigint;
};

export const DEFAULT_STATE_QUEUE_COMMIT_LAYOUT: StateQueueCommitLayout = {
  schedulerRefInputIndex: 0n,
  latestBlockInputIndex: 0n,
  newBlockOutputIndex: 0n,
  continuedLatestBlockOutputIndex: 1n,
  activeOperatorsInputIndex: 1n,
  activeOperatorsRedeemerIndex: 1n,
  activeOperatorOutputIndex: 2n,
  hubOracleRefInputIndex: 0n,
  stateQueueSpendRedeemerIndex: 0n,
} as const;

const compareOutRefs = (a: OutRefLike, b: OutRefLike): number => {
  const txHashComparison = a.txHash.localeCompare(b.txHash);
  if (txHashComparison !== 0) {
    return txHashComparison;
  }
  return a.outputIndex - b.outputIndex;
};

const outRefId = ({ txHash, outputIndex }: OutRefLike): string =>
  `${txHash}#${outputIndex}`;

export const deriveStateQueueCommitLayout = ({
  latestBlockInput,
  activeOperatorInput,
  schedulerRefInput,
  hubOracleRefInput,
  txReferenceInputs,
  txInputs,
}: {
  readonly latestBlockInput: OutRefLike;
  readonly activeOperatorInput: OutRefLike;
  readonly schedulerRefInput?: OutRefLike;
  readonly hubOracleRefInput?: OutRefLike;
  readonly txReferenceInputs?: readonly OutRefLike[];
  readonly txInputs: readonly OutRefLike[];
}): StateQueueCommitLayout => {
  const sortedInputs = [...txInputs].sort(compareOutRefs);
  const latestOutRef = outRefId(latestBlockInput);
  const latestBlockInputIndex = sortedInputs.findIndex(
    (input) => outRefId(input) === latestOutRef,
  );
  if (latestBlockInputIndex < 0) {
    throw new Error(
      `Latest state-queue input ${latestOutRef} missing from tx input set`,
    );
  }
  const activeOutRef = outRefId(activeOperatorInput);
  const activeOperatorsInputIndex = sortedInputs.findIndex(
    (input) => outRefId(input) === activeOutRef,
  );
  if (activeOperatorsInputIndex < 0) {
    throw new Error(
      `Active operator input ${activeOutRef} missing from tx input set`,
    );
  }
  const stateQueueSpendRedeemerIndex =
    compareOutRefs(latestBlockInput, activeOperatorInput) < 0 ? 0n : 1n;
  const activeOperatorsRedeemerIndex =
    compareOutRefs(activeOperatorInput, latestBlockInput) < 0 ? 0n : 1n;

  if (schedulerRefInput === undefined && hubOracleRefInput === undefined) {
    return {
      ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
      latestBlockInputIndex: BigInt(latestBlockInputIndex),
      activeOperatorsInputIndex: BigInt(activeOperatorsInputIndex),
      stateQueueSpendRedeemerIndex,
      activeOperatorsRedeemerIndex,
    };
  }

  if (schedulerRefInput === undefined || hubOracleRefInput === undefined) {
    throw new Error(
      "State queue commit layout requires both scheduler and hub-oracle reference inputs when deriving non-default reference indices",
    );
  }

  const sortedReferenceInputs = [
    ...(txReferenceInputs ?? [schedulerRefInput, hubOracleRefInput]),
  ].sort(compareOutRefs);
  const schedulerRefOutRef = outRefId(schedulerRefInput);
  const hubOracleRefOutRef = outRefId(hubOracleRefInput);
  const schedulerRefInputIndex = sortedReferenceInputs.findIndex(
    (input) => outRefId(input) === schedulerRefOutRef,
  );
  if (schedulerRefInputIndex < 0) {
    throw new Error(
      `Scheduler reference input ${schedulerRefOutRef} missing from tx reference input set`,
    );
  }
  const hubOracleRefInputIndex = sortedReferenceInputs.findIndex(
    (input) => outRefId(input) === hubOracleRefOutRef,
  );
  if (hubOracleRefInputIndex < 0) {
    throw new Error(
      `Hub-oracle reference input ${hubOracleRefOutRef} missing from tx reference input set`,
    );
  }
  return {
    ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
    schedulerRefInputIndex: BigInt(schedulerRefInputIndex),
    latestBlockInputIndex: BigInt(latestBlockInputIndex),
    activeOperatorsInputIndex: BigInt(activeOperatorsInputIndex),
    hubOracleRefInputIndex: BigInt(hubOracleRefInputIndex),
    stateQueueSpendRedeemerIndex,
    activeOperatorsRedeemerIndex,
  };
};

export type ActiveOperatorSpendRedeemer = unknown;
export const ActiveOperatorSpendRedeemer =
  SDK.ActiveOperatorSpendRedeemer as unknown;

export const makeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): StateQueueCommitRedeemer => ({
  CommitBlockHeader: {
    latest_block_input_index: layout.latestBlockInputIndex,
    new_block_output_index: layout.newBlockOutputIndex,
    continued_latest_block_output_index: layout.continuedLatestBlockOutputIndex,
    operator: operatorKeyHash,
    scheduler_ref_input_index: layout.schedulerRefInputIndex,
    active_operators_input_index: layout.activeOperatorsInputIndex,
    active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
  },
});

export const makeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): ActiveOperatorCommitRedeemer => ({
  UpdateBondHoldNewState: {
    active_operator: operatorKeyHash,
    active_node_input_index: layout.activeOperatorsInputIndex,
    active_node_output_index: layout.activeOperatorOutputIndex,
    hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    state_queue_input_index: layout.latestBlockInputIndex,
    state_queue_redeemer_index: layout.stateQueueSpendRedeemerIndex,
  },
});

export const encodeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): string =>
  Data.to(
    makeStateQueueCommitRedeemer(operatorKeyHash, layout) as never,
    SDK.StateQueueRedeemer as never,
  );

export const encodeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): string =>
  Data.to(
    makeActiveOperatorCommitRedeemer(operatorKeyHash, layout) as never,
    SDK.ActiveOperatorSpendRedeemer as never,
  );
