import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

type OutRefLike = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type StateQueueCommitLayout = {
  readonly schedulerRefInputIndex: bigint;
  readonly activeNodeInputIndex: bigint;
  readonly headerNodeOutputIndex: bigint;
  readonly previousHeaderNodeOutputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly activeNodeOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueRedeemerIndex: bigint;
};

export const DEFAULT_STATE_QUEUE_COMMIT_LAYOUT: StateQueueCommitLayout = {
  schedulerRefInputIndex: 0n,
  activeNodeInputIndex: 1n,
  headerNodeOutputIndex: 0n,
  previousHeaderNodeOutputIndex: 1n,
  activeOperatorsRedeemerIndex: 1n,
  activeNodeOutputIndex: 2n,
  hubOracleRefInputIndex: 0n,
  stateQueueRedeemerIndex: 2n,
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
  txInputs,
}: {
  readonly latestBlockInput: OutRefLike;
  readonly activeOperatorInput: OutRefLike;
  readonly schedulerRefInput?: OutRefLike;
  readonly hubOracleRefInput?: OutRefLike;
  readonly txInputs: readonly OutRefLike[];
}): StateQueueCommitLayout => {
  const sortedInputs = [...txInputs].sort(compareOutRefs);
  const activeOutRef = outRefId(activeOperatorInput);
  const activeNodeInputIndex = sortedInputs.findIndex(
    (input) => outRefId(input) === activeOutRef,
  );
  if (activeNodeInputIndex < 0) {
    throw new Error(
      `Active operator input ${activeOutRef} missing from tx input set`,
    );
  }
  const activeOperatorsRedeemerIndex =
    compareOutRefs(activeOperatorInput, latestBlockInput) < 0 ? 0n : 1n;

  if (schedulerRefInput === undefined && hubOracleRefInput === undefined) {
    return {
      ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
      activeNodeInputIndex: BigInt(activeNodeInputIndex),
      activeOperatorsRedeemerIndex,
    };
  }

  if (schedulerRefInput === undefined || hubOracleRefInput === undefined) {
    throw new Error(
      "State queue commit layout requires both scheduler and hub-oracle reference inputs when deriving non-default reference indices",
    );
  }

  const sortedReferenceInputs = [schedulerRefInput, hubOracleRefInput].sort(
    compareOutRefs,
  );
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
    activeNodeInputIndex: BigInt(activeNodeInputIndex),
    hubOracleRefInputIndex: BigInt(hubOracleRefInputIndex),
    activeOperatorsRedeemerIndex,
  };
};

export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateBondHoldNewState: Data.Object({
      active_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      state_queue_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    UpdateBondHoldNewSettlement: Data.Object({
      active_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      settlement_input_index: Data.Integer(),
      settlement_redeemer_index: Data.Integer(),
      new_bond_unlock_time: Data.Integer(),
    }),
  }),
]);

export type ActiveOperatorSpendRedeemer = Data.Static<
  typeof ActiveOperatorSpendRedeemerSchema
>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

export const makeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): SDK.StateQueueRedeemer => ({
  CommitBlockHeader: {
    operator: operatorKeyHash,
    scheduler_ref_input_index: layout.schedulerRefInputIndex,
    active_node_input_index: layout.activeNodeInputIndex,
    header_node_output_index: layout.headerNodeOutputIndex,
    previous_header_node_output_index: layout.previousHeaderNodeOutputIndex,
    active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
  },
});

export const makeActiveOperatorCommitRedeemer = (
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): ActiveOperatorSpendRedeemer => ({
  UpdateBondHoldNewState: {
    active_node_output_index: layout.activeNodeOutputIndex,
    hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    state_queue_redeemer_index: layout.stateQueueRedeemerIndex,
  },
});

export const encodeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): string =>
  Data.to(
    makeStateQueueCommitRedeemer(operatorKeyHash, layout),
    SDK.StateQueueRedeemer,
  );

export const encodeActiveOperatorCommitRedeemer = (
  layout: StateQueueCommitLayout = DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
): string =>
  Data.to(makeActiveOperatorCommitRedeemer(layout), ActiveOperatorSpendRedeemer);
