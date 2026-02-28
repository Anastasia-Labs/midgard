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
  txInputs,
}: {
  readonly latestBlockInput: OutRefLike;
  readonly activeOperatorInput: OutRefLike;
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
  return {
    ...DEFAULT_STATE_QUEUE_COMMIT_LAYOUT,
    activeNodeInputIndex: BigInt(activeNodeInputIndex),
    activeOperatorsRedeemerIndex,
  };
};

const layoutKey = (layout: StateQueueCommitLayout): string =>
  [
    layout.schedulerRefInputIndex,
    layout.activeNodeInputIndex,
    layout.headerNodeOutputIndex,
    layout.previousHeaderNodeOutputIndex,
    layout.activeOperatorsRedeemerIndex,
    layout.activeNodeOutputIndex,
    layout.hubOracleRefInputIndex,
    layout.stateQueueRedeemerIndex,
  ].join("|");

const prioritizedIndexes = (
  base: bigint,
  values: readonly bigint[],
): readonly bigint[] => {
  const seen = new Set<bigint>();
  const ordered: bigint[] = [];
  const push = (value: bigint) => {
    if (seen.has(value)) {
      return;
    }
    seen.add(value);
    ordered.push(value);
  };
  push(base);
  for (const value of values) {
    push(value);
  }
  return ordered;
};

export const enumerateCommitLayoutCandidates = (
  base: StateQueueCommitLayout,
): readonly StateQueueCommitLayout[] => {
  const candidates: StateQueueCommitLayout[] = [base];
  const schedulerRefInputIndexes = prioritizedIndexes(
    base.schedulerRefInputIndex,
    [0n, 1n],
  );
  const hubOracleRefInputIndexes = prioritizedIndexes(
    base.hubOracleRefInputIndex,
    [0n, 1n],
  );
  const activeNodeInputs = prioritizedIndexes(base.activeNodeInputIndex, [0n, 1n, 2n]);
  const activeOperatorsRedeemerIndexes = prioritizedIndexes(
    base.activeOperatorsRedeemerIndex,
    [0n, 1n, 2n],
  );
  const stateQueueRedeemerIndexes = prioritizedIndexes(
    base.stateQueueRedeemerIndex,
    [0n, 1n, 2n],
  );
  const headerNodeOutputIndexes = prioritizedIndexes(base.headerNodeOutputIndex, [
    0n,
    1n,
    2n,
    3n,
  ]);
  const previousHeaderNodeOutputIndexes = prioritizedIndexes(
    base.previousHeaderNodeOutputIndex,
    [0n, 1n, 2n, 3n],
  );
  const activeNodeOutputIndexes = prioritizedIndexes(base.activeNodeOutputIndex, [
    0n,
    1n,
    2n,
    3n,
  ]);

  for (const activeNodeInputIndex of activeNodeInputs) {
    for (const activeOperatorsRedeemerIndex of activeOperatorsRedeemerIndexes) {
      for (const stateQueueRedeemerIndex of stateQueueRedeemerIndexes) {
        for (const headerNodeOutputIndex of headerNodeOutputIndexes) {
          for (const previousHeaderNodeOutputIndex of previousHeaderNodeOutputIndexes) {
            if (headerNodeOutputIndex === previousHeaderNodeOutputIndex) {
              continue;
            }
            for (const activeNodeOutputIndex of activeNodeOutputIndexes) {
              for (const schedulerRefInputIndex of schedulerRefInputIndexes) {
                for (const hubOracleRefInputIndex of hubOracleRefInputIndexes) {
                  if (schedulerRefInputIndex === hubOracleRefInputIndex) {
                    continue;
                  }
                  candidates.push({
                    ...base,
                    schedulerRefInputIndex,
                    hubOracleRefInputIndex,
                    activeNodeInputIndex,
                    activeOperatorsRedeemerIndex,
                    stateQueueRedeemerIndex,
                    headerNodeOutputIndex,
                    previousHeaderNodeOutputIndex,
                    activeNodeOutputIndex,
                  });
                }
              }
            }
          }
        }
      }
    }
  }

  const seen = new Set<string>();
  const unique: StateQueueCommitLayout[] = [];
  for (const candidate of candidates) {
    const key = layoutKey(candidate);
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    unique.push(candidate);
  }
  return unique;
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
