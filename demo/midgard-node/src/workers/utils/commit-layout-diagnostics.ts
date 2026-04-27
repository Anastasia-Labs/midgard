/**
 * Deterministic diagnostics for commit-layout derivation and draft mismatches.
 * The commit worker uses this module to explain why a locally evaluated draft
 * diverged before submission without mixing diagnostics into orchestration.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data as LucidData,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
} from "@/cml-redeemers.js";
import {
  collectIndexedOutputs,
  collectSortedInputOutRefs,
  findOutRefIndex,
  outRefLabel,
} from "@/tx-context.js";
import {
  StateQueueCommitLayout,
} from "@/workers/utils/commit-redeemers.js";

const assetsEqual = (
  left: Readonly<Record<string, bigint>>,
  right: Readonly<Record<string, bigint>>,
): boolean => {
  const keys = new Set([...Object.keys(left), ...Object.keys(right)]);
  for (const key of keys) {
    if ((left[key] ?? 0n) !== (right[key] ?? 0n)) {
      return false;
    }
  }
  return true;
};

type CanonicalSchedulerDatum =
  | "NoActiveOperators"
  | {
      readonly ActiveOperator: {
        readonly operator: string;
        readonly start_time: bigint;
      };
    };

const linkedListAssetNameFromOutputAssets = (
  assets: Readonly<Record<string, bigint>>,
): string => {
  const nonAdaUnits = Object.keys(assets).filter((unit) => unit !== "lovelace");
  return nonAdaUnits.length === 1 ? nonAdaUnits[0]!.slice(56) : "";
};

const decodeLinkedListNodeViewFromOutput = (
  output: { readonly datum?: string | null; readonly assets: Record<string, bigint> },
): SDK.LinkedListNodeView | undefined => {
  if (output.datum == null) {
    return undefined;
  }
  const linkedListDatum = LucidData.from(output.datum, SDK.LinkedListDatum);
  return SDK.linkedListDatumToNodeView(
    linkedListDatum,
    linkedListAssetNameFromOutputAssets(output.assets),
  );
};

export const deriveCommitLayoutFromDraftTx = ({
  tx,
  latestBlockInput,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  stateQueueAddress,
  headerNodeUnit,
  headerNodeDatum,
  previousHeaderNodeDatum,
}: {
  readonly tx: CML.Transaction;
  readonly latestBlockInput: UTxO;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly stateQueueAddress: string;
  readonly headerNodeUnit: string;
  readonly headerNodeDatum: string;
  readonly previousHeaderNodeDatum: string;
}): StateQueueCommitLayout => {
  const txBody = tx.body();
  const inputList = collectSortedInputOutRefs(txBody.inputs());
  const referenceInputListRaw = txBody.reference_inputs();
  const indexedOutputs = collectIndexedOutputs(txBody.outputs());

  const latestBlockInputIndex = findOutRefIndex(
    inputList,
    latestBlockInput,
  );
  if (latestBlockInputIndex === undefined) {
    throw new Error(
      `Unable to find latest state-queue input ${outRefLabel(latestBlockInput)} in balanced draft tx body inputs`,
    );
  }

  const activeOperatorsInputIndex = findOutRefIndex(
    inputList,
    activeOperatorInput,
  );
  if (activeOperatorsInputIndex === undefined) {
    throw new Error(
      `Unable to find active-operator input ${outRefLabel(activeOperatorInput)} in balanced draft tx body inputs`,
    );
  }

  if (referenceInputListRaw === undefined) {
    throw new Error(
      "Balanced draft tx body did not include reference inputs for scheduler witness",
    );
  }
  const referenceInputList = collectSortedInputOutRefs(referenceInputListRaw);
  const schedulerRefInputIndex = findOutRefIndex(
    referenceInputList,
    schedulerRefInput,
  );
  if (schedulerRefInputIndex === undefined) {
    throw new Error(
      `Unable to find scheduler reference input ${outRefLabel(schedulerRefInput)} in balanced draft tx reference inputs`,
    );
  }
  const hubOracleRefInputIndex = findOutRefIndex(
    referenceInputList,
    hubOracleRefInput,
  );
  if (hubOracleRefInputIndex === undefined) {
    throw new Error(
      `Unable to find hub-oracle reference input ${outRefLabel(hubOracleRefInput)} in balanced draft tx reference inputs`,
    );
  }

  const headerNodeOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === headerNodeDatum &&
      (output.assets[headerNodeUnit] ?? 0n) === 1n,
  );
  if (headerNodeOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one header-node output at ${stateQueueAddress} with datum ${headerNodeDatum.slice(0, 24)}..., found ${headerNodeOutputCandidates.length}`,
    );
  }
  const newBlockOutputIndex = headerNodeOutputCandidates[0].index;

  const previousHeaderOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === stateQueueAddress &&
      output.datum === previousHeaderNodeDatum,
  );
  if (previousHeaderOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one previous-header output at ${stateQueueAddress} with datum ${previousHeaderNodeDatum.slice(0, 24)}..., found ${previousHeaderOutputCandidates.length}`,
    );
  }
  const continuedLatestBlockOutputIndex = previousHeaderOutputCandidates[0].index;

  const activeNodeOutputCandidates = indexedOutputs.filter(
    (output) =>
      output.address === activeOperatorInput.address &&
      assetsEqual(output.assets, activeOperatorInput.assets),
  );
  if (activeNodeOutputCandidates.length !== 1) {
    throw new Error(
      `Expected exactly one active-operator output at ${activeOperatorInput.address} with unchanged assets, found ${activeNodeOutputCandidates.length}`,
    );
  }
  const activeOperatorOutputIndex = activeNodeOutputCandidates[0].index;

  const redeemerPointers = getRedeemerPointersInContextOrder(tx);
  const txInfoRedeemerIndexes = getTxInfoRedeemerIndexes(redeemerPointers);
  if (redeemerPointers.length <= 0) {
    throw new Error("Balanced draft tx did not contain redeemers");
  }
  const activeOperatorSpendRedeemerContextIndex = redeemerPointers.findIndex(
    (pointer) =>
      pointer.tag === CML.RedeemerTag.Spend &&
      pointer.index === BigInt(activeOperatorsInputIndex),
  );
  if (activeOperatorSpendRedeemerContextIndex < 0) {
    throw new Error(
      `Unable to find active-operator spend redeemer for input index ${activeOperatorsInputIndex}`,
    );
  }
  const stateQueueSpendRedeemerContextIndex = redeemerPointers.findIndex(
    (pointer) =>
      pointer.tag === CML.RedeemerTag.Spend &&
      pointer.index === BigInt(latestBlockInputIndex),
  );
  if (stateQueueSpendRedeemerContextIndex < 0) {
    throw new Error(
      `Unable to find state-queue spend redeemer for input index ${latestBlockInputIndex}`,
    );
  }
  const activeOperatorsRedeemerIndex =
    txInfoRedeemerIndexes[activeOperatorSpendRedeemerContextIndex];
  const stateQueueSpendRedeemerIndex =
    txInfoRedeemerIndexes[stateQueueSpendRedeemerContextIndex];
  if (activeOperatorsRedeemerIndex === undefined) {
    throw new Error(
      `Unable to map active-operator spend redeemer context index ${activeOperatorSpendRedeemerContextIndex} to tx-info order`,
    );
  }
  if (stateQueueSpendRedeemerIndex === undefined) {
    throw new Error(
      `Unable to map state-queue spend redeemer context index ${stateQueueSpendRedeemerContextIndex} to tx-info order`,
    );
  }

  return {
    schedulerRefInputIndex: BigInt(schedulerRefInputIndex),
    latestBlockInputIndex: BigInt(latestBlockInputIndex),
    activeOperatorsInputIndex: BigInt(activeOperatorsInputIndex),
    newBlockOutputIndex: BigInt(newBlockOutputIndex),
    continuedLatestBlockOutputIndex: BigInt(continuedLatestBlockOutputIndex),
    activeOperatorsRedeemerIndex: BigInt(activeOperatorsRedeemerIndex),
    activeOperatorOutputIndex: BigInt(activeOperatorOutputIndex),
    hubOracleRefInputIndex: BigInt(hubOracleRefInputIndex),
    stateQueueSpendRedeemerIndex: BigInt(stateQueueSpendRedeemerIndex),
  };
};

const redeemerTagLabel = (tag: number): string => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "cert";
    case CML.RedeemerTag.Reward:
      return "reward";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      return `unknown(${tag})`;
  }
};

const describeUtxoAssets = (
  assets: Readonly<Record<string, bigint>>,
): string => {
  const nonAda = Object.entries(assets)
    .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
    .map(([unit, amount]) => `${unit}:${amount.toString()}`);
  return `lovelace=${(assets.lovelace ?? 0n).toString()},nonAda=${
    nonAda.length > 0 ? nonAda.join("|") : "none"
  }`;
};

const findTxInputAtIndex = (
  inputs: CML.TransactionInputList,
  index: bigint,
): string | undefined => {
  const i = Number(index);
  if (!Number.isSafeInteger(i) || i < 0 || i >= inputs.len()) {
    return undefined;
  }
  const input = inputs.get(i);
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

const findTxReferenceInputAtIndex = (
  referenceInputs: CML.TransactionInputList | undefined,
  index: bigint,
): string | undefined => {
  if (referenceInputs === undefined) {
    return undefined;
  }
  const i = Number(index);
  if (!Number.isSafeInteger(i) || i < 0 || i >= referenceInputs.len()) {
    return undefined;
  }
  const input = referenceInputs.get(i);
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

const listRequiredSigners = (tx: CML.Transaction): readonly string[] => {
  const signers = tx.body().required_signers();
  if (signers === undefined) {
    return [];
  }
  const hashes: string[] = [];
  for (let i = 0; i < signers.len(); i += 1) {
    hashes.push(signers.get(i).to_hex());
  }
  return hashes;
};

export const buildRealCommitDraftDiagnostics = ({
  tx,
  layout,
  operatorKeyHash,
  latestBlockInput,
  schedulerRefInput,
  hubOracleRefInput,
  activeOperatorInput,
  stateQueueAddress,
  headerNodeUnit,
  appendedNodeDatumCbor,
  previousHeaderNodeDatumCbor,
  schedulerPolicyId,
  hubOraclePolicyId,
  activeOperatorsPolicyId,
  txValidityUpperBoundSlot,
  txValidityUpperBoundUnixTime,
}: {
  readonly tx: CML.Transaction;
  readonly layout: StateQueueCommitLayout;
  readonly operatorKeyHash: string;
  readonly latestBlockInput: UTxO;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly stateQueueAddress: string;
  readonly headerNodeUnit: string;
  readonly appendedNodeDatumCbor: string;
  readonly previousHeaderNodeDatumCbor: string;
  readonly schedulerPolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly activeOperatorsPolicyId: string;
  readonly txValidityUpperBoundSlot: string | undefined;
  readonly txValidityUpperBoundUnixTime: string | undefined;
}): Record<string, unknown> => {
  const txBody = tx.body();
  const inputs = txBody.inputs();
  const referenceInputs = txBody.reference_inputs();
  const outputs = collectIndexedOutputs(txBody.outputs());
  const redeemerPointers = getRedeemerPointersInContextOrder(tx);
  const requiredSigners = listRequiredSigners(tx);

  const expectedActiveUnit = toUnit(
    activeOperatorsPolicyId,
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );
  const expectedSchedulerUnit = toUnit(
    schedulerPolicyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const expectedHubOracleUnit = toUnit(
    hubOraclePolicyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const headerOutputIndex = Number(layout.newBlockOutputIndex);
  const previousHeaderOutputIndex = Number(
    layout.continuedLatestBlockOutputIndex,
  );
  const activeRedeemerLayoutIndex = Number(layout.activeOperatorsRedeemerIndex);
  const stateQueueRedeemerLayoutIndex = Number(
    layout.stateQueueSpendRedeemerIndex,
  );
  const txInfoRedeemerIndexes = getTxInfoRedeemerIndexes(redeemerPointers);
  const findRedeemerPointerByTxInfoIndex = (
    txInfoIndex: number,
  ): (typeof redeemerPointers)[number] | undefined => {
    const contextIndex = txInfoRedeemerIndexes.findIndex(
      (candidate) => candidate === txInfoIndex,
    );
    return contextIndex < 0 ? undefined : redeemerPointers[contextIndex];
  };
  const activeRedeemerPointer =
    activeRedeemerLayoutIndex >= 0
      ? findRedeemerPointerByTxInfoIndex(activeRedeemerLayoutIndex)
      : undefined;
  const stateQueueRedeemerPointer =
    stateQueueRedeemerLayoutIndex >= 0
      ? findRedeemerPointerByTxInfoIndex(stateQueueRedeemerLayoutIndex)
      : undefined;
  const activeRedeemerCbor = findRedeemerDataCbor(tx, activeRedeemerPointer);
  const stateQueueRedeemerCbor = findRedeemerDataCbor(
    tx,
    stateQueueRedeemerPointer,
  );
  const headerOutput = outputs.find(
    (output) => output.index === headerOutputIndex,
  );
  const previousHeaderOutput = outputs.find(
    (output) => output.index === previousHeaderOutputIndex,
  );
  const parsedHeaderDatum =
    headerOutput === undefined || headerOutput.datum == null
      ? undefined
      : (() => {
          try {
            const nodeDatum = decodeLinkedListNodeViewFromOutput(headerOutput);
            if (nodeDatum === undefined) {
              return undefined;
            }
            const header = LucidData.castFrom(nodeDatum.data, SDK.Header);
            return {
              startTime: header.startTime.toString(),
              endTime: header.endTime.toString(),
              operatorVkey: header.operatorVkey,
            };
          } catch {
            return undefined;
          }
        })();
  const parsedPreviousDatum =
    previousHeaderOutput === undefined || previousHeaderOutput.datum == null
      ? undefined
      : (() => {
          try {
            const nodeDatum =
              decodeLinkedListNodeViewFromOutput(previousHeaderOutput);
            if (nodeDatum === undefined) {
              return undefined;
            }
            try {
              const header = LucidData.castFrom(nodeDatum.data, SDK.Header);
              return {
                kind: "Header" as const,
                endTime: header.endTime.toString(),
              };
            } catch {
              const confirmedState = LucidData.castFrom(
                nodeDatum.data,
                SDK.ConfirmedState,
              );
              return {
                kind: "ConfirmedState" as const,
                endTime: confirmedState.endTime.toString(),
              };
            }
          } catch {
            return undefined;
          }
        })();
  const parsedSchedulerDatum =
    schedulerRefInput.datum == null
      ? undefined
      : (() => {
          try {
            const schedulerDatum = LucidData.from(
              schedulerRefInput.datum,
              SDK.SchedulerDatum as never,
            ) as CanonicalSchedulerDatum;
            return schedulerDatum === "NoActiveOperators"
              ? { kind: "NoActiveOperators" as const }
              : {
                  kind: "ActiveOperator" as const,
                  operator: schedulerDatum.ActiveOperator.operator,
                  startTime:
                    schedulerDatum.ActiveOperator.start_time.toString(),
                };
          } catch {
            return undefined;
          }
        })();

  return {
    requiredSigners,
    operatorSignerPresent: requiredSigners.includes(operatorKeyHash),
    inputs: [...Array(inputs.len()).keys()].map((i) => {
      const input = inputs.get(i);
      return `${i}:${input.transaction_id().to_hex()}#${input.index().toString()}`;
    }),
    referenceInputs:
      referenceInputs === undefined
        ? []
        : [...Array(referenceInputs.len()).keys()].map((i) => {
            const input = referenceInputs.get(i);
            return `${i}:${input.transaction_id().to_hex()}#${input.index().toString()}`;
          }),
    expectedLatestBlockInput: outRefLabel(latestBlockInput),
    expectedSchedulerRefInput: outRefLabel(schedulerRefInput),
    expectedHubOracleRefInput: outRefLabel(hubOracleRefInput),
    expectedActiveOperatorInput: outRefLabel(activeOperatorInput),
    layout: {
      schedulerRefInputIndex: layout.schedulerRefInputIndex.toString(),
      latestBlockInputIndex: layout.latestBlockInputIndex.toString(),
      activeOperatorsInputIndex: layout.activeOperatorsInputIndex.toString(),
      newBlockOutputIndex: layout.newBlockOutputIndex.toString(),
      continuedLatestBlockOutputIndex:
        layout.continuedLatestBlockOutputIndex.toString(),
      activeOperatorsRedeemerIndex:
        layout.activeOperatorsRedeemerIndex.toString(),
      activeOperatorOutputIndex: layout.activeOperatorOutputIndex.toString(),
      hubOracleRefInputIndex: layout.hubOracleRefInputIndex.toString(),
      stateQueueSpendRedeemerIndex:
        layout.stateQueueSpendRedeemerIndex.toString(),
    },
    latestBlockInputAtLayoutIndex: findTxInputAtIndex(
      inputs,
      layout.latestBlockInputIndex,
    ),
    activeOperatorsInputAtLayoutIndex: findTxInputAtIndex(
      inputs,
      layout.activeOperatorsInputIndex,
    ),
    schedulerRefInputAtLayoutIndex: findTxReferenceInputAtIndex(
      referenceInputs,
      layout.schedulerRefInputIndex,
    ),
    hubOracleRefInputAtLayoutIndex: findTxReferenceInputAtIndex(
      referenceInputs,
      layout.hubOracleRefInputIndex,
    ),
    activeRedeemerPointer:
      activeRedeemerPointer === undefined
        ? "missing"
        : `${redeemerTagLabel(activeRedeemerPointer.tag)}:${activeRedeemerPointer.index.toString()}`,
    stateQueueRedeemerPointer:
      stateQueueRedeemerPointer === undefined
        ? "missing"
        : `${redeemerTagLabel(stateQueueRedeemerPointer.tag)}:${stateQueueRedeemerPointer.index.toString()}`,
    activeRedeemerShape:
      activeRedeemerCbor === undefined
        ? "missing"
        : (() => {
            try {
              return LucidData.from(
                activeRedeemerCbor,
                SDK.ActiveOperatorSpendRedeemer,
              );
            } catch {
              return `decode-failed:${activeRedeemerCbor}`;
            }
          })(),
    stateQueueRedeemerShape:
      stateQueueRedeemerCbor === undefined
        ? "missing"
        : (() => {
            try {
              return LucidData.from(
                stateQueueRedeemerCbor,
                SDK.StateQueueRedeemer,
              );
            } catch {
              return `decode-failed:${stateQueueRedeemerCbor}`;
            }
          })(),
    headerTiming: {
      header: parsedHeaderDatum,
      previous: parsedPreviousDatum,
      startMatchesPreviousEnd:
        parsedHeaderDatum === undefined || parsedPreviousDatum === undefined
          ? undefined
          : parsedHeaderDatum.startTime === parsedPreviousDatum.endTime,
      endMatchesTxValidityUpperBound:
        parsedHeaderDatum === undefined ||
        txValidityUpperBoundUnixTime === undefined
          ? undefined
          : parsedHeaderDatum.endTime === txValidityUpperBoundUnixTime,
      txValidityUpperBoundSlot: txValidityUpperBoundSlot ?? "missing",
      txValidityUpperBoundUnixTime: txValidityUpperBoundUnixTime ?? "missing",
    },
    schedulerDatum: parsedSchedulerDatum,
    redeemerPointersInOrder: redeemerPointers.map(
      (pointer, index) =>
        `${index}:${redeemerTagLabel(pointer.tag)}:${pointer.index.toString()}`,
    ),
    headerOutputAtLayoutIndex:
      headerOutput === undefined
        ? "missing"
        : {
            address: headerOutput.address,
            datumMatches: headerOutput.datum === appendedNodeDatumCbor,
            stateQueueTokenQty: (
              headerOutput.assets[headerNodeUnit] ?? 0n
            ).toString(),
            assets: describeUtxoAssets(headerOutput.assets),
          },
    previousHeaderOutputAtLayoutIndex:
      previousHeaderOutput === undefined
        ? "missing"
        : {
            address: previousHeaderOutput.address,
            datumMatches:
              previousHeaderOutput.datum === previousHeaderNodeDatumCbor,
            assets: describeUtxoAssets(previousHeaderOutput.assets),
            matchesLatestInputAssets: assetsEqual(
              previousHeaderOutput.assets,
              latestBlockInput.assets,
            ),
          },
    latestBlockInputAssets: describeUtxoAssets(latestBlockInput.assets),
    activeOperatorInputAssets: describeUtxoAssets(activeOperatorInput.assets),
    activeOperatorExpectedTokenQty: (
      activeOperatorInput.assets[expectedActiveUnit] ?? 0n
    ).toString(),
    schedulerRefInputAssets: describeUtxoAssets(schedulerRefInput.assets),
    schedulerExpectedTokenQty: (
      schedulerRefInput.assets[expectedSchedulerUnit] ?? 0n
    ).toString(),
    hubOracleRefInputAssets: describeUtxoAssets(hubOracleRefInput.assets),
    hubOracleExpectedTokenQty: (
      hubOracleRefInput.assets[expectedHubOracleUnit] ?? 0n
    ).toString(),
    stateQueueAddress,
  };
};
