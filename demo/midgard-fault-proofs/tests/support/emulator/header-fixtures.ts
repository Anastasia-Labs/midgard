import { MIDGARD_PROTOCOL_V1_VERSION } from "@al-ft/midgard-core";
import {
  EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  HeaderV1,
  OutputReference,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { encodeData } from "../../../src/index.js";

export const SETUP_OUTPUT_INDEX = {
  stateQueueRoot: 2n,
  activeOperatorsRoot: 3n,
  retiredOperatorsRoot: 4n,
} as const;

export const ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX = {
  root: 0n,
  insertedNode: 1n,
} as const;

export const SCHEDULER_APPOINTMENT_OUTPUT_INDEX = {
  scheduler: 0n,
} as const;

export const h32 = (byte: string): string => byte.repeat(32);

export const deploymentManifest = (
  contracts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown> = {},
) => ({
  referenceScriptAuthPolicy,
  contracts,
});

export const makeHeader = (
  operatorVkey: string,
  now: number,
  transactionsRoot = EMPTY_MERKLE_TREE_ROOT,
  l2TransactionCount = 0n,
  withdrawalsRoot = EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount = 0n,
): HeaderV1 => {
  const totalEventCount = l2TransactionCount + withdrawalCount;
  const hasEvents = totalEventCount > 0n;
  const eventCommitmentRoot = hasEvents
    ? transactionsRoot
    : EMPTY_MERKLE_TREE_ROOT;
  return {
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot,
    ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
    l2TransactionCount,
    withdrawalCount,
    totalEventCount,
    transitionStepCount: totalEventCount,
    validationTraceCount: l2TransactionCount,
    transactionsRoot,
    transitionTraceRoot: eventCommitmentRoot,
    eventToStepRoot: eventCommitmentRoot,
    validationTracesRoot: eventCommitmentRoot,
    depositsRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: BigInt(now),
    endTime: BigInt(now + 1_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_V1_VERSION),
  };
};

export const transitionTraceOutRef = (byte: string): OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

export const transitionTraceDaEntry = <K, V>({
  key,
  keySchema,
  value,
  valueSchema,
}: {
  readonly key: K;
  readonly keySchema: Parameters<typeof Data.Nullable>[0];
  readonly value: V;
  readonly valueSchema: Parameters<typeof Data.Nullable>[0];
}): [string, string] => [
  encodeData(key, keySchema).toString("hex"),
  encodeData(value, valueSchema).toString("hex"),
];
