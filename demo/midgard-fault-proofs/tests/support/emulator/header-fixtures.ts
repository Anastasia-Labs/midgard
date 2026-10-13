import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  Header,
  OutputReference,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { encodeData } from "../../../src/index.js";

export const SETUP_OUTPUT_INDEX = {
  stateQueueRoot: 3n,
  activeOperatorsRoot: 4n,
  retiredOperatorsRoot: 5n,
  registeredOperatorsRoot: 6n,
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
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  // The mapping the emulator families' validation-trace contexts are built
  // with (the fixtures' synthetic network is Preprod).
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  contracts,
});

/**
 * The slot whose last millisecond is `blockTime` under the slot mapping the
 * deployment's commit yield was built with, as the commit requires of
 * `block_slot`. Refuses a `blockTime` that is not a slot's last millisecond
 * under that mapping.
 */
export const commitSlotForBlockTime = (
  l1SlotConfig: L1SlotConfig,
  blockTime: bigint,
): bigint => {
  const slotLength = BigInt(l1SlotConfig.slotLength);
  const elapsed = blockTime + 1n - BigInt(l1SlotConfig.zeroTime);
  if (elapsed <= 0n || elapsed % slotLength !== 0n)
    throw new Error(
      `block time ${blockTime.toString()} is not the last millisecond of a slot`,
    );
  return BigInt(l1SlotConfig.zeroSlot) + elapsed / slotLength - 1n;
};

/**
 * The slot mapping of the fault-proofs emulator deployments: their synthetic
 * network is Preprod, and each emulator starts on a whole second
 * (`alignEmulatorToWholeSecond`) so its slot boundaries fall on Preprod's.
 */
export const EMULATOR_DEPLOYMENT_L1_SLOT_CONFIG: L1SlotConfig =
  NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

/**
 * The block_slot a fixture header ending at `endTime` commits with on an
 * emulator deployment (block_time = endTime). A fixture whose `endTime` is not
 * a slot's last millisecond never commits and keeps `uncommittedSlot`.
 */
export const fixtureBlockSlot = (
  endTime: bigint,
  uncommittedSlot = 0n,
): bigint => {
  const config = EMULATOR_DEPLOYMENT_L1_SLOT_CONFIG;
  const elapsed = endTime + 1n - BigInt(config.zeroTime);
  return elapsed > 0n && elapsed % BigInt(config.slotLength) === 0n
    ? commitSlotForBlockTime(config, endTime)
    : uncommittedSlot;
};

/**
 * `header` as committed at `blockTime` (default endTime): its block_slot is the
 * slot whose last millisecond is that block_time under the deployment's mapping.
 */
export const withCommitSlot = (
  l1SlotConfig: L1SlotConfig,
  header: Header,
  blockTime: bigint = header.endTime,
): Header => ({
  ...header,
  blockSlot: commitSlotForBlockTime(l1SlotConfig, blockTime),
});

export const makeHeader = (
  operatorVkey: string,
  now: number,
  transactionsRoot = EMPTY_MERKLE_TREE_ROOT,
  l2TransactionCount = 0n,
  withdrawalsRoot = EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount = 0n,
): Header => {
  const totalEventCount = l2TransactionCount + withdrawalCount;
  const hasEvents = totalEventCount > 0n;
  const eventCommitmentRoot = hasEvents
    ? transactionsRoot
    : EMPTY_MERKLE_TREE_ROOT;
  return {
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot,
    ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
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
    blockSlot: fixtureBlockSlot(BigInt(now + 1_000)),
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
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
