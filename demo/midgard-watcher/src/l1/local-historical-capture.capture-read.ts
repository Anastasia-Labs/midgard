import { isProxy } from "node:util/types";

import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
  type LocalKupmiosRawBlockAtPoint,
} from "@al-ft/midgard-fault-proofs";

import { type WatcherConfig } from "../runtime/config.js";
import { type WatcherNativeBlockAdmission } from "./native-block-admission.js";
import { openWatcherNativeExactPointQuery } from "./native-chain-sync.js";

export const captureBrand = Symbol("local-historical-capture");

const MAX_UINT64 = (1n << 64n) - 1n;

/** Capture-time corroboration only; this is not historical/W12 authority. */
export type WatcherLocalHistoricalCaptureReceipt = Readonly<{
  [captureBrand]: true;
}>;

export type CaptureRead = Readonly<{
  network: WatcherConfig["targetNetwork"];
  deploymentIdentityDigest: string;
  blueprintHash: string;
  sourceId: string;
  sourceBinding: Readonly<{
    sourceMode: "local_node";
    network: WatcherConfig["targetNetwork"];
    authorityNodeId: string;
    genesisIdentitySha256: string;
    chainSyncSocketPath: string;
    queryServices: readonly Readonly<{
      kind: "kupo" | "ogmios";
      providerId: string;
      endpoint: string;
      admittedSourceUrl: string;
    }>[];
  }>;
  rawBlock: LocalKupmiosRawBlockAtPoint;
  creatingTransactionBodies: readonly string[];
  finalityConfig: WatcherConfig["l1"]["finality"];
  startedAtMonotonicMs: number;
  nativeAuthorityDigest: string;
  nativeStartupDigest: string;
  targetEventDigest: string;
  point: FraudProofRawL1Point;
  predecessorPoint: FraudProofRawL1Point;
  nativeBlock: WatcherNativeBlockAdmission;
  observedNativeTip: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
  }>;
  depthAtObservedTip: string;
  observedAt: string;
  expiresAt: string;
}>;

export const captures = new WeakMap<
  WatcherLocalHistoricalCaptureReceipt,
  Readonly<{ read(): CaptureRead }>
>();

/** A read checks the owned snapshot's liveness; it acquires no new chain tip. */
export const readWatcherLocalHistoricalCapture = (
  receipt: WatcherLocalHistoricalCaptureReceipt,
): CaptureRead => {
  const owner = captures.get(receipt);
  if (owner === undefined)
    throw new Error("local historical capture receipt is absent or stale");
  return owner.read();
};

export const exactData = (
  value: unknown,
  required: readonly string[],
  optional: readonly string[],
  label: string,
): void => {
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be exact plain data`);
  }
  const descriptors = Object.getOwnPropertyDescriptors(value);
  if (
    Reflect.ownKeys(value).some(
      (key) =>
        typeof key !== "string" ||
        (!required.includes(key) && !optional.includes(key)),
    ) ||
    required.some((key) => !Object.hasOwn(descriptors, key)) ||
    Object.values(descriptors).some(
      (descriptor) => !("value" in descriptor) || !descriptor.enumerable,
    )
  )
    throw new Error(`${label} has unknown, missing or non-data fields`);
};

export const boundedInteger = (
  value: number,
  minimum: number,
  maximum: number,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || value < minimum || value > maximum)
    throw new Error(`${label} is outside its operational bound`);
  return value;
};

export const admitPoint = (
  value: FraudProofRawL1Point,
): FraudProofRawL1Point => {
  exactData(
    value,
    ["blockHash", "blockNo", "slot", "pointId"],
    [],
    "capture point",
  );
  const point = admitFraudProofRawL1Point(value, "capture point");
  for (const value of [point.blockNo, point.slot]) {
    if (value.length > 20 || BigInt(value) > MAX_UINT64)
      throw new Error("capture point exceeds UInt64");
  }
  return Object.freeze(point);
};

export const queryPoint = ({
  blockHash,
  blockNo,
  slot,
}: FraudProofRawL1Point) => Object.freeze({ blockHash, blockNo, slot });

export const httpEndpoint = (endpoint: string): string => {
  const url = new URL(endpoint.trim());
  if (url.protocol === "ws:") url.protocol = "http:";
  if (url.protocol === "wss:") url.protocol = "https:";
  url.hash = "";
  return url.toString().replace(/\/$/u, "");
};

const sameStrings = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

export const assertBlockAgreement = (
  block: WatcherNativeBlockAdmission,
  raw: LocalKupmiosRawBlockAtPoint,
  predecessor: FraudProofRawL1Point,
): void => {
  if (
    block.blockHash !== raw.point.blockHash ||
    block.blockNo !== raw.point.blockNo ||
    block.slot !== raw.point.slot ||
    block.prevHash !== raw.parentBlockHash ||
    block.prevHash !== predecessor.blockHash ||
    BigInt(predecessor.blockNo) + 1n !== BigInt(block.blockNo) ||
    BigInt(predecessor.slot) >= BigInt(block.slot) ||
    raw.kupoCheckpoint.blockHash !== block.blockHash ||
    raw.kupoCheckpoint.slot.toString() !== block.slot ||
    !sameStrings(
      block.transactionIds,
      raw.transactions.map(({ txHash }) => txHash),
    ) ||
    !sameStrings(
      block.transactionCbors,
      raw.transactions.map(({ transactionCbor }) => transactionCbor),
    )
  )
    throw new Error(
      "historical capture native and Kupo/Ogmios blocks disagree",
    );
};

export type NativeQuery = Awaited<
  ReturnType<typeof openWatcherNativeExactPointQuery>
>;
