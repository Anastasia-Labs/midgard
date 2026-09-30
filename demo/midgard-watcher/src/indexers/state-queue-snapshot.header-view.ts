import { Header } from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";

export const WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION =
  "midgard-watcher-state-queue-snapshot-v1" as const;

/** Structural bounds a header or snapshot record must satisfy to parse. */
export const STATE_QUEUE_SNAPSHOT_BOUNDS = Object.freeze({
  queueNodes: 4_096,
  activeOperators: 4_096,
  uint64Maximum: 18_446_744_073_709_551_615n,
  withdrawalCount: 10_000n,
  forcedTransactionCount: 10_000n,
  l2TransactionCount: 10_000n,
  depositCount: 10_000n,
});

export type WatcherConfirmedState = Readonly<{
  headerHash: string;
  prevHeaderHash: string;
  utxosRoot: string;
  startTime: string;
  endTime: string;
  protocolVersion: string;
  datumSha256: string;
}>;

export type WatcherStateQueueHeader = Readonly<{
  headerHash: string;
  headerCborHex: string;
  nextHeaderHash: string | null;
  datumSha256: string;
  prevUtxosRoot: string;
  utxosRoot: string;
  withdrawalsRoot: string;
  forcedTransactionsRoot: string;
  transactionsRoot: string;
  depositsRoot: string;
  transitionTraceRoot: string;
  eventToStepRoot: string;
  validationTracesRoot: string;
  withdrawalCount: string;
  forcedTransactionCount: string;
  l2TransactionCount: string;
  depositCount: string;
  totalEventCount: string;
  transitionStepCount: string;
  validationTraceCount: string;
  startTime: string;
  endTime: string;
  blockSlot: string;
  expectedNetworkId: string;
  minFeeA: string;
  minFeeB: string;
  prevHeaderHash: string;
  operatorVkey: string;
  protocolVersion: string;
  daAttestationPolicyId: string | null;
}>;

export type WatcherIndexedActiveOperator = Readonly<{
  operatorVkey: string;
  nextOperatorVkey: string | null;
  bondUnlockTime: string | null;
  inactivityStrikes: string;
  datumSha256: string;
}>;

export type WatcherIndexedRetiredOperator = Readonly<{
  operatorVkey: string;
  nextOperatorVkey: string | null;
  bondUnlockTime: string | null;
  datumSha256: string;
}>;

export type WatcherIndexedScheduler = Readonly<{
  operatorVkey: string | null;
  shiftStartTime: string | null;
  datumSha256: string;
}>;

export type WatcherStateQueueSnapshot = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION;
  confirmedState: WatcherConfirmedState;
  queue: readonly WatcherStateQueueHeader[];
  scheduler: WatcherIndexedScheduler;
  activeOperators: readonly WatcherIndexedActiveOperator[];
  retiredOperators: readonly WatcherIndexedRetiredOperator[];
  quarantinedFromHeaderHash: string | null;
  snapshotDigest: string;
}>;

type PlainRecord = Record<string, unknown>;

const HEX_28 = /^[0-9a-f]{56}$/u;

const HEX_32 = /^[0-9a-f]{64}$/u;

export const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

export const sha256Canonical = watcherSha256CanonicalJson;

export const same = watcherSameCanonicalJson;

export const headerHashFromCbor = (cborHex: string): string =>
  Buffer.from(blake2b(Buffer.from(cborHex, "hex"), { dkLen: 28 })).toString(
    "hex",
  );

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  if (
    actual.length !== keys.length ||
    actual.some((key) => typeof key !== "string" || !keys.includes(key))
  ) {
    return null;
  }
  for (const key of keys) {
    const descriptor = Object.getOwnPropertyDescriptor(value, key);
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as PlainRecord;
};

export const exactArray = (
  value: unknown,
  maximum: number,
): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    value.length > maximum ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value;
};

export const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);

export const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);

export const isNatural = (value: unknown): value is string =>
  typeof value === "string" &&
  NATURAL.test(value) &&
  value.length <= 20 &&
  BigInt(value) <= STATE_QUEUE_SNAPSHOT_BOUNDS.uint64Maximum;

export const isNullableNatural = (value: unknown): value is string | null =>
  value === null || isNatural(value);

export const isNullableHex28 = (value: unknown): value is string | null =>
  value === null || isHex28(value);

export const dataRoundTrip = <T>(
  cborHex: string,
  schema: unknown,
): T | null => {
  try {
    const decoded = Data.from(cborHex, schema as never) as T;
    const lucidHex = Data.to(decoded as never, schema as never);
    const cardanoHex =
      CML.PlutusData.from_cbor_hex(cborHex).to_canonical_cbor_hex();
    return lucidHex === cborHex || cardanoHex === cborHex ? decoded : null;
  } catch {
    return null;
  }
};

export const headerData = (
  value: Omit<
    WatcherStateQueueHeader,
    | "headerHash"
    | "headerCborHex"
    | "nextHeaderHash"
    | "datumSha256"
    | "daAttestationPolicyId"
  >,
): Header => ({
  prevUtxosRoot: value.prevUtxosRoot,
  utxosRoot: value.utxosRoot,
  withdrawalsRoot: value.withdrawalsRoot,
  forcedTransactionsRoot: value.forcedTransactionsRoot,
  transactionsRoot: value.transactionsRoot,
  depositsRoot: value.depositsRoot,
  transitionTraceRoot: value.transitionTraceRoot,
  eventToStepRoot: value.eventToStepRoot,
  validationTracesRoot: value.validationTracesRoot,
  withdrawalCount: BigInt(value.withdrawalCount),
  forcedTransactionCount: BigInt(value.forcedTransactionCount),
  l2TransactionCount: BigInt(value.l2TransactionCount),
  depositCount: BigInt(value.depositCount),
  totalEventCount: BigInt(value.totalEventCount),
  transitionStepCount: BigInt(value.transitionStepCount),
  validationTraceCount: BigInt(value.validationTraceCount),
  startTime: BigInt(value.startTime),
  endTime: BigInt(value.endTime),
  blockSlot: BigInt(value.blockSlot),
  expectedNetworkId: BigInt(value.expectedNetworkId),
  minFeeA: BigInt(value.minFeeA),
  minFeeB: BigInt(value.minFeeB),
  prevHeaderHash: value.prevHeaderHash,
  operatorVkey: value.operatorVkey,
  protocolVersion: BigInt(value.protocolVersion),
});

export const headerView = (
  header: Header,
  nextHeaderHash: string | null,
  datumSha256: string,
): WatcherStateQueueHeader => {
  const headerCborHex = Data.to(header, Header);
  return Object.freeze({
    headerHash: headerHashFromCbor(headerCborHex),
    headerCborHex,
    nextHeaderHash,
    datumSha256,
    prevUtxosRoot: header.prevUtxosRoot,
    utxosRoot: header.utxosRoot,
    withdrawalsRoot: header.withdrawalsRoot,
    forcedTransactionsRoot: header.forcedTransactionsRoot,
    transactionsRoot: header.transactionsRoot,
    depositsRoot: header.depositsRoot,
    transitionTraceRoot: header.transitionTraceRoot,
    eventToStepRoot: header.eventToStepRoot,
    validationTracesRoot: header.validationTracesRoot,
    withdrawalCount: header.withdrawalCount.toString(),
    forcedTransactionCount: header.forcedTransactionCount.toString(),
    l2TransactionCount: header.l2TransactionCount.toString(),
    depositCount: header.depositCount.toString(),
    totalEventCount: header.totalEventCount.toString(),
    transitionStepCount: header.transitionStepCount.toString(),
    validationTraceCount: header.validationTraceCount.toString(),
    startTime: header.startTime.toString(),
    endTime: header.endTime.toString(),
    blockSlot: header.blockSlot.toString(),
    expectedNetworkId: header.expectedNetworkId.toString(),
    minFeeA: header.minFeeA.toString(),
    minFeeB: header.minFeeB.toString(),
    prevHeaderHash: header.prevHeaderHash,
    operatorVkey: header.operatorVkey,
    protocolVersion: header.protocolVersion.toString(),
    daAttestationPolicyId: null,
  });
};

export const HEADER_KEYS = [
  "headerHash",
  "headerCborHex",
  "nextHeaderHash",
  "datumSha256",
  "prevUtxosRoot",
  "utxosRoot",
  "withdrawalsRoot",
  "forcedTransactionsRoot",
  "transactionsRoot",
  "depositsRoot",
  "transitionTraceRoot",
  "eventToStepRoot",
  "validationTracesRoot",
  "withdrawalCount",
  "forcedTransactionCount",
  "l2TransactionCount",
  "depositCount",
  "totalEventCount",
  "transitionStepCount",
  "validationTraceCount",
  "startTime",
  "endTime",
  "blockSlot",
  "expectedNetworkId",
  "minFeeA",
  "minFeeB",
  "prevHeaderHash",
  "operatorVkey",
  "protocolVersion",
  "daAttestationPolicyId",
] as const;
