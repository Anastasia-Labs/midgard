/**
 * Structural state-queue header and snapshot records.
 *
 * These are the canonical JSON shapes the watcher uses to carry an
 * L1-authenticated state-queue header (W22 header-root reconstruction, W24
 * Phase A) and a queue snapshot (attestation-timeout observation). `make*`
 * builds a digest-bound record; `parse*` admits one only when it is exactly
 * structural and its header hash or snapshot digest recomputes. Neither
 * performs chain observation:
 * production state-queue authority is the local node, via
 * `authenticated-state-queue-observation.ts`.
 */
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
const STATE_QUEUE_SNAPSHOT_BOUNDS = Object.freeze({
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
const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const sha256Canonical = watcherSha256CanonicalJson;
const same = watcherSameCanonicalJson;
const headerHashFromCbor = (cborHex: string): string =>
  Buffer.from(blake2b(Buffer.from(cborHex, "hex"), { dkLen: 28 })).toString(
    "hex",
  );

const exactRecord = (
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

const exactArray = (
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

const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);
const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);
const isNatural = (value: unknown): value is string =>
  typeof value === "string" &&
  NATURAL.test(value) &&
  value.length <= 20 &&
  BigInt(value) <= STATE_QUEUE_SNAPSHOT_BOUNDS.uint64Maximum;
const isNullableNatural = (value: unknown): value is string | null =>
  value === null || isNatural(value);
const isNullableHex28 = (value: unknown): value is string | null =>
  value === null || isHex28(value);

const dataRoundTrip = <T>(cborHex: string, schema: unknown): T | null => {
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

const headerData = (
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

const headerView = (
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

const HEADER_KEYS = [
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

export const makeWatcherStateQueueHeader = (
  value: Omit<WatcherStateQueueHeader, "headerHash" | "headerCborHex">,
): WatcherStateQueueHeader | null => {
  try {
    const { nextHeaderHash, datumSha256, daAttestationPolicyId, ...fields } =
      value;
    const header = headerData(fields);
    const view = {
      ...headerView(header, nextHeaderHash, datumSha256),
      daAttestationPolicyId,
    };
    return parseWatcherStateQueueHeader(view);
  } catch {
    return null;
  }
};

export const parseWatcherStateQueueHeader = (
  value: unknown,
): WatcherStateQueueHeader | null => {
  const record = exactRecord(value, HEADER_KEYS);
  if (
    record === null ||
    !isHex28(record.headerHash) ||
    typeof record.headerCborHex !== "string" ||
    !HEX_BYTES.test(record.headerCborHex) ||
    !isNullableHex28(record.nextHeaderHash) ||
    !isHex32(record.datumSha256) ||
    !isHex32(record.prevUtxosRoot) ||
    !isHex32(record.utxosRoot) ||
    !isHex32(record.withdrawalsRoot) ||
    !isHex32(record.forcedTransactionsRoot) ||
    !isHex32(record.transactionsRoot) ||
    !isHex32(record.depositsRoot) ||
    !isHex32(record.transitionTraceRoot) ||
    !isHex32(record.eventToStepRoot) ||
    !isHex32(record.validationTracesRoot) ||
    !isNatural(record.withdrawalCount) ||
    !isNatural(record.forcedTransactionCount) ||
    !isNatural(record.l2TransactionCount) ||
    !isNatural(record.depositCount) ||
    !isNatural(record.totalEventCount) ||
    !isNatural(record.transitionStepCount) ||
    !isNatural(record.validationTraceCount) ||
    !isNatural(record.startTime) ||
    !isNatural(record.endTime) ||
    !isNatural(record.blockSlot) ||
    !isNatural(record.expectedNetworkId) ||
    !isNatural(record.minFeeA) ||
    !isNatural(record.minFeeB) ||
    !isHex28(record.prevHeaderHash) ||
    !isHex28(record.operatorVkey) ||
    !isNatural(record.protocolVersion) ||
    !isNullableHex28(record.daAttestationPolicyId)
  ) {
    return null;
  }
  const decoded = dataRoundTrip<Header>(record.headerCborHex, Header);
  if (decoded === null) {
    return null;
  }
  const expected = headerView(
    decoded,
    record.nextHeaderHash,
    record.datumSha256,
  );
  const comparable = {
    ...expected,
    daAttestationPolicyId: record.daAttestationPolicyId,
  };
  if (
    !same(value, comparable) ||
    record.headerHash !== headerHashFromCbor(record.headerCborHex) ||
    BigInt(record.endTime) <= BigInt(record.startTime) ||
    BigInt(record.withdrawalCount) >
      STATE_QUEUE_SNAPSHOT_BOUNDS.withdrawalCount ||
    BigInt(record.forcedTransactionCount) >
      STATE_QUEUE_SNAPSHOT_BOUNDS.forcedTransactionCount ||
    BigInt(record.l2TransactionCount) >
      STATE_QUEUE_SNAPSHOT_BOUNDS.l2TransactionCount ||
    BigInt(record.depositCount) > STATE_QUEUE_SNAPSHOT_BOUNDS.depositCount ||
    BigInt(record.totalEventCount) !==
      BigInt(record.withdrawalCount) +
        BigInt(record.forcedTransactionCount) +
        BigInt(record.l2TransactionCount) +
        BigInt(record.depositCount) ||
    BigInt(record.transitionStepCount) !== BigInt(record.totalEventCount) ||
    BigInt(record.validationTraceCount) !==
      BigInt(record.forcedTransactionCount) +
        BigInt(record.l2TransactionCount) ||
    (BigInt(record.withdrawalCount) === 0n) !==
      (record.withdrawalsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.forcedTransactionCount) === 0n) !==
      (record.forcedTransactionsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.l2TransactionCount) === 0n) !==
      (record.transactionsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.depositCount) === 0n) !==
      (record.depositsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.totalEventCount) === 0n) !==
      (record.transitionTraceRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.totalEventCount) === 0n) !==
      (record.eventToStepRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.validationTraceCount) === 0n) !==
      (record.validationTracesRoot === EMPTY_MERKLE_ROOT)
  ) {
    return null;
  }
  return Object.freeze(comparable);
};

const parseConfirmed = (value: unknown): WatcherConfirmedState | null => {
  const record = exactRecord(value, [
    "headerHash",
    "prevHeaderHash",
    "utxosRoot",
    "startTime",
    "endTime",
    "protocolVersion",
    "datumSha256",
  ]);
  return record !== null &&
    isHex28(record.headerHash) &&
    isHex28(record.prevHeaderHash) &&
    isHex32(record.utxosRoot) &&
    isNatural(record.startTime) &&
    isNatural(record.endTime) &&
    isNatural(record.protocolVersion) &&
    isHex32(record.datumSha256) &&
    BigInt(record.endTime) >= BigInt(record.startTime)
    ? Object.freeze(record as unknown as WatcherConfirmedState)
    : null;
};

const parseScheduler = (value: unknown): WatcherIndexedScheduler | null => {
  const record = exactRecord(value, [
    "operatorVkey",
    "shiftStartTime",
    "datumSha256",
  ]);
  if (
    record === null ||
    !isNullableHex28(record.operatorVkey) ||
    !isNullableNatural(record.shiftStartTime) ||
    !isHex32(record.datumSha256) ||
    (record.operatorVkey === null) !== (record.shiftStartTime === null)
  ) {
    return null;
  }
  return Object.freeze(record as unknown as WatcherIndexedScheduler);
};

const parseActive = (value: unknown): WatcherIndexedActiveOperator | null => {
  const record = exactRecord(value, [
    "operatorVkey",
    "nextOperatorVkey",
    "bondUnlockTime",
    "inactivityStrikes",
    "datumSha256",
  ]);
  return record !== null &&
    isHex28(record.operatorVkey) &&
    isNullableHex28(record.nextOperatorVkey) &&
    isNullableNatural(record.bondUnlockTime) &&
    isNatural(record.inactivityStrikes) &&
    isHex32(record.datumSha256)
    ? Object.freeze(record as unknown as WatcherIndexedActiveOperator)
    : null;
};

const parseRetired = (value: unknown): WatcherIndexedRetiredOperator | null => {
  const record = exactRecord(value, [
    "operatorVkey",
    "nextOperatorVkey",
    "bondUnlockTime",
    "datumSha256",
  ]);
  return record !== null &&
    isHex28(record.operatorVkey) &&
    isNullableHex28(record.nextOperatorVkey) &&
    isNullableNatural(record.bondUnlockTime) &&
    isHex32(record.datumSha256)
    ? Object.freeze(record as unknown as WatcherIndexedRetiredOperator)
    : null;
};

const snapshotWithoutDigest = (
  value: Omit<WatcherStateQueueSnapshot, "snapshotDigest">,
) => ({ ...value });

export const makeWatcherStateQueueSnapshot = (
  value: Omit<WatcherStateQueueSnapshot, "schemaVersion" | "snapshotDigest">,
): WatcherStateQueueSnapshot | null => {
  const canonical = {
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherStateQueueSnapshot({
    ...canonical,
    snapshotDigest: sha256Canonical(canonical),
  });
};

/**
 * Parses the indexer's derived projection for durable restart/replay.
 * Snapshots are outputs of node-derived topology reconstruction, never an
 * accepted observation or security-boundary input.
 */
export const parseWatcherStateQueueSnapshot = (
  value: unknown,
): WatcherStateQueueSnapshot | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "confirmedState",
    "queue",
    "scheduler",
    "activeOperators",
    "retiredOperators",
    "quarantinedFromHeaderHash",
    "snapshotDigest",
  ]);
  const confirmed =
    record === null ? null : parseConfirmed(record.confirmedState);
  const scheduler = record === null ? null : parseScheduler(record.scheduler);
  const queueValues =
    record === null
      ? null
      : exactArray(record.queue, STATE_QUEUE_SNAPSHOT_BOUNDS.queueNodes);
  const activeValues =
    record === null
      ? null
      : exactArray(
          record.activeOperators,
          STATE_QUEUE_SNAPSHOT_BOUNDS.activeOperators,
        );
  const retiredValues =
    record === null
      ? null
      : exactArray(
          record.retiredOperators,
          STATE_QUEUE_SNAPSHOT_BOUNDS.activeOperators,
        );
  const queue = queueValues?.map(parseWatcherStateQueueHeader) ?? null;
  const active = activeValues?.map(parseActive) ?? null;
  const retired = retiredValues?.map(parseRetired) ?? null;
  if (
    record === null ||
    confirmed === null ||
    scheduler === null ||
    queue === null ||
    active === null ||
    retired === null ||
    queue.some((entry) => entry === null) ||
    active.some((entry) => entry === null) ||
    retired.some((entry) => entry === null) ||
    !isNullableHex28(record.quarantinedFromHeaderHash) ||
    !isHex32(record.snapshotDigest)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    confirmedState: confirmed,
    queue: Object.freeze(queue as WatcherStateQueueHeader[]),
    scheduler,
    activeOperators: Object.freeze(active as WatcherIndexedActiveOperator[]),
    retiredOperators: Object.freeze(retired as WatcherIndexedRetiredOperator[]),
    quarantinedFromHeaderHash: record.quarantinedFromHeaderHash,
  });
  const allOperators = [
    ...canonical.activeOperators.map(({ operatorVkey }) => operatorVkey),
    ...canonical.retiredOperators.map(({ operatorVkey }) => operatorVkey),
  ];
  const queueLinks = canonical.queue.every(
    (header, index) =>
      header.nextHeaderHash ===
      (canonical.queue[index + 1]?.headerHash ?? null),
  );
  const chainBreaks = canonical.queue
    .map((header, index) => {
      const previous = canonical.queue[index - 1];
      return index > 0 &&
        (header.prevHeaderHash !== previous?.headerHash ||
          header.prevUtxosRoot !== previous.utxosRoot ||
          BigInt(header.startTime) !== BigInt(previous.endTime))
        ? (previous?.headerHash ?? null)
        : null;
    })
    .filter((entry): entry is string => entry !== null);
  const queueHead = canonical.queue[0];
  if (
    sha256Canonical(snapshotWithoutDigest(canonical)) !==
      record.snapshotDigest ||
    !queueLinks ||
    chainBreaks.length > 1 ||
    (chainBreaks[0] ?? null) !== canonical.quarantinedFromHeaderHash ||
    (queueHead !== undefined &&
      (queueHead.prevHeaderHash !== canonical.confirmedState.headerHash ||
        queueHead.prevUtxosRoot !== canonical.confirmedState.utxosRoot ||
        BigInt(queueHead.startTime) !==
          BigInt(canonical.confirmedState.endTime))) ||
    new Set(canonical.queue.map(({ headerHash }) => headerHash)).size !==
      canonical.queue.length ||
    new Set(allOperators).size !== allOperators.length ||
    !linkedKeys(
      canonical.activeOperators.map((entry) => [
        entry.operatorVkey,
        entry.nextOperatorVkey,
      ]),
    ) ||
    !linkedKeys(
      canonical.retiredOperators.map((entry) => [
        entry.operatorVkey,
        entry.nextOperatorVkey,
      ]),
    ) ||
    (canonical.scheduler.operatorVkey !== null &&
      !canonical.activeOperators.some(
        ({ operatorVkey }) => operatorVkey === canonical.scheduler.operatorVkey,
      ))
  ) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    snapshotDigest: record.snapshotDigest,
  });
};

const linkedKeys = (
  entries: readonly (readonly [string, string | null])[],
): boolean =>
  entries.every(
    ([key, next], index) =>
      key === entries[index]?.[0] &&
      next === (entries[index + 1]?.[0] ?? null) &&
      (next === null || key < next),
  );
