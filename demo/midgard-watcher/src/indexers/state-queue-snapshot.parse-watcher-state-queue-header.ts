import { Header } from "@al-ft/midgard-sdk";

import {
  dataRoundTrip,
  EMPTY_MERKLE_ROOT,
  exactRecord,
  HEADER_KEYS,
  headerData,
  headerHashFromCbor,
  headerView,
  HEX_BYTES,
  isHex28,
  isHex32,
  isNatural,
  isNullableHex28,
  isNullableNatural,
  same,
  STATE_QUEUE_SNAPSHOT_BOUNDS,
  type WatcherConfirmedState,
  type WatcherIndexedActiveOperator,
  type WatcherIndexedRetiredOperator,
  type WatcherIndexedScheduler,
  type WatcherStateQueueHeader,
  type WatcherStateQueueSnapshot,
} from "./state-queue-snapshot.header-view.js";

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

export const parseConfirmed = (
  value: unknown,
): WatcherConfirmedState | null => {
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

export const parseScheduler = (
  value: unknown,
): WatcherIndexedScheduler | null => {
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

export const parseActive = (
  value: unknown,
): WatcherIndexedActiveOperator | null => {
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

export const parseRetired = (
  value: unknown,
): WatcherIndexedRetiredOperator | null => {
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

export const snapshotWithoutDigest = (
  value: Omit<WatcherStateQueueSnapshot, "snapshotDigest">,
) => ({ ...value });

export const linkedKeys = (
  entries: readonly (readonly [string, string | null])[],
): boolean =>
  entries.every(
    ([key, next], index) =>
      key === entries[index]?.[0] &&
      next === (entries[index + 1]?.[0] ?? null) &&
      (next === null || key < next),
  );
