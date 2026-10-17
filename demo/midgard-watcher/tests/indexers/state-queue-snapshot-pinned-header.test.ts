import { DA_ATTESTATION_TIMEOUT_MS } from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import { deriveWatcherAttestationTimeoutObservation } from "../../src/indexers/attestation-timeout-observation.js";
import {
  EMPTY_MERKLE_ROOT,
  headerData,
  headerView,
  sha256Canonical,
  WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
  type WatcherStateQueueHeader,
} from "../../src/indexers/state-queue-snapshot.header-view.js";
import { parseWatcherStateQueueSnapshot } from "../../src/indexers/state-queue-snapshot.js";

type HeaderFields = Omit<
  WatcherStateQueueHeader,
  "headerHash" | "headerCborHex"
>;

const confirmedState = {
  headerHash: h28(0x11),
  prevHeaderHash: h28(0x00),
  utxosRoot: h32(0x22),
  startTime: "0",
  endTime: "1000",
  protocolVersion: "1",
  datumSha256: h32(0x33),
} as const;

/**
 * A header for a block with one forced and two L2 transactions. Its
 * validation-trace pair is pinned to the empty root and a zero count, as the
 * state-queue validator requires, while the block body keeps three traces.
 */
const pinnedFields: HeaderFields = {
  nextHeaderHash: null,
  datumSha256: h32(0x44),
  prevUtxosRoot: confirmedState.utxosRoot,
  utxosRoot: h32(0x55),
  withdrawalsRoot: EMPTY_MERKLE_ROOT,
  forcedTransactionsRoot: h32(0x61),
  transactionsRoot: h32(0x62),
  depositsRoot: EMPTY_MERKLE_ROOT,
  transitionTraceRoot: h32(0x63),
  eventToStepRoot: h32(0x64),
  validationTracesRoot: EMPTY_MERKLE_ROOT,
  withdrawalCount: "0",
  forcedTransactionCount: "1",
  l2TransactionCount: "2",
  depositCount: "0",
  totalEventCount: "3",
  transitionStepCount: "3",
  validationTraceCount: "0",
  startTime: confirmedState.endTime,
  endTime: "2000",
  blockTime: "2000",
  blockSlot: "1",
  expectedNetworkId: "0",
  minFeeA: "44",
  minFeeB: "155381",
  prevHeaderHash: confirmedState.headerHash,
  operatorVkey: h28(0xdd),
  protocolVersion: "1",
  daAttestationPolicyId: null,
};

/** Encodes the header record exactly as the state-queue datum reader does. */
const headerRecord = ({
  nextHeaderHash,
  datumSha256,
  daAttestationPolicyId,
  blockTime,
  ...fields
}: HeaderFields): WatcherStateQueueHeader => ({
  ...headerView(headerData(fields), nextHeaderHash, datumSha256, blockTime),
  daAttestationPolicyId,
});

/** A raw, digest-bound snapshot whose queue holds one header record. */
const rawSnapshot = (fields: HeaderFields): unknown => {
  const canonical = {
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    confirmedState,
    queue: [headerRecord(fields)],
    scheduler: {
      operatorVkey: h28(0xdd),
      shiftStartTime: "900",
      datumSha256: h32(0xee),
    },
    activeOperators: [
      {
        operatorVkey: h28(0xdd),
        nextOperatorVkey: null,
        bondUnlockTime: null,
        inactivityStrikes: "0",
        datumSha256: h32(0xff),
      },
    ],
    retiredOperators: [],
    quarantinedFromHeaderHash: null,
  };
  return { ...canonical, snapshotDigest: sha256Canonical(canonical) };
};

const observe = (fields: HeaderFields) =>
  deriveWatcherAttestationTimeoutObservation({
    snapshot: rawSnapshot(fields),
    nowMs: 2_000n + DA_ATTESTATION_TIMEOUT_MS - 120_001n,
    alertLeadMs: 120_000n,
  });

describe("watcher state-queue snapshot with a pinned validation-trace pair", () => {
  it("indexes an honest pinned header of a block with forced and L2 transactions", () => {
    expect(
      BigInt(pinnedFields.forcedTransactionCount) +
        BigInt(pinnedFields.l2TransactionCount),
    ).toBeGreaterThan(0n);
    const snapshot = parseWatcherStateQueueSnapshot(rawSnapshot(pinnedFields));
    expect(snapshot).not.toBeNull();
    expect(snapshot!.queue[0]).toMatchObject({
      validationTracesRoot: EMPTY_MERKLE_ROOT,
      validationTraceCount: "0",
      forcedTransactionCount: "1",
      l2TransactionCount: "2",
    });
    expect(observe(pinnedFields)).toMatchObject({
      status: "waiting",
      headerHash: headerRecord(pinnedFields).headerHash,
      daAttestationApplied: false,
    });
  });

  it.each([
    [
      "the built root and count",
      { validationTracesRoot: h32(0x65), validationTraceCount: "3" },
    ],
    [
      "the empty root with the built count",
      { validationTracesRoot: EMPTY_MERKLE_ROOT, validationTraceCount: "3" },
    ],
    [
      "a built root with a zero count",
      { validationTracesRoot: h32(0x65), validationTraceCount: "0" },
    ],
  ])("drops a header that commits %s", (_label, pair) => {
    const fields = { ...pinnedFields, ...pair };
    expect(parseWatcherStateQueueSnapshot(rawSnapshot(fields))).toBeNull();
    expect(observe(fields)).toBeNull();
  });

  it.each([
    ["total_event_count", { totalEventCount: "4" }],
    ["transition_step_count", { transitionStepCount: "2" }],
    [
      "forced_transaction_count",
      { forcedTransactionCount: "0", totalEventCount: "2" },
    ],
  ])(
    "still drops a pinned header whose %s disagrees with its other fields",
    (_label, counts) => {
      const fields = { ...pinnedFields, ...counts };
      expect(parseWatcherStateQueueSnapshot(rawSnapshot(fields))).toBeNull();
      expect(observe(fields)).toBeNull();
    },
  );
});
