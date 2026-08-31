import { describe, expect, it } from "vitest";

import { deriveWatcherAttestationTimeoutObservationV1 } from "../src/attestation-timeout-observation-v1.js";
import {
  makeWatcherStateQueueHeaderV1,
  makeWatcherStateQueueSnapshotV1,
} from "../src/state-queue-indexer.js";

const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const emptyRoot =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

const snapshot = (daAttestationPolicyId: string | null) => {
  const confirmedState = {
    headerHash: h28("1"),
    prevHeaderHash: h28("0"),
    utxosRoot: h32("2"),
    startTime: "0",
    endTime: "1000",
    protocolVersion: "1",
    datumSha256: h32("3"),
  } as const;
  const header = makeWatcherStateQueueHeaderV1({
    nextHeaderHash: null,
    datumSha256: h32("4"),
    prevUtxosRoot: confirmedState.utxosRoot,
    utxosRoot: h32("5"),
    withdrawalsRoot: emptyRoot,
    forcedTransactionsRoot: emptyRoot,
    transactionsRoot: emptyRoot,
    depositsRoot: emptyRoot,
    transitionTraceRoot: emptyRoot,
    eventToStepRoot: emptyRoot,
    validationTracesRoot: emptyRoot,
    withdrawalCount: "0",
    forcedTransactionCount: "0",
    l2TransactionCount: "0",
    depositCount: "0",
    totalEventCount: "0",
    transitionStepCount: "0",
    validationTraceCount: "0",
    startTime: confirmedState.endTime,
    endTime: "2000",
    blockSlot: "1",
    expectedNetworkId: "0",
    minFeeA: "44",
    minFeeB: "155381",
    prevHeaderHash: confirmedState.headerHash,
    operatorVkey: h28("d"),
    protocolVersion: "1",
    daAttestationPolicyId,
  });
  expect(header).not.toBeNull();
  const value = makeWatcherStateQueueSnapshotV1({
    confirmedState,
    queue: [header!],
    scheduler: {
      operatorVkey: h28("d"),
      shiftStartTime: "900",
      datumSha256: h32("e"),
    },
    activeOperators: [
      {
        operatorVkey: h28("d"),
        nextOperatorVkey: null,
        bondUnlockTime: null,
        inactivityStrikes: "0",
        datumSha256: h32("f"),
      },
    ],
    retiredOperators: [],
    quarantinedFromHeaderHash: null,
  });
  expect(value).not.toBeNull();
  return value!;
};

describe("watcher attestation-timeout observation", () => {
  it("reports waiting, near-timeout, and boundary timed-out states", () => {
    const value = snapshot(null);
    expect(
      deriveWatcherAttestationTimeoutObservationV1({
        snapshot: value,
        nowMs: 3_000_000n,
        alertLeadMs: 120_000n,
      })?.status,
    ).toBe("waiting");
    expect(
      deriveWatcherAttestationTimeoutObservationV1({
        snapshot: value,
        nowMs: 3_550_000n,
        alertLeadMs: 120_000n,
      })?.status,
    ).toBe("near_timeout");
    expect(
      deriveWatcherAttestationTimeoutObservationV1({
        snapshot: value,
        nowMs: 3_602_000n,
        alertLeadMs: 120_000n,
      })?.status,
    ).toBe("timed_out");
  });

  it("uses the applied attestation marker and never emits a timeout for it", () => {
    expect(
      deriveWatcherAttestationTimeoutObservationV1({
        snapshot: snapshot(h28("a")),
        nowMs: 9_000_000n,
        alertLeadMs: 120_000n,
      }),
    ).toMatchObject({ status: "attested", daAttestationApplied: true });
  });

  it("rejects a snapshot whose authenticated digest no longer matches", () => {
    const value = snapshot(null);
    expect(
      deriveWatcherAttestationTimeoutObservationV1({
        snapshot: {
          ...value,
          queue: [{ ...value.queue[0], endTime: "999999999" }],
        },
        nowMs: 3_000_000n,
        alertLeadMs: 120_000n,
      }),
    ).toBeNull();
  });
});
