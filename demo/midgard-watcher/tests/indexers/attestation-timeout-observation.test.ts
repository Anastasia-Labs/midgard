import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import { deriveWatcherAttestationTimeoutObservation } from "../../src/indexers/attestation-timeout-observation.js";
import {
  makeWatcherStateQueueHeader,
  makeWatcherStateQueueSnapshot,
} from "../../src/indexers/state-queue-snapshot.js";

const emptyRoot =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

const snapshot = (daAttestationPolicyId: string | null) => {
  const confirmedState = {
    headerHash: h28(0x11),
    prevHeaderHash: h28(0x00),
    utxosRoot: h32(0x22),
    startTime: "0",
    endTime: "1000",
    protocolVersion: "1",
    datumSha256: h32(0x33),
  } as const;
  const header = makeWatcherStateQueueHeader({
    nextHeaderHash: null,
    datumSha256: h32(0x44),
    prevUtxosRoot: confirmedState.utxosRoot,
    utxosRoot: h32(0x55),
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
    operatorVkey: h28(0xdd),
    protocolVersion: "1",
    daAttestationPolicyId,
  });
  expect(header).not.toBeNull();
  const value = makeWatcherStateQueueSnapshot({
    confirmedState,
    queue: [header!],
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
  });
  expect(value).not.toBeNull();
  return value!;
};

describe("watcher attestation-timeout observation", () => {
  it("reports waiting, near-timeout, and boundary timed-out states", () => {
    const value = snapshot(null);
    expect(
      deriveWatcherAttestationTimeoutObservation({
        snapshot: value,
        nowMs: 3_000_000n,
        alertLeadMs: 120_000n,
      })?.status,
    ).toBe("waiting");
    expect(
      deriveWatcherAttestationTimeoutObservation({
        snapshot: value,
        nowMs: 3_550_000n,
        alertLeadMs: 120_000n,
      })?.status,
    ).toBe("near_timeout");
    expect(
      deriveWatcherAttestationTimeoutObservation({
        snapshot: value,
        nowMs: 3_602_000n,
        alertLeadMs: 120_000n,
      })?.status,
    ).toBe("timed_out");
  });

  it("uses the applied attestation marker and never emits a timeout for it", () => {
    expect(
      deriveWatcherAttestationTimeoutObservation({
        snapshot: snapshot(h28(0xaa)),
        nowMs: 9_000_000n,
        alertLeadMs: 120_000n,
      }),
    ).toMatchObject({ status: "attested", daAttestationApplied: true });
  });

  it("rejects a snapshot whose authenticated digest no longer matches", () => {
    const value = snapshot(null);
    expect(
      deriveWatcherAttestationTimeoutObservation({
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

it("observes an expired unattested suffix behind an attested head", () => {
  const base = snapshot(h28(0xaa));
  const first = base.queue[0]!;
  const { headerHash: _hash, headerCborHex: _cbor, ...fields } = first;
  const tail = makeWatcherStateQueueHeader({
    ...fields,
    prevHeaderHash: first.headerHash,
    prevUtxosRoot: first.utxosRoot,
    startTime: first.endTime,
    endTime: "3000",
    blockSlot: "2",
    daAttestationPolicyId: null,
  });
  expect(tail).not.toBeNull();
  const { snapshotDigest: _digest, ...snapshotFields } = base;
  const extended = makeWatcherStateQueueSnapshot({
    ...snapshotFields,
    queue: [{ ...first, nextHeaderHash: tail!.headerHash }, tail!],
  });
  expect(extended).not.toBeNull();
  expect(
    deriveWatcherAttestationTimeoutObservation({
      snapshot: extended,
      nowMs: 3_603_000n,
      alertLeadMs: 120_000n,
    }),
  ).toMatchObject({
    status: "timed_out",
    headerHash: tail!.headerHash,
    deadlineMs: "3603000",
  });
});
