import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { BridgeDependencies } from "../../src/fault-proofs/fault-decision-bridge.selected-target.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";

// A Locked fraud-proof correction no admitted fault reproduces gives the pass
// no target. The watcher still finishes that removal from the proven record
// on L1, which needs no decision of its own.

type FinishLockedRemoval = NonNullable<
  BridgeDependencies["driveProvenRemoval"]
>;

const PROVEN_RECORD = `${"31".repeat(32)}#0`;

const lockedBy = (category: "doubleSpend" | "invalidRange") => {
  const header = headerFixture("03");
  const plain = observation([header]);
  const headerHash = plain.finalizedHeaders[0]!.headerHash;
  const fraudProofAssetName = `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]}${headerHash}`;
  return {
    current: observation([header], {
      Locked: {
        target_header_hash: headerHash,
        correction_identity: {
          FraudProof: { fraud_proof_asset_name: fraudProofAssetName },
        },
      },
    }),
    headerHash,
    fraudProofAssetName,
  };
};

const settled = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (error: Error) => void;
  const promise = new Promise<T>((onResolve, onReject) => {
    resolve = onResolve;
    reject = onReject;
  });
  return { promise, resolve, reject };
};

/** Lets the bridge's handlers on a settled removal run. */
const drain = async () => await new Promise((resolve) => setTimeout(resolve));

// The classifier reproduces doubleSpend; the lock names invalidRange.
const unreproducedLock = (
  driveProvenRemoval: FinishLockedRemoval,
  monotonicNowMs?: () => number,
) => {
  const lock = lockedBy("invalidRange");
  const warnings: unknown[] = [];
  const current = harness({
    current: lock.current,
    categoryByHeader: { [lock.headerHash]: "doubleSpend" },
    warn: (warning) => void warnings.push(warning),
    driveProvenRemoval,
    ...(monotonicNowMs === undefined ? {} : { monotonicNowMs }),
  });
  return { ...lock, ...current, warnings };
};

const failureMessages = (warnings: readonly unknown[]) =>
  warnings
    .filter(
      (warning) =>
        (warning as { event: string }).event ===
        "correction_lock_finishing_removal_failed",
    )
    .map((warning) => (warning as { message: string }).message);

describe("a Locked correction no admitted fault reproduces", () => {
  it("finishes the removal from the proven record until the lock moves on", async () => {
    const calls: unknown[] = [];
    const removal = settled<unknown>();
    let liveLock: "locked" | "moved" = "locked";
    const lock = unreproducedLock(async (locked) => {
      calls.push(locked);
      return liveLock === "moved"
        ? { kind: "finished" }
        : {
            kind: "proven",
            fraudProofOutRef: PROVEN_RECORD,
            removal: removal.promise,
          };
    });
    const finishing = {
      reason: "correction_lock_finishing_removal",
      detail: `${lock.fraudProofAssetName}: locked: finishing removal from the proven record ${PROVEN_RECORD}`,
    };

    const first = await lock.bridge.reconcileAndDispatch(lock.current);
    expect(first.target).toBe(null);
    expect(lock.enqueued).toHaveLength(0);
    expect(calls).toEqual([
      {
        headerHash: lock.headerHash,
        fraudProofAssetName: lock.fraudProofAssetName,
      },
    ]);
    expect(lock.bridge.readiness()).toEqual([finishing]);

    // One removal at a time: a pass while it runs starts no other.
    await lock.bridge.reconcileAndDispatch(lock.current);
    expect(calls).toHaveLength(1);
    expect(lock.bridge.readiness()).toEqual([finishing]);

    removal.resolve({});
    liveLock = "moved";
    await drain();
    // The finalized lock still reads Locked; the live one has moved on.
    await lock.bridge.reconcileAndDispatch(lock.current);
    expect(calls).toHaveLength(2);
    expect(lock.bridge.readiness()).toEqual([]);
    expect(
      lock.warnings.filter(
        (warning) =>
          (warning as { event: string }).event ===
          "correction_lock_finishing_removal",
      ),
    ).toEqual([
      {
        event: "correction_lock_finishing_removal",
        headerHash: lock.headerHash,
        fraudProofAssetName: lock.fraudProofAssetName,
        fraudProofOutRef: PROVEN_RECORD,
      },
    ]);
  });

  it("reports a failed removal and starts it again once its backoff has run", async () => {
    const removals = [settled<unknown>(), settled<unknown>()];
    let started = 0;
    let nowMs = 0;
    const lock = unreproducedLock(
      async () => ({
        kind: "proven",
        fraudProofOutRef: PROVEN_RECORD,
        removal: removals[started++]!.promise,
      }),
      () => nowMs,
    );
    await lock.bridge.reconcileAndDispatch(lock.current);
    removals[0]!.reject(new Error("prover funding unavailable"));
    await drain();
    expect(lock.bridge.readiness()).toEqual([
      {
        reason: "correction_lock_finishing_removal",
        detail: `${lock.fraudProofAssetName}: locked: finishing removal from the proven record ${PROVEN_RECORD}; last attempt failed: prover funding unavailable`,
      },
    ]);
    expect(lock.warnings).toContainEqual({
      event: "correction_lock_finishing_removal_failed",
      headerHash: lock.headerHash,
      fraudProofAssetName: lock.fraudProofAssetName,
      message: "prover funding unavailable",
    });

    nowMs = 1_000;
    const retried = await lock.bridge.reconcileAndDispatch(lock.current);
    expect(retried.target).toBe(null);
    expect(started).toBe(2);
    expect(lock.bridge.readiness()).toEqual([
      {
        reason: "correction_lock_finishing_removal",
        detail: `${lock.fraudProofAssetName}: locked: finishing removal from the proven record ${PROVEN_RECORD}`,
      },
    ]);
  });

  it("retries a failing removal on a doubling backoff, warning once per message", async () => {
    let nowMs = 0;
    const failures = ["coin taken", "coin taken", "provider unavailable"];
    let calls = 0;
    const lock = unreproducedLock(
      async () => ({
        kind: "proven",
        fraudProofOutRef: PROVEN_RECORD,
        removal: Promise.reject(new Error(failures[calls++] ?? "done")),
      }),
      () => nowMs,
    );
    const passAt = async (atMs: number) => {
      nowMs = atMs;
      await lock.bridge.reconcileAndDispatch(lock.current);
      await drain();
      return calls;
    };
    expect(await passAt(0)).toBe(1);
    // The first retry waits 1 s, the next 2 s.
    expect(await passAt(999)).toBe(1);
    expect(await passAt(1_000)).toBe(2);
    expect(await passAt(2_999)).toBe(2);
    expect(await passAt(3_000)).toBe(3);
    expect(failureMessages(lock.warnings)).toEqual([
      "coin taken",
      "provider unavailable",
    ]);
    expect(lock.bridge.readiness()).toEqual([
      {
        reason: "correction_lock_finishing_removal",
        detail: `${lock.fraudProofAssetName}: locked: finishing removal from the proven record ${PROVEN_RECORD}; last attempt failed: provider unavailable`,
      },
    ]);
  });

  it("starts a confirmed correction's removal afresh, with no backoff left", async () => {
    let nowMs = 0;
    const outcomes = [
      () => Promise.reject(new Error("coin taken")),
      () => Promise.resolve({}),
      () => Promise.reject(new Error("coin taken")),
    ];
    let calls = 0;
    const lock = unreproducedLock(
      async () => ({
        kind: "proven",
        fraudProofOutRef: PROVEN_RECORD,
        removal: outcomes[calls++]!(),
      }),
      () => nowMs,
    );
    const pass = async () => {
      await lock.bridge.reconcileAndDispatch(lock.current);
      await drain();
    };
    await pass();
    nowMs = 1_000;
    await pass();
    // The removal confirmed: the next pass runs at once, and its failure is
    // warned again as a first failure.
    await pass();
    expect(calls).toBe(3);
    expect(failureMessages(lock.warnings)).toEqual([
      "coin taken",
      "coin taken",
    ]);
  });

  it("starts a correction afresh once L1 shows it finished, with no backoff left", async () => {
    let nowMs = 0;
    const outcomes = [
      { kind: "proven", failure: "coin taken" },
      { kind: "finished" },
      { kind: "proven", failure: "coin taken" },
      { kind: "proven", failure: "coin taken" },
    ] as const;
    let calls = 0;
    const lock = unreproducedLock(
      async () => {
        const outcome = outcomes[calls++]!;
        return outcome.kind === "finished"
          ? outcome
          : {
              kind: "proven",
              fraudProofOutRef: PROVEN_RECORD,
              removal: Promise.reject(new Error(outcome.failure)),
            };
      },
      () => nowMs,
    );
    const passAt = async (atMs: number) => {
      nowMs = atMs;
      await lock.bridge.reconcileAndDispatch(lock.current);
      await drain();
      return calls;
    };
    expect(await passAt(0)).toBe(1);
    expect(await passAt(1_000)).toBe(2);
    // Finished: the next pass runs at once, its failure is warned as a
    // first failure, and its retry waits the first backoff, not a doubled one.
    expect(await passAt(1_000)).toBe(3);
    expect(failureMessages(lock.warnings)).toEqual([
      "coin taken",
      "coin taken",
    ]);
    expect(await passAt(2_000)).toBe(4);
  });

  it("keeps holding the lock when no proven record names it, and the pass succeeds", async () => {
    let calls = 0;
    const lock = unreproducedLock(async () => {
      calls += 1;
      return { kind: "unproven" };
    });
    for (let pass = 1; pass <= 2; pass += 1) {
      const result = await lock.bridge.reconcileAndDispatch(lock.current);
      expect(result.target).toBe(null);
      expect(lock.enqueued).toHaveLength(0);
      expect(lock.bridge.readiness()).toEqual([
        {
          reason: "correction_lock_unreproduced",
          detail: lock.fraudProofAssetName,
        },
      ]);
      expect(calls).toBe(pass);
    }
  });

  it("reports a failing L1 read as the held lock and the pass succeeds", async () => {
    const lock = unreproducedLock(async () => {
      throw new Error("provider unavailable");
    });
    const result = await lock.bridge.reconcileAndDispatch(lock.current);
    expect(result.target).toBe(null);
    expect(lock.bridge.readiness()).toEqual([
      {
        reason: "correction_lock_unreproduced",
        detail: `${lock.fraudProofAssetName}; last attempt failed: provider unavailable`,
      },
    ]);
  });

  it("starts nothing from recovery, which never dispatches", async () => {
    let calls = 0;
    const lock = unreproducedLock(async () => {
      calls += 1;
      return { kind: "unproven" };
    });
    await lock.bridge.prepareForRecovery(lock.current);
    expect(calls).toBe(0);
  });
});

// A completed proof's record stays on L1 and its node names it. With the
// lock Idle, the watcher removes that header from the record even when the
// classifier, say after an upgrade, now reproduces another category.
const provenUnderIdle = (
  driveProvenRemoval: FinishLockedRemoval,
  reproduced: "doubleSpend" | "invalidRange",
) => {
  const header = headerFixture("03");
  const headerHash = observation([header]).finalizedHeaders[0]!.headerHash;
  const fraudProofAssetName = `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.invalidRange}${headerHash}`;
  const current = observation([header], "Idle", [], [], [fraudProofAssetName]);
  return {
    ...harness({
      current,
      categoryByHeader: { [headerHash]: reproduced },
      driveProvenRemoval,
    }),
    current,
    headerHash,
    fraudProofAssetName,
  };
};

describe("a queued header a completed proof convicts, under an Idle lock", () => {
  it("starts its removal from the record whatever the classifier now reproduces", async () => {
    const calls: unknown[] = [];
    const removal = settled<unknown>();
    const proven = provenUnderIdle(async (correction) => {
      calls.push(correction);
      return {
        kind: "proven",
        fraudProofOutRef: PROVEN_RECORD,
        removal: removal.promise,
      };
    }, "doubleSpend");

    const result = await proven.bridge.reconcileAndDispatch(proven.current);
    // The fresh fault is still the target; the record's removal runs beside it.
    expect(result.target?.category).toBe("doubleSpend");
    expect(calls).toEqual([
      {
        headerHash: proven.headerHash,
        fraudProofAssetName: proven.fraudProofAssetName,
      },
    ]);
    expect(proven.bridge.readiness()).toEqual([
      {
        reason: "correction_lock_finishing_removal",
        detail: `${proven.fraudProofAssetName}: idle: removing the queued header from the proven record ${PROVEN_RECORD}`,
      },
    ]);

    removal.resolve({});
    await drain();
    expect(proven.bridge.readiness()).toEqual([]);
  });

  it("leaves the removal to the target's own workflow when it is that correction", async () => {
    let calls = 0;
    const proven = provenUnderIdle(async () => {
      calls += 1;
      return { kind: "unproven" };
    }, "invalidRange");
    const result = await proven.bridge.reconcileAndDispatch(proven.current);
    expect(result.target?.category).toBe("invalidRange");
    expect(calls).toBe(0);
    expect(proven.bridge.readiness()).toEqual([]);
  });

  it("starts nothing and reports nothing when this watcher only observes", async () => {
    const warnings: unknown[] = [];
    const lock = lockedBy("invalidRange");
    const observer = harness({
      current: lock.current,
      categoryByHeader: { [lock.headerHash]: "doubleSpend" },
      warn: (warning) => void warnings.push(warning),
      driveProvenRemoval: async () => ({ kind: "observing" }),
    });
    for (let pass = 1; pass <= 2; pass += 1)
      await observer.bridge.reconcileAndDispatch(lock.current);
    expect(
      warnings.map((warning) => (warning as { event: string }).event),
    ).toEqual(["correction_lock_unreproduced"]);
  });
});
