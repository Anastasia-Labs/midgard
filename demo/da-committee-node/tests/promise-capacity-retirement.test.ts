import { describe, expect, it, vi } from "vitest";

import {
  type PromiseCapacityEvidence,
  promiseCapacityEvidenceKey,
  type PromiseCapacityPoint,
  promiseCapacityPointId,
} from "../src/availability/promise-capacity-evidence.js";
import {
  type PromiseCapacityLiability,
  retiredPromiseCutoffs,
} from "../src/availability/promise-cutoff-source.js";
import {
  openTestCommitteeStore,
  saveHealthyL1SourceState,
  testStoreDatabase,
} from "./helpers/committee-store.js";

const identity = {
  deploymentFingerprint: "ab".repeat(32),
  contractManifestId: "cd".repeat(32),
  actorId: "ef".repeat(28),
};
const liability: PromiseCapacityLiability = {
  headerHash: "12".repeat(28),
  commitmentDigest: "34".repeat(32),
  cutoffTimeMs: 100000,
};
const fixture = async () => {
  const database = await testStoreDatabase();
  let store = await saveHealthyL1SourceState(
    await openTestCommitteeStore(database),
  );
  let boundary: PromiseCapacityPoint = {
    slot: 100,
    blockNo: 100,
    blockHash: "56".repeat(32),
  };
  let liveLiability = liability;
  let absent = false;
  const assertCurrent = vi.fn(async () => {});
  const readCanonicalPoint = vi.fn(async (point: PromiseCapacityPoint) =>
    absent && point.blockHash === "56".repeat(32)
      ? null
      : { point, tip: boundary },
  );
  const run = () =>
    retiredPromiseCutoffs({
      store,
      ...identity,
      recoveryDepth: 2160,
      boundary,
      canonicalTimeMs: boundary.slot * 1000,
      slotTimeMs: (slot) => slot * 1000,
      liabilities: [liveLiability],
      readCanonicalPoint,
      assertCurrent,
    });
  return {
    run,
    readCanonicalPoint,
    assertCurrent,
    getStore: () => store,
    getBoundary: () => boundary,
    setBoundary: (next: PromiseCapacityPoint) => {
      boundary = next;
    },
    setLiability: (next: PromiseCapacityLiability) => {
      liveLiability = next;
    },
    setAbsent: (next: boolean) => {
      absent = next;
    },
    reopen: async () => {
      await store.close();
      store = await openTestCommitteeStore(database);
    },
    key: promiseCapacityEvidenceKey({
      ...identity,
      commitmentDigest: liability.commitmentDigest,
    }),
  };
};
describe("durable recovery-safe capacity release", () => {
  it("releases at the observed cutoff, certifies only beyond 2160 blocks and reconstructs its protected floor on restart", async () => {
    const f = await fixture();
    const certifiedAt = async () =>
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt;
    // Confirmed but unretired: the observation alone releases capacity.
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    const captured = await f.getStore().getPromiseCapacityEvidence(f.key);
    expect(captured).toMatchObject({
      point: f.getBoundary(),
      retirementKind: "open_cutoff",
    });
    expect(await certifiedAt()).toBeUndefined();
    // Depth 2160 (k) is not final; depth 2161 is.
    f.setBoundary({ slot: 2259, blockNo: 2259, blockHash: "78".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(await certifiedAt()).toBeUndefined();
    f.setBoundary({ slot: 2260, blockNo: 2260, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect((await certifiedAt())?.blockNo).toBe(2260);
    await f.reopen();
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    // The certified floor is monotonic even after the current tip becomes shallow.
    f.setBoundary({ slot: 110, blockNo: 110, blockHash: "bc".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt
        ?.blockNo,
    ).toBe(2260);
  });
  it("does not capture early time or active state; a later shallow Close releases at once and starts a new certification horizon", async () => {
    const f = await fixture();
    f.setBoundary({ slot: 99, blockNo: 99, blockHash: "56".repeat(32) });
    expect(await f.run()).toEqual(new Set());
    expect(
      await f.getStore().getPromiseCapacityEvidence(f.key),
    ).toBeUndefined();
    f.setBoundary({ slot: 3000, blockNo: 3000, blockHash: "78".repeat(32) });
    f.setLiability({ ...liability, hasActiveChallenge: true });
    expect(await f.run()).toEqual(new Set());
    expect(
      await f.getStore().getPromiseCapacityEvidence(f.key),
    ).toBeUndefined();
    f.setLiability({ ...liability, hasActiveChallenge: false });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(3000);
    f.setBoundary({ slot: 3001, blockNo: 3001, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt,
    ).toBeUndefined();
  });
  it("reanchors an owned shallow orphan, charges it while a within-k Open restores a challenge and releases it at the reanchored cutoff", async () => {
    const f = await fixture();
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    f.setAbsent(true);
    f.setBoundary({ slot: 110, blockNo: 110, blockHash: "78".repeat(32) });
    f.setLiability({ ...liability, hasActiveChallenge: true });
    expect(await f.run()).toEqual(new Set());
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(100);
    f.setLiability({ ...liability, hasActiveChallenge: false });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(110);
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt,
    ).toBeUndefined();
  });
  it("charges a released promise again when a rollback within k returns the chain before its cutoff", async () => {
    const f = await fixture();
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    // The new fork is shallower than k and its tip precedes the cutoff.
    f.setAbsent(true);
    f.setBoundary({ slot: 99, blockNo: 99, blockHash: "78".repeat(32) });
    expect(await f.run()).toEqual(new Set());
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(100);
    // Its cutoff is observed again on the new fork.
    f.setBoundary({ slot: 101, blockNo: 101, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(await f.getStore().getPromiseCapacityEvidence(f.key)).toMatchObject({
      point: { blockNo: 101, blockHash: "9a".repeat(32) },
      retirementKind: "open_cutoff",
    });
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt,
    ).toBeUndefined();
  });
  it("persists a crossed certified floor and refuses after restart or a healthy-looking return", async () => {
    const f = await fixture();
    await f.run();
    f.setBoundary({ slot: 2261, blockNo: 2261, blockHash: "78".repeat(32) });
    await f.run();
    f.setAbsent(true);
    await expect(f.run()).rejects.toThrow(
      "protected promise capacity rollback floor",
    );
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.floorViolatedAt,
    ).toEqual(f.getBoundary());
    await f.reopen();
    f.setAbsent(false);
    await expect(f.run()).rejects.toThrow("previously crossed");
  });
  it("rejects stale-tip, height, generation and clock evidence without a certificate", async () => {
    const f = await fixture();
    await f.run();
    f.setBoundary({ slot: 2261, blockNo: 2261, blockHash: "78".repeat(32) });
    f.readCanonicalPoint.mockImplementationOnce(async (point) => ({
      point,
      tip: { ...f.getBoundary(), blockHash: "9a".repeat(32) },
    }));
    await expect(f.run()).rejects.toThrow("read boundary");
    f.readCanonicalPoint.mockImplementationOnce(async (point) => ({
      point: { ...point, blockNo: 0 },
      tip: f.getBoundary(),
    }));
    await expect(f.run()).rejects.toThrow("height");
    f.assertCurrent.mockRejectedValueOnce(new Error("generation changed"));
    await expect(f.run()).rejects.toThrow("generation changed");
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt,
    ).toBeUndefined();
    f.setLiability({ ...liability, cutoffTimeMs: 100001 });
    await expect(f.run()).rejects.toThrow("matches the promise");
  });
  it("binds evidence atomically to actor, deployment, commitment and immutable certified point", async () => {
    const f = await fixture();
    await f.run();
    const captured = (await f.getStore().getPromiseCapacityEvidence(f.key))!;
    await expect(
      f
        .getStore()
        .savePromiseCapacityEvidence(
          { ...captured, cutoffTimeMs: 1 },
          promiseCapacityPointId(captured.point),
        ),
    ).rejects.toThrow("identity changed");
    await expect(
      f.getStore().savePromiseCapacityEvidence(captured, "stale-point"),
    ).rejects.toThrow("compare-and-set");
    const certified: PromiseCapacityEvidence = {
      ...captured,
      certifiedAt: { slot: 2261, blockNo: 2261, blockHash: "78".repeat(32) },
    };
    await f
      .getStore()
      .savePromiseCapacityEvidence(
        certified,
        promiseCapacityPointId(captured.point),
      );
    await expect(
      f.getStore().savePromiseCapacityEvidence(
        {
          ...certified,
          point: { slot: 101, blockNo: 101, blockHash: "9a".repeat(32) },
          certifiedAt: {
            slot: 3000,
            blockNo: 3000,
            blockHash: "bc".repeat(32),
          },
        },
        promiseCapacityPointId(captured.point),
      ),
    ).rejects.toThrow("protected capacity rollback floor");
  });
  it("releases at a freshly authenticated terminal point before Open cutoff, and certifies it with the same protected floor", async () => {
    const f = await fixture();
    const terminal = { slot: 90, blockNo: 90, blockHash: "56".repeat(32) };
    f.setLiability({
      ...liability,
      cutoffTimeMs: 10000000,
      terminalPoint: terminal,
    });
    f.setBoundary({ slot: 2249, blockNo: 2249, blockHash: "78".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt,
    ).toBeUndefined();
    f.setBoundary({ slot: 2250, blockNo: 2250, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(await f.getStore().getPromiseCapacityEvidence(f.key)).toMatchObject({
      retirementKind: "terminal",
      certifiedAt: { blockNo: 2250 },
    });
    f.setAbsent(true);
    await expect(f.run()).rejects.toThrow(
      "protected promise capacity rollback floor",
    );
  });
});
