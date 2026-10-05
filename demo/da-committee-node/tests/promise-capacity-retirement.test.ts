import { afterEach, describe, expect, it, vi } from "vitest";

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
import { JsonFileCommitteeStore } from "../src/store.js";
import { tempDir } from "./helpers.js";

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
const stores = new Set<JsonFileCommitteeStore>();
afterEach(async () => {
  for (const store of stores) await store.close();
  stores.clear();
});
const fixture = async () => {
  const dir = await tempDir();
  let store = await JsonFileCommitteeStore.open(dir);
  stores.add(store);
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
      stores.delete(store);
      store = await JsonFileCommitteeStore.open(dir);
      stores.add(store);
    },
    key: promiseCapacityEvidenceKey({
      ...identity,
      commitmentDigest: liability.commitmentDigest,
    }),
  };
};
describe("durable recovery-safe capacity release", () => {
  it("retains at2160 blocks AFTER cutoff inclusion, releases at2161 and reconstructs its protected floor on restart", async () => {
    const f = await fixture();
    expect(await f.run()).toEqual(new Set());
    const captured = await f.getStore().getPromiseCapacityEvidence(f.key);
    expect(captured).toMatchObject({
      point: f.getBoundary(),
      retirementKind: "open_cutoff",
    });
    f.setBoundary({ slot: 2260, blockNo: 2260, blockHash: "78".repeat(32) });
    expect(await f.run()).toEqual(new Set());
    f.setBoundary({ slot: 2261, blockNo: 2261, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    await f.reopen();
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    // The certified floor is monotonic even after the current tip becomes shallow.
    f.setBoundary({ slot: 110, blockNo: 110, blockHash: "bc".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.certifiedAt
        ?.blockNo,
    ).toBe(2261);
  });
  it("does not capture early time or active state; a later shallow Close starts a new full horizon", async () => {
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
    expect(await f.run()).toEqual(new Set());
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(3000);
    f.setBoundary({ slot: 3001, blockNo: 3001, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set());
  });
  it("reanchors an owned shallow orphan and retains work after within-k Open restoration", async () => {
    const f = await fixture();
    await f.run();
    f.setAbsent(true);
    f.setBoundary({ slot: 110, blockNo: 110, blockHash: "78".repeat(32) });
    f.setLiability({ ...liability, hasActiveChallenge: true });
    expect(await f.run()).toEqual(new Set());
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(100);
    f.setLiability({ ...liability, hasActiveChallenge: false });
    expect(await f.run()).toEqual(new Set());
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.point.blockNo,
    ).toBe(110);
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
    await expect(f.run()).rejects.toThrow("selected tip");
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
  it("uses a freshly authenticated k-safe terminal point before Open cutoff, with the same protected floor", async () => {
    const f = await fixture();
    const terminal = { slot: 90, blockNo: 90, blockHash: "56".repeat(32) };
    f.setLiability({
      ...liability,
      cutoffTimeMs: 10000000,
      terminalPoint: terminal,
    });
    f.setBoundary({ slot: 2250, blockNo: 2250, blockHash: "78".repeat(32) });
    expect(await f.run()).toEqual(new Set());
    f.setBoundary({ slot: 2251, blockNo: 2251, blockHash: "9a".repeat(32) });
    expect(await f.run()).toEqual(new Set([liability.commitmentDigest]));
    expect(
      (await f.getStore().getPromiseCapacityEvidence(f.key))?.retirementKind,
    ).toBe("terminal");
    f.setAbsent(true);
    await expect(f.run()).rejects.toThrow(
      "protected promise capacity rollback floor",
    );
  });
});
