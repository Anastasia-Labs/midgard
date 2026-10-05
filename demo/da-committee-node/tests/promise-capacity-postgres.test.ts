import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type PromiseCapacityEvidence,
  promiseCapacityEvidenceKey,
  promiseCapacityPointId,
} from "../src/availability/promise-capacity-evidence.js";
import { retiredPromiseCutoffs } from "../src/availability/promise-cutoff-source.js";
import type { L1SourceState } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

const databases = postgresTestDatabases("codex_rel_promise_capacity");
const stores = new Set<PostgresCommitteeStore>();
afterEach(async () => {
  for (const store of stores) await store.close();
  stores.clear();
});
afterAll(async () => databases.dropAll());
const identity = {
  deploymentFingerprint: "ab".repeat(32),
  contractManifestId: "cd".repeat(32),
  actorId: "ef".repeat(28),
};
const record: PromiseCapacityEvidence = {
  ...identity,
  headerHash: "12".repeat(28),
  commitmentDigest: "34".repeat(32),
  cutoffTimeMs: 100000,
  recoveryDepth: 2160,
  retirementKind: "open_cutoff",
  point: { slot: 100, blockNo: 100, blockHash: "56".repeat(32) },
};
const source: L1SourceState = {
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "78".repeat(32),
  status: "healthy",
  observations: [],
  observedAt: "2026-10-02T00:00:00.000Z",
};
const open = async (url: string) => {
  const store = await PostgresCommitteeStore.open(url);
  stores.add(store);
  return store;
};

describe("Postgres durable promise capacity evidence", () => {
  it("initializes its schema, releases at the observed cutoff, certifies only beyond k, and reconstructs exact bindings and protected floor on restart", async () => {
    const database = await databases.create();
    let store = await open(database.url);
    await store.saveL1SourceState(source);
    const beforeUsage = await store.promiseStoreResourceUsage();
    expect(beforeUsage.storeRecords).toBe(1);
    let boundary = record.point;
    const run = () =>
      retiredPromiseCutoffs({
        store,
        ...identity,
        recoveryDepth: 2160,
        boundary,
        canonicalTimeMs: boundary.slot * 1000,
        slotTimeMs: (slot) => slot * 1000,
        liabilities: [record],
        readCanonicalPoint: async (point) => ({ point, tip: boundary }),
        assertCurrent: async () => {},
      });
    const certifiedAt = async () =>
      (
        await store.getPromiseCapacityEvidence(
          promiseCapacityEvidenceKey(record),
        )
      )?.certifiedAt;
    expect(await run()).toEqual(new Set([record.commitmentDigest]));
    expect(await certifiedAt()).toBeUndefined();
    boundary = { slot: 2260, blockNo: 2260, blockHash: "9a".repeat(32) };
    expect(await run()).toEqual(new Set([record.commitmentDigest]));
    expect(await certifiedAt()).toBeUndefined();
    boundary = { slot: 2261, blockNo: 2261, blockHash: "bc".repeat(32) };
    expect(await run()).toEqual(new Set([record.commitmentDigest]));
    await store.close();
    stores.delete(store);
    store = await open(database.url);
    expect(await run()).toEqual(new Set([record.commitmentDigest]));
    expect(
      await store.getPromiseCapacityEvidence(
        promiseCapacityEvidenceKey(record),
      ),
    ).toEqual({ ...record, certifiedAt: boundary });
    const afterUsage = await store.promiseStoreResourceUsage();
    expect(afterUsage.storeRecords).toBe(2);
    expect(afterUsage.storeEncodedBytes).toBeGreaterThan(
      beforeUsage.storeEncodedBytes,
    );
    expect(
      await store.getPromiseCapacityEvidence(
        promiseCapacityEvidenceKey({ ...record, actorId: "de".repeat(28) }),
      ),
    ).toBeUndefined();
  });
  it("requires durable source authority and rejects quarantined writes without changing an existing observation", async () => {
    const store = await open((await databases.create()).url);
    await expect(store.savePromiseCapacityEvidence(record)).rejects.toThrow(
      "lacks durable L1 source state",
    );
    await store.saveL1SourceState(source);
    await store.savePromiseCapacityEvidence(record);
    await store.saveL1SourceState({
      ...source,
      status: "quarantined",
      quarantineReason: "fork_requires_recovery",
      quarantinedAt: "2026-10-02T00:00:01.000Z",
    });
    await expect(
      store.savePromiseCapacityEvidence(
        {
          ...record,
          certifiedAt: {
            slot: 2261,
            blockNo: 2261,
            blockHash: "9a".repeat(32),
          },
        },
        promiseCapacityPointId(record.point),
      ),
    ).rejects.toThrow("source is quarantined");
    expect(
      await store.getPromiseCapacityEvidence(
        promiseCapacityEvidenceKey(record),
      ),
    ).toEqual(record);
  });
  it("serializes first capture and fences a stale point replacement", async () => {
    const store = await open((await databases.create()).url);
    await store.saveL1SourceState(source);
    const later = {
      ...record,
      point: { slot: 101, blockNo: 101, blockHash: "9a".repeat(32) },
    };
    const captures = await Promise.all([
      store.savePromiseCapacityEvidence(record),
      store.savePromiseCapacityEvidence(later),
    ]);
    expect(captures[0]).toEqual(captures[1]);
    const captured = captures[0]!;
    await expect(
      store.savePromiseCapacityEvidence(later, "stale-point"),
    ).rejects.toThrow("compare-and-set");
    expect(
      await store.getPromiseCapacityEvidence(
        promiseCapacityEvidenceKey(record),
      ),
    ).toEqual(captured);
  });
});
