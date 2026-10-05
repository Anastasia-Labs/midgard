import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { Client } from "pg";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { type CommitteeStore, JsonFileCommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { retirementStoreRecords } from "../src/store/retirement-transition.js";
import { tempDir } from "./helpers.js";
import {
  descendantPoint,
  expiryPoint,
  horizonMs,
  oldPoint,
  retentionFixture,
} from "./helpers/committee-retirement.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

const databases = postgresTestDatabases("codex_rel_committee_retirement");
const stores: CommitteeStore[] = [];
const journals: ReturnType<typeof retentionFixture> extends Promise<infer T>
  ? T extends { journal: infer J }
    ? J[]
    : never
  : never = [];
afterEach(async () => {
  journals.splice(0).forEach((j) => j.close());
  for (const s of stores.splice(0)) await s.close?.();
});
afterAll(async () => databases.dropAll());
const open = async (backend: "json" | "postgres") => {
  if (backend === "json") {
    const path = join(await tempDir(), "committee.json");
    const store = await JsonFileCommitteeStore.open(path);
    stores.push(store);
    return {
      store,
      path,
      reopen: async () => {
        const s = await JsonFileCommitteeStore.open(path);
        stores.push(s);
        return s;
      },
      url: undefined,
    };
  }
  const db = await databases.create();
  const store = await PostgresCommitteeStore.open(db.url);
  stores.push(store);
  return {
    store,
    path: undefined,
    reopen: async () => {
      const s = await PostgresCommitteeStore.open(db.url);
      stores.push(s);
      return s;
    },
    url: db.url,
  };
};
const setup = async (backend: "json" | "postgres") => {
  const b = await open(backend),
    f = await retentionFixture(b.store);
  journals.push(f.journal);
  return { ...b, ...f };
};

for (const backend of ["json", "postgres"] as const)
  describe(`paired committee retirement ${backend}`, () => {
    it("initializes and charges one bounded singleton before a promise capture; preserves exact retained signature", async () => {
      const f = await setup(backend);
      const s = f.scope();
      await expect(f.source.capture(s)).rejects.toThrow("initialized");
      await f.compact();
      expect((await f.store.getRetirementFloor())?.generation).toBe(1);
      expect((await f.store.promiseStoreResourceUsage()).storeRecords).toBe(3);
      const seeded = await f.seed(1);
      expect((await f.store.listDaSignatures())[0]?.signatureWitness).toBe(
        seeded.signature.signatureWitness,
      );
      const token = await f.source.capture(s);
      await f.source.prove(token, s);
      f.source.assert(token, seeded.header);
      s.close();
    });
    it("keeps bytes for the signed15days and exactly2160 descendants, then atomically removes every cohort family and reopens", async () => {
      const f = await setup(backend);
      await f.compact();
      const seeded = await f.seed(1);
      f.setPoint(
        {
          ...expiryPoint,
          slot: expiryPoint.slot - 1,
          blockHash: "90".repeat(32),
        },
        horizonMs,
      );
      await f.compact();
      expect((await f.store.getRetirementFloor())?.checkpoint).toBeUndefined();
      f.setPoint(expiryPoint);
      await f.compact();
      const provisional = await f.store.getRetirementFloor();
      expect(provisional?.checkpoint?.point).toEqual(expiryPoint);
      f.setPoint({ ...descendantPoint, blockNo: expiryPoint.blockNo + 2160 });
      await f.compact();
      expect(
        await f.store.getDaPayload(seeded.header.headerHash),
      ).toBeDefined();
      f.setPoint(descendantPoint);
      const oldToken = f.store.captureRetirementGuard();
      expect(await f.compact()).toEqual([seeded.header.headerHash]);
      expect(() =>
        f.store.assertRetirementGuard(oldToken, seeded.header),
      ).toThrow("generation");
      const data = (await f.store.readRetirementSnapshot()).data;
      expect(retirementStoreRecords(data)).toBe(3);
      for (const family of [
        "stateQueueHeaders",
        "daPayloads",
        "daSignatures",
        "daConflictEvidence",
        "daAttestationCandidates",
        "l1Submissions",
        "peerBroadcasts",
        "decisionOutbox",
        "promiseCapacityEvidence",
      ] as const)
        expect(Object.keys(data[family]), family).toEqual([]);
      expect(data.chainCursor?.observations).toEqual([]);
      expect(data.retirementFloor).toMatchObject({
        headerEndTimeMs: 1,
        point: expiryPoint,
        certifiedAt: descendantPoint,
        generation: 3,
      });
      expect(data.retirementFloor?.checkpoint).toBeUndefined();
      stores.splice(stores.indexOf(f.store), 1);
      await f.store.close?.();
      const reopened = await f.reopen();
      expect(await reopened.getRetirementFloor()).toEqual(data.retirementFloor);
      await expect(
        reopened.upsertStateQueueHeader(seeded.header),
      ).rejects.toThrow("retired");
      await expect(reopened.saveDaSignature(seeded.signature)).rejects.toThrow(
        "retired",
      );
    });
    it("holds complete prefix cohorts and unknown submitted financial rows; never guesses raw absence", async () => {
      const f = await setup(backend);
      await f.compact();
      const a = await f.seed(1),
        b = await f.seed(2);
      f.pinned.add(a.header.headerHash);
      f.setPoint(expiryPoint);
      await f.compact();
      f.setPoint(descendantPoint);
      expect(await f.compact()).toEqual([]);
      expect(await f.store.listDaSignatures()).toHaveLength(2);
      f.pinned.clear();
      f.setSubmissionReader(async () => null);
      expect(await f.compact()).toEqual([]);
      expect(await f.store.listL1Submissions()).toHaveLength(2);
      f.setSubmissionReader(async (_tx, c) => ({ point: oldPoint, tip: c }));
      f.setRaw({
        stateQueueUtxos: [],
        availabilityUtxos: [],
        correctionLockUtxos: [f.lock],
      });
      await expect(f.compact()).rejects.toThrow();
      f.setRaw({
        stateQueueUtxos: [f.root],
        availabilityUtxos: [],
        correctionLockUtxos: [f.lock],
      });
      expect(await f.compact()).toEqual(
        [a.header.headerHash, b.header.headerHash].sort(),
      );
    });
    it("retains complete equal-end-time cohorts, every latest merged block member, and real SQLite signed-attempt pins", async () => {
      const f = await setup(backend);
      await f.compact();
      const a = await f.seed(1),
        b = await f.seed(1, 1);
      f.setPoint(expiryPoint);
      await f.compact();
      f.setPoint(descendantPoint);
      f.pinned.add(b.header.headerHash);
      expect(await f.compact()).toEqual([]);
      expect(await f.store.listDaSignatures()).toHaveLength(2);
      f.pinned.clear();
      await f.store.upsertStateQueueHeader({ ...a.header, status: "merged" });
      await f.store.upsertStateQueueHeader({ ...b.header, status: "merged" });
      expect(await f.compact()).toEqual([]);
      await f.store.upsertStateQueueHeader(a.header);
      await f.store.upsertStateQueueHeader(b.header);
      const intent = f.persistFinancial(b.header.headerHash);
      expect(await f.compact()).toEqual([]);
      expect(f.journal.get(intent.id)?.intent.signedCbor).toBe(
        intent.signedCbor,
      );
      expect(await f.store.listDaSignatures()).toHaveLength(2);
    });
    it("resumes under unchanged512/8MiB bounds for three all-family fill/retire/resume cycles", async () => {
      const f = await setup(backend);
      await f.compact();
      for (let cycle = 0; cycle < 3; cycle++) {
        const end = (cycle + 1) * 1000000;
        for (let i = 0; i < 63; i++) await f.seed(end + i);
        const full = await f.store.promiseStoreResourceUsage();
        expect(full.storeRecords).toBe(507);
        const checkpoint = {
          slot: 3000000 + cycle * 200000,
          blockNo: 10000 + cycle * 10000,
          blockHash: (70 + cycle).toString(16).repeat(32),
        };
        const tip = {
          slot: checkpoint.slot + 100000,
          blockNo: checkpoint.blockNo + 2161,
          blockHash: (80 + cycle).toString(16).repeat(32),
        };
        f.setPoint(checkpoint, horizonMs + end + 100000);
        await f.compact();
        f.setPoint(tip, horizonMs + end + 200000);
        expect(await f.compact()).toHaveLength(63);
        const usage = await f.store.promiseStoreResourceUsage();
        expect(usage.storeRecords).toBe(3);
        expect(usage.storeEncodedBytes).toBeLessThan(8 * 1024 * 1024);
      }
      expect((await f.store.getRetirementFloor())?.generation).toBe(7);
    }, 180000);
    it("pins actual in-flight callbacks, rejects a stale writer and preserves singleton on failed claims", async () => {
      const f = await setup(backend);
      await f.compact();
      const a = await f.seed(1);
      f.setPoint(expiryPoint);
      await f.compact();
      f.setPoint(descendantPoint);
      let release!: () => void;
      const held = new Promise<void>((r) => {
        release = r;
      });
      let entered!: () => void;
      const ready = new Promise<void>((r) => {
        entered = r;
      });
      const callback = f.store.withRetainedHeaderPin(
        a.header.headerHash,
        async () => {
          entered();
          await held;
        },
      );
      await ready;
      await expect(f.compact()).rejects.toThrow("live callback");
      expect(await f.store.getDaPayload(a.header.headerHash)).toBeDefined();
      release();
      await callback;
      const before = await f.store.getRetirementFloor();
      f.setClaims(false);
      await expect(f.compact()).rejects.toThrow("reconciliation");
      expect(await f.store.getRetirementFloor()).toEqual(before);
      f.setClaims(true);
      const compact = f.compact();
      const writer = f.store.upsertStateQueueHeader(f.header(3));
      const result = await Promise.allSettled([compact, writer]);
      // Ordering can admit the writer before capture, or refuse it when compaction
      // holds the generation; either result must not recreate the retired cohort.
      if (result[0]?.status === "rejected") await f.compact();
      expect(
        await f.store.getStateQueueHeader(a.header.headerHash),
      ).toBeUndefined();
    });
    it("records a sticky exact selected-chain breach with an empty suffix and refuses signing/reimport after restart", async () => {
      const f = await setup(backend);
      await f.compact();
      const a = await f.seed(1);
      f.setPoint(expiryPoint);
      await f.compact();
      f.setPoint(descendantPoint);
      await f.compact();
      f.proofs.delete(`${expiryPoint.slot}:${expiryPoint.blockHash}`);
      f.setPoint({ ...descendantPoint, blockHash: "aa".repeat(32) });
      const s = f.scope();
      await expect(f.source.capture(s)).rejects.toThrow("crossed");
      s.close();
      expect((await f.store.getRetirementFloor())?.breach?.reason).toBe(
        "selected_chain_crossed_retirement_floor",
      );
      stores.splice(stores.indexOf(f.store), 1);
      await f.store.close?.();
      const reopened = await f.reopen();
      expect(() => reopened.captureRetirementGuard()).toThrow("held");
      await expect(reopened.upsertStateQueueHeader(a.header)).rejects.toThrow(
        "held",
      );
    });
    it("uses exact backend bytes/rows including the singleton and never mutates state during retained reads", async () => {
      const f = await setup(backend);
      await f.compact();
      await f.seed(1);
      const before = await f.store.promiseStoreResourceUsage();
      const snapshot = await f.store.readRetirementSnapshot();
      expect(before.storeRecords).toBe(retirementStoreRecords(snapshot.data));
      if (f.path) {
        const bytes = await readFile(f.path);
        expect(before.storeEncodedBytes).toBe(bytes.length);
        await f.store.readRetirementSnapshot();
        expect(await readFile(f.path)).toEqual(bytes);
      }
      if (f.url) {
        const client = new Client({ connectionString: f.url });
        await client.connect();
        try {
          const row = await client.query(
            "SELECT COUNT(*)::int n, SUM(octet_length(record::text))::int b FROM committee_retirement_metadata",
          );
          expect(row.rows[0]?.n).toBe(1);
          expect(row.rows[0]?.b).toBeGreaterThan(0);
        } finally {
          await client.end();
        }
      }
      expect(await f.store.promiseStoreResourceUsage()).toEqual(before);
    });
  });
