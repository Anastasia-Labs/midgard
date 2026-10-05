import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { Client } from "pg";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { checkL1RollbackFeed } from "../src/committee-service.check-l1-rollback-feed.js";
import { CommitteeService } from "../src/committee-service.js";
import { signVerifiedCommitteePayload } from "../src/committee-service.sign-verified-payload.js";
import type { StateQueueProvider } from "../src/l1/state-queue-scanner.js";
import { validateDaSignerMembership } from "../src/signer.js";
import {
  type CommitteeStore,
  JsonFileCommitteeStore,
  retirementMetadataGrowthReserve,
} from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { minimalConfig, tempDir } from "./helpers.js";
import {
  descendantPoint,
  expiryPoint,
  retentionFixture,
} from "./helpers/committee-retirement.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

const databases = postgresTestDatabases(
  "codex_rel_committee_retirement_atomic",
);
const stores: CommitteeStore[] = [],
  journals: Awaited<ReturnType<typeof retentionFixture>>["journal"][] = [];
afterEach(async () => {
  journals.splice(0).forEach((j) => j.close());
  for (const s of stores.splice(0)) await s.close?.();
});
afterAll(async () => databases.dropAll());
const setup = async (backend: "json" | "postgres") => {
  const url =
    backend === "postgres" ? (await databases.create()).url : undefined;
  const path = join(await tempDir(), "committee.json");
  const store = url
    ? await PostgresCommitteeStore.open(url)
    : await JsonFileCommitteeStore.open(path);
  stores.push(store);
  const f = await retentionFixture(store);
  journals.push(f.journal);
  await f.compact();
  const seeded = await f.seed(1);
  f.setPoint(expiryPoint);
  await f.compact();
  f.setPoint(descendantPoint);
  return { ...f, seeded, url, path };
};
for (const backend of ["json", "postgres"] as const)
  describe(`retirement atomic authority ${backend}`, () => {
    it("holds retirement before the actual Service tick's first read and releases on an observation failure", async () => {
      const f = await setup(backend),
        dir = await tempDir(),
        before = await f.store.readRetirementSnapshot();
      const config = minimalConfig({
        dir,
        manifestPath: join(dir, "manifest.json"),
        deploymentInfoPath: join(dir, "deployment.json"),
        signerSeed: "00".repeat(31) + "01",
        signerPublicKey: f.signer.publicKeyHex,
      });
      let entered!: () => void, release!: () => void;
      const ready = new Promise<void>((r) => {
          entered = r;
        }),
        paused = new Promise<void>((r) => {
          release = r;
        });
      const read = vi
        .spyOn(f.store, "getL1SourceState")
        .mockImplementationOnce(async () => {
          entered();
          await paused;
          throw new Error("fabricated observation refusal");
        });
      const service = new CommitteeService({
        config,
        store: f.store,
        stateQueueProvider: { fetchStateQueueNodes: async () => [] },
        payloadSource: {
          fetchPayloadCandidates: async () => ({ ok: false, attempts: [] }),
        },
        writeEvent: () => {},
      });
      const tick = service.tick();
      const rejected = expect(tick).rejects.toThrow(
        "fabricated observation refusal",
      );
      await ready;
      expect(await f.compact()).toEqual([]);
      expect((await f.store.readRetirementSnapshot()).digest).toBe(
        before.digest,
      );
      release();
      await rejected;
      read.mockRestore();
      expect(f.store.retirementDiscoveryActive()).toBe(false);
      expect(await f.compact()).toEqual([f.seeded.header.headerHash]);
    });
    it("refuses a fresh signature after retirement changes generation during the final admission await", async () => {
      const f = await setup(backend),
        dir = await tempDir(),
        base = minimalConfig({
          dir,
          manifestPath: join(dir, "manifest.json"),
          deploymentInfoPath: join(dir, "deployment.json"),
          signerSeed: "00".repeat(31) + "01",
          signerPublicKey: f.signer.publicKeyHex,
        });
      const config = {
        ...base,
        deploymentFingerprint: f.binding.deploymentFingerprint,
        hubOraclePolicyId: f.deployment.hubOraclePolicyId,
        daParams: {
          ...base.daParams,
          committeeSignersHash: f.binding.committeeSignersHash,
        },
      };
      const signerValidation = validateDaSignerMembership({
          daParams: config.daParams,
          signer: f.signer,
          signerIndex: 0,
        }),
        observe = vi.fn();
      const deps = {
        config,
        store: f.store,
        stateQueueProvider: { fetchStateQueueNodes: async () => [] },
        payloadSource: {
          fetchPayloadCandidates: async () => ({
            ok: false as const,
            attempts: [],
          }),
        },
        signer: f.signer,
        signerValidation,
        promiseAdmission: {
          policyStatus: () => ({
            status: "unavailable" as const,
            reason: "controlled final signing fence",
          }),
          check: async () => ({
            decision: {
              status: "admitted" as const,
              deploymentId: f.binding.deploymentFingerprint,
              reason: "controlled finite gate",
            },
            assertCurrent: async () => {
              await f.compact();
            },
          }),
        },
      };
      await expect(
        signVerifiedCommitteePayload(
          deps,
          f.seeded.header,
          f.seeded.verified,
          observe,
        ),
      ).rejects.toThrow("generation");
      expect(observe).not.toHaveBeenCalled();
      const next = await f.seed(2);
      const positive = {
        ...deps,
        promiseAdmission: {
          policyStatus: () => ({
            status: "unavailable" as const,
            reason: "controlled final signing fence",
          }),
          check: async () => ({
            decision: {
              status: "admitted" as const,
              deploymentId: f.binding.deploymentFingerprint,
              reason: "controlled finite gate",
            },
            assertCurrent: async () => {},
          }),
        },
      };
      expect(
        (
          await signVerifiedCommitteePayload(
            positive,
            next.header,
            next.verified,
            observe,
          )
        )?.signature.signatureWitness,
      ).toBe(next.signature.signatureWitness);
    });
    it("reserves bounded singleton growth separately from exact physical backend accounting", async () => {
      const f = await setup(backend),
        before = await f.store.promiseStoreResourceUsage(),
        floor = (await f.store.getRetirementFloor())!;
      const reserve = retirementMetadataGrowthReserve(floor);
      expect(reserve).toBeGreaterThan(3000);
      await f.store.recordRetirementBreach("\u0001".repeat(512), {
        slot: Number.MAX_SAFE_INTEGER,
        blockHash: "ee".repeat(32),
        blockNo: Number.MAX_SAFE_INTEGER,
      });
      const after = await f.store.promiseStoreResourceUsage();
      expect(after.storeRecords).toBe(before.storeRecords);
      expect(after.storeEncodedBytes).toBeGreaterThan(before.storeEncodedBytes);
      expect(after.storeEncodedBytes).toBeLessThanOrEqual(
        before.storeEncodedBytes + reserve,
      );
    });
    it("keeps all rows and the exact generation while discovery is active, then resumes immediately", async () => {
      const f = await setup(backend),
        before = await f.store.readRetirementSnapshot();
      let release!: () => void, entered!: () => void;
      const ready = new Promise<void>((r) => {
          entered = r;
        }),
        pending = new Promise<void>((r) => {
          release = r;
        });
      const scan = f.store.withRetirementDiscovery(async () => {
        entered();
        await pending;
      });
      await ready;
      expect(f.store.retirementDiscoveryActive()).toBe(true);
      expect(await f.compact()).toEqual([]);
      expect((await f.store.readRetirementSnapshot()).digest).toBe(
        before.digest,
      );
      release();
      await scan;
      expect(f.store.retirementDiscoveryActive()).toBe(false);
      expect(await f.compact()).toEqual([f.seeded.header.headerHash]);
    });
    it("cannot use the legacy payload-only sweeper or import a late peer row below the paired floor", async () => {
      const f = await setup(backend);
      expect(
        await f.store.deleteDaPayloadIfPrunable({
          headerHash: f.seeded.header.headerHash,
          nowMs: Number.MAX_SAFE_INTEGER,
          confirmedHeadHash: "00".repeat(32),
          liveQueueHeaderHashes: new Set(),
          automaticRecoveryMaxDepth: 2160,
          retentionDays: 15,
        }),
      ).toBe(false);
      await f.compact();
      await expect(
        f.store.savePeerBroadcast({
          deploymentFingerprint: f.binding.deploymentFingerprint,
          peerId: "peer1",
          headerHash: f.seeded.header.headerHash,
          availabilityCommitmentDigest:
            f.seeded.signature.availabilityCommitmentDigest,
          signerIndex: 0,
          status: "posted",
          attempts: 1,
          updatedAt: "2026-10-02T00:00:00.000Z",
        }),
      ).rejects.toThrow("retired");
      expect(
        await f.store.getDaPayload(f.seeded.header.headerHash),
      ).toBeUndefined();
    });
    it("detects below-P rollback even with no retained header or decision, and durably holds the exact native point", async () => {
      const f = await setup(backend);
      await f.compact();
      const point = (slot: number, blockHash: string) => ({
        slot,
        blockHash,
        network: "Preprod",
        providerSource: "chain-sync:test",
        observedAt: "2026-10-02T00:00:00.000Z",
      });
      const consumed = {
        sequence: 0,
        point: point(expiryPoint.slot, expiryPoint.blockHash),
        rollbackGeneration: 0,
      };
      const rollback = point(expiryPoint.slot - 1, "dd".repeat(32)),
        current = { sequence: 1, point: rollback, rollbackGeneration: 1 };
      const provider = {
        fetchStateQueueNodes: async () => [],
        currentChainSyncCursor: async () => current,
        loadConsumedChainSyncCursor: async () => consumed,
        replayChainSyncEvents: async () => [
          { direction: "roll_backward", point: rollback },
        ],
        acknowledgeChainSyncCursor: async () => ({
          rollbackSinceCapture: false,
        }),
      } as unknown as StateQueueProvider;
      const source = await f.store.getL1SourceState();
      expect(source?.observations).toEqual([]);
      const check = await checkL1RollbackFeed(
        source,
        provider,
        current,
        f.store,
      );
      expect(check.failure).toBe(
        `l1_source_retirement_floor_crossed:${rollback.slot}:${rollback.blockHash}`,
      );
      expect((await f.store.getRetirementFloor())?.breach?.observedAt).toEqual({
        slot: rollback.slot,
        blockHash: rollback.blockHash,
      });
      expect(() => f.store.captureRetirementGuard()).toThrow("held");
    });
  });

it("rolls back every PostgreSQL cohort deletion and floor/source update when the floor write fails, then retries", async () => {
  const f = await setup("postgres"),
    before = await f.store.readRetirementSnapshot();
  const client = new Client({ connectionString: f.url });
  await client.connect();
  try {
    await client.query(
      "CREATE FUNCTION refuse_test_floor() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'isolated retirement crash boundary'; END $$",
    );
    await client.query(
      "CREATE TRIGGER refuse_test_floor BEFORE INSERT OR UPDATE ON committee_retirement_metadata FOR EACH ROW EXECUTE FUNCTION refuse_test_floor()",
    );
    await expect(f.compact()).rejects.toThrow(
      "isolated retirement crash boundary",
    );
    expect((await f.store.readRetirementSnapshot()).digest).toBe(before.digest);
    expect((await f.store.listDaSignatures())[0]?.signatureWitness).toBe(
      f.seeded.signature.signatureWitness,
    );
    await client.query(
      "DROP TRIGGER refuse_test_floor ON committee_retirement_metadata",
    );
    await client.query("DROP FUNCTION refuse_test_floor()");
    expect(await f.compact()).toEqual([f.seeded.header.headerHash]);
    expect((await f.store.getRetirementFloor())?.generation).toBe(3);
  } finally {
    await client.end();
  }
});

for (const boundary of ["before-rename", "after-rename"] as const)
  it(`reopens an entire compiled JSON store after owned child death ${boundary}`, async () => {
    const f = await setup("json"),
      before = await readFile(f.path, "utf8");
    stores.splice(stores.indexOf(f.store), 1);
    await f.store.close?.();
    const child = spawn(
      process.execPath,
      [
        fileURLToPath(
          new URL("./fixtures/committee-retirement-crash.mjs", import.meta.url),
        ),
        f.path,
        boundary,
      ],
      { stdio: ["ignore", "ignore", "pipe"] },
    );
    let error = "";
    child.stderr.on("data", (chunk) => {
      error += String(chunk);
    });
    const exit = await new Promise<{
      code: number | null;
      signal: NodeJS.Signals | null;
    }>((resolve, reject) => {
      child.once("error", reject);
      child.once("exit", (code, signal) => resolve({ code, signal }));
    });
    expect(exit.signal, error).toBe("SIGKILL");
    const after = await readFile(f.path, "utf8"),
      old = JSON.parse(before) as Record<string, unknown>,
      next = JSON.parse(after) as Record<string, unknown>;
    if (boundary === "before-rename") expect(after).toBe(before);
    else
      expect(next.retirementFloor).toMatchObject({
        generation: 3,
        breach: { reason: "controlled native rollback" },
      });
    delete old.retirementFloor;
    delete next.retirementFloor;
    expect(next).toEqual(old);
    const reopened = await JsonFileCommitteeStore.open(f.path);
    stores.push(reopened);
    expect((await reopened.listDaSignatures())[0]?.signatureWitness).toBe(
      f.seeded.signature.signatureWitness,
    );
    if (boundary === "after-rename")
      expect(() => reopened.captureRetirementGuard()).toThrow("held");
    else expect(() => reopened.captureRetirementGuard()).not.toThrow();
  }, 10000);
