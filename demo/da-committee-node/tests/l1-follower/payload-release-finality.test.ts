import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  applyChainSyncEvent,
  depth,
  type FactStore,
  type FollowStatus,
  isFinal,
  isSafe,
  openPostgresFactStore,
  openSqliteFactStore,
  stepSettled,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, describe, expect, it } from "vitest";

import {
  CommitteeService,
  retentionReadinessFromDeadlines,
} from "../../src/committee-service.js";
import { committeeL1Source } from "../../src/l1/follower/l1-follower.js";
import type { SlotTime } from "../../src/l1/follower/obligations.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import {
  retentionCycleOptions,
  runRetentionCycle,
} from "../../src/store/retention.js";
import { minimalConfig, tempDir } from "../helpers.js";
import { openTestCommitteeStore } from "../helpers/committee-store.js";
import { postgresTestDatabases } from "../helpers/postgres-database.js";
import { payloadRecord } from "../store-retention.header-record.js";
import { QueueChain } from "./queue-chain.js";
import { SIM_DEPTHS, SIM_QUEUE, SIM_SLOT_TIME } from "./queue-sim.js";

/**
 * §15 C3: releasing a retained payload is irreversible, so it waits for
 * `final` (depth > k). The committee service runs on its L1 follower's
 * facts over the simulator's chain, in both fact-store dialects, and the
 * runtime retention cycle runs on the view each tick accepted.
 *
 * A header is committed, attested and merged within k blocks of the tip,
 * and a later header merges after it as the confirmed head: none of it is
 * final, and the wall clock is long past the first header's
 * challengeability horizon. A rollback at depth cd + 1 then takes the
 * merges back. Nothing may have been released, and the header and payload
 * are still served. Once the merges land again and the chain re-extends
 * past k, the payload is released, exactly once.
 */

const K = SIM_DEPTHS.securityParameter;
const CD = SIM_DEPTHS.confirmationDepth;
const HORIZON_MS = MIDGARD_RETENTION_WINDOW.requiredRetentionMs;
/** One slot spans a whole challengeability horizon, so a few blocks pass it. */
const SLOT_TIME: SlotTime = { ...SIM_SLOT_TIME, slotLength: HORIZON_MS };

const projection = committeeProjection(SIM_QUEUE);
const databases = postgresTestDatabases("midgard_test_c3_payload_release");
afterAll(async () => {
  await databases.dropAll();
}, 120_000);

const openSqlite = async (): Promise<FactStore> =>
  openSqliteFactStore({
    ...simStoreOptions([projection], K, "sqlite"),
    path: ":memory:",
  });

const openPostgres = async (): Promise<FactStore> => {
  const database = await databases.create();
  return openPostgresFactStore({
    ...simStoreOptions([projection], K, "postgres"),
    connection: { connectionString: database.url },
  });
};

const harness = async (factStore: FactStore) => {
  expect(await factStore.start()).toMatchObject({ kind: "ready" });
  expect(
    await factStore.initialize({
      point: SIM_ORIGIN.point,
      height: SIM_ORIGIN.height,
    }),
  ).toMatchObject({ kind: "initialized" });
  const queue = new QueueChain(SLOT_TIME);
  const apply = async (
    event: Parameters<typeof applyChainSyncEvent>[1],
  ): Promise<void> => {
    expect(stepSettled(await applyChainSyncEvent(factStore, event))).toBe(true);
  };
  const dir = await tempDir();
  const config = {
    ...minimalConfig({
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: `${"00".repeat(31)}47`,
      signerPublicKey: "00".repeat(32),
    }),
    finalityDepth: CD,
    automaticRecoveryMaxDepth: K,
  };
  const store = await openTestCommitteeStore();
  const service = new CommitteeService({
    config,
    store,
    l1: committeeL1Source({
      store: factStore,
      parameters: SIM_DEPTHS,
      status: () =>
        ({
          readiness: [],
          cursor: {
            slot: queue.chain.tip.point.slot,
            height: queue.chain.tip.height,
            generation: 0,
          },
        }) as unknown as FollowStatus,
      slotTime: async () => SLOT_TIME,
    }),
    payloadSource: {
      fetchPayloadCandidates: async () => ({
        ok: true,
        candidates: [],
        attempts: [],
      }),
    },
  });
  await service.initialize();
  /** One runtime tick, then one retention cycle on the view it accepted. */
  const cycle = async (): Promise<readonly string[]> => {
    await service.tick();
    const view = service.latestL1View();
    expect(view).toBeDefined();
    const { deadlines, prune } = await runRetentionCycle(
      store,
      retentionCycleOptions(config, view!, Date.now()),
    );
    // Waiting for finality is normal: never a readiness failure.
    expect(retentionReadinessFromDeadlines(deadlines).status).toBe("ok");
    return prune.prunedHeaderHashes;
  };
  return { queue, apply, config, store, service, cycle };
};

describe.each([
  ["SQLite", openSqlite],
  ["Postgres", openPostgres],
] as const)("payload release waits for final (%s)", (_, open) => {
  it(
    "releases nothing through a rollback at depth cd + 1, and releases exactly once after the chain re-extends past k",
    { timeout: 120_000 },
    async () => {
      const factStore = await open();
      try {
        const { queue, apply, config, store, service, cycle } =
          await harness(factStore);
        await apply(queue.init());
        for (let i = 0; i < 3; i += 1) await apply(queue.empty());
        // A is committed, attested and merged, then B merges after it as
        // the confirmed head: A has left every queue the retention
        // exemptions name, all of it within k blocks of the tip. A's merge
        // sits at depth cd + 1: safe, not final.
        await apply(queue.append());
        const header = queue.nodes[0]!;
        const headerHash = header.hash;
        const endTimeMs = Number(header.header.endTime);
        await store.saveDaPayload(
          payloadRecord(headerHash, config.deploymentFingerprint),
        );
        await apply(queue.attest());
        await apply(queue.append());
        await apply(queue.merge());
        await apply(queue.attest());
        await apply(queue.merge());
        const mergeDepth = (): number =>
          depth(queue.chain.tip.height, queue.merged[0]!.mergedAt);
        expect(isSafe(mergeDepth(), SIM_DEPTHS)).toBe(true);
        expect(isFinal(mergeDepth(), SIM_DEPTHS)).toBe(false);
        // The wall clock is long past A's horizon.
        expect(Date.now()).toBeGreaterThan(endTimeMs + HORIZON_MS);
        expect(await cycle()).toEqual([]);
        expect(service.latestL1View()?.liveQueueHeaderHashes).not.toContain(
          headerHash,
        );
        expect(await store.getDaPayload(headerHash)).toBeDefined();

        // The rollback at depth cd + 1 takes A's merge back (and B's): A is
        // queued again, and its record and payload are still there to serve.
        await apply(queue.rollBack(mergeDepth()));
        expect(await cycle()).toEqual([]);
        expect(service.latestL1View()?.liveQueueHeaderHashes).toContain(
          headerHash,
        );
        expect(await store.getDaPayload(headerHash)).toBeDefined();
        expect(await store.getStateQueueHeader(headerHash)).toMatchObject({
          status: "attested",
        });

        // The merges land again; the chain re-extends past k.
        await apply(queue.merge());
        await apply(queue.attest());
        await apply(queue.merge());
        const released: number[] = [];
        // On until the merge is final with 8 blocks to spare.
        const settled = (): boolean =>
          isFinal(
            depth(queue.chain.tip.height, queue.merged[0]!.mergedAt + 8),
            SIM_DEPTHS,
          );
        while (!settled()) {
          await apply(queue.empty());
          const pruned = await cycle();
          if (pruned.includes(headerHash)) {
            expect(isFinal(mergeDepth(), SIM_DEPTHS)).toBe(true);
            released.push(mergeDepth());
          } else if (released.length === 0) {
            expect(await store.getDaPayload(headerHash)).toBeDefined();
          }
        }
        expect(released).toHaveLength(1);
        expect(await store.getDaPayload(headerHash)).toBeUndefined();
        expect(await store.getStateQueueHeader(headerHash)).toMatchObject({
          status: "merged",
        });
      } finally {
        await factStore.close();
      }
    },
  );
});
