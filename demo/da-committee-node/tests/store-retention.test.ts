import { randomBytes } from "node:crypto";

import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Client } from "pg";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { LIBP2P_DA_MIN_RETENTION_DAYS } from "../src/config.js";
import { assertLibp2pDaRetentionDays } from "../src/config.js";
import type {
  DaPayloadRecord,
  StateQueueHeaderRecord,
  StateQueueHeaderStatus,
} from "../src/domain.js";
import {
  hashBlockHeader,
  scanStateQueue,
} from "../src/l1/state-queue-scanner.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import {
  pruneExpiredDaPayloads,
  retentionCandidates,
  retentionDeadlineReport,
  runRetentionCycle,
} from "../src/store/retention.js";
import {
  fixtureHeaderBase,
  makeObservedNode,
  makePayloadFixture,
  tempDir,
} from "./helpers.js";

const FINGERPRINT = "cd".repeat(32);
const NOW = Date.UTC(2026, 7, 3);
const REQUIRED_RETENTION_MS = MIDGARD_RETENTION_WINDOW.requiredRetentionMs;
const hashOf = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(28);
const retentionOptions = (nowMs = NOW) => ({
  nowMs,
  deploymentFingerprint: FINGERPRINT,
  minimumFinalityDepth: 30,
  confirmedHeadHash: hashOf(200),
  liveQueueHeaderHashes: new Set([hashOf(201), hashOf(202)]),
});

const openStores = new Set<CommitteeStore>();

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close?.()));
  openStores.clear();
});

const openStore = async (): Promise<JsonFileCommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(await tempDir());
  openStores.add(store);
  return store;
};

const postgresAdmin = {
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  user: process.env.POSTGRES_USER ?? "postgres",
  password: process.env.POSTGRES_PASSWORD ?? "postgres",
};
const postgresDatabases: string[] = [];

/**
 * A committee store on a fresh database of the workspace test cluster
 * (`scripts/start-test-postgres.sh`); fails closed when none is reachable.
 */
const openPostgresStore = async (): Promise<PostgresCommitteeStore> => {
  const databaseName = `committee_retention_${randomBytes(6).toString("hex")}`;
  const admin = new Client({
    ...postgresAdmin,
    database: process.env.POSTGRES_DB ?? "postgres",
  });
  await admin.connect();
  try {
    await admin.query(`CREATE DATABASE ${databaseName}`);
  } finally {
    await admin.end();
  }
  postgresDatabases.push(databaseName);
  const store = await PostgresCommitteeStore.open(
    `postgresql://${postgresAdmin.user}:${postgresAdmin.password}@${postgresAdmin.host}:${postgresAdmin.port.toString()}/${databaseName}`,
  );
  openStores.add(store);
  return store;
};

afterAll(async () => {
  if (postgresDatabases.length === 0) return;
  const admin = new Client({
    ...postgresAdmin,
    database: process.env.POSTGRES_DB ?? "postgres",
  });
  await admin.connect();
  try {
    for (const databaseName of postgresDatabases) {
      await admin.query(`DROP DATABASE IF EXISTS ${databaseName}`);
    }
  } finally {
    await admin.end();
  }
});

const headerRecord = (
  headerHash: string,
  endTimeMs: number | bigint,
  status: StateQueueHeaderStatus,
): StateQueueHeaderRecord => ({
  deploymentFingerprint: FINGERPRINT,
  headerHash,
  stateQueueOutRef: `${"11".repeat(32)}#0`,
  blockAssetName: headerHash,
  header: {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    endTime: typeof endTimeMs === "bigint" ? endTimeMs : BigInt(endTimeMs),
  },
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint:
    status === "merged" || status === "removed"
      ? {
          slot: 100,
          blockHash: "12".repeat(32),
          blockHeight: 90,
          depth: 30,
          finalized: true,
          providerSource: "authenticated_state_queue_transition_v1",
        }
      : { finalized: true },
  finalized: true,
  status,
  validationErrors: [],
  updatedAt: new Date(NOW).toISOString(),
});

const payloadRecord = (
  headerHash: string,
  deploymentFingerprint = FINGERPRINT,
  fetchedAtMs = NOW,
): DaPayloadRecord => ({
  deploymentFingerprint,
  headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: "80",
  payloadSha256: "ef".repeat(32),
  sourcePeerId: "peer-1",
  fetchedAt: new Date(fetchedAtMs).toISOString(),
  validationStatus: "verified",
});

const seed = async (
  store: CommitteeStore,
  entries: readonly {
    readonly headerHash: string;
    readonly endTimeMs?: number | bigint;
    readonly status?: StateQueueHeaderStatus;
    readonly deploymentFingerprint?: string;
    readonly withoutHeader?: boolean;
    readonly fetchedAtMs?: number;
  }[],
): Promise<void> => {
  for (const entry of entries) {
    await store.saveDaPayload(
      payloadRecord(
        entry.headerHash,
        entry.deploymentFingerprint,
        entry.fetchedAtMs,
      ),
    );
    if (entry.withoutHeader === true) {
      continue;
    }
    await store.upsertStateQueueHeader(
      headerRecord(
        entry.headerHash,
        entry.endTimeMs ?? NOW,
        entry.status ?? "attested",
      ),
    );
  }
};

const HEAD = hashOf(200);
const LIVE_A = hashOf(201);
const LIVE_B = hashOf(202);
const PAST_HORIZON = NOW - REQUIRED_RETENTION_MS - 1;

describe("retentionCandidatesV1", () => {
  it("retains the L1 confirmed head's payload however old or removed", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: HEAD,
        endTimeMs: NOW - 60 * RETENTION_MS_PER_DAY,
        status: "removed",
      },
    ]);
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate).toMatchObject({
      queueReference: "confirmed_head",
      decision: { decision: "retain", reasonCode: "confirmed_head_payload" },
    });
  });

  it("retains headers live in the L1 queue past the horizon", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: LIVE_A, endTimeMs: PAST_HORIZON, status: "attested" },
      { headerHash: LIVE_B, withoutHeader: true, fetchedAtMs: PAST_HORIZON },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates.map((candidate) => candidate.decision)).toEqual([
      expect.objectContaining({
        decision: "retain",
        reasonCode: "live_queue_header",
      }),
      expect.objectContaining({
        decision: "retain",
        reasonCode: "live_queue_header",
      }),
    ]);
  });

  it("prunes a removed header immediately, inside the horizon", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(1), status: "removed" }]);
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate?.decision).toMatchObject({
      decision: "prune",
      reasonCode: "removed_header",
    });
  });

  it("prunes any status once the horizon has strictly passed, not at it", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(2), endTimeMs: PAST_HORIZON, status: "merged" },
      { headerHash: hashOf(3), endTimeMs: PAST_HORIZON, status: "attested" },
      { headerHash: hashOf(4), endTimeMs: PAST_HORIZON, status: "conflicted" },
      {
        headerHash: hashOf(5),
        endTimeMs: NOW - REQUIRED_RETENTION_MS,
        status: "merged",
      },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(
      candidates.map((candidate) => candidate.decision.reasonCode),
    ).toEqual([
      "past_challengeability_horizon",
      "past_challengeability_horizon",
      "past_challengeability_horizon",
      "still_challengeable",
    ]);
  });

  it("decides a payload with no header row on its receipt time", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(6), withoutHeader: true, fetchedAtMs: NOW },
      { headerHash: hashOf(7), withoutHeader: true, fetchedAtMs: PAST_HORIZON },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates).toMatchObject([
      {
        headerStatus: "unobserved",
        blockEndTimeMs: NOW,
        decision: { decision: "retain", reasonCode: "still_challengeable" },
      },
      {
        headerStatus: "unobserved",
        blockEndTimeMs: PAST_HORIZON,
        decision: {
          decision: "prune",
          reasonCode: "past_challengeability_horizon",
        },
      },
    ]);
  });

  it("prunes an unexempt payload whose receipt time cannot be parsed", async () => {
    const store = await openStore();
    for (const headerHash of [hashOf(8), LIVE_A]) {
      await store.saveDaPayload({
        ...payloadRecord(headerHash),
        fetchedAt: "not-a-timestamp",
      });
    }
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates).toMatchObject([
      {
        headerHash: hashOf(8),
        blockEndTimeMs: 0,
        decision: {
          decision: "prune",
          reasonCode: "past_challengeability_horizon",
        },
      },
      {
        headerHash: LIVE_A,
        decision: { decision: "retain", reasonCode: "live_queue_header" },
      },
    ]);
  });

  it("keeps deployment and terminal-history mismatches as diagnostics only", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(8),
        endTimeMs: PAST_HORIZON,
        status: "merged",
        deploymentFingerprint: "ee".repeat(32),
      },
    ]);
    const [candidate] = await retentionCandidates(store, {
      ...retentionOptions(),
      minimumFinalityDepth: 31,
    });
    expect(candidate).toMatchObject({
      fingerprintMismatch: true,
      terminalHistoryAuthorityMismatch: true,
      decision: { decision: "prune" },
    });
  });
});

describe("pruneExpiredDaPayloadsV1", () => {
  it("deletes exactly the prunable payloads and keeps the exempt ones", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: HEAD, endTimeMs: PAST_HORIZON, status: "merged" },
      { headerHash: LIVE_A, endTimeMs: PAST_HORIZON, status: "attested" },
      { headerHash: hashOf(10), endTimeMs: PAST_HORIZON, status: "merged" },
      { headerHash: hashOf(11), status: "removed" },
      { headerHash: hashOf(12), status: "attested" },
    ]);
    const result = await pruneExpiredDaPayloads(store, retentionOptions());
    expect(result).toEqual({
      scanned: 5,
      prunedHeaderHashes: [hashOf(10), hashOf(11)],
      retained: 3,
    });
    expect(
      (await store.listDaPayloads()).map(({ headerHash }) => headerHash),
    ).toEqual([hashOf(12), HEAD, LIVE_A].sort());
  });

  it.each([
    ["the JSON file store", openStore],
    ["the Postgres store", openPostgresStore],
  ] as const)(
    "re-decides inside %s write boundary against the caller's view",
    async (_label, open) => {
      const store = await open();
      await seed(store, [
        { headerHash: hashOf(13), endTimeMs: PAST_HORIZON, status: "merged" },
      ]);
      const request = {
        headerHash: hashOf(13),
        nowMs: NOW,
        confirmedHeadHash: HEAD,
        liveQueueHeaderHashes: new Set<string>(),
      };
      expect(
        await store.deleteDaPayloadIfPrunable({
          ...request,
          confirmedHeadHash: hashOf(13),
        }),
      ).toBe(false);
      expect(
        await store.deleteDaPayloadIfPrunable({
          ...request,
          liveQueueHeaderHashes: new Set([hashOf(13)]),
        }),
      ).toBe(false);
      expect(
        await store.deleteDaPayloadIfPrunable({
          ...request,
          nowMs: NOW - 2 * RETENTION_MS_PER_DAY,
        }),
      ).toBe(false);
      expect(await store.getDaPayload(hashOf(13))).toBeDefined();
      expect(await store.deleteDaPayloadIfPrunable(request)).toBe(true);
      expect(await store.getDaPayload(hashOf(13))).toBeUndefined();
      expect(await store.deleteDaPayloadIfPrunable(request)).toBe(false);
    },
  );

  it("bounds the retained set by the head, the live queue, and the horizon", async () => {
    const store = await openStore();
    const statuses: readonly StateQueueHeaderStatus[] = [
      "unattested",
      "attesting",
      "attested",
      "merged",
      "removed",
      "conflicted",
    ];
    const entries = Array.from({ length: 60 }, (_, index) => ({
      headerHash: hashOf(20 + index),
      endTimeMs: NOW - index * RETENTION_MS_PER_DAY,
      status: statuses[index % statuses.length]!,
      withoutHeader: index % 7 === 0,
      fetchedAtMs: NOW - index * RETENTION_MS_PER_DAY,
    }));
    await seed(store, entries);
    const view = {
      confirmedHeadHash: hashOf(79),
      liveQueueHeaderHashes: new Set([hashOf(78), hashOf(77)]),
    };
    await pruneExpiredDaPayloads(store, { ...retentionOptions(), ...view });
    const retained = (await store.listDaPayloads()).map(
      ({ headerHash }) => headerHash,
    );
    const insideHorizon = entries.filter(
      (entry) =>
        NOW <= entry.endTimeMs + REQUIRED_RETENTION_MS &&
        (entry.withoutHeader || entry.status !== "removed"),
    );
    expect(retained.length).toBeLessThanOrEqual(1 + 2 + insideHorizon.length);
    for (const headerHash of retained) {
      expect(
        headerHash === view.confirmedHeadHash ||
          view.liveQueueHeaderHashes.has(headerHash) ||
          insideHorizon.some((entry) => entry.headerHash === headerHash),
      ).toBe(true);
    }
    expect(retained).toEqual(
      expect.arrayContaining([hashOf(79), hashOf(78), hashOf(77)]),
    );
  });
});

describe("retentionDeadlineReportV1", () => {
  it("reports derived window arithmetic and alerts on burned headroom", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(40),
        endTimeMs:
          NOW - REQUIRED_RETENTION_MS + MIDGARD_RETENTION_WINDOW.marginMs,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.requiredRetentionMs).toBe(907_200_000);
    expect(report.deployedRetentionMs).toBe(1_296_000_000);
    expect(report.marginMs).toBe(388_800_000);
    expect(report.alertThresholdMs).toBe(388_800_000);
    expect(report.entries[0]).toMatchObject({ headroomMs: 0, alerting: true });
    expect(report.alerting).toBe(1);
  });

  it("does not alert one millisecond above the threshold", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(41),
        endTimeMs:
          NOW - REQUIRED_RETENTION_MS + MIDGARD_RETENTION_WINDOW.marginMs + 1,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.entries[0]).toMatchObject({ headroomMs: 1, alerting: false });
    expect(report.alerting).toBe(0);
  });

  it("computes a deadline for a payload with no header row from its receipt time", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(42), withoutHeader: true }]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.entries[0]).toEqual({
      headerHash: hashOf(42),
      reasonCode: "still_challengeable",
      challengeableUntilMs: NOW + REQUIRED_RETENTION_MS,
      remainingMs: REQUIRED_RETENTION_MS,
      headroomMs: REQUIRED_RETENTION_MS - MIDGARD_RETENTION_WINDOW.marginMs,
      alerting: false,
    });
  });

  it("rejects malformed alert thresholds", async () => {
    const store = await openStore();
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      await expect(
        retentionDeadlineReport(store, {
          ...retentionOptions(),
          alertThresholdMs: bad,
        }),
      ).rejects.toThrow(/alertThresholdMs/u);
    }
  });
});

describe("runRetentionCycleV1", () => {
  it("reports and deletes a removed header in one cycle", async () => {
    const store = await openStore();
    const headerHash = hashOf(43);
    await seed(store, [{ headerHash, status: "removed" }]);

    const cycle = await runRetentionCycle(store, retentionOptions());
    expect(cycle.deadlines).toMatchObject({
      scanned: 1,
      retained: 0,
      prunable: 1,
      alerting: 0,
    });
    expect(cycle.prune).toEqual({
      scanned: 1,
      prunedHeaderHashes: [headerHash],
      retained: 0,
    });
    expect(await store.getDaPayload(headerHash)).toBeUndefined();
  });

  it("never deletes a concurrent payload absent from the preceding report", async () => {
    const store = await openStore();
    const reported = hashOf(44);
    const concurrent = hashOf(45);
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    await seed(store, [
      { headerHash: reported, endTimeMs: expired, status: "merged" },
    ]);
    let injected = false;
    const wrapped = new Proxy(store, {
      get(target, property, receiver) {
        if (property === "listStateQueueHeaders") {
          return async () => {
            if (!injected) {
              injected = true;
              await seed(store, [
                {
                  headerHash: concurrent,
                  endTimeMs: expired,
                  status: "removed",
                },
              ]);
            }
            return store.listStateQueueHeaders();
          };
        }
        const value = Reflect.get(target, property, receiver) as unknown;
        return typeof value === "function" ? value.bind(target) : value;
      },
    });

    const cycle = await runRetentionCycle(wrapped, retentionOptions());
    expect(cycle.deadlines.entries.map(({ headerHash }) => headerHash)).toEqual(
      [reported],
    );
    expect(cycle.prune.prunedHeaderHashes).toEqual([reported]);
    expect(await store.getDaPayload(concurrent)).toBeDefined();
  });
});

describe("state-queue scanner L1 view", () => {
  it("hands the confirmed head and every live queue header to the poller", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({ header, headerHash });
    const views: unknown[] = [];
    await scanStateQueue(
      {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => ({
          nodes: [node],
          confirmedHeaderHash: "AB".repeat(28),
          confirmedStateOutRef: `${"66".repeat(32)}#0`,
          observedChainPoint: { ...node.chainPoint, depth: 30 },
        }),
      },
      {
        deploymentFingerprint: FINGERPRINT,
        deploymentIdentityDigest: FINGERPRINT,
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 30,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        recordL1View: (view) => views.push(view),
      },
    );
    expect(views).toEqual([
      {
        confirmedHeaderHash: "ab".repeat(28),
        liveQueueHeaderHashes: [hashBlockHeader(header)],
      },
    ]);
  });

  it("keeps a conflicted queue node in the live set", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({
      header,
      headerHash,
      assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"44".repeat(28)}`,
    });
    const views: unknown[] = [];
    const records = await scanStateQueue(
      {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => ({
          nodes: [node],
          confirmedHeaderHash: "AB".repeat(28),
          confirmedStateOutRef: `${"66".repeat(32)}#0`,
          observedChainPoint: { ...node.chainPoint, depth: 30 },
        }),
      },
      {
        deploymentFingerprint: FINGERPRINT,
        deploymentIdentityDigest: FINGERPRINT,
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 30,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        recordL1View: (view) => views.push(view),
      },
    );
    expect(records.map(({ status }) => status)).toEqual(["conflicted"]);
    expect(views).toEqual([
      {
        confirmedHeaderHash: "ab".repeat(28),
        liveQueueHeaderHashes: [hashBlockHeader(header)],
      },
    ]);
  });

  it("reports no view when the provider cannot supply a full snapshot", async () => {
    const views: unknown[] = [];
    await scanStateQueue(
      { fetchStateQueueNodes: async () => [] },
      {
        deploymentFingerprint: FINGERPRINT,
        deploymentIdentityDigest: FINGERPRINT,
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 30,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        recordL1View: (view) => views.push(view),
      },
    );
    expect(views).toEqual([]);
  });
});

describe("assertLibp2pDaRetentionDaysV1", () => {
  it("accepts the canonical 15-day window matching the manifest", () => {
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
        manifestRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
      }),
    ).toBe(15);
    expect(LIBP2P_DA_MIN_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW.retentionDays,
    );
  });

  it("rejects 14 days and accepts 15 at the boundary", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 14,
        manifestRetentionDays: 14,
      }),
    ).toThrow(/must be at least 15 days/u);
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 15,
        manifestRetentionDays: 15,
      }),
    ).toBe(15);
  });

  it("rejects a runtime window that differs from the manifest window", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 16,
        manifestRetentionDays: 15,
      }),
    ).toThrow(/must exactly equal the verified deployment manifest/u);
  });

  it("rejects malformed runtime retention days", () => {
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        assertLibp2pDaRetentionDays({
          runtimeRetentionDays: bad,
          manifestRetentionDays: 15,
        }),
      ).toThrow(/da_transport\.retention_days/u);
    }
  });
});
