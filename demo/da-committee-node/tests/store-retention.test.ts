import {
  MIDGARD_RETENTION_WINDOW_V1,
  RETENTION_MS_PER_DAY_V1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import { LIBP2P_DA_MIN_RETENTION_DAYS } from "../src/config.js";
import { assertLibp2pDaRetentionDaysV1 } from "../src/config.js";
import type {
  DaPayloadRecord,
  StateQueueHeaderRecord,
  StateQueueHeaderStatus,
} from "../src/domain.js";
import { JsonFileWatcherStore } from "../src/store.js";
import {
  pruneExpiredDaPayloadsV1,
  retentionCandidatesV1,
  retentionDeadlineReportV1,
} from "../src/store/retention.js";
import { fixtureHeaderBase, tempDir } from "./helpers.js";

const FINGERPRINT = "cd".repeat(32);
const NOW = Date.UTC(2026, 7, 3);
const REQUIRED_RETENTION_MS = MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs;

const openStores = new Set<JsonFileWatcherStore>();

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close()));
  openStores.clear();
});

const openStore = async (): Promise<JsonFileWatcherStore> => {
  const store = await JsonFileWatcherStore.open(await tempDir());
  openStores.add(store);
  return store;
};

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
  observedChainPoint: { finalized: true },
  finalized: true,
  status,
  validationErrors: [],
  updatedAt: new Date(NOW).toISOString(),
});

const payloadRecord = (
  headerHash: string,
  deploymentFingerprint = FINGERPRINT,
): DaPayloadRecord => ({
  deploymentFingerprint,
  headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: "80",
  payloadSha256: "ef".repeat(32),
  sourcePeerId: "peer-1",
  fetchedAt: new Date(NOW).toISOString(),
  validationStatus: "verified",
});

const hashOf = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(28);

const seed = async (
  store: JsonFileWatcherStore,
  entries: readonly {
    readonly headerHash: string;
    readonly endTimeMs?: number | bigint;
    readonly status?: StateQueueHeaderStatus;
    readonly deploymentFingerprint?: string;
    readonly withoutHeader?: boolean;
  }[],
): Promise<void> => {
  for (const entry of entries) {
    await store.saveDaPayload(
      payloadRecord(entry.headerHash, entry.deploymentFingerprint),
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

describe("retentionCandidatesV1", () => {
  it("joins payloads to headers and prunes only expired terminal records", async () => {
    const store = await openStore();
    const expired = NOW - 16 * RETENTION_MS_PER_DAY_V1;
    await seed(store, [
      { headerHash: hashOf(1), endTimeMs: expired, status: "merged" },
      { headerHash: hashOf(2), endTimeMs: expired, status: "removed" },
      { headerHash: hashOf(3), endTimeMs: expired, status: "attested" },
      { headerHash: hashOf(4), endTimeMs: NOW, status: "merged" },
    ]);

    const candidates = await retentionCandidatesV1(store, { nowMs: NOW });
    expect(candidates).toHaveLength(4);
    expect(
      candidates.map((candidate) => [
        candidate.headerHash,
        candidate.decision.decision,
        candidate.decision.reasonCode,
      ]),
    ).toEqual([
      [hashOf(1), "prune", "expired_and_terminal"],
      [hashOf(2), "prune", "expired_and_terminal"],
      [hashOf(3), "retain", "header_status_not_terminal"],
      [hashOf(4), "retain", "still_within_maturity"],
    ]);
  });

  it("retains a payload with no header row (fail closed)", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(5), withoutHeader: true }]);
    const [candidate] = await retentionCandidatesV1(store, { nowMs: NOW });
    expect(candidate).toMatchObject({
      headerPresent: false,
      headerStatus: undefined,
      blockEndTimeMs: null,
    });
    expect(candidate?.decision).toEqual({
      decision: "retain",
      reasonCode: "missing_block_end_time",
    });
  });

  it("retains a payload written under a foreign deployment fingerprint", async () => {
    const store = await openStore();
    const expired = NOW - 16 * RETENTION_MS_PER_DAY_V1;
    await seed(store, [
      {
        headerHash: hashOf(6),
        endTimeMs: expired,
        status: "merged",
        deploymentFingerprint: "ab".repeat(32),
      },
    ]);
    const [candidate] = await retentionCandidatesV1(store, {
      nowMs: NOW,
      deploymentFingerprint: FINGERPRINT,
    });
    expect(candidate?.fingerprintMismatch).toBe(true);
    expect(candidate?.decision.decision).toBe("retain");
  });

  it("retains malformed or unusable block end times", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(7), endTimeMs: -1n, status: "merged" },
      {
        headerHash: hashOf(8),
        endTimeMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
        status: "merged",
      },
    ]);
    const candidates = await retentionCandidatesV1(store, { nowMs: NOW });
    for (const candidate of candidates) {
      expect(candidate.blockEndTimeMs).toBeNull();
      expect(candidate.decision).toEqual({
        decision: "retain",
        reasonCode: "missing_block_end_time",
      });
    }
  });

  it("retains at the exact challengeability deadline and prunes 1ms past it", async () => {
    const store = await openStore();
    const endTime = NOW - REQUIRED_RETENTION_MS;
    await seed(store, [
      { headerHash: hashOf(9), endTimeMs: endTime, status: "merged" },
      { headerHash: hashOf(10), endTimeMs: endTime - 1, status: "merged" },
    ]);
    const candidates = await retentionCandidatesV1(store, { nowMs: NOW });
    expect(candidates[0]?.decision).toMatchObject({
      decision: "retain",
      reasonCode: "still_within_retention_window",
      remainingMs: 0,
    });
    expect(candidates[1]?.decision).toMatchObject({
      decision: "prune",
      reasonCode: "expired_and_terminal",
      remainingMs: -1,
    });
  });
});

describe("pruneExpiredDaPayloadsV1", () => {
  it("deletes only expired_and_terminal records", async () => {
    const store = await openStore();
    const expired = NOW - 16 * RETENTION_MS_PER_DAY_V1;
    await seed(store, [
      { headerHash: hashOf(1), endTimeMs: expired, status: "merged" },
      { headerHash: hashOf(2), endTimeMs: expired, status: "attested" },
      { headerHash: hashOf(3), endTimeMs: expired, status: "conflicted" },
      { headerHash: hashOf(4), withoutHeader: true },
    ]);
    const result = await pruneExpiredDaPayloadsV1(store, { nowMs: NOW });
    expect(result).toEqual({
      scanned: 4,
      prunedHeaderHashes: [hashOf(1)],
      retained: 3,
    });
    expect(await store.getDaPayload(hashOf(1))).toBeUndefined();
    expect(await store.getDaPayload(hashOf(2))).toBeDefined();
    expect((await store.listDaPayloads()).map((row) => row.headerHash)).toEqual(
      [hashOf(2), hashOf(3), hashOf(4)],
    );
  });

  it("is inert against today's scanner statuses (no merged/removed emitted)", async () => {
    // Regression lock: the L1 state-queue scanner only ever writes
    // unattested/attesting/attested/conflicted today, so the committee pruner
    // must delete nothing. Loosening the terminal-status requirement to make
    // this pruner active must fail here.
    const store = await openStore();
    const expired = NOW - 40 * RETENTION_MS_PER_DAY_V1;
    await seed(
      store,
      (["unattested", "attesting", "attested", "conflicted"] as const).map(
        (status, index) => ({
          headerHash: hashOf(20 + index),
          endTimeMs: expired,
          status,
        }),
      ),
    );
    const result = await pruneExpiredDaPayloadsV1(store, { nowMs: NOW });
    expect(result.prunedHeaderHashes).toEqual([]);
    expect(result.retained).toBe(4);
  });

  it("never deletes across a deployment fingerprint mismatch", async () => {
    const store = await openStore();
    const expired = NOW - 40 * RETENTION_MS_PER_DAY_V1;
    await seed(store, [
      {
        headerHash: hashOf(30),
        endTimeMs: expired,
        status: "merged",
        deploymentFingerprint: "ab".repeat(32),
      },
    ]);
    const result = await pruneExpiredDaPayloadsV1(store, {
      nowMs: NOW,
      deploymentFingerprint: FINGERPRINT,
    });
    expect(result.prunedHeaderHashes).toEqual([]);
    expect(await store.getDaPayload(hashOf(30))).toBeDefined();
  });
});

describe("retentionDeadlineReportV1", () => {
  it("reports derived window arithmetic and alerts on burned headroom", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(40),
        endTimeMs:
          NOW - REQUIRED_RETENTION_MS + MIDGARD_RETENTION_WINDOW_V1.marginMs,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReportV1(store, { nowMs: NOW });
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
          NOW -
          REQUIRED_RETENTION_MS +
          MIDGARD_RETENTION_WINDOW_V1.marginMs +
          1,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReportV1(store, { nowMs: NOW });
    expect(report.entries[0]).toMatchObject({ headroomMs: 1, alerting: false });
    expect(report.alerting).toBe(0);
  });

  it("alerts on records with no computable deadline", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(42), withoutHeader: true }]);
    const report = await retentionDeadlineReportV1(store, { nowMs: NOW });
    expect(report.entries[0]).toEqual({
      headerHash: hashOf(42),
      reasonCode: "missing_block_end_time",
      challengeableUntilMs: null,
      remainingMs: null,
      headroomMs: null,
      alerting: true,
    });
  });

  it("rejects malformed alert thresholds", async () => {
    const store = await openStore();
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      await expect(
        retentionDeadlineReportV1(store, {
          nowMs: NOW,
          alertThresholdMs: bad,
        }),
      ).rejects.toThrow(/alertThresholdMs/u);
    }
  });
});

describe("assertLibp2pDaRetentionDaysV1", () => {
  it("accepts the canonical 15-day window matching the manifest", () => {
    expect(
      assertLibp2pDaRetentionDaysV1({
        runtimeRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
        manifestRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
      }),
    ).toBe(15);
    expect(LIBP2P_DA_MIN_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW_V1.retentionDays,
    );
  });

  it("rejects 14 days and accepts 15 at the boundary", () => {
    expect(() =>
      assertLibp2pDaRetentionDaysV1({
        runtimeRetentionDays: 14,
        manifestRetentionDays: 14,
      }),
    ).toThrow(/must be at least 15 days/u);
    expect(
      assertLibp2pDaRetentionDaysV1({
        runtimeRetentionDays: 15,
        manifestRetentionDays: 15,
      }),
    ).toBe(15);
  });

  it("rejects a runtime window that differs from the manifest window", () => {
    expect(() =>
      assertLibp2pDaRetentionDaysV1({
        runtimeRetentionDays: 16,
        manifestRetentionDays: 15,
      }),
    ).toThrow(/must exactly equal the verified deployment manifest/u);
  });

  it("rejects malformed runtime retention days", () => {
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        assertLibp2pDaRetentionDaysV1({
          runtimeRetentionDays: bad,
          manifestRetentionDays: 15,
        }),
      ).toThrow(/da_transport\.retention_days/u);
    }
  });
});
