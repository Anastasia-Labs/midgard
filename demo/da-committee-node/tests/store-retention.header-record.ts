import { randomBytes } from "node:crypto";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Client } from "pg";
import { afterAll, afterEach } from "vitest";

import type {
  DaPayloadRecord,
  StateQueueHeaderRecord,
  StateQueueHeaderStatus,
} from "../src/domain.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { fixtureHeaderBase, tempDir } from "./helpers.js";

export const FINGERPRINT = "cd".repeat(32);

export const NOW = Date.UTC(2026, 7, 3);

export const REQUIRED_RETENTION_MS =
  MIDGARD_RETENTION_WINDOW.requiredRetentionMs;

export const hashOf = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(28);

export const retentionOptions = (nowMs = NOW) => ({
  nowMs,
  deploymentFingerprint: FINGERPRINT,
  minimumFinalityDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  confirmedHeadHash: hashOf(200),
  liveQueueHeaderHashes: new Set([hashOf(201), hashOf(202)]),
});

const openStores = new Set<CommitteeStore>();

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close?.()));
  openStores.clear();
});

export const openStore = async (): Promise<JsonFileCommitteeStore> => {
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
export const openPostgresStore = async (): Promise<PostgresCommitteeStore> => {
  const databaseName = `${process.env.MIDGARD_TEST_DATABASE_PREFIX ?? "codex_rel_committee_retention"}_${randomBytes(6).toString("hex")}`;
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
          depth: 2161,
          finalized: true,
          providerSource: "authenticated_state_queue_transition_v1",
        }
      : { finalized: true },
  finalized: true,
  status,
  validationErrors: [],
  updatedAt: new Date(NOW).toISOString(),
});

export const payloadRecord = (
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

export const seed = async (
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

export const HEAD = hashOf(200);

export const LIVE_A = hashOf(201);

export const LIVE_B = hashOf(202);

export const PAST_HORIZON = NOW - REQUIRED_RETENTION_MS - 1;
