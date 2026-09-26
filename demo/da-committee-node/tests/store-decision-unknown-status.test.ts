import { randomBytes } from "node:crypto";

import { Client } from "pg";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type CommitteeStore,
  decisionEffectId,
  type DecisionOutboxRecord,
  JsonFileCommitteeStore,
  type L1ObservedDecision,
  type L1ObservedStatus,
  type L1SourceState,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { tempDir } from "./helpers.js";

const openStores = new Set<CommitteeStore>();
const postgresDatabases: string[] = [];

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close?.()));
  openStores.clear();
});

const postgresAdmin = {
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  user: process.env.POSTGRES_USER ?? "postgres",
  password: process.env.POSTGRES_PASSWORD ?? "postgres",
};

const openJsonStore = async (): Promise<CommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(await tempDir());
  openStores.add(store);
  return store;
};

/**
 * A committee store on a fresh database of the workspace test cluster
 * (`scripts/start-test-postgres.sh`); fails closed when none is reachable.
 */
const openPostgresStore = async (): Promise<CommitteeStore> => {
  const databaseName = `committee_decision_${randomBytes(6).toString("hex")}`;
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

const deploymentFingerprint = "cd".repeat(32);
const headerHash = "12".repeat(28);
const stateQueueOutRef = `${"34".repeat(32)}#0`;

const reconcile: DecisionOutboxRecord = {
  schemaVersion: 1,
  effectId: decisionEffectId({
    deploymentFingerprint,
    headerHash,
    stateQueueOutRef,
    effectKind: "l1_reconcile",
  }),
  deploymentFingerprint,
  sourceMode: "local_node",
  network: "Preprod",
  effectKind: "l1_reconcile",
  headerHash,
  stateQueueOutRef,
  slot: 1,
  blockHash: "66".repeat(32),
  finalized: true,
  status: "pending",
  attemptCount: 1,
  createdAt: "2026-07-28T00:00:00.000Z",
  updatedAt: "2026-07-28T00:00:00.000Z",
};

const sourceState = (stateQueueStatus: L1ObservedStatus): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "91".repeat(32),
  status: "healthy",
  observations: [
    {
      headerHash,
      stateQueueOutRef,
      stateQueueStatus,
      ...(stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS
        ? { lastKnownStatus: "unattested" as const }
        : {}),
      slot: 1,
      blockHash: "66".repeat(32),
      finalized: true,
      hasPersistedDecision: true,
    },
  ],
  observedAt: "2026-07-28T00:00:00.000Z",
});

describe("decision effects on an observation whose status is unknown", () => {
  it.each([
    ["the JSON file store", openJsonStore],
    ["the Postgres store", openPostgresStore],
  ] as const)(
    "are refused by %s, which begins one once the status is known",
    async (_label, open) => {
      const store = await open();
      await expect(
        store.beginDecisionEffect({
          effect: reconcile,
          sourceState: sourceState(UNKNOWN_STATE_QUEUE_STATUS),
        }),
      ).rejects.toThrow(
        "decision outbox lacks matching durable L1 observation",
      );
      await expect(store.listDecisionOutbox(headerHash)).resolves.toEqual([]);
      await store.beginDecisionEffect({
        effect: reconcile,
        sourceState: sourceState("attested"),
      });
      await expect(store.listDecisionOutbox(headerHash)).resolves.toEqual([
        reconcile,
      ]);
    },
  );
});

describe("an unknown status across the stores", () => {
  const movedTo = `${"35".repeat(32)}#1`;
  const withObservation = (observation: L1ObservedDecision): L1SourceState => ({
    ...sourceState("attested"),
    observations: [observation],
    stateQueueReplayAnchor: {
      deploymentIdentityDigest: "aa".repeat(32),
      stateQueuePolicyId: "bb".repeat(28),
      queue: [{ headerHash: null, outRef: `${"00".repeat(32)}#0` }],
      blockNo: "90",
      transactionIndex: "0",
    },
  });
  const attested = withObservation(sourceState("attested").observations[0]!);
  const moved = {
    ...attested.observations[0]!,
    stateQueueOutRef: movedTo,
    slot: 2,
    blockHash: "67".repeat(32),
    authenticatedSteps: [
      {
        fromOutRef: stateQueueOutRef,
        toOutRef: movedTo,
        slot: 2,
        blockHash: "67".repeat(32),
      },
    ],
  };

  it.each([
    ["the JSON file store", openJsonStore],
    ["the Postgres store", openPostgresStore],
  ] as const)(
    "persists the status known before it, which %s refuses to see contradicted",
    async (_label, open) => {
      const store = await open();
      await store.saveL1SourceState(attested);
      const unknown: L1ObservedDecision = {
        ...moved,
        stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
        lastKnownStatus: "attested",
      };
      await store.saveL1SourceState(withObservation(unknown));
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        observations: [unknown],
      });
      const { lastKnownStatus: _lastKnownStatus, ...known } = {
        ...unknown,
        hasPersistedDecision: false,
      };
      await expect(
        store.saveL1SourceState(
          withObservation({ ...known, stateQueueStatus: "unattested" }),
        ),
      ).rejects.toThrow(/persisted L1 decision changed canonical output/u);
      const filled = { ...known, stateQueueStatus: "attested" as const };
      await store.saveL1SourceState(withObservation(filled));
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        observations: [{ ...filled, hasPersistedDecision: true }],
      });
    },
  );
});
