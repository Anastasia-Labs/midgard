import { Client } from "pg";
import { describe, expect, it } from "vitest";

import { isFatalStartupError, startupReason } from "../src/startup.js";
import { decisionEffectId } from "../src/store.js";
import {
  closeTestCommitteeStore,
  healthyL1SourceState,
  openTestCommitteeStore,
  saveHealthyL1SourceState,
  testStoreDatabase,
} from "./helpers/committee-store.js";

/**
 * A committee store a build from before the L1 follower (C1) wrote, opened
 * by this build. The old L1 source state is class A and rebuilt by the next
 * healthy tick, so the open deletes it when it does not parse; outbox
 * records lose the quarantine fields and keep the rest; a record the
 * upgrade cannot repair refuses the open with a named readiness reason,
 * which the startup retry reports with the process up.
 */

const OBSERVATION = {
  headerHash: "12".repeat(28),
  stateQueueOutRef: `${"34".repeat(32)}#0`,
  stateQueueStatus: "attested",
  slot: 90,
  blockHash: "56".repeat(32),
  finalized: true,
  hasPersistedDecision: true,
};

/** The L1 source-state rows the earlier build persisted. */
const OLD_SOURCE_STATES = {
  "a healthy row carrying the replay anchor": {
    ...healthyL1SourceState,
    observations: [OBSERVATION],
    stateQueueReplayAnchor: {
      deploymentIdentityDigest: "78".repeat(32),
      stateQueuePolicyId: "9a".repeat(28),
      queue: [{ headerHash: null, outRef: `${"bc".repeat(32)}#0` }],
      blockNo: "120",
      transactionIndex: "0",
    },
  },
  "a quarantined row": {
    ...healthyL1SourceState,
    status: "quarantined",
    quarantineReason: "l1_rollback_beyond_finality",
    quarantinedAt: "2026-10-06T00:00:00.000Z",
  },
  "an external-provider row": {
    ...healthyL1SourceState,
    sourceMode: "external_providers",
  },
  "a row with an unknown observation": {
    ...healthyL1SourceState,
    observations: [
      {
        ...OBSERVATION,
        stateQueueStatus: "unknown",
        lastKnownStatus: "attested",
        authenticatedSteps: [
          {
            fromOutRef: OBSERVATION.stateQueueOutRef,
            slot: 90,
            blockHash: OBSERVATION.blockHash,
          },
        ],
      },
    ],
  },
} as const;

const outboxRecord = (sourceMode = "local_node") => {
  const identity = {
    deploymentFingerprint: "de".repeat(32),
    headerHash: OBSERVATION.headerHash,
    stateQueueOutRef: OBSERVATION.stateQueueOutRef,
    effectKind: "l1_reconcile" as const,
  };
  return {
    schemaVersion: 1,
    effectId: decisionEffectId(identity),
    ...identity,
    sourceMode,
    network: healthyL1SourceState.network,
    slot: OBSERVATION.slot,
    blockHash: OBSERVATION.blockHash,
    finalized: true,
    status: "failed",
    attemptCount: 2,
    createdAt: "2026-10-05T00:00:00.000Z",
    updatedAt: "2026-10-06T00:00:00.000Z",
    lastError: "L1 source quarantined",
  };
};

/** A failed outbox record quarantine stamped. */
const QUARANTINED_OUTBOX = {
  ...outboxRecord(),
  quarantineReason: "l1_rollback_beyond_finality",
  quarantinedAt: "2026-10-06T00:00:00.000Z",
};

/** A store as the earlier build left it, with `rows` written raw. */
const oldStore = async (rows: {
  readonly sourceState: unknown;
  readonly outbox: readonly ReturnType<typeof outboxRecord>[];
}) => {
  const database = await testStoreDatabase();
  await closeTestCommitteeStore(await openTestCommitteeStore(database));
  const client = new Client({ connectionString: database.url });
  await client.connect();
  try {
    await client.query(
      "INSERT INTO committee_l1_source_state (id, record) VALUES (1, $1::jsonb)",
      [JSON.stringify(rows.sourceState)],
    );
    for (const record of rows.outbox)
      await client.query(
        `INSERT INTO committee_decision_outbox (effect_id, header_hash, record)
         VALUES ($1, $2, $3::jsonb)`,
        [record.effectId, record.headerHash, JSON.stringify(record)],
      );
  } finally {
    await client.end();
  }
  return database;
};

describe("a committee store from before the L1 follower, opened by this build", () => {
  it.each(Object.entries(OLD_SOURCE_STATES))(
    "deletes %s and strips the quarantine fields from the outbox, so the next healthy tick writes afresh",
    async (_name, sourceState) => {
      const database = await oldStore({
        sourceState,
        outbox: [QUARANTINED_OUTBOX],
      });
      const store = await openTestCommitteeStore(database);
      await expect(store.getL1SourceState()).resolves.toBeUndefined();
      const { quarantineReason, quarantinedAt, ...kept } = QUARANTINED_OUTBOX;
      void quarantineReason;
      void quarantinedAt;
      await expect(store.listDecisionOutbox()).resolves.toEqual([kept]);
      await expect(
        store.getDecisionOutbox(kept.effectId),
      ).resolves.toStrictEqual(kept);
      // The follower's next healthy tick persists the state again.
      await saveHealthyL1SourceState(store);
      await expect(store.getL1SourceState()).resolves.toEqual(
        healthyL1SourceState,
      );
      await closeTestCommitteeStore(store);
      // A second open of the upgraded store changes nothing.
      const again = await openTestCommitteeStore(database);
      await expect(again.getL1SourceState()).resolves.toEqual(
        healthyL1SourceState,
      );
      await expect(again.listDecisionOutbox()).resolves.toEqual([kept]);
    },
  );

  it("keeps a source state this build wrote", async () => {
    const database = await oldStore({
      sourceState: { ...healthyL1SourceState, observations: [OBSERVATION] },
      outbox: [],
    });
    const store = await openTestCommitteeStore(database);
    await expect(store.getL1SourceState()).resolves.toEqual({
      ...healthyL1SourceState,
      observations: [OBSERVATION],
    });
  });

  it("refuses the open with a named readiness reason, deleting nothing, for an outbox record of the external-provider source mode", async () => {
    const external = outboxRecord("external_providers");
    const database = await oldStore({
      sourceState: OLD_SOURCE_STATES["an external-provider row"],
      outbox: [external],
    });
    const refused = await openTestCommitteeStore(database).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refused).toBeInstanceOf(Error);
    // The startup retry reports it on /readyz and tries again; it is not
    // one of the errors a node exits on.
    expect(isFatalStartupError(refused)).toBe(false);
    expect(startupReason(refused)).toBe(
      `starting:committee_store_record_unreadable: 1 decision outbox record(s) were written under a source mode other than local_node (first ${external.effectId}); resolve or delete them`,
    );
    const client = new Client({ connectionString: database.url });
    await client.connect();
    try {
      const rows = await client.query<{ readonly record: unknown }>(
        "SELECT record FROM committee_decision_outbox",
      );
      expect(rows.rows.map(({ record }) => record)).toEqual([external]);
    } finally {
      await client.end();
    }
  });
});
