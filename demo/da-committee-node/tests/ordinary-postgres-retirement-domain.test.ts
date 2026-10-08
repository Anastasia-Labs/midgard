import * as SDK from "@al-ft/midgard-sdk";
import { Client } from "pg";
import { describe, expect, it } from "vitest";

import type { StateQueueHeaderRecord } from "../src/domain.js";
import { headerHashOf } from "../src/l1/follower/queue-derivation.js";
import { PostgresCommitteeStore } from "../src/store/postgres.postgres-committee-store.js";
import { makePayloadFixture } from "./helpers.make-payload-fixture.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

const deploymentFingerprint = "a".repeat(64);
const nonce = (index: number) => ({
  deploymentFingerprint,
  signerIndex: 0,
  nonce: index.toString(16).padStart(64, "0"),
  timestampMs: 1_790_000_000_000 + index,
  receivedAt: "2026-10-02T00:00:00.000Z",
});
const health = (index: number) => ({
  peerId: `peer-${index}`,
  consecutiveFailures: 0,
  updatedAt: "2026-10-02T00:00:00.000Z",
});

describe("ordinary store has no adopted retirement floor", () => {
  for (const family of ["peerHealth", "peerNonces"] as const) {
    it(`persists 513 ordinary PG ${family} records without activation`, async () => {
      const databases = postgresTestDatabases("midgard_pg_cap_independent");
      const db = await databases.create();
      let store: PostgresCommitteeStore | undefined;
      const observer = new Client({ connectionString: db.url });
      try {
        store = await PostgresCommitteeStore.open(db.url);
        expect(await store.getRetirementFloor()).toBeUndefined();
        let failure: unknown;
        let completed = 0;
        for (let index = 0; index < 513; index++) {
          try {
            if (family === "peerHealth")
              await store.savePeerHealth(health(index));
            else expect(await store.recordPeerNonce(nonce(index))).toBe(true);
            completed++;
          } catch (error) {
            failure = error;
            break;
          }
        }
        await observer.connect();
        const table =
          family === "peerHealth"
            ? "committee_peer_health"
            : "committee_peer_nonces";
        const records = Number(
          (await observer.query(`SELECT COUNT(*) AS count FROM ${table}`))
            .rows[0].count,
        );
        console.log(
          JSON.stringify({
            family,
            completed,
            records,
            failure: failure instanceof Error ? failure.message : failure,
            floorAbsent: (await store.getRetirementFloor()) === undefined,
          }),
        );
        expect(records).toBe(completed); // Actual failed transition rolls back.
        expect(failure).toBeUndefined();
        expect(completed).toBe(513);
        await expect(store.readRetirementSnapshot()).rejects.toThrow(
          "bounded row domain",
        );
        expect(
          await store.deleteDaPayloadIfPrunable({
            headerHash: "00".repeat(28),
            nowMs: 1_790_000_000_000,
            confirmedHeadHash: "00".repeat(28),
            liveQueueHeaderHashes: new Set(),
          }),
        ).toBe(false);
        await store.recordRetirementBreach("unconfigured diagnostic", {
          slot: 100,
          blockHash: "12".repeat(32),
          blockNo: 100,
        });
        expect(await store.getRetirementFloor()).toBeUndefined();
        const guard = store.captureRetirementGuard();
        expect(() => store!.assertRetirementGuard(guard)).not.toThrow();
        if (family === "peerHealth") await store.savePeerHealth(health(513));
        else expect(await store.recordPeerNonce(nonce(513))).toBe(true);
      } finally {
        await observer.end();
        await store?.close();
        await databases.dropAll();
      }
    }, 120000);
  }
});

it("ordinary production header upsert also persists 513 exact linked headers", async () => {
  const databases = postgresTestDatabases("midgard_pg_header_cap_independent");
  const db = await databases.create();
  const store = await PostgresCommitteeStore.open(db.url);
  const observer = new Client({ connectionString: db.url });
  try {
    const base = await makePayloadFixture(1);
    let previous = base.header.prevHeaderHash;
    let completed = 0;
    let failure: unknown;
    for (let index = 0; index < 513; index++) {
      const header = {
        ...base.header,
        prevHeaderHash: previous,
        startTime: BigInt(index * 2 + 1),
        endTime: BigInt(index * 2 + 2),
        blockSlot: BigInt(index),
      };
      const headerHash = headerHashOf(header);
      const record: StateQueueHeaderRecord = {
        deploymentFingerprint,
        headerHash,
        stateQueueOutRef: `${"66".repeat(32)}#${index}`,
        blockAssetName: headerHash,
        header,
        computedHeaderHash: headerHash,
        daAttestation: SDK.NO_DA_ATTESTATION,
        observedChainPoint: {
          slot: index,
          blockHash: "12".repeat(32),
          blockHeight: index,
          depth: 2161,
          finalized: true,
          providerSource: "authenticated_state_queue_transition_v1",
        },
        finalized: true,
        status: "removed",
        validationErrors: [],
        updatedAt: "2026-10-02T00:00:00.000Z",
      };
      try {
        await store.upsertStateQueueHeader(record);
        completed++;
        previous = headerHash;
      } catch (error) {
        failure = error;
        break;
      }
    }
    await observer.connect();
    const records = Number(
      (
        await observer.query(
          "SELECT COUNT(*) AS count FROM committee_state_queue_headers",
        )
      ).rows[0].count,
    );
    console.log(
      JSON.stringify({
        family: "stateQueueHeaders",
        completed,
        records,
        failure: failure instanceof Error ? failure.message : failure,
        floorAbsent: (await store.getRetirementFloor()) === undefined,
      }),
    );
    expect(records).toBe(completed);
    expect(failure).toBeUndefined();
    expect(completed).toBe(513);
  } finally {
    await observer.end();
    await store.close();
    await databases.dropAll();
  }
}, 120000);

it("malformed persisted floor refuses the ordinary write before mutation", async () => {
  const databases = postgresTestDatabases("midgard_pg_floor_metadata");
  const db = await databases.create();
  const store = await PostgresCommitteeStore.open(db.url);
  const observer = new Client({ connectionString: db.url });
  try {
    await observer.connect();
    await observer.query(
      "INSERT INTO committee_retirement_metadata (id, record) VALUES (1, '{}'::jsonb)",
    );
    await expect(store.savePeerHealth(health(0))).rejects.toThrow();
    expect(await store.listPeerHealth()).toHaveLength(0);
    await expect(
      store.recordRetirementBreach("unconfigured diagnostic", {
        slot: 100,
        blockHash: "12".repeat(32),
      }),
    ).rejects.toThrow();
  } finally {
    await observer.end();
    await store.close();
    await databases.dropAll();
  }
});
