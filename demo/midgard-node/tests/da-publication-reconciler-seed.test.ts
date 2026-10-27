import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { DaPayloadsDB } from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import {
  type DaPublicationSeedState,
  seedPublicationOutboxes,
} from "../src/fibers/da-publication-reconciler.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const RETENTION_DAYS = 15;

const peerOf = (signerIndex: number, peerId: string) =>
  ({
    signerIndex,
    daVkey: signerIndex.toString(16).padStart(2, "0").repeat(32),
    peerId,
    multiaddrs: [`/ip4/127.0.0.1/tcp/${(4101 + signerIndex).toString()}`],
    roles: ["committee"],
  }) as const;
const PEER_A = peerOf(0, "peer-a");
const PEER_B = peerOf(1, "peer-b");

const payloadFixture = (label: string): DaPayloadsDB.InsertInput => {
  const headerHash = deterministicFixtureBytes(`da-seed:${label}`, 28);
  const payload = deterministicFixtureBytes(`da-seed:${label}:payload`, 96);
  return {
    [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
    [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
    [DaPayloadsDB.Columns.VERSION]: 1,
    [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
    [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
      .update(payload)
      .digest(),
    [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
    [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
    [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
    [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(),
    [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(),
    [DaPayloadsDB.Columns.BLOCK_TIME]: new Date(),
  };
};

/**
 * Stores a payload with no outbox rows, as a crash between its commit and
 * its per-peer seeding leaves it, created `ageDays` ago.
 */
const storeUnseeded = (label: string, ageDays = 0) =>
  Effect.gen(function* () {
    const payload = payloadFixture(label);
    const headerHash = payload[DaPayloadsDB.Columns.HEADER_HASH];
    yield* DaPayloadsDB.upsertAvailable(payload);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE da_payloads
      SET created_at = NOW() - (${ageDays} * INTERVAL '1 day')
      WHERE header_hash = ${headerHash}`;
    return headerHash.toString("hex");
  });

/** The header hashes in each outbox: per peer, and for announcement. */
const outboxes = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const publications = yield* sql<{
    readonly header_hash: Buffer;
    readonly peer_id: string;
  }>`SELECT header_hash, peer_id FROM da_payload_publications`;
  const announcements = yield* sql<{ readonly header_hash: Buffer }>`
    SELECT header_hash FROM da_payload_announcements`;
  const hashesFor = (peerId: string) =>
    publications
      .filter((row) => row.peer_id === peerId)
      .map((row) => row.header_hash.toString("hex"))
      .sort();
  return {
    [PEER_A.peerId]: hashesFor(PEER_A.peerId),
    [PEER_B.peerId]: hashesFor(PEER_B.peerId),
    announced: announcements
      .map((row) => row.header_hash.toString("hex"))
      .sort(),
  };
});

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "da-publication-reconciler-seed.test",
        });
        yield* resetApplicationTables;
        return yield* effect;
      }),
    ) as Effect.Effect<A, E, never>,
  );

describe("DA publication outbox seeding", () => {
  it("seeds the whole window first, then only payloads created since", async () => {
    await run(
      Effect.gen(function* () {
        const seed: DaPublicationSeedState = {};
        const peers = [PEER_A];
        const old = yield* storeUnseeded("old", 2);
        const fresh = yield* storeUnseeded("fresh");
        yield* storeUnseeded("expired", RETENTION_DAYS + 1);
        yield* seedPublicationOutboxes(seed, {
          peers,
          retentionDays: RETENTION_DAYS,
        });
        const first = [old, fresh].sort();
        expect(yield* outboxes).toEqual({
          [PEER_A.peerId]: first,
          [PEER_B.peerId]: [],
          announced: first,
        });

        // A payload left unseeded by a crash long before the last seed is
        // not looked at again in steady state; a new one is.
        yield* storeUnseeded("late-old", 2);
        const lateFresh = yield* storeUnseeded("late-fresh");
        yield* seedPublicationOutboxes(seed, {
          peers,
          retentionDays: RETENTION_DAYS,
        });
        const second = [...first, lateFresh].sort();
        expect(yield* outboxes).toEqual({
          [PEER_A.peerId]: second,
          [PEER_B.peerId]: [],
          announced: second,
        });
      }),
    );
  });

  it("gives a new peer every in-window payload", async () => {
    await run(
      Effect.gen(function* () {
        const seed: DaPublicationSeedState = {};
        const stored = [
          yield* storeUnseeded("one", 1),
          yield* storeUnseeded("two", 5),
          yield* storeUnseeded("three"),
        ].sort();
        yield* seedPublicationOutboxes(seed, {
          peers: [PEER_A],
          retentionDays: RETENTION_DAYS,
        });
        yield* seedPublicationOutboxes(seed, {
          peers: [PEER_A, PEER_B],
          retentionDays: RETENTION_DAYS,
        });
        expect(yield* outboxes).toEqual({
          [PEER_A.peerId]: stored,
          [PEER_B.peerId]: stored,
          announced: stored,
        });
      }),
    );
  });
});
