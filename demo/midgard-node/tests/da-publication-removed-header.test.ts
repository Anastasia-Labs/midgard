import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  DaPayloadAnnouncementsDB,
  DaPayloadPublicationsDB,
  DaPayloadsDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

const RETENTION_DAYS = 15;
const peer = {
  signerIndex: 0,
  daVkey: "01".repeat(32),
  peerId: "peer-a",
  multiaddrs: ["/ip4/127.0.0.1/tcp/4101"],
  roles: ["committee"],
} as const;

const payloadFixture = (label: string): DaPayloadsDB.InsertInput => {
  const headerHash = deterministicFixtureBytes(`da-removed:${label}`, 28);
  const payload = deterministicFixtureBytes(`da-removed:${label}:payload`, 96);
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
  };
};

/** Records a terminal outcome row for `headerHash` exactly as the observer save shapes it. */
const recordOutcome = (headerHash: Buffer, outcome: "removed" | "merged") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const digest = (label: string, length: number) =>
      deterministicFixtureBytes(
        `da-removed:${headerHash.toString("hex")}:${label}`,
        length,
      );
    yield* sql`
      INSERT INTO da_payload_terminal_outcomes (
        header_hash, terminal_outcome, transition_kind,
        deployment_identity_digest, state_queue_policy_id,
        transaction_hash, block_hash, slot, block_no,
        transaction_index, chain_point_id, finality_depth,
        transition_digest, transition_record
      ) VALUES (
        ${headerHash}, ${outcome},
        ${outcome === "merged" ? "merge" : "timeout_correction"},
        ${digest("deployment", 32)}, ${digest("policy", 28)},
        ${digest("tx", 32)}, ${digest("block", 32)}, 10, 10, 0,
        ${digest("point", 32)}, 3, ${digest("transition", 32)}, ${"{}"}
      )`;
  });

const claimPublications = (token: string) =>
  DaPayloadPublicationsDB.claimDue({
    retentionDays: RETENTION_DAYS,
    limit: 10,
    leaseOwner: "reconciler",
    leaseToken: token,
    leaseMs: 30_000,
  });

const claimAnnouncements = (token: string) =>
  DaPayloadAnnouncementsDB.claimDue({
    retentionDays: RETENTION_DAYS,
    limit: 10,
    leaseOwner: "reconciler",
    leaseToken: token,
    leaseMs: 30_000,
  });

const hex = (rows: readonly { readonly header_hash: Buffer }[]) =>
  rows.map((row) => row.header_hash.toString("hex")).sort();

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "da-publication-removed-header.test",
        });
        // `resetApplicationTables` is avoided: its seed-row replay currently
        // re-runs schema DDL. Only the tables this file touches are cleared.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`
          TRUNCATE da_payload_publications, da_payload_announcements,
            da_payload_terminal_outcomes, da_payloads CASCADE`;
        return yield* effect;
      }),
    ) as Effect.Effect<A, E, never>,
  );

describe("DA reconciliation of removed headers", () => {
  it("neither republishes, re-announces nor counts a removed header's payload, and resumes when the removal is revoked", async () => {
    await run(
      Effect.gen(function* () {
        const live = payloadFixture("live");
        const merged = payloadFixture("merged");
        const removed = payloadFixture("removed");
        for (const payload of [live, merged, removed]) {
          yield* DaPayloadsDB.upsertAvailable(payload);
          yield* DaPayloadPublicationsDB.seedForPayload(
            payload[DaPayloadsDB.Columns.HEADER_HASH],
            [peer],
          );
        }
        yield* DaPayloadAnnouncementsDB.seedRecentPayloads(RETENTION_DAYS);
        const liveHash = live[DaPayloadsDB.Columns.HEADER_HASH];
        const mergedHash = merged[DaPayloadsDB.Columns.HEADER_HASH];
        const removedHash = removed[DaPayloadsDB.Columns.HEADER_HASH];
        // The positive polarity first: with no terminal outcome every
        // payload is owed.
        expect(
          yield* DaPayloadPublicationsDB.backlogCount(RETENTION_DAYS),
        ).toBe(3);
        expect(
          yield* DaPayloadAnnouncementsDB.backlogCount(RETENTION_DAYS),
        ).toBe(3);

        yield* recordOutcome(mergedHash, "merged");
        yield* recordOutcome(removedHash, "removed");

        const owed = [liveHash, mergedHash]
          .map((h) => h.toString("hex"))
          .sort();
        expect(
          yield* DaPayloadPublicationsDB.backlogCount(RETENTION_DAYS),
        ).toBe(2);
        expect(
          yield* DaPayloadAnnouncementsDB.backlogCount(RETENTION_DAYS),
        ).toBe(2);
        expect(hex(yield* claimPublications("first"))).toEqual(owed);
        expect(hex(yield* claimAnnouncements("first"))).toEqual(owed);

        // A conflict on a removed header is not an operator alert either.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`
          UPDATE da_payload_publications
          SET status = 'conflict', lease_owner = NULL, lease_token = NULL,
            lease_expires_at = NULL
          WHERE header_hash = ${removedHash}`;
        expect(
          yield* DaPayloadPublicationsDB.conflictCount(RETENTION_DAYS),
        ).toBe(0);
        yield* sql`
          UPDATE da_payload_publications SET status = 'pending'
          WHERE header_hash = ${removedHash}`;

        // An authenticated rollback revokes the removal: the payload is owed
        // again, without any outbox row having been rewritten.
        yield* sql`
          DELETE FROM da_payload_terminal_outcomes
          WHERE header_hash = ${removedHash}`;
        expect(hex(yield* claimPublications("second"))).toEqual([
          removedHash.toString("hex"),
        ]);
        expect(hex(yield* claimAnnouncements("second"))).toEqual([
          removedHash.toString("hex"),
        ]);
      }),
    );
  });
});
