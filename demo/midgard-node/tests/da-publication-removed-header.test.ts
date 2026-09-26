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
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

const RETENTION_DAYS = 15;
/** The running deployment's verified manifest ID, and another deployment's. */
const DEPLOYMENT = deterministicFixtureBytes("da-removed:deployment", 32);
const FOREIGN_DEPLOYMENT = deterministicFixtureBytes(
  "da-removed:foreign-deployment",
  32,
);
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
const recordOutcome = (
  headerHash: Buffer,
  outcome: "removed" | "merged",
  deployment: Buffer = DEPLOYMENT,
) =>
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
        ${deployment}, ${digest("policy", 28)},
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
    deploymentIdentityDigest: DEPLOYMENT,
  });

const claimAnnouncements = (token: string) =>
  DaPayloadAnnouncementsDB.claimDue({
    retentionDays: RETENTION_DAYS,
    limit: 10,
    leaseOwner: "reconciler",
    leaseToken: token,
    leaseMs: 30_000,
    deploymentIdentityDigest: DEPLOYMENT,
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
            da_payload_terminal_outcomes, da_payloads,
            pending_block_finalizations CASCADE`;
        return yield* effect;
      }),
    ) as Effect.Effect<A, E, never>,
  );

const ZERO_ROOT = "00".repeat(32);
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

/** A committed block's journal row for `headerHash`. */
const insertJournal = (
  headerHash: Buffer,
  status: PendingBlockFinalizationsDB.Status,
  correctionTransitionDigest: string | null,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const at = new Date();
    yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
      header_hash: headerHash,
      submitted_tx_hash: null,
      block_end_time: at,
      status,
      correction_transition_digest: correctionTransitionDigest,
      observed_confirmed_at_ms: null,
      created_at: at,
      updated_at: at,
      state_queue_lease_token: `da-removed:${headerHash.toString("hex").slice(0, 8)}`,
      base_snapshot_id: "da-removed",
      base_tail_out_ref: "base#0",
      base_tail_header_hash: Buffer.from("bb".repeat(28), "hex"),
      base_tail_datum_cbor: "d87980",
      base_utxos_root: ZERO_ROOT,
      base_transactions_root: ZERO_ROOT,
      base_deposits_root: ZERO_ROOT,
      base_withdrawals_root: ZERO_ROOT,
      block_start_time: at,
      expected_utxos_root: ZERO_ROOT,
      expected_transactions_root: ZERO_ROOT,
      expected_deposits_root: ZERO_ROOT,
      expected_withdrawals_root: ZERO_ROOT,
      base_forced_transactions_root: ZERO_ROOT,
      expected_forced_transactions_root: ZERO_ROOT,
      header_cbor: Buffer.from("a0", "hex"),
      format_version: 1,
      replay_kind: "ledger_delta_v1",
      deployment_marker_schema_version: "midgard-deployment-marker-v1",
      deployment_manifest_id: DEPLOYMENT.toString("hex"),
      expected_transition_trace_root: ZERO_ROOT,
      expected_event_to_step_root: ZERO_ROOT,
      expected_withdrawal_count: 0n,
      expected_forced_transaction_count: 0n,
      expected_l2_transaction_count: 0n,
      expected_deposit_count: 0n,
      expected_total_event_count: 0n,
      expected_transition_step_count: 0n,
      consensus_profile_id: MIDGARD_CONSENSUS_PROFILE_ID,
      expected_validation_traces_root: EMPTY_MERKLE_ROOT,
      expected_validation_trace_count: 0n,
      ledger_delta_spent: "[]",
      ledger_delta_produced: "[]",
    } as never)}`;
  });

/** Stores each labelled payload with its peer and announcement rows. */
const seedPayloads = (labels: readonly string[]) =>
  Effect.gen(function* () {
    const hashes: Buffer[] = [];
    for (const label of labels) {
      const payload = payloadFixture(label);
      yield* DaPayloadsDB.upsertAvailable(payload);
      yield* DaPayloadPublicationsDB.seedForPayload(
        payload[DaPayloadsDB.Columns.HEADER_HASH],
        [peer],
      );
      hashes.push(payload[DaPayloadsDB.Columns.HEADER_HASH]);
    }
    yield* DaPayloadAnnouncementsDB.seedRecentPayloads(RETENTION_DAYS);
    return hashes;
  });

/** Every payload the reconciler owes now: claimed for peers and for
 * announcement, and counted, identically; each claim's lease is released. */
const owedNow = (token: string) =>
  Effect.gen(function* () {
    const publications = hex(yield* claimPublications(token));
    const announcements = hex(yield* claimAnnouncements(token));
    expect(announcements).toEqual(publications);
    expect(
      yield* DaPayloadPublicationsDB.backlogCount(RETENTION_DAYS, DEPLOYMENT),
    ).toBe(publications.length);
    expect(
      yield* DaPayloadAnnouncementsDB.backlogCount(RETENTION_DAYS, DEPLOYMENT),
    ).toBe(publications.length);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE da_payload_publications
      SET lease_owner = NULL, lease_token = NULL, lease_expires_at = NULL`;
    yield* sql`UPDATE da_payload_announcements
      SET lease_owner = NULL, lease_token = NULL, lease_expires_at = NULL`;
    return publications;
  });

const CORRECTION_DIGEST = "c0".repeat(32);

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
          yield* DaPayloadPublicationsDB.backlogCount(
            RETENTION_DAYS,
            DEPLOYMENT,
          ),
        ).toBe(3);
        expect(
          yield* DaPayloadAnnouncementsDB.backlogCount(
            RETENTION_DAYS,
            DEPLOYMENT,
          ),
        ).toBe(3);

        yield* recordOutcome(mergedHash, "merged");
        yield* recordOutcome(removedHash, "removed");

        const owed = [liveHash, mergedHash]
          .map((h) => h.toString("hex"))
          .sort();
        expect(
          yield* DaPayloadPublicationsDB.backlogCount(
            RETENTION_DAYS,
            DEPLOYMENT,
          ),
        ).toBe(2);
        expect(
          yield* DaPayloadAnnouncementsDB.backlogCount(
            RETENTION_DAYS,
            DEPLOYMENT,
          ),
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
          yield* DaPayloadPublicationsDB.conflictCount(
            RETENTION_DAYS,
            DEPLOYMENT,
          ),
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

  it("counts only this deployment's authenticated removal outcomes", async () => {
    await run(
      Effect.gen(function* () {
        const [foreignRemoved, removed] = yield* seedPayloads([
          "foreign-removed",
          "own-removed",
        ]);
        yield* recordOutcome(foreignRemoved!, "removed", FOREIGN_DEPLOYMENT);
        yield* recordOutcome(removed!, "removed");
        // Another deployment's removal of the same header hash says nothing
        // about this chain: the payload stays owed.
        expect(yield* owedNow("scoped")).toEqual([
          foreignRemoved!.toString("hex"),
        ]);
        // A derived contract bundle has no authenticated outcomes to consult.
        expect(
          yield* DaPayloadPublicationsDB.backlogCount(RETENTION_DAYS),
        ).toBe(2);
      }),
    );
  });

  it("neither republishes nor counts a header abandoned under a named correction, owes one abandoned without one, and owes it again once its journal is revived", async () => {
    await run(
      Effect.gen(function* () {
        const [released, abandoned, live] = yield* seedPayloads([
          "released",
          "abandoned-without-correction",
          "live",
        ]);
        const Status = PendingBlockFinalizationsDB.Status;
        // A signed-intent release (or a rewind) abandons its journal under the
        // digest that proves it; another abandonment names nothing.
        yield* insertJournal(released!, Status.Abandoned, CORRECTION_DIGEST);
        yield* insertJournal(abandoned!, Status.Abandoned, null);
        expect(yield* owedNow("abandoned")).toEqual(
          [abandoned!, live!].map((hash) => hash.toString("hex")).sort(),
        );
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE da_payload_publications SET status = 'conflict'
          WHERE header_hash = ${released!}`;
        expect(
          yield* DaPayloadPublicationsDB.conflictCount(
            RETENTION_DAYS,
            DEPLOYMENT,
          ),
        ).toBe(0);
        yield* sql`UPDATE da_payload_publications SET status = 'pending'
          WHERE header_hash = ${released!}`;

        // The released journal is revived as canonical (the column change of
        // `reviveAbandonedCanonical`); its correction digest stays, and its
        // payload is owed again.
        yield* sql`UPDATE pending_block_finalizations
          SET status = ${Status.ObservedWaitingStability}
          WHERE header_hash = ${released!} AND status = ${Status.Abandoned}`;
        expect(yield* owedNow("revived")).toEqual(
          [released!, abandoned!, live!]
            .map((hash) => hash.toString("hex"))
            .sort(),
        );
      }),
    );
  });
});
