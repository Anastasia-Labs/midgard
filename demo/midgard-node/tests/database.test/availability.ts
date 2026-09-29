import { createHash } from "node:crypto";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
} from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect, Option } from "effect";
import { describe, expect } from "vitest";

import {
  DaPayloadAnnouncementsDB,
  DaPayloadPublicationsDB,
  DaPayloadsDB,
  ForeignTipReconciliationsDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import {
  daPayloadInsertFixture,
  databaseFixtureBytes,
  databaseTxHash,
  isolatedDb,
} from "./fixtures.js";

export const registerAvailabilityTests = () => {
  describe("DaPayloadsDB", () => {
    it.effect(
      "stores payloads idempotently and rejects conflicting bytes for a header",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const headerHash = databaseFixtureBytes("da-payload-header", 28);
            const payload = databaseFixtureBytes("da-payload-cbor", 48);
            const payloadHash = createHash("sha256").update(payload).digest();
            const insert = {
              [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
              [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]:
                MIDGARD_CONSENSUS_PROFILE_ID,
              [DaPayloadsDB.Columns.VERSION]: 1,
              [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
              [DaPayloadsDB.Columns.PAYLOAD_SHA256]: payloadHash,
              [DaPayloadsDB.Columns.UTXOS_ROOT]: "11".repeat(32),
              [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: "22".repeat(32),
              [DaPayloadsDB.Columns.DEPOSITS_ROOT]: "33".repeat(32),
              [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: "44".repeat(32),
              [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
              [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
              [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
              [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
              [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
              [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
              [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
              [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(
                "2026-06-12T00:00:00.000Z",
              ),
              [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(
                "2026-06-12T00:00:10.000Z",
              ),
            } satisfies DaPayloadsDB.InsertInput;

            yield* DaPayloadsDB.upsertAvailable(insert);
            yield* DaPayloadsDB.upsertAvailable(insert);

            const stored = yield* DaPayloadsDB.retrieveByHeaderHash(headerHash);
            expect(stored._tag).toBe("Some");
            if (stored._tag === "Some") {
              expect(stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR]).toEqual(
                payload,
              );
            }

            const conflict = yield* Effect.either(
              DaPayloadsDB.upsertAvailable({
                ...insert,
                [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from([
                  ...payload,
                  0,
                ]),
              }),
            );
            expect(conflict._tag).toBe("Left");
          }),
        ),
    );

    it.effect(
      "rolls back a local-finalization mutation on DA conflict and retries idempotently",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const insert = {
              ...daPayloadInsertFixture("da-finalization-atomicity"),
              [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(
                "2026-07-10T00:00:00.000Z",
              ),
              [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(
                "2026-07-10T00:00:20.000Z",
              ),
            };
            yield* DaPayloadsDB.upsertAvailable(insert);
            const sql = yield* SqlClient.SqlClient;
            const transaction = Effect.gen(function* () {
              yield* sql`UPDATE da_payloads
              SET block_start_time = ${new Date("2026-07-11T00:00:00.000Z")}
              WHERE header_hash = ${insert.header_hash}`;
              yield* DaPayloadsDB.upsertAvailable({
                ...insert,
                [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from([
                  ...insert.payload_cbor,
                  0,
                ]),
              });
            });

            const failed = yield* Effect.either(
              sql.withTransaction(transaction),
            );
            expect(failed._tag).toBe("Left");
            const afterRollback = yield* DaPayloadsDB.retrieveByHeaderHash(
              insert.header_hash,
            );
            expect(afterRollback._tag).toBe("Some");
            if (afterRollback._tag === "Some") {
              expect(afterRollback.value.block_start_time).toEqual(
                insert.block_start_time,
              );
              expect(afterRollback.value.payload_cbor).toEqual(
                insert.payload_cbor,
              );
            }

            yield* sql.withTransaction(DaPayloadsDB.upsertAvailable(insert));
            yield* sql.withTransaction(DaPayloadsDB.upsertAvailable(insert));
          }),
        ),
    );

    it.effect(
      "seeds a persisted payload after restart, claims once, and resumes after a failed scan",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const insert = daPayloadInsertFixture("da-publication-restart");
            const peers = [
              {
                signerIndex: 0,
                daVkey: "01".repeat(32),
                peerId: "peer-a",
                multiaddrs: ["/ip4/127.0.0.1/tcp/4101"],
                roles: ["committee"],
              },
              {
                signerIndex: 1,
                daVkey: "02".repeat(32),
                peerId: "peer-b",
                multiaddrs: ["/ip4/127.0.0.1/tcp/4102"],
                roles: ["committee"],
              },
              {
                signerIndex: 2,
                daVkey: "03".repeat(32),
                peerId: "peer-c",
                multiaddrs: ["/ip4/127.0.0.1/tcp/4103"],
                roles: ["committee"],
              },
            ] as const;
            // Simulates a crash after durable payload commit but before the
            // publication outbox was seeded.
            yield* DaPayloadsDB.upsertAvailable(insert);
            expect(yield* DaPayloadPublicationsDB.backlogCount(15)).toBe(0);
            yield* DaPayloadPublicationsDB.seedRecentPayloads({
              peers,
              retentionDays: 15,
            });
            expect(yield* DaPayloadPublicationsDB.backlogCount(15)).toBe(3);

            const firstClaim = yield* DaPayloadPublicationsDB.claimDue({
              retentionDays: 15,
              limit: 10,
              leaseOwner: "reconciler-a",
              leaseToken: "lease-a",
              leaseMs: 30_000,
            });
            expect(firstClaim).toHaveLength(3);
            const competingClaim = yield* DaPayloadPublicationsDB.claimDue({
              retentionDays: 15,
              limit: 10,
              leaseOwner: "reconciler-b",
              leaseToken: "lease-b",
              leaseMs: 30_000,
            });
            expect(competingClaim).toHaveLength(0);
            const sql = yield* SqlClient.SqlClient;

            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[0],
                status: "accepted",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: { owner: "reconciler-a", token: "stale-token" },
              }),
            ).toBe(false);
            // A detached foreground straggler is deliberately unleased. It must
            // not clear or overwrite the active reconciler claim.
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[0],
                status: "transport_error",
                error: "late foreground failure during reconciler claim",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(false);
            const fencedRows = yield* sql<{
              readonly status: string;
              readonly lease_token: string | null;
            }>`
            SELECT status, lease_token FROM da_payload_publications
            WHERE header_hash = ${insert.header_hash} AND peer_id = 'peer-a'
          `;
            expect(fencedRows[0]).toEqual({
              status: "pending",
              lease_token: "lease-a",
            });

            yield* DaPayloadPublicationsDB.releaseClaim({
              headerHash: insert.header_hash,
              peerId: "peer-a",
              leaseOwner: "reconciler-a",
              leaseToken: "lease-a",
            });
            const resumed = yield* DaPayloadPublicationsDB.claimDue({
              retentionDays: 15,
              limit: 10,
              leaseOwner: "reconciler-b",
              leaseToken: "lease-b",
              leaseMs: 30_000,
            });
            expect(resumed.map((row) => row.peer_id)).toEqual(["peer-a"]);

            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[0],
                status: "accepted",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: { owner: "reconciler-b", token: "lease-b" },
              }),
            ).toBe(true);
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[1],
                status: "duplicate",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: { owner: "reconciler-a", token: "lease-a" },
              }),
            ).toBe(true);
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[2],
                status: "transport_error",
                error: "first process exited",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: { owner: "reconciler-a", token: "lease-a" },
              }),
            ).toBe(true);
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[2],
                status: "accepted",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(true);
            // Late failures cannot downgrade success; conflict is evidence-grade
            // and has monotone precedence over every other outcome.
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[0],
                status: "transport_error",
                error: "late straggler failure",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(true);
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[0],
                status: "conflict",
                error: "divergent bytes",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(true);
            expect(
              yield* DaPayloadPublicationsDB.recordAttempt({
                headerHash: insert.header_hash,
                peer: peers[0],
                status: "accepted",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(true);
            const statuses = yield* sql<{
              readonly peer_id: string;
              readonly status: string;
            }>`SELECT peer_id, status FROM da_payload_publications ORDER BY peer_id`;
            expect(statuses).toEqual([
              { peer_id: "peer-a", status: "conflict" },
              { peer_id: "peer-b", status: "duplicate" },
              { peer_id: "peer-c", status: "accepted" },
            ]);
            expect(yield* DaPayloadPublicationsDB.backlogCount(15)).toBe(0);
            expect(yield* DaPayloadPublicationsDB.conflictCount(15)).toBe(1);
          }),
        ),
    );

    it.effect(
      "retries announcements durably and fences stale claim writers",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const insert = daPayloadInsertFixture("da-announcement-outbox");
            yield* DaPayloadsDB.upsertAvailable(insert);
            yield* DaPayloadAnnouncementsDB.seedRecentPayloads(15);
            expect(yield* DaPayloadAnnouncementsDB.backlogCount(15)).toBe(1);

            const claimed = yield* DaPayloadAnnouncementsDB.claimDue({
              retentionDays: 15,
              limit: 1,
              leaseOwner: "announcer-a",
              leaseToken: "announcement-lease-a",
              leaseMs: 30_000,
            });
            expect(claimed).toHaveLength(1);
            expect(
              yield* DaPayloadAnnouncementsDB.recordAttempt({
                headerHash: insert.header_hash,
                published: true,
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: { owner: "announcer-a", token: "stale-token" },
              }),
            ).toBe(false);
            expect(
              yield* DaPayloadAnnouncementsDB.recordAttempt({
                headerHash: insert.header_hash,
                published: false,
                error: "late foreground failure during announcement claim",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(false);
            const sql = yield* SqlClient.SqlClient;
            const fenced = yield* sql<{
              readonly status: string;
              readonly lease_token: string | null;
            }>`SELECT status, lease_token FROM da_payload_announcements
             WHERE header_hash = ${insert.header_hash}`;
            expect(fenced[0]).toEqual({
              status: "pending",
              lease_token: "announcement-lease-a",
            });

            expect(
              yield* DaPayloadAnnouncementsDB.recordAttempt({
                headerHash: insert.header_hash,
                published: false,
                error: "zero recipients",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: {
                  owner: "announcer-a",
                  token: "announcement-lease-a",
                },
              }),
            ).toBe(true);
            yield* sql`UPDATE da_payload_announcements
            SET next_retry_at = NOW()
            WHERE header_hash = ${insert.header_hash}`;
            const retry = yield* DaPayloadAnnouncementsDB.claimDue({
              retentionDays: 15,
              limit: 1,
              leaseOwner: "announcer-b",
              leaseToken: "announcement-lease-b",
              leaseMs: 30_000,
            });
            expect(retry).toHaveLength(1);
            expect(
              yield* DaPayloadAnnouncementsDB.recordAttempt({
                headerHash: insert.header_hash,
                published: true,
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
                lease: {
                  owner: "announcer-b",
                  token: "announcement-lease-b",
                },
              }),
            ).toBe(true);
            expect(
              yield* DaPayloadAnnouncementsDB.recordAttempt({
                headerHash: insert.header_hash,
                published: false,
                error: "late duplicate callback",
                retryBackoffMs: 1,
                retryBackoffMaxMs: 2,
              }),
            ).toBe(true);
            expect(yield* DaPayloadAnnouncementsDB.backlogCount(15)).toBe(0);
            const finalRows = yield* sql<{
              readonly status: string;
              readonly attempts: number;
            }>`SELECT status, attempts FROM da_payload_announcements
             WHERE header_hash = ${insert.header_hash}`;
            expect(finalRows[0]).toMatchObject({ status: "published" });
            expect(finalRows[0]?.attempts).toBeGreaterThanOrEqual(3);
          }),
        ),
    );

    it.effect(
      "retrieves only finalized pending journals missing DA payload rows",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const missingHeader = databaseFixtureBytes(
              "missing-da-payload-finalized-header",
              28,
            );
            const coveredHeader = databaseFixtureBytes(
              "covered-da-payload-finalized-header",
              28,
            );
            const activeHeader = databaseFixtureBytes(
              "active-da-payload-header",
              28,
            );
            const baseTime = new Date("2026-06-12T00:00:00.000Z");
            const deploymentMarker = makeDeploymentMarker("de".repeat(32));
            const row = (
              headerHash: Buffer,
              status: PendingBlockFinalizationsDB.Status,
            ) => ({
              [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: headerHash,
              [PendingBlockFinalizationsDB.Columns.HEADER_CBOR]: Buffer.from(
                "d87980",
                "hex",
              ),
              [PendingBlockFinalizationsDB.Columns.FORMAT_VERSION]:
                PendingBlockFinalizationsDB.PENDING_BLOCK_FINALIZATION_VERSION,
              [PendingBlockFinalizationsDB.Columns.REPLAY_KIND]:
                PendingBlockFinalizationsDB.PendingBlockFinalizationReplayKind
                  .LedgerDelta,
              [PendingBlockFinalizationsDB.Columns
                .DEPLOYMENT_MARKER_SCHEMA_VERSION]:
                deploymentMarker.schemaVersion,
              [PendingBlockFinalizationsDB.Columns.DEPLOYMENT_MANIFEST_ID]:
                deploymentMarker.manifestId,
              [PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID]:
                MIDGARD_CONSENSUS_PROFILE_ID,
              [PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]:
                databaseTxHash(`submitted-${headerHash.toString("hex")}`),
              [PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN]:
                "lease",
              [PendingBlockFinalizationsDB.Columns.BASE_SNAPSHOT_ID]:
                "snapshot",
              [PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF]: "base#0",
              [PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH]:
                databaseFixtureBytes("base-tail-header", 28),
              [PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR]:
                "d87980",
              [PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns
                .BASE_FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME]: baseTime,
              [PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME]: new Date(
                baseTime.getTime() + 1_000,
              ),
              [PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_TRANSITION_TRACE_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_VALIDATION_TRACES_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_VALIDATION_TRACE_COUNT]: 0n,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWAL_COUNT]:
                0n,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_FORCED_TRANSACTION_COUNT]: 0n,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_L2_TRANSACTION_COUNT]: 0n,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSIT_COUNT]: 0n,
              [PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT]:
                0n,
              [PendingBlockFinalizationsDB.Columns
                .EXPECTED_TRANSITION_STEP_COUNT]: 0n,
              [PendingBlockFinalizationsDB.Columns.LEDGER_DELTA_SPENT]:
                JSON.stringify([]),
              [PendingBlockFinalizationsDB.Columns.LEDGER_DELTA_PRODUCED]:
                JSON.stringify([]),
              [PendingBlockFinalizationsDB.Columns.STATUS]: status,
              [PendingBlockFinalizationsDB.Columns.OBSERVED_CONFIRMED_AT_MS]:
                1n,
            });
            yield* sql`INSERT INTO ${sql(
              PendingBlockFinalizationsDB.tableName,
            )} ${sql.insert([
              row(missingHeader, PendingBlockFinalizationsDB.Status.Finalized),
              row(coveredHeader, PendingBlockFinalizationsDB.Status.Finalized),
              row(
                activeHeader,
                PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
              ),
            ])}`;
            yield* DaPayloadsDB.upsertAvailable({
              [DaPayloadsDB.Columns.HEADER_HASH]: coveredHeader,
              [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]:
                MIDGARD_CONSENSUS_PROFILE_ID,
              [DaPayloadsDB.Columns.VERSION]: 1,
              [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from("a100", "hex"),
              [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
                .update(Buffer.from("a100", "hex"))
                .digest(),
              [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]:
                SDK.EMPTY_MERKLE_TREE_ROOT,
              [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
              [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
              [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
              [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
              [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
              [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
              [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
              [DaPayloadsDB.Columns.BLOCK_START_TIME]: baseTime,
              [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(
                baseTime.getTime() + 1_000,
              ),
            });

            const missing =
              yield* PendingBlockFinalizationsDB.retrieveFinalizedMissingDaPayloads(
                {
                  limit: 10,
                },
              );
            expect(
              missing.map((record) =>
                record[
                  PendingBlockFinalizationsDB.Columns.HEADER_HASH
                ].toString("hex"),
              ),
            ).toEqual([missingHeader.toString("hex")]);

            const covered =
              yield* PendingBlockFinalizationsDB.retrieveFinalizedMissingDaPayloads(
                {
                  headerHash: coveredHeader,
                  limit: 10,
                },
              );
            expect(covered).toEqual([]);
          }),
        ),
    );
  });

  describe("ForeignTipReconciliationsDB", () => {
    it.effect(
      "round-trips exact V1 deployment/DA identity and rejects substitutions",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const header: SDK.Header = {
              prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              depositsRoot: "11".repeat(32),
              transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              withdrawalCount: 0n,
              forcedTransactionCount: 0n,
              l2TransactionCount: 0n,
              depositCount: 1n,
              totalEventCount: 1n,
              transitionStepCount: 1n,
              validationTraceCount: 0n,
              startTime: 1n,
              endTime: 2n,
              blockSlot: 0n,
              expectedNetworkId: 0n,
              minFeeA: 0n,
              minFeeB: 0n,
              prevHeaderHash: "21".repeat(28),
              operatorVkey: "22".repeat(28),
              protocolVersion: 1n,
            };
            const foreignHeaderHash = yield* SDK.hashBlockHeader(header);
            const replacedBaseHeaderHash = "23".repeat(28);
            const deploymentMarker = makeDeploymentMarker("de".repeat(32));
            yield* ForeignTipReconciliationsDB.recordMismatch({
              foreignHeaderHash,
              replacedBaseHeaderHash,
              foreignHeader: header,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              deploymentMarker,
            });

            const awaiting =
              yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                foreignHeaderHash,
              );
            expect(awaiting._tag).toBe("Some");
            if (Option.isNone(awaiting)) return;
            expect(
              awaiting.value[
                ForeignTipReconciliationsDB.Columns.FORMAT_VERSION
              ],
            ).toBe(1);
            expect(
              awaiting.value[
                ForeignTipReconciliationsDB.Columns.DEPLOYMENT_MANIFEST_ID
              ],
            ).toBe(deploymentMarker.manifestId);
            expect(
              awaiting.value[ForeignTipReconciliationsDB.Columns.EVIDENCE_KIND],
            ).toBe(ForeignTipReconciliationsDB.EvidenceKind.Pending);

            const payload = Buffer.from("d8799f4101ff", "hex");
            const daIdentity = {
              headerHash: Buffer.from(foreignHeaderHash, "hex"),
              schemaVersion: 1 as const,
              consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
              payloadCbor: payload,
              payloadSha256: createHash("sha256").update(payload).digest(),
            };
            yield* ForeignTipReconciliationsDB.markResolved({
              foreignHeaderHash,
              deploymentMarker,
              evidence: {
                kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
                daIdentity,
              },
            });
            yield* ForeignTipReconciliationsDB.markResolved({
              foreignHeaderHash,
              deploymentMarker,
              evidence: {
                kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
                daIdentity,
              },
            });

            const resolved =
              yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                foreignHeaderHash,
              );
            expect(resolved._tag).toBe("Some");
            if (Option.isNone(resolved)) return;
            expect(
              resolved.value[ForeignTipReconciliationsDB.Columns.EVIDENCE_KIND],
            ).toBe(ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa);
            expect(
              resolved.value[
                ForeignTipReconciliationsDB.Columns.VERIFIED_DA_PAYLOAD_SHA256
              ],
            ).toEqual(daIdentity.payloadSha256);

            const substitutedPayload = Buffer.from("d8799f4102ff", "hex");
            const substituted = yield* Effect.either(
              ForeignTipReconciliationsDB.markResolved({
                foreignHeaderHash,
                deploymentMarker,
                evidence: {
                  kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
                  daIdentity: {
                    ...daIdentity,
                    payloadCbor: substitutedPayload,
                    payloadSha256: createHash("sha256")
                      .update(substitutedPayload)
                      .digest(),
                  },
                },
              }),
            );
            expect(substituted._tag).toBe("Left");

            const wrongDeployment = yield* Effect.either(
              ForeignTipReconciliationsDB.markResolved({
                foreignHeaderHash,
                deploymentMarker: makeDeploymentMarker("ff".repeat(32)),
                evidence: {
                  kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
                  daIdentity,
                },
              }),
            );
            expect(wrongDeployment._tag).toBe("Left");
          }),
        ),
    );
  });
};
