import "./utils.js";

import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildListenRouter,
  encodePipelineStatusOldestActive,
  PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES,
  type PipelineStatusOldestActiveRow,
} from "../src/commands/listen-router.js";
import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { BatchSql } from "../src/services/database.js";
import { Globals } from "../src/services/index.js";
import { provideDatabaseLayers } from "./utils.js";

/**
 * Independent definition of "active": every journal status that is not one of
 * the two terminal ones. Derived from the status enum by exclusion rather than
 * copied from the route's own tuple, so a status added to the enum is active
 * here until someone deliberately marks it terminal.
 */
const TERMINAL_STATUSES: readonly PendingBlockFinalizationsDB.Status[] = [
  PendingBlockFinalizationsDB.Status.Finalized,
  PendingBlockFinalizationsDB.Status.Abandoned,
];
const EXPECTED_ACTIVE_STATUSES = Object.values(
  PendingBlockFinalizationsDB.Status,
).filter((status) => !TERMINAL_STATUSES.includes(status));

const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const ZERO_ROOT = "00".repeat(32);

/**
 * The smallest row the schema accepts, so the route's real query runs against
 * real rows. Only the columns the route reads vary per scenario.
 */
const journalRow = ({
  headerHash,
  status,
  createdAt,
  submittedTxHash,
}: {
  readonly headerHash: string;
  readonly status: PendingBlockFinalizationsDB.Status;
  readonly createdAt: Date;
  readonly submittedTxHash: string | null;
}) => ({
  header_hash: Buffer.from(headerHash, "hex"),
  submitted_tx_hash:
    submittedTxHash === null ? null : Buffer.from(submittedTxHash, "hex"),
  block_end_time: createdAt,
  status,
  observed_confirmed_at_ms: null,
  created_at: createdAt,
  updated_at: createdAt,
  state_queue_lease_token: `pipeline-status-test:${headerHash.slice(0, 8)}`,
  base_snapshot_id: "pipeline-status-test",
  base_tail_out_ref: "base#0",
  base_tail_header_hash: Buffer.from("bb".repeat(28), "hex"),
  base_tail_datum_cbor: "d87980",
  base_utxos_root: ZERO_ROOT,
  base_transactions_root: ZERO_ROOT,
  base_deposits_root: ZERO_ROOT,
  base_withdrawals_root: ZERO_ROOT,
  block_start_time: createdAt,
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
  deployment_manifest_id: "de".repeat(32),
  expected_transition_trace_root: ZERO_ROOT,
  expected_event_to_step_root: ZERO_ROOT,
  expected_withdrawal_count: 0n,
  expected_forced_transaction_count: 0n,
  expected_l2_transaction_count: 0n,
  expected_deposit_count: 0n,
  expected_total_event_count: 0n,
  expected_transition_step_count: 0n,
  consensus_profile_id: "midgard-consensus-v1",
  expected_validation_traces_root: EMPTY_MERKLE_ROOT,
  expected_validation_trace_count: 0n,
  ledger_delta_spent: "[]",
  ledger_delta_produced: "[]",
});

const getPipelineStatus = Effect.gen(function* () {
  const response = (yield* buildListenRouter().pipe(
    Effect.provideService(
      HttpServerRequest.HttpServerRequest,
      HttpServerRequest.fromWeb(
        new Request("http://midgard.test/pipeline-status"),
      ),
    ),
  )) as HttpServerResponse.HttpServerResponse;
  const webResponse = HttpServerResponse.toWeb(response);
  const body = (yield* Effect.promise(() => webResponse.json())) as {
    readonly pendingBlockFinalizations: {
      readonly countsByStatus: Record<string, string>;
      readonly oldestActive: Record<string, unknown> | null;
    };
  };
  return { status: webResponse.status, body };
});

/**
 * Runs against the per-worker Postgres shard with an empty journal table. The
 * cast narrows the router's declared requirements to the ones this route
 * actually uses: `/pipeline-status` reads the journal through SqlClient and
 * never touches the L1 wallet, contract, or validation services the wider
 * router declares.
 */
const runAgainstShard = <A, E, R>(program: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* BatchSql;
        yield* sql`DELETE FROM pending_block_finalizations`;
        return yield* (
          program as Effect.Effect<A, E, SqlClient.SqlClient>
        ).pipe(Effect.provideService(SqlClient.SqlClient, sql));
      }).pipe(Effect.provide(Globals.Default)),
    ) as Effect.Effect<A, E, never>,
  );

describe("GET /pipeline-status pending-finalization reporting", () => {
  it("binds exactly the non-terminal journal statuses", () => {
    expect(
      [...PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES].sort(),
    ).toEqual([...EXPECTED_ACTIVE_STATUSES].sort());
  });

  it("reports the active journal through the real query even when older terminal journals exist", async () => {
    // Anchored ten minutes back so the reported age is checked against a
    // wide, unambiguous band: a scheduling hiccup cannot move it, but an age
    // taken from the wrong column, the wrong row, or the wrong sign falls
    // outside it.
    const ACTIVE_AGE_MS = 600_000;
    const activeCreatedAt = new Date(Date.now() - ACTIVE_AGE_MS);
    const result = await runAgainstShard(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO pending_block_finalizations ${sql.insert([
          journalRow({
            headerHash: "aa".repeat(28),
            status: PendingBlockFinalizationsDB.Status.Finalized,
            createdAt: new Date(Date.now() - 3_600_000),
            submittedTxHash: "cc".repeat(32),
          }),
          journalRow({
            headerHash: "ab".repeat(28),
            status: PendingBlockFinalizationsDB.Status.Abandoned,
            createdAt: new Date(Date.now() - 1_800_000),
            submittedTxHash: null,
          }),
          journalRow({
            headerHash: "11".repeat(28),
            status: PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
            createdAt: activeCreatedAt,
            submittedTxHash: "22".repeat(32),
          }),
        ] as never)}`;
        return yield* getPipelineStatus;
      }),
    );

    expect(result.status).toBe(200);
    expect(result.body.pendingBlockFinalizations.countsByStatus).toEqual({
      finalized: "1",
      abandoned: "1",
      submitted_unconfirmed: "1",
    });
    const oldestActive = result.body.pendingBlockFinalizations.oldestActive;
    expect(oldestActive).toMatchObject({
      headerHash: "11".repeat(28),
      submittedTxHash: "22".repeat(32),
      status: PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
      createdAt: activeCreatedAt.toISOString(),
      observedConfirmedAt: null,
    });
    expect(Number(oldestActive?.ageMs)).toBeGreaterThanOrEqual(ACTIVE_AGE_MS);
    expect(Number(oldestActive?.ageMs)).toBeLessThan(ACTIVE_AGE_MS + 60_000);
  });

  it("reports no active journal when only terminal journals remain", async () => {
    const result = await runAgainstShard(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO pending_block_finalizations ${sql.insert([
          journalRow({
            headerHash: "aa".repeat(28),
            status: PendingBlockFinalizationsDB.Status.Finalized,
            createdAt: new Date(Date.now() - 3_600_000),
            submittedTxHash: "cc".repeat(32),
          }),
          journalRow({
            headerHash: "ab".repeat(28),
            status: PendingBlockFinalizationsDB.Status.Abandoned,
            createdAt: new Date(Date.now() - 1_800_000),
            submittedTxHash: null,
          }),
        ] as never)}`;
        return yield* getPipelineStatus;
      }),
    );

    expect(result.status).toBe(200);
    expect(result.body.pendingBlockFinalizations.oldestActive).toBeNull();
    expect(result.body.pendingBlockFinalizations.countsByStatus).toEqual({
      finalized: "1",
      abandoned: "1",
    });
  });

  // Driven by the independent derivation, not by the route's own tuple: a
  // status dropped from the route must fail here as a journal the route stops
  // reporting, not merely as a case that stops being generated.
  it.each(EXPECTED_ACTIVE_STATUSES)(
    "reports an active %s journal rather than nothing",
    async (status) => {
      const result = await runAgainstShard(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO pending_block_finalizations ${sql.insert([
            journalRow({
              headerHash: "11".repeat(28),
              status,
              createdAt: new Date(Date.now() - 1_000),
              submittedTxHash: null,
            }),
          ] as never)}`;
          return yield* getPipelineStatus;
        }),
      );

      expect(result.body.pendingBlockFinalizations.oldestActive).toMatchObject({
        headerHash: "11".repeat(28),
        status,
      });
    },
  );

  it("derives ageMs and observedConfirmedAt from the stored row", () => {
    const row: PipelineStatusOldestActiveRow = {
      header_hash: "11".repeat(28),
      submitted_tx_hash: "22".repeat(32),
      status: PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
      created_at: new Date("2026-07-29T00:00:00.000Z"),
      updated_at: new Date("2026-07-29T00:00:01.000Z"),
      observed_confirmed_at_ms: Date.parse("2026-07-29T00:00:02.000Z"),
    };

    expect(
      encodePipelineStatusOldestActive(
        row,
        new Date("2026-07-29T00:00:05.000Z"),
      ),
    ).toStrictEqual({
      headerHash: "11".repeat(28),
      submittedTxHash: "22".repeat(32),
      status: PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
      ageMs: 5_000,
      createdAt: "2026-07-29T00:00:00.000Z",
      updatedAt: "2026-07-29T00:00:01.000Z",
      observedConfirmedAt: "2026-07-29T00:00:02.000Z",
    });
  });

  it("reports null only when the active query returns no row", () => {
    expect(
      encodePipelineStatusOldestActive(
        undefined,
        new Date("2026-07-29T00:00:05.000Z"),
      ),
    ).toBeNull();
  });
});
