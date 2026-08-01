import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  buildListenRouter,
  encodePipelineStatusOldestActive,
  PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES,
  type PipelineStatusOldestActiveRow,
} from "@/commands/listen-router.js";
import { PendingBlockFinalizationsDB } from "@/database/index.js";
import { Globals } from "@/services/index.js";

const ACTIVE_STATUSES = [
  PendingBlockFinalizationsDB.Status.PendingSubmission,
  PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
  PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
  PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
] as const;

describe("GET /pipeline-status pending-finalization reporting", () => {
  it("returns the oldest active journal through the real HTTP route and binds every active status", async () => {
    expect(PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES).toEqual(
      ACTIVE_STATUSES,
    );
    expect(PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES).not.toContain(
      PendingBlockFinalizationsDB.Status.Finalized,
    );
    expect(PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES).not.toContain(
      PendingBlockFinalizationsDB.Status.Abandoned,
    );

    const oldestActive: PipelineStatusOldestActiveRow = {
      header_hash: "11".repeat(28),
      submitted_tx_hash: "22".repeat(32),
      status: PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
      created_at: new Date(Date.now() - 5_000),
      updated_at: new Date(),
      observed_confirmed_at_ms: null,
    };
    const activeStatusesSeen: string[][] = [];
    const sql = Object.assign(
      ((
        strings: TemplateStringsArray | string,
        ..._values: readonly unknown[]
      ) => {
        if (typeof strings === "string") {
          return strings;
        }
        const query = `${strings.join(" ")} ${_values.map(String).join(" ")}`;
        if (query.includes("SELECT NOW() AS now")) {
          return Effect.succeed([{ now: new Date() }]);
        }
        if (query.includes("state_queue_mutation_leases")) {
          return Effect.succeed([]);
        }
        if (query.includes("GROUP BY status")) {
          return Effect.succeed([{ status: oldestActive.status, count: 1n }]);
        }
        if (query.includes("ORDER BY created_at ASC")) {
          return Effect.succeed([oldestActive]);
        }
        if (query.includes("FROM mempool")) {
          return Effect.succeed([{ count: 0n }]);
        }
        if (query.includes("FROM processed_mempool")) {
          return Effect.succeed([{ count: 0n }]);
        }
        return Effect.succeed([{ count: 0n }]);
      }) as unknown as SqlClient.SqlClient,
      {
        in: vi.fn((values: readonly unknown[]) => {
          activeStatusesSeen.push(values.map(String));
          return "active-statuses";
        }),
      },
    );
    const globals = {
      BLOCKS_IN_QUEUE: Effect.runSync(Ref.make(3)),
      LOCAL_FINALIZATION_PENDING: Effect.runSync(Ref.make(false)),
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: Effect.runSync(Ref.make("")),
      UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: Effect.runSync(Ref.make(0)),
    } as unknown as Globals;

    const response = await Effect.runPromise(
      buildListenRouter().pipe(
        Effect.provideService(
          HttpServerRequest.HttpServerRequest,
          HttpServerRequest.fromWeb(
            new Request("http://midgard.test/pipeline-status"),
          ),
        ),
        Effect.provideService(SqlClient.SqlClient, sql),
        Effect.provideService(Globals, globals),
      ) as Effect.Effect<HttpServerResponse.HttpServerResponse>,
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = (await webResponse.json()) as {
      readonly pendingBlockFinalizations: {
        readonly oldestActive: Record<string, unknown> | null;
      };
    };

    expect(webResponse.status).toBe(200);
    expect(body.pendingBlockFinalizations.oldestActive).toMatchObject({
      headerHash: oldestActive.header_hash,
      submittedTxHash: oldestActive.submitted_tx_hash,
      status: oldestActive.status,
    });
    expect(activeStatusesSeen).toEqual([ACTIVE_STATUSES.map(String)]);
  });

  it.each(ACTIVE_STATUSES)(
    "encodes an active %s row instead of reporting no active journal",
    (status) => {
      const row: PipelineStatusOldestActiveRow = {
        header_hash: "11".repeat(28),
        submitted_tx_hash: "22".repeat(32),
        status,
        created_at: new Date("2026-07-29T00:00:00.000Z"),
        updated_at: new Date("2026-07-29T00:00:01.000Z"),
        observed_confirmed_at_ms:
          status === PendingBlockFinalizationsDB.Status.ObservedWaitingStability
            ? Date.parse("2026-07-29T00:00:02.000Z")
            : null,
      };

      expect(
        encodePipelineStatusOldestActive(
          row,
          new Date("2026-07-29T00:00:05.000Z"),
        ),
      ).toMatchObject({
        headerHash: row.header_hash,
        submittedTxHash: row.submitted_tx_hash,
        status,
        ageMs: 5_000,
      });
    },
  );

  it("reports null only when the active query returns no row", () => {
    expect(
      encodePipelineStatusOldestActive(
        undefined,
        new Date("2026-07-29T00:00:05.000Z"),
      ),
    ).toBeNull();
  });
});
