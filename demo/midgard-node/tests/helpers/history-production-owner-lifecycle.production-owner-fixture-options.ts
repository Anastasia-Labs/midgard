import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { HistoryTransportOptions } from "../../src/l1-event-history-transport.js";
import { Database } from "../../src/services/database.js";
import { makeProductionEventHistoryOwner } from "../../src/services/event-history-runtime.js";
import { Globals } from "../../src/services/globals.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import { testDatabaseName } from "../test-env.js";
import { makeStreamingHistoryTransport } from "./history-source-owner-emulator.js";

/** One actual deployment, real production reconciliation and native MPF owner.
 * Only the network transport's genesis, point identifiers and ancestry are synthetic. */
export type ProductionOwnerFixtureOptions = {
  readonly eventHistoryProtectionDurationMs?: bigint;
  readonly transportFactory?: (
    recorded: Parameters<typeof makeStreamingHistoryTransport>[0],
  ) => Omit<ReturnType<typeof makeStreamingHistoryTransport>, "options"> & {
    readonly options: Omit<HistoryTransportOptions, "signal">;
  };
  /** The deployment's automatic rollback horizon k as the production owner
   * reads it (manifest l1Finality.automaticRecoveryMaxDepth), overridden in
   * this fixture's in-memory identity only; retention scenarios need a k
   * smaller than the 2160 the manifest pins. The manifest id is unchanged. */
  readonly rollbackHorizon?: number;
  /** Runs first in every startup completion preparation, before the fixture's
   * listen-startup steps: observes the state a completion starts from. */
  readonly beforeCompletion?: (
    generation: number,
    globals: Globals,
  ) => Effect.Effect<void, unknown, SqlClient.SqlClient>;
  readonly afterNativePreparation?: NonNullable<
    Parameters<
      typeof makeProductionEventHistoryOwner<
        unknown,
        SqlClient.SqlClient | Globals | MempoolLedgerCache
      >
    >[0]["prepareCompletion"]
  >;
};

type HistoryAuthorityRow = {
  readonly owner_token: string;
  readonly generation: string;
  readonly state: string;
  readonly reason: string;
  readonly live: boolean;
  readonly remaining_ms: string;
  readonly updated_at: string;
};

export const readHistoryAuthority = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<HistoryAuthorityRow>`SELECT owner_token,
        generation::text AS generation, state, reason,
        state <> 'suspended' AND lease_until > clock_timestamp() AS live,
        round(extract(epoch FROM lease_until - clock_timestamp()) * 1000)::text
          AS remaining_ms, updated_at::text AS updated_at
        FROM event_history_authority WHERE singleton = true`;
      return rows[0];
    }).pipe(Effect.provide(Database.layer)),
  );

/**
 * A restart starts the next generation only after the stopped one gave up
 * the history authority. Its close releases the lease before it returns, so
 * the lease is never waited out: the stopped owner gets a short grace, and a
 * lease held by any other token means another process shares this worker's
 * database shard. Either way the next owner could only fail with "History
 * authority still has a live owner", so the restart refuses here with the
 * exact holder instead.
 */
export const awaitHistoryAuthorityReleased = async (
  stoppedHolder: string | undefined,
) => {
  // Date is faked by the emulator fixture; measure real elapsed time.
  const deadline = performance.now() + 2_000;
  for (;;) {
    const row = await readHistoryAuthority();
    if (row === undefined || !row.live) return;
    if (row.owner_token !== stoppedHolder)
      throw new Error(
        `History authority lease is held by ${row.owner_token} (generation ${row.generation}, ${row.state}), not the stopped owner ${stoppedHolder ?? "none"}: another process is using test database ${testDatabaseName()}`,
      );
    if (performance.now() >= deadline)
      throw new Error(
        `Stopped history owner ${row.owner_token} did not release its authority lease (generation ${row.generation}, ${row.state}: ${row.reason}, ${row.remaining_ms} ms left)`,
      );
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
};

/** Real time a refused recovery must keep the gate closed after the owner
 * journals a new source point: long enough for its convergence attempt. */
export const GATE_CLOSED_SETTLE_MS = 1_500;
