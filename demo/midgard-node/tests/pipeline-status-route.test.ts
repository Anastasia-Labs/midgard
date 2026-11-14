import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildListenRouter,
  encodePipelineStatusOldestActive,
  type PipelineStatusOldestActiveRow,
} from "../src/commands/listen-router.js";
import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { BatchSql } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/index.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  insertOwnJournal,
  simCommitTxHash,
} from "./helpers/landed-blocks-sim.own.js";
import { land, ownLandedRow } from "./helpers/own-block-facts.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const ZERO_ROOT = "00".repeat(32);
const S = PendingBlockFinalizationsDB.OwnBlockStatus;
const FRONTIER = "0f".repeat(28);
const APPLIED = "aa".repeat(28);
const DEAD = "ab".repeat(28);
const OUTSTANDING = "11".repeat(28);

/**
 * The signed journal of own block `headerHash` on `base`, written
 * `minutesAgo` back, and its landed row if it landed. Its status is
 * derived from these facts, as the route reads it.
 */
const ownBlock = (
  headerHash: string,
  base: string,
  minutesAgo: number,
  landed?: { readonly applied: boolean },
) =>
  Effect.gen(function* () {
    yield* withFollowerWrite(
      Frontier.upsert({ headerHash: FRONTIER, utxosRoot: ZERO_ROOT }),
    );
    yield* insertOwnJournal({
      headerHash,
      baseHeaderHash: base,
      baseUtxosRoot: ZERO_ROOT,
      expectedUtxosRoot: ZERO_ROOT,
      spent: [],
      produced: [],
      txIds: [],
      at: new Date(Date.now() - minutesAgo * 60_000),
    });
    if (landed !== undefined)
      yield* land(ownLandedRow(headerHash, base, landed));
  });

/**
 * A folded journal (the frontier's block), an applied one on the frontier,
 * one dead by the applied block's taking its base slot, and, given its age
 * in minutes, an outstanding one built on the applied block.
 */
const ownChain = (outstandingMinutesAgo?: number) =>
  Effect.gen(function* () {
    yield* ownBlock(FRONTIER, "0e".repeat(28), 90);
    yield* ownBlock(APPLIED, FRONTIER, 60, { applied: true });
    yield* ownBlock(DEAD, FRONTIER, 30);
    if (outstandingMinutesAgo !== undefined)
      yield* ownBlock(OUTSTANDING, APPLIED, outstandingMinutesAgo);
  });

const getPipelineStatus = Effect.gen(function* () {
  const response = (yield* buildListenRouter().pipe(
    Effect.provideService(
      HttpServerRequest.HttpServerRequest,
      HttpServerRequest.fromWeb(
        new Request("http://midgard.test/pipeline-status"),
      ).modify({ remoteAddress: "127.0.0.1" }),
    ),
  )) as HttpServerResponse.HttpServerResponse;
  const webResponse = HttpServerResponse.toWeb(response);
  const body = (yield* Effect.promise(() => webResponse.json())) as {
    readonly pendingBlockFinalizations: {
      readonly countsByStatus: Record<string, string>;
      readonly live: string;
      readonly oldestActive: Record<string, unknown> | null;
    };
    readonly settlement: {
      readonly unfinishedJobs: string;
      readonly failingJobs: readonly Record<string, unknown>[];
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
const runAgainstShard = <A, E, R>(
  program: Effect.Effect<A, E, R>,
  manifestId?: string,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* BatchSql;
        return yield* Effect.zipRight(
          resetApplicationTables,
          program as Effect.Effect<A, E, SqlClient.SqlClient>,
        ).pipe(Effect.provideService(SqlClient.SqlClient, sql));
      }).pipe(
        Effect.provide(Globals.Default),
        // The settlement backlog is the deployment manifest's.
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: manifestId === undefined ? "derived" : "manifest",
            ...(manifestId === undefined ? {} : { manifestId }),
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
      ),
    ) as Effect.Effect<A, E, never>,
  );

describe("GET /pipeline-status pending-finalization reporting", () => {
  it("counts every journal under its derived status and reports the outstanding one even when older folded, applied and dead journals exist", async () => {
    // Anchored ten minutes back so the reported age is checked against a
    // wide, unambiguous band: a scheduling hiccup cannot move it, but an age
    // taken from the wrong column, the wrong row, or the wrong sign falls
    // outside it.
    const ACTIVE_AGE_MS = 600_000;
    const result = await runAgainstShard(
      Effect.gen(function* () {
        yield* ownChain(ACTIVE_AGE_MS / 60_000);
        return yield* getPipelineStatus;
      }),
    );

    expect(result.status).toBe(200);
    const counts = result.body.pendingBlockFinalizations.countsByStatus;
    expect(counts).toEqual({
      [S.Unlanded]: "2",
      [S.Landed]: "0",
      [S.Applied]: "1",
      [S.Folded]: "1",
    });
    expect(
      Object.values(counts).reduce((sum, count) => sum + Number(count), 0),
    ).toBe(4);
    expect(result.body.pendingBlockFinalizations.live).toBe("1");
    const oldestActive = result.body.pendingBlockFinalizations.oldestActive;
    expect(oldestActive).toMatchObject({
      headerHash: OUTSTANDING,
      intendedTxHash: simCommitTxHash(Buffer.from(OUTSTANDING, "hex")).toString(
        "hex",
      ),
      status: S.Unlanded,
    });
    expect(Number(oldestActive?.ageMs)).toBeGreaterThanOrEqual(ACTIVE_AGE_MS);
    expect(Number(oldestActive?.ageMs)).toBeLessThan(ACTIVE_AGE_MS + 60_000);
  });

  it("reports no outstanding journal when only folded, applied and dead journals remain", async () => {
    const result = await runAgainstShard(
      Effect.gen(function* () {
        yield* ownChain();
        return yield* getPipelineStatus;
      }),
    );

    expect(result.status).toBe(200);
    expect(result.body.pendingBlockFinalizations.oldestActive).toBeNull();
    expect(result.body.pendingBlockFinalizations.live).toBe("0");
    expect(result.body.pendingBlockFinalizations.countsByStatus).toEqual({
      [S.Unlanded]: "1",
      [S.Landed]: "0",
      [S.Applied]: "1",
      [S.Folded]: "1",
    });
  });

  it("reports a landed journal the rebase has not applied yet as outstanding, and not live", async () => {
    const result = await runAgainstShard(
      Effect.gen(function* () {
        yield* ownBlock(OUTSTANDING, FRONTIER, 1, { applied: false });
        return yield* getPipelineStatus;
      }),
    );

    expect(result.body.pendingBlockFinalizations.oldestActive).toMatchObject({
      headerHash: OUTSTANDING,
      status: S.Landed,
    });
    expect(result.body.pendingBlockFinalizations.live).toBe("0");
  });

  it("derives ageMs from the stored row", () => {
    const row: PipelineStatusOldestActiveRow = {
      header_hash: "11".repeat(28),
      intended_tx_hash: "22".repeat(32),
      status: S.Landed,
      created_at: new Date("2026-07-29T00:00:00.000Z"),
      updated_at: new Date("2026-07-29T00:00:01.000Z"),
    };

    expect(
      encodePipelineStatusOldestActive(
        row,
        new Date("2026-07-29T00:00:05.000Z"),
      ),
    ).toStrictEqual({
      headerHash: "11".repeat(28),
      intendedTxHash: "22".repeat(32),
      status: S.Landed,
      ageMs: 5_000,
      createdAt: "2026-07-29T00:00:00.000Z",
      updatedAt: "2026-07-29T00:00:01.000Z",
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

  it("counts the deployment's unfinished settlement jobs and names the failing ones with their last error", async () => {
    const deploymentId = "a1".repeat(32);
    const unpayable =
      "No spendable reserve UTxO can fund the payout of withdrawal 02";
    const result = await runAgainstShard(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const job = (
          deployment: string,
          kind: "deposit" | "withdrawal",
          eventId: string,
          phase: string,
          lastError: string | null,
        ) => sql`INSERT INTO settlement_jobs
          (deployment_id, kind, event_id, phase, last_error, failures)
          VALUES (${deployment}, ${kind}, ${eventId}, ${phase}, ${lastError},
            ${lastError === null ? 0 : 3})`;
        yield* job(deploymentId, "deposit", "01", "absorb", null);
        yield* job(deploymentId, "withdrawal", "02", "fund", unpayable);
        yield* job(deploymentId, "withdrawal", "03", "complete", null);
        // Another deployment's leftovers are not this node's backlog.
        yield* job("b2".repeat(32), "withdrawal", "04", "fund", "stale");
        return yield* getPipelineStatus;
      }),
      deploymentId,
    );

    expect(result.status).toBe(200);
    expect(result.body.settlement.unfinishedJobs).toBe("2");
    expect(result.body.settlement.failingJobs).toEqual([
      {
        kind: "withdrawal",
        eventId: "02",
        phase: "fund",
        failures: 3,
        lastError: unpayable,
        dueAt: expect.any(String),
      },
    ]);
  });
});
