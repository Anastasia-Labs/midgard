/**
 * The settlement worker's backoff for a failing status read of the attempt
 * that blocks new work: the job's own schedule, never its failure count.
 */
import { randomUUID } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SqlClient } from "@effect/sql";
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import {
  type SettlementHealth,
  settlementTick,
  settlementWalletAddress,
} from "../src/services/settlement.js";
import {
  clearSettlementReconcileFailures,
  recordSettlementReconcileFailure,
  SETTLEMENT_JOB_FAILURE_CAP,
  SETTLEMENT_RECONCILE_MAX_BACKOFF_MS,
  settlementReconcileBackoff,
  settlementReconcileStuck,
  settlementReconcileWait,
} from "../src/services/settlement-retry.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import { pendingSettlementAttempt } from "./helpers/settlement-pending-attempt.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

vi.mock("../src/l1-provider-view.js", async (original) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await original<object>()),
    // The L1 view point, past the pending body's validity bound.
    providerViewPoint: () => E.succeed({ slot: 10, id: "b1".repeat(32) }),
  };
});

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));
const deploymentId = "a3".repeat(32);

beforeEach(() =>
  run(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      yield* openFollowerWriteGate;
    }),
  ),
);

describe("settlement reconcile backoff", () => {
  it("backs off a failing status read without counting it against the job", async () => {
    const statusReads = vi.fn(() =>
      Promise.reject(new Error("socket hang up")),
    );
    const result = await run(
      withoutFollowerJournal(
        Effect.gen(function* () {
          const seed = generateSeedPhrase();
          const config = {
            ...(yield* NodeConfig),
            L1_SETTLEMENT_SEED_PHRASE: seed,
          };
          const owner = {
            deploymentId,
            walletAddress: settlementWalletAddress(config),
            token: randomUUID(),
          };
          // Past its validity bound at the indexer tip (slot 10): every
          // reconcile reads the body's status first.
          const attempt = pendingSettlementAttempt({
            deploymentId,
            eventId: "cc".repeat(34),
            ttl: 5n,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
            VALUES (${deploymentId}, ${attempt.kind}, ${attempt.event_id}, ${attempt.phase})`;
          yield* Journal.renew(owner);
          yield* Journal.saveAttempt(owner, attempt, Effect.void);
          const backoff = settlementReconcileBackoff();
          const reports: SettlementHealth[] = [];
          const api = {
            transactionStatus: statusReads,
          } as unknown as Parameters<typeof settlementTick>[1];
          const tick = settlementTick(
            owner,
            api,
            (health) => reports.push(health),
            backoff,
          ).pipe(
            Effect.provideService(NodeConfig, config),
            Effect.provideService(Lucid, new Lucid({} as unknown as Lucid)),
            Effect.provideService(ContractDeploymentIdentity, {
              kind: "manifest",
              manifestId: deploymentId,
              l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
            } as unknown as ContractDeploymentIdentity),
            Effect.provideService(
              MidgardContracts,
              {} as unknown as MidgardContracts,
            ),
            Effect.exit,
          );
          const first = yield* tick;
          const firstDue = backoff.dueAt - Date.now();
          // Within the backoff the tick reports the wait and reads nothing.
          const waiting = yield* tick;
          backoff.dueAt = 0;
          const second = yield* tick;
          const secondDue = backoff.dueAt - Date.now();
          const jobs = yield* sql<{
            failures: number;
          }>`SELECT failures FROM settlement_jobs`;
          return {
            first,
            firstDue,
            waiting,
            second,
            secondDue,
            failures: backoff.failures,
            reports,
            jobFailures: jobs[0]?.failures,
          };
        }),
      ),
    );
    expect(result.first._tag).toBe("Failure");
    expect(result.firstDue).toBeGreaterThan(4_000);
    expect(result.firstDue).toBeLessThanOrEqual(5_000);
    expect(result.waiting._tag).toBe("Success");
    expect(result.reports).toHaveLength(1);
    expect(result.reports[0]!.state).toBe("error");
    // Waiting is not a completed tick: it must not clear a failure streak.
    expect(result.reports[0]!.tickCompleted).toBe(false);
    expect(result.reports[0]!.detail).toMatch(
      /^settlement reconcile retry at .*: settlement withdrawal .* initialize reconcile: transactionStatus .*: socket hang up/u,
    );
    expect(result.second._tag).toBe("Failure");
    expect(result.secondDue).toBeGreaterThan(9_000);
    expect(result.secondDue).toBeLessThanOrEqual(10_000);
    expect(statusReads).toHaveBeenCalledTimes(2);
    expect(result.failures).toBe(2);
    // The job's own failure count, which makes it stuck, never moves.
    expect(result.jobFailures).toBe(0);
  }, 30_000);

  it("polls a failing status read at most every five minutes, stuck from the cap, and restarts for another attempt", () => {
    const backoff = settlementReconcileBackoff();
    expect(settlementReconcileWait(backoff, "aa", 1_000)).toBeUndefined();
    const waits: number[] = [];
    for (let failure = 0; failure < SETTLEMENT_JOB_FAILURE_CAP; failure++) {
      expect(settlementReconcileStuck(backoff)).toBeUndefined();
      recordSettlementReconcileFailure(backoff, 1_000, "down");
      waits.push(backoff.dueAt - 1_000);
    }
    expect(waits).toEqual([
      5_000, 10_000, 20_000, 40_000, 80_000, 160_000, 300_000, 300_000,
    ]);
    expect(SETTLEMENT_RECONCILE_MAX_BACKOFF_MS).toBe(300_000);
    expect(settlementReconcileStuck(backoff)).toEqual({
      txHash: "aa",
      failures: SETTLEMENT_JOB_FAILURE_CAP,
      lastError: "down",
    });
    // A success clears the count.
    clearSettlementReconcileFailures(backoff);
    expect(settlementReconcileWait(backoff, "aa", 1_000)).toBeUndefined();
    recordSettlementReconcileFailure(backoff, 1_000, "down");
    expect(settlementReconcileWait(backoff, "aa", 1_000)).toBe(6_000);
    // Another blocking attempt starts its own schedule.
    expect(settlementReconcileWait(backoff, "bb", 1_000)).toBeUndefined();
    expect(backoff).toMatchObject({ txHash: "bb", failures: 0 });
  });
});
