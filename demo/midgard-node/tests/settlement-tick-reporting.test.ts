/**
 * What the settlement worker reports when a provider call on its path fails:
 * the call's operation and the provider's own words, never Effect's bare
 * "An unknown error occurred in Effect.tryPromise". Runs the worker program
 * against the journal, with a pending settlement body and a stubbed L1.
 */
import { randomUUID } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SqlClient } from "@effect/sql";
import { generateSeedPhrase, OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { Cause, Effect, Fiber } from "effect";
import { UnknownException } from "effect/Cause";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
import {
  IntentJournal,
  IntentJournalWithoutFollower,
} from "../src/services/intent-journal.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import {
  type SettlementHealth,
  settlementProgram,
  settlementWalletAddress,
} from "../src/services/settlement.js";
import {
  settlementCall,
  settlementCauseDetail,
} from "../src/services/settlement-call.js";
import {
  settlementReconcileStuckDetail,
  settlementStuckDetail,
} from "../src/services/settlement-readiness.js";
import {
  SETTLEMENT_JOB_FAILURE_CAP,
  SETTLEMENT_STUCK_RETRY_MS,
  type SettlementReconcileBackoff,
  settlementRetryDelayMs,
} from "../src/services/settlement-retry.js";
import { runSettlementWorker } from "../src/workers/settlement.run-settlement-worker.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import { pendingSettlementAttempt } from "./helpers/settlement-pending-attempt.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

vi.mock("../src/l1-provider-view.js", async (original) => {
  const { Effect: E } = await import("effect");
  return {
    ...(await original<object>()),
    // The L1 view point, below the pending body's validity bound.
    providerViewPoint: () => E.succeed({ slot: 10, id: "b1".repeat(32) }),
  };
});

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));
/** The stub L1 client's selected wallet (one object, as Lucid keeps it). */
const stubWallet = {};
const deploymentId = "a2".repeat(32);
const UNKNOWN = "An unknown error occurred";
/** A withdrawal event id as the journal keys it: the event's bytes in hex. */
const EVENT_ID = "cc".repeat(34);

beforeEach(() =>
  run(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      yield* openFollowerWriteGate;
    }),
  ),
);

/** A signed-looking withdrawal initialize body, valid until slot `ttl`. */
const pendingAttempt = (ttl = 120n): Journal.SettlementAttempt =>
  pendingSettlementAttempt({ deploymentId, eventId: EVENT_ID, ttl });

type L1 = {
  readonly transactionStatus?: (hash: string) => Promise<unknown>;
  readonly submitTx: (cbor: string) => Promise<string>;
  readonly utxosAt?: (address: string) => Promise<unknown>;
  /** Queue the job with no journaled body, so the tick builds it. */
  readonly unbuilt?: boolean;
  /** The body's validity bound (default slot 120, above the indexer tip). */
  readonly ttl?: bigint;
  /** The job's consecutive failures before this tick (default 0). */
  readonly failures?: number;
  /** The worker run's reconcile backoff before this tick. */
  readonly reconcileBackoff?: SettlementReconcileBackoff;
};

/** Runs the worker program over one pending body (or one queued job) until
 * its first report after startup, and returns every report. */
const firstTick = (l1: (attempt: Journal.SettlementAttempt) => L1) =>
  Effect.gen(function* () {
    const seed = generateSeedPhrase();
    const config = { ...(yield* NodeConfig), L1_SETTLEMENT_SEED_PHRASE: seed };
    const token = randomUUID();
    const owner = {
      deploymentId,
      walletAddress: settlementWalletAddress(config),
      token,
    };
    const probe = l1(pendingAttempt());
    const attempt = pendingAttempt(probe.ttl);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
      VALUES (${deploymentId}, ${attempt.kind}, ${attempt.event_id}, ${attempt.phase})`;
    const {
      transactionStatus,
      submitTx,
      utxosAt,
      unbuilt,
      failures,
      reconcileBackoff,
    } = l1(attempt);
    yield* sql`UPDATE settlement_jobs SET failures = ${failures ?? 0}`;
    yield* Journal.renew(owner);
    if (unbuilt !== true)
      yield* Journal.saveAttempt(owner, attempt, Effect.void);
    const reports: SettlementHealth[] = [];
    const fiber = yield* Effect.fork(
      settlementProgram(
        (health) => reports.push(health),
        token,
        reconcileBackoff,
      ).pipe(
        Effect.provideService(NodeConfig, config),
        Effect.provideService(ContractDeploymentIdentity, {
          kind: "manifest",
          manifestId: deploymentId,
          l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
        } as unknown as ContractDeploymentIdentity),
        Effect.provideService(
          MidgardContracts,
          {} as unknown as MidgardContracts,
        ),
        Effect.provideService(
          Lucid,
          new Lucid({
            api: {
              selectWallet: { fromSeed: () => undefined },
              // `selectNodeWallet` keys the settlement wallet's signer by it.
              wallet: () => stubWallet,
              transactionStatus:
                transactionStatus ??
                (async (txHash: string) => ({ txHash, status: "not_found" })),
              config: () => ({ network: "Preprod", provider: { submitTx } }),
              utxosAt,
            },
          } as unknown as Lucid),
        ),
      ),
    );
    for (let polls = 0; polls < 200; polls++) {
      if (reports.some((report) => report.state !== "starting")) break;
      yield* Effect.sleep("25 millis");
    }
    yield* Fiber.interrupt(fiber);
    const report = reports.find((value) => value.state !== "starting");
    if (report === undefined)
      throw new Error(`no tick report: ${JSON.stringify(reports)}`);
    const jobs = yield* sql<{
      last_error: string | null;
      failures: number;
      due_in_ms: number;
    }>`SELECT last_error, failures,
      (EXTRACT(EPOCH FROM due_at - clock_timestamp()) * 1000)::float8 AS due_in_ms
      FROM settlement_jobs`;
    return {
      report,
      reports,
      attempt,
      owner,
      lastError: jobs[0]?.last_error,
      failures: jobs[0]?.failures,
      dueInMs: jobs[0]?.due_in_ms,
    };
  });

/** Runs `program` as the settlement worker thread runs its program, through
 * the worker entry's own runner, and returns what it posted, whether it
 * closed its port, and the exit code it set. */
const exitReport = async (program: Effect.Effect<unknown, unknown>) => {
  const posted: SettlementHealth[] = [];
  let closed = false;
  const previous = process.exitCode;
  process.exitCode = undefined;
  try {
    await runSettlementWorker(
      {
        postMessage: (health) => posted.push(health),
        close: () => {
          closed = true;
        },
      },
      program,
    );
    return { posted, closed, exitCode: process.exitCode };
  } finally {
    process.exitCode = previous;
  }
};

describe("settlement tick reporting", () => {
  it("names a job's failing build step in its stored error and the report", async () => {
    const { report, lastError } = await run(
      withoutFollowerJournal(
        firstTick(() => ({
          unbuilt: true,
          submitTx: () => Promise.reject(new Error("never reached")),
        })),
      ),
    );
    // The build's first step reads the payout from the event id, which
    // this probe's id does not decode as.
    expect(lastError).toMatch(
      new RegExp(
        `^settlement withdrawal ${EVENT_ID} initialize: Invalid --withdrawal-event-id: `,
        "u",
      ),
    );
    expect(report.state).toBe("error");
    expect(report.detail).toBe(lastError);
    expect(report.tickCompleted).toBeUndefined();
  }, 30_000);

  it("doubles the backoff up to the cap, then retries a stuck job hourly", () => {
    expect(
      Array.from({ length: SETTLEMENT_JOB_FAILURE_CAP + 1 }, (_, failures) =>
        settlementRetryDelayMs(failures),
      ),
    ).toEqual([
      5_000,
      10_000,
      20_000,
      40_000,
      80_000,
      160_000,
      320_000,
      SETTLEMENT_STUCK_RETRY_MS,
      SETTLEMENT_STUCK_RETRY_MS,
    ]);
  });

  it("backs off a failing job, then marks it stuck at the cap and moves on", async () => {
    const tick = (failures: number) =>
      run(
        withoutFollowerJournal(
          firstTick(() => ({
            unbuilt: true,
            failures,
            submitTx: () => Promise.reject(new Error("never reached")),
          })),
        ),
      );
    // Below the cap: the failure is counted and retried after its backoff.
    const early = await tick(2);
    expect(early.failures).toBe(3);
    expect(early.dueInMs).toBeGreaterThan(15_000);
    expect(early.dueInMs).toBeLessThanOrEqual(20_000);
    expect(
      await run(Journal.stuckJobs(deploymentId, SETTLEMENT_JOB_FAILURE_CAP)),
    ).toEqual({ count: 0, first: undefined });

    // The failure that reaches the cap: stuck, retried only hourly, and
    // named for /readyz.
    await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* openFollowerWriteGate;
      }),
    );
    const capped = await tick(SETTLEMENT_JOB_FAILURE_CAP - 1);
    expect(capped.failures).toBe(SETTLEMENT_JOB_FAILURE_CAP);
    expect(capped.dueInMs).toBeGreaterThan(SETTLEMENT_STUCK_RETRY_MS - 60_000);
    expect(capped.dueInMs).toBeLessThanOrEqual(SETTLEMENT_STUCK_RETRY_MS);
    const stuck = await run(
      Journal.stuckJobs(deploymentId, SETTLEMENT_JOB_FAILURE_CAP),
    );
    expect(stuck.count).toBe(1);
    expect(settlementStuckDetail(stuck)).toBe(
      `settlement_jobs_stuck:1:withdrawal:${EVENT_ID}:initialize:${(capped.lastError!.split("\n", 1)[0] ?? "").trim().slice(0, 160)}`,
    );

    // The node's other settlement jobs keep moving past the stuck one.
    const next = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
          VALUES (${deploymentId}, 'deposit', ${"dd".repeat(34)}, 'absorb')`;
        return yield* Journal.nextJob(capped.owner);
      }),
    );
    expect(next).toMatchObject({ kind: "deposit", event_id: "dd".repeat(34) });
  }, 60_000);

  it("names a failing status read with the provider's message", async () => {
    const { report, attempt } = await run(
      withoutFollowerJournal(
        firstTick(() => ({
          // Past its validity bound at the indexer tip (slot 10): the tick
          // reads its status before it may expire it.
          ttl: 5n,
          transactionStatus: () => Promise.reject(new Error("socket hang up")),
          submitTx: () => Promise.reject(new Error("never reached")),
        })),
      ),
    );
    expect(report.state).toBe("error");
    expect(report.detail).toContain(
      `settlement withdrawal ${EVENT_ID} initialize reconcile: transactionStatus ${attempt.tx_hash}: socket hang up`,
    );
    expect(report.detail).not.toContain(UNKNOWN);
  }, 30_000);

  it("names a blocking reconcile stuck at the cap, in a wait tick that does not complete", async () => {
    const statusReads = vi.fn(() => Promise.reject(new Error("never reached")));
    const { report } = await run(
      withoutFollowerJournal(
        firstTick((attempt) => ({
          ttl: 5n,
          transactionStatus: statusReads,
          submitTx: () => Promise.reject(new Error("never reached")),
          reconcileBackoff: {
            txHash: attempt.tx_hash,
            failures: SETTLEMENT_JOB_FAILURE_CAP,
            dueAt: Date.now() + 60_000,
            lastError: "socket hang up",
          },
        })),
      ),
    );
    expect(statusReads).not.toHaveBeenCalled();
    expect(report).toMatchObject({ state: "error", tickCompleted: false });
    expect(report.reconcileStuck).toMatchObject({
      failures: SETTLEMENT_JOB_FAILURE_CAP,
      lastError: "socket hang up",
    });
    expect(settlementReconcileStuckDetail(report)).toBe(
      `settlement_reconcile_stuck:${report.reconcileStuck!.txHash}:socket hang up`,
    );
  }, 30_000);

  it("reports a journaled pending body as a completed waiting tick and never sends it (S6 does)", async () => {
    const submitTx = vi.fn(() => Promise.reject(new Error("never reached")));
    const { report, reports, attempt } = await run(
      withoutFollowerJournal(firstTick(() => ({ submitTx }))),
    );
    expect(report).toMatchObject({
      state: "waiting",
      detail: `settlement transaction ${attempt.tx_hash} journaled; S6 sends its exact bytes until it lands`,
      tickCompleted: true,
    });
    expect(submitTx).not.toHaveBeenCalled();
    expect(reports.some((value) => value.state === "error")).toBe(false);
  }, 30_000);

  it("hands the node the refusal holds its journal could not write, once, in its next report", async () => {
    const hold = {
      family: "settlement",
      hold: { reason: "intent_input_untracked", detail: "settlement probe" },
      txHash: "ab".repeat(32),
      signedTxCbor: "84a0a0f5f6",
    };
    let unwritten = [hold];
    const noFollower = await Effect.runPromise(
      Effect.provide(IntentJournal, IntentJournalWithoutFollower),
    );
    const { reports } = await run(
      firstTick(() => ({
        submitTx: () => Promise.reject(new Error("never reached")),
      })).pipe(
        Effect.provideService(IntentJournal, {
          ...noFollower,
          handOff: () => {
            const handed = unwritten;
            unwritten = [];
            return handed;
          },
        }),
      ),
    );
    expect(reports[0]!.intentRefusalHolds).toEqual([hold]);
    expect(
      reports.flatMap((report) => report.intentRefusalHolds ?? []),
    ).toEqual([hold]);
  }, 30_000);

  it("gives the worker-exit report the failing call and its reason", async () => {
    const config = {
      ...(await run(NodeConfig)),
      L1_SETTLEMENT_SEED_PHRASE: "",
    };
    const { posted, closed, exitCode } = await exitReport(
      withoutFollowerJournal(
        settlementProgram(() => undefined).pipe(
          Effect.provideService(NodeConfig, config),
          Effect.provideService(ContractDeploymentIdentity, {
            kind: "manifest",
            manifestId: deploymentId,
          } as unknown as ContractDeploymentIdentity),
          Effect.provideService(
            MidgardContracts,
            {} as unknown as MidgardContracts,
          ),
          Effect.provideService(Lucid, new Lucid({} as unknown as Lucid)),
          provideDatabaseLayers,
        ),
      ),
    );
    expect(posted).toEqual([
      {
        observedAt: expect.any(Number),
        state: "error",
        detail:
          "settlement wallet address: Automatic settlement requires a funded L1_SETTLEMENT_SEED_PHRASE (a distinct fee/collateral wallet)",
      },
    ]);
    expect(closed).toBe(true);
    expect(exitCode).toBe(1);
  }, 30_000);

  it("posts a worker program's labelled provider rejection as its exit report and exits 1", async () => {
    const { posted, closed, exitCode } = await exitReport(
      settlementCall("submit settlement transaction", () =>
        Promise.reject(
          new OgmiosJsonRpcError({
            code: 3005,
            message: "Some transactions failed to pass validation.",
            data: { valueNotConserved: true },
            method: "submitTransaction",
            id: null,
          }),
        ),
      ),
    );
    expect(posted).toHaveLength(1);
    expect(posted[0]!.state).toBe("error");
    expect(posted[0]!.detail).toBe(
      'submit settlement transaction: Ogmios JSON-RPC error 3005: Some transactions failed to pass validation.: {"valueNotConserved":true}',
    );
    expect(closed).toBe(true);
    expect(exitCode).toBe(1);

    // A program that ends without failing posts nothing and exits cleanly.
    const clean = await exitReport(Effect.void);
    expect(clean).toEqual({ posted: [], closed: false, exitCode: undefined });
  });

  it("unwraps an uncaught promise rejection and a defect to their own messages", () => {
    const rejection = new OgmiosJsonRpcError({
      code: 3117,
      message: "The transaction contains unknown UTxO references as inputs.",
      method: "submitTransaction",
      id: null,
    });
    expect(
      settlementCauseDetail(
        Cause.sequential(
          Cause.fail(new UnknownException(rejection)),
          Cause.die(new Error("worker bootstrap", { cause: rejection })),
        ),
      ),
    ).toBe(
      [
        "Ogmios JSON-RPC error 3117: The transaction contains unknown UTxO references as inputs.",
        "worker bootstrap: Ogmios JSON-RPC error 3117: The transaction contains unknown UTxO references as inputs.",
      ].join("\n"),
    );
    expect(settlementCauseDetail(Cause.interrupt("fiber" as never))).toBe(
      "settlement interrupted",
    );
  });
});
