/**
 * What the settlement worker reports when a provider call on its path fails:
 * the call's operation and the provider's own words, never Effect's bare
 * "An unknown error occurred in Effect.tryPromise". Runs the worker program
 * against the journal, with a pending settlement body and a stubbed L1.
 */
import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import {
  CML,
  generateSeedPhrase,
  OgmiosJsonRpcError,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Fiber } from "effect";
import { UnknownException } from "effect/Cause";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
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
import { runSettlementWorker } from "../src/workers/settlement.run-settlement-worker.js";
import { provideDatabaseLayers } from "./utils.js";

vi.mock(
  "../src/transactions/reference-publication-provider.js",
  async (original) => ({
    ...(await original<object>()),
    // The indexer tip, below the pending body's validity bound.
    synchronizePublicationIndexerPoint: async () => ({
      slot: 10,
      id: "b1".repeat(32),
    }),
  }),
);

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));
const deploymentId = "a2".repeat(32);
const UNKNOWN = "An unknown error occurred";
/** A withdrawal event id as the journal keys it: the event's bytes in hex. */
const EVENT_ID = "cc".repeat(34);

beforeEach(() =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE settlement_attempts, settlement_jobs, settlement_owners, event_history_authority CASCADE`;
      const token = yield* Authority.acquire({
        deploymentIdentity: deploymentId,
        ownerToken: randomUUID(),
        leaseDurationMs: 60_000,
      });
      yield* Authority.publishReady(token, {
        point: { slot: 10, id: "b1".repeat(32) },
        snapshotDigest: "c1".repeat(32),
      });
    }),
  ),
);

/** A signed-looking withdrawal initialize body, valid until slot 120. */
const pendingAttempt = (): Journal.SettlementAttempt => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("ab".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(
        walletFromSeed(generateSeedPhrase(), { network: "Preprod" }).address,
      ),
      CML.Value.from_coin(2_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_ttl(120n);
  const tx = CML.Transaction.new(body, CML.TransactionWitnessSet.new(), true);
  return {
    deployment_id: deploymentId,
    kind: "withdrawal",
    event_id: EVENT_ID,
    phase: "initialize",
    signed_cbor: tx.to_cbor_hex(),
    tx_hash: CML.hash_transaction(body).to_hex(),
    required_outputs: [0],
    fee_inputs: [`${"ab".repeat(32)}#0`],
    status: "pending",
    recovery: false,
  };
};

type L1 = {
  readonly transactionStatus?: (hash: string) => Promise<unknown>;
  readonly submitTx: (cbor: string) => Promise<string>;
  readonly utxosAt?: (address: string) => Promise<unknown>;
  /** Queue the job with no journaled body, so the tick builds it. */
  readonly unbuilt?: boolean;
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
    const attempt = pendingAttempt();
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
      VALUES (${deploymentId}, ${attempt.kind}, ${attempt.event_id}, ${attempt.phase})`;
    const { transactionStatus, submitTx, utxosAt, unbuilt } = l1(attempt);
    yield* Journal.renew(owner);
    if (unbuilt !== true) yield* Journal.saveAttempt(owner, attempt);
    const reports: SettlementHealth[] = [];
    const fiber = yield* Effect.fork(
      settlementProgram((health) => reports.push(health), token).pipe(
        Effect.provideService(NodeConfig, config),
        Effect.provideService(ContractDeploymentIdentity, {
          kind: "manifest",
          manifestId: deploymentId,
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
              transactionStatus:
                transactionStatus ??
                (async (txHash: string) => ({ txHash, status: "not_found" })),
              config: () => ({ provider: { submitTx } }),
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
    }>`SELECT last_error FROM settlement_jobs`;
    return { report, reports, attempt, lastError: jobs[0]?.last_error };
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
  it("names the failing submit, its job and the provider's error", async () => {
    const { report } = await run(
      firstTick(() => ({
        submitTx: () =>
          Promise.reject(
            new OgmiosJsonRpcError({
              code: 3005,
              message: "Some transactions failed to pass validation.",
              data: { valueNotConserved: true },
              method: "submitTransaction",
              id: null,
            }),
          ),
      })),
    );
    expect(report.state).toBe("error");
    expect(report.detail).toContain(
      `settlement withdrawal ${EVENT_ID} initialize reconcile: submit settlement transaction: Ogmios JSON-RPC error 3005: Some transactions failed to pass validation.`,
    );
    expect(report.detail).not.toContain(UNKNOWN);
    // A failed tick is not a completed one.
    expect(report.tickCompleted).toBeUndefined();
  }, 30_000);

  it("names a job's failing build step in its stored error and the report", async () => {
    const { report, lastError } = await run(
      firstTick(() => ({
        unbuilt: true,
        utxosAt: () => Promise.reject(new Error("Kupo timed out")),
        submitTx: () => Promise.reject(new Error("never reached")),
      })),
    );
    const detail = `settlement withdrawal ${EVENT_ID} initialize: settlement wallet utxosAt: Kupo timed out`;
    expect(lastError).toBe(detail);
    expect(report.state).toBe("error");
    expect(report.detail).toBe(detail);
    expect(report.tickCompleted).toBeUndefined();
  }, 30_000);

  it("names a failing status read with the provider's message", async () => {
    const { report, attempt } = await run(
      firstTick(() => ({
        transactionStatus: () => Promise.reject(new Error("socket hang up")),
        submitTx: () => Promise.reject(new Error("never reached")),
      })),
    );
    expect(report.state).toBe("error");
    expect(report.detail).toContain(
      `settlement withdrawal ${EVENT_ID} initialize reconcile: transactionStatus ${attempt.tx_hash}: socket hang up`,
    );
    expect(report.detail).not.toContain(UNKNOWN);
  }, 30_000);

  it("waits, without an error, while the node refuses the resubmitted body's spent inputs", async () => {
    const { report, reports, attempt } = await run(
      firstTick(() => ({
        submitTx: () =>
          Promise.reject(
            new OgmiosJsonRpcError({
              code: 3997,
              message:
                "All inputs are spent. Transaction has probably already been included",
              method: "submitTransaction",
              id: null,
            }),
          ),
      })),
    );
    expect(report.state).toBe("waiting");
    expect(report.detail).toBe(
      `settlement transaction ${attempt.tx_hash} not confirmed yet; its resubmission is refused because its inputs are already spent (by it in a mempool, or by a block): Ogmios JSON-RPC error 3997: All inputs are spent. Transaction has probably already been included`,
    );
    expect(report.tickCompleted).toBe(true);
    expect(reports.some((value) => value.state === "error")).toBe(false);
  }, 30_000);

  it("reports a successful submit as a completed tick with no error", async () => {
    const { report, reports } = await run(
      firstTick((attempt) => ({
        submitTx: async () => attempt.tx_hash,
      })),
    );
    expect(report).toMatchObject({
      state: "waiting",
      detail: "submitted exact journaled settlement transaction",
      tickCompleted: true,
    });
    expect(reports.some((value) => value.state === "error")).toBe(false);
  }, 30_000);

  it("gives the worker-exit report the failing call and its reason", async () => {
    const config = {
      ...(await run(NodeConfig)),
      L1_SETTLEMENT_SEED_PHRASE: "",
    };
    const { posted, closed, exitCode } = await exitReport(
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
