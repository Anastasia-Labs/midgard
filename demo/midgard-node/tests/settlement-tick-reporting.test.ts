/**
 * What the settlement worker reports when a provider call on its path fails:
 * the call's operation and the provider's own words, never Effect's bare
 * "An unknown error occurred in Effect.tryPromise". Runs the worker program
 * against the journal, with a pending settlement body and a stubbed L1.
 */
import { randomUUID } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
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
import { runSettlementWorker } from "../src/workers/settlement.run-settlement-worker.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
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
const pendingAttempt = (ttl = 120n): Journal.SettlementAttempt => {
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
  body.set_ttl(ttl);
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
  };
};

type L1 = {
  readonly transactionStatus?: (hash: string) => Promise<unknown>;
  readonly submitTx: (cbor: string) => Promise<string>;
  readonly utxosAt?: (address: string) => Promise<unknown>;
  /** Queue the job with no journaled body, so the tick builds it. */
  readonly unbuilt?: boolean;
  /** The body's validity bound (default slot 120, above the indexer tip). */
  readonly ttl?: bigint;
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
    const { transactionStatus, submitTx, utxosAt, unbuilt } = l1(attempt);
    yield* Journal.renew(owner);
    if (unbuilt !== true)
      yield* Journal.saveAttempt(owner, attempt, Effect.void);
    const reports: SettlementHealth[] = [];
    const fiber = yield* Effect.fork(
      settlementProgram((health) => reports.push(health), token).pipe(
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
