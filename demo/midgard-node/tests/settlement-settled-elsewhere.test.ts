/**
 * Every operator queues a settlement job for every event, so another
 * operator often settles it first. The job reads that settlement from the
 * follower's facts and completes once it is cd deep, as it does from its
 * own attempts; while it is shallower the job waits and counts no failure,
 * and a rollback that undoes it puts the job back on its own path.
 *
 * The node's own earlier attempt never reopens a job completed this way.
 *
 * The settlement worker's tick runs against a migrated node database with
 * a real follower store over it, fed a simulated chain whose settlement
 * transactions are built raw.
 */
import "./utils.js";

import { randomUUID } from "node:crypto";

import {
  currentViewIn,
  type FactStore,
  intentJournalProjection,
  mergeTrackedSets,
  openPostgresFactStore,
  type OutRef,
  projectionStoreOptions,
  recordIntentIn,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import {
  encodeSimTx,
  type SimTx,
  simTxHash,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Layer, ManagedRuntime, Redacted } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import * as MigrationRunner from "../src/database/migrations/runner.js";
import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { settlementProjection } from "../src/services/settlement.final-hook.js";
import {
  type SettlementHealth,
  settlementTick,
} from "../src/services/settlement.js";
import {
  noOpenAttempt,
  settlementDepthParameters,
} from "../src/services/settlement.status.js";
import { SETTLEMENT_JOB_FAILURE_CAP } from "../src/services/settlement-retry.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import {
  admissionTx,
  ELSEWHERE,
  eventIdOf,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
  retirementTx,
} from "./helpers/l1-events-chain.js";
import { ChainDriver, testDatabases } from "./helpers/l1-events-store.js";

const K = 6;
const CD = 3;
const deploymentId = "a8".repeat(32);
const PAYOUT_POLICY = "c1".repeat(28);
const PAYOUT_ADDRESS = Buffer.from(`70${"c2".repeat(28)}`, "hex");
const databases = testDatabases();
const closers: (() => Promise<void>)[] = [];

afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});
afterAll(async () => {
  await databases.dropAll();
});

/** The event lists and, as the node tracks every protocol validator, the
 * payout address. */
const TRACKED: TrackedSet = mergeTrackedSets(eventTrackedSet(EVENTS_CONFIG), {
  addresses: new Set(),
  paymentCredentials: new Set([PAYOUT_ADDRESS.subarray(1).toString("hex")]),
  policies: new Set(),
});

/** What the settlement tick reads of the deployment: the event lists, the
 * payout policy, and cd and k. */
const contracts = {
  eventHistory: {
    deposit: { list: { policyId: listOf("deposit").policyId } },
    withdrawal: { list: { policyId: listOf("withdrawal").policyId } },
  },
  payout: { policyId: PAYOUT_POLICY },
} as unknown as MidgardContracts;
const identity = {
  kind: "manifest",
  manifestId: deploymentId,
  manifest: {
    l1Finality: { confirmationDepth: CD, automaticRecoveryMaxDepth: K },
  },
} as unknown as ContractDeploymentIdentity;

const owner: Journal.SettlementOwner = {
  deploymentId,
  walletAddress: "settlement-wallet",
  token: randomUUID(),
};

/** A migrated node database, a follower store over it carrying the event
 * projection and, as the node's does, the settlement and intent journal
 * projections, and the settlement worker's tick against both. */
const scenario = async () => {
  const connectionString = await databases.create();
  const runtime = ManagedRuntime.make(
    Layer.merge(
      PgClient.layer({ url: Redacted.make(connectionString) }),
      NodeConfig.layer,
    ),
  );
  closers.push(() => runtime.dispose());
  const run = <A, E>(
    effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
  ) => runtime.runPromise(effect);
  await run(MigrationRunner.migrate({ appVersion: "test", actor: "race" }));
  const store: FactStore = openPostgresFactStore({
    ...projectionStoreOptions(
      [
        eventProjection(EVENTS_CONFIG),
        settlementProjection,
        intentJournalProjection,
      ],
      { securityParameter: K, trackedSet: TRACKED },
      "postgres",
    ),
    connection: { connectionString },
  });
  closers.push(() => store.close());
  const started = await store.start();
  if (started.kind !== "ready")
    throw new Error(`store start: ${JSON.stringify(started)}`);
  const driver = new ChainDriver(store, TRACKED);
  await driver.init();
  await run(
    Effect.gen(function* () {
      yield* openFollowerWriteGate;
      yield* Journal.renew(owner);
    }),
  );
  const idle = async (blocks: number) => {
    for (let i = 0; i < blocks; i += 1)
      await driver.forward([
        {
          inputs: [driver.chain.outsideInput()],
          outputs: [
            { address: simUniverse().untrackedAddress, lovelace: 2_000_000n },
          ],
          nonce: driver.chain.nonce(),
        },
      ]);
  };
  const reports: SettlementHealth[] = [];
  /** One settlement tick, with its job due now. */
  const tick = () =>
    run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE settlement_jobs SET due_at = clock_timestamp()`;
        yield* withoutFollowerJournal(
          settlementTick(owner, {} as LucidEvolution, (health) =>
            reports.push(health),
          ),
        ).pipe(
          Effect.provideService(ContractDeploymentIdentity, identity),
          Effect.provideService(MidgardContracts, contracts),
          Effect.provideService(
            Lucid,
            new Lucid({
              switchToOperatorsMainWallet: Effect.void,
            } as unknown as Lucid),
          ),
          Effect.either,
        );
      }),
    );
  const job = (eventId: string) =>
    run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows = yield* sql<{
          phase: string;
          failures: number;
          last_error: string | null;
        }>`SELECT phase, failures, last_error FROM settlement_jobs
          WHERE deployment_id = ${deploymentId} AND event_id = ${eventId}`;
        return rows[0];
      }),
    );
  const queue = (kind: Journal.SettlementKind, eventId: string) =>
    run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
          VALUES (${deploymentId}, ${kind}, ${eventId},
            ${kind === "deposit" ? "absorb" : "initialize"})`;
      }),
    );
  const stuck = () =>
    run(Journal.stuckJobs(deploymentId, SETTLEMENT_JOB_FAILURE_CAP));
  /** The node's own settlement attempt for `body`, journaled and saved as
   * the tick's send step saves it, before it lands. */
  const ownAttempt = async (
    kind: Journal.SettlementKind,
    eventId: string,
    phase: Journal.SettlementAttempt["phase"],
    body: SimTx,
  ) => {
    const recorded = await store.transaction("write", async (tx) =>
      recordIntentIn(tx, store.dialect, {
        family: "settlement",
        workflowKey: `settlement:${simTxHash(body).toString("hex")}`,
        txCbor: encodeSimTx(body),
        isOwnOutput: () => false,
        builtAt: (await currentViewIn(tx, store.dialect))!,
      }),
    );
    if (recorded.kind !== "recorded")
      throw new Error(`record: ${recorded.kind}`);
    await run(
      Effect.gen(function* () {
        const depths = yield* settlementDepthParameters.pipe(
          Effect.provideService(ContractDeploymentIdentity, identity),
        );
        yield* Journal.saveAttempt(
          owner,
          {
            deployment_id: deploymentId,
            kind,
            event_id: eventId,
            phase,
            tx_hash: simTxHash(body).toString("hex"),
            signed_cbor: encodeSimTx(body).toString("hex"),
            required_outputs: [0],
            fee_inputs: [
              `${body.inputs[0]!.txHash.toString("hex")}#${body.inputs[0]!.index}`,
            ],
            status: "pending",
          },
          noOpenAttempt(owner, depths),
        );
      }),
    );
  };
  const pruneAll = async (): Promise<void> => {
    for (;;) {
      const pruned = await store.prune();
      if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
      if (pruned.done) return;
    }
  };
  return { driver, idle, tick, job, queue, stuck, ownAttempt, pruneAll };
};

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd7);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: 0 };
};

/** The job's event id: the event id CBOR, in hex. */
const eventIdHex = (nonce: OutRef): string =>
  Data.to(eventIdOf(nonce), SDK.OutputReference);

/** A withdrawal admitted on the chain, with its job queued. */
const queuedWithdrawal = async (s: Awaited<ReturnType<typeof scenario>>) => {
  const withdrawal = eventOrder("withdrawal", nonceRef());
  const [admission] = await s.driver.forward([
    admissionTx(withdrawal, s.driver.chain.nonce()),
  ]);
  const eventId = eventIdHex(withdrawal.nonce);
  await s.queue("withdrawal", eventId);
  return { withdrawal, eventId, order: { txHash: admission!, index: 0 } };
};

/** A payout's transactions, built raw: the initialize retires the event
 * into a payout output holding the payout token, a fund spends that output
 * into a richer one, and a conclude spends it and burns the token. */
const payoutTxs = (
  s: Awaited<ReturnType<typeof scenario>>,
  withdrawal: ReturnType<typeof eventOrder>,
  order: OutRef,
) => {
  const token = new Map([[PAYOUT_POLICY, new Map([[withdrawal.key, 1n]])]]);
  const payout = (lovelace: bigint) => ({
    address: PAYOUT_ADDRESS,
    lovelace,
    assets: token,
  });
  const retire = retirementTx(
    withdrawal,
    order,
    "payout_initialized",
    s.driver.chain.nonce(),
  );
  const initialize: SimTx = {
    ...retire,
    outputs: [...retire.outputs, payout(9_000_000n)],
    mint: new Map([...retire.mint!, ...token]),
  };
  const fund: SimTx = {
    inputs: [{ txHash: simTxHash(initialize), index: 1 }],
    outputs: [payout(12_000_000n)],
    nonce: s.driver.chain.nonce(),
  };
  const conclude: SimTx = {
    inputs: [{ txHash: simTxHash(fund), index: 0 }],
    outputs: [{ address: ELSEWHERE, lovelace: 11_000_000n }],
    mint: new Map([[PAYOUT_POLICY, new Map([[withdrawal.key, -1n]])]]),
    nonce: s.driver.chain.nonce(),
  };
  return { initialize, fund, conclude };
};

/** A deposit admitted on the chain, with its job queued. */
const queuedDeposit = async (s: Awaited<ReturnType<typeof scenario>>) => {
  const deposit = eventOrder("deposit", nonceRef());
  const [admission] = await s.driver.forward([
    admissionTx(deposit, s.driver.chain.nonce()),
  ]);
  const eventId = eventIdHex(deposit.nonce);
  await s.queue("deposit", eventId);
  return { deposit, eventId, order: { txHash: admission!, index: 0 } };
};

const QUIET = { failures: 0, last_error: null };

describe("a settlement another operator landed first", () => {
  it("completes a deposit job once a foreign absorb is cd deep, with no failure and nothing stuck", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.idle(CD - 1);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("waits on a foreign absorb shallower than cd without counting a failure", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.idle(CD - 2);
    for (let i = 0; i < SETTLEMENT_JOB_FAILURE_CAP; i += 1) await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "absorb", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("puts the job back on its own path when the foreign absorb is rolled back", async () => {
    const s = await scenario();
    const { deposit, eventId, order } = await queuedDeposit(s);
    await s.driver.forward([
      retirementTx(deposit, order, "absorbed", s.driver.chain.nonce()),
    ]);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "absorb", ...QUIET });

    // The absorb is rolled back: nothing is settled, so the job builds its
    // own absorb again (here refused: this node never projected the event).
    await s.driver.backward(1);
    await s.tick();
    const reopened = await s.job(eventId);
    expect(reopened).toMatchObject({ phase: "absorb", failures: 1 });
    expect(reopened?.last_error).toMatch(
      new RegExp(`^settlement deposit ${eventId} absorb: `, "u"),
    );
    expect(reopened?.last_error).not.toMatch(/retired/u);
  });

  it("completes a withdrawal job once another operator's payout conclusion is cd deep", async () => {
    const s = await scenario();
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund, conclude } = payoutTxs(s, withdrawal, order);
    await s.driver.forward([initialize]);
    await s.driver.forward([fund]);
    await s.idle(CD);
    await s.driver.forward([conclude]);
    // The conclusion is shallower than cd: the job waits.
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "initialize", ...QUIET });

    await s.idle(CD - 1);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });

  it("never completes a withdrawal job whose foreign payout is initialized or funded but not concluded", async () => {
    const s = await scenario();
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund } = payoutTxs(s, withdrawal, order);
    // The event is retired cd deep, but the payout output is unspent: the
    // job takes its own path (refused here: this node never projected the
    // withdrawal).
    await s.driver.forward([initialize]);
    await s.idle(CD);
    await s.tick();
    expect((await s.job(eventId))?.phase).toBe("initialize");

    // Funded cd deep: the first payout output is spent, its successor is
    // not; the payout is still open.
    await s.driver.forward([fund]);
    await s.idle(CD);
    await s.tick();
    expect((await s.job(eventId))?.phase).toBe("initialize");
  });

  it("keeps a job complete that its own settled initialize precedes, after another operator funded and concluded the payout", async () => {
    const s = await scenario();
    // Another deposit, absorbed by another operator; its job is queued
    // later, to be due before the withdrawal's.
    const deposit = eventOrder("deposit", nonceRef());
    const [admitted] = await s.driver.forward([
      admissionTx(deposit, s.driver.chain.nonce()),
    ]);
    await s.driver.forward([
      retirementTx(
        deposit,
        { txHash: admitted!, index: 0 },
        "absorbed",
        s.driver.chain.nonce(),
      ),
    ]);
    const { withdrawal, eventId, order } = await queuedWithdrawal(s);
    const { initialize, fund, conclude } = payoutTxs(s, withdrawal, order);
    // This node's initialize lands and stays its latest attempt; another
    // operator funds and concludes the payout.
    await s.ownAttempt("withdrawal", eventId, "initialize", initialize);
    await s.driver.forward([initialize]);
    await s.driver.forward([fund]);
    await s.driver.forward([conclude]);
    await s.idle(CD - 1);
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });

    // At the same tip, the own initialize safe but not final: the deposit's
    // job is due first, so the tick builds that one, and the completed
    // withdrawal job must still read complete after it.
    const depositId = eventIdHex(deposit.nonce);
    await s.queue("deposit", depositId);
    await s.tick();
    expect(await s.job(depositId)).toEqual({ phase: "complete", ...QUIET });
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });

    // More than k deep, the prune step stores the attempt final and prunes
    // the retirement; the job stays complete.
    await s.idle(K + 1);
    await s.pruneAll();
    await s.tick();
    await s.tick();
    expect(await s.job(eventId)).toEqual({ phase: "complete", ...QUIET });
    expect((await s.stuck()).count).toBe(0);
  });
});
