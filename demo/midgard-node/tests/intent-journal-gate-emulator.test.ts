/**
 * The journal row and the workflow's pre-broadcast gate
 * (`BeforeSignedTransactionSubmission.persist`) commit in one SQL
 * transaction (plan §8.2, I1-fix F1), on a Lucid emulator with the node's
 * follower-backed journal:
 *
 * - a gate that refuses leaves no journal row: the workflow sends nothing
 *   and S6 has nothing to send at the next tip;
 * - a stop between the row write and the gate (the fiber interrupted, as a
 *   process shutdown interrupts it) leaves no row either;
 * - a gate that passes commits with the row, and its write is visible with
 *   it: the workflow's send lands and S6 follows it.
 */
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Layer, ManagedRuntime, Redacted } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  IntentJournal,
  journaledIntent,
} from "../src/services/intent-journal.js";
import {
  BeforeSignedTransactionSubmission,
  handleSignSubmitNoConfirmation,
} from "../src/transactions/utils.js";
import {
  type IntentEmulator,
  openIntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const open = async () => {
  const env = await openIntentEmulator(databases);
  opened.push(env);
  expect(await env.stage.run()).toEqual([]);
  return env;
};

const journaledRows = async (env: IntentEmulator): Promise<number> =>
  env.store.transaction("read", async (tx) => {
    const rows = await tx.query("SELECT count(*) AS n FROM l1_intents");
    return Number(rows[0]!.n);
  });

/** Submits a payment from the own wallet as a commit under `persist`, counting direct sends. */
const submitUnderGate = async (
  env: IntentEmulator,
  persist: (
    intent: Readonly<{ txHash: string; signedTxCbor: string }>,
  ) => Effect.Effect<void, unknown>,
) => {
  const lucid = await env.wallet();
  const unsigned = await lucid
    .newTx()
    .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
    .complete();
  const provider = lucid.config().provider!;
  const direct: string[] = [];
  const submitTx = provider.submitTx.bind(provider);
  provider.submitTx = (tx) => {
    direct.push(tx);
    return submitTx(tx);
  };
  const exit = await Effect.runPromiseExit(
    handleSignSubmitNoConfirmation(
      lucid,
      unsigned,
      journaledIntent("commit", "commit:gate", Buffer.alloc(32, 7)),
    ).pipe(
      Effect.provideService(BeforeSignedTransactionSubmission, { persist }),
      Effect.provide(Layer.succeed(IntentJournal, env.journal)),
    ),
  );
  return { exit, direct };
};

describe("the journal row commits with the pre-broadcast gate", () => {
  it("a refused gate leaves no journal row, so S6 sends nothing at the next tip", async () => {
    const env = await open();
    const { exit, direct } = await submitUnderGate(env, () =>
      Effect.fail(
        new Error(
          "Journal member no longer identifies its canonical history row",
        ),
      ),
    );
    expect(exit._tag).toBe("Failure");
    expect(direct).toEqual([]);
    expect(await journaledRows(env)).toBe(0);
    env.emulator.awaitBlock(1);
    await env.follow();
    await env.stage.run();
    expect(env.stage.lastReport()!.intents).toEqual([]);
    expect(env.sent).toEqual([]);
  });

  it("a stop between the row write and the gate leaves no journal row", async () => {
    const env = await open();
    const { exit, direct } = await submitUnderGate(env, () => Effect.interrupt);
    expect(exit._tag).toBe("Failure");
    expect(direct).toEqual([]);
    expect(await journaledRows(env)).toBe(0);
    await env.stage.run();
    expect(env.stage.lastReport()!.intents).toEqual([]);
    expect(env.sent).toEqual([]);
  });

  it("a passing gate's write commits with the row, the send lands and S6 follows it", async () => {
    const env = await open();
    const connectionString = env.connectionString;
    const runtime = ManagedRuntime.make(
      PgClient.layer({ url: Redacted.make(connectionString) }),
    );
    try {
      await runtime.runPromise(
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql`CREATE TABLE gate_marks (tx_hash text PRIMARY KEY)`,
        ),
      );
      const sql = await runtime.runPromise(SqlClient.SqlClient);
      const { exit, direct } = await submitUnderGate(env, ({ txHash }) =>
        Effect.asVoid(sql`INSERT INTO gate_marks VALUES (${txHash})`),
      );
      expect(exit._tag).toBe("Success");
      expect(direct).toHaveLength(1);
      const marks = await runtime.runPromise(
        sql<{ tx_hash: string }>`SELECT tx_hash FROM gate_marks`,
      );
      expect(await journaledRows(env)).toBe(1);
      env.emulator.awaitBlock(1);
      await env.follow();
      expect(await env.stage.run()).toEqual([]);
      const entry = env.stage.lastReport()!.intents[0]!;
      expect(marks.map((row) => row.tx_hash)).toEqual([
        entry.intent.txHash.toString("hex"),
      ]);
      expect(entry).toMatchObject({
        action: "follow",
        status: { kind: "landed" },
      });
      expect(env.sent).toEqual([]);
    } finally {
      await runtime.dispose();
    }
  });
});
