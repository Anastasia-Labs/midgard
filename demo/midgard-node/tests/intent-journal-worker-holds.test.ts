/**
 * I1-H1: a refusal the intent journal raises in a worker thread (the commit
 * and settlement workers each run their own journal) reaches the main
 * process's `/readyz` under its named reason. The worker is a real
 * `worker_threads` thread running the workers' journal stack, bundled from
 * source; the main process reads the holds through its own journal's
 * `refresh` (run at every tip by the follower) and the follower readiness.
 * A refusal whose hold write fails in the worker is handed to the main
 * process, which names it and writes it until it lands.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { Worker } from "node:worker_threads";

import { type FollowStatus } from "@al-ft/midgard-l1-follower";
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { encodeSimTx } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { build as bundleWithTsup } from "tsup";
import { afterAll, beforeAll, expect, it } from "vitest";

import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import type { Globals } from "../src/services/globals.js";
import {
  INTENT_CONTENT_REF_MISSING,
  intentJournalOver,
} from "../src/services/intent-journal.js";
import {
  type L1FollowerHandle,
  l1FollowerReadiness,
} from "../src/services/l1-follower.readiness.js";
import { db } from "./helpers/forced-orders-node-store.js";
import type {
  IntentJournalWorkerProbeInput,
  IntentJournalWorkerProbeResult,
} from "./helpers/intent-journal-worker-probe.js";
import { QUEUE_ADDRESS } from "./helpers/state-queue-sim.fixtures.js";
import { resetApplicationTables } from "./utils.js";

const packageRoot = join(import.meta.dirname, "..");
let bundleDir: string;

beforeAll(async () => {
  bundleDir = await mkdtemp(join(packageRoot, ".intent-journal-worker-"));
  // A worker thread resolves workspace packages through their dist; bundle
  // them from source, as tests/settlement-worker-heap.test.ts does.
  await bundleWithTsup({
    entry: [join(packageRoot, "tests/helpers/intent-journal-worker-probe.ts")],
    format: ["esm"],
    platform: "node",
    target: "node22",
    outDir: bundleDir,
    config: false,
    splitting: false,
    silent: true,
    noExternal: [/^@al-ft\//],
    loader: { ".sql": "text" },
    banner: {
      js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
    },
    esbuildOptions(options) {
      options.conditions = ["midgard-source", ...(options.conditions ?? [])];
    },
  });
}, 300_000);
afterAll(async () => {
  if (bundleDir !== undefined)
    await rm(bundleDir, { recursive: true, force: true });
});

const runProbe = async (
  input: IntentJournalWorkerProbeInput,
): Promise<IntentJournalWorkerProbeResult> => {
  const worker = new Worker(join(bundleDir, "intent-journal-worker-probe.js"), {
    workerData: input,
  });
  try {
    return await new Promise((resolve, reject) => {
      worker.once("message", resolve);
      worker.once("error", reject);
      worker.once("exit", (code) =>
        reject(new Error(`probe exited (${code}) without a result`)),
      );
    });
  } finally {
    await worker.terminate();
  }
};

const following: FollowStatus = {
  state: "following",
  interventions: [],
  waiting: null,
  stuck: null,
  protocolInit: "unknown",
  cursor: null,
  tip: null,
  atTip: true,
  node: null,
  nodeBehind: null,
  replaying: false,
  events: 0,
  lastError: null,
  prune: {
    steps: 0,
    prunedThroughSlot: null,
    lastError: null,
    failures: 0,
    floorLags: [],
  },
  readiness: [],
};

const signed = (nonce: number) => {
  const cbor = encodeSimTx({
    inputs: [{ txHash: Buffer.alloc(32, nonce), index: 0 }],
    outputs: [{ address: QUEUE_ADDRESS, lovelace: 2_000_000n }],
    nonce,
  });
  return {
    signedTxCbor: Buffer.from(cbor).toString("hex"),
    txHash: decodeTransaction(cbor).hash.toString("hex"),
  };
};

it("commit and settlement refusals raised in a worker thread fail the main process's /readyz by name", async () => {
  await db(resetApplicationTables);
  const commit = signed(1);
  const settlement = signed(2);
  // Each family names its content; these name none, so the journal refuses.
  expect(
    (
      await runProbe({
        records: [
          { family: "commit", workflowKey: "commit:probe", ...commit },
          {
            family: "settlement",
            workflowKey: "settlement:probe",
            ...settlement,
          },
        ],
      })
    ).reasons,
  ).toEqual([INTENT_CONTENT_REF_MISSING, INTENT_CONTENT_REF_MISSING]);

  await db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const main = intentJournalOver(sql, () => false);
      const handle: L1FollowerHandle = {
        kind: "running",
        status: () => following,
        holds: main.holds,
        planCurrent: () => Promise.resolve({ kind: "none", detail: "" }),
      };
      // The worker's refusals live in its own memory until the main process
      // re-reads the table, which the follower does at every tip.
      expect(l1FollowerReadiness(handle).reasons).toEqual([]);
      yield* main.refresh();
      const readiness = l1FollowerReadiness(handle);
      expect(readiness.reasons).toEqual([INTENT_CONTENT_REF_MISSING]);
      expect(
        (readiness.report.readiness as readonly { detail: string }[]).map(
          ({ detail }) => detail.split(":")[0],
        ),
      ).toEqual(["commit commit", "settlement settlement"]);
    }),
  );
}, 300_000);

it("a worker's refusal hold whose write fails is handed to the main process, named on /readyz, and written once the table takes it", async () => {
  await db(resetApplicationTables);
  const commit = signed(3);
  // The hold table refuses every write until it is put back.
  await db(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) =>
        sql`ALTER TABLE intent_refusal_holds RENAME TO intent_refusal_holds_away`,
    ),
  );
  const putBack = Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`ALTER TABLE intent_refusal_holds_away RENAME TO intent_refusal_holds`,
  );
  try {
    const { reasons, notices } = await runProbe({
      records: [{ family: "commit", workflowKey: "commit:probe", ...commit }],
      handOff: true,
    });
    expect(reasons).toEqual([INTENT_CONTENT_REF_MISSING]);
    // The run's retries did not land it, so it reaches the parent, which
    // takes it over the way the commit fiber does.
    expect(notices).toEqual([
      {
        type: "IntentRefusalHoldsNotice",
        holds: [
          expect.objectContaining({
            family: "commit",
            hold: expect.objectContaining({
              reason: INTENT_CONTENT_REF_MISSING,
            }),
            txHash: commit.txHash,
            signedTxCbor: commit.signedTxCbor,
          }),
        ],
      },
    ]);
    await db(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const main = intentJournalOver(sql, () => false);
        const handle: L1FollowerHandle = {
          kind: "running",
          status: () => following,
          holds: main.holds,
          planCurrent: () => Promise.resolve({ kind: "none", detail: "" }),
        };
        expect(
          takeCommitWorkerOutput({} as Globals, notices[0]!, 0, main.adopt),
        ).toBeUndefined();
        // Named at once, and still named while its write keeps failing.
        expect(l1FollowerReadiness(handle).reasons).toEqual([
          INTENT_CONTENT_REF_MISSING,
        ]);
        yield* main.refresh();
        expect(l1FollowerReadiness(handle).reasons).toEqual([
          INTENT_CONTENT_REF_MISSING,
        ]);
        expect(main.handOff()).toHaveLength(1);
        main.adopt(notices[0]!.holds);

        // The table takes writes again: the next refresh writes it.
        yield* putBack;
        yield* main.refresh();
        expect(l1FollowerReadiness(handle).reasons).toEqual([
          INTENT_CONTENT_REF_MISSING,
        ]);
        expect(main.handOff()).toEqual([]);
        const rows = yield* sql<{
          readonly family: string;
          readonly reason: string;
        }>`SELECT family, reason FROM intent_refusal_holds`;
        expect(rows).toEqual([
          { family: "commit", reason: INTENT_CONTENT_REF_MISSING },
        ]);
        // A restarted node reads it from the table.
        const restarted = intentJournalOver(sql, () => false);
        yield* restarted.refresh();
        expect(restarted.holds().map(({ reason }) => reason)).toEqual([
          INTENT_CONTENT_REF_MISSING,
        ]);
      }),
    );
  } finally {
    await db(
      Effect.flatMap(SqlClient.SqlClient, (sql) =>
        sql`SELECT to_regclass('intent_refusal_holds_away') AS away`.pipe(
          Effect.flatMap(([row]) =>
            row?.away === null ? Effect.void : putBack,
          ),
        ),
      ),
    );
  }
}, 300_000);
