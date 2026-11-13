/**
 * The unresolved submission a commit tick sets once its worker returns
 * `SubmittedAwaitingConfirmationOutput` is cleared by the rebase that
 * applies the block it landed, however the two interleave: the commit lands
 * (its processed own row is ingested) while the worker is still running, and
 * the follower-change driver's `rebaseIfDue` starts then. The tick is the
 * real commitment action up to its worker, which waits for the test; the
 * rebase is the driver's (`testDriverRecompute`) with the modelled native
 * MPF owner of `landed-blocks-rebase.fixture.ts`.
 *
 * The tick runs at the follower view (`runAtFollowerView`), so the driver
 * drains it before its rebase: the rebase applies the block only after the
 * tick has set the hash, and clears it.
 *
 * The rebase stamps the block's journal with its landed time, and no other
 * journal; applying the block again keeps the stamp, and the rebase that
 * reverts its landing after a rollback clears it.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Deferred, Effect, Fiber, Ref, Schedule } from "effect";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { buildAndSubmitCommitmentBlockAction } from "../src/fibers/block-commitment.build-and-submit-commitment-block-action.js";
import { applyOwnBlocks } from "../src/landed-blocks/own-blocks.js";
import { rollBackRows } from "../src/landed-blocks/settlements.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { Globals, Lucid, MidgardContracts } from "../src/services/index.js";
import type { SubmittedAwaitingConfirmationOutput } from "../src/workers/utils/commit-block-header.js";
import {
  manifestFixture,
  PRODUCER_PRIVATE_KEY_SOURCE,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { testDriverRecompute, testWrite } from "./helpers/driver-recompute.js";
import { writeFollowerTip } from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "./helpers/mutation-lease.js";
import { ownJournal } from "./landed-blocks-own-journal.fixture.js";
import {
  attempt,
  freshNative,
  processOf,
  R1,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";

const { worker } = vi.hoisted(() => ({
  worker: {
    /** Resolved once the tick's worker started. */
    started: () => undefined as void,
    /** The worker returns once this resolves. */
    returns: Promise.resolve(undefined as unknown),
  },
}));

vi.mock(
  "../src/fibers/block-commitment.run-commit-worker-in-thread.js",
  async () => {
    const { Effect } = await import("effect");
    return {
      runCommitWorkerInThread: () =>
        Effect.promise(() => {
          worker.started();
          return worker.returns;
        }),
    };
  },
);
vi.mock("../src/fibers/native-mpf-worker-input.js", async () => {
  const { Effect } = await import("effect");
  return {
    nativeMpfWorkerInput: () =>
      Effect.succeed({
        port: undefined,
        durableRoot: "",
        ownerBinarySha256: "",
      }),
  };
});
vi.mock("../src/services/landed-state-queue.js", async (importOriginal) => {
  const { Effect } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../src/services/landed-state-queue.js")
    >()),
    landedStateQueueSnapshot: () =>
      Effect.succeed({
        snapshotId: "commit-preflight:root#0:tail#0",
        root: { outRef: "root#0" },
        tailCommitBase: { outRef: "tail#0", blockEndTimeMs: 0, utxo: "" },
      }),
    refreshStateQueueGlobalsFromSnapshot: () => Effect.void,
  };
});
vi.mock("../src/lucid-time.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/lucid-time.js")>()),
  canonicalSlotConfigForLucid: () => ({
    zeroTime: 0,
    zeroSlot: 0,
    slotLength: 1_000,
  }),
}));

const Journals = PendingBlockFinalizationsDB;
/** The own block's journal, with one transaction `TX` (built before the tests). */
let journal: PendingBlockFinalizationsDB.PrepareInput;
let TX: Awaited<ReturnType<typeof ownJournal>>["tx"];
let HEADER: Buffer;

/** The commit transaction the journal signed (bytes, hash). */
const SIGNED = Buffer.from(makeCardanoSignedMapOutputTxBytes());
const COMMIT = (() => {
  const tx = CML.Transaction.from_cbor_bytes(SIGNED);
  const body = tx.body();
  const hash = CML.hash_transaction(body);
  const hex = hash.to_hex();
  hash.free();
  body.free();
  tx.free();
  return hex;
})();

/** The worker's output for the submitted commit. */
const submitted = (): SubmittedAwaitingConfirmationOutput => ({
  type: "SubmittedAwaitingConfirmationOutput",
  submittedTxHash: COMMIT,
  txSize: SIGNED.length,
  mempoolTxsCount: 1,
  sizeOfBlocksTxs: TX.cbor.length,
  blockEndTimeMs: journal.blockEndTime.getTime(),
  submittedHeaderHash: HEADER.toString("hex"),
  submittedUtxosRoot: R1,
});

// The node's DA manifest: applying an own block seeds its payload's
// publication outbox for the manifest's committee.
let manifestDirectory: string;
beforeAll(async () => {
  ({ journal, tx: TX } = await ownJournal());
  HEADER = journal.headerHash;
  manifestDirectory = await mkdtemp(join(tmpdir(), "landed-own-hash-"));
  const manifestPath = join(manifestDirectory, "manifest.json");
  await writeFile(manifestPath, JSON.stringify(manifestFixture()));
  vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", manifestPath);
  vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", PRODUCER_PRIVATE_KEY_SOURCE);
});
afterAll(async () => {
  vi.unstubAllEnvs();
  await rm(manifestDirectory, { recursive: true, force: true });
});

/** The unresolved submission, the journal's derived status and the row's
 * `applied`. */
const shown = Effect.gen(function* () {
  const globals = yield* Globals;
  return {
    hash: yield* Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
    journal: yield* Journals.retrieveStatusByHeaderHash(HEADER),
    applied: (yield* retrieveRows).map((row) => row.applied),
    // The landed time the throughput metrics read once the row is folded.
    landedTime: (yield* Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ recorded: boolean }>`SELECT
        observed_confirmed_at_ms IS NOT NULL AS recorded
        FROM pending_block_finalizations WHERE header_hash = ${HEADER}`,
    ))[0]?.recorded,
  };
});

/** Another header's journal: a copy of the own block's, never stamped. */
const OTHER = Buffer.alloc(28, 0xee);

/** Each journal's landed time, by header. */
const landedTimes = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql<{ header_hash: Buffer; at: string | null }>`SELECT header_hash,
      observed_confirmed_at_ms::text AS at
    FROM pending_block_finalizations ORDER BY header_hash`,
).pipe(
  Effect.map((rows) =>
    Object.fromEntries(
      rows.map((row) => [row.header_hash.toString("hex"), row.at]),
    ),
  ),
);

describe("the unresolved submission of an own block that lands while its tick runs", () => {
  it("is set by the tick, then cleared by the rebase that applies the block", async () => {
    const globals = await processOf(freshNative());
    // The commit landed: its block is processed and unapplied, its journal
    // signed under the tick's lease.
    await seed(globals, {
      headerHash: HEADER.toString("hex"),
      kind: "own",
      applied: false,
      txIds: [TX.id],
    });
    await run(
      globals,
      testWrite(
        Effect.gen(function* () {
          yield* prepareUnderTestLease({
            ...journal,
            preparedTxHash: Buffer.from(COMMIT, "hex"),
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE pending_block_finalizations
            SET intended_tx_hash = ${Buffer.from(COMMIT, "hex")}
            WHERE header_hash = ${HEADER}`;
        }),
      ),
    );
    const outcome = await run(
      globals,
      Effect.gen(function* () {
        const permit = yield* openFollowerWriteGateAt(
          yield* writeFollowerTip(10),
        );
        yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
          ...local,
          epoch: permit.epoch,
        }));
        const started = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        worker.started = () => {
          Effect.runSync(Deferred.succeed(started, undefined));
        };
        worker.returns = Effect.runPromise(
          Deferred.await(release).pipe(Effect.as(submitted())),
        );

        const tick = yield* Effect.fork(
          buildAndSubmitCommitmentBlockAction(
            journal.metadata.stateQueueLeaseToken,
          ).pipe(
            withTestMutationLease(journal.metadata.stateQueueLeaseToken),
            Effect.provideService(Lucid, { api: {} } as unknown as Lucid),
            Effect.provideService(MidgardContracts, {} as MidgardContracts),
            withoutFollowerJournal,
          ),
        );
        yield* Deferred.await(started);
        const recompute = yield* testDriverRecompute();
        const rebase = yield* Effect.fork(
          recompute.rebaseIfDue("own commit landed"),
        );
        // The driver's recompute has begun and waits for the tick.
        yield* Effect.repeat(Ref.get(globals.FOLLOWER_WRITE_GATE), {
          schedule: Schedule.spaced("10 millis"),
          until: (local) => local.recomputing,
        });
        yield* Effect.sleep("200 millis");
        const whileRunning = yield* shown;

        yield* Deferred.succeed(release, undefined);
        const output = yield* Fiber.join(tick);
        const hold = yield* Fiber.join(rebase);
        return {
          whileRunning,
          output,
          hold,
          after: yield* shown,
          blockEndTimeMs: yield* Ref.get(
            globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          ),
        };
      }),
    );

    // While the tick runs, the rebase has not applied the landed block.
    expect(outcome.whileRunning).toEqual({
      hash: "",
      journal: Journals.OwnBlockStatus.Landed,
      applied: [false],
      landedTime: false,
    });
    // The tick took the worker's output: the branch that sets the hash ran.
    expect(outcome.output.type).toBe("SubmittedAwaitingConfirmationOutput");
    expect(outcome.blockEndTimeMs).toBe(journal.blockEndTime.getTime());
    // The rebase applied it after the tick set the hash, and cleared it.
    expect(outcome.hold).toBeUndefined();
    expect(outcome.after).toEqual({
      hash: "",
      journal: Journals.OwnBlockStatus.Applied,
      applied: [true],
      landedTime: true,
    });

    // Applying the block again stamps only its journal, and keeps its time.
    await sqlRun(
      globals,
      (sql) => sql`INSERT INTO pending_block_finalizations
        SELECT (jsonb_populate_record(NULL::pending_block_finalizations,
          to_jsonb(p) || jsonb_build_object(
            'header_hash', ${OTHER}::bytea,
            'observed_confirmed_at_ms', NULL))).*
        FROM pending_block_finalizations p WHERE p.header_hash = ${HEADER}`,
    );
    const stamped = await run(globals, landedTimes);
    await new Promise((resolve) => setTimeout(resolve, 5));
    const [row] = await run(globals, retrieveRows);
    await run(
      globals,
      testWrite(applyOwnBlocks([{ ...row!, applied: false }])),
    );
    expect(await run(globals, landedTimes)).toEqual(stamped);
    expect(stamped[OTHER.toString("hex")]).toBeNull();
    expect(stamped[HEADER.toString("hex")]).not.toBeNull();

    // A rollback takes the block off the landed chain: the rebase that
    // reverts the landing clears its time.
    await sqlRun(globals, () => rollBackRows([row!], []));
    const reverted = await attempt(globals);
    expect(reverted.hold).toBeUndefined();
    expect(reverted.applied).toEqual([]);
    expect((await run(globals, landedTimes))[HEADER.toString("hex")]).toBe(
      null,
    );
  });
});
