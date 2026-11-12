/**
 * The commit tick, holding the state-queue mutation lease, abandons an
 * active journal its predecessor prepared but never signed before it reads
 * the queue or launches its worker: no lease holder can sign that journal any
 * more, so it would only refuse every other header's preparation. A journal
 * with a signed intent is left alone. The site is driven up to the worker it
 * launches.
 */
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Exit, Option, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { buildAndSubmitCommitmentBlockAction } from "../src/fibers/block-commitment.build-and-submit-commitment-block-action.js";
import { FollowerWrite } from "../src/services/follower-write-gate.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { writeFollowerTip } from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "./helpers/mutation-lease.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const { launched } = vi.hoisted(() => ({ launched: { count: 0 } }));

vi.mock("worker_threads", async (importOriginal) => ({
  ...(await importOriginal<typeof import("worker_threads")>()),
  Worker: class {
    constructor() {
      launched.count += 1;
      throw new Error("stop at the commitment worker");
    }
  },
}));
vi.mock("../src/fibers/resolve-worker-entry.js", () => ({
  resolveWorkerEntry: () => new URL("file:///commit-block-header.js"),
}));
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

const config = makeTestNodeConfig({
  MPF_NATIVE_OWNER_BINARY_SHA256: "",
  VALIDATION_LEDGER_DELTA_LOG_MAX: 1,
});

/** The tick's own lease, taken after its predecessor's ended. */
const TICK_LEASE = "next-tick-lease";

/**
 * Prepares `journal` under its predecessor's lease, then runs one commit
 * tick under the next lease, at an applied follower view; returns the
 * journal's status after it and whether the tick launched its worker.
 */
const tickOver = (journal: Effect.Effect<Buffer, unknown, never>) =>
  Effect.runPromise(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      const headerHash = yield* journal;
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, {} as never);
      const permit = yield* openFollowerWriteGateAt(
        yield* writeFollowerTip(10),
      );
      yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
        ...local,
        epoch: permit.epoch,
      }));
      launched.count = 0;
      const exit = yield* Effect.exit(
        buildAndSubmitCommitmentBlockAction(TICK_LEASE).pipe(
          Effect.provideService(FollowerWrite, permit),
          withTestMutationLease(TICK_LEASE),
        ),
      );
      const status = Option.map(
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
        (record) => record[PendingBlockFinalizationsDB.Columns.STATUS],
      );
      const active = yield* PendingBlockFinalizationsDB.retrieveActive();
      yield* Effect.orDie(resetApplicationTables);
      return {
        failed: Exit.isFailure(exit),
        launched: launched.count,
        status,
        active: Option.isSome(active),
      };
    }).pipe(
      provideDatabaseLayers,
      Effect.provideService(NodeConfig, config),
      Effect.provideService(Lucid, { api: {} } as unknown as Lucid),
      Effect.provideService(MidgardContracts, {} as MidgardContracts),
      Effect.provide(Globals.Default),
      withoutFollowerJournal,
    ) as Effect.Effect<
      {
        failed: boolean;
        launched: number;
        status: Option.Option<string>;
        active: boolean;
      },
      unknown,
      never
    >,
  );

describe("the commit tick over its predecessor's journal", () => {
  it("abandons an unsigned journal and launches the worker for the next block", async () => {
    const result = await tickOver(
      Effect.gen(function* () {
        const headerHash = header("tick-unsigned");
        yield* prepareUnderTestLease(journalFixture(headerHash));
        return headerHash;
      }) as Effect.Effect<Buffer, unknown, never>,
    );
    expect(result.status).toEqual(
      Option.some(PendingBlockFinalizationsDB.Status.Abandoned),
    );
    expect(result.active).toBe(false);
    // The worker that prepares the next block was launched (the test's
    // stand-in stops it there).
    expect(result.launched).toBe(1);
    expect(result.failed).toBe(true);
  });

  it("keeps a journal holding a signed intent", async () => {
    const result = await tickOver(
      Effect.gen(function* () {
        const headerHash = header("tick-signed");
        const signed = Buffer.from(makeCardanoSignedMapOutputTxBytes());
        const tx = CML.Transaction.from_cbor_bytes(signed);
        const body = tx.body();
        const hash = CML.hash_transaction(body);
        const txHash = Buffer.from(hash.to_hex(), "hex");
        hash.free();
        body.free();
        tx.free();
        yield* prepareUnderTestLease({
          ...journalFixture(headerHash),
          preparedTxHash: txHash,
        });
        // Signed before its attempt stopped: the intent is durable.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE pending_block_finalizations
          SET intended_tx_hash = ${txHash}, signed_tx_cbor = ${signed}
          WHERE header_hash = ${headerHash}`;
        return headerHash;
      }) as Effect.Effect<Buffer, unknown, never>,
    );
    expect(result.status).toEqual(
      Option.some(PendingBlockFinalizationsDB.Status.PendingSubmission),
    );
    expect(result.active).toBe(true);
  });
});
