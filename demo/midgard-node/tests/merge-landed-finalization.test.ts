import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Deferred, Effect, Either, Fiber, Ref } from "effect";
import { describe, expect } from "vitest";

import {
  BlocksDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { Globals } from "../src/services/index.js";
import { finalizeMergesLandedThrough } from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import { provideDatabaseLayers } from "./utils.js";

/**
 * The landed-merge walk over real journals: which merges it finalizes, in
 * which order, where it stops, and the journal checks that refuse a merge
 * before anything is written. Every block is empty, so each finalization
 * takes the confirmed ledger's already-at-root path and only the walk varies.
 */

const DURABLE_ROOT = "ab".repeat(32);

/** Clears only the tables these tests write, then runs `effect` with an
 * Architecture G native owner whose diagnostics `diagnostics` answers. */
const isolatedDb = <A, E>(
  effect: Effect.Effect<A, E, any>,
  diagnostics: () => Promise<{
    readonly durableRoot: string;
    readonly activeGenerations: number;
  }> = async () => ({ durableRoot: DURABLE_ROOT, activeGenerations: 1 }),
) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE TABLE local_mutation_jobs, pending_block_finalizations,
        blocks, confirmed_ledger RESTART IDENTITY CASCADE`;
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, { diagnostics } as never);
      return yield* effect;
    }).pipe(Effect.provide(Globals.Default)),
  ) as Effect.Effect<A, E, never>;

const emptyRoots = {
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
};
const expectedRoots = {
  ...emptyRoots,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
};
const expectedCounts = {
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
};

/** An empty block's header; `index` sets its time window. */
const blockHeader = (index: number, prevHeaderHash: string): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  ...expectedRoots,
  ...expectedCounts,
  startTime: BigInt(index * 1_000),
  endTime: BigInt(index * 1_000 + 999),
  blockSlot: BigInt(index),
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash,
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
});

type Block = {
  readonly header: SDK.Header;
  readonly headerHash: string;
};

const block = (index: number, prev: string) =>
  Effect.map(SDK.hashBlockHeader(blockHeader(index, prev)), (headerHash) => ({
    header: blockHeader(index, prev),
    headerHash,
  }));

/**
 * Journals `stored` under `headerHash` with one block-row tx, advanced to
 * `status` (Finalized or ObservedWaitingStability).
 */
const journal = ({
  headerHash,
  stored,
  status,
}: {
  readonly headerHash: string;
  readonly stored: SDK.Header;
  readonly status:
    | typeof PendingBlockFinalizationsDB.Status.Finalized
    | typeof PendingBlockFinalizationsDB.Status.ObservedWaitingStability;
}) =>
  Effect.gen(function* () {
    const hash = Buffer.from(headerHash, "hex");
    const blockStartTime = new Date("2026-06-12T00:00:00.000Z");
    yield* PendingBlockFinalizationsDB.preparePendingSubmission({
      headerHash: hash,
      headerCbor: Buffer.from(
        LucidData.to(stored as never, SDK.Header as never),
        "hex",
      ),
      metadata: {
        deploymentMarker: makeDeploymentMarker("de".repeat(32)),
        consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
        stateQueueLeaseToken: "lease-token",
        baseSnapshotId: "snapshot",
        baseTailOutRef: `${headerHash}#0`,
        baseTailHeaderHash: Buffer.from(stored.prevHeaderHash, "hex"),
        baseTailDatumCbor: "d87980",
        baseRoots: emptyRoots,
        blockStartTime,
        expectedRoots,
        expectedCounts,
      },
      blockEndTime: new Date(blockStartTime.getTime() + 60_000),
      depositEventIds: [],
      depositEntries: [],
      forcedTransactionEventIds: [],
      forcedTransactionEntries: [],
      withdrawalEventIds: [],
      withdrawalEntries: [],
      mempoolTxIds: [],
      mempoolTxs: [],
      mempoolTxSourceTable: "none",
      transitionTraceMembers: [],
      eventToStepMembers: [],
      validationTraceMembers: [],
      validationTraceWitnessMembers: [],
      ledgerDelta: { spent: [], produced: [] },
    });
    yield* PendingBlockFinalizationsDB.markSubmitted(
      hash,
      Buffer.from(headerHash.padEnd(64, "7"), "hex"),
    );
    yield* PendingBlockFinalizationsDB.markObservedWaitingStability(hash, 1n);
    if (status === PendingBlockFinalizationsDB.Status.Finalized)
      yield* PendingBlockFinalizationsDB.markFinalized(hash);
    yield* BlocksDB.insert(hash, [
      Buffer.from(headerHash.padEnd(64, "0"), "hex"),
    ]);
  });

const finalizedJournal = ({ header, headerHash }: Block) =>
  journal({
    headerHash,
    stored: header,
    status: PendingBlockFinalizationsDB.Status.Finalized,
  });

/** L1's confirmed state naming `headerHash`. */
const confirmedAt = (
  headerHash: string,
  utxoRoot: string = SDK.EMPTY_MERKLE_TREE_ROOT,
): SDK.ConfirmedState => ({
  headerHash,
  prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  utxoRoot,
  startTime: 0n,
  endTime: 0n,
  protocolVersion: 1n,
});

const mergeJob = (headerHash: string) =>
  MutationJobsDB.retrieveByJobId(
    MutationJobsDB.confirmedMergeFinalizationJobId(headerHash),
  );

const blockRows = (headerHash: string) =>
  Effect.map(
    BlocksDB.retrieveTxHashesByHeaderHash(Buffer.from(headerHash, "hex")),
    (rows) => rows.length,
  );

/** The job completed after `attempts` attempts and the block rows are gone. */
const expectFinalized = (headerHash: string, attempts: number) =>
  Effect.gen(function* () {
    expect(yield* mergeJob(headerHash)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Completed,
      [MutationJobsDB.Columns.ATTEMPTS]: attempts,
    });
    expect(yield* blockRows(headerHash)).toBe(0);
  });

/** No job was started and the block rows are still there. */
const expectUntouched = (headerHash: string) =>
  Effect.gen(function* () {
    expect(yield* mergeJob(headerHash)).toBeUndefined();
    expect(yield* blockRows(headerHash)).toBe(1);
  });

describe("landed-merge finalization walk", () => {
  it.effect(
    "finalizes the landed merges up to the confirmed header oldest first, stopping at a completed one and never reaching an unlanded one",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const a = yield* block(1, SDK.GENESIS_HEADER_HASH);
          const b = yield* block(2, a.headerHash);
          const c = yield* block(3, b.headerHash);
          const unlanded = yield* block(4, c.headerHash);
          for (const each of [a, b, c, unlanded]) yield* finalizedJournal(each);
          expect(
            yield* finalizeMergesLandedThrough(confirmedAt(a.headerHash)),
          ).toEqual([a.headerHash]);
          yield* expectFinalized(a.headerHash, 1);
          yield* expectUntouched(b.headerHash);

          // A's job completed, so the walk from C stops there and does not
          // run A's finalization again; the block past C has not landed.
          expect(
            yield* finalizeMergesLandedThrough(confirmedAt(c.headerHash)),
          ).toEqual([b.headerHash, c.headerHash]);
          yield* expectFinalized(a.headerHash, 1);
          yield* expectFinalized(b.headerHash, 1);
          yield* expectFinalized(c.headerHash, 1);
          yield* expectUntouched(unlanded.headerHash);
          expect(
            yield* finalizeMergesLandedThrough(confirmedAt(c.headerHash)),
          ).toEqual([]);
        }),
      ),
  );

  const refusals = [
    {
      name: "a journal whose header does not hash to its header hash",
      message:
        "Pending-finalization journal header does not hash to its header hash",
      arrange: (landed: Block, forged: Block) =>
        Effect.as(
          journal({
            headerHash: landed.headerHash,
            stored: forged.header,
            status: PendingBlockFinalizationsDB.Status.Finalized,
          }),
          confirmedAt(landed.headerHash),
        ),
    },
    {
      name: "an L1 confirmed-state UTxO root other than the journal header's",
      message:
        "L1 confirmed-state UTxO root does not match its header's journal",
      arrange: (landed: Block) =>
        Effect.as(
          finalizedJournal(landed),
          confirmedAt(landed.headerHash, "cd".repeat(32)),
        ),
    },
    {
      name: "a landed block that is not locally finalized yet",
      message:
        "A landed merge's block is not locally finalized yet, so its merge cannot be finalized",
      arrange: (landed: Block) =>
        Effect.as(
          journal({
            headerHash: landed.headerHash,
            stored: landed.header,
            status: PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
          }),
          confirmedAt(landed.headerHash),
        ),
    },
  ] as const;

  it.effect.each(refusals)(
    "refuses $name before writing anything",
    ({ message, arrange }) =>
      isolatedDb(
        Effect.gen(function* () {
          const landed = yield* block(1, SDK.GENESIS_HEADER_HASH);
          const forged = yield* block(9, SDK.GENESIS_HEADER_HASH);
          const confirmedState = yield* arrange(landed, forged);
          const result = yield* Effect.either(
            finalizeMergesLandedThrough(confirmedState),
          );
          expect(Either.isLeft(result) && result.left.message).toBe(message);
          yield* expectUntouched(landed.headerHash);
        }),
      ),
  );

  it.effect(
    "runs a started finalization to completion when the caller is interrupted",
    () => {
      const reached = Effect.runSync(Deferred.make<void>());
      const release = Effect.runSync(Deferred.make<void>());
      return isolatedDb(
        Effect.gen(function* () {
          const landed = yield* block(1, SDK.GENESIS_HEADER_HASH);
          yield* finalizedJournal(landed);
          const walk = yield* Effect.fork(
            finalizeMergesLandedThrough(confirmedAt(landed.headerHash)),
          );
          // The finalization's SQL fold has committed; the MPF check is the
          // last step before the job completes.
          yield* Deferred.await(reached);
          const interrupter = yield* Effect.fork(Fiber.interrupt(walk));
          yield* Deferred.succeed(release, undefined);
          yield* Fiber.join(interrupter);
          yield* expectFinalized(landed.headerHash, 1);
        }),
        async () => {
          Effect.runSync(Deferred.succeed(reached, undefined));
          await Effect.runPromise(Deferred.await(release));
          return { durableRoot: DURABLE_ROOT, activeGenerations: 1 };
        },
      );
    },
  );
});
