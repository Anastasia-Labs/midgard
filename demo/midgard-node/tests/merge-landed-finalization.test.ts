import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Deferred, Effect, Either, Fiber } from "effect";
import { describe, expect } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { processRow } from "../src/landed-blocks/settlements.js";
import { CONFIRMED_MERGE_JOURNAL_UNBOUND } from "../src/transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import { finalizeMergesLandedThrough } from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import {
  type Block,
  block,
  confirmedAt,
  confirmedAtBlock,
  DURABLE_ROOT,
  expectFinalized,
  expectUntouched,
  finalizedJournal,
  fold,
  foreignRow,
  isolatedDb,
  journal,
  ownRow,
} from "./merge-landed-finalization.fixtures.js";

/**
 * The landed-merge walk over real journals: which merges it finalizes, in
 * which order, where it stops, and the journal checks that refuse a merge
 * before anything is written.
 */

describe("landed-merge finalization walk", () => {
  it.effect(
    "finalizes the landed merges up to the confirmed header oldest first, stopping at a folded one and never reaching an unlanded one",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const a = yield* block(1, SDK.GENESIS_HEADER_HASH);
          const b = yield* block(2, a.headerHash);
          const c = yield* block(3, b.headerHash);
          const unlanded = yield* block(4, c.headerHash);
          for (const each of [a, b, c, unlanded]) yield* finalizedJournal(each);
          expect(
            yield* finalizeMergesLandedThrough(confirmedAtBlock(a)),
          ).toEqual([a.headerHash]);
          yield* expectFinalized(a.headerHash);
          yield* expectUntouched(b.headerHash);

          // A is folded, so the walk from C stops there and does not
          // run A's finalization again; the block past C has not landed.
          expect(
            yield* finalizeMergesLandedThrough(confirmedAtBlock(c)),
          ).toEqual([b.headerHash, c.headerHash]);
          yield* expectFinalized(a.headerHash);
          yield* expectFinalized(b.headerHash);
          yield* expectFinalized(c.headerHash);
          yield* expectUntouched(unlanded.headerHash);
          expect(
            yield* finalizeMergesLandedThrough(confirmedAtBlock(c)),
          ).toEqual([]);
        }),
      ),
  );

  it.effect(
    "finalizes this node's merges below another operator's merged block, and stops where no landed fact names a header",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const a = yield* block(1, SDK.GENESIS_HEADER_HASH);
          yield* finalizedJournal(a);
          // Another operator's block on top of A: this node holds no journal
          // for it, only its landed row.
          const foreign = "f2".repeat(28);
          yield* processRow({
            headerHash: foreign,
            parentHeaderHash: a.headerHash,
            parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            kind: "foreign",
            state: "processed",
            applied: true,
            spent: [],
            produced: [],
            depositIds: [],
            withdrawals: [],
            forcedIds: [],
            txIds: [],
          });
          expect(
            yield* finalizeMergesLandedThrough(confirmedAt(foreign, 2_999n)),
          ).toEqual([a.headerHash]);
          yield* expectFinalized(a.headerHash);

          // A header no journal and no landed row names ends the walk.
          const b = yield* block(2, SDK.GENESIS_HEADER_HASH);
          yield* finalizedJournal(b);
          expect(
            yield* finalizeMergesLandedThrough(
              confirmedAt("f3".repeat(28), 2_999n),
            ),
          ).toEqual([]);
          yield* expectUntouched(b.headerHash);
        }),
      ),
  );

  it.effect(
    "defers this node's merge above another operator's block that has not folded, and finalizes it once that block folds",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          // Own A, then another operator's F on A, then own B on F.
          const a = yield* block(1, SDK.GENESIS_HEADER_HASH);
          yield* finalizedJournal(a);
          const foreign = foreignRow("f2".repeat(28), a.headerHash);
          const b = yield* block(3, foreign.headerHash);
          yield* finalizedJournal(b);
          // F is not processed yet: the walk from B ends at F, and B's
          // fold waits for its base.
          expect(
            yield* finalizeMergesLandedThrough(confirmedAtBlock(b)),
          ).toEqual([b.headerHash]);
          yield* expectUntouched(b.headerHash);
          yield* expectUntouched(a.headerHash);
          // Landed-block processing folds A, then F, oldest first.
          yield* processRow(foreign);
          yield* fold(ownRow(a));
          yield* fold(foreign);
          // Another operator's block on B: the walk from it passes it, folds
          // B on F's fold, and stops at A's fold.
          const above = foreignRow("f4".repeat(28), b.headerHash);
          yield* processRow(above);
          expect(
            yield* finalizeMergesLandedThrough(
              confirmedAt(above.headerHash, 4_999n),
            ),
          ).toEqual([b.headerHash]);
          yield* expectFinalized(b.headerHash);
          yield* expectFinalized(a.headerHash);
          expect(
            yield* finalizeMergesLandedThrough(
              confirmedAt(above.headerHash, 4_999n),
            ),
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
            status: PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
          }),
          confirmedAtBlock(landed),
        ),
    },
    {
      name: "a journal whose base is not its header's parent",
      message: CONFIRMED_MERGE_JOURNAL_UNBOUND,
      arrange: (landed: Block) =>
        Effect.as(
          journal({
            headerHash: landed.headerHash,
            stored: landed.header,
            status: PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
            base: "ff".repeat(28),
          }),
          confirmedAtBlock(landed),
        ),
    },
    {
      name: "an L1 confirmed-state UTxO root other than the journal header's",
      message:
        "L1 confirmed-state UTxO root does not match its header's journal",
      arrange: (landed: Block) =>
        Effect.as(
          finalizedJournal(landed),
          confirmedAtBlock(landed, "cd".repeat(32)),
        ),
    },
    {
      name: "a landed block that is not applied yet",
      message:
        "A landed merge's block is not applied yet, so its merge cannot be finalized",
      arrange: (landed: Block) =>
        Effect.as(
          journal({
            headerHash: landed.headerHash,
            stored: landed.header,
            status: PendingBlockFinalizationsDB.OwnBlockStatus.Landed,
          }),
          confirmedAtBlock(landed),
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
            finalizeMergesLandedThrough(confirmedAtBlock(landed)),
          );
          // The finalization's SQL fold has committed; the MPF check is the
          // last step before the job completes.
          yield* Deferred.await(reached);
          const interrupter = yield* Effect.fork(Fiber.interrupt(walk));
          yield* Deferred.succeed(release, undefined);
          yield* Fiber.join(interrupter);
          yield* expectFinalized(landed.headerHash);
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
