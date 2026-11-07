import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { processRow } from "../src/landed-blocks/settlements.js";
import { Frontier, type LandedBlockRow } from "../src/landed-blocks/store.js";
import { finalizeMergesLandedThrough } from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import {
  block,
  confirmedAt,
  expectFinalized,
  expectUntouched,
  finalizedJournal,
  fold,
  foreignRow,
  isolatedDb,
} from "./merge-landed-finalization.fixtures.js";

/**
 * What the walk costs: a node finds unfinalized merges by walking back from
 * the confirmed header, and another operator's blocks carry no journal, so
 * a long run of them is walked only when an unfinished merge of this node
 * can be below it.
 */
describe("landed-merge finalization walk cost", () => {
  const RUN = 100;
  /** A header no landed fact or journal names: the retained facts end there. */
  const GAP = "e0".repeat(28);

  /** `length` processed blocks of another operator, each on the previous,
   * the first on `parentHeaderHash`; oldest first. */
  const foreignRun = (parentHeaderHash: string, length: number) =>
    Effect.gen(function* () {
      const rows: LandedBlockRow[] = [];
      let parent = parentHeaderHash;
      for (let index = 0; index < length; index++) {
        const row = foreignRow(
          `fa${index.toString(16).padStart(54, "0")}`,
          parent,
        );
        yield* processRow(row);
        rows.push(row);
        parent = row.headerHash;
      }
      return rows;
    });

  /**
   * The walk to `confirmedState`, with how many reads of the landed facts
   * (landed rows and retained folds) it made, counted by Postgres for this
   * transaction.
   */
  const walkCost = (confirmedState: SDK.ConfirmedState) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const reads = sql<{ reads: number }>`
        SELECT COALESCE(SUM(seq_scan + COALESCE(idx_scan, 0)), 0)::int AS reads
        FROM pg_stat_xact_user_tables
        WHERE relname IN ('node_landed_blocks', 'node_confirmed_merges')`;
      return yield* sql.withTransaction(
        Effect.gen(function* () {
          const [before] = yield* reads;
          const finalized = yield* finalizeMergesLandedThrough(confirmedState);
          const [after] = yield* reads;
          return { finalized, reads: after!.reads - before!.reads };
        }),
      );
    });

  it.effect(
    "reads no run of another operator's blocks when this node has no unfinished merge",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const run = yield* foreignRun(GAP, RUN);
          const { finalized, reads } = yield* walkCost(
            confirmedAt(run.at(-1)!.headerHash, 1_000_000n),
          );
          expect(finalized).toEqual([]);
          expect(reads).toBeLessThan(RUN);
        }),
      ),
  );

  it.effect(
    "reads no run of another operator's blocks for this node's block above the confirmed header",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const run = yield* foreignRun(GAP, RUN);
          const top = run.at(-1)!;
          // Committed on the run's tip, not merged yet; another operator
          // already built on it.
          const unmerged = yield* block(10, top.headerHash);
          yield* finalizedJournal(unmerged);
          yield* processRow(foreignRow("f5".repeat(28), unmerged.headerHash));
          const { finalized, reads } = yield* walkCost(
            confirmedAt(top.headerHash, 5_000n),
          );
          expect(finalized).toEqual([]);
          expect(reads).toBeLessThan(RUN);
          yield* expectUntouched(unmerged.headerHash);
        }),
      ),
  );

  it.effect(
    "reads no run of another operator's blocks for this node's merge no retained fact links to",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          // A's successor's facts are gone, so no walk can reach A.
          const a = yield* block(1, SDK.GENESIS_HEADER_HASH);
          yield* finalizedJournal(a);
          const run = yield* foreignRun(GAP, RUN);
          const { finalized, reads } = yield* walkCost(
            confirmedAt(run.at(-1)!.headerHash, 1_000_000n),
          );
          expect(finalized).toEqual([]);
          expect(reads).toBeLessThan(RUN);
          yield* expectUntouched(a.headerHash);
        }),
      ),
  );

  it.effect(
    "finalizes this node's merge below a long run of another operator's blocks, folded and processed",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const a = yield* block(1, SDK.GENESIS_HEADER_HASH);
          yield* finalizedJournal(a);
          const run = yield* foreignRun(a.headerHash, RUN);
          yield* Frontier.upsert({
            headerHash: a.headerHash,
            utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          });
          for (const row of run.slice(0, RUN / 2)) yield* fold(row);
          expect(
            yield* finalizeMergesLandedThrough(
              confirmedAt(run.at(-1)!.headerHash, 1_000_000n),
            ),
          ).toEqual([a.headerHash]);
          yield* expectFinalized(a.headerHash, 1);
        }),
      ),
  );
});
