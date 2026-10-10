/**
 * The pending-table marks of landed blocks (plan §7.3, N3): every write that
 * makes a landed row processed marks the rows it includes in its own
 * transaction (the processing insert, a reland, and the rebase that applies
 * the row), and a rollback that takes the block off the landed chain clears
 * them (`mempoolInclusions.ts`).
 */
import { Effect } from "effect";

import * as MempoolInclusionsDB from "../database/mempoolInclusions.js";
import {
  deleteRows,
  insertRow,
  type LandedBlockRow,
  setState,
} from "./store.js";

/** Marks the pending-table rows each block in `rows` includes. */
export const markRows = (
  rows: readonly Pick<LandedBlockRow, "headerHash" | "txIds">[],
) =>
  Effect.forEach(
    rows,
    (row) => MempoolInclusionsDB.markIncluded(row.headerHash, row.txIds),
    { discard: true },
  );

/** Processes a landed block: inserts its row and marks the pending-table
 * rows it includes. */
export const processRow = (row: LandedBlockRow) =>
  Effect.gen(function* () {
    yield* insertRow(row);
    yield* markRows([row]);
  });

/**
 * A rollback took the processed rows `left` off the landed chain: an
 * applied row (foreign, or own) stays as `removed` until the rebase reverts
 * it (and disposes of an own row's journal), every other row goes, and the
 * marks they set are cleared, so the rows they included are pending again.
 * The removed rows `relands` landed again before a rebase reverted them:
 * they are processed again and mark their rows again.
 */
export const rollBackRows = (
  left: readonly LandedBlockRow[],
  relands: readonly LandedBlockRow[],
) =>
  Effect.gen(function* () {
    yield* setState(
      left.filter((row) => row.applied).map((row) => row.headerHash),
      "removed",
    );
    yield* deleteRows(
      left.filter((row) => !row.applied).map((row) => row.headerHash),
    );
    yield* MempoolInclusionsDB.clearMarks(left.map((row) => row.headerHash));
    yield* setState(
      relands.map((row) => row.headerHash),
      "processed",
    );
    yield* markRows(relands);
  });
