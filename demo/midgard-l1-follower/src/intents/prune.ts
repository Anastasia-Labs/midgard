import type { Dialect, SqlTx } from "../sql/backend.js";
import type { PruneHook } from "../store/context.js";
import { readCursor } from "../store/rows.js";
import { chunks, distinctHashes, placeholders } from "./journal.js";
import { deriveIntentClosureIn } from "./status.js";
import {
  allIntentsIn,
  dependantsIn,
  intentsAbandonedThroughIn,
  intentsExpiringIn,
  intentsTouchedBySpendsIn,
  readPruneMarkIn,
  rollbacksExplainIn,
  writePruneMarkIn,
} from "./windows.js";

/**
 * The intents that may be terminal at or below `boundarySlot`, read by
 * index (§8.2 retention, bounded per step). An intent's terminal slot is
 * the least of its landing slot, its earliest conflicting spend, its
 * `invalid_hereafter` once passed, a dead dependency's terminal slot, and
 * its first abandon event's tip slot. So an intent terminal at or below the
 * boundary is one of:
 *
 * - an intent with an input, reference input or collateral spent at or
 *   below it (landed, failed or conflicted there: a landing spends the
 *   intent's inputs, a phase-2 failure its collateral);
 * - an intent whose `invalid_hereafter` is at or below it;
 * - an intent abandoned at a tip slot at or below it;
 * - a dependant (transitively) of one of those.
 *
 * Each of the first three sets holds only intents this step deletes, but
 * for an abandoned intent that later landed above the boundary (kept until
 * its landing is k deep). Spent outputs at or below the boundary are
 * deleted by the same prune step after the hooks, so the spend read covers
 * the outputs spent since the previous step plus any budget backlog. None
 * of the reads grows with the number of retained intents.
 */
const candidatesIn = async (
  tx: SqlTx,
  boundarySlot: number,
): Promise<Buffer[]> => {
  const range = { after: null, through: boundarySlot };
  const seeds = distinctHashes([
    ...(await intentsTouchedBySpendsIn(tx, range)),
    ...(await intentsExpiringIn(tx, range)),
    ...(await intentsAbandonedThroughIn(tx, boundarySlot)),
  ]);
  return [...seeds, ...(await dependantsIn(tx, seeds))];
};

/**
 * Whether this run derives every retained intent instead of the candidates:
 * the hook's first run, and its first run after a generation change the
 * rollback log does not explain (a reset, after whose replay prune steps
 * may have run without this hook and deleted spent outputs it never read).
 * The mark (`l1_intent_prune_mark`) is written in the step's transaction,
 * so a restart neither loses a pending full run nor repeats a done one.
 */
const fullRunDue = async (
  tx: SqlTx,
  cursor: Readonly<{
    generation: number;
    origin: Readonly<{ slot: number; hash: Buffer }>;
  }>,
): Promise<boolean> => {
  const mark = await readPruneMarkIn(tx);
  return (
    mark === null ||
    !(await rollbacksExplainIn(
      tx,
      mark.generation,
      cursor.generation,
      cursor.origin,
    ))
  );
};

/**
 * Deletes every intent terminal for k blocks (§8.2 retention): its
 * `terminalSlot` is at or below the prune boundary (the block k below the
 * cursor). Runs in the follower's prune step before spent outputs are
 * deleted at the same boundary, so a retained intent never loses a fact its
 * status reads; a dependant whose only dead reason is a pruned dependency
 * shares that dependency's terminal slot and goes in the same step. Events
 * and spend rows go with their intent (cascade). Reads only the candidates
 * (`candidatesIn`) and their dependencies, but on a run `fullRunDue` names,
 * which derives every retained intent. Returns the number deleted.
 */
export const pruneIntentsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  boundarySlot: number,
): Promise<number> => {
  const cursor = await readCursor(tx, dialect);
  if (cursor === null) return 0;
  const candidates = (await fullRunDue(tx, cursor))
    ? await allIntentsIn(tx)
    : await candidatesIn(tx, boundarySlot);
  await writePruneMarkIn(tx, { generation: cursor.generation, boundarySlot });
  if (candidates.length === 0) return 0;
  const states = await deriveIntentClosureIn(tx, dialect, cursor, candidates);
  const terminal = [...states.values()]
    .filter(
      (state) =>
        state.terminalSlot !== null && state.terminalSlot <= boundarySlot,
    )
    .map((state) => state.intent.txHash);
  for (const chunk of chunks(terminal))
    await tx.query(
      `DELETE FROM l1_intents WHERE tx_hash IN (${placeholders(chunk.length)})`,
      chunk,
    );
  return terminal.length;
};

/** The journal's retention, as a follower prune hook. */
export const INTENT_PRUNE_HOOK: PruneHook = {
  kind: "retention",
  table: "l1_intents",
  apply: ({ tx, dialect, boundarySlot }) =>
    pruneIntentsIn(tx, dialect, boundarySlot),
};
