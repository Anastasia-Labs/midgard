/**
 * What the working ledger and the native MPF must hold (plan §7.3, N3): the
 * processed landed chain from the confirmed-ledger frontier to its tip `T`,
 * then this node's live own block, the active journal built on `T` that has
 * not landed yet. Own journals whose block cannot be on the landed chain are
 * disposed of, and abandoned ones whose block landed revived, by the same
 * rebase (`own-journals.ts`). The rebase cannot run while a journal it keeps
 * is built on anything but `T` (a base it cannot place yet): S6 derives that
 * commit dead or landed. Nor can it run while a pending-table row is marked
 * by a block the target neither holds nor disposes of, nor `confirmed_ledger`
 * holds (this node's block between its local finalization and its
 * processing): that row is neither pending nor in the base until processing
 * takes the block in or the reopening of its journal clears the mark. A
 * folded block's rows stay marked until its fold is final (`final-folds.ts`);
 * they are in the base, so they never block.
 */
import { Effect } from "effect";

import * as MempoolInclusionsDB from "../database/mempoolInclusions.js";
import { ledgerOutputToInsertBatchOp } from "../mpf/ledger-delta.js";
import type { NativeMpfEventOp } from "../services/mpf-native-owner/service.normalize-owner-options.js";
import { retrieveMergeLinks } from "./confirmed-merges.js";
import { activeJournal } from "./journal.js";
import {
  applyDelta,
  type LandedLedger,
  landedLedger,
  type LedgerMap,
  ledgerMap,
} from "./ledger.js";
import {
  type OwnJournalDisposition,
  ownJournalDisposition,
} from "./own-journals.js";
import type { OwnJournal } from "./ports.js";
import { rebaseNeeded } from "./process.js";
import { type RetiredPlan, retiredPlans } from "./retired-plans.js";
import { type HeaderRoot, type LandedBlockRow, retrieveRows } from "./store.js";

/** One step of the target: a processed row, or the live own block. */
export type TargetStep = Readonly<{
  headerHash: string;
  utxosRoot: string;
  kind: "foreign" | "own" | "live";
  spent: readonly Buffer[];
  produced: LandedBlockRow["produced"];
  /** The landed row, for processed steps. */
  row?: LandedBlockRow;
}>;

export type RebaseTarget = Readonly<{
  rows: readonly LandedBlockRow[];
  landed: LandedLedger;
  /** The processed chain then the live own block, in order. */
  steps: readonly TargetStep[];
  /** The processed tip `T`. */
  tip: HeaderRoot;
  live: (OwnJournal & { headerHash: string }) | undefined;
  /** The own journals the rebase disposes of and revives. */
  journals: OwnJournalDisposition;
  /** The retained plans of retired kinds the rebase discards. */
  retired: readonly RetiredPlan[];
}>;

/** The blocked detail while a block the target does not hold marks rows. */
export const AWAITING_MARKING_BLOCK = "awaiting landed-block processing";

export type RebasePlan =
  | Readonly<{ kind: "none" }>
  | Readonly<{ kind: "blocked"; detail: string }>
  | Readonly<{ kind: "ready"; target: RebaseTarget }>;

const NO_FRONTIER = {
  kind: "blocked",
  detail: "confirmed_ledger has no frontier yet",
} as const satisfies RebasePlan;

/** The target over `rows` and their landed chain `landed`. */
const targetOn = (
  rows: readonly LandedBlockRow[],
  landed: LandedLedger,
  journals: OwnJournalDisposition,
  retired: readonly RetiredPlan[],
) =>
  Effect.gen(function* () {
    const onChain = new Set(landed.chain.map((row) => row.headerHash));
    const stray = rows.find(
      (row) => row.state === "processed" && !onChain.has(row.headerHash),
    );
    if (stray !== undefined)
      return {
        kind: "blocked",
        detail: `processed block ${stray.headerHash} does not chain from the confirmed-ledger frontier ${landed.frontier.headerHash}`,
      } satisfies RebasePlan;
    const last = landed.chain.at(-1);
    const tip: HeaderRoot = last ?? landed.frontier;
    const steps: TargetStep[] = landed.chain.map((row) => ({
      headerHash: row.headerHash,
      utxosRoot: row.utxosRoot,
      kind: row.kind,
      spent: row.spent,
      produced: row.produced,
      row,
    }));
    const disposed = new Set(
      journals.dispose.map((disposal) => disposal.headerHash),
    );
    const active = yield* activeJournal;
    const processed = new Set(rows.map((row) => row.headerHash));
    let live: RebaseTarget["live"];
    if (
      active !== undefined &&
      !processed.has(active.headerHash) &&
      !disposed.has(active.headerHash)
    ) {
      if (
        active.baseTailHeaderHash !== tip.headerHash ||
        active.baseUtxosRoot !== tip.utxosRoot
      )
        return {
          kind: "blocked",
          detail: `awaiting own journal resolution: active own block ${active.headerHash} is built on ${active.baseTailHeaderHash}, the processed landed tip is ${tip.headerHash}`,
        } satisfies RebasePlan;
      live = active;
      steps.push({
        headerHash: active.headerHash,
        utxosRoot: active.expectedUtxosRoot,
        kind: "live",
        spent: active.spent,
        produced: active.produced,
      });
    }
    const held = new Set([
      ...steps.map((step) => step.headerHash),
      ...disposed,
      ...(yield* retrieveMergeLinks).keys(),
    ]);
    const unheld = (yield* MempoolInclusionsDB.markingHeaders).find(
      (headerHash) => !held.has(headerHash),
    );
    if (unheld !== undefined)
      return {
        kind: "blocked",
        detail: `${AWAITING_MARKING_BLOCK}: block ${unheld} marks pending-table rows as included and the processed landed chain does not hold it`,
      } satisfies RebasePlan;
    return {
      kind: "ready",
      target: { rows, landed, steps, tip, live, journals, retired },
    } satisfies RebasePlan;
  });

/**
 * The rebase target over `rows`, due or not: what the working ledger and
 * the native MPF hold once the rebase runs, or why it cannot run.
 */
export const rebaseTargetOf = (rows: readonly LandedBlockRow[]) =>
  Effect.gen(function* () {
    const landed = yield* landedLedger(rows);
    if (landed === undefined) return NO_FRONTIER;
    return yield* targetOn(
      rows,
      landed,
      yield* ownJournalDisposition(rows, landed),
      yield* retiredPlans,
    );
  });

/**
 * Reads the rebase target, and whether a rebase is due (a landed row the
 * working ledger does not match, an own journal to dispose of, or a
 * retained plan of a retired kind, `retired-plans.ts`) and can run.
 */
export const rebasePlan = Effect.gen(function* () {
  const rows = yield* retrieveRows;
  const retired = yield* retiredPlans;
  const landed = yield* landedLedger(rows);
  if (landed === undefined)
    return rebaseNeeded(rows) || retired.length > 0
      ? (NO_FRONTIER as RebasePlan)
      : ({ kind: "none" } satisfies RebasePlan);
  const journals = yield* ownJournalDisposition(rows, landed);
  if (
    !rebaseNeeded(rows) &&
    journals.dispose.length === 0 &&
    retired.length === 0
  )
    return { kind: "none" } satisfies RebasePlan;
  return yield* targetOn(rows, landed, journals, retired);
});

/** A step's MPF mutation: its spends and replaced outputs, then its outputs. */
export type StepEvents = readonly (readonly NativeMpfEventOp[])[];

/**
 * Walks the target from the frontier: the root after each step (index 0 is
 * the frontier's), each step's MPF events against its parent's ledger, and
 * the ledger at the end.
 */
export const walkTarget = (target: RebaseTarget) => {
  const ledger: LedgerMap = ledgerMap(target.landed.confirmed);
  const roots = [target.landed.frontier.utxosRoot];
  const events: StepEvents[] = [];
  for (const step of target.steps) {
    const deletes = [
      ...new Set([
        ...step.spent.map((outRef) => outRef.toString("hex")),
        ...step.produced
          .map((entry) => entry.outref.toString("hex"))
          .filter((key) => ledger.has(key)),
      ]),
    ]
      .sort()
      .map((key) => ({
        type: "delete" as const,
        key: Buffer.from(key, "hex"),
      }));
    const inserts = step.produced.map((entry) =>
      ledgerOutputToInsertBatchOp({
        outRef: entry.outref,
        outputCbor: entry.output,
      }),
    );
    events.push([deletes, inserts].filter((event) => event.length > 0));
    applyDelta(ledger, step);
    roots.push(step.utxosRoot);
  }
  return { ledger, roots, events };
};
