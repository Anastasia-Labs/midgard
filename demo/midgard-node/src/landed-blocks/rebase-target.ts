/**
 * What the working ledger and the native MPF must hold (plan §7.3, N3): the
 * processed landed chain from the confirmed-ledger frontier to its tip `T`,
 * then this node's live own block, the active journal built on `T` that has
 * not landed yet. The rebase cannot run while the active journal is built on
 * anything else (its base left the queue, or another block took its slot):
 * that journal must be resolved first (released, replaced or revived).
 */
import { Effect } from "effect";

import { ledgerOutputToInsertBatchOp } from "../mpf/ledger-delta.js";
import type { NativeMpfEventOp } from "../services/mpf-native-owner/service.normalize-owner-options.js";
import { activeJournal } from "./journal.js";
import {
  applyDelta,
  type LandedLedger,
  landedLedger,
  type LedgerMap,
  ledgerMap,
} from "./ledger.js";
import type { OwnJournal } from "./ports.js";
import { rebaseNeeded } from "./process.js";
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
}>;

export type RebasePlan =
  | Readonly<{ kind: "none" }>
  | Readonly<{ kind: "blocked"; detail: string }>
  | Readonly<{ kind: "ready"; target: RebaseTarget }>;

/**
 * The rebase target over `rows`, due or not: what the working ledger and
 * the native MPF hold once the rebase runs, or why it cannot run.
 */
export const rebaseTargetOf = (rows: readonly LandedBlockRow[]) =>
  Effect.gen(function* () {
    const landed = yield* landedLedger(rows);
    if (landed === undefined)
      return {
        kind: "blocked",
        detail: "confirmed_ledger has no frontier yet",
      } satisfies RebasePlan;
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
    const active = yield* activeJournal;
    const processed = new Set(rows.map((row) => row.headerHash));
    let live: RebaseTarget["live"];
    if (active !== undefined && !processed.has(active.headerHash)) {
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
    return {
      kind: "ready",
      target: { rows, landed, steps, tip, live },
    } satisfies RebasePlan;
  });

/** Reads the rebase target, and whether a rebase is due and can run. */
export const rebasePlan = Effect.gen(function* () {
  const rows = yield* retrieveRows;
  if (!rebaseNeeded(rows)) return { kind: "none" } satisfies RebasePlan;
  return yield* rebaseTargetOf(rows);
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
