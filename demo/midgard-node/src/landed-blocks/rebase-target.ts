/**
 * What the working ledger and the native MPF must hold: the
 * processed landed chain from the confirmed-ledger frontier to its tip `T`,
 * then this node's live own block, its live journal built on `T`. Own
 * journals whose block can no longer land are disposed of by the same rebase
 * (`own-journals.ts`). The rebase cannot run while the live journal is built
 * on anything but `T` (a base it cannot place yet): S6 derives that commit
 * dead or landed. Nor can it run while a pending-table row is marked
 * by a block the target neither holds nor disposes of, nor `confirmed_ledger`
 * holds: that row is neither pending nor in the base until processing takes
 * the block in or a rollback clears the mark. A
 * folded block's rows stay marked until its fold is final (`final-folds.ts`);
 * they are in the base, so they never block.
 */
import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import * as MempoolInclusionsDB from "../database/mempoolInclusions.js";
import type { DriverHold } from "../l1-events/driver.js";
import { followerWriteHeld } from "../services/follower-write-gate.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import { retrieveMergeLinks } from "./confirmed-merges.js";
import { LANDED_BLOCK_REBASE_PENDING } from "./holds.js";
import { liveJournal } from "./journal.js";
import { type LandedChain, landedChain } from "./ledger.js";
import {
  type OwnJournalDisposition,
  ownJournalDisposition,
} from "./own-journals.js";
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
  /** The landed chain the target is over. */
  landed: LandedChain;
  /** The processed chain then the live own block, in order. */
  steps: readonly TargetStep[];
  /** The processed tip `T`. */
  tip: HeaderRoot;
  live: (OwnJournal & { headerHash: string }) | undefined;
  /** The dead own journals, and those the rebase disposes of. */
  journals: OwnJournalDisposition;
}>;

/** The blocked detail while a block the target does not hold marks rows. */
export const AWAITING_MARKING_BLOCK = "awaiting landed-block processing";

export type RebasePlan =
  | Readonly<{ kind: "none" }>
  | Readonly<{
      kind: "blocked";
      detail: string;
    }>
  | Readonly<{
      kind: "ready";
      target: RebaseTarget;
      /** Set when the native MPF's root is the only reason it is due. */
      drift?: NativeRootDrift;
    }>;

/** The native MPF's durable root, away from the target's end root. */
export type NativeRootDrift = Readonly<{
  durableRoot: string;
  targetRoot: string;
}>;

/** The root the target ends at: the live own block's, or the processed tip's. */
export const targetRoot = (target: RebaseTarget) =>
  target.steps.at(-1)?.utxosRoot ?? target.landed.frontier.utxosRoot;

/**
 * Where the native MPF's durable root stands against the target's end
 * root: undefined when it is there. The owner's diagnostics read its stored
 * durable marker; only a generation still open (a fork not yet promoted or
 * discarded) has its reachable records walked for the byte count.
 */
export const nativeRootDrift = (
  owner: NativeMpfOwnerService,
  target: RebaseTarget,
) =>
  Effect.map(
    Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) => cause,
    }),
    ({ durableRoot }): NativeRootDrift | undefined => {
      const end = targetRoot(target);
      return durableRoot === end ? undefined : { durableRoot, targetRoot: end };
    },
  );

/**
 * The hold a blocked rebase plan holds: it waits on landed processing,
 * which the driver's backoff runs again.
 */
export const blockedRebaseHold = (
  plan: Extract<RebasePlan, { kind: "blocked" }>,
): DriverHold => ({ reason: LANDED_BLOCK_REBASE_PENDING, detail: plan.detail });

const NO_FRONTIER: Extract<RebasePlan, { kind: "blocked" }> = {
  kind: "blocked",
  detail: "confirmed_ledger has no frontier yet",
};
const NOT_DUE: Extract<RebasePlan, { kind: "none" }> = { kind: "none" };

/** The target over `rows` and their landed chain `landed`. */
const targetOn = (
  rows: readonly LandedBlockRow[],
  landed: LandedChain,
  journals: OwnJournalDisposition,
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
    const active = yield* liveJournal(journals.dead);
    let live: RebaseTarget["live"];
    if (active !== undefined) {
      if (
        active.baseTailHeaderHash !== tip.headerHash ||
        active.baseUtxosRoot !== tip.utxosRoot
      )
        return {
          kind: "blocked",
          detail: `awaiting own journal resolution: live own block ${active.headerHash} is built on ${active.baseTailHeaderHash}, the processed landed tip is ${tip.headerHash}`,
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
      target: { rows, landed, steps, tip, live, journals },
    } satisfies RebasePlan;
  });

/**
 * The rebase target over `rows` and the own-journal disposition `journals`
 * read in the same transaction, due or not: what the working ledger and
 * the native MPF hold once the rebase runs, or why it cannot run.
 */
export const rebaseTargetOf = (
  rows: readonly LandedBlockRow[],
  journals: OwnJournalDisposition,
) =>
  Effect.gen(function* () {
    const landed = yield* landedChain(rows);
    if (landed === undefined) return NO_FRONTIER;
    return yield* targetOn(rows, landed, journals);
  });

/**
 * Whether a rebase is due over `journals` (a landed row the working ledger
 * does not match, a dead own journal to dispose of, or the native MPF
 * `owner` away from the target's end root) and can run, with its target
 * over the landed chain; it reads no `confirmed_ledger` row. Without
 * `owner`, the native root is not read.
 */
export const rebasePlanOf = (
  journals: OwnJournalDisposition,
  owner?: NativeMpfOwnerService,
) =>
  Effect.gen(function* () {
    const rows = yield* retrieveRows;
    const landed = yield* landedChain(rows);
    if (landed === undefined) return rebaseNeeded(rows) ? NO_FRONTIER : NOT_DUE;
    const due = rebaseNeeded(rows) || journals.dispose.length > 0;
    if (!due && owner === undefined) return NOT_DUE;
    const on: RebasePlan = yield* targetOn(rows, landed, journals);
    if (due) return on;
    if (owner === undefined || on.kind !== "ready") return NOT_DUE;
    const drift = yield* nativeRootDrift(owner, on.target);
    return drift === undefined ? NOT_DUE : { ...on, drift };
  });

/** Whether a rebase is due and can run (`rebasePlanOf`), over the
 * own-journal disposition it reads first. */
export const rebasePlan = (owner?: NativeMpfOwnerService) =>
  Effect.flatMap(ownJournalDisposition, (journals) =>
    rebasePlanOf(journals, owner),
  );

/**
 * The dead own journals, read again in the repair's transaction: the
 * rebase of `target` holds when they are not the ones it disposes of.
 */
export const deadJournalsFor = (target: RebaseTarget | undefined) =>
  Effect.gen(function* () {
    const dead = PendingBlockFinalizationsDB.deadHeadersOf(
      yield* PendingBlockFinalizationsDB.deadJournals,
    );
    const named = new Set(dead.map((header) => header.toString("hex")));
    const same =
      target === undefined ||
      (named.size === target.journals.dead.length &&
        target.journals.dead.every((header) =>
          named.has(header.toString("hex")),
        ));
    if (!same)
      return yield* Effect.fail(
        followerWriteHeld(
          LANDED_BLOCK_REBASE_PENDING,
          "the dead own journals changed after the rebase target was read",
        ),
      );
    return dead;
  });

/** The root after each step of the target; index 0 is the frontier's. */
export const targetRoots = (target: RebaseTarget) => [
  target.landed.frontier.utxosRoot,
  ...target.steps.map((step) => step.utxosRoot),
];
