/**
 * `confirmed_ledger` follows the merged queue root (plan §7.3, §10.5, N3,
 * N5): every processed block the root has passed is folded in, in queue
 * order, through the stored-delta fold (`confirmed-merges.ts`), and the
 * frontier names the header `confirmed_ledger` is at.
 *
 * - A foreign block folds once the working-ledger rebase applied it (its
 *   events' rows are then projected to it).
 * - This node's own block folds the same way once its journal is locally
 *   applied (its events are then projected to it); until then the fold
 *   waits on `confirmed_ledger_own_block_pending`. The merge fiber may fold
 *   it first (`finalizeConfirmedMergeProgram`), through the same fold.
 * - A fold whose base is wrong is refused and holds
 *   `confirmed_ledger_base_mismatch`, writing nothing.
 * - A root that is no longer on the frontier's lineage (a rollback undid a
 *   merge) is first rewound to: the retained folds are unfolded back to the
 *   newest header the root's lineage shares with them, by header identity,
 *   never by an equal root. Processing then walks forward from there like
 *   any other run. A root on no lineage the frontier can reach either way
 *   holds the narrowed `confirmed_ledger_behind`.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { ConfirmedLedgerDB } from "../database/index.js";
import { type DriverHold, notRetried } from "../l1-events/driver.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/ledger-hydration.js";
import type { Database } from "../services/database.js";
import {
  ConfirmedLedgerBaseMismatch,
  foldMerge,
  type MergePoint,
  pruneMerges,
  retainedAncestors,
  retrieveMergeLinks,
  setMergePoints,
  unfoldFrontier,
} from "./confirmed-merges.js";
import { lineageBack, type QueueHistory } from "./history.js";
import {
  CONFIRMED_LEDGER_BASE_MISMATCH,
  CONFIRMED_LEDGER_BEHIND,
  CONFIRMED_LEDGER_OWN_BLOCK_PENDING,
} from "./holds.js";
import { processedChain } from "./ledger.js";
import { type LandedBlockPorts, viewChecked } from "./ports.js";
import { Frontier, type HeaderRoot, retrieveRows } from "./store.js";

/** A verdict on the view: the next follower change re-reads it, no timer. */
const behind = (detail: string): DriverHold =>
  notRetried({ reason: CONFIRMED_LEDGER_BEHIND, detail });

const confirmedRoot = Effect.gen(function* () {
  const entries = yield* ConfirmedLedgerDB.retrieve;
  return {
    entries,
    root: yield* computeLedgerMpfRootFromLedgerEntries(entries),
  };
});

/**
 * Sets the frontier when there is none, on the merged root's lineage: at
 * the root if `confirmed_ledger` is there already; else at the latest
 * header of the lineage (`history.ts`) whose post-state root
 * `confirmed_ledger` holds, or at genesis (`confirmed_ledger` holding the
 * configured genesis ledger, or empty and written in). Processing then
 * walks forward from it.
 */
export const bootstrapFrontier = <R>(
  ports: LandedBlockPorts<R>,
  view: View,
  root: HeaderRoot,
  history: Effect.Effect<QueueHistory, unknown, R | Database>,
) =>
  Effect.gen(function* () {
    if ((yield* Frontier.retrieve) !== undefined) return undefined;
    const confirmed = yield* confirmedRoot;
    if (confirmed.root === root.utxosRoot) {
      yield* viewChecked(ports, view, Frontier.upsert(root));
      return undefined;
    }
    const walked =
      root.headerHash === SDK.GENESIS_HEADER_HASH ? undefined : yield* history;
    const lineage =
      walked === undefined
        ? { anchor: SDK.GENESIS_HEADER_HASH, headers: [] }
        : lineageBack(
            walked,
            root.headerHash,
            (hash) =>
              hash === SDK.GENESIS_HEADER_HASH ||
              walked.headers.get(hash)?.utxosRoot === confirmed.root,
          );
    if (lineage !== undefined && lineage.anchor !== SDK.GENESIS_HEADER_HASH) {
      yield* viewChecked(
        ports,
        view,
        Frontier.upsert({
          headerHash: lineage.anchor,
          utxosRoot: confirmed.root,
        }),
      );
      return undefined;
    }
    if (lineage !== undefined) {
      const genesis = yield* ports.genesis;
      const genesisRoot = yield* computeLedgerMpfRootFromLedgerEntries(genesis);
      const expected =
        lineage.headers[0]?.header.prevUtxosRoot ?? root.utxosRoot;
      const frontier = {
        headerHash: SDK.GENESIS_HEADER_HASH,
        utxosRoot: genesisRoot,
      };
      if (genesisRoot === expected && confirmed.root === genesisRoot) {
        yield* viewChecked(ports, view, Frontier.upsert(frontier));
        return undefined;
      }
      if (genesisRoot === expected && confirmed.entries.length === 0) {
        yield* viewChecked(
          ports,
          view,
          Effect.gen(function* () {
            yield* ConfirmedLedgerDB.insertMultiple([...genesis]);
            yield* Frontier.upsert(frontier);
          }),
        );
        return undefined;
      }
    }
    return behind(
      `confirmed_ledger (root ${confirmed.root}) is not at the merged queue root ${root.headerHash} (${root.utxosRoot}) and no header on its retained lineage, nor genesis, reaches it`,
    );
  });

/** The base mismatch `error` carries, if any (a gate may wrap the work's failure). */
const baseMismatchOf = (error: unknown) => {
  const cause = (error as { cause?: unknown } | undefined)?.cause;
  return error instanceof ConfirmedLedgerBaseMismatch
    ? error
    : cause instanceof ConfirmedLedgerBaseMismatch
      ? cause
      : undefined;
};

/** Runs a fold or unfold write; a refused base is the named hold. */
const checkedFold = <R, A>(
  ports: LandedBlockPorts<R>,
  view: View,
  work: Effect.Effect<A, unknown, R | Database>,
) =>
  Effect.gen(function* () {
    const result = yield* Effect.either(viewChecked(ports, view, work));
    if (result._tag === "Right") return undefined;
    const refused = baseMismatchOf(result.left);
    if (refused === undefined) return yield* Effect.fail(result.left);
    return notRetried({
      reason: CONFIRMED_LEDGER_BASE_MISMATCH,
      detail: refused.detail,
    });
  });

export type FoldOutcome =
  | Readonly<{ kind: "at_root" }>
  /** The root is not on the processed chain from the frontier. */
  | Readonly<{ kind: "off_chain"; frontier: HeaderRoot }>
  /** A foreign block before the root waits for the rebase to apply it. */
  | Readonly<{ kind: "awaiting_rebase" }>
  | Readonly<{ kind: "held"; hold: DriverHold }>;

/**
 * Folds every processed block up to the merged root `root`, in order;
 * `mergePoint` names the root output that made a header the queue root.
 */
export const foldToRoot = <R>(
  ports: LandedBlockPorts<R>,
  view: View,
  root: HeaderRoot,
  mergePoint: (
    headerHash: string,
  ) => Effect.Effect<MergePoint | null, unknown, R | Database>,
) =>
  Effect.gen(function* () {
    for (;;) {
      const frontier = yield* Frontier.retrieve;
      if (frontier === undefined)
        return {
          kind: "held",
          hold: behind("confirmed_ledger has no frontier yet"),
        } satisfies FoldOutcome;
      if (frontier.headerHash === root.headerHash) {
        if (frontier.utxosRoot !== root.utxosRoot)
          return {
            kind: "held",
            hold: notRetried({
              reason: CONFIRMED_LEDGER_BASE_MISMATCH,
              detail: `the confirmed-ledger frontier ${frontier.headerHash} has root ${frontier.utxosRoot}, the queue root ${root.utxosRoot}`,
            }),
          } satisfies FoldOutcome;
        return { kind: "at_root" } satisfies FoldOutcome;
      }
      const chain = processedChain(yield* retrieveRows, frontier.headerHash);
      if (!chain.some((row) => row.headerHash === root.headerHash))
        return { kind: "off_chain", frontier } satisfies FoldOutcome;
      const next = chain[0]!;
      if (next.kind === "foreign" && !next.applied)
        return { kind: "awaiting_rebase" } satisfies FoldOutcome;
      if (next.kind === "own") {
        const journal = yield* ports.ownJournal(next.headerHash);
        if (journal?.status !== "locally_applied")
          return {
            kind: "held",
            hold: {
              reason: CONFIRMED_LEDGER_OWN_BLOCK_PENDING,
              detail: `own merged block ${next.headerHash} is not locally applied yet (journal ${journal?.status ?? "missing"})`,
            },
          } satisfies FoldOutcome;
      }
      const point = yield* mergePoint(next.headerHash);
      const refused = yield* checkedFold(ports, view, foldMerge(next, point));
      if (refused !== undefined)
        return { kind: "held", hold: refused } satisfies FoldOutcome;
    }
  });

/**
 * Rewinds `confirmed_ledger` to the merged root's lineage: when the root is
 * neither the frontier nor ahead of it on the processed chain, the retained
 * folds are unfolded back to the newest header the root's lineage (in the
 * queue history) shares with them, or to the root itself. Returns the hold
 * when the root is on no reachable lineage or an unfold is refused.
 */
export const rewindToRoot = <R>(
  ports: LandedBlockPorts<R>,
  view: View,
  root: HeaderRoot,
  history: Effect.Effect<QueueHistory, unknown, R | Database>,
) =>
  Effect.gen(function* () {
    const frontier = yield* Frontier.retrieve;
    if (frontier === undefined || frontier.headerHash === root.headerHash)
      return undefined;
    const chain = processedChain(yield* retrieveRows, frontier.headerHash);
    if (chain.some((row) => row.headerHash === root.headerHash))
      return undefined;
    const ancestors = retainedAncestors(frontier, yield* retrieveMergeLinks);
    const back = ancestors.map((ancestor) => ancestor.headerHash);
    let target = back.indexOf(root.headerHash);
    if (target < 0) {
      const onChain = new Set([
        frontier.headerHash,
        ...chain.map((row) => row.headerHash),
      ]);
      const lineage = lineageBack(
        yield* history,
        root.headerHash,
        (hash) => onChain.has(hash) || back.includes(hash),
      );
      if (lineage === undefined)
        return behind(
          `the merged queue root ${root.headerHash} is on no lineage the confirmed-ledger frontier ${frontier.headerHash} reaches, forward through the queue history or back through its ${(back.length - 1).toString()} retained folds`,
        );
      if (onChain.has(lineage.anchor)) return undefined;
      target = back.indexOf(lineage.anchor);
    }
    return yield* checkedFold(
      ports,
      view,
      Effect.forEach(ancestors.slice(0, target), unfoldFrontier, {
        discard: true,
      }),
    );
  });

/** What a run needs besides the queue. */
export type ProcessOptions = Readonly<{
  /** The follower's prune boundary: no rollback reaches a merge at or below it. */
  prunedThroughSlot?: number;
}>;

/**
 * With the frontier at the root: every retained fold's merge point from the
 * queue history (the root's own from the queue), when one is unknown or the
 * root's was re-created by another merge. Then, wherever the frontier is,
 * the prune of the folds no rollback reaches.
 */
export const settleMerges = <R>(
  ports: LandedBlockPorts<R>,
  view: View,
  root: HeaderRoot,
  point: MergePoint,
  history: Effect.Effect<QueueHistory, unknown, R | Database>,
  options: ProcessOptions,
) =>
  Effect.gen(function* () {
    if ((yield* Frontier.retrieve)?.headerHash === root.headerHash) {
      const links = yield* retrieveMergeLinks;
      const stale =
        [...links.values()].some((link) => link.merge === null) ||
        (links.has(root.headerHash) &&
          links.get(root.headerHash)?.merge?.outRef !== point.outRef);
      if (stale) {
        const walked = yield* history;
        // Every retained fold is the root or its ancestor: the root's merge
        // is no earlier than theirs, so it bounds an unknown one from above.
        const points = new Map(
          [...links.keys()].map((hash) => [
            hash,
            hash === root.headerHash
              ? point
              : (walked.roots.get(hash) ?? point),
          ]),
        );
        yield* viewChecked(ports, view, setMergePoints(points));
      }
    }
    if (options.prunedThroughSlot !== undefined)
      yield* viewChecked(ports, view, pruneMerges(options.prunedThroughSlot));
  });
