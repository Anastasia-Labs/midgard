/**
 * `confirmed_ledger` follows the merged queue root (plan §7.3, §10.5, N3,
 * N5): every processed block the root has passed is folded in, in queue
 * order, through the stored-delta fold (`confirmed-merges.ts`), and the
 * frontier names the header `confirmed_ledger` is at.
 *
 * - A block, foreign or this node's own, folds once the working-ledger
 *   rebase applied it (its events' rows are then projected to it). This
 *   is the only runtime fold; the merge fiber writes no local ledger.
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
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Either } from "effect";

import { ConfirmedLedgerDB } from "../database/index.js";
import type { ForcedAdmissionDivergences } from "../forced-orders/admission-divergence.js";
import { type DriverHold, notRetried } from "../l1-events/driver.js";
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
import { holdFrontierRoot } from "./frontier-root.js";
import { lineageBack, type QueueHistory } from "./history.js";
import {
  CONFIRMED_LEDGER_BASE_MISMATCH,
  CONFIRMED_LEDGER_BEHIND,
} from "./holds.js";
import { processedChain } from "./ledger.js";
import { type LandedBlockPorts, viewChecked } from "./ports.js";
import { Frontier, type HeaderRoot, retrieveRows } from "./store.js";

/** A verdict on the view: the next follower change re-reads it, no timer. */
const behind = (detail: string): DriverHold =>
  notRetried({ reason: CONFIRMED_LEDGER_BEHIND, detail });

/**
 * Sets the frontier when there is none, on the merged root's lineage: at
 * the root if it is the empty ledger's; else at the latest header of the
 * lineage (`history.ts`) whose post-state root is the empty ledger's, or at
 * genesis, with the configured genesis ledger written into
 * `confirmed_ledger` at the lineage's first parent root. Processing then
 * walks forward from it.
 *
 * `confirmed_ledger` is written only in the transaction that sets or moves
 * the frontier (here, `foldMerge`, `unfoldFrontier`), so with no frontier
 * it holds no row and its root is the empty ledger's. The node admits the
 * configured genesis into the ledger store at startup, checked against the
 * clean queue's root, so the configured genesis ledger has the chain's
 * genesis root and is not hashed again here.
 */
export const bootstrapFrontier = <R>(
  ports: LandedBlockPorts<R>,
  view: View,
  root: HeaderRoot,
  history: Effect.Effect<QueueHistory, unknown, R | Database>,
) =>
  Effect.gen(function* () {
    if ((yield* Frontier.retrieve) !== undefined) return undefined;
    const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
    if (root.utxosRoot === empty) {
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
              walked.headers.get(hash)?.utxosRoot === empty,
          );
    if (lineage !== undefined && lineage.anchor !== SDK.GENESIS_HEADER_HASH) {
      yield* viewChecked(
        ports,
        view,
        Frontier.upsert({
          headerHash: lineage.anchor,
          utxosRoot: empty,
        }),
      );
      return undefined;
    }
    if (lineage !== undefined) {
      const genesisRoot =
        lineage.headers[0]?.header.prevUtxosRoot ?? root.utxosRoot;
      const rows = yield* ports.genesis;
      yield* viewChecked(
        ports,
        view,
        Effect.gen(function* () {
          if (rows.length > 0)
            yield* ConfirmedLedgerDB.insertMultiple([...rows]);
          yield* Frontier.upsert({
            headerHash: SDK.GENESIS_HEADER_HASH,
            utxosRoot: genesisRoot,
          });
        }),
      );
      return undefined;
    }
    return behind(
      `confirmed_ledger is empty and the merged queue root ${root.headerHash} (${root.utxosRoot}) is not on a retained lineage from genesis or from a header whose ledger is empty`,
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

/** Runs a fold or unfold write: its committed result, or a refused base
 * as the named hold. */
const checkedFold = <R, A>(
  ports: LandedBlockPorts<R>,
  view: View,
  work: Effect.Effect<A, unknown, R | Database>,
) =>
  Effect.gen(function* () {
    const result = yield* Effect.either(viewChecked(ports, view, work));
    if (Either.isRight(result)) return Either.right(result.right);
    const refused = baseMismatchOf(result.left);
    if (refused === undefined) return yield* Effect.fail(result.left);
    return Either.left(
      notRetried({
        reason: CONFIRMED_LEDGER_BASE_MISMATCH,
        detail: refused.detail,
      }),
    );
  });

export type FoldOutcome =
  | Readonly<{ kind: "at_root" }>
  /** The root is not on the processed chain from the frontier. */
  | Readonly<{ kind: "off_chain"; frontier: HeaderRoot }>
  /** A block before the root, foreign or own, waits for the rebase to apply it. */
  | Readonly<{ kind: "awaiting_rebase" }>
  | Readonly<{ kind: "held"; hold: DriverHold }>;

/**
 * Folds every processed block up to the merged root `root`, in order;
 * `mergePoint` names the root output that made a header the queue root.
 * Each fold, once committed, goes to the `folded` port with the frontier
 * generations it read and left: the one place the node's confirmed-ledger
 * verification advances (`advanceVerifiedConfirmedLedger`).
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
      // Before each fold and once the frontier stops: a new block whose
      // parent is the frontier may be checked on its root.
      yield* holdFrontierRoot(ports, frontier);
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
      if (!next.applied)
        return { kind: "awaiting_rebase" } satisfies FoldOutcome;
      const point = yield* mergePoint(next.headerHash);
      const folded = yield* checkedFold(ports, view, foldMerge(next, point));
      if (Either.isLeft(folded))
        return { kind: "held", hold: folded.left } satisfies FoldOutcome;
      if (ports.folded !== undefined)
        yield* ports.folded({ row: next, ...folded.right });
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
    const unfolded = yield* checkedFold(
      ports,
      view,
      Effect.forEach(ancestors.slice(0, target), unfoldFrontier, {
        discard: true,
      }),
    );
    return Either.isLeft(unfolded) ? unfolded.left : undefined;
  });

/** What a run needs besides the queue. */
export type ProcessOptions = Readonly<{
  /** The follower's prune boundary: no rollback reaches a merge at or below it. */
  prunedThroughSlot?: number;
  /** Called with each block's header hash once its row is committed. */
  committed?: (headerHash: string) => void;
  /**
   * Where processing names the admission divergence of a forced row it
   * writes (`screenForcedAdmissions`): the forced-order hook's register.
   */
  divergences?: ForcedAdmissionDivergences;
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
