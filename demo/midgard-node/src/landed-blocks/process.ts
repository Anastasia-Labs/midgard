/**
 * In-order landed-block processing (plan §7.3, §5.5 P3, P10, N3, N5). On
 * every driver run, at the follower view the run applies:
 *
 * 1. `confirmed_ledger` is rewound to the merged queue root's lineage, then
 *    folded up to the root (`fold.ts`, `confirmed-merges.ts`). A root a
 *    rollback moved back unfolds the retained folds down to the newest
 *    header the root's lineage shares with them. A root that passed blocks
 *    this node never processed (an append and its merge in one run, or a
 *    hold that outlasted maturity) is reached by its lineage in the queue
 *    history (`history.ts`): the walk below starts at the last processed
 *    header on it and takes the merged headers first.
 * 2. The landed queue is walked from the root (or that header). A node already processed on
 *    the same parent is kept (a `removed` one relands); every processed row
 *    the walk did not reach left the queue: a row the working ledger took in
 *    becomes `removed` (the rebase reverts it), any other is deleted,
 *    and the pending-table marks they set are cleared (`settlements.ts`).
 *    The working ledger is rewound by that walk plus the rebase's recompute.
 * 3. Each new node, in queue order and exactly once (the row's primary key),
 *    must link to its parent (hash, root, start time). This node's own block
 *    is adopted from its journal and never replayed (its journal must
 *    describe the block and its delta reach the header's root on the
 *    parent's ledger, or it holds the local-fault
 *    `landed_block_own_journal_mismatch`); one whose journal was
 *    abandoned (whichever lands wins) is adopted unapplied for the rebase to
 *    revive, and the blocks after it hold `landed_block_own_revival_pending`
 *    until the commit path finalized it locally. A foreign block is
 *    replayed on its parent's ledger and must reach its header's root. A
 *    block that does not is never recorded: it holds `landed_block_invalid`
 *    and nothing after it is processed. A mismatch never feeds a fault-proof
 *    path.
 * 4. A row the working ledger does not hold yet (a foreign or a revived own
 *    one), or a removed row, runs the rebase (the driver's recompute, which
 *    also disposes of the own journals that cannot land). One that cannot
 *    finish yet holds its reason (`landed_block_rebase_pending` and its
 *    detail, or `landed_block_rebase_failed` with the failure), and the
 *    driver retries it on its backoff.
 * 5. With the frontier at the root, the retained folds' merge points are
 *    brought up to the queue history, and the folds whose merge is at or
 *    below the follower's prune boundary are dropped.
 *
 * Every write (fold, unfold and bootstrap included) re-checks the
 * follower view in its transaction; a view that moved ends the run with the
 * transient `landed_blocks_waiting` ("view moved"), which the next run, the
 * move's, replaces. Every hold keeps the process up and clears on a later
 * run.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { type DriverHold, notRetried } from "../l1-events/driver.js";
import type { LandedStateQueue } from "../l1-state-queue/index.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/ledger-hydration.js";
import type { Database } from "../services/database.js";
import { followerWriteHoldOf } from "../services/follower-write-gate.js";
import { failureHold } from "../services/l1-follower.failure-hold.js";
import type { MergePoint } from "./confirmed-merges.js";
import {
  bootstrapFrontier,
  foldToRoot,
  type ProcessOptions,
  rewindToRoot,
  settleMerges,
} from "./fold.js";
import {
  decodeHistory,
  type LandedNode,
  lineageBack,
  type QueueHistory,
} from "./history.js";
import {
  combineHolds,
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_DA_REFETCH_PENDING,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
  LANDED_BLOCK_OWN_REVIVAL_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCKS_WAITING,
} from "./holds.js";
import {
  applyDelta,
  landedChain,
  landedStart,
  ledgerAfter,
  ledgerEntries,
  type LedgerMap,
  netDelta,
} from "./ledger.js";
import { type LandedBlockPorts, viewChecked, ViewMoved } from "./ports.js";
import { processRow, rollBackRows } from "./settlements.js";
import { type LandedBlockRow, retrieveRows } from "./store.js";

type Parent = Readonly<{
  headerHash: string;
  utxosRoot: string;
  endTime: bigint;
}>;

const decodeQueue = (queue: LandedStateQueue) =>
  Effect.gen(function* () {
    const root = queue.root;
    if (root === null) return undefined;
    const state = (yield* SDK.getConfirmedStateFromStateQueueDatum(
      root.element.datum,
    )).data;
    const nodes: LandedNode[] = [];
    for (const node of queue.nodes)
      nodes.push({
        headerHash: node.headerHash,
        header: yield* SDK.getHeaderFromStateQueueDatum(node.element.datum),
      });
    return {
      root: {
        headerHash: state.headerHash,
        utxosRoot: state.utxoRoot,
        endTime: state.endTime,
      } satisfies Parent,
      // A seeded root output predates the follower's origin: no rollback
      // reaches its merge.
      point: {
        slot: root.created?.slot ?? 0,
        outRef: root.outRef,
      } satisfies MergePoint,
      nodes,
    };
  });

/** A wait: retried on the driver's backoff. */
const hold = (reason: string, detail: string): DriverHold => ({
  reason,
  detail,
});

/**
 * A verdict on the view (an invalid block, an own journal that does not
 * describe its landed block): replaying it again at the same view gives the
 * same answer, so no timer re-runs it; the next follower change re-reads it.
 */
const verdict = (reason: string, detail: string): DriverHold =>
  notRetried(hold(reason, detail));

/** Why `node` cannot follow `parent`, if it cannot. */
const linkageFault = (node: LandedNode, parent: Parent): string | undefined =>
  node.header.prevHeaderHash !== parent.headerHash
    ? `block ${node.headerHash} names parent ${node.header.prevHeaderHash}, the queue's is ${parent.headerHash}`
    : node.header.prevUtxosRoot !== parent.utxosRoot
      ? `block ${node.headerHash} starts from root ${node.header.prevUtxosRoot}, its parent ends at ${parent.utxosRoot}`
      : node.header.startTime !== parent.endTime
        ? `block ${node.headerHash} starts at ${node.header.startTime.toString()}, its parent ends at ${parent.endTime.toString()}`
        : undefined;

const HOLD_OF = {
  missing: LANDED_BLOCK_AWAITING_DA,
  da_refetch_pending: LANDED_BLOCK_DA_REFETCH_PENDING,
  event_unknown: LANDED_BLOCK_EVENT_UNKNOWN,
  forced_order_pending: LANDED_BLOCK_FORCED_ORDER_PENDING,
  incomplete: LANDED_BLOCK_REPLAY_INCOMPLETE,
  invalid: LANDED_BLOCK_INVALID,
} as const;

/**
 * The hold a replay outcome other than `replayed` stops processing at. The
 * waits on DA peers and on the follower's facts are retried on the driver's
 * backoff; `invalid` (a verdict) and `incomplete` (a failure the replay
 * cannot classify, so not known to be transient) are `notRetried`.
 */
export const replayOutcomeHold = (
  kind: keyof typeof HOLD_OF,
  detail: string,
): DriverHold =>
  kind === "invalid" || kind === "incomplete"
    ? verdict(HOLD_OF[kind], detail)
    : hold(HOLD_OF[kind], detail);

type Step =
  | Readonly<{ kind: "row"; row: LandedBlockRow }>
  | Readonly<{ kind: "held"; hold: DriverHold }>;

/** The row a new node makes, or the hold that stops processing at it. */
const newRow = <R>(
  ports: LandedBlockPorts<R>,
  view: View,
  node: LandedNode,
  parent: Parent,
  ledger: LedgerMap,
) =>
  Effect.gen(function* () {
    const fault = linkageFault(node, parent);
    if (fault !== undefined)
      return {
        kind: "held",
        hold: verdict(LANDED_BLOCK_INVALID, fault),
      } satisfies Step;
    const base = {
      headerHash: node.headerHash,
      parentHeaderHash: parent.headerHash,
      parentUtxosRoot: parent.utxosRoot,
      utxosRoot: node.header.utxosRoot,
      state: "processed",
    } as const;
    const journal = yield* ports.ownJournal(node.headerHash);
    if (journal !== undefined) {
      // The block's header hash is this node's: a journal that does not
      // describe it, or whose delta does not reach its root, is a local
      // fault, never the block's.
      if (
        journal.baseTailHeaderHash !== parent.headerHash ||
        journal.baseUtxosRoot !== parent.utxosRoot ||
        journal.expectedUtxosRoot !== node.header.utxosRoot
      )
        return {
          kind: "held",
          hold: verdict(
            LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
            `own block ${node.headerHash}'s journal does not describe the landed block (base ${journal.baseTailHeaderHash}/${journal.baseUtxosRoot}, expected ${journal.expectedUtxosRoot})`,
          ),
        } satisfies Step;
      const root = yield* computeLedgerMpfRootFromLedgerEntries(
        ledgerEntries(applyDelta(new Map(ledger), journal)),
      );
      if (root !== node.header.utxosRoot)
        return {
          kind: "held",
          hold: verdict(
            LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
            `own block ${node.headerHash}'s journal delta reaches root ${root} on its parent's ledger, its header commits ${node.header.utxosRoot}`,
          ),
        } satisfies Step;
      // An abandoned own block that landed anyway is revived by the rebase:
      // its row waits unapplied, with its withdrawals, until the rebase took
      // it in.
      const abandoned = journal.status === "abandoned";
      return {
        kind: "row",
        row: {
          ...base,
          kind: "own",
          applied: !abandoned,
          spent: journal.spent,
          produced: journal.produced,
          depositIds: journal.depositIds,
          withdrawals: abandoned ? journal.withdrawals : [],
          forcedIds: journal.forcedIds,
          txIds: journal.txIds,
        },
      } satisfies Step;
    }
    const outcome = yield* Effect.either(
      ports.replay({
        headerHash: node.headerHash,
        header: node.header,
        parentHeaderHash: parent.headerHash,
        parentUtxosRoot: parent.utxosRoot,
        parentEntries: ledgerEntries(ledger),
        view,
      }),
    );
    if (outcome._tag === "Left")
      return {
        kind: "held",
        hold: failureHold(
          LANDED_BLOCK_REPLAY_FAILED,
          `block ${node.headerHash}: ${String(outcome.left)}`,
          outcome.left,
        ),
      } satisfies Step;
    const replayed = outcome.right;
    if (replayed.kind !== "replayed")
      return {
        kind: "held",
        hold: replayOutcomeHold(
          replayed.kind,
          `block ${node.headerHash}: ${replayed.detail}`,
        ),
      } satisfies Step;
    if (replayed.root !== node.header.utxosRoot)
      return {
        kind: "held",
        hold: verdict(
          LANDED_BLOCK_INVALID,
          `block ${node.headerHash} replays to root ${replayed.root}, its header commits ${node.header.utxosRoot}`,
        ),
      } satisfies Step;
    return {
      kind: "row",
      row: {
        ...base,
        kind: "foreign",
        applied: false,
        ...netDelta(ledger, replayed.entries),
        depositIds: replayed.depositIds,
        withdrawals: replayed.withdrawals,
        forcedIds: replayed.forcedIds,
        txIds: replayed.txIds,
      },
    } satisfies Step;
  });

/**
 * The hold before a node built on an own block that is revived and not yet
 * locally finalized here: the revived block stays the newest processed one
 * until the commit path finalized it.
 */
const revivalHold = <R>(ports: LandedBlockPorts<R>, parent: Parent) =>
  Effect.gen(function* () {
    const journal = yield* ports.ownJournal(parent.headerHash);
    if (
      journal === undefined ||
      (journal.status !== "abandoned" && !journal.revived)
    )
      return undefined;
    return hold(
      LANDED_BLOCK_OWN_REVIVAL_PENDING,
      `own block ${parent.headerHash} landed after its journal was abandoned; the blocks after it wait for its revival and local finalization`,
    );
  });

/** Whether the working ledger and native MPF lag the processed rows. */
export const rebaseNeeded = (rows: readonly LandedBlockRow[]): boolean =>
  rows.some(
    (row) =>
      row.state === "removed" || (row.state === "processed" && !row.applied),
  );

const run = <R>(
  ports: LandedBlockPorts<R>,
  queue: LandedStateQueue,
  options: ProcessOptions,
) =>
  Effect.gen(function* () {
    const decoded = yield* decodeQueue(queue);
    if (decoded === undefined) return undefined; // P1 holds an unhealthy queue
    const { root, nodes, point } = decoded;
    const holds: DriverHold[] = [];
    let read: QueueHistory | undefined;
    const history = Effect.suspend(() =>
      read === undefined
        ? ports
            .queueHistory(queue.view)
            .pipe(Effect.map((elements) => (read = decodeHistory(elements))))
        : Effect.succeed(read),
    );
    const booted = yield* bootstrapFrontier(ports, queue.view, root, history);
    if (booted !== undefined) return booted;
    const rewound = yield* rewindToRoot(ports, queue.view, root, history);
    if (rewound !== undefined) return rewound;
    const mergePoint = (headerHash: string) =>
      headerHash === root.headerHash
        ? Effect.succeed(point)
        : history.pipe(
            // The root passed it: the root's merge bounds its merge's slot.
            Effect.map((walked) => walked.roots.get(headerHash) ?? point),
          );
    const folded = yield* foldToRoot(ports, queue.view, root, mergePoint);
    if (folded.kind === "held") holds.push(folded.hold);
    // The root passed blocks this node never processed: its lineage back to
    // the last processed header on it, processed in order before the nodes.
    let missed:
      | Readonly<{
          anchor: string;
          endTime: bigint;
          headers: readonly LandedNode[];
        }>
      | undefined;
    if (folded.kind === "off_chain") {
      const before = yield* landedChain(yield* retrieveRows);
      const onChain = new Set([
        folded.frontier.headerHash,
        ...(before?.chain ?? []).map((row) => row.headerHash),
      ]);
      const walked = yield* history;
      const lineage = lineageBack(walked, root.headerHash, (hash) =>
        onChain.has(hash),
      );
      const endTime =
        lineage === undefined ? undefined : walked.endTimes.get(lineage.anchor);
      if (lineage === undefined || endTime === undefined)
        return combineHolds([
          ...holds,
          verdict(
            CONFIRMED_LEDGER_BEHIND,
            `the merged queue root ${root.headerHash} is not reachable forward from the confirmed-ledger frontier ${folded.frontier.headerHash} through the retained queue history`,
          ),
        ]);
      missed = { anchor: lineage.anchor, endTime, headers: lineage.headers };
    }
    const rows = yield* retrieveRows;
    const startHash = missed?.anchor ?? root.headerHash;
    const start = yield* landedStart(rows, startHash);
    if (start === undefined) return combineHolds(holds);
    const sequence =
      missed === undefined ? nodes : [...missed.headers, ...nodes];
    // The walk: rows kept, relanded, and where new processing starts.
    const walked = [...start.through];
    const visited = new Set(walked.map((row) => row.headerHash));
    const byHash = new Map(rows.map((row) => [row.headerHash, row] as const));
    const relands: LandedBlockRow[] = [];
    let parent: Parent = {
      headerHash: startHash,
      utxosRoot: start.root,
      endTime: missed?.endTime ?? root.endTime,
    };
    let next = 0;
    for (; next < sequence.length; next++) {
      const node = sequence[next]!;
      const row = byHash.get(node.headerHash);
      if (
        row === undefined ||
        row.parentHeaderHash !== parent.headerHash ||
        row.parentUtxosRoot !== parent.utxosRoot
      )
        break;
      visited.add(row.headerHash);
      if (row.state === "removed") relands.push(row);
      walked.push(row);
      parent = {
        headerHash: row.headerHash,
        utxosRoot: row.utxosRoot,
        endTime: node.header.endTime,
      };
    }
    const left = rows.filter(
      (row) => row.state === "processed" && !visited.has(row.headerHash),
    );
    if (left.length > 0 || relands.length > 0)
      yield* viewChecked(ports, queue.view, rollBackRows(left, relands));
    // Only new processing reads the ledger, so a run that only folds or
    // unfolds stays O(delta) in `confirmed_ledger`'s size.
    if (next < sequence.length) {
      const ledger = yield* ledgerAfter(walked);
      for (; next < sequence.length; next++) {
        const node = sequence[next]!;
        const revival = yield* revivalHold(ports, parent);
        if (revival !== undefined) {
          holds.push(revival);
          break;
        }
        const step = yield* newRow(ports, queue.view, node, parent, ledger);
        if (step.kind === "held") {
          holds.push(step.hold);
          break;
        }
        yield* viewChecked(ports, queue.view, processRow(step.row));
        applyDelta(ledger, step.row);
        parent = {
          headerHash: step.row.headerHash,
          utxosRoot: step.row.utxosRoot,
          endTime: node.header.endTime,
        };
      }
    }
    if (missed !== undefined) {
      // The rows just processed may fold already (an own merged block).
      const refolded = yield* foldToRoot(ports, queue.view, root, mergePoint);
      if (refolded.kind === "held") holds.push(refolded.hold);
    }
    yield* settleMerges(ports, queue.view, root, point, history, options);
    if (rebaseNeeded(yield* retrieveRows)) {
      const held = yield* ports.rebase(
        "Landed blocks changed what the working ledger must hold",
      );
      if (held !== undefined) holds.push(held);
    }
    return combineHolds(holds);
  });

/** A write the follower gate refused waits by its named reason; any other failure is named. */
const writeRefusedHold = (error: unknown) => {
  const refused = followerWriteHoldOf(error);
  return refused === undefined
    ? failureHold(LANDED_BLOCK_REPLAY_FAILED, String(error), error)
    : hold(LANDED_BLOCKS_WAITING, `${refused.reason}: ${refused.detail}`);
};

/**
 * Processes the landed queue `queue` (a healthy P1 read at the run's view).
 * Never fails: what it cannot do yet is the hold it returns.
 */
export const processLandedQueue = <R>(
  ports: LandedBlockPorts<R>,
  queue: LandedStateQueue,
  options: ProcessOptions = {},
): Effect.Effect<DriverHold | undefined, never, R | Database> =>
  run(ports, queue, options).pipe(
    Effect.catchAll((error) =>
      error instanceof ViewMoved
        ? Effect.succeed(hold(LANDED_BLOCKS_WAITING, "view moved"))
        : Effect.succeed(writeRefusedHold(error)),
    ),
  );
