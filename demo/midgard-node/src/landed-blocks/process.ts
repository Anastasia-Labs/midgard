/**
 * In-order landed-block processing (plan §7.3, §5.5 P3, N3). On every
 * driver run, at the follower view the run applies:
 *
 * 1. `confirmed_ledger` is folded up to the merged queue root (`fold.ts`).
 * 2. The landed queue is walked from the root. A node already processed on
 *    the same parent is kept (a `removed` one relands); every processed row
 *    the walk did not reach left the queue: a foreign row the working ledger
 *    took in becomes `removed` (the rebase reverts it), any other is deleted.
 *    Rewind is that walk plus the rebase's recompute, never an inverse.
 * 3. Each new node, in queue order and exactly once (the row's primary key),
 *    must link to its parent (hash, root, start time). This node's own block
 *    is adopted from its journal and never replayed; a foreign block is
 *    replayed on its parent's ledger and must reach its header's root. A
 *    block that does not is never recorded: it holds `landed_block_invalid`
 *    and nothing after it is processed. A mismatch never feeds a fault-proof
 *    path.
 * 4. A foreign row the working ledger does not hold yet, or a removed row,
 *    asks the history owner for the rebase and holds
 *    `landed_block_rebase_pending` until it ran.
 *
 * Every write re-checks the follower view in its transaction; a view that
 * moved ends the run quietly (the move triggers the next one). Every hold
 * keeps the process up and clears on a later run.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, Effect } from "effect";

import type { DriverHold } from "../l1-events/driver.js";
import type { LandedStateQueue } from "../l1-state-queue/index.js";
import type { Database } from "../services/database.js";
import { isHistoryProducerGateClosed } from "../services/event-history-producer.js";
import { bootstrapFrontier, foldToRoot, reanchorFrontier } from "./fold.js";
import {
  combineHolds,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_ABANDONED,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCKS_WAITING,
} from "./holds.js";
import {
  applyDelta,
  landedLedger,
  ledgerAt,
  ledgerEntries,
  type LedgerMap,
  netDelta,
} from "./ledger.js";
import type { LandedBlockPorts } from "./ports.js";
import {
  deleteRows,
  insertRow,
  type LandedBlockRow,
  retrieveRows,
  setState,
} from "./store.js";

/** The follower moved off the view during a write. */
class ViewMoved extends Data.TaggedError("ViewMoved")<Record<string, never>> {}

type Parent = Readonly<{
  headerHash: string;
  utxosRoot: string;
  endTime: bigint;
}>;

type LandedNode = Readonly<{ headerHash: string; header: SDK.Header }>;

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
      nodes,
    };
  });

const viewChecked = <R, A, E>(
  ports: LandedBlockPorts<R>,
  view: View,
  work: Effect.Effect<A, E, R | Database>,
) =>
  ports.write(
    Effect.gen(function* () {
      if (!(yield* ports.confirmView(view)))
        return yield* Effect.fail(new ViewMoved({}));
      return yield* work;
    }),
  );

const hold = (reason: string, detail: string): DriverHold => ({
  reason,
  detail,
});

/** Why `node` cannot follow `parent`, if it cannot. */
const linkageFault = (node: LandedNode, parent: Parent): string | undefined =>
  node.header.prevHeaderHash !== parent.headerHash
    ? `block ${node.headerHash} names parent ${node.header.prevHeaderHash}, the queue's is ${parent.headerHash}`
    : node.header.prevUtxosRoot !== parent.utxosRoot
      ? `block ${node.headerHash} starts from root ${node.header.prevUtxosRoot}, its parent ends at ${parent.utxosRoot}`
      : node.header.startTime !== parent.endTime
        ? `block ${node.headerHash} starts at ${node.header.startTime.toString()}, its parent ends at ${parent.endTime.toString()}`
        : undefined;

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
        hold: hold(LANDED_BLOCK_INVALID, fault),
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
      if (journal.status === "abandoned")
        return {
          kind: "held",
          hold: hold(
            LANDED_BLOCK_OWN_JOURNAL_ABANDONED,
            `own block ${node.headerHash} landed but its journal is abandoned`,
          ),
        } satisfies Step;
      if (
        journal.baseTailHeaderHash !== parent.headerHash ||
        journal.baseUtxosRoot !== parent.utxosRoot ||
        journal.expectedUtxosRoot !== node.header.utxosRoot
      )
        return {
          kind: "held",
          hold: hold(
            LANDED_BLOCK_INVALID,
            `own block ${node.headerHash}'s journal does not describe the landed block (base ${journal.baseTailHeaderHash}/${journal.baseUtxosRoot}, expected ${journal.expectedUtxosRoot})`,
          ),
        } satisfies Step;
      return {
        kind: "row",
        row: {
          ...base,
          kind: "own",
          applied: true,
          spent: journal.spent,
          produced: journal.produced,
          depositIds: journal.depositIds,
          withdrawals: [],
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
        hold: hold(
          LANDED_BLOCK_REPLAY_FAILED,
          `block ${node.headerHash}: ${String(outcome.left)}`,
        ),
      } satisfies Step;
    const replayed = outcome.right;
    if (replayed.kind !== "replayed")
      return {
        kind: "held",
        hold: hold(
          replayed.kind === "missing"
            ? LANDED_BLOCK_AWAITING_DA
            : LANDED_BLOCK_INVALID,
          `block ${node.headerHash}: ${replayed.detail}`,
        ),
      } satisfies Step;
    if (replayed.root !== node.header.utxosRoot)
      return {
        kind: "held",
        hold: hold(
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

/** Whether the working ledger and native MPF lag the processed rows. */
export const rebaseNeeded = (rows: readonly LandedBlockRow[]): boolean =>
  rows.some(
    (row) =>
      row.state === "removed" ||
      (row.kind === "foreign" && row.state === "processed" && !row.applied),
  );

const run = <R>(ports: LandedBlockPorts<R>, queue: LandedStateQueue) =>
  Effect.gen(function* () {
    const decoded = yield* decodeQueue(queue);
    if (decoded === undefined) return undefined; // P1 holds an unhealthy queue
    const { root, nodes } = decoded;
    const holds: DriverHold[] = [];
    const booted = yield* bootstrapFrontier(ports, root);
    if (booted !== undefined) return booted;
    const folded = yield* foldToRoot(ports, root);
    if (folded.kind === "held") holds.push(folded.hold);
    if (folded.kind === "off_chain") {
      const anchored = yield* reanchorFrontier(ports, folded.frontier, root);
      if (anchored !== undefined) return combineHolds([...holds, anchored]);
    }
    const rows = yield* retrieveRows;
    const landed = yield* landedLedger(rows);
    const anchor =
      landed === undefined ? undefined : ledgerAt(landed, root.headerHash);
    if (landed === undefined || anchor === undefined)
      return combineHolds(holds);
    // The walk: rows kept, relanded, and where new processing starts.
    const visited = new Set(
      landed.chain
        .slice(
          0,
          landed.chain.findIndex((row) => row.headerHash === root.headerHash) +
            1,
        )
        .map((row) => row.headerHash),
    );
    const byHash = new Map(rows.map((row) => [row.headerHash, row] as const));
    const ledger = anchor.ledger;
    const relands: string[] = [];
    let parent: Parent = root;
    let next = 0;
    for (; next < nodes.length; next++) {
      const node = nodes[next]!;
      const row = byHash.get(node.headerHash);
      if (
        row === undefined ||
        row.parentHeaderHash !== parent.headerHash ||
        row.parentUtxosRoot !== parent.utxosRoot
      )
        break;
      visited.add(row.headerHash);
      if (row.state === "removed") relands.push(row.headerHash);
      applyDelta(ledger, row);
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
      yield* viewChecked(
        ports,
        queue.view,
        Effect.gen(function* () {
          yield* setState(
            left
              .filter((row) => row.kind === "foreign" && row.applied)
              .map((row) => row.headerHash),
            "removed",
          );
          yield* deleteRows(
            left
              .filter((row) => row.kind === "own" || !row.applied)
              .map((row) => row.headerHash),
          );
          yield* setState(relands, "processed");
        }),
      );
    for (; next < nodes.length; next++) {
      const node = nodes[next]!;
      const step = yield* newRow(ports, queue.view, node, parent, ledger);
      if (step.kind === "held") {
        holds.push(step.hold);
        break;
      }
      yield* viewChecked(ports, queue.view, insertRow(step.row));
      applyDelta(ledger, step.row);
      parent = {
        headerHash: step.row.headerHash,
        utxosRoot: step.row.utxosRoot,
        endTime: node.header.endTime,
      };
    }
    if (rebaseNeeded(yield* retrieveRows)) {
      const blocked = yield* ports.requestRebase(
        "Landed blocks changed what the working ledger must hold",
      );
      holds.push(
        hold(
          LANDED_BLOCK_REBASE_PENDING,
          blocked ??
            "the working ledger and native MPF wait for the rebase onto the processed landed blocks",
        ),
      );
    }
    return combineHolds(holds);
  });

/**
 * Processes the landed queue `queue` (a healthy P1 read at the run's view).
 * Never fails: what it cannot do yet is the hold it returns.
 */
export const processLandedQueue = <R>(
  ports: LandedBlockPorts<R>,
  queue: LandedStateQueue,
): Effect.Effect<DriverHold | undefined, never, R | Database> =>
  run(ports, queue).pipe(
    Effect.catchAll((error) =>
      Effect.succeed(
        error instanceof ViewMoved
          ? undefined
          : isHistoryProducerGateClosed(error)
            ? hold(LANDED_BLOCKS_WAITING, "the history owner is recovering")
            : hold(LANDED_BLOCK_REPLAY_FAILED, String(error)),
      ),
    ),
  );
