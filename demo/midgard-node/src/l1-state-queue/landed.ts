/**
 * The node's landed state queue (plan §5.5 P1, N2): the live state-queue
 * outputs at a follower view, walked from the root to the tail with the
 * follower package's shared walk, and `{healthy, reason}`.
 *
 * - Only live outputs at the queue address that carry the queue policy are
 *   queue elements. What a third party pays to the address carries no queue
 *   token and never enters the queue.
 * - A queue output that is not a root or a V1 node (a malformed NFT or
 *   datum), an orphan, a duplicate key, a cycle, a broken link, more than one
 *   root or more live nodes than the node cap make the queue unhealthy with
 *   that named reason. The walk so far is still served: unhealthy stops
 *   proposals, not reads.
 * - It is a pure function of the follower facts at the view, so it equals a
 *   fresh replay whenever the facts do, and a rewind recomputes it.
 */
import {
  currentViewIn,
  type Dialect,
  type LinkedQueueEntry,
  type LinkedQueueUnhealthyReason,
  liveUtxosIn,
  type SqlTx,
  type StoredOutput,
  type View,
  viewValidIn,
  walkLinkedQueue,
} from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";

import type { StateQueueProjectionConfig } from "./config.js";

/** One element of the landed queue: the root or a block node. */
export type LandedStateQueueElement = Readonly<{
  /** `<tx hash hex>#<index>`. */
  outRef: string;
  /** The element as the SDK's builders take it. */
  element: SDK.StateQueueUTxO;
  /** A node: its header hash. The root: the confirmed header hash. */
  headerHash: string;
  /** A node: its header's end time. The root: the confirmed state's. */
  endTimeMs: bigint;
  /** A node's DA status identity; null for the root. */
  daStatus: string | null;
  /** A node's key and asset-name mismatches (it is still walked). */
  problems: readonly SDK.StateQueueOutputProblem[];
  /** Where the output was created (null for a seeded row). */
  created: Readonly<{ slot: number; txIndex: number }> | null;
}>;

export type LandedStateQueue = Readonly<{
  /** The follower view the queue is read at. */
  view: View;
  healthy: boolean;
  /** Null when healthy. */
  reason: LinkedQueueUnhealthyReason | null;
  detail: string | null;
  root: LandedStateQueueElement | null;
  /** The walk from the root, in list order (cut where an unhealthy walk failed). */
  nodes: readonly LandedStateQueueElement[];
  /** Live queue outputs the walk did not reach, by outref. */
  strays: readonly string[];
  /** Live outputs at the address carrying the queue policy. */
  policyOutputCount: number;
}>;

/** Why the landed queue cannot be read at a view. */
export type LandedStateQueueRefusal = Readonly<{
  kind:
    | "not_initialized"
    | "point_not_canonical"
    | "point_beyond_retention"
    | "view_moved";
  detail: string;
}>;

export type LandedStateQueueRead =
  | Readonly<{ kind: "ok"; queue: LandedStateQueue }>
  | LandedStateQueueRefusal;

type Entry = LinkedQueueEntry &
  Readonly<{ landed: LandedStateQueueElement | null }>;

const outRefText = (stored: StoredOutput): string =>
  `${stored.outRef.txHash.toString("hex")}#${stored.outRef.index.toString()}`;

/** One live output at the queue address as a walk entry, or null if it is not a queue output. */
const entryOf = (
  stored: StoredOutput,
  config: StateQueueProjectionConfig,
): Entry | null => {
  const decoded = SDK.decodeStateQueueOutput(stored.output, config.policyId);
  if (decoded === null) return null;
  const id = outRefText(stored);
  if (
    decoded.kind === "invalid" ||
    decoded.datum === null ||
    decoded.assetName === null
  )
    return { id, kind: "invalid", key: null, next: null, landed: null };
  const landed: LandedStateQueueElement = {
    outRef: id,
    element: {
      utxo: toLucidUtxo(stored.outRef, stored.output),
      datum: SDK.linkedListDatumToNodeView(decoded.datum, decoded.assetName),
      assetName: decoded.assetName,
    },
    headerHash: decoded.headerHash ?? "",
    endTimeMs: decoded.endTimeMs ?? 0n,
    daStatus: decoded.daStatus,
    problems: decoded.problems,
    created: stored.created,
  };
  return {
    id,
    kind: decoded.kind,
    key: decoded.nodeKey,
    next: decoded.nextKey,
    landed,
  };
};

/** The landed queue from the live outputs at `view`. A pure function. */
export const walkLandedStateQueue = (
  view: View,
  live: readonly StoredOutput[],
  config: StateQueueProjectionConfig,
): LandedStateQueue => {
  const address = Buffer.from(config.address, "hex");
  const entries = live
    .filter((stored) => stored.output.address.equals(address))
    .flatMap((stored) => {
      const entry = entryOf(stored, config);
      return entry === null ? [] : [entry];
    });
  const walk = walkLinkedQueue(entries, { maxNodes: config.maxNodes });
  return {
    view,
    healthy: walk.healthy,
    reason: walk.reason,
    detail: walk.detail,
    root: walk.root?.landed ?? null,
    nodes: walk.nodes.flatMap((entry) =>
      entry.landed === null ? [] : [entry.landed],
    ),
    strays: walk.strays,
    policyOutputCount: entries.length,
  };
};

/**
 * The landed queue at `at` (default: the follower's current view), read in
 * the caller's transaction. A view the follower rewound past while the read
 * ran is `view_moved`: nothing mixes two chain states.
 */
export const landedStateQueueIn = async (
  tx: SqlTx,
  dialect: Dialect,
  config: StateQueueProjectionConfig,
  at?: View,
): Promise<LandedStateQueueRead> => {
  const view = at ?? (await currentViewIn(tx, dialect));
  if (view === null)
    return { kind: "not_initialized", detail: "the follower has no view yet" };
  const read = await liveUtxosIn(
    tx,
    dialect,
    { by: "unit", policyId: Buffer.from(config.policyId, "hex") },
    view.point,
  );
  if (read.kind !== "ok") return read;
  if (!(await viewValidIn(tx, dialect, view)))
    return {
      kind: "view_moved",
      detail: `the follower moved off generation ${view.generation.toString()} at slot ${view.point.slot.toString()}`,
    };
  return {
    kind: "ok",
    queue: walkLandedStateQueue(view, read.utxos, config),
  };
};

/** The elements root first, in list order (the root is absent only when unhealthy). */
export const landedElements = (
  queue: LandedStateQueue,
): readonly LandedStateQueueElement[] =>
  queue.root === null ? queue.nodes : [queue.root, ...queue.nodes];

/** The tail: the last node, or the root of an empty queue. */
export const landedTail = (
  queue: LandedStateQueue,
): LandedStateQueueElement | null => queue.nodes.at(-1) ?? queue.root;

/** A one-line health summary for logs and refusals. */
export const formatLandedStateQueue = (queue: LandedStateQueue): string =>
  [
    `view=${queue.view.generation.toString()}:${queue.view.point.slot.toString()}`,
    `policy_outputs=${queue.policyOutputCount.toString()}`,
    `nodes=${queue.nodes.length.toString()}`,
    `healthy=${String(queue.healthy)}`,
    ...(queue.reason === null
      ? []
      : [`reason=${queue.reason}`, `detail=${queue.detail ?? ""}`]),
  ].join(",");
