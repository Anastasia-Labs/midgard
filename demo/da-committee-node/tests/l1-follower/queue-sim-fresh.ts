import { isDeepStrictEqual } from "node:util";

import type { CommitteeView } from "../../src/l1/follower/projection.js";
import type { ExpectedQueue } from "./queue-sim-oracle.js";

/**
 * The obligation states a pruned store may read where a fresh replay reads
 * another (store state, fresh state), each with the reason; every one is
 * conservative or terminal for the same reason as the fresh read.
 */
const prunedPairs: ReadonlyMap<string, string> = new Map([
  // The earliest output carrying the header was pruned, so the earliest
  // one left is younger: still committed, read less deep. Keeps the record.
  ["landed/final", "earliest commit output pruned"],
  // Every output carrying the header was pruned after it merged, and a
  // final block is past its end time: terminal, and nothing reads it as a
  // proof that it could not land.
  ["beyond_retention/final", "merged header's outputs all pruned"],
  ["beyond_retention/cannot_land", "absent header past retention"],
  // The block the fresh replay reads as the latest final one was pruned
  // (after a rollback), so an older one stands in: an earlier time, read as
  // not yet terminal. Keeps the record and the decision.
  ["pending/cannot_land", "latest final block pruned; an older one read"],
  // Every output carrying a permanent commit (final, or at or below the
  // pruned-through slot) was pruned, and no final block read is past its
  // end time yet. Keeps the record and the decision.
  ["pending/final", "final commit's outputs all pruned, end not yet passed"],
  ["pending/landed", "permanent commit's outputs all pruned, end not passed"],
]);

const describeValue = (value: unknown): string =>
  JSON.stringify(value, (_, inner: unknown) =>
    inner instanceof Map ? [...(inner as Map<unknown, unknown>)] : inner,
  );

/**
 * Over a pruned store, the queue at the latest final block is read from the
 * rows pruning kept: an output spent at or below `prunedThroughSlot` is
 * gone, and the latest final block itself may be gone (an older retained one
 * stands in). Every header the fresh replay pins and the store does not must
 * have left the queue at or below `prunedThroughSlot`: the follower refuses
 * every rollback below it, so that exit is permanent.
 */
const prunedFinalQueueFailure = (
  view: CommitteeView,
  fresh: CommitteeView,
  expected: ExpectedQueue,
): string | null => {
  if (
    view.finalSlot !== fresh.finalSlot &&
    (view.finalSlot === null ||
      fresh.finalSlot === null ||
      view.finalSlot > fresh.finalSlot)
  )
    return `pruned final block at ${String(view.finalSlot)}, fresh ${String(fresh.finalSlot)}`;
  const pinned = new Set(view.finalQueueHeaderHashes);
  const freshPinned = new Set(fresh.finalQueueHeaderHashes);
  for (const headerHash of pinned)
    if (!freshPinned.has(headerHash))
      return `pruned final queue pins ${headerHash}, a fresh replay does not`;
  const live = new Set(view.queue.nodes.map((node) => node.headerHash));
  for (const headerHash of freshPinned) {
    if (pinned.has(headerHash) || live.has(headerHash)) continue;
    // The outputs that made the fresh replay pin it: live at its final block.
    const reversible = expected.rows.filter(
      (row) =>
        row.headerHash === headerHash &&
        row.createdSlot <= (fresh.finalSlot ?? -1) &&
        (row.spentSlot === null || row.spentSlot > view.prunedThroughSlot),
    );
    if (reversible.length > 0)
      return `pruned final queue drops ${headerHash}, whose output ${describeValue(reversible[0])} is live past ${view.prunedThroughSlot.toString()} (final ${String(view.finalSlot)}, fresh ${String(fresh.finalSlot)})`;
  }
  return null;
};

export const compareWithFreshReplay = (
  view: CommitteeView,
  fresh: CommitteeView,
  pruned: boolean,
  expected: ExpectedQueue,
): string | null => {
  if (view.at.slot !== fresh.at.slot || view.at.height !== fresh.at.height)
    return `view at ${view.at.slot.toString()}/${view.at.height.toString()}, fresh ${fresh.at.slot.toString()}/${fresh.at.height.toString()}`;
  const parts: Readonly<Record<string, [unknown, unknown]>> = {
    queue: [view.queue, fresh.queue],
    awaiting: [view.awaiting, fresh.awaiting],
    exits: [view.exits, fresh.exits],
    nodeBlocks: [view.nodeBlocks, fresh.nodeBlocks],
    ...(pruned
      ? {}
      : {
          finalQueueHeaderHashes: [
            view.finalQueueHeaderHashes,
            fresh.finalQueueHeaderHashes,
          ],
          finalSlot: [view.finalSlot, fresh.finalSlot],
          obligations: [view.obligations, fresh.obligations],
          prunedThroughSlot: [view.prunedThroughSlot, fresh.prunedThroughSlot],
        }),
  };
  for (const [name, [mine, theirs]] of Object.entries(parts))
    if (!isDeepStrictEqual(mine, theirs))
      return `view ${name} differs from a fresh replay (pruned ${String(pruned)}): ${describeValue(mine)} vs ${describeValue(theirs)}`;
  if (!pruned) return null;
  const finalQueue = prunedFinalQueueFailure(view, fresh, expected);
  if (finalQueue !== null) return finalQueue;
  const freshByHash = new Map(
    fresh.obligations.map((obligation) => [obligation.headerHash, obligation]),
  );
  for (const obligation of view.obligations) {
    const other = freshByHash.get(obligation.headerHash);
    if (other === undefined)
      return `obligation ${obligation.headerHash} missing from a fresh replay`;
    if (obligation.state === other.state) {
      // The earliest output carrying a committed header may be pruned: the
      // earliest one left is younger, so it reads less deep.
      const shallower =
        obligation.depth !== null &&
        other.depth !== null &&
        obligation.depth <= other.depth;
      if (
        !isDeepStrictEqual(
          { ...obligation, depth: null, level: null },
          { ...other, depth: null, level: null },
        ) ||
        (obligation.depth !== other.depth && !shallower)
      )
        return `obligation ${obligation.headerHash} ${obligation.state} differs from a fresh replay: ${describeValue(obligation)} vs ${describeValue(other)}`;
      continue;
    }
    if (!prunedPairs.has(`${obligation.state}/${other.state}`))
      return `pruned obligation ${obligation.headerHash} ${obligation.state}, fresh ${other.state}`;
  }
  return view.obligations.length === fresh.obligations.length
    ? null
    : "obligation count differs from a fresh replay";
};
