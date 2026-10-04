import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import type { Checkpoint } from "../database/eventHistoryJournal.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { type EventHistorySourceBinding } from "../l1-event-history-source.js";
import { journalAbandonment } from "./canonical-journal-recovery.js";
import {
  journalBase,
  sameBaseJournals,
} from "./history-expired-intent-release.base-spend.js";
import {
  type QueueNode,
  type QueueView,
} from "./history-expired-intent-release.signed-commit-node.js";
import { C, canonicalDepth } from "./history-expired-intent-release.table.js";
import { loadStateQueueCorrectionObserverState } from "./state-queue-correction-rewind.js";

/** The retained canonical history's evidence at `checkpoint` (complete
 * transaction rosters from its anchor to the checkpoint head), read once: the
 * depth of every valid (inputs spending) transaction in it, and which of
 * `txHashes` it includes. Unavailable coverage is no evidence (no depth, none
 * included); any other failure (a database error, which aborts the owned
 * transaction) fails the attempt, which recovery retries. */
export const canonicalEvidence = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
  txHashes: readonly string[],
) =>
  canonicalDepth(binding, checkpoint).pipe(
    Effect.map((depth) => ({
      canonicalDepth: depth,
      canonicalHistory: new Set(
        txHashes.filter((hash) => depth?.of(hash) !== undefined),
      ) as ReadonlySet<string>,
    })),
  );

type ObserverView = Effect.Effect.Success<
  ReturnType<typeof loadStateQueueCorrectionObserverState>
>;

/** Authenticated evidence that this node's replaced block `record` landed:
 * its node on the exact-point queue (returned), the confirmed state equal to
 * its header, its signed commit in the journaled canonical history, or an
 * admitted (final) state-queue transition that merged it or saw it on the
 * queue. A block a recorded correction removed never counts: its members stay
 * reopened, which is what that correction's path does anyway. */
export const replacedBlockLanding = (
  record: Pending.Record,
  queue: QueueView,
  observer: ObserverView,
  canonicalHistory: ReadonlySet<string>,
): { onQueue: QueueNode | undefined; evidence: string } | undefined => {
  const header = record[C.HEADER_HASH].toString("hex");
  const pending = observer.kind === "observed" ? observer.state.pending : [];
  const admitted = observer.kind === "observed" ? observer.state.admitted : [];
  if (
    [...pending, ...admitted].some(
      (transition) =>
        transition.transitionKind !== "merge" &&
        transition.removedHeaderHashes.includes(header),
    )
  )
    return undefined;
  const onQueue = queue.nodes.find(
    (entry) => entry.headerHash === header && entry !== queue.root,
  );
  if (onQueue !== undefined)
    return { onQueue, evidence: "its node is on the queue" };
  const signed = record[C.INTENDED_TX_HASH]?.toString("hex");
  const seen = admitted.find(
    (transition) =>
      (transition.transitionKind === "merge" &&
        transition.removedHeaderHashes.includes(header)) ||
      [...transition.previousQueue, ...transition.nextQueue].some(
        (node) => node.headerHash === header,
      ),
  );
  const evidence =
    queue.root.headerHash === header
      ? "the confirmed state is its header"
      : signed !== undefined && canonicalHistory.has(signed)
        ? "its signed commit is in the journaled canonical history"
        : seen !== undefined
          ? `admitted state-queue transition ${seen.transactionHash} saw it on the queue`
          : undefined;
  return evidence === undefined ? undefined : { onQueue: undefined, evidence };
};

/** This node's journals built on the same base as `record` (the same base
 * output, or another incarnation of the same non-root base node, so their
 * commits and its commit are mutually exclusive) that were abandoned for
 * replacement. Sorted by header. */
export const replacedSiblings = (record: Pending.Record) =>
  Effect.gen(function* () {
    const rows = (yield* sameBaseJournals(journalBase(record), [
      record[C.HEADER_HASH],
    ]))
      .filter(({ status }) => status === Pending.Status.Abandoned)
      .sort((left, right) =>
        Buffer.compare(left.header_hash, right.header_hash),
      );
    const siblings: Pending.Record[] = [];
    for (const row of rows) {
      const found = yield* Pending.retrieveByHeaderHash(row.header_hash);
      if (
        Option.isSome(found) &&
        journalAbandonment(found.value) === "replacement"
      )
        siblings.push(found.value);
    }
    return siblings;
  });

/** L1 order of two recorded state-queue transitions. */
export const chainOrder = (
  left: SDK.StateQueueAuthenticatedTransition,
  right: SDK.StateQueueAuthenticatedTransition,
) => {
  const block = BigInt(left.blockNo) - BigInt(right.blockNo);
  const index =
    block === 0n
      ? BigInt(left.transactionIndex) - BigInt(right.transactionIndex)
      : block;
  return index === 0n ? 0 : index < 0n ? -1 : 1;
};
