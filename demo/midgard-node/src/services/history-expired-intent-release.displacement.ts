import type * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { SignedIntentReplacementIntegrityError } from "./canonical-journal-recovery.js";
import type {
  QueueNode,
  QueueView,
} from "./history-expired-intent-release.signed-commit-node.js";
import {
  C,
  type CanonicalDepth,
} from "./history-expired-intent-release.table.js";
import { LOCALLY_FINALIZED_STATUSES } from "./state-queue-correction-recovery.js";
import { nonAbandonedChildren } from "./state-queue-correction-rewind.admitted-removals.js";
import type { loadStateQueueCorrectionObserverState } from "./state-queue-correction-rewind.js";

export type ObserverView = Effect.Effect.Success<
  ReturnType<typeof loadStateQueueCorrectionObserverState>
>;

/** The correction observer's recorded removal of `hash` of one kind (a merge
 * or not), an admitted one first. */
export const observedRemoval = (
  observer: ObserverView,
  hash: string,
  merge: boolean,
) => {
  if (observer.kind !== "observed") return undefined;
  const matches = (transition: SDK.StateQueueAuthenticatedTransition) =>
    (transition.transitionKind === "merge") === merge &&
    transition.removedHeaderHashes.includes(hash);
  const admitted = observer.state.admitted.find(matches);
  if (admitted !== undefined) return { transition: admitted, admitted: true };
  const pending = observer.state.pending.find(matches);
  return pending === undefined
    ? undefined
    : { transition: pending, admitted: false };
};

/** The locally finalized blocks `blocking` (siblings on the base `base` that
 * this node's replaced block `winner` holds the slot of) and their
 * descendants, earliest first, once L1 evidence alone shows an L1 rollback
 * displaced them: `winner`'s commit holds the base's slot at the confirmation
 * depth `required`, so none of them is on the chain or can return without a
 * deeper rollback. Each must preserve its parent's root or belong to one
 * contiguous retained native replay chain, be absent from the queue and the
 * canonical history, and have no
 * recorded removal. Otherwise why not, and the caller waits; a displaced
 * block shown landed beside the winner is the integrity failure of two
 * landed siblings. Read-only. */
export const displacement = (input: {
  readonly blocking: readonly string[];
  /** The winner's node, and whether it is on the exact-point queue. */
  readonly node: QueueNode;
  readonly queued: boolean;
  readonly winner: Pending.Record;
  /** The base's header and ledger root. */
  readonly base: string;
  readonly baseRoot: string;
  readonly queue: QueueView;
  readonly observer: ObserverView;
  readonly depth: CanonicalDepth;
  readonly required: bigint;
}) =>
  Effect.gen(function* () {
    const { node, queued, winner, base, depth, required } = input;
    // A node output on the exact-point queue was created by a transaction
    // canonical at the checkpoint, at or after the winner's commit; absent
    // from the complete retained chain, it lies deeper than all of it.
    const held =
      depth.of(node.node.utxo.txHash) ??
      (queued ? depth.retained + 1n : undefined);
    if (held === undefined || held < required)
      return `the commit holding the slot is ${held === undefined ? "not in the journaled canonical history" : `${held.toString()} blocks deep`}, short of the confirmation depth ${required.toString()}`;
    const landedBeside = (header: string, how: string) =>
      Effect.fail(
        new SignedIntentReplacementIntegrityError(
          header,
          `it ${how} while block ${winner[C.HEADER_HASH].toString("hex")} of this node holds the slot of its base ${base} at depth ${held.toString()}`,
        ),
      );
    const displaced: Pending.Record[] = [];
    const roots = new Map([[base, input.baseRoot]]);
    let movingTail = base;
    const pending = [...input.blocking];
    const seen = new Set<string>();
    for (
      let next = pending.shift();
      next !== undefined;
      next = pending.shift()
    ) {
      if (seen.has(next)) continue;
      seen.add(next);
      const found = yield* Pending.retrieveByHeaderHash(
        Buffer.from(next, "hex"),
      );
      if (Option.isNone(found)) return `block ${next} has no journal`;
      const block = found.value;
      const status = block[C.STATUS];
      if (!LOCALLY_FINALIZED_STATUSES.includes(status))
        return `block ${next} built over it has journal status ${status}, not locally finalized`;
      // A root-preserving descendant still has to extend its retained parent.
      // Root-moving blocks are recoverable only along one native replay chain:
      // the recovery CAS restores its retained base before SQL reopens members.
      const parent = block[C.BASE_TAIL_HEADER_HASH].toString("hex");
      const moving = block[C.EXPECTED_UTXOS_ROOT] !== block[C.BASE_UTXOS_ROOT];
      const replay = block.nativeMpfReplay;
      if (
        roots.get(parent) !== block[C.BASE_UTXOS_ROOT] ||
        (moving &&
          (parent !== movingTail ||
            block[C.DEPLOYMENT_MANIFEST_ID] !==
              winner[C.DEPLOYMENT_MANIFEST_ID] ||
            replay === undefined ||
            replay.baseRoot.toString("hex") !== block[C.BASE_UTXOS_ROOT] ||
            replay.candidateRoot.toString("hex") !==
              block[C.EXPECTED_UTXOS_ROOT]))
      )
        return yield* landedBeside(
          next,
          `has no contiguous retained replay from its parent's root ${roots.get(parent) ?? "(missing)"} through ${block[C.BASE_UTXOS_ROOT]} to ${block[C.EXPECTED_UTXOS_ROOT]},`,
        );
      if (input.queue.nodes.some((entry) => entry.headerHash === next))
        return yield* landedBeside(next, "is on the queue");
      const merge = observedRemoval(input.observer, next, true);
      if (merge?.admitted === true)
        return yield* landedBeside(next, "was merged");
      if (merge !== undefined)
        return `pending state-queue merge ${merge.transition.transactionHash} names block ${next}; it is not admitted yet`;
      const signed = block[C.INTENDED_TX_HASH]?.toString("hex");
      const signedDepth = signed === undefined ? undefined : depth.of(signed);
      if (signedDepth !== undefined)
        return yield* landedBeside(
          next,
          `has its signed commit ${signedDepth.toString()} blocks deep in the canonical history`,
        );
      const correction = observedRemoval(input.observer, next, false);
      if (correction !== undefined)
        return `${correction.admitted ? "admitted" : "pending"} state-queue correction ${correction.transition.transactionHash} removed block ${next}, which the correction path reconciles`;
      displaced.push(block);
      roots.set(next, block[C.EXPECTED_UTXOS_ROOT]);
      // An empty block in the same chain may precede the next root-moving one.
      if (parent === movingTail) movingTail = next;
      pending.push(...(yield* nonAbandonedChildren(next)));
    }
    return displaced;
  });
