/**
 * What the operator watchdog reads of the landed state queue so it never
 * strikes progress (B7): when the tail was committed, and when the queue
 * stops accepting appends. The strike validator checks neither; both are
 * this node's policy.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either } from "effect";

import type { LandedStateQueue } from "../l1-state-queue/index.js";
import { landedTail } from "../l1-state-queue/index.js";

export type StateQueueStrikeFacts = Readonly<{
  /** The tail node's `block_time`; null when the tail is the root. */
  tailBlockTimeMs: bigint | null;
  /**
   * The earliest time from which no append can land, or null when nothing in
   * the queue closes it. The head fails the state queue's append fence
   * (`state_queue_head_allows_append_v1`) for good once it carries a
   * completed fraud proof or is Challenged (0), and from `block_time +
   * da_attestation_timeout` while Unattested. Any other node still
   * Unattested past that deadline also stops an honest commit until the
   * timeout correction removes it (the commit worker's expired-suffix
   * pause).
   */
  appendFenceClosesAtMs: bigint | null;
}>;

const minOf = (left: bigint | null, right: bigint): bigint =>
  left === null || right < left ? right : left;

/** The strike facts of a healthy landed queue; null for an unhealthy one. */
export const stateQueueStrikeFactsOf = (
  queue: LandedStateQueue,
): StateQueueStrikeFacts | null => {
  if (!queue.healthy) return null;
  const tail = landedTail(queue);
  if (tail === null) return null;
  let closesAt: bigint | null = null;
  const head = queue.nodes[0];
  if (head !== undefined) {
    const node = Effect.runSync(
      Effect.either(
        SDK.getStateQueueNodeFromStateQueueDatum(head.element.datum),
      ),
    );
    // An undecodable head accepts no append.
    if (Either.isLeft(node)) closesAt = 0n;
    else if (
      node.right.proven_fraud !== null ||
      SDK.daAvailabilityStateQueueStatusKind(node.right.da_attestation) ===
        "Challenged"
    )
      closesAt = 0n;
  }
  for (const node of queue.nodes)
    if (node.daStatus === "Unattested" && node.blockTimeMs !== null)
      closesAt = minOf(
        closesAt,
        node.blockTimeMs + SDK.DA_ATTESTATION_TIMEOUT_MS,
      );
  return {
    tailBlockTimeMs: tail === queue.root ? null : tail.blockTimeMs,
    appendFenceClosesAtMs: closesAt,
  };
};
