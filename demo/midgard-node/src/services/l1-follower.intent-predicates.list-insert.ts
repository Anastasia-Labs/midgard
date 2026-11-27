/**
 * The node predicate of the list-insert family (`list_insert`): a
 * transaction that inserts into a protocol list from the node process, by
 * workflow-key verb. It is wanted while the follower's facts show the
 * insert's target unreached:
 *
 * - `list_insert:protocol_init:<nonce outref>`, the atomic protocol
 *   initialization, which creates every list's root: no live output at the
 *   state-queue address carries the state-queue policy (the landed queue's
 *   `policyOutputCount`, the test `GET /init` makes before it builds).
 *
 * A queue the follower cannot read throws: S6 keeps the intent live and
 * raises its transient hold.
 */
import { landedStateQueueIn } from "../l1-state-queue/landed.js";
import { keyRest, type Read } from "./l1-follower.intent-predicates.read.js";

export const listInsert = async (read: Read): Promise<boolean> => {
  const [verb] = keyRest(read.state, "list_insert:").split(":");
  switch (verb) {
    case "protocol_init": {
      const queue = await landedStateQueueIn(
        read.tx,
        read.dialect,
        read.deps.stateQueue,
        read.view,
      );
      if (queue.kind !== "ok")
        throw new Error(
          `the landed state queue is unreadable: ${queue.detail}`,
        );
      return queue.queue.policyOutputCount === 0;
    }
    default:
      throw new Error(
        `no list insert for intent key ${read.state.intent.workflowKey}`,
      );
  }
};
