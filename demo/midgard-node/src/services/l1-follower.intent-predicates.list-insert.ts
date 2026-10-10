/**
 * The node §8.4 predicate of the list-insert family (`list_insert`): a
 * transaction that inserts into a protocol list from the node process, by
 * workflow-key verb. Each is wanted while the follower's facts show the
 * insert's target unreached:
 *
 * - `list_insert:deposit:<event key hex>`, a deposit the node admits into the
 *   deposit list (the genesis deposit): its event key is not in the
 *   follower's never-reuse key set (`l1_event_keys`, kind `deposit`). A key
 *   there is listed, or was, and the event projection refuses it again.
 * - `list_insert:protocol_init:<nonce outref>`, the atomic protocol
 *   initialization, which creates every list's root: no live output at the
 *   state-queue address carries the state-queue policy (the landed queue's
 *   `policyOutputCount`, the test `GET /init` makes before it builds).
 *
 * A queue the follower cannot read throws: S6 keeps the intent live and
 * raises its transient hold.
 */
import { landedStateQueueIn } from "../l1-state-queue/index.js";
import {
  count,
  keyRest,
  type Read,
} from "./l1-follower.intent-predicates.read.js";

const HEX = /^[0-9a-f]+$/u;

export const listInsert = async (read: Read): Promise<boolean> => {
  const [verb, ...rest] = keyRest(read.state, "list_insert:").split(":");
  const target = rest.join(":");
  switch (verb) {
    case "deposit": {
      if (!HEX.test(target))
        throw new Error(
          `deposit list insert ${read.state.intent.workflowKey} names no event key`,
        );
      return (
        (await count(
          read.tx,
          "SELECT count(*) AS n FROM l1_event_keys WHERE kind = 'deposit' AND key = ?",
          [Buffer.from(target, "hex")],
        )) === 0
      );
    }
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
