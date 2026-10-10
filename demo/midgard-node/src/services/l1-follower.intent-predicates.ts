/**
 * The node's §8.4 family predicates (I1): whether a live journaled intent
 * can still land and is still wanted, read over the follower's projections
 * in one store transaction at the follower's current view. No L1 read.
 *
 * - `false` abandons the intent (S6 writes it only at the tip its status
 *   was derived at). A workflow that still wants the effect builds a new
 *   transaction, so a predicate errs towards `false` only on a fact that
 *   rules the transition out.
 * - A projection that cannot be read (no view, an unhealthy queue or
 *   operator set, a missing input to the predicate) throws: S6 keeps the
 *   intent live and raises a named transient `/readyz` hold.
 * - A predicate that a later view will decide throws `IntentPredicateWait`
 *   with its own named hold (`INTENT_COMMIT_ANCHOR_NOT_DEEP`).
 * - Every node family has a predicate (`PREDICATES`); the payout,
 *   reference-script and stake-registration families are in
 *   `l1-follower.intent-predicates.wallet.ts`, the list inserts in
 *   `l1-follower.intent-predicates.list-insert.ts`.
 */
import { currentViewIn, type IntentState } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { commitAnchorCanonicalText } from "../database/commit-anchor.js";
import {
  createOperatorSetMirror,
  type OperatorSet,
  type OperatorSetMirror,
} from "../l1-operator-set/index.js";
import {
  type LandedStateQueue,
  landedStateQueueIn,
  landedTail,
} from "../l1-state-queue/index.js";
import type { NodeIntentFamily } from "./intent-journal.js";
import { listInsert } from "./l1-follower.intent-predicates.list-insert.js";
import {
  contentRefHex,
  keyRest,
  type NodeFamilyPredicateDeps,
  type Read,
  spends,
  validFromMs,
} from "./l1-follower.intent-predicates.read.js";
import {
  payout,
  referenceFunding,
  referencePublication,
  referenceSweep,
  stakeRegistration,
} from "./l1-follower.intent-predicates.wallet.js";
import { IntentPredicateWait } from "./l1-follower.intents.js";

export type { NodeFamilyPredicateDeps } from "./l1-follower.intent-predicates.read.js";

const queueNode = (queue: LandedStateQueue, headerHash: string) =>
  queue.nodes.find((node) => node.headerHash === headerHash);

/**
 * A commit's anchor is canonical but the follower tip is fewer than d blocks
 * above it (a rewind shortened the chain above it): the commit waits for the
 * chain to regrow.
 */
export const INTENT_COMMIT_ANCHOR_NOT_DEEP = "intent_commit_anchor_not_deep";

/**
 * Whether the commit's anchor allows it at this view (plan §8.1): every
 * included event's block is below the anchor (`database/commit-anchor.ts`).
 *
 * - `false` when the journal has no anchor, or its anchor is no longer on
 *   the follower's chain;
 * - an `IntentPredicateWait` (`INTENT_COMMIT_ANCHOR_NOT_DEEP`) while the
 *   view is fewer than d blocks above the anchor;
 * - `true` otherwise.
 */
const commitAnchorSettled = async (
  read: Read,
  headerHash: Buffer,
): Promise<boolean> => {
  const { tx, view, deps } = read;
  const rows = await tx.query(
    `SELECT p.commit_anchor_height AS height,
        ${commitAnchorCanonicalText("p")} AS canonical
      FROM pending_block_finalizations p WHERE p.header_hash = ?`,
    [headerHash],
  );
  const row = rows[0];
  if (row === undefined || row.height == null || row.canonical !== true)
    return false;
  const anchorHeight = Number(row.height);
  if (view.height < anchorHeight + deps.commitEventDepth)
    throw new IntentPredicateWait(
      INTENT_COMMIT_ANCHOR_NOT_DEEP,
      `commit ${headerHash.toString("hex")}: the view at height ${view.height.toString()} is fewer than ${deps.commitEventDepth.toString()} blocks above its anchor at height ${anchorHeight.toString()}`,
    );
  return true;
};

/**
 * The state queue's Q61 append fence (`state_queue_head_allows_append_v1`,
 * `onchain/aiken/validators/state-queue.ak:137-158`), which a non-empty
 * append checks against the queue's head (`state-queue.ak:1207-1229`): no
 * completed fraud proof on the head, and its DA status Attested or
 * Published, or Unattested with the transaction's inclusive upper validity
 * bound strictly before `end_time + da_attestation_timeout`; Challenged
 * never. The inclusive upper bound is the POSIX time of the exclusive upper
 * slot bound minus one; an unattested head with no upper bound cannot pass
 * (the validator requires a closed range). An empty queue (the root is the
 * tail) has no head to check. The head's header validity is the walk's
 * decode (a node of another protocol version is not a queue node).
 */
const headAllowsAppend = (read: Read, queue: LandedStateQueue): boolean => {
  const head = queue.nodes[0];
  if (head === undefined) return true;
  const node = Effect.runSync(
    SDK.getStateQueueNodeFromStateQueueDatum(head.element.datum),
  );
  if (node.proven_fraud !== null) return false;
  const status = SDK.daAvailabilityStateQueueStatusKind(node.da_attestation);
  if (status === "Challenged") return false;
  if (status !== "Unattested") return true;
  const { validToSlot } = read.state.intent;
  if (validToSlot === null) return false;
  const inclusiveUpper = BigInt(read.deps.slotToPosixMs(validToSlot)) - 1n;
  return inclusiveUpper < head.endTimeMs + SDK.DA_ATTESTATION_TIMEOUT_MS;
};

/**
 * The scheduler names this operator, and its shift has not ended by the
 * commit's lower validity bound. A bound before the shift's start does not
 * rule the commit out: the state queue checks only that the scheduler names
 * the committer, and the node builds a commit against an appointment it
 * submits just before it, whose start is that appointment's time.
 */
const schedulerOurs = (read: Read, set: OperatorSet): boolean => {
  const datum = set.scheduler?.datum;
  if (datum === undefined || datum === "NoActiveOperators") return false;
  const { operator, start_time: start } = datum.ActiveOperator;
  return (
    operator === set.ownKey &&
    BigInt(validFromMs(read)) <= start + SDK.SHIFT_DURATION_MS - 1n
  );
};

const commit = async (read: Read): Promise<boolean> => {
  const tail = keyRest(read.state, "commit:tail=");
  const header = read.state.intent.contentRef;
  if (header === null) throw new Error("a commit intent names no header");
  const journal = await read.tx.query(
    "SELECT status FROM pending_block_finalizations WHERE header_hash = ?",
    [header],
  );
  if (journal.length === 0 || journal[0]!.status === "abandoned") return false;
  const queue = await read.queue();
  if (landedTail(queue)?.outRef !== tail || !spends(read.state, tail))
    return false;
  if (!headAllowsAppend(read, queue)) return false;
  if (!schedulerOurs(read, await read.operators())) return false;
  return commitAnchorSettled(read, header);
};

const merge = async (read: Read): Promise<boolean> => {
  const queue = await read.queue();
  const head = queue.nodes[0];
  return (
    head !== undefined &&
    queue.root !== null &&
    head.headerHash === contentRefHex(read.state) &&
    spends(read.state, head.outRef) &&
    spends(read.state, queue.root.outRef)
  );
};

const attest = async (read: Read): Promise<boolean> =>
  queueNode(await read.queue(), contentRefHex(read.state)) !== undefined;

/**
 * The removed header is still landed, and its correction's target is still
 * unattested and timed out at the transaction's lower validity bound.
 * Key: `correction:<target>:<kind>:<removed>`.
 */
const correction = async (read: Read): Promise<boolean> => {
  const [target] = keyRest(read.state, "correction:").split(":");
  const queue = await read.queue();
  const node = queueNode(queue, target ?? "");
  return (
    queueNode(queue, contentRefHex(read.state)) !== undefined &&
    node !== undefined &&
    node.daStatus === "Unattested" &&
    node.endTimeMs + SDK.DA_ATTESTATION_TIMEOUT_MS <= BigInt(validFromMs(read))
  );
};

const registeredOf = (set: OperatorSet, operator: string) =>
  set.registered.filter(
    (node) => SDK.registeredNodeOperator(node.datum) === operator,
  );

const activeOf = (set: OperatorSet, operator: string) =>
  set.active.filter((node) => SDK.nodeKeyHex(node.datum.key) === operator);

/** The operator-set transitions, by workflow-key prefix. */
const operatorTransition = async (read: Read): Promise<boolean> => {
  const set = await read.operators();
  const [verb, ...rest] = read.state.intent.workflowKey.split(":");
  const key = rest.at(-1) ?? "";
  switch (verb) {
    case "register":
      return (
        registeredOf(set, key).length === 0 && activeOf(set, key).length === 0
      );
    case "activate":
      return (
        registeredOf(set, key).length > 0 && activeOf(set, key).length === 0
      );
    case "deregister":
      return registeredOf(set, key).length > 0;
    case "retire":
      return activeOf(set, key).length > 0;
    case "recover_bond":
      if (key !== set.ownKey)
        throw new Error(`recover_bond for ${key} is not this operator's`);
      return set.ownRetired !== null;
    case "exit":
      // `exit:slash_duplicate:<registered node key>`: that node is still listed.
      return set.registered.some(
        (node) => SDK.nodeKeyHex(node.datum.key) === key,
      );
    case "takeover": {
      // `takeover:<skipped operator>:<new start time>`.
      const [skipped, newStart] = rest;
      const datum = set.scheduler?.datum;
      return (
        datum !== undefined &&
        datum !== "NoActiveOperators" &&
        datum.ActiveOperator.operator === skipped &&
        datum.ActiveOperator.start_time < BigInt(newStart ?? "0")
      );
    }
    case "scheduler": {
      const scheduler = set.scheduler?.utxo;
      return (
        scheduler !== undefined &&
        `${scheduler.txHash}#${scheduler.outputIndex.toString()}` ===
          rest.join(":")
      );
    }
    default:
      throw new Error(
        `no operator-set transition for intent key ${read.state.intent.workflowKey}`,
      );
  }
};

const PREDICATES: Readonly<
  Record<NodeIntentFamily, (read: Read) => Promise<boolean>>
> = {
  commit,
  scheduler_refresh: operatorTransition,
  merge,
  attest,
  correction,
  register: operatorTransition,
  activate: operatorTransition,
  deregister: operatorTransition,
  exit: operatorTransition,
  takeover: operatorTransition,
  retire: operatorTransition,
  recover_bond: operatorTransition,
  reserve_payout: payout,
  settlement: payout,
  reference_publication: referencePublication,
  reference_sweep: referenceSweep,
  reference_funding: referenceFunding,
  script_reward_registration: stakeRegistration,
  phas_membership: stakeRegistration,
  list_insert: listInsert,
};

/** The §8.4 predicate over the projections (see the module doc). */
export const nodeFamilyPredicate = (
  deps: NodeFamilyPredicateDeps,
): ((state: IntentState) => Promise<boolean>) => {
  const mirror: OperatorSetMirror | null =
    deps.operatorSet === null
      ? null
      : createOperatorSetMirror(deps.operatorSet);
  return async (state) => {
    const predicate = (
      PREDICATES as Readonly<
        Record<string, ((read: Read) => Promise<boolean>) | undefined>
      >
    )[state.intent.family];
    if (predicate === undefined)
      throw new Error(`no §8.4 predicate for family ${state.intent.family}`);
    return deps.store.transaction("write", async (tx) => {
      const { dialect } = deps.store;
      const view = await currentViewIn(tx, dialect);
      if (view === null) throw new Error("the follower has no view");
      let queue: LandedStateQueue | undefined;
      return predicate({
        tx,
        dialect,
        view,
        state,
        deps,
        queue: async () => {
          if (queue !== undefined) return queue;
          const read = await landedStateQueueIn(
            tx,
            dialect,
            deps.stateQueue,
            view,
          );
          if (read.kind !== "ok")
            throw new Error(
              `the landed state queue is unreadable: ${read.detail}`,
            );
          if (!read.queue.healthy)
            throw new Error(
              `the landed state queue is unhealthy (${read.queue.reason ?? "unknown"})`,
            );
          queue = read.queue;
          return queue;
        },
        operators: async () => {
          if (mirror === null)
            throw new Error("the operator key is unreadable: no operator set");
          const read = await mirror.refresh(tx, dialect);
          if (read.kind !== "ok")
            throw new Error(`the operator set is unreadable: ${read.detail}`);
          if (read.set.unhealthy !== null)
            throw new Error(
              `the operator set is unhealthy: ${read.set.unhealthy}`,
            );
          return read.set;
        },
      });
    });
  };
};
