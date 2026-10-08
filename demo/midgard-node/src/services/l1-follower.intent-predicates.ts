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
 * - The families whose target state is not in the facts (certificate
 *   registrations, working-capital funding) throw `FamilyPredicateUnavailable`:
 *   their intents are never resubmitted and never abandoned.
 */
import {
  currentViewIn,
  decodeTransaction,
  type Dialect,
  encodeOutRef,
  type FactStore,
  type IntentState,
  liveUtxosIn,
  type OutRef,
  readIntentIn,
  type SqlTx,
  type View,
} from "@al-ft/midgard-l1-follower";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";

import {
  createOperatorSetMirror,
  type OperatorSet,
  type OperatorSetConfig,
  type OperatorSetMirror,
} from "../l1-operator-set/index.js";
import {
  type LandedStateQueue,
  landedStateQueueIn,
  landedTail,
  type StateQueueProjectionConfig,
} from "../l1-state-queue/index.js";

/** A family whose target state the follower's facts do not hold. */
export class FamilyPredicateUnavailable extends Error {
  constructor(family: string) {
    super(
      `the ${family} family's target state is not in the follower's facts; its intent is held, never resent or abandoned`,
    );
    this.name = "FamilyPredicateUnavailable";
  }
}

export type NodeFamilyPredicateDeps = Readonly<{
  store: Pick<FactStore, "dialect" | "transaction">;
  stateQueue: StateQueueProjectionConfig;
  /** The operator set and this operator's key hash; null when the key is unreadable. */
  operatorSet: Readonly<{ config: OperatorSetConfig; ownKey: string }> | null;
  /** POSIX milliseconds at the start of a slot (the node's slot clock). */
  slotToPosixMs: (slot: number) => number;
  /** The horizon lag d: a commit's events are at least d blocks below the tip. */
  horizonLagBlocks: number;
}>;

type Read = Readonly<{
  tx: SqlTx;
  dialect: Dialect;
  view: View;
  state: IntentState;
  deps: NodeFamilyPredicateDeps;
  queue: () => Promise<LandedStateQueue>;
  operators: () => Promise<OperatorSet>;
}>;

const outRefText = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

const spends = (state: IntentState, outRef: string): boolean =>
  state.intent.inputs.some((input) => outRefText(input) === outRef);

/** The text after `prefix` in the workflow key; a key of another shape throws. */
const keyRest = (state: IntentState, prefix: string): string => {
  const { workflowKey } = state.intent;
  if (!workflowKey.startsWith(prefix))
    throw new Error(
      `${state.intent.family} intent key ${workflowKey} lacks ${prefix}`,
    );
  return workflowKey.slice(prefix.length);
};

const contentRefHex = (state: IntentState): string => {
  if (state.intent.contentRef === null)
    throw new Error(
      `${state.intent.family} intent ${state.intent.workflowKey} has no content reference`,
    );
  return state.intent.contentRef.toString("hex");
};

const queueNode = (queue: LandedStateQueue, headerHash: string) =>
  queue.nodes.find((node) => node.headerHash === headerHash);

const count = async (
  tx: SqlTx,
  sql: string,
  params: readonly (Buffer | number | string)[],
): Promise<number> => {
  const rows = await tx.query(sql, [...params]);
  return Number(rows[0]?.n ?? 0);
};

/** The time the intent's validity starts at, or the tip's when it has no lower bound. */
const validFromMs = (read: Read): number =>
  read.deps.slotToPosixMs(
    read.state.intent.validFromSlot ?? read.view.point.slot,
  );

/**
 * Every event the commit's block journal includes is canonical (its
 * admission identity is in the follower's key set) and admitted at least
 * d blocks below the view.
 */
const includedEventsSettled = async (
  read: Read,
  headerHash: Buffer,
): Promise<boolean> => {
  const { tx, view, deps } = read;
  const highest = view.height - deps.horizonLagBlocks;
  for (const [kind, table] of [
    ["deposit", "pending_block_finalization_deposits"],
    ["withdrawal", "pending_block_finalization_withdrawals"],
  ] as const) {
    const unsettled = await count(
      tx,
      `SELECT count(*) AS n FROM ${table} m WHERE m.header_hash = ?
        AND (m.l1_event_key IS NULL OR m.l1_origin_outref IS NULL
          OR NOT EXISTS (SELECT 1 FROM l1_event_keys k WHERE k.kind = ?
            AND k.key = m.l1_event_key AND k.origin_outref = m.l1_origin_outref)
          OR EXISTS (SELECT 1 FROM node_l1_events e WHERE e.kind = ?
            AND e.event_key = m.l1_event_key AND e.admitted_height > ?))`,
      [headerHash, kind, kind, highest],
    );
    if (unsettled > 0) return false;
  }
  const forced = await tx.query(
    `SELECT f.tx_order_l1_tx_hash AS tx_hash, f.tx_order_l1_output_index AS output_index
      FROM pending_block_finalization_forced_transactions m
      LEFT JOIN forced_transaction_utxos f ON f.tx_order_id = m.member_id
      WHERE m.header_hash = ?`,
    [headerHash],
  );
  for (const row of forced) {
    if (!Buffer.isBuffer(row.tx_hash)) return false;
    const txHash = row.tx_hash;
    const index = Number(row.output_index);
    const key = encodeOutRef({ txHash, index });
    const keyed = await count(
      tx,
      "SELECT count(*) AS n FROM l1_event_keys WHERE kind = 'forced' AND key = ? AND origin_outref = ?",
      [key, key],
    );
    const orders = await tx.query(
      "SELECT height FROM node_l1_forced_order_fields WHERE order_tx_hash = ? AND order_output_index = ?",
      [txHash, index],
    );
    if (keyed === 0 && orders.length === 0) return false;
    if (orders.some((order) => Number(order.height) > highest)) return false;
  }
  return true;
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
  if (!schedulerOurs(read, await read.operators())) return false;
  return includedEventsSettled(read, header);
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

/**
 * Settlement and reserve payouts; the content reference is the settled
 * event's id CBOR. Absorb and initialize retire the list event, so the
 * event must still be listed (its admission identity in the key set) and
 * not retired. Fund and conclude spend the payout the initialize created
 * (the event is retired by then): every input must still be a live fact,
 * so no other transaction settled the payout. Key: `...:<step>`.
 */
const payout = async (read: Read): Promise<boolean> => {
  const step = read.state.intent.workflowKey.split(":").at(-1);
  const eventId = read.state.intent.contentRef;
  if (eventId === null) throw new Error("a payout intent names no event");
  if (step === "absorb" || step === "absorb_deposit" || step === "initialize")
    return (
      (await count(
        read.tx,
        `SELECT count(*) AS n FROM node_l1_events e JOIN l1_event_keys k
          ON k.kind = e.kind AND k.key = e.event_key
          WHERE e.kind = ? AND e.event_key = ? AND e.event_id = ?
            AND e.retired_slot IS NULL`,
        [
          step === "initialize" ? "withdrawal" : "deposit",
          eventKeyOfId(eventId),
          eventId,
        ],
      )) > 0
    );
  if (step !== "fund" && step !== "add_funds" && step !== "conclude")
    throw new Error(`unknown payout step ${step ?? ""}`);
  return allInputsLive(read);
};

const allInputsLive = async (read: Read): Promise<boolean> => {
  const live = await liveUtxosIn(read.tx, read.dialect, {
    by: "outref",
    outRefs: read.state.intent.inputs,
  });
  if (live.kind !== "ok") throw new Error(`intent inputs: ${live.kind}`);
  return live.utxos.length === read.state.intent.inputs.length;
};

/** Publication: some script it publishes is not yet live at a reference output of another tx. */
const referencePublication = async (read: Read): Promise<boolean> => {
  const intent = await readIntentIn(
    read.tx,
    read.dialect,
    read.state.intent.txHash,
  );
  if (intent === null) throw new Error("the intent left the journal");
  const scripts = decodeTransaction(intent.txCbor).outputs.flatMap((output) =>
    output.scriptRef === null ? [] : [output.scriptRef.hash],
  );
  if (scripts.length === 0)
    throw new Error("a reference publication publishes no script");
  for (const hash of scripts)
    if (
      (await count(
        read.tx,
        "SELECT count(*) AS n FROM l1_outputs WHERE script_ref_hash = ? AND spent_slot IS NULL AND tx_hash <> ?",
        [hash, intent.txHash],
      )) === 0
    )
      return true;
  return false;
};

/** Sweep: every reference-script output it spends is still live. */
const referenceSweep = async (read: Read): Promise<boolean> => {
  const live = await liveUtxosIn(read.tx, read.dialect, {
    by: "outref",
    outRefs: read.state.intent.inputs,
  });
  if (live.kind !== "ok") throw new Error(`sweep inputs: ${live.kind}`);
  const swept = live.utxos.filter((utxo) => utxo.output.scriptRef !== null);
  return (
    swept.length > 0 && live.utxos.length === read.state.intent.inputs.length
  );
};

const PREDICATES: Readonly<Record<string, (read: Read) => Promise<boolean>>> = {
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
    const predicate = PREDICATES[state.intent.family];
    if (predicate === undefined)
      throw new FamilyPredicateUnavailable(state.intent.family);
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
