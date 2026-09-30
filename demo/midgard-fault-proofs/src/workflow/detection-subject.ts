import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";

/**
 * The source frontier of the event a detection convicts. `block` names a fault
 * of the committed block as a whole (a count, link or omission fault) rather
 * than of one event in it.
 */
export type DetectionFrontier =
  | "accepted"
  | "block"
  | "deposit"
  | "forced"
  | "withdrawal";

/** What a detector states about the events its finding convicts. */
export type DetectionSubject = Readonly<{
  frontier: DetectionFrontier;
  /**
   * Canonical `EventKey` CBOR of every event the finding convicts: none for
   * `block`, one for a single-event finding, and both events for a pair
   * finding, whose later event is the transition it convicts.
   */
  subjectEventKeyCbors: readonly string[];
}>;

const FRONTIER_PHASE: Readonly<
  Record<Exclude<DetectionFrontier, "block">, SDK.TransitionPhase>
> = {
  withdrawal: "Withdrawal",
  forced: "ForcedTransaction",
  accepted: "L2Transaction",
  deposit: "Deposit",
};

const PHASE_FRONTIER: Readonly<
  Record<SDK.TransitionPhase, Exclude<DetectionFrontier, "block">>
> = {
  Withdrawal: "withdrawal",
  ForcedTransaction: "forced",
  L2Transaction: "accepted",
  Deposit: "deposit",
};

export const BLOCK_SUBJECT: DetectionSubject = Object.freeze({
  frontier: "block",
  subjectEventKeyCbors: Object.freeze([]),
});

/** The subject of a finding about exactly the given events of one phase. */
export const eventSubject = (
  ...eventKeys: readonly [SDK.EventKey, ...SDK.EventKey[]]
): DetectionSubject => {
  const phase = eventKeyPhase(eventKeys[0]);
  if (eventKeys.some((eventKey) => eventKeyPhase(eventKey) !== phase))
    throw new Error("a detection subject names events of one phase");
  return Object.freeze({
    frontier: PHASE_FRONTIER[phase],
    subjectEventKeyCbors: Object.freeze(eventKeys.map(eventKeyFingerprint)),
  });
};

/**
 * The subject of a finding that proves exactly one committed event, given as
 * canonical `EventKey` CBOR, or of a block-level finding that proves none.
 */
export const eventKeyCborSubject = (
  eventKeyCbor: string | undefined,
): DetectionSubject =>
  eventKeyCbor === undefined
    ? BLOCK_SUBJECT
    : eventSubject(Data.from(eventKeyCbor, SDK.EventKey));

export const sourceEventSubject = (
  source: Pick<SourceEventRecord, "eventKey">,
): DetectionSubject => eventSubject(source.eventKey);

export const acceptedTransactionSubject = (
  ...txIds: readonly [string, ...string[]]
): DetectionSubject =>
  eventSubject(
    ...(txIds.map((txId) => ({ L2TransactionEventKey: { tx_id: txId } })) as [
      SDK.EventKey,
      ...SDK.EventKey[],
    ]),
  );

export const forcedTransactionSubject = (
  orderKey: SDK.OutputReference,
): DetectionSubject =>
  eventSubject({ ForcedTransactionEventKey: { tx_order_id: orderKey } });

/**
 * The event a proof-thread verdict subject names: the accepted transaction by
 * id, or the forced source by its order key.
 */
export const verdictDetectionSubject = (
  subject: SDK.VerdictSubject,
): DetectionSubject => {
  if (subject.source_kind === SDK.PROOF_THREAD_SOURCE_KIND_ACCEPTED)
    return acceptedTransactionSubject(subject.transaction_id);
  if (subject.source_kind === SDK.PROOF_THREAD_SOURCE_KIND_FORCED)
    return forcedTransactionSubject(
      Data.from(subject.source_key, SDK.OutputReference),
    );
  throw new Error(
    `verdict subject has unknown source kind ${subject.source_kind.toString()}`,
  );
};

export const withdrawalSubject = (
  ...withdrawalIds: readonly [SDK.OutputReference, ...SDK.OutputReference[]]
): DetectionSubject =>
  eventSubject(
    ...(withdrawalIds.map((withdrawalId) => ({
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    })) as [SDK.EventKey, ...SDK.EventKey[]]),
  );

export const depositSubject = (
  depositId: SDK.OutputReference,
): DetectionSubject =>
  eventSubject({ DepositEventKey: { deposit_id: depositId } });

/**
 * The committed event that introduced a post-state output: the accepted or
 * forced transaction whose id the output reference names, or the deposit whose
 * event id it is. The earliest such event in canonical order is the producer.
 * An output no committed event produced is a fault of the block's post-state
 * as a whole.
 */
export const introducedOutputSubject = (
  reconstruction: Pick<TransitionTraceReconstruction, "sourceEvents">,
  outRef: Readonly<{ transactionId: string; outputIndex: bigint }>,
): DetectionSubject => {
  const transactionId = outRef.transactionId.toLowerCase();
  const producer = reconstruction.sourceEvents.find((source) => {
    switch (source.phase) {
      case "Withdrawal":
        return false;
      case "ForcedTransaction":
        return source.entry.value.tx_id.toLowerCase() === transactionId;
      case "L2Transaction":
        return source.entry.txId.toLowerCase() === transactionId;
      case "Deposit":
        return (
          source.entry.key.transactionId.toLowerCase() === transactionId &&
          source.entry.key.outputIndex === outRef.outputIndex
        );
    }
  });
  return producer === undefined ? BLOCK_SUBJECT : sourceEventSubject(producer);
};

/** Copies exactly the subject fields of a family detection. */
export const subjectOf = ({
  frontier,
  subjectEventKeyCbors,
}: DetectionSubject): DetectionSubject =>
  Object.freeze({ frontier, subjectEventKeyCbors });

/**
 * A point on the one total event order of a block: the `step_index` of the
 * event's step in the authenticated transition trace, the order the replay
 * applies events in (GOAL_SPEC §6: the earliest invalid transition is the
 * earliest trace step). Within a phase that is the operator's application
 * order, which need not be the committed list order: a transaction that
 * spends a same-block output steps after the transaction producing it. A
 * block-level finding precedes every event. An event of the block that no
 * trace step names follows every traced event, in source-event order; the
 * block then has a block-level structural finding, which sorts first. The
 * committed `event_to_step` is never read: it is operator-written, so an
 * omitted or permuted entry is a fault to prove, never an input that orders
 * the proofs.
 */
export type EventOrder = number;

const BLOCK_ORDER: EventOrder = -1;

export const compareEventOrder = (
  left: EventOrder,
  right: EventOrder,
): number => left - right;

type EventOrderSource = Pick<
  TransitionTraceReconstruction,
  "sourceEvents" | "sourceEventsByFingerprint" | "transitionTrace"
>;

const eventOrders = new WeakMap<
  EventOrderSource["sourceEvents"],
  ReadonlyMap<string, EventOrder>
>();

/**
 * Every source event's order, built once per reconstruction. The trace is
 * scanned in `step_index` order and the lowest step naming an event wins;
 * untraced events are placed after the last step in source-event order.
 */
const eventOrdersOf = (
  reconstruction: EventOrderSource,
): ReadonlyMap<string, EventOrder> => {
  const cached = eventOrders.get(reconstruction.sourceEvents);
  if (cached !== undefined) return cached;
  const stepByFingerprint = new Map<string, number>();
  for (const { key, value } of [...reconstruction.transitionTrace].sort(
    (left, right) => (left.key < right.key ? -1 : left.key > right.key ? 1 : 0),
  )) {
    const fingerprint = eventKeyFingerprint(value.event_key);
    if (!stepByFingerprint.has(fingerprint))
      stepByFingerprint.set(fingerprint, Number(key));
  }
  const untracedBase = reconstruction.transitionTrace.length;
  const orders = new Map(
    reconstruction.sourceEvents.map(
      ({ fingerprint }, index) =>
        [
          fingerprint,
          stepByFingerprint.get(fingerprint) ?? untracedBase + index,
        ] as const,
    ),
  );
  eventOrders.set(reconstruction.sourceEvents, orders);
  return orders;
};

/**
 * Places one committed event on the block's event order. An event that is
 * not a source event of this block at all is a watcher-internal
 * inconsistency and fails closed; an event of the block never throws, whether
 * or not the trace or the committed `event_to_step` names it.
 */
export const eventOrder = (
  reconstruction: EventOrderSource,
  eventKeyCbor: string,
): EventOrder => {
  const fingerprint = eventKeyFingerprint(
    Data.from(eventKeyCbor, SDK.EventKey),
  );
  if (fingerprint !== eventKeyCbor)
    throw new Error(`event key ${eventKeyCbor} is not canonical EventKey CBOR`);
  const order = eventOrdersOf(reconstruction).get(fingerprint);
  if (
    order === undefined ||
    !reconstruction.sourceEventsByFingerprint.has(fingerprint)
  )
    throw new Error(`event ${fingerprint} is not a source event of this block`);
  return order;
};

/**
 * The event a detection convicts, placed on the event order from its
 * declared subject only, never from its position, its violation id or the
 * committed `event_to_step`. A pair finding convicts its later event.
 */
export const detectionEventOrder = (
  reconstruction: EventOrderSource,
  detection: DetectionSubject & Readonly<{ detectionId: string }>,
): EventOrder => {
  const { frontier, subjectEventKeyCbors } = detection;
  if (frontier === "block") {
    if (subjectEventKeyCbors.length !== 0)
      throw new Error(
        `block-level detection ${detection.detectionId} must not name events`,
      );
    return BLOCK_ORDER;
  }
  const phase = FRONTIER_PHASE[frontier] as SDK.TransitionPhase | undefined;
  if (phase === undefined)
    throw new Error(
      `detection ${detection.detectionId} has unknown frontier ${String(frontier)}`,
    );
  if (subjectEventKeyCbors.length === 0)
    throw new Error(
      `detection ${detection.detectionId} names no subject event`,
    );
  return Math.max(
    ...subjectEventKeyCbors.map((eventKeyCbor) => {
      if (eventKeyPhase(Data.from(eventKeyCbor, SDK.EventKey)) !== phase)
        throw new Error(
          `detection ${detection.detectionId} names an event outside its ${frontier} frontier`,
        );
      return eventOrder(reconstruction, eventKeyCbor);
    }),
  );
};
