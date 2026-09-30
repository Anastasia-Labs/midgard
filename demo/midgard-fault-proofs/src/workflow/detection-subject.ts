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

/** Canonical transition phase order: withdrawals, forced, normal, deposits. */
const PHASE_RANK: Readonly<Record<SDK.TransitionPhase, number>> = {
  Withdrawal: 0,
  ForcedTransaction: 1,
  L2Transaction: 2,
  Deposit: 3,
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
 * A point on the one total event order of a block: phase rank, then the
 * authenticated transition step. Block-level findings precede every event.
 */
export type EventOrder = Readonly<{ rank: number; stepIndex: bigint }>;

const BLOCK_ORDER: EventOrder = Object.freeze({ rank: -1, stepIndex: -1n });

export const compareEventOrder = (left: EventOrder, right: EventOrder) =>
  left.rank !== right.rank
    ? left.rank - right.rank
    : left.stepIndex < right.stepIndex
      ? -1
      : left.stepIndex > right.stepIndex
        ? 1
        : 0;

/**
 * Places one committed event on the block's event order from the
 * authenticated `event_to_step` root. An event the root does not map fails
 * closed.
 */
export const authenticatedEventOrder = (
  reconstruction: Pick<
    TransitionTraceReconstruction,
    "eventToStepByFingerprint"
  >,
  eventKeyCbor: string,
): EventOrder => {
  const eventKey = Data.from(eventKeyCbor, SDK.EventKey);
  const fingerprint = eventKeyFingerprint(eventKey);
  if (fingerprint !== eventKeyCbor)
    throw new Error(`event key ${eventKeyCbor} is not canonical EventKey CBOR`);
  const entry = reconstruction.eventToStepByFingerprint.get(fingerprint);
  if (entry === undefined)
    throw new Error(
      `event ${fingerprint} has no step in the authenticated transition trace`,
    );
  return Object.freeze({
    rank: PHASE_RANK[eventKeyPhase(eventKey)],
    stepIndex: entry.value.step_index,
  });
};

/**
 * The transition a detection convicts, derived only from its declared subject
 * and the authenticated trace, never from its position or violation id. A pair
 * finding convicts its later event.
 */
export const detectionEventOrder = (
  reconstruction: Pick<
    TransitionTraceReconstruction,
    "eventToStepByFingerprint"
  >,
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
  return subjectEventKeyCbors
    .map((eventKeyCbor) => {
      if (eventKeyPhase(Data.from(eventKeyCbor, SDK.EventKey)) !== phase)
        throw new Error(
          `detection ${detection.detectionId} names an event outside its ${frontier} frontier`,
        );
      return authenticatedEventOrder(reconstruction, eventKeyCbor);
    })
    .reduce((later, order) =>
      compareEventOrder(order, later) > 0 ? order : later,
    );
};
