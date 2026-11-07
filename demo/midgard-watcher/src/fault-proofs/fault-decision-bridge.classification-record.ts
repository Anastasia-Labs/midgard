import {
  type HeaderDecision,
  isRetainedDaPayloadUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { Header } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
  WatcherStateQueueRemovalKind,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherVerificationDiagnostic } from "../runtime/operations-observability.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";
import type { WatcherPersistedFaultDecisionRecord } from "./fault-decision-journal.js";

const observationKey = (
  decision: Pick<
    HeaderDecision,
    "headerHash" | "authenticatedObservationDigest"
  >,
): string =>
  `${decision.headerHash}\u0000${decision.authenticatedObservationDigest}`;

/**
 * Only fault decisions carry durable authority: the selected target, proof
 * progress and prover-funding recovery read nothing else. Healthy and
 * unprovable decisions fold replay context that later L1 activity moves
 * (event anchors, settlement snapshots), so the bridge no longer journals
 * them. Such records an earlier writer left stay admitted by the journal's
 * chain but are inert here: not compared, and not counted as a repeated
 * observation identity.
 */
export const durableFaultDecisions = (
  records: readonly WatcherPersistedFaultDecisionRecord[],
): ReadonlyMap<string, WatcherPersistedFaultDecisionRecord> => {
  const byObservation = new Map<string, WatcherPersistedFaultDecisionRecord>();
  for (const record of records) {
    if (record.decision.decision !== "fault_detected") continue;
    const key = observationKey(record.decision);
    if (byObservation.has(key)) {
      throw new Error(
        "durable decision evidence repeats a header observation identity",
      );
    }
    byObservation.set(key, record);
  }
  return byObservation;
};

export const assertDurableDecisionEvidence = (
  durable: ReadonlyMap<string, WatcherPersistedFaultDecisionRecord>,
  decision: HeaderDecision,
): void => {
  const prior = durable.get(observationKey(decision));
  if (
    prior !== undefined &&
    prior.decision.decisionDigest !== decision.decisionDigest
  ) {
    throw new Error(
      "fresh production classification differs from durable decision evidence",
    );
  }
};

/**
 * Classification of the header at `index` waits for a public retained-DA
 * payload that is not served, in exactly two cases:
 *
 * - the header's own payload, while it is Unattested: the node pushes it to
 *   the committee only after its commit confirms, so a freshly committed
 *   header can reach classification first, and an Unattested header cannot
 *   merge;
 * - its predecessor's payload, when the watcher's own L1 evidence shows that
 *   predecessor merged: the observation's confirmed head, or a queued header
 *   the release-final walk proved merged. Retention keeps such a payload until
 *   a later merge is final, so a miss here is a prune race, not a fault.
 *
 * Any other header, availability state or failure (conflicting or malformed
 * DA, an unmerged or removed predecessor's payload) keeps its fail-closed
 * path.
 */
export const awaitsRetainedDaPayload = (
  error: unknown,
  observation: WatcherAuthenticatedStateQueueObservation,
  index: number,
  merged: ReadonlyMap<string, WatcherReleasedHeaderProof>,
): boolean => {
  const header = observation.finalizedHeaders[index];
  if (header === undefined || !isRetainedDaPayloadUnavailableError(error))
    return false;
  if (error.headerHash === header.headerHash)
    return header.daAvailability === "Unattested";
  const predecessor = observation.finalizedHeaders[index - 1];
  if (predecessor === undefined)
    return (
      index === 0 &&
      error.headerHash ===
        Data.from(header.headerCborHex, Header).prevHeaderHash
    );
  const proof = merged.get(predecessor.headerHash);
  return (
    error.headerHash === predecessor.headerHash &&
    proof !== undefined &&
    "mergeTransactionHash" in proof
  );
};

const verificationOutcome = (
  decision: HeaderDecision,
): WatcherVerificationDiagnostic["outcome"] =>
  decision.decision === "fault_detected"
    ? "fault_detected"
    : decision.decision === "healthy"
      ? "verified"
      : "unprovable_gap";

/**
 * Times one header classification for the operations sink. A decided header
 * also carries the payload envelope digest it was replayed from: healthy
 * decisions are not journaled, so this record is their only evidence.
 */
export const verificationRecorder = (
  dependencies: Pick<
    BridgeDependencies,
    "operationsSink" | "nowMs" | "monotonicNowMs"
  >,
  headerHash: string,
) => {
  const nowMs = dependencies.nowMs ?? (() => BigInt(Date.now()));
  const monotonicNowMs =
    dependencies.monotonicNowMs ?? (() => performance.now());
  const queuedAtMs = nowMs().toString();
  let startedMonotonicMs = monotonicNowMs();
  let startedAtMs = queuedAtMs;
  const record = (
    subjectDigest: string,
    outcome: WatcherVerificationDiagnostic["outcome"],
    evidence: Pick<
      WatcherVerificationDiagnostic,
      | "payloadEnvelopeSha256"
      | "mergeTransactionHash"
      | "removalTransactionHash"
      | "removalKind"
    > = {},
  ): void =>
    dependencies.operationsSink?.recordVerification({
      subjectDigest,
      headerHash,
      ...evidence,
      queuedAtMs,
      startedAtMs,
      completedAtMs: nowMs().toString(),
      elapsedMs: Math.ceil(monotonicNowMs() - startedMonotonicMs).toString(),
      outcome,
    });
  return Object.freeze({
    start: (): void => {
      startedAtMs = nowMs().toString();
      startedMonotonicMs = monotonicNowMs();
    },
    record: (
      subjectDigest: string,
      outcome: "pending_da" | "pending_l1" | "failed",
    ): void => record(subjectDigest, outcome),
    recordPastHorizon: (subjectDigest: string): void =>
      record(subjectDigest, "unverified_past_horizon"),
    recordMerged: (subjectDigest: string, mergeTransactionHash: string): void =>
      record(subjectDigest, "unverified_merged", { mergeTransactionHash }),
    recordRemoved: (
      subjectDigest: string,
      removalTransactionHash: string,
      removalKind: WatcherStateQueueRemovalKind,
    ): void =>
      record(subjectDigest, "unverified_removed", {
        removalTransactionHash,
        removalKind,
      }),
    recordDecision: (subjectDigest: string, decision: HeaderDecision): void =>
      record(
        subjectDigest,
        verificationOutcome(decision),
        decision.payloadEnvelopeSha256 === undefined
          ? {}
          : { payloadEnvelopeSha256: decision.payloadEnvelopeSha256 },
      ),
  });
};
