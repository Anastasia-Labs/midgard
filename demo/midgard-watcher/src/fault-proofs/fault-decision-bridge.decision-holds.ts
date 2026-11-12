import {
  createValidationTraceReplayHolds,
  type HeaderDecision,
} from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import {
  durableDisagreement,
  durableFaultDecisions,
} from "./fault-decision-bridge.classification-record.js";
import {
  type BridgeDependencies,
  exactLockedFraudProof,
} from "./fault-decision-bridge.selected-target.js";
import type { WatcherDecisionSupersedeHoldReason } from "./fault-proof-progress-authority.js";

type Hold = Readonly<{
  headerHash: string;
  reason: WatcherDecisionSupersedeHoldReason;
  detail: string;
}>;

type LockedFraudProof = Readonly<{
  headerHash: string;
  fraudProofAssetName: string;
}>;

/** A correction with no target this pass, and the lock state it is read in. */
type Untargeted = Readonly<{
  correction: LockedFraudProof;
  lock: "locked" | "idle";
}>;

/** The one removal this process runs from a proven correction's record. */
type Finishing = {
  readonly fraudProofAssetName: string;
  lock: Untargeted["lock"];
  fraudProofOutRef: string | null;
  running: boolean;
  failure: string | null;
};

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const failureSuffix = ({ failure }: Finishing): string =>
  failure === null ? "" : `; last attempt failed: ${failure}`;

/**
 * The bridge's side of durable decision continuity. A fresh classification
 * whose durable fault for the same observation differs (the classifier or
 * its configuration changed) supersedes that fault through the progress
 * authority, or holds the header while a signed attempt of it is unresolved
 * or the CorrectionLock is Locked under it. A held header yields no target
 * for the pass; every other header is still selected. A Locked fraud-proof
 * correction no admitted fault reproduces is held the same way. Each hold
 * is on readiness while it lasts and warned once when it begins.
 *
 * A proven correction is driven to the end from L1, whatever the decisions
 * say, where L1 holds its record: finished from a Locked lock that yields no
 * target, held or unreproduced, or started from an Idle lock on the first
 * queued header a completed proof convicts, unless that correction is the
 * pass's target. One removal at a time, started on a dispatching pass and,
 * after a failure, started again on a dispatching pass once the retry
 * backoff (1 s doubling to 60 s) has run; a failure is warned when it first
 * happens or its message changes.
 */
export const decisionHolds = (
  dependencies: Pick<
    BridgeDependencies,
    | "readRecords"
    | "supersede"
    | "warn"
    | "driveProvenRemoval"
    | "monotonicNowMs"
  >,
) => {
  let holds: readonly Hold[] = [];
  let untargeted: Untargeted | null = null;
  let unreproduced: LockedFraudProof | null = null;
  let warnedLock: string | null = null;
  let finishing: Finishing | null = null;
  /** Failed removals, by fraud-proof asset name, and the message last
   * warned for each. */
  const retries = createValidationTraceReplayHolds({
    now: dependencies.monotonicNowMs ?? (() => performance.now()),
  });
  const warnedFailures = new Map<string, string>();
  const retryEvent = (key: string) => ({
    headerHash: key,
    position: 0,
    txId: key,
  });
  const settled = (key: string): void => {
    retries.decided(key);
    warnedFailures.delete(key);
  };
  return Object.freeze({
    /**
     * Supersedes or holds, in queue order, each classified header whose
     * durable fault differs; a held header's classification becomes null.
     */
    settle: async (
      observation: WatcherAuthenticatedStateQueueObservation,
      classifications: (HeaderDecision | null)[],
    ): Promise<void> => {
      const durable = durableFaultDecisions(await dependencies.readRecords());
      const lock = observation.finalizedCorrectionLock;
      const locked = lock === null ? null : exactLockedFraudProof(lock);
      const next: Hold[] = [];
      for (let index = 0; index < classifications.length; index += 1) {
        const fresh = classifications[index];
        if (fresh === null || fresh === undefined) continue;
        const prior = durableDisagreement(durable, fresh)?.decision;
        if (prior?.decision !== "fault_detected") continue;
        const lockedName = `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[prior.category]}${prior.headerHash}`;
        const outcome =
          locked?.headerHash === prior.headerHash &&
          locked.fraudProofAssetName === lockedName
            ? ({
                kind: "held",
                reason: "correction_locked",
                detail: lockedName,
              } as const)
            : await dependencies.supersede({ observation, prior, fresh });
        if (outcome.kind === "superseded") continue;
        classifications[index] = null;
        next.push(
          Object.freeze({
            headerHash: prior.headerHash,
            reason: outcome.reason,
            detail: outcome.detail,
          }),
        );
      }
      for (const hold of next) {
        if (
          !holds.some(
            ({ headerHash, reason }) =>
              headerHash === hold.headerHash && reason === hold.reason,
          )
        )
          dependencies.warn?.({
            event: "fault_decision_supersede_held",
            headerHash: hold.headerHash,
            cause: hold.reason,
          });
      }
      holds = Object.freeze(next);
      untargeted = null;
      unreproduced = null;
    },
    /** Target selection found a Locked correction no admitted fault reproduces. */
    unreproducedLock: (locked: LockedFraudProof): void => {
      untargeted = { correction: locked, lock: "locked" };
      // A lock under a held journaled fault is that hold, not a stray lock.
      if (
        holds.some(
          ({ reason, detail }) =>
            reason === "correction_locked" &&
            detail === locked.fraudProofAssetName,
        )
      )
        return;
      unreproduced = locked;
      if (warnedLock === locked.fraudProofAssetName) return;
      warnedLock = locked.fraudProofAssetName;
      dependencies.warn?.({ event: "correction_lock_unreproduced", ...locked });
    },
    /** Target selection found a queued header a completed proof convicts. */
    provenHeader: (proven: LockedFraudProof): void => {
      untargeted = { correction: proven, lock: "idle" };
    },
    /** Drives the removal of this pass's untargeted proven correction. */
    finishUntargeted: async (): Promise<void> => {
      const current = untargeted;
      if (
        current?.correction.fraudProofAssetName !==
          finishing?.fraudProofAssetName &&
        finishing?.running !== true
      ) {
        if (finishing !== null)
          warnedFailures.delete(finishing.fraudProofAssetName);
        finishing = null;
      }
      const finish = dependencies.driveProvenRemoval;
      if (current === null || finish === undefined || finishing?.running)
        return;
      const locked = current.correction;
      const key = locked.fraudProofAssetName;
      if (retries.pass().waiting(key, retryEvent(key)) !== undefined) return;
      const attempt: Finishing = finishing ?? {
        fraudProofAssetName: locked.fraudProofAssetName,
        lock: current.lock,
        fraudProofOutRef: null,
        running: false,
        failure: null,
      };
      attempt.lock = current.lock;
      const failed = (error: unknown) => {
        attempt.running = false;
        attempt.failure = message(error);
        retries.failed(key, { ...retryEvent(key), detail: attempt.failure });
        if (warnedFailures.get(key) === attempt.failure) return;
        warnedFailures.set(key, attempt.failure);
        dependencies.warn?.({
          event: "correction_lock_finishing_removal_failed",
          ...locked,
          message: attempt.failure,
        });
      };
      finishing = attempt;
      let started;
      try {
        started = await finish(locked);
      } catch (error) {
        failed(error);
        return;
      }
      if (started.kind !== "proven") {
        finishing = null;
        if (started.kind === "finished") {
          unreproduced = null;
          settled(key);
        }
        return;
      }
      if (attempt.fraudProofOutRef !== started.fraudProofOutRef)
        dependencies.warn?.({
          event: "correction_lock_finishing_removal",
          ...locked,
          fraudProofOutRef: started.fraudProofOutRef,
        });
      attempt.fraudProofOutRef = started.fraudProofOutRef;
      attempt.running = true;
      attempt.failure = null;
      // A confirmed removal is seen as the live lock moving on next pass.
      started.removal.then(() => {
        attempt.running = false;
        settled(key);
        if (finishing === attempt) finishing = null;
      }, failed);
    },
    readiness: (): readonly Readonly<{ reason: string; detail: string }>[] => [
      ...holds.map(({ headerHash, reason, detail }) => ({
        reason: "fault_decision_supersede_held",
        detail: `${headerHash}: ${reason}: ${detail}`,
      })),
      ...(finishing?.fraudProofOutRef != null
        ? [
            {
              reason: "correction_lock_finishing_removal",
              detail: `${finishing.fraudProofAssetName}: ${finishing.lock === "locked" ? "locked: finishing removal" : "idle: removing the queued header"} from the proven record ${finishing.fraudProofOutRef}${failureSuffix(finishing)}`,
            },
          ]
        : unreproduced === null
          ? []
          : [
              {
                reason: "correction_lock_unreproduced",
                detail: `${unreproduced.fraudProofAssetName}${finishing?.fraudProofAssetName === unreproduced.fraudProofAssetName ? failureSuffix(finishing) : ""}`,
              },
            ]),
    ],
  });
};
