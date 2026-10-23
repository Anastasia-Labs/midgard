import { daRetentionPruneDecision } from "@al-ft/midgard-core";
import {
  isFraudProofL1WaitError,
  isReplayLocalFault,
  isRetainedDaPayloadUnavailableError,
  isValidationTraceReplayHeldError,
  type VALIDATION_TRACE_REPLAY_HELD,
  type VALIDATION_TRACE_REPLAY_STUCK,
  validationTraceCaseOpenAtRisk,
  validationTraceCaseOpenWindowEndMs,
} from "@al-ft/midgard-fault-proofs";
import { StateQueueNode } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
  WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import { watcherDaFetchAlertSubject } from "../runtime/operations-observability.alert-book.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import { isWatcherRetainedDaTransportUnavailable } from "../storage/retained-da-transport-unavailable.js";
import {
  awaitsRetainedDaPayload,
  verificationRecorder,
} from "./fault-decision-bridge.classification-record.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";
import {
  replayLocalFaultHolds,
  type WatcherReplayLocalFaultHold,
} from "./replay-local-fault-bound.js";

/** A header held on a local replay fault whose backoff has not passed: it
 * is deferred without a classification. */
class WatcherReplayLocalFaultWaiting extends Error {
  constructor(readonly hold: WatcherReplayLocalFaultHold) {
    super(`${hold.reason}: ${hold.detail}`);
  }
}

/**
 * What the bridge does with one header it could not classify:
 * - `defer`: keep the classified prefix and retry this header and the rest of
 *   the queue on a later wake. Nothing is journaled.
 * - `skip`: record the header unverified and go on with the next one.
 * - `fail`: the existing fail-closed path.
 */
export type WatcherClassificationMiss = "defer" | "skip" | "fail";

/** Operator warning, once per header and kind, never once per attempt. */
export type WatcherClassificationWarning = Readonly<
  | {
      event: "classification_deferred";
      headerHash: string;
      cause:
        | "l1_source_unavailable"
        | "retained_da_transport_unavailable"
        | "retained_da_payload_unavailable"
        | "replay_local_fault"
        | typeof VALIDATION_TRACE_REPLAY_HELD
        | typeof VALIDATION_TRACE_REPLAY_STUCK;
    }
  | {
      event: "unverified_past_horizon";
      headerHash: string;
      /** The header whose public DA payload was not served. */
      missingPayloadHeaderHash: string;
      challengeableUntilMs: string;
    }
>;

/**
 * The header's challengeability horizon, from its node's block_time and the
 * DA store's own prune rule. `past_challengeability_horizon` is the reason
 * the committee drops a payload that is not held for another reason, so this
 * asks that same rule about this header alone.
 */
const challengeability = (
  header: WatcherStateQueueHeaderObservation,
  nowMs: bigint,
) => {
  const decision = daRetentionPruneDecision({
    nowMs: Number(nowMs),
    blockTimeMs: Number(
      Data.from(header.stateQueueNodeCborHex, StateQueueNode).block_time,
    ),
    headerStatus: "unobserved",
    queueReference: "none",
  });
  return {
    past: decision.reasonCode === "past_challengeability_horizon",
    challengeableUntilMs: decision.challengeableUntilMs,
  };
};

/** The end of `header`'s case open window; `undefined` when its node
 * cannot be read. */
const caseOpenWindowEndMs = (
  header: WatcherStateQueueHeaderObservation,
): number | undefined => {
  try {
    return validationTraceCaseOpenWindowEndMs(
      Data.from(header.stateQueueNodeCborHex, StateQueueNode).block_time,
    );
  } catch {
    return undefined;
  }
};

const MAXIMUM_WARNED_HEADERS = 4_096;

/**
 * A header whose classification waits on held script re-executions, or on a
 * local replay fault, names its hold on readiness while its classification
 * keeps being retried: each retry renews it, so one never retried again
 * (classified, merged, removed or rolled back) stops holding readiness on
 * its own once this old.
 */
export const WATCHER_REPLAY_HOLD_MAXIMUM_AGE_MS = 180_000;

type MissInput = Readonly<{
  error: unknown;
  observation: WatcherAuthenticatedStateQueueObservation;
  index: number;
  merged: ReadonlyMap<string, WatcherReleasedHeaderProof>;
  predecessor: WatcherStateQueueHeaderObservation | undefined;
  verification: ReturnType<typeof verificationRecorder>;
  subjectDigest: string | null;
}>;

export const classificationMissRecorder = (
  dependencies: Pick<
    BridgeDependencies,
    "operationsSink" | "nowMs" | "monotonicNowMs" | "warn"
  >,
) => {
  const pastHorizon = new Set<string>();
  const warned = new Set<string>();
  const warnOnce = (warning: WatcherClassificationWarning): void => {
    const key = `${warning.event}:${warning.headerHash}`;
    if (warned.has(key)) return;
    if (warned.size >= MAXIMUM_WARNED_HEADERS) warned.clear();
    warned.add(key);
    dependencies.warn?.(warning);
  };
  const nowMs = dependencies.nowMs ?? (() => BigInt(Date.now()));
  const localFaults = replayLocalFaultHolds(
    dependencies.monotonicNowMs ?? (() => performance.now()),
  );
  let localFaultPass = { token: -1, waiting: localFaults.pass() };
  const replayHolds = new Map<
    string,
    Readonly<{ reason: string; detail: string; atMs: bigint }>
  >();
  /**
   * The missing payload is now accounted for by this header's own record, so
   * its failed fetch stops holding readiness; the deferral stays visible.
   */
  const accountFor = (missingHeaderHash: string): void =>
    dependencies.operationsSink?.setAlert({
      code: "da_fetch_failure",
      subjectDigest: watcherDaFetchAlertSubject(missingHeaderHash),
      active: false,
      observedAtMs: nowMs().toString(),
    });
  const holdOnReadiness = (
    headerHash: string,
    hold: Readonly<{ reason: string; detail: string }>,
  ): void => {
    replayHolds.delete(headerHash);
    if (replayHolds.size >= MAXIMUM_WARNED_HEADERS)
      replayHolds.delete(replayHolds.keys().next().value!);
    replayHolds.set(headerHash, { ...hold, atMs: nowMs() });
  };
  const handle = (input: MissInput): WatcherClassificationMiss => {
    const { error, subjectDigest, verification } = input;
    const header = input.observation.finalizedHeaders[input.index]!;
    if (error instanceof WatcherReplayLocalFaultWaiting) return "defer";
    // The follower, the node or a provider did not answer, or moved while a
    // snapshot was read (see `isWatcherL1TransientFailure`); or this
    // watcher's own DA node is not up, so no source was asked. Either way
    // the read is incomplete, which says nothing about the header.
    if (isWatcherL1TransientFailure(error) || isFraudProofL1WaitError(error)) {
      if (subjectDigest !== null)
        verification.record(subjectDigest, "pending_l1");
      warnOnce({
        event: "classification_deferred",
        headerHash: header.headerHash,
        cause: "l1_source_unavailable",
      });
      return "defer";
    }
    // This watcher's own replay failed, which says nothing about the
    // header: it is held by name and classified again on a later wake once
    // its backoff passed, for as long as the fault recurs.
    if (isReplayLocalFault(error)) {
      warnOnce({
        event: "classification_deferred",
        headerHash: header.headerHash,
        cause: "replay_local_fault",
      });
      holdOnReadiness(
        header.headerHash,
        localFaults.fault(header.headerHash, error.message),
      );
      return "defer";
    }
    // Its script re-execution threw on an event that may be the one that
    // fails, and no earlier finding decides the block: not a verdict, so it
    // is classified again on a later wake, held by name meanwhile. Once the
    // case its finding would open is at risk (stage-unbuilt.ts), the hold
    // is an alarm, logged at error level.
    if (isValidationTraceReplayHeldError(error)) {
      const by = validationTraceCaseOpenAtRisk(
        caseOpenWindowEndMs(header),
        Number(nowMs()),
      );
      const hold = {
        reason: error.readiness,
        detail:
          by === undefined
            ? error.message
            : `${error.message}, and its case must open by ${by}`,
      };
      holdOnReadiness(header.headerHash, hold);
      if (by !== undefined)
        process.stderr.write(
          `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "error", ...hold })}\n`,
        );
      warnOnce({
        event: "classification_deferred",
        headerHash: header.headerHash,
        cause: error.readiness,
      });
      return "defer";
    }
    if (isWatcherRetainedDaTransportUnavailable(error)) {
      if (subjectDigest !== null)
        verification.record(subjectDigest, "pending_da");
      warnOnce({
        event: "classification_deferred",
        headerHash: header.headerHash,
        cause: "retained_da_transport_unavailable",
      });
      return "defer";
    }
    if (subjectDigest === null || !isRetainedDaPayloadUnavailableError(error))
      return "fail";
    const own = challengeability(header, nowMs());
    if (own.past) {
      // Nothing about this header can be challenged any more: the DA store's
      // horizon is 1.5 x maturity, and the state queue refuses every removal
      // after maturity. Waiting, even on a source that did not answer, cannot
      // change that, and it would hold every later header behind this one.
      // An in-window miss never reaches this arm. A predecessor's payload
      // needs no separate horizon check: commit times only grow along the queue.
      pastHorizon.add(header.headerHash);
      verification.recordPastHorizon(subjectDigest);
      accountFor(error.headerHash);
      warnOnce({
        event: "unverified_past_horizon",
        headerHash: header.headerHash,
        missingPayloadHeaderHash: error.headerHash,
        challengeableUntilMs: own.challengeableUntilMs.toString(),
      });
      return "skip";
    }
    // An unreachable source said nothing about the payload: always wait.
    if (
      error.availability !== "unreachable" &&
      !awaitsRetainedDaPayload(
        error,
        input.observation,
        input.index,
        input.merged,
      )
    )
      return "fail";
    verification.record(subjectDigest, "pending_da");
    accountFor(error.headerHash);
    warnOnce({
      event: "classification_deferred",
      headerHash: header.headerHash,
      cause: "retained_da_payload_unavailable",
    });
    return "defer";
  };
  return Object.freeze({
    /**
     * Throws, for `resolve` to defer, while `headerHash` is held on a local
     * replay fault whose backoff has not passed, or whose retry this pass
     * (`token`) already gave another header.
     */
    assertDue: (headerHash: string, token: number): void => {
      if (localFaultPass.token !== token)
        localFaultPass = { token, waiting: localFaults.pass() };
      const hold = localFaultPass.waiting(headerHash);
      if (hold !== undefined) throw new WatcherReplayLocalFaultWaiting(hold);
    },
    /** `headerHash` was classified: it no longer waits on a hold, and its
     * local-fault hold is released. */
    classified: (headerHash: string): void => {
      replayHolds.delete(headerHash);
      localFaults.progressed(headerHash);
    },
    /** The headers whose classification waits on held re-executions or a
     * local replay fault. */
    readiness: (): readonly Readonly<{ reason: string; detail: string }>[] => {
      const now = nowMs();
      for (const [headerHash, { atMs }] of replayHolds)
        if (now - atMs > BigInt(WATCHER_REPLAY_HOLD_MAXIMUM_AGE_MS))
          replayHolds.delete(headerHash);
      return Object.freeze(
        [...replayHolds.values()].map(({ reason, detail }) =>
          Object.freeze({ reason, detail }),
        ),
      );
    },
    /**
     * A header already skipped past its horizon stays skipped: time only
     * moves it further past, so its DA is not read again.
     */
    skippedPastHorizon: (headerHash: string): boolean =>
      pastHorizon.has(headerHash),
    /**
     * Resolves one classification failure. A failure keeps its fail-closed
     * path and is recorded `failed`; a diagnostics failure fails it too.
     */
    resolve: (
      input: MissInput,
    ):
      | Readonly<{ outcome: "defer" | "skip" }>
      | Readonly<{ outcome: "fail"; failure: unknown }> => {
      let outcome: WatcherClassificationMiss;
      try {
        outcome = handle(input);
      } catch (diagnosticError) {
        return { outcome: "fail", failure: diagnosticError };
      }
      if (outcome !== "fail") return { outcome };
      if (input.subjectDigest === null)
        return { outcome, failure: input.error };
      try {
        input.verification.record(input.subjectDigest, "failed");
      } catch (diagnosticError) {
        return {
          outcome,
          failure: new AggregateError(
            [input.error, diagnosticError],
            "fault classification and failure diagnostics failed",
            { cause: input.error },
          ),
        };
      }
      return { outcome, failure: input.error };
    },
  });
};
