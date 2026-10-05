import { daRetentionPruneDecision } from "@al-ft/midgard-core";
import { isRetainedDaPayloadUnavailableError } from "@al-ft/midgard-fault-proofs";
import { Header } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
  WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import { watcherDaFetchAlertSubject } from "../runtime/operations-observability.alert-book.js";
import { isWatcherRetainedDaTransportUnavailable } from "../storage/retained-da-transport-unavailable.js";
import {
  awaitsRetainedDaPayload,
  verificationRecorder,
} from "./fault-decision-bridge.classification-record.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";

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
        | "retained_da_payload_unavailable";
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
 * The header's challengeability horizon, from its own block end time and the
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
    blockEndTimeMs: Number(Data.from(header.headerCborHex, Header).endTime),
    headerStatus: "unobserved",
    queueReference: "none",
  });
  return {
    past: decision.reasonCode === "past_challengeability_horizon",
    challengeableUntilMs: decision.challengeableUntilMs,
  };
};

const MAXIMUM_WARNED_HEADERS = 4_096;

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
  const handle = (input: MissInput): WatcherClassificationMiss => {
    const { error, subjectDigest, verification } = input;
    const header = input.observation.finalizedHeaders[input.index]!;
    // Kupo, Ogmios, the node or a provider did not answer, or moved while a
    // snapshot was read (see `isWatcherL1TransientFailure`); or this
    // watcher's own DA node is not up, so no source was asked. Either way
    // the read is incomplete, which says nothing about the header.
    if (isWatcherL1TransientFailure(error)) {
      if (subjectDigest !== null)
        verification.record(subjectDigest, "pending_l1");
      warnOnce({
        event: "classification_deferred",
        headerHash: header.headerHash,
        cause: "l1_source_unavailable",
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
      // needs no separate horizon check: end times only grow along the queue.
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

/** Production retry spacing for a deferred suffix: 1 s doubling to 60 s. */
export const watcherDeferredRetryDelayMs = (consecutive: number): number =>
  Math.min(60_000, 1_000 * 2 ** Math.min(Math.max(consecutive - 1, 0), 6));

/**
 * Bounds how often a deferred suffix is retried. Every canonical wake asks;
 * only a wake after the current delay retries, and the delay grows with each
 * retry that defers again. A new queue observation always classifies.
 */
export const deferredRetryBackoff = (
  dependencies: Pick<
    BridgeDependencies,
    "deferredRetryDelayMs" | "monotonicNowMs"
  >,
) => {
  const now = dependencies.monotonicNowMs ?? (() => performance.now());
  let consecutive = 0;
  let notBeforeMs = Number.NEGATIVE_INFINITY;
  return Object.freeze({
    settle: (deferred: boolean): void => {
      consecutive = deferred ? consecutive + 1 : 0;
      notBeforeMs = deferred
        ? now() + (dependencies.deferredRetryDelayMs?.(consecutive) ?? 0)
        : Number.NEGATIVE_INFINITY;
    },
    due: (): boolean => now() >= notBeforeMs,
    reset: (): void => {
      consecutive = 0;
      notBeforeMs = Number.NEGATIVE_INFINITY;
    },
  });
};
