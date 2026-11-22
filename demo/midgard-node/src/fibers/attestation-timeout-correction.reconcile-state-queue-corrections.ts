import {
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
} from "@al-ft/midgard-fault-proofs/remove-fraudulent-block";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { Effect, type Either, Ref } from "effect";

import { ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD } from "../commands/readiness.js";
import {
  type AttestationTimeoutObservation,
  observeAttestationTimeoutQueue,
} from "../services/attestation-timeout-observation.js";
import { type AttestationTimeoutCorrectionHealth } from "../services/globals.js";

export const ATTESTATION_TIMEOUT_ALERT_LEAD_MS =
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;

export const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

/**
 * How long readiness lets the correction go without progress, and the state
 * queue go unread, before it reports the node unready. `tickIntervalMs` is the
 * fiber's schedule interval.
 *
 * Stall: a removal a step submits has a validity range that starts no later
 * than it is built and spans STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS, so
 * within that window it either lands or can never land, and a later step
 * builds the next one from the chain. The bound takes the profile's
 * MAX_VALIDITY_RANGE_LENGTH_MS (the protocol's cap on any validity range, 8
 * minutes in every shipped profile) where it is the longer. A further
 * failure-threshold of tick intervals covers the wait for the next tick and
 * provider indexing lag.
 *
 * Queue unknown: a commit's header end time equals its transaction's validity
 * upper bound (commit_bound_header_time_is_valid), which is at or after the
 * moment it lands, so a header the last read did not see comes due no sooner
 * than DA_ATTESTATION_TIMEOUT_MS after that read. Shorter outages are L1 blips
 * that hide nothing due. The bound is never below the stall bound.
 */
export const attestationTimeoutCorrectionReadinessBounds = (
  tickIntervalMs: number,
  profile: {
    readonly maxValidityRangeMs: bigint;
    readonly daAttestationTimeoutMs: bigint;
  } = {
    maxValidityRangeMs: SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
    daAttestationTimeoutMs: SDKStateQueue.DA_ATTESTATION_TIMEOUT_MS,
  },
): { readonly stallBoundMs: number; readonly queueUnknownBoundMs: number } => {
  const removalWaitMs =
    profile.maxValidityRangeMs > STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS
      ? profile.maxValidityRangeMs
      : STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS;
  const stallBoundMs =
    Number(removalWaitMs) +
    ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD * tickIntervalMs;
  return {
    stallBoundMs,
    queueUnknownBoundMs: Math.max(
      Number(profile.daAttestationTimeoutMs),
      stallBoundMs,
    ),
  };
};

/** Classifies the state queue once for the tick and records the result for
 * readiness. A classification failure is returned rather than raised, so the
 * tick raises it where it uses the classification and recording never moves
 * that failure ahead of the tick's earlier work. The deadline is judged
 * against `l1NowMs`, the L1 `slotNow` (plan §3.6); `readAtMs` is the local
 * clock reading that readiness measures queue freshness with. */
export const observeAndRecordAttestationTimeoutQueue = (
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
  queue: readonly SDKStateQueue.StateQueueUTxO[],
  times: { readonly l1NowMs: number; readonly readAtMs: number },
): Effect.Effect<
  Either.Either<AttestationTimeoutObservation, SDK.DataCoercionError>
> =>
  observeAttestationTimeoutQueue(
    queue,
    BigInt(times.l1NowMs),
    ATTESTATION_TIMEOUT_ALERT_LEAD_MS,
  ).pipe(
    Effect.tap((observation) =>
      Ref.update(health, (current) => ({
        ...current,
        lastQueueReadAtMs: times.readAtMs,
        oldestUnattestedHeader:
          "headerHash" in observation
            ? {
                headerHash: observation.headerHash,
                deadlineMs: Number(observation.deadlineMs),
              }
            : null,
      })),
    ),
    Effect.either,
  );
