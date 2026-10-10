import {
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionJournalStore,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, type Either, Ref } from "effect";

import { ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD } from "../commands/readiness.js";
import {
  type AttestationTimeoutObservation,
  observeAttestationTimeoutQueue,
} from "../services/attestation-timeout-observation.js";
import { type AttestationTimeoutCorrectionHealth } from "../services/globals.js";
import {
  type IntentJournalService,
  type IntentPlan,
  journaledIntent,
} from "../services/intent-journal.js";

export const ATTESTATION_TIMEOUT_ALERT_LEAD_MS =
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;

export const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

/**
 * How long readiness lets the correction go without progress, and the state
 * queue go unread, before it reports the node unready. `tickIntervalMs` is the
 * fiber's schedule interval.
 *
 * Stall: between two progress marks a healthy step waits on at most one
 * removal transaction. Its validity range starts no later than it is built
 * and spans STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS, so within that window it
 * either lands or can never land. The bound takes the profile's
 * MAX_VALIDITY_RANGE_LENGTH_MS (the protocol's cap on any validity range, 8
 * minutes in every shipped profile) where it is the longer, and its surplus
 * over the removal window covers building, submitting and confirmation
 * polling. A further failure-threshold of tick intervals covers the wait for
 * the tick that starts the step and provider indexing lag.
 *
 * Queue unknown: a commit's header end time equals its transaction's validity
 * upper bound (commit_bound_header_time_is_valid), which is at or after the
 * moment it lands, so a header the last read did not see comes due no sooner
 * than DA_ATTESTATION_TIMEOUT_MS after that read. Shorter outages are L1 blips
 * that hide nothing due. The bound is never below the stall bound, because a
 * step waiting on a removal records no queue read while it waits.
 */
export const attestationTimeoutCorrectionReadinessBounds = (
  tickIntervalMs: number,
  profile: {
    readonly maxValidityRangeMs: bigint;
    readonly daAttestationTimeoutMs: bigint;
  } = {
    maxValidityRangeMs: SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
    daAttestationTimeoutMs: SDK.DA_ATTESTATION_TIMEOUT_MS,
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
  queue: readonly SDK.StateQueueUTxO[],
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

/** Credits a saved correction journal as progress when it starts a correction
 * or confirms a removal not yet credited. Re-saving an unchanged journal, or
 * resubmitting a removal that never lands, is not progress. */
export const recordTimeoutCorrectionJournalProgress = (
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
  journal: TimeoutCorrectionJournal,
  nowMs: number,
): Effect.Effect<void> =>
  Ref.update(health, (current) => {
    const confirmedRemovals = journal.steps.filter(
      (step) => step.status === "confirmed",
    ).length;
    const credited = current.correctionProgress;
    return credited !== null &&
      credited.targetHeaderHash === journal.targetHeaderHash &&
      credited.confirmedRemovals >= confirmedRemovals
      ? current
      : {
          ...current,
          lastProgressAtMs: nowMs,
          correctionProgress: {
            targetHeaderHash: journal.targetHeaderHash,
            confirmedRemovals,
          },
        };
  });

/** The journal store the correction writes through, crediting each save that
 * moves the correction forward, so readiness sees a step that is pruning
 * several descendants as progressing rather than stalled. */
export const withTimeoutCorrectionProgress = (
  store: TimeoutCorrectionJournalStore,
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
): TimeoutCorrectionJournalStore => ({
  ...store,
  save: async (journal) => {
    await store.save(journal);
    Effect.runSync(
      recordTimeoutCorrectionJournalProgress(health, journal, Date.now()),
    );
  },
});

/**
 * The journal store with the intent journal in front (§8.2, family
 * `correction`). A step is journaled when the workflow decides to send it:
 * the save that holds a `prepared` step whose bytes the journal last read
 * or saved did not hold at that index (an appended step, or a replacement
 * moved to the end). A step reopened in place by a rollback (`confirmed` or
 * `superseded` back to `prepared`, same bytes) is not a send decision and
 * is not journaled; its bytes were journaled when first prepared.
 * Recording precedes the save, so a refusal fails it and nothing is sent.
 * The record takes S6's send decision under the pass's `plan` (§8.1): a
 * held one (`IntentSubmitHeld`) fails the save too, so the workflow sends
 * nothing and S6's reconciler decides the journaled step.
 *
 * Key: `correction:<target header>:<kind>:<removed header>`; the content
 * reference is the removed header.
 */
export const withCorrectionIntentJournal = (
  store: TimeoutCorrectionJournalStore,
  journal: IntentJournalService,
  pass: Readonly<{
    /** The workflow pass's plan, opened before its first L1 read. */
    plan: IntentPlan;
    slotTime: (slot: number) => number;
  }>,
): TimeoutCorrectionJournalStore => {
  let last: TimeoutCorrectionJournal | undefined;
  const decided = (
    next: TimeoutCorrectionJournal,
  ): TimeoutCorrectionJournal["steps"] =>
    next.steps.filter((step, index) => {
      if (step.status !== "prepared") return false;
      const before = last?.steps[index];
      return before === undefined || before.txHash !== step.txHash;
    });
  return {
    ...store,
    load: async () => {
      last = await store.load();
      return last;
    },
    save: async (next) => {
      for (const step of decided(next))
        await Effect.runPromise(
          journal.record(
            journaledIntent(
              "correction",
              `correction:${next.targetHeaderHash}:${step.kind}:${step.removedHeaderHash}`,
              pass.plan,
              Buffer.from(step.removedHeaderHash, "hex"),
            ),
            step.signedCbor,
            step.txHash,
            { kind: "send", slotTime: pass.slotTime },
          ),
        );
      await store.save(next);
      last = next;
    },
  };
};
