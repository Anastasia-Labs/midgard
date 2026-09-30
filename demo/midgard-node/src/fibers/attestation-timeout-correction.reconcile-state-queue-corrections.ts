import {
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionJournalStore,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, type Either, Ref, Runtime } from "effect";

import { ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD } from "../commands/readiness.js";
import { DaPayloadTerminalOutcomesDB } from "../database/index.js";
import {
  type AttestationTimeoutObservation,
  observeAttestationTimeoutQueue,
} from "../services/attestation-timeout-observation.js";
import { type AttestationTimeoutCorrectionHealth } from "../services/globals.js";
import {
  authorizeStateQueueCorrectionReinclusion,
  createDatabaseStateQueueCorrectionObserverStore,
  Database,
  Globals,
  reconcileStateQueueCorrectionObserver,
  refuseRewoundStateQueueCorrectionRollback,
  type StateQueueCorrectionObserverResult,
  type StateQueueCorrectionObserverSource,
} from "../services/index.js";

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
 * that failure ahead of the tick's earlier work. */
export const observeAndRecordAttestationTimeoutQueue = (
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
  queue: readonly SDK.StateQueueUTxO[],
  nowMs: number,
): Effect.Effect<
  Either.Either<AttestationTimeoutObservation, SDK.DataCoercionError>
> =>
  observeAttestationTimeoutQueue(
    queue,
    BigInt(nowMs),
    ATTESTATION_TIMEOUT_ALERT_LEAD_MS,
  ).pipe(
    Effect.tap((observation) =>
      Ref.update(health, (current) => ({
        ...current,
        lastQueueReadAtMs: nowMs,
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
 * Admits authenticated state-queue corrections into the durable observer.
 *
 * A removed block this node committed has already moved the native ledger
 * root, so its reinclusion is a rewind, not a forward write: the fiber only
 * admits the correction (the observer persists it) and the history owner's
 * recovery rewinds the native root and reincludes the payloads (see
 * state-queue-correction-rewind). The admission needs no producer, so a gate
 * the owner closed for an earlier removal of the same suffix never blocks
 * admitting the later one. The native rewind has no inverse, so a post-finality
 * rollback of a rewound removal is refused as an integrity failure.
 */
export const reconcileStateQueueCorrections = ({
  source,
  deploymentIdentityDigest,
  stateQueuePolicyId,
  requiredFinalityDepth,
  deploymentManifest,
}: {
  readonly source: StateQueueCorrectionObserverSource;
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly requiredFinalityDepth: bigint;
  readonly deploymentManifest: unknown;
}): Effect.Effect<
  StateQueueCorrectionObserverResult,
  unknown,
  Database | Globals
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const run = Runtime.runPromise(yield* Effect.runtime<Database | Globals>());
    const authority = {
      expectedDeploymentIdentityDigest: deploymentIdentityDigest,
      requiredFinalityDepth,
    };
    return yield* Effect.tryPromise({
      try: () =>
        reconcileStateQueueCorrectionObserver({
          deploymentIdentityDigest,
          stateQueuePolicyId,
          requiredFinalityDepth,
          source,
          store: createDatabaseStateQueueCorrectionObserverStore({
            sql,
            deploymentManifest,
          }),
          reinclude: async (transition) => {
            // Refuse an unauthorized transition before the observer admits it.
            authorizeStateQueueCorrectionReinclusion(transition, authority);
          },
          // Refused before the terminal outcome is revoked, so the DA
          // 'removed' authority of a rewound block survives the refusal.
          assertRollbackPermitted: async (transition) => {
            await run(
              refuseRewoundStateQueueCorrectionRollback(transition, authority),
            );
          },
          restoreAfterRollback: async (transition) => {
            // The native rewind has no inverse: a rolled-back removal whose
            // rewind ran is an explicit integrity failure, and one whose
            // rewind never ran left nothing to restore.
            await run(
              refuseRewoundStateQueueCorrectionRollback(transition, authority),
            );
          },
          revokeTerminal: async (transition) => {
            await run(
              DaPayloadTerminalOutcomesDB.revokeAuthenticatedTransition(
                transition,
                deploymentManifest,
              ),
            );
          },
        }),
      catch: (cause) => cause,
    });
  });
