import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  pendingHistoryLedgerDisposition,
  repairUnpublishedHistoryLedger,
} from "../database/eventHistoryLedgerRepair.js";
import { DatabaseError } from "../database/utils/common.js";
import { makeEventHistorySourceBinding } from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import {
  landedBlockRebaseDisposition,
  prepareLandedBlockRebase,
} from "../landed-blocks/index.js";
import { NodeConfig } from "./config.js";
import { makeEventHistoryOwner } from "./event-history-owner.js";
import { HistoryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.globals.js";
import {
  historyRecoveryPass,
  NATIVE_RESTORE_HELD,
} from "./history-dependent-recovery.js";
import {
  expiredIntentReleaseDisposition,
  makeSignedIntentDeferral,
  prepareExpiredIntentRelease,
  prepareReplacedBlockRevival,
  replacedBlockRevivalDisposition,
} from "./history-expired-intent-release.js";
import {
  activeSignedIntent,
  deferralKey,
} from "./history-expired-intent-release.table.js";
import { prepareSignedHeaderRecovery } from "./history-signed-header-recovery.js";
import { ingestAtFollowerView } from "./l1-follower.recovery.js";
import {
  clearLivenessIncident,
  HISTORY_CORRECTION_REWIND_SOURCE,
  HISTORY_SIGNED_INTENT_RELEASE_SOURCE,
} from "./liveness-halt.js";
import { Lucid } from "./lucid.js";
import { MempoolLedgerCache } from "./mempool-ledger-cache.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "./midgard-contracts.js";
import {
  CORRECTION_REWIND_HELD_ON_NATIVE_STATE,
  prepareStateQueueCorrectionRewind,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.js";
import { WriteBehind } from "./write-behind.js";

type OwnerOptions<E, R> = Parameters<typeof makeEventHistoryOwner<E, R>>[0];

/** The active signed intent the release disposition last saw. */
export type ReleaseIncidentJournal = { current: string | undefined };

/**
 * `expiredIntentReleaseDisposition`, clearing the undecided-release incident
 * (`signed_intent_undecided`, which only `decide` raises) once its condition
 * no longer holds: when no release is in question (no active signed intent,
 * one that can still land, or a deferral), and when the active journal
 * changes, since the incident was raised for the one before. Only `decide`
 * clears it otherwise, and `decide` runs only while a release is in question.
 */
export const expiredIntentReleaseClearingIncident = (
  input: Parameters<typeof expiredIntentReleaseDisposition>[0],
  journal: ReleaseIncidentJournal,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const intent = yield* activeSignedIntent;
    const key = intent === undefined ? undefined : deferralKey(intent);
    if (key !== journal.current) {
      journal.current = key;
      yield* clearLivenessIncident(
        globals,
        HISTORY_SIGNED_INTENT_RELEASE_SOURCE,
      );
    }
    const release = yield* expiredIntentReleaseDisposition(input);
    if (release === undefined)
      yield* clearLivenessIncident(
        globals,
        HISTORY_SIGNED_INTENT_RELEASE_SOURCE,
      );
    return release;
  });

/** Production composition shared by listen and acceptance. Only source IO and
 * recovery preparation are injected; journal materialization and deposit
 * projection always use the same production SQL writers and cache instance.
 */
export const makeProductionEventHistoryOwner = <E = never, R = never>(input: {
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly expectedGenesisLosslessSha256: string;
  readonly heartbeatIntervalMs: number;
  readonly retainedPointLimit: number;
  readonly maximumReceiptBytes: number;
  readonly leaseDurationMs: number;
  readonly prepareCompletion?: OwnerOptions<E, R>["prepareCompletion"];
}) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    const lucid = yield* Lucid;
    const cache = yield* MempoolLedgerCache;
    const writeBehind = yield* WriteBehind;
    const binding = yield* makeEventHistorySourceBinding({
      contracts,
      identity,
      network: config.NETWORK,
      expectedGenesisLosslessSha256: input.expectedGenesisLosslessSha256,
    });
    const histories = yield* Effect.try({
      try: () => SDK.requireEventHistoryContracts(contracts),
      catch: (cause) =>
        new DatabaseError({
          table: "event_history_cursor",
          message: "History deployment is missing",
          cause,
        }),
    });
    // A signed intent whose base a correction removed defers to the
    // correction path; this runtime remembers it until a rollback. A replaced
    // block without evidence it landed is re-read at the next source point.
    const signedIntentDeferral = makeSignedIntentDeferral();
    const releaseIncidentJournal: ReleaseIncidentJournal = {
      current: undefined,
    };
    // The ledger root is a promoted native owner: a correction that removes a
    // committed block rewinds it through this owner's recovery.
    const rewindAuthority =
      identity.manifestId !== undefined && identity.manifest !== undefined
        ? {
            manifestId: identity.manifestId,
            stateQueuePolicyId: contracts.stateQueue.policyId,
            requiredFinalityDepth: BigInt(
              identity.manifest.l1Finality.confirmationDepth,
            ),
          }
        : undefined;
    return yield* makeEventHistoryOwner<
      | E
      | DatabaseError
      | Effect.Effect.Error<ReturnType<typeof prepareSignedHeaderRecovery>>
      | Effect.Effect.Error<
          ReturnType<typeof prepareStateQueueCorrectionRewind>
        >
      | Effect.Effect.Error<ReturnType<typeof prepareExpiredIntentRelease>>
      | Effect.Effect.Error<ReturnType<typeof prepareReplacedBlockRevival>>
      | Effect.Effect.Error<ReturnType<typeof prepareLandedBlockRebase>>,
      | R
      | SqlClient.SqlClient
      | Effect.Effect.Context<ReturnType<typeof prepareSignedHeaderRecovery>>
      | Effect.Effect.Context<
          ReturnType<typeof prepareStateQueueCorrectionRewind>
        >
      | Effect.Effect.Context<ReturnType<typeof prepareExpiredIntentRelease>>
      | Effect.Effect.Context<ReturnType<typeof prepareReplacedBlockRevival>>
      | Effect.Effect.Context<ReturnType<typeof prepareLandedBlockRebase>>
    >({
      ...input,
      prepareCompletion: (checkpoint, preparation) =>
        (
          input.prepareCompletion?.(checkpoint, preparation) ?? Effect.void
        ).pipe(Effect.provideService(HistoryPreparation, preparation)),
      preparePendingReconciliation: (checkpoint, preparation) =>
        identity.manifest === undefined
          ? Effect.fail(
              new DatabaseError({
                table: "event_history_recovery_plans",
                message:
                  "Recovery requires the bound deployment finality profile",
                cause: undefined,
              }),
            )
          : // A retained or owed correction rewind runs first: it resolves
            // the removed blocks' journals, and a prepared plan of either kind
            // must be applied before another can be prepared. A recovery held
            // on the native owner's state (the rewind, or a signed-header,
            // release or revival held on its native restore) still owns the
            // native root, so the landed-block rebase waits for the next pass
            // instead of moving a root a held recovery owns.
            historyRecoveryPass({
              correctionRewind: (rewindAuthority === undefined
                ? Effect.succeed(undefined)
                : prepareStateQueueCorrectionRewind({
                    bindingDigest: binding.digest,
                    checkpoint,
                    preparation,
                    config,
                    authority: rewindAuthority,
                  })
              ).pipe(
                Effect.map((rewind) =>
                  rewind === CORRECTION_REWIND_HELD_ON_NATIVE_STATE
                    ? NATIVE_RESTORE_HELD
                    : rewind,
                ),
              ),
              signedHeaderRecovery: prepareSignedHeaderRecovery({
                binding,
                checkpoint,
                preparation,
                transport: input.transport,
                contracts,
                config,
                confirmationDepth:
                  identity.manifest.l1Finality.confirmationDepth,
                slotToUnixTime: lucid.api.slotToUnixTime,
              }),
              // A signed commit past its TTL, or once the journaled history
              // shows its base output spent, is reconciled to whichever block
              // holds its base's state-queue slot: confirmed, replaced (members
              // reopened, Architecture G native root restored) or, when an
              // earlier replaced block of this node won, revived.
              expiredIntentRelease:
                rewindAuthority === undefined
                  ? Effect.void
                  : prepareExpiredIntentRelease({
                      binding,
                      checkpoint,
                      preparation,
                      config,
                      rewindAuthority,
                      transport: input.transport,
                      contracts,
                      deferral: signedIntentDeferral,
                    }),
              // With no journal active, a replaced block of this node that
              // holds its base's slot after all (it landed late, or a rollback
              // brought it back) is revived.
              replacedBlockRevival:
                rewindAuthority === undefined
                  ? Effect.void
                  : prepareReplacedBlockRevival({
                      binding,
                      checkpoint,
                      preparation,
                      config,
                      rewindAuthority,
                      transport: input.transport,
                      contracts,
                      deferral: signedIntentDeferral,
                    }),
              landedBlockRebase: prepareLandedBlockRebase(preparation),
            }),
      binding,
      histories,
      cache,
      rollbackHorizon: (
        identity.manifest?.l1Finality ?? DEPLOYMENT_MANIFEST_L1_FINALITY
      ).automaticRecoveryMaxDepth,
      drainBeforeRepair: writeBehind.flushNow,
      slotToUnixTime: lucid.api.slotToUnixTime,
      expectedInitializationTransactionHash:
        identity.manifest?.steps.initProtocol.txHash,
      reconcile: (change) =>
        Effect.gen(function* () {
          const rebase = yield* landedBlockRebaseDisposition;
          if (rebase !== undefined) return rebase;
          const pending = yield* pendingHistoryLedgerDisposition(change);
          if (pending !== undefined) return pending;
          if (rewindAuthority !== undefined) {
            const rewind =
              yield* stateQueueCorrectionRewindDisposition(rewindAuthority);
            if (rewind !== undefined) return rewind;
            // No admitted removal leaves a local journal unresolved, so no
            // rewind evaluation will run to clear its held reason.
            yield* clearLivenessIncident(
              yield* Globals,
              HISTORY_CORRECTION_REWIND_SOURCE,
            );
            const release = yield* expiredIntentReleaseClearingIncident(
              {
                binding,
                change,
                deferral: signedIntentDeferral,
                rewindAuthority,
              },
              releaseIncidentJournal,
            );
            if (release !== undefined) return release;
            const revival = yield* replacedBlockRevivalDisposition({
              change,
              deferral: signedIntentDeferral,
              rewindAuthority,
            });
            if (revival !== undefined) return revival;
          }
          // The follower-change driver writes the event rows (E-N1-2
          // ruling 1); the owner's reconcile repairs orphans and, in a
          // recovery, ingests at the follower's view.
          return yield* ingestAtFollowerView({
            change,
            repair: repairUnpublishedHistoryLedger(change),
            network: config.NETWORK,
            slotToUnixTime: lucid.api.slotToUnixTime,
          });
        }),
    });
  });
