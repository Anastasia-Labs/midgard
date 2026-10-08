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
import { ingestAtFollowerView } from "./l1-follower.recovery.js";
import {
  clearLivenessIncident,
  HISTORY_CORRECTION_REWIND_SOURCE,
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
      | Effect.Effect.Error<
          ReturnType<typeof prepareStateQueueCorrectionRewind>
        >
      | Effect.Effect.Error<ReturnType<typeof prepareLandedBlockRebase>>,
      | R
      | SqlClient.SqlClient
      | Effect.Effect.Context<
          ReturnType<typeof prepareStateQueueCorrectionRewind>
        >
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
            // must be applied before another can be prepared.
            (rewindAuthority === undefined
              ? Effect.succeed(undefined)
              : prepareStateQueueCorrectionRewind({
                  bindingDigest: binding.digest,
                  checkpoint,
                  preparation,
                  config,
                  authority: rewindAuthority,
                })
            ).pipe(
              // A correction rewind held on the native owner's state still
              // owns the removed local suffix and that root: the landed-block
              // rebase waits for the next pass instead of moving a root a
              // held recovery owns. Otherwise the rebase follows the landed
              // blocks, disposing of the own journals that cannot land and
              // reviving the abandoned ones that landed (whichever lands
              // wins).
              Effect.flatMap((rewind) =>
                rewind === CORRECTION_REWIND_HELD_ON_NATIVE_STATE
                  ? Effect.void
                  : prepareLandedBlockRebase(preparation),
              ),
            ),
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
