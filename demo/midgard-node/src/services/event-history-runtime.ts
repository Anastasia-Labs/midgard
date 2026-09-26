import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import { pendingHistoryLedgerDisposition } from "../database/eventHistoryLedgerRepair.js";
import { materializeCanonicalHistory } from "../database/eventHistoryMaterialization.js";
import { DatabaseError } from "../database/utils/common.js";
import { reconcileDepositProjection } from "../fibers/project-deposits-to-mempool-ledger.js";
import { makeEventHistorySourceBinding } from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import { NodeConfig } from "./config.js";
import { makeEventHistoryOwner } from "./event-history-owner.js";
import { HistoryPreparation } from "./event-history-recovery.js";
import { prepareSignedHeaderRecovery } from "./history-signed-header-recovery.js";
import { Lucid } from "./lucid.js";
import { MempoolLedgerCache } from "./mempool-ledger-cache.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "./midgard-contracts.js";
import {
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
      ogmiosUrl: input.transport.ogmiosUrl,
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
    const prepareCompletion = input.prepareCompletion;
    // Architecture G is the only engine whose ledger root is a promoted native
    // owner: a correction that removes a committed block rewinds it through
    // this owner's recovery. Other engines keep the correction fiber's plain
    // reinclusion producer.
    const rewindAuthority =
      config.MPF_ENGINE === "architecture_g" &&
      identity.manifestId !== undefined &&
      identity.manifest !== undefined
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
        >,
      | R
      | SqlClient.SqlClient
      | Effect.Effect.Context<ReturnType<typeof prepareSignedHeaderRecovery>>
      | Effect.Effect.Context<
          ReturnType<typeof prepareStateQueueCorrectionRewind>
        >
    >({
      ...input,
      prepareCompletion:
        prepareCompletion === undefined
          ? undefined
          : (checkpoint, preparation) =>
              prepareCompletion(checkpoint, preparation).pipe(
                Effect.provideService(HistoryPreparation, preparation),
              ),
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
              ? Effect.void
              : prepareStateQueueCorrectionRewind({
                  bindingDigest: binding.digest,
                  checkpoint,
                  preparation,
                  config,
                  authority: rewindAuthority,
                })
            ).pipe(
              Effect.zipRight(
                prepareSignedHeaderRecovery({
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
          const pending = yield* pendingHistoryLedgerDisposition(change);
          if (pending !== undefined) return pending;
          if (rewindAuthority !== undefined) {
            const rewind =
              yield* stateQueueCorrectionRewindDisposition(rewindAuthority);
            if (rewind !== undefined) return rewind;
          }
          yield* materializeCanonicalHistory(change, config.NETWORK);
          const { reconciled } = yield* reconcileDepositProjection(
            new Date(lucid.api.slotToUnixTime(change.after.head.slot)),
          );
          // Newly projected deposits stay hidden from the validation cache
          // until a header is assigned. Restoring an already spendable row
          // changes cache state, which only a recovery's reload may publish;
          // inside a Ready append this closes the gate instead.
          const owned = yield* Authority.currentOwnedTransaction;
          if (
            reconciled.spendableUpserts.length > 0 &&
            Option.isSome(owned) &&
            owned.value.state === "ready"
          )
            return {
              status: "pending" as const,
              reason:
                "Deposit projection restored spendable ledger rows; the validation cache must reload",
            };
          return undefined;
        }),
    });
  });
