import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

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
    return yield* makeEventHistoryOwner<
      | E
      | DatabaseError
      | Effect.Effect.Error<ReturnType<typeof prepareSignedHeaderRecovery>>,
      | R
      | SqlClient.SqlClient
      | Effect.Effect.Context<ReturnType<typeof prepareSignedHeaderRecovery>>
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
          : prepareSignedHeaderRecovery({
              binding,
              checkpoint,
              preparation,
              transport: input.transport,
              contracts,
              config,
              confirmationDepth: identity.manifest.l1Finality.confirmationDepth,
              slotToUnixTime: lucid.api.slotToUnixTime,
            }),
      binding,
      histories,
      cache,
      drainBeforeRepair: writeBehind.flushNow,
      slotToUnixTime: lucid.api.slotToUnixTime,
      expectedInitializationTransactionHash:
        identity.manifest?.steps.initProtocol.txHash,
      reconcile: (change) =>
        Effect.gen(function* () {
          const pending = yield* pendingHistoryLedgerDisposition(change);
          if (pending !== undefined) return pending;
          yield* materializeCanonicalHistory(change, config.NETWORK);
          yield* reconcileDepositProjection(
            new Date(lucid.api.slotToUnixTime(change.after.head.slot)),
          );
          return undefined;
        }),
    });
  });
