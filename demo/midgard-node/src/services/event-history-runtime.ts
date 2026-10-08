import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import type { SqlError } from "@effect/sql/SqlError";
import { Effect } from "effect";

import {
  pendingHistoryLedgerDisposition,
  repairUnpublishedHistoryLedger,
} from "../database/eventHistoryLedgerRepair.js";
import { DatabaseError } from "../database/utils/common.js";
import { makeEventHistorySourceBinding } from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import { NodeConfig } from "./config.js";
import { makeEventHistoryOwner } from "./event-history-owner.js";
import { HistoryPreparation } from "./event-history-recovery.js";
import type { Globals } from "./globals.globals.js";
import { ingestAtFollowerView } from "./l1-follower.recovery.js";
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
    return yield* makeEventHistoryOwner<
      E | DatabaseError | SqlError,
      R | SqlClient.SqlClient | Globals
    >({
      ...input,
      prepareCompletion: (checkpoint, preparation) =>
        (
          input.prepareCompletion?.(checkpoint, preparation) ?? Effect.void
        ).pipe(Effect.provideService(HistoryPreparation, preparation)),
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
