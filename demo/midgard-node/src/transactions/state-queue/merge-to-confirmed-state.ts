/**
 * State-queue merge transaction for advancing committed blocks into confirmed
 * state.
 * This module owns the off-chain merge flow that replays the oldest queued
 * block into the confirmed ledger and then submits the corresponding merge tx.
 *
 * It performs the following tasks:
 *
 * 1. Fetches the confirmed state and the block it points to (i.e. the oldest
 *    block in the queue).
 * 2. Fetches the transactions of that block by querying BlocksDB and its
 *    associated inputs table..
 * 3. Apply those transactions to ConfirmedLedgerDB and update the table to
 *    store the updated UTxO set.
 * 4. Remove all header hashes from BlocksDB associated with the merged block.
 * 5. Build and submit the merge transaction.
 */

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Address,
  credentialToAddress,
  LucidEvolution,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Duration, Effect, Metric, Option, Ref } from "effect";

import { jsonReplacer } from "@/commands/command-utils.js";
import {
  BlocksDB,
  DepositsDB,
  ForcedTransactionsDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "@/database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import { emitQueueStateMetrics } from "@/fibers/queue-metrics.js";
import {
  registerSlotAwareDueWork,
  type SlotAwareDueWork,
} from "@/fibers/slot-aware-due-work.js";
import {
  localOgmiosSubmitSlotEvidence,
  makeLocalOgmiosSubmitSlotSnapshotProvider,
  SUBMIT_SLOT_LENGTH_MS,
  type SubmitSlotSnapshot,
} from "@/local-ledger-slot.js";
import {
  availableOperatorWalletUtxos,
  fetchOperatorWalletView,
} from "@/operator-wallet-view.js";
import type { NodeConfigDep } from "@/services/config.js";
import { Database, Globals, NodeConfig } from "@/services/index.js";
import {
  assertNativeMpfHashHex,
  type NativeMpfOwnerService,
} from "@/services/mpf-native-owner/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";
import { slotAwareDueWorkFromSubmitTiming } from "@/transactions/submit-timing-due-work.js";
import {
  BlockTxPayload,
  fetchFirstBlockTxs,
  handleSignSubmit,
  type NoInlineSubmitDefer,
  type NoInlineSubmitRecoveryOptions,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { breakDownTx } from "@/utils.js";
import { synchronizeCommitMpfStoresFromConfirmedLedger } from "@/workers/utils/mpf.js";

import {
  applyConfirmedLedgerDeltaChainTransaction,
  type ConfirmedLedgerSnapshot,
  materializeConfirmedLedgerSnapshot,
} from "./confirmed-ledger-snapshot.js";
import {
  classifyOldestQueuedBlockCandidateReadiness,
  DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS,
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeMaturityWindow,
  type MergeReadinessStatus,
  mergeSubmitValidityEvidence,
  type OldestQueuedBlockCandidateReadiness,
  planMergeLocalLedgerGate,
} from "./merge-readiness.js";

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

type PersistentCommitMpfSynchronization = {
  readonly ledgerEntryCount: number;
  readonly ledgerRoot: string;
  readonly transactionsRoot: string;
};

export type ConfirmedMergeMpfSynchronization =
  | ({
      readonly mode: "persistent_stores";
    } & PersistentCommitMpfSynchronization)
  | {
      readonly mode: "architecture_g_owner";
      readonly confirmedLedgerEntryCount: number;
      readonly confirmedLedgerRoot: string;
      readonly durableLedgerRoot: string;
      readonly activeGenerations: number;
    };

export const finalizeConfirmedMergeTransaction = ({
  headerHash,
  snapshot,
  projectedDepositEventIds,
  projectedWithdrawalEventIds,
  projectedForcedTransactionEventIds,
}: {
  readonly headerHash: Buffer;
  readonly snapshot: ConfirmedLedgerSnapshot;
  readonly projectedDepositEventIds: readonly Buffer[];
  readonly projectedWithdrawalEventIds: readonly Buffer[];
  readonly projectedForcedTransactionEventIds: readonly Buffer[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* Effect.logInfo(
          `🔸 Apply finalized confirmed-ledger V1 delta chain (deltas=${snapshot.deltaChain.length.toString()},root=${snapshot.root})...`,
        );
        yield* applyConfirmedLedgerDeltaChainTransaction(snapshot);
        yield* Effect.logInfo("🔸 Clear block from BlocksDB...");
        yield* BlocksDB.clearBlock(headerHash).pipe(
          Effect.withSpan("clear-block-from-BlocksDB"),
        );
        yield* DepositsDB.markConsumedByEventIds(projectedDepositEventIds).pipe(
          Effect.withSpan("mark-merged-deposits-consumed"),
        );
        yield* WithdrawalsDB.markFinalizedByEventIds(
          projectedWithdrawalEventIds,
          headerHash,
        ).pipe(Effect.withSpan("mark-merged-withdrawals-finalized"));
        yield* ForcedTransactionsDB.markFinalizedByEventIds(
          projectedForcedTransactionEventIds,
          headerHash,
        ).pipe(Effect.withSpan("mark-merged-forced-transactions-finalized"));
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      "confirmed_merge_finalization",
      "Failed to finalize confirmed-state merge locally",
    ),
  );

/**
 * A confirmed-state merge advances the L1 queue head but does not change the
 * latest committed L2 tail. Architecture G therefore retains the root already
 * promoted by its single native owner and verifies that owner is live instead
 * of reopening the owner's LevelDB path through MidgardMpf.
 */
export const synchronizeCommitMpfAfterConfirmedMerge = <E, R>({
  mpfEngine,
  nativeMpfOwner,
  confirmedLedgerEntryCount,
  confirmedLedgerRoot,
  synchronizePersistentStores,
}: {
  readonly mpfEngine: NodeConfigDep["MPF_ENGINE"];
  readonly nativeMpfOwner:
    | Pick<NativeMpfOwnerService, "diagnostics">
    | undefined;
  readonly confirmedLedgerEntryCount: number;
  readonly confirmedLedgerRoot: string;
  readonly synchronizePersistentStores: Effect.Effect<
    PersistentCommitMpfSynchronization,
    E,
    R
  >;
}): Effect.Effect<ConfirmedMergeMpfSynchronization, E | Error, R> => {
  if (mpfEngine !== "architecture_g") {
    return synchronizePersistentStores.pipe(
      Effect.map((result) => ({
        mode: "persistent_stores" as const,
        ...result,
      })),
    );
  }
  return Effect.tryPromise({
    try: async () => {
      if (nativeMpfOwner === undefined) {
        throw new Error(
          "Architecture G native owner is not initialized during confirmed-state merge finalization",
        );
      }
      const diagnostics = await nativeMpfOwner.diagnostics();
      assertNativeMpfHashHex(
        diagnostics.durableRoot,
        "Architecture G durable root",
      );
      return {
        mode: "architecture_g_owner" as const,
        confirmedLedgerEntryCount,
        confirmedLedgerRoot,
        durableLedgerRoot: diagnostics.durableRoot,
        activeGenerations: diagnostics.activeGenerations,
      };
    },
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Architecture G owner observation failed", { cause }),
  });
};

export const MERGE_CONFIRMATION_PROVIDER_RETRIES = 12;

const mergeFailureCounter = Metric.counter("merge_failure_count", {
  description: "A counter for tracking merge failures",
  bigint: true,
  incremental: true,
});

const mergeMissingBlockTxsCounter = Metric.counter(
  "merge_missing_block_txs_count",
  {
    description: "A counter for merge attempts blocked by missing BlocksDB txs",
    bigint: true,
    incremental: true,
  },
);

const mergeBlockTxDecodeFailureCounter = Metric.counter(
  "merge_block_tx_decode_failure_count",
  {
    description:
      "A counter for merge attempts blocked by malformed block transactions",
    bigint: true,
    incremental: true,
  },
);

const mergeLocalFinalizationFailureCounter = Metric.counter(
  "merge_local_finalization_failure_count",
  {
    description:
      "A counter for failed local DB finalization after merge submit",
    bigint: true,
    incremental: true,
  },
);

const mergeDurationTimer = Metric.timer(
  "merge_duration",
  "Duration of one merge attempt in milliseconds",
);

// 30 minutes.
const MAX_LIFE_OF_LOCAL_SYNC: number = 1_800_000;

type MergeErrorCode =
  | "E_MERGE_LAYOUT_DERIVATION_FAILED"
  | "E_MERGE_REDEEMER_INDEX_MISMATCH"
  | "E_MERGE_MISSING_BLOCK_TXS"
  | "E_MERGE_BLOCK_TX_DECODE_FAILED"
  | "E_MERGE_UPLC_EVAL_FAILED";

type MissingBlockTxsDiagnosis = {
  readonly reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE";
  readonly txHashesFound: number;
  readonly txsResolved: number;
};

export const diagnoseMissingBlockTxs = (
  txHashesFound: number,
  txsResolved: number,
): MissingBlockTxsDiagnosis | undefined => {
  if (txsResolved !== txHashesFound) {
    return {
      reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE",
      txHashesFound,
      txsResolved,
    };
  }
  return undefined;
};

type MergeOptions = {
  readonly bypassQueueLengthGuard?: boolean;
  readonly referenceScriptsAddress?: string;
  readonly leaseToken?: string;
  readonly headerHash?: string;
  readonly submitSlotSnapshot?: () => Effect.Effect<
    SubmitSlotSnapshot,
    unknown
  >;
  readonly revalidateMutationLease?: () => Effect.Effect<
    void,
    unknown,
    Database
  >;
};

export type CanonicalMergeCandidateReadiness =
  | {
      readonly status: "candidate";
      readonly confirmedUTxO: SDK.StateQueueUTxO;
      readonly firstBlockUTxO: SDK.StateQueueUTxO;
      readonly blockHeader: SDK.HeaderV1;
      readonly firstBlockNode: SDK.StateQueueNodeV1;
      readonly readiness: OldestQueuedBlockCandidateReadiness;
    }
  | {
      readonly status: "no_candidate";
      readonly reason: string;
    };

type SubmitSlotConfig = {
  readonly L1_OGMIOS_KEY: string;
  readonly L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: number;
};

export type MergeTxResult =
  | {
      readonly status: "merged";
      readonly headerHash: string;
      readonly txHash: string;
    }
  | {
      readonly status: Exclude<MergeReadinessStatus, "ready">;
      readonly reason: string;
      readonly headerHash?: string;
      readonly queueLength?: number;
      readonly minQueueLength?: number;
      readonly readyAfterUnixTime?: number;
      readonly nowUnixTime?: number;
    };

const makeJsonSafe = (value: unknown): unknown => {
  try {
    return JSON.parse(JSON.stringify(value, jsonReplacer)) as unknown;
  } catch {
    return formatUnknownError(value);
  }
};

const makeMergeStateQueueError = (
  errorCode: MergeErrorCode,
  message: string,
  cause: unknown,
): SDK.StateQueueError =>
  new SDK.StateQueueError({
    message: `${errorCode}: ${message}`,
    cause: {
      error_code: errorCode,
      details: makeJsonSafe(cause),
    },
  });

type MergeFailureOptions = {
  readonly missingBlockTxs?: boolean;
};

const failMergeWithCode = (
  errorCode: MergeErrorCode,
  message: string,
  cause: unknown,
  options?: MergeFailureOptions,
): Effect.Effect<never, SDK.StateQueueError> =>
  Effect.gen(function* () {
    yield* Metric.increment(mergeFailureCounter);
    if (options?.missingBlockTxs === true) {
      yield* Metric.increment(mergeMissingBlockTxsCounter);
    }
    return yield* Effect.fail(
      makeMergeStateQueueError(errorCode, message, cause),
    );
  });

const firstBlockOutRef = (firstBlockUTxO: SDK.StateQueueUTxO): string =>
  `${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex.toString()}`;

const isEmptyConfirmedStateLinkError = (error: unknown): boolean =>
  typeof error === "object" &&
  error !== null &&
  "_tag" in error &&
  (error as { readonly _tag?: unknown })._tag === "LinkedListError" &&
  "cause" in error &&
  (error as { readonly cause?: unknown }).cause === 'Given link is "Empty"';

export const fetchCanonicalMergeCandidateReadiness = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  contracts: SDK.MidgardValidators,
  nowUnixTime: number = Date.now(),
): Effect.Effect<
  CanonicalMergeCandidateReadiness,
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const fetchedCandidate = yield* Effect.either(
      SDK.fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig),
    );
    if (fetchedCandidate._tag === "Left") {
      if (isEmptyConfirmedStateLinkError(fetchedCandidate.left)) {
        return {
          status: "no_candidate",
          reason: "confirmed_state_link_empty",
        } satisfies CanonicalMergeCandidateReadiness;
      }
      return yield* Effect.fail(fetchedCandidate.left);
    }

    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      fetchedCandidate.right;
    if (firstBlockUTxO.datum.key === "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to classify merge candidate",
          cause: "first queued block cannot be a root node",
        }),
      );
    }

    const headerNodeKey = firstBlockUTxO.datum.key.Key.key;
    const firstBlockNode = yield* SDK.getStateQueueNodeV1FromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const blockHeader = yield* SDK.getHeaderV1FromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const recomputedHeaderHash = yield* SDK.hashBlockHeaderV1(blockHeader);
    if (recomputedHeaderHash !== headerNodeKey) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to classify merge candidate: queued block key/hash mismatch",
          cause: `datumKey=${headerNodeKey},computed=${recomputedHeaderHash}`,
        }),
      );
    }

    const mergeMaturity = mergeMaturityWindow(
      lucid,
      Number(blockHeader.endTime),
    );
    return {
      status: "candidate",
      confirmedUTxO,
      firstBlockUTxO,
      blockHeader,
      firstBlockNode,
      readiness: classifyOldestQueuedBlockCandidateReadiness({
        firstBlockOutRef: firstBlockOutRef(firstBlockUTxO),
        headerHash: recomputedHeaderHash,
        currentDaAttestation: firstBlockNode.da_attestation,
        requiredDaAttestation: contracts.daAttestation.policyId,
        validFromUnixTime: mergeMaturity.validFromUnixTime,
        readyAfterUnixTime: mergeMaturity.readyAfterUnixTime,
        nowUnixTime,
      }),
    } satisfies CanonicalMergeCandidateReadiness;
  });

export const mergeSemanticSkipResult = (
  readiness: Exclude<
    OldestQueuedBlockCandidateReadiness,
    { readonly status: "ready" }
  >,
): MergeTxResult => ({
  status: readiness.status,
  headerHash: readiness.headerHash,
  reason: readiness.reason,
  readyAfterUnixTime: readiness.readyAfterUnixTime,
  nowUnixTime: readiness.nowUnixTime,
});

type MergeDecodedBlockTx = {
  readonly txId: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly LedgerEntry[];
};

type MergeBlockTxPreflightError = {
  readonly index: number;
  readonly txIdHex: string;
  readonly reason: "DECODE_FAILED" | "TX_ID_MISMATCH";
  readonly details: string;
  readonly decodedTxIdHex?: string;
};

export const preflightDecodeBlockTxs = (
  blockTxs: readonly BlockTxPayload[],
): Effect.Effect<readonly MergeDecodedBlockTx[], MergeBlockTxPreflightError> =>
  Effect.forEach(
    blockTxs,
    (blockTx, index) =>
      Effect.gen(function* () {
        const txIdHex = blockTx.txId.toString("hex");
        const decoded = yield* breakDownTx(blockTx.txCbor).pipe(
          Effect.mapError(
            (cause): MergeBlockTxPreflightError => ({
              index,
              txIdHex,
              reason: "DECODE_FAILED",
              details: formatUnknownError(cause),
            }),
          ),
        );
        if (!decoded.txId.equals(blockTx.txId)) {
          return yield* Effect.fail<MergeBlockTxPreflightError>({
            index,
            txIdHex,
            reason: "TX_ID_MISMATCH",
            decodedTxIdHex: decoded.txId.toString("hex"),
            details:
              "Computed tx_id from payload does not match BlocksDB tx_id",
          });
        }
        return {
          txId: blockTx.txId,
          spent: decoded.spent,
          produced: decoded.produced,
        } satisfies MergeDecodedBlockTx;
      }),
    { concurrency: "unbounded" },
  );

const getStateQueueLength = (
  lucid: LucidEvolution,
  stateQueueAddress: Address,
): Effect.Effect<number, SDK.LucidError, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH = yield* Ref.get(
      globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
    );
    const now_millis = Date.now();
    if (
      now_millis - LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH >
      MAX_LIFE_OF_LOCAL_SYNC
    ) {
      // We consider in-memory state queue length stale.
      yield* Effect.logInfo(
        `🔸 Fetching state queue length from ${stateQueueAddress}...`,
      );
      const stateQueueUtxos = yield* Effect.tryPromise({
        try: () => lucid.utxosAt(stateQueueAddress),
        catch: (e) =>
          new SDK.LucidError({
            message: `Failed to fetch UTxOs at state queue address: ${stateQueueAddress}`,
            cause: e,
          }),
      });

      yield* Ref.set(
        globals.BLOCKS_IN_QUEUE,
        Math.max(0, stateQueueUtxos.length - 1),
      );
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );
      yield* emitQueueStateMetrics;

      return Math.max(0, stateQueueUtxos.length - 1);
    } else {
      return yield* Ref.get(globals.BLOCKS_IN_QUEUE);
    }
  });

const slotFromUnixTime = (
  lucid: LucidEvolution,
  unixTimeMs: number,
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      const slot = Number(lucid.unixTimeToSlot(unixTimeMs));
      if (!Number.isSafeInteger(slot) || slot < 0) {
        throw new Error(`invalid slot=${slot.toString()}`);
      }
      return slot;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: "Failed to convert merge valid-from time to a slot",
        cause,
      }),
  });

const mergeSubmitRecoveryOptions = (
  nodeConfig: SubmitSlotConfig,
  deferEvidence: {
    readonly key: string;
    readonly dependencyKey: string;
    readonly invalidationKey: string;
  },
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
): NoInlineSubmitRecoveryOptions => ({
  label: "merge",
  confirmationRetries: MERGE_CONFIRMATION_PROVIDER_RETRIES,
  slotSnapshot:
    submitSlotSnapshot ??
    makeLocalOgmiosSubmitSlotSnapshotProvider({
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    }),
  requireSlotForBoundedTx: true,
  maxPreSubmitWaitMs: DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS,
  inlineWaitPolicy: "defer_positive_wait",
  noInlineSubmitDefer: deferEvidence,
});

export const mergeNoInlineSubmitDueWorkFromDefer = ({
  defer,
  localSubmitSlotSnapshot,
  nowMs,
}: {
  readonly defer: NoInlineSubmitDefer;
  readonly localSubmitSlotSnapshot?: {
    readonly currentSlot: number;
    readonly slotLengthMs: number;
    readonly source: string;
  };
  readonly nowMs?: number;
}): SlotAwareDueWork => {
  if (defer.kind !== "provider_slot_wait") {
    return {
      kind: "merge_submit_validity",
      key: defer.key,
      callerLabel: defer.callerLabel,
      reason: `merge_submit_${defer.kind}_not_reached`,
      observedSlot: defer.currentSlot,
      dueSlot: defer.dueSlot,
      dueAtMs: (nowMs ?? Date.now()) + defer.waitMs,
      waitMs: defer.waitMs,
      slotSource: defer.slotSource,
      dependencyKey: defer.dependencyKey,
      invalidationKey: defer.invalidationKey,
    };
  }
  if (localSubmitSlotSnapshot === undefined) {
    throw new Error(
      "local submit slot snapshot is required for merge provider-slot defer",
    );
  }
  const slotLengthMs = Math.max(
    1,
    Math.floor(localSubmitSlotSnapshot.slotLengthMs || SUBMIT_SLOT_LENGTH_MS),
  );
  return {
    kind: "merge_submit_validity",
    key: defer.key,
    callerLabel: defer.callerLabel,
    reason: `merge_submit_${defer.kind}_not_reached,provider_current_slot=${defer.currentSlot.toString()},provider_due_slot=${defer.dueSlot.toString()}`,
    observedSlot: localSubmitSlotSnapshot.currentSlot,
    dueSlot:
      localSubmitSlotSnapshot.currentSlot +
      Math.max(1, Math.ceil(defer.waitMs / slotLengthMs)),
    dueAtMs: (nowMs ?? Date.now()) + defer.waitMs,
    waitMs: defer.waitMs,
    slotSource: localSubmitSlotSnapshot.source,
    dependencyKey: defer.dependencyKey,
    invalidationKey: defer.invalidationKey,
  };
};

const registerMergeNoInlineSubmitDueWork = (
  defer: NoInlineSubmitDefer,
  nodeConfig: SubmitSlotConfig,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
) =>
  Effect.gen(function* () {
    if (defer.kind !== "provider_slot_wait") {
      return registerSlotAwareDueWork(
        mergeNoInlineSubmitDueWorkFromDefer({ defer }),
      );
    }
    const localSnapshot = yield* (
      submitSlotSnapshot ??
      makeLocalOgmiosSubmitSlotSnapshotProvider({
        ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
        timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
      })
    )().pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to read local Ogmios submit-ledger slot before registering merge provider-slot defer",
            cause,
          }),
      ),
    );
    return registerSlotAwareDueWork(
      mergeNoInlineSubmitDueWorkFromDefer({
        defer,
        localSubmitSlotSnapshot: localSnapshot,
      }),
    );
  });

export const captureMergeLocalLedgerGate = ({
  lucid,
  nodeConfig,
  validFromUnixTime,
  leaseToken,
  headerHash,
  candidateIdentity,
  submitSlotSnapshot,
}: {
  readonly lucid: LucidEvolution;
  readonly nodeConfig: SubmitSlotConfig;
  readonly validFromUnixTime: number;
  readonly leaseToken?: string;
  readonly headerHash: string;
  readonly candidateIdentity?: string;
  readonly submitSlotSnapshot?: () => Effect.Effect<
    SubmitSlotSnapshot,
    unknown
  >;
}): Effect.Effect<
  | { readonly status: "ready" }
  | { readonly status: "retry_later"; readonly reason: string },
  SDK.StateQueueError,
  Database
> =>
  Effect.gen(function* () {
    const validFromSlot = yield* slotFromUnixTime(lucid, validFromUnixTime);
    const dueWorkEvidence = mergeSubmitValidityEvidence({
      headerHash,
      validFromSlot,
      ...(candidateIdentity === undefined ? {} : { candidateIdentity }),
    });
    const slotSnapshotProvider =
      submitSlotSnapshot ??
      makeLocalOgmiosSubmitSlotSnapshotProvider({
        ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
        timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
      });
    const snapshot = yield* slotSnapshotProvider().pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to read submit-ledger slot before merge build",
            cause,
          }),
      ),
    );
    const gate = planMergeLocalLedgerGate({
      validFromSlot,
      localLedgerSlot: snapshot.currentSlot,
      slotLengthMs: snapshot.slotLengthMs,
      slotSource: snapshot.source,
      observedAtMs: snapshot.observedAtMs,
      maxWaitMs: DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS,
      inlineWaitPolicy: "defer_positive_wait",
      dependencyKey: dueWorkEvidence.dependencyKey,
      invalidationKey: dueWorkEvidence.invalidationKey,
    });
    const logFields = [
      `validFromUnixTime=${validFromUnixTime.toString()}`,
      `validFromSlot=${validFromSlot.toString()}`,
      `headerHash=${headerHash}`,
      `candidateIdentity=${candidateIdentity ?? "none"}`,
      `targetSlot=${gate.targetSlot.toString()}`,
      `localLedgerSlot=${snapshot.currentSlot.toString()}`,
      `deltaSlots=${gate.deltaSlots.toString()}`,
      `waitMs=${gate.waitMs.toString()}`,
      `slotSource=${snapshot.source}`,
      `leaseToken=${leaseToken ?? "none"}`,
      "callerLabel=merge",
      `dependencyKey=${dueWorkEvidence.dependencyKey}`,
      `invalidationKey=${dueWorkEvidence.invalidationKey}`,
      localOgmiosSubmitSlotEvidence(snapshot),
    ].join(",");
    if (gate.status === "ready") {
      yield* Effect.logInfo(`🔸 Merge local-ledger gate ready (${logFields}).`);
      return { status: "ready" } as const;
    }
    if (gate.status === "retry_later") {
      if (gate.submitTimingNotDuePlan === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Merge due-work planning lost submit-timing evidence",
            cause: gate.reason,
          }),
        );
      }
      const dueWork = slotAwareDueWorkFromSubmitTiming({
        kind: "merge_submit_validity",
        key: dueWorkEvidence.key,
        callerLabel: "merge",
        reason: "merge_submit_validity_not_reached",
        plan: gate.submitTimingNotDuePlan,
        nowMs: Date.now(),
      });
      registerSlotAwareDueWork(dueWork);
      yield* Effect.logInfo(
        `🔸 Skipping merge until local submit ledger reaches merge validity lower bound (kind=${dueWork.kind},key=${dueWork.key},current_slot=${dueWork.observedSlot.toString()},due_slot=${dueWork.dueSlot.toString()},wait_ms=${dueWork.waitMs.toString()},slot_source=${dueWork.slotSource},dependency_key=${dueWork.dependencyKey},${logFields}).`,
      );
      return {
        status: "retry_later",
        reason: `${gate.reason},valid_from_unix_time=${validFromUnixTime.toString()}`,
      } as const;
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Merge local-ledger planner unexpectedly allowed an inline wait in no-inline mode",
        cause: logFields,
      }),
    );
  });

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param contracts - Midgard script bundle used for state_queue and settlement.
 * @returns An Effect that resolves to either a submitted merge transaction or a
 *          structured skip result explaining why no merge was attempted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  contracts: SDK.MidgardValidators,
  options?: MergeOptions,
): Effect.Effect<
  MergeTxResult,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | TxSubmitError
  | TxConfirmError
  | TxSignError,
  Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const mergeStartedAt = Date.now();
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const currentStateQueueLength = yield* getStateQueueLength(
      lucid,
      fetchConfig.stateQueueAddress,
    );
    const minQueueLengthForMerging =
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING;
    const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
    if (resetInProgress) {
      return {
        status: "skipped_reset_in_progress",
        reason: "reset_in_progress=true",
        queueLength: currentStateQueueLength,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }
    if (currentStateQueueLength <= 0) {
      return {
        status: "no_queued_block",
        reason: `queue_length=${currentStateQueueLength.toString()}`,
        queueLength: currentStateQueueLength,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }
    // Avoid a merge tx if the queue is too short (performing a merge with such
    // conditions has a chance of wasting the work done for root computations).
    if (
      !options?.bypassQueueLengthGuard &&
      currentStateQueueLength < minQueueLengthForMerging
    ) {
      return {
        status: "skipped_below_min_queue_length",
        reason: `queue_length=${currentStateQueueLength.toString()},min_queue_length=${minQueueLengthForMerging.toString()}`,
        queueLength: currentStateQueueLength,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }

    yield* Effect.logInfo("🔸 Merging of oldest block started.");

    yield* Effect.logInfo(
      "🔸 Fetching confirmed state and the first block in queue from L1...",
    );
    const candidate = yield* fetchCanonicalMergeCandidateReadiness(
      lucid,
      fetchConfig,
      contracts,
    );
    if (candidate.status === "candidate") {
      const {
        confirmedUTxO,
        firstBlockUTxO,
        blockHeader,
        readiness: oldestBlockReadiness,
      } = candidate;
      yield* Effect.logInfo(
        `🔸 First block found: ${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex}`,
      );
      if (oldestBlockReadiness.status !== "ready") {
        if (oldestBlockReadiness.status === "skipped_oldest_block_unattested") {
          yield* Effect.logInfo(
            `🔸 Skipping merge because oldest block is not DA-attested yet (${oldestBlockReadiness.reason}).`,
          );
        } else {
          yield* Effect.logInfo(
            `🔸 Oldest block is not mature enough for merge yet (${oldestBlockReadiness.reason}).`,
          );
        }
        return mergeSemanticSkipResult(oldestBlockReadiness);
      }
      const mergeMaturity = {
        validFromUnixTime: oldestBlockReadiness.validFromUnixTime,
        readyAfterUnixTime: oldestBlockReadiness.readyAfterUnixTime,
      };
      const recomputedHeaderHash = oldestBlockReadiness.headerHash;
      // Fetch transactions from the first block
      yield* Effect.logInfo("🔸 Looking up its transactions from BlocksDB...");
      const {
        txs: firstBlockTxs,
        txHashes: firstBlockTxHashes,
        headerHash,
      } = yield* fetchFirstBlockTxs(firstBlockUTxO).pipe(
        Effect.withSpan("fetchFirstBlockTxs"),
      );
      const missingBlockTxsDiagnosis = diagnoseMissingBlockTxs(
        firstBlockTxHashes.length,
        firstBlockTxs.length,
      );
      if (missingBlockTxsDiagnosis !== undefined) {
        return yield* failMergeWithCode(
          "E_MERGE_MISSING_BLOCK_TXS",
          "Failed to merge block into confirmed state",
          {
            headerHash: headerHash.toString("hex"),
            ...missingBlockTxsDiagnosis,
          },
          { missingBlockTxs: true },
        );
      }
      if (firstBlockTxHashes.length === 0) {
        yield* Effect.logInfo(
          `🔸 No native block tx payloads indexed for header=${headerHash.toString("hex")}; treating merge replay as a no-op for immutable txs.`,
        );
      }
      const preflightDecodedBlockTxsResult = yield* Effect.either(
        preflightDecodeBlockTxs(firstBlockTxs),
      );
      if (preflightDecodedBlockTxsResult._tag === "Left") {
        yield* Metric.increment(mergeBlockTxDecodeFailureCounter);
        return yield* failMergeWithCode(
          "E_MERGE_BLOCK_TX_DECODE_FAILED",
          "Failed preflight decode of block transactions before merge submission",
          {
            headerHash: headerHash.toString("hex"),
            failingTx: preflightDecodedBlockTxsResult.left,
            txCount: firstBlockTxs.length,
          },
        );
      }
      const preflightDecodedBlockTxs = preflightDecodedBlockTxsResult.right;
      const preflightSpentOutRefs: Buffer[] = [];
      const preflightProducedUTxOs: LedgerEntry[] = [];
      for (const decoded of preflightDecodedBlockTxs) {
        preflightSpentOutRefs.push(...decoded.spent);
        preflightProducedUTxOs.push(...decoded.produced);
      }
      yield* Effect.logInfo(
        `🔸 Preflight decoded ${preflightDecodedBlockTxs.length} block tx(s) successfully (header=${headerHash.toString("hex")}).`,
      );
      yield* Effect.logInfo("🔸 Building merge transaction...");

      const localLedgerGate = yield* captureMergeLocalLedgerGate({
        lucid,
        nodeConfig,
        validFromUnixTime: mergeMaturity.validFromUnixTime,
        leaseToken: options?.leaseToken,
        headerHash: recomputedHeaderHash,
        candidateIdentity: oldestBlockReadiness.candidateIdentity,
        submitSlotSnapshot: options?.submitSlotSnapshot,
      });
      if (localLedgerGate.status === "retry_later") {
        return {
          status: "skipped_oldest_block_local_ledger_not_ready",
          headerHash: recomputedHeaderHash,
          reason: localLedgerGate.reason,
          readyAfterUnixTime: mergeMaturity.readyAfterUnixTime,
          nowUnixTime: Date.now(),
        } satisfies MergeTxResult;
      }

      yield* Effect.logInfo(
        `🔸 Merge policies: state_queue=${fetchConfig.stateQueuePolicyId},settlement=${contracts.settlement.policyId},state_queue_script_has_settlement_param=${contracts.stateQueue.mintingScriptCBOR.includes(contracts.settlement.policyId)}`,
      );

      const network = lucid.config().network;
      if (network === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to build merge transaction: Cardano network is undefined",
            cause: "lucid.config().network",
          }),
        );
      }
      const hubOracleAddress = credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      );
      const hubOracleUnit = toUnit(
        contracts.hubOracle.policyId,
        SDK.HUB_ORACLE_ASSET_NAME,
      );
      const hubOracleWitnessUtxos = yield* Effect.tryPromise({
        try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to fetch hub-oracle witness UTxOs for merge tx",
            cause,
          }),
      });
      if (hubOracleWitnessUtxos.length !== 1) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to resolve unique hub-oracle UTxO for merge transaction",
            cause: `expected=1,found=${hubOracleWitnessUtxos.length},address=${hubOracleAddress},unit=${hubOracleUnit}`,
          }),
        );
      }
      const hubOracleRefInput = hubOracleWitnessUtxos[0];

      const resolvedReferenceScripts =
        options?.referenceScriptsAddress === undefined
          ? []
          : yield* fetchReferenceScriptUtxosProgram(
              lucid,
              options.referenceScriptsAddress,
              [
                {
                  name: "state-queue spending",
                  script: contracts.stateQueue.spendingScript,
                },
                {
                  name: "state-queue minting",
                  script: contracts.stateQueue.mintingScript,
                },
                {
                  name: "settlement minting",
                  script: contracts.settlement.mintingScript,
                },
              ],
              contracts.referenceScriptAuth,
            );
      const referenceScripts: SDK.StateQueueMergeReferenceScripts | undefined =
        options?.referenceScriptsAddress === undefined
          ? undefined
          : {
              stateQueueSpending: referenceScriptByName(
                resolvedReferenceScripts,
                "state-queue spending",
              ),
              stateQueueMinting: referenceScriptByName(
                resolvedReferenceScripts,
                "state-queue minting",
              ),
              settlementMinting: referenceScriptByName(
                resolvedReferenceScripts,
                "settlement minting",
              ),
            };

      const operatorWalletView = yield* Effect.tryPromise({
        try: () => fetchOperatorWalletView(lucid),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to initialize merge wallet view",
            cause,
          }),
      });
      const presetWalletInputs = yield* SDK.requireOperatorWalletInputs(
        availableOperatorWalletUtxos(operatorWalletView),
        "state_queue merge tx",
      );
      yield* Effect.logInfo(
        `🔸 Using ${presetWalletInputs.length.toString()} preset operator wallet input(s) for merge tx (known_wallet_utxos=${operatorWalletView.knownUtxos.length.toString()}).`,
      );

      const builtMerge =
        yield* SDK.buildProductionMergeToConfirmedStateTxProgram({
          lucid,
          fetchConfig,
          contracts,
          confirmedUTxO,
          firstBlockUTxO,
          validFrom: mergeMaturity.validFromUnixTime,
          presetWalletInputs,
          hubOracleRefInput,
          referenceScripts,
        }).pipe(Effect.tapError(() => Metric.increment(mergeFailureCounter)));
      const txBuilder = builtMerge.tx;

      // Submit the transaction
      /**
       * Normalizes transaction-submission failures during confirmed-state merging.
       */
      const onSubmitFailure = (err: TxSubmitError) =>
        Effect.gen(function* () {
          yield* Effect.logError(`Submit tx error: ${err}`);
          yield* Effect.fail(
            new TxSubmitError({
              message: "failed to submit the merge tx",
              cause: err,
              txHash: txBuilder.toHash(),
            }),
          );
        });
      /**
       * Normalizes transaction-confirmation failures during confirmed-state merging.
       */
      const onConfirmFailure = (err: TxConfirmError) =>
        Effect.gen(function* () {
          yield* Effect.logError(
            `Confirm tx error: ${err}; refusing local merge finalization until L1 confirmation is verified`,
          );
          yield* Effect.fail(
            new TxConfirmError({
              message:
                "failed to confirm the merge tx; local merge finalization blocked",
              cause: err,
              txHash: txBuilder.toHash(),
            }),
          );
        });
      const txHash = txBuilder.toHash();
      const submitValidFromSlot = yield* slotFromUnixTime(
        lucid,
        mergeMaturity.validFromUnixTime,
      );
      const submitDueWorkEvidence = mergeSubmitValidityEvidence({
        headerHash: recomputedHeaderHash,
        validFromSlot: submitValidFromSlot,
        candidateIdentity: oldestBlockReadiness.candidateIdentity,
      });
      const submitRecoveryOptions = mergeSubmitRecoveryOptions(
        nodeConfig,
        submitDueWorkEvidence,
        options?.submitSlotSnapshot,
      );
      const rawSubmitOutcome = yield* handleSignSubmit(
        lucid,
        txBuilder,
        submitRecoveryOptions,
      ).pipe(
        Effect.as({ status: "submitted" } as const),
        Effect.catchTag("NoInlineSubmitDefer", (defer) =>
          Effect.succeed({ status: "deferred", defer } as const),
        ),
        Effect.catchTag("TxSubmitError", onSubmitFailure),
        Effect.catchTag("TxConfirmError", onConfirmFailure),
        Effect.withSpan("handleSignSubmit-merge-tx"),
      );
      const submitOutcome = rawSubmitOutcome ?? {
        status: "submitted",
      };
      if (submitOutcome.status === "deferred") {
        const dueWork = yield* registerMergeNoInlineSubmitDueWork(
          submitOutcome.defer,
          nodeConfig,
          options?.submitSlotSnapshot,
        );
        yield* Effect.logInfo(
          `🔸 Skipping merge after no-inline submit defer (kind=${dueWork.kind},key=${dueWork.key},callerLabel=${dueWork.callerLabel},deferKind=${submitOutcome.defer.kind},current_slot=${dueWork.observedSlot.toString()},due_slot=${dueWork.dueSlot.toString()},wait_ms=${dueWork.waitMs.toString()},slot_source=${dueWork.slotSource},dependency_key=${dueWork.dependencyKey},invalidation_key=${dueWork.invalidationKey},leaseToken=${options?.leaseToken ?? "none"},headerHash=${recomputedHeaderHash}).`,
        );
        return {
          status: "skipped_oldest_block_local_ledger_not_ready",
          headerHash: recomputedHeaderHash,
          reason: `no_inline_submit_defer,kind=${submitOutcome.defer.kind},current_slot=${submitOutcome.defer.currentSlot.toString()},target_slot=${submitOutcome.defer.targetSlot.toString()},due_slot=${submitOutcome.defer.dueSlot.toString()},wait_ms=${submitOutcome.defer.waitMs.toString()},slot_source=${submitOutcome.defer.slotSource}`,
          readyAfterUnixTime: mergeMaturity.readyAfterUnixTime,
          nowUnixTime: Date.now(),
        } satisfies MergeTxResult;
      }
      yield* Effect.logInfo(
        "🔸 Merge transaction submitted, updating the db...",
      );

      const finalizeLocalMergeProgram = Effect.gen(function* () {
        const jobId = `confirmed_merge_finalization:${headerHash.toString(
          "hex",
        )}`;
        const projectedDepositEntries =
          yield* DepositsDB.retrieveByProjectedHeaderHash(headerHash);
        const projectedForcedTransactionEntries =
          yield* ForcedTransactionsDB.retrieveByProjectedHeaderHash(headerHash);
        const projectedWithdrawalEntries =
          yield* WithdrawalsDB.retrieveByProjectedHeaderHash(headerHash);
        const projectedDepositEventIds = projectedDepositEntries.map(
          (entry) => entry[DepositsDB.Columns.ID],
        );
        const projectedWithdrawalEventIds = projectedWithdrawalEntries.map(
          (entry) => entry[WithdrawalsDB.Columns.ID],
        );
        const projectedForcedTransactionEventIds =
          projectedForcedTransactionEntries.map(
            (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          );
        const finalizedJournal =
          yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
        if (Option.isNone(finalizedJournal)) {
          return yield* Effect.fail(
            new DatabaseError({
              table: PendingBlockFinalizationsDB.tableName,
              message:
                "Failed to finalize confirmed-state merge locally because the pending-finalization journal is missing",
              cause: `header_hash=${headerHash.toString("hex")}`,
            }),
          );
        }
        const confirmedLedgerSnapshot =
          yield* materializeConfirmedLedgerSnapshot(finalizedJournal.value);
        const confirmedLedgerSnapshotRoot = confirmedLedgerSnapshot.root;
        const expectedSnapshotRoot =
          finalizedJournal.value[
            PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
          ];
        if (
          confirmedLedgerSnapshotRoot !== expectedSnapshotRoot ||
          confirmedLedgerSnapshotRoot !== blockHeader.utxosRoot
        ) {
          return yield* Effect.fail(
            new DatabaseError({
              table: PendingBlockFinalizationsDB.tableName,
              message:
                "Failed to finalize confirmed-state merge locally because the durable UTxO snapshot root does not match the confirmed block",
              cause: `header_hash=${headerHash.toString(
                "hex",
              )},snapshot_root=${confirmedLedgerSnapshotRoot},journal_expected_root=${expectedSnapshotRoot},confirmed_header_root=${blockHeader.utxosRoot}`,
            }),
          );
        }
        yield* MutationJobsDB.start({
          jobId,
          kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
          payload: {
            headerHash: headerHash.toString("hex"),
            spentOutRefCount: preflightSpentOutRefs.length,
            producedUtxoCount: preflightProducedUTxOs.length,
            depositEventCount: projectedDepositEventIds.length,
            forcedTransactionEventCount:
              projectedForcedTransactionEventIds.length,
            withdrawalEventCount: projectedWithdrawalEventIds.length,
            confirmedLedgerSnapshotRoot,
            ledgerDeltaSpentCount: confirmedLedgerSnapshot.delta.spent.length,
            ledgerDeltaProducedCount:
              confirmedLedgerSnapshot.delta.produced.length,
          },
        });
        yield* finalizeConfirmedMergeTransaction({
          headerHash,
          snapshot: confirmedLedgerSnapshot,
          projectedDepositEventIds,
          projectedWithdrawalEventIds,
          projectedForcedTransactionEventIds,
        });
        const syncResult = yield* synchronizeCommitMpfAfterConfirmedMerge({
          mpfEngine: nodeConfig.MPF_ENGINE,
          nativeMpfOwner: yield* Ref.get(globals.NATIVE_MPF_OWNER),
          confirmedLedgerEntryCount: confirmedLedgerSnapshot.entries.length,
          confirmedLedgerRoot: confirmedLedgerSnapshotRoot,
          synchronizePersistentStores:
            synchronizeCommitMpfStoresFromConfirmedLedger,
        }).pipe(
          Effect.mapError(
            (error) =>
              new DatabaseError({
                table: "confirmed_merge_finalization",
                message:
                  "Failed to synchronize commit MPF stores after confirmed-state merge",
                cause: formatUnknownError(error),
              }),
          ),
        );
        yield* syncResult.mode === "architecture_g_owner"
          ? Effect.logInfo(
              `🔸 Retained Architecture G owner after merge local finalization (header=${headerHash.toString(
                "hex",
              )},confirmed_ledger_entries=${syncResult.confirmedLedgerEntryCount.toString()},confirmed_ledger_root=${syncResult.confirmedLedgerRoot},durable_tail_root=${syncResult.durableLedgerRoot},active_generations=${syncResult.activeGenerations.toString()}).`,
            )
          : Effect.logInfo(
              `🔸 Synchronized commit MPFs after merge local finalization (header=${headerHash.toString(
                "hex",
              )},ledger_entries=${syncResult.ledgerEntryCount.toString()},ledger_root=${syncResult.ledgerRoot}).`,
            );
        yield* MutationJobsDB.markCompleted(jobId);
      }).pipe(
        Effect.tapError((error) =>
          MutationJobsDB.markFailed(
            `confirmed_merge_finalization:${headerHash.toString("hex")}`,
            formatUnknownError(error),
          ).pipe(Effect.catchAll(() => Effect.void)),
        ),
      );
      yield* finalizeLocalMergeProgram.pipe(
        Effect.tapError((error) =>
          Effect.gen(function* () {
            yield* Metric.increment(mergeLocalFinalizationFailureCounter);
            yield* Effect.logError(
              `🔸 Merge local finalization failed after on-chain submit (header=${headerHash.toString(
                "hex",
              )},tx_count=${preflightDecodedBlockTxs.length},sample_tx_ids=${JSON.stringify(
                preflightDecodedBlockTxs
                  .slice(0, 10)
                  .map((decoded) => decoded.txId.toString("hex")),
              )},error=${formatUnknownError(error)})`,
            );
          }),
        ),
      );
      yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");

      yield* Metric.increment(mergeBlockCounter).pipe(
        Effect.withSpan("increment-merge-block-counter"),
      );
      yield* mergeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mergeStartedAt)),
      );

      yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => Math.max(0, n - 1));
      yield* emitQueueStateMetrics;
      return {
        status: "merged",
        headerHash: headerHash.toString("hex"),
        txHash,
      } satisfies MergeTxResult;
    } else {
      yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
      yield* Ref.set(
        globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
        Date.now(),
      );
      yield* emitQueueStateMetrics;
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      yield* mergeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mergeStartedAt)),
      );
      return {
        status: "no_queued_block",
        reason: "no_first_block_utxo",
        queueLength: 0,
        minQueueLength: minQueueLengthForMerging,
      } satisfies MergeTxResult;
    }
  });
