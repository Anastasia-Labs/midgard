/**
 * The commit-time MPF pipeline: processMpfs and the root-transaction and block-overlay scopes.
 */

import { dirname } from "node:path";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  canonicalCommittedWithdrawalTransitionEffect,
  canonicalDepositTransitionEffect,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { Effect, Fiber, Metric, Option } from "effect";
import * as FS from "fs";

import * as ConfirmedLedgerDB from "../database/confirmedLedger.js";
import * as DepositsDB from "../database/deposits.js";
import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import * as MempoolDB from "../database/mempool.js";
import * as MempoolLedgerDB from "../database/mempoolLedger.js";
import * as MempoolTxDeltasDB from "../database/mempoolTxDeltas.js";
import * as PendingBlockFinalizationsDB from "../database/pendingBlockFinalizations.js";
import * as TxAdmissionsDB from "../database/txAdmissions.js";
import * as TxRejectionsDB from "../database/txRejections.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import * as WithdrawalsDB from "../database/withdrawals.js";
import { Database } from "../services/index.js";
import { type NativeMpfGenerationHandle } from "../services/mpf-native-owner/index.js";
import {
  type ClassifiedWithdrawal,
  classifyWithdrawal,
  indexSelectedLedgerOutputs,
} from "../workers/utils/mpf/withdrawal-classification.js";
import {
  COMMIT_REJECT_CODE_FORCED_TRANSACTION_INPUT,
  COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT,
  COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  persistCommitStageRejectedTransactions,
  resolveTxDeltaForCommit,
} from "./commit-rejection.js";
import { MpfError } from "./errors.js";
import {
  applyValidationLedgerMutations,
  type ClassifiedForcedTransaction,
  classifyForcedTransactions,
  programMaterialSidecarForEnvelopes,
  resolveIncludedDepositEntriesForWindow,
  resolveIncludedForcedTransactionEntriesForWindow,
  resolveIncludedWithdrawalEntriesForWindow,
  validationLedgerWitnesses,
} from "./event-window.js";
import {
  collapseLedgerDelta,
  type LedgerDelta,
  ledgerEntryToInsertBatchOp,
  ledgerOutputToInsertBatchOp,
  transitionEffectToLedgerOps,
} from "./ledger-delta.js";
import { encodeTransactionRootValue } from "./ledger-hydration.js";
import {
  type DecodedMempoolTxForCommit,
  establishEffectiveEndTimeFromDecodedMempool,
  orderDecodedMempoolTxsForLedgerApplication,
} from "./mempool-order.js";
import {
  applyLedgerOpsToUtxoPayloadAggregateFromFullValues,
  computeUtxoPayloadRoot,
  ledgerPayloadAggregateFromEntries,
  materializeUtxoPayloadEntries,
  type UtxoPayloadSizeAggregate,
} from "./payload-size.js";
import { type ProcessMpfsConfig } from "./process-config.js";
import {
  type CorpusHexEntry,
  type CorpusLedgerOp,
  type MpfReplayCorpusBlock,
} from "./replay-corpus.js";
import { MidgardMpf } from "./store.js";
import {
  depositTraceEventKey,
  eventKeyCbor,
  forcedTransactionTraceEventKey,
  l2TransactionTraceEventKey,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
  withdrawalTraceEventKey,
} from "./trace-events.js";
import {
  buildNativeTransitionTraceResult,
  buildTransactionsSourceRoot,
  buildTransitionTraceResult,
  countedRootFromEncodedEntries,
  type NativeMpfReplayBuild,
  type TransitionTraceBuildResult,
} from "./transition-trace.js";
import {
  type MpfBatchOp,
  type MpfInsertBatchOp,
  type UtxoPayloadEntry,
} from "./types.js";
import {
  buildDeterministicValidationTraceMembers,
  type RetainedValidationTraceMember,
  validateValidationTraceEventKeySet,
  type ValidationTraceBuildResult,
} from "./validation-trace.js";

const logCommitMpfPhaseTiming = (
  phase: string,
  startedAtMs: number,
  counts: Record<string, number>,
): Effect.Effect<void, never> => {
  const countSummary = Object.entries(counts)
    .map(([key, value]) => `${key}=${value.toString()}`)
    .join(",");
  const suffix = countSummary.length > 0 ? `,${countSummary}` : "";
  return Effect.logInfo(
    `🔹 Commit MPF phase ${phase} completed duration_ms=${Math.max(
      0,
      Date.now() - startedAtMs,
    ).toString()}${suffix}`,
  );
};

export const processMpfs = (
  ledgerMpf: MidgardMpf | undefined,
  transactionsMpf: MidgardMpf,
  mempoolTxs: readonly Tx.EntryWithTimeStamp[],
  config?: ProcessMpfsConfig,
): Effect.Effect<
  {
    utxoRoot: string;
    rawTxRoot: string;
    txRoot: string;
    transitionTraceRoot: string;
    eventToStepRoot: string;
    validationTracesRoot: string;
    transitionTraceMembers: readonly RetainedTransitionTraceMember[];
    eventToStepMembers: readonly RetainedEventToStepMember[];
    validationTraceMembers: readonly RetainedValidationTraceMember[];
    transitionStepCount: number;
    validationTraceCount: number;
    totalEventCount: number;
    utxoPayloadEntries: readonly UtxoPayloadEntry[];
    ledgerDelta: LedgerDelta;
    utxoPayloadAggregate: UtxoPayloadSizeAggregate;
    mempoolTxHashes: Buffer[];
    processedMempoolTxs: readonly Tx.EntryWithTimeStamp[];
    sizeOfProcessedTxs: number;
    rejectedMempoolTxsCount: number;
    rejectedMempoolTxHashes: readonly Buffer[];
    rejectionEntries: readonly TxRejectionsDB.EntryNoTimestamp[];
    includedDepositEntriesCount: number;
    includedDepositEntries: readonly DepositsDB.Entry[];
    includedDepositEventIds: readonly Buffer[];
    includedForcedTransactionEntriesCount: number;
    includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
    includedForcedTransactionEventIds: readonly Buffer[];
    includedWithdrawalEntriesCount: number;
    includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
    includedWithdrawalEventIds: readonly Buffer[];
    transitionTraceBuild: TransitionTraceBuildResult;
    nativeMpfReplay?: NativeMpfReplayBuild;
    nativeMpfHandle?: NativeMpfGenerationHandle;
  },
  MpfError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const nativeMpf = config?.nativeMpf;
    if ((ledgerMpf === undefined) === (nativeMpf === undefined)) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "ledger engine selection",
          new Error(
            "Exactly one of a local ledger MPF or Architecture G build context must be supplied",
          ),
        ),
      );
    }
    const processedMempoolTxs: Tx.EntryWithTimeStamp[] = [];
    const rejectedTxHashes: Buffer[] = [];
    const rejectionEntries: TxRejectionsDB.EntryNoTimestamp[] = [];
    const mempoolTxHashes: Buffer[] = [];
    const transactionOps: MpfBatchOp[] = [];
    const transactionSourceOps: MpfInsertBatchOp[] = [];
    const decodedMempoolTxs: DecodedMempoolTxForCommit[] = [];
    let txDeltaCacheHitCount = 0;
    let txDeltaFallbackDecodedCount = 0;
    let sizeOfProcessedTxs = 0;
    const txDeltaResolutionStartedAtMs = Date.now();
    const txDeltasByTxHash = yield* MempoolTxDeltasDB.retrieveByTxIds(
      mempoolTxs.map((entry) => entry[Tx.Columns.TX_ID]),
    );
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(mempoolTxs, (entry: Tx.EntryWithTimeStamp) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        const txHashHex = txHash.toString("hex");
        const existingDelta = txDeltasByTxHash.get(txHashHex);
        const resolved = yield* resolveTxDeltaForCommit(
          entry,
          existingDelta,
        ).pipe(Effect.withSpan("resolveTxDeltaForCommit"));
        if (resolved._tag === "Rejected") {
          rejectedTxHashes.push(Buffer.from(txHash));
          rejectionEntries.push(resolved.rejection);
          yield* Effect.logWarning(
            `Skipping malformed mempool tx ${txHashHex}: ${resolved.rejection[TxRejectionsDB.Columns.REJECT_DETAIL]}`,
          );
          return;
        }
        if (existingDelta === undefined) {
          txDeltaFallbackDecodedCount += 1;
        } else {
          txDeltaCacheHitCount += 1;
        }
        const { spent, produced } = resolved;
        decodedMempoolTxs.push({
          entry,
          txHash,
          txCbor,
          spent,
          produced,
        });
      }),
    );
    yield* Metric.incrementBy(
      commitTxDeltaCacheHitCounter,
      BigInt(txDeltaCacheHitCount),
    );
    yield* Metric.incrementBy(
      commitTxDeltaFallbackDecodedCounter,
      BigInt(txDeltaFallbackDecodedCount),
    );
    yield* logCommitMpfPhaseTiming(
      "tx_delta_resolution",
      txDeltaResolutionStartedAtMs,
      {
        candidate_tx_count: mempoolTxs.length,
        decoded_tx_count: decodedMempoolTxs.length,
        cache_hit_tx_count: txDeltaCacheHitCount,
        fallback_decoded_tx_count: txDeltaFallbackDecodedCount,
        rejected_tx_count: rejectedTxHashes.length,
      },
    );

    const effectiveEndTime = establishEffectiveEndTimeFromDecodedMempool(
      decodedMempoolTxs,
      config?.processedOnlyEndTime,
      config?.depositOnlyEndTime,
    );

    if (
      effectiveEndTime !== undefined &&
      config?.depositVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.depositVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DepositsDB.tableName,
          message:
            "Refusing to build a block because deposit ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},deposit_visibility_barrier_time=${config.depositVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    if (
      effectiveEndTime !== undefined &&
      config?.withdrawalVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() >
        config.withdrawalVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Refusing to build a block because withdrawal ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},withdrawal_visibility_barrier_time=${config.withdrawalVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    if (
      effectiveEndTime !== undefined &&
      config?.txOrderVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.txOrderVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Refusing to build a block because tx-order ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},tx_order_visibility_barrier_time=${config.txOrderVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    let includedDepositEntries: readonly DepositsDB.Entry[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedDepositEntries = yield* resolveIncludedDepositEntriesForWindow({
        currentBlockStartTime: config.currentBlockStartTime,
        effectiveEndTime,
        persistProjection: config.deferDatabaseWrites !== true,
      });
      includedDepositEntries = includedDepositEntries.filter(
        (entry) =>
          !config.excludedDepositEventIds?.has(
            entry[DepositsDB.Columns.ID].toString("hex"),
          ),
      );
    }
    const includedDepositEntriesCount = includedDepositEntries.length;
    const includedDepositEventIds = includedDepositEntries.map((entry) =>
      Buffer.from(entry[DepositsDB.Columns.ID]),
    );
    const depositLedgerEntries = yield* Effect.forEach(
      includedDepositEntries,
      DepositsDB.toLedgerEntry,
    );
    const sameBlockDepositOutputsByOutRef = new Map(
      depositLedgerEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );
    const rawInsertedLedgerOutputsByOutRef = new Map(
      sameBlockDepositOutputsByOutRef,
    );

    let includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[] =
      [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedForcedTransactionEntries =
        yield* resolveIncludedForcedTransactionEntriesForWindow({
          currentBlockStartTime: config.currentBlockStartTime,
          effectiveEndTime,
          persistProjection: config.deferDatabaseWrites !== true,
        });
      includedForcedTransactionEntries =
        includedForcedTransactionEntries.filter(
          (entry) =>
            !config.excludedForcedTransactionEventIds?.has(
              entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
            ),
        );
    }
    const includedForcedTransactionEntriesCount =
      includedForcedTransactionEntries.length;
    const includedForcedTransactionEventIds =
      includedForcedTransactionEntries.map((entry) =>
        Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]),
      );
    const shouldCheckPayloadRoot =
      (config?.payloadRootCheck ?? "every_block") === "every_block";
    const initialLedgerEntries =
      config?.initialLedgerEntries ??
      (shouldCheckPayloadRoot ? yield* ConfirmedLedgerDB.retrieve : []);
    const selectedLedgerOutputs =
      yield* indexSelectedLedgerOutputs(initialLedgerEntries);

    let includedWithdrawalEntries: readonly WithdrawalsDB.Entry[] = [];
    let classifiedWithdrawals: readonly ClassifiedWithdrawal[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedWithdrawalEntries =
        yield* resolveIncludedWithdrawalEntriesForWindow({
          currentBlockStartTime: config.currentBlockStartTime,
          effectiveEndTime,
        });
      includedWithdrawalEntries = includedWithdrawalEntries.filter(
        (entry) =>
          !config.excludedWithdrawalEventIds?.has(
            entry[WithdrawalsDB.Columns.ID].toString("hex"),
          ),
      );

      const seenWithdrawalTarget = new Map<string, Buffer>();
      const mutableClassifiedWithdrawals: ClassifiedWithdrawal[] = [];
      for (const entry of includedWithdrawalEntries) {
        const ledgerOutRef = yield* WithdrawalsDB.toLedgerOutRef(entry);
        const ledgerOutRefHex = ledgerOutRef.toString("hex");
        const priorWithdrawalEventId =
          seenWithdrawalTarget.get(ledgerOutRefHex);
        if (priorWithdrawalEventId !== undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to build a block because multiple withdrawals target the same L2 outref in one candidate window",
              cause: `l2_outref=${ledgerOutRefHex},first_event_id=${priorWithdrawalEventId.toString(
                "hex",
              )},duplicate_event_id=${entry[WithdrawalsDB.Columns.ID].toString(
                "hex",
              )}`,
            }),
          );
        }

        const selectedLedgerOutput = selectedLedgerOutputs.get(ledgerOutRefHex);
        const rawLedgerOutput =
          selectedLedgerOutput === undefined
            ? Option.none<Buffer>()
            : Option.some(Buffer.from(selectedLedgerOutput));
        const classifiedWithdrawal = yield* classifyWithdrawal({
          entry,
          ledgerOutRef,
          ledgerOutput: rawLedgerOutput,
        });
        mutableClassifiedWithdrawals.push(classifiedWithdrawal);
        seenWithdrawalTarget.set(
          ledgerOutRefHex,
          entry[WithdrawalsDB.Columns.ID],
        );
      }
      classifiedWithdrawals = mutableClassifiedWithdrawals;

      if (config.deferDatabaseWrites !== true) {
        yield* WithdrawalsDB.setSettlementInfoForEventIds(
          classifiedWithdrawals.map((classified) => ({
            eventId: classified.entry[WithdrawalsDB.Columns.ID],
            settlementEventInfo: classified.settlementEventInfo,
            validity: classified.validity,
            validityDetail: classified.validityDetail,
          })),
        );
        yield* WithdrawalsDB.markAwaitingAsProjected(
          classifiedWithdrawals.map(
            (classified) => classified.entry[WithdrawalsDB.Columns.ID],
          ),
        );
      }

      includedWithdrawalEntries = classifiedWithdrawals.map((classified) => ({
        ...classified.entry,
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
          classified.settlementEventInfo,
        [WithdrawalsDB.Columns.VALIDITY]: classified.validity,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: classified.validityDetail,
        [WithdrawalsDB.Columns.STATUS]:
          classified.entry[WithdrawalsDB.Columns.STATUS] ===
          WithdrawalsDB.Status.Awaiting
            ? WithdrawalsDB.Status.Projected
            : classified.entry[WithdrawalsDB.Columns.STATUS],
      }));
    }

    const validWithdrawalClassifications = classifiedWithdrawals.filter(
      (classified) => classified.shouldDeleteLedgerUtxo,
    );
    const withdrawnOutRefHexes = new Set(
      validWithdrawalClassifications.map((classified) =>
        classified.ledgerOutRef.toString("hex"),
      ),
    );
    const orderedDecodedMempoolTxs =
      yield* orderDecodedMempoolTxsForLedgerApplication(decodedMempoolTxs);

    const consensusProfile =
      config?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE;
    if (!isMidgardConsensusProfile(consensusProfile)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message: "Block construction requires the canonical V1 profile",
          cause: "non-v1 consensus profile",
        }),
      );
    }
    if (
      (includedForcedTransactionEntries.length > 0 ||
        orderedDecodedMempoolTxs.length > 0) &&
      (config?.forcedValidation === undefined || effectiveEndTime === undefined)
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "V1 transactions require an exact block-time validation context",
          cause: `forced_count=${includedForcedTransactionEntries.length.toString()},normal_count=${orderedDecodedMempoolTxs.length.toString()},effective_end_time=${effectiveEndTime?.toISOString() ?? "missing"}`,
        }),
      );
    }
    const forcedPreState = new Map(
      initialLedgerEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );
    for (const classifiedWithdrawal of validWithdrawalClassifications) {
      forcedPreState.delete(classifiedWithdrawal.ledgerOutRef.toString("hex"));
    }
    const classifiedForcedTransactions: readonly ClassifiedForcedTransaction[] =
      includedForcedTransactionEntries.length === 0
        ? []
        : yield* classifyForcedTransactions({
            entries: includedForcedTransactionEntries,
            initialState: forcedPreState,
            effectiveEndTime: effectiveEndTime!,
            consensusProfile,
            validation: config!.forcedValidation!,
            resolveProgramMaterialSidecar: programMaterialSidecarForEnvelopes,
          });
    includedForcedTransactionEntries = classifiedForcedTransactions.map(
      ({ entry }) => entry,
    );
    for (const classified of classifiedForcedTransactions) {
      for (const operation of classified.rawLedgerOps) {
        if (operation.type === "insert") {
          rawInsertedLedgerOutputsByOutRef.set(
            operation.key.toString("hex"),
            Buffer.from(operation.value),
          );
        }
      }
    }
    if (
      config?.deferDatabaseWrites !== true &&
      classifiedForcedTransactions.length > 0
    ) {
      yield* ForcedTransactionsDB.setProofClassifications(
        classifiedForcedTransactions.map(({ entry }) => ({
          txOrderId: entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          operatorValidity:
            entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY],
          forcedInclusionValue:
            entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
          programMaterialSidecarCbor:
            entry[
              ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
            ],
        })),
      );
    }
    const forcedLedgerOpsByEventId = new Map(
      classifiedForcedTransactions.map(({ entry, ledgerOps }) => [
        entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        ledgerOps,
      ]),
    );
    const forcedTransitionEffectsByEventId = new Map(
      classifiedForcedTransactions.map(({ entry, transitionEffect }) => [
        entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        transitionEffect,
      ]),
    );
    const forcedSpentOutRefHexes = new Set(
      [...forcedLedgerOpsByEventId.values()].flatMap((ops) =>
        ops.flatMap((op) =>
          op.type === "delete" ? [op.key.toString("hex")] : [],
        ),
      ),
    );
    const proofNormalLedgerOpsByTxId = new Map<string, readonly MpfBatchOp[]>();
    const proofNormalLedgerWitnessesByTxId = new Map<
      string,
      readonly ValidationMachineLedgerEntry[]
    >();
    const proofNormalLedgerMutationsByTxId = new Map<
      string,
      readonly ValidationMachineLedgerMutationStep[]
    >();
    const proofNormalProgramMaterialByTxId = new Map<string, Buffer>();

    yield* Effect.forEach(orderedDecodedMempoolTxs, (decoded) =>
      Effect.gen(function* () {
        const txHashHex = decoded.txHash.toString("hex");
        const withdrawnOutRef = decoded.spent.find((outRef) =>
          withdrawnOutRefHexes.has(outRef.toString("hex")),
        );
        if (withdrawnOutRef !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${withdrawnOutRef.toString(
                "hex",
              )}, which is consumed by a valid withdrawal in the same block window`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref consumed by a due withdrawal event.`,
          );
          return;
        }
        const forcedSpentOutRef = decoded.spent.find((outRef) =>
          forcedSpentOutRefHexes.has(outRef.toString("hex")),
        );
        if (forcedSpentOutRef !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_FORCED_TRANSACTION_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${forcedSpentOutRef.toString(
                "hex",
              )}, which is consumed by a valid forced transaction earlier in the same block`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref consumed by a valid forced transaction.`,
          );
          return;
        }
        const sameBlockDepositInput = decoded.spent.find((outRef) =>
          sameBlockDepositOutputsByOutRef.has(outRef.toString("hex")),
        );
        if (sameBlockDepositInput !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${sameBlockDepositInput.toString(
                "hex",
              )}, which is produced by a deposit that executes later in the same block window`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref produced by a due deposit event that executes after L2 transactions.`,
          );
          return;
        }

        mempoolTxHashes.push(decoded.txHash);
        processedMempoolTxs.push(decoded.entry);
        sizeOfProcessedTxs += decoded.txCbor.length;
        const transactionInsertOp = {
          type: "insert",
          key: decoded.txHash,
          value: encodeTransactionRootValue(
            decoded.txCbor,
            config?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE,
          ),
        } as const satisfies MpfInsertBatchOp;
        transactionOps.push(transactionInsertOp);
        transactionSourceOps.push(transactionInsertOp);
      }),
    );

    if (processedMempoolTxs.length > 0) {
      const validation = config!.forcedValidation!;
      const durableProgramMaterial =
        yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(
          processedMempoolTxs.map((entry) => entry[Tx.Columns.TX_ID]),
        );
      for (const material of durableProgramMaterial) {
        proofNormalProgramMaterialByTxId.set(
          material.txId.toString("hex"),
          Buffer.from(material.sidecarCbor),
        );
      }
      if (
        proofNormalProgramMaterialByTxId.size !== processedMempoolTxs.length ||
        processedMempoolTxs.some(
          (entry) =>
            !proofNormalProgramMaterialByTxId.has(
              entry[Tx.Columns.TX_ID].toString("hex"),
            ),
        )
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: TxAdmissionsDB.payloadTableName,
            message:
              "V1 validation requires one durable canonical CEK material sidecar per normal transaction",
            cause: `transactions=${processedMempoolTxs.length.toString()},sidecars=${proofNormalProgramMaterialByTxId.size.toString()}`,
          }),
        );
      }
      const proofPreState = new Map(
        initialLedgerEntries.map((entry) => [
          entry[Ledger.Columns.OUTREF].toString("hex"),
          Buffer.from(entry[Ledger.Columns.OUTPUT]),
        ]),
      );
      for (const classifiedWithdrawal of validWithdrawalClassifications) {
        proofPreState.delete(classifiedWithdrawal.ledgerOutRef.toString("hex"));
      }
      for (const classified of classifiedForcedTransactions) {
        for (const op of classified.rawLedgerOps) {
          if (op.type === "delete") {
            proofPreState.delete(op.key.toString("hex"));
          } else {
            proofPreState.set(op.key.toString("hex"), Buffer.from(op.value));
          }
        }
      }

      const proofPhaseA = yield* runPhaseAValidation(
        processedMempoolTxs.map((entry, index) => ({
          txId: entry[Tx.Columns.TX_ID],
          txCbor: entry[Tx.Columns.TX],
          arrivalSeq: BigInt(index),
          createdAt: entry[Tx.Columns.TIMESTAMPTZ],
          programMaterialSidecarCbor: proofNormalProgramMaterialByTxId.get(
            entry[Tx.Columns.TX_ID].toString("hex"),
          )!,
        })),
        {
          expectedNetworkId: validation.expectedNetworkId,
          minFeeA: validation.minFeeA,
          minFeeB: validation.minFeeB,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
          consensusProfile,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: MempoolDB.tableName,
              message: "V1 normal transaction Phase A failed",
              cause,
            }),
        ),
      );
      const proofPhaseB = yield* runPhaseBValidationWithPatch(
        proofPhaseA.accepted,
        proofPreState,
        {
          nowCardanoSlotNo: validation.slotForUnixTime(
            effectiveEndTime!.getTime(),
          ),
          bucketConcurrency: validation.bucketConcurrency,
          enforceScriptBudget: true,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: MempoolDB.tableName,
              message: "V1 normal transaction Phase B failed",
              cause,
            }),
        ),
      );
      const proofRejected = [...proofPhaseA.rejected, ...proofPhaseB.rejected];
      for (const rejected of proofRejected) {
        rejectedTxHashes.push(Buffer.from(rejected.txId));
        rejectionEntries.push({
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(rejected.txId),
          [TxRejectionsDB.Columns.REJECT_CODE]: rejected.code,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: rejected.detail,
        });
      }

      const acceptedByTxId = new Map(
        proofPhaseB.accepted.map((accepted) => [
          accepted.ledgerTx.txId.toString("hex"),
          accepted,
        ]),
      );
      const proofNormalReplayState = new Map(
        [...proofPreState.entries()].map(([outRef, output]) => [
          outRef,
          Buffer.from(output),
        ]),
      );
      const proofNormalMutationTrie = yield* Effect.tryPromise({
        try: () =>
          Trie.fromList(
            [...proofNormalReplayState.entries()].map(([key, value]) =>
              ledgerOutputToInsertBatchOp({
                outRef: Buffer.from(key, "hex"),
                outputCbor: value,
              }),
            ),
            new Store(undefined),
          ),
        catch: (cause) =>
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Failed to construct the normal-transaction validation mutation trie",
            cause,
          }),
      });
      processedMempoolTxs.length = 0;
      mempoolTxHashes.length = 0;
      transactionOps.length = 0;
      transactionSourceOps.length = 0;
      sizeOfProcessedTxs = 0;
      for (const decoded of orderedDecodedMempoolTxs) {
        const txIdHex = decoded.txHash.toString("hex");
        const accepted = acceptedByTxId.get(txIdHex);
        if (accepted === undefined) continue;
        const ledgerOps: readonly MpfBatchOp[] = [
          ...accepted.graph.spentOutRefHexes.map((outRef) => ({
            type: "delete" as const,
            key: Buffer.from(outRef, "hex"),
          })),
          ...accepted.graph.produced.map((produced) =>
            ledgerOutputToInsertBatchOp({
              outRef: produced[Ledger.Columns.OUTREF],
              outputCbor: produced[Ledger.Columns.OUTPUT],
            }),
          ),
        ];
        proofNormalLedgerOpsByTxId.set(txIdHex, ledgerOps);
        proofNormalLedgerWitnessesByTxId.set(
          txIdHex,
          validationLedgerWitnesses(proofNormalReplayState, [
            ...accepted.graph.spentOutRefHexes,
            ...accepted.graph.referenceOutRefHexes,
          ]),
        );
        proofNormalLedgerMutationsByTxId.set(
          txIdHex,
          yield* Effect.tryPromise({
            try: () =>
              applyValidationLedgerMutations(
                proofNormalMutationTrie,
                ledgerOps,
              ),
            catch: (cause) =>
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Failed to derive normal-transaction ledger mutation roots",
                cause,
              }),
          }),
        );
        for (const outRef of accepted.graph.spentOutRefHexes) {
          proofNormalReplayState.delete(outRef);
        }
        for (const produced of accepted.graph.produced) {
          rawInsertedLedgerOutputsByOutRef.set(
            produced[Ledger.Columns.OUTREF].toString("hex"),
            Buffer.from(produced[Ledger.Columns.OUTPUT]),
          );
          proofNormalReplayState.set(
            produced[Ledger.Columns.OUTREF].toString("hex"),
            Buffer.from(produced[Ledger.Columns.OUTPUT]),
          );
        }
        processedMempoolTxs.push(decoded.entry);
        mempoolTxHashes.push(Buffer.from(decoded.txHash));
        sizeOfProcessedTxs += decoded.txCbor.length;
        const transactionInsertOp = {
          type: "insert",
          key: Buffer.from(decoded.txHash),
          value: encodeTransactionRootValue(decoded.txCbor, consensusProfile),
        } as const satisfies MpfInsertBatchOp;
        transactionOps.push(transactionInsertOp);
        transactionSourceOps.push(transactionInsertOp);
      }
    }

    if (depositLedgerEntries.length > 0) {
      yield* Effect.logInfo(
        `🔹 Including ${depositLedgerEntries.length} projected deposit UTxO(s) in the deposit phase.`,
      );
    }

    if (validWithdrawalClassifications.length > 0) {
      yield* Effect.logInfo(
        `🔹 Including ${validWithdrawalClassifications.length} valid withdrawal event(s) in the withdrawal phase.`,
      );
    }

    if (rejectedTxHashes.length > 0 && config?.deferDatabaseWrites !== true) {
      yield* Effect.logWarning(
        `Dropping ${rejectedTxHashes.length} transaction(s) from MempoolDB`,
      );
      yield* persistCommitStageRejectedTransactions({
        rejectedTxHashes,
        rejectionEntries,
      });
    }

    const transactionRootBeforeApply = yield* transactionsMpf.root();
    const ledgerRootBeforeApply =
      ledgerMpf === undefined
        ? Buffer.from(nativeMpf!.handle.baseRoot, "hex")
        : yield* ledgerMpf.root();
    const ledgerRootBeforeApplyHex = ledgerRootBeforeApply.toString("hex");
    const selectedBaseUtxoRoot =
      config?.selectedBaseUtxoRoot ??
      (shouldCheckPayloadRoot
        ? yield* computeUtxoPayloadRoot(
            materializeUtxoPayloadEntries(
              initialLedgerEntries,
              [],
              rawInsertedLedgerOutputsByOutRef,
            ),
          )
        : ledgerRootBeforeApplyHex);
    if (selectedBaseUtxoRoot !== ledgerRootBeforeApplyHex) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the transition trace base UTxO snapshot root does not match the ledger MPF root",
          cause: `selected_base_utxos_root=${selectedBaseUtxoRoot},ledger_mpf_root=${ledgerRootBeforeApplyHex}`,
        }),
      );
    }
    const withdrawalSourceEvents = yield* Effect.forEach(
      includedWithdrawalEntries,
      (entry) =>
        Effect.gen(function* () {
          const eventKey = yield* withdrawalTraceEventKey(entry);
          const valid = entry[WithdrawalsDB.Columns.VALIDITY];
          const effect = canonicalCommittedWithdrawalTransitionEffect({
            committedValid: valid === WithdrawalsDB.Validity.WithdrawalIsValid,
            outRefCbor: yield* WithdrawalsDB.toLedgerOutRef(entry),
          });
          return {
            eventKey,
            phase: "Withdrawal" as const,
            ledgerOps: transitionEffectToLedgerOps(effect),
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const forcedTransactionSourceEvents = yield* Effect.forEach(
      includedForcedTransactionEntries,
      (entry) =>
        Effect.gen(function* () {
          const ledgerOps = forcedLedgerOpsByEventId.get(
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          );
          const effect = forcedTransitionEffectsByEventId.get(
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          );
          if (
            entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY] ===
              "TxIsValid" &&
            ledgerOps === undefined
          ) {
            return yield* Effect.fail(
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message:
                  "Refusing to build a transition trace for an effectful forced transaction before forced transaction ledger deltas are available",
                cause: `tx_order_id=${entry[
                  ForcedTransactionsDB.Columns.TX_ORDER_ID
                ].toString("hex")}`,
              }),
            );
          }
          return {
            eventKey: yield* forcedTransactionTraceEventKey(entry),
            phase: "ForcedTransaction" as const,
            ledgerOps:
              effect === undefined
                ? (ledgerOps ?? [])
                : transitionEffectToLedgerOps(effect),
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const decodedByTxHash = new Map(
      orderedDecodedMempoolTxs.map((decoded) => [
        decoded.txHash.toString("hex"),
        decoded,
      ]),
    );
    const l2TransactionSourceEvents = yield* Effect.forEach(
      processedMempoolTxs,
      (entry, index) =>
        Effect.gen(function* () {
          const txHash = entry[Tx.Columns.TX_ID];
          const decoded = decodedByTxHash.get(txHash.toString("hex"));
          if (decoded === undefined) {
            return yield* Effect.fail(
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Refusing to build a transition trace because an included transaction is missing decoded ledger deltas",
                cause: `source_index=${index.toString()},tx_id=${txHash.toString(
                  "hex",
                )}`,
              }),
            );
          }
          const proofLedgerOps = proofNormalLedgerOpsByTxId.get(
            txHash.toString("hex"),
          );
          if (proofLedgerOps === undefined) {
            return yield* Effect.fail(
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Refusing to build a V1 transition trace because an included transaction is missing its sequentially validated ledger delta",
                cause: `source_index=${index.toString()},tx_id=${txHash.toString(
                  "hex",
                )}`,
              }),
            );
          }
          return {
            eventKey: l2TransactionTraceEventKey(decoded.txHash),
            phase: "L2Transaction" as const,
            ledgerOps: proofLedgerOps,
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const depositSourceEvents = yield* Effect.forEach(
      includedDepositEntries,
      (entry) =>
        Effect.gen(function* () {
          const ledgerEntry = yield* DepositsDB.toLedgerEntry(entry);
          const effect = canonicalDepositTransitionEffect({
            outRefCbor: ledgerEntry[Ledger.Columns.OUTREF],
            outputCbor: ledgerEntry[Ledger.Columns.OUTPUT],
          });
          return {
            eventKey: yield* depositTraceEventKey(entry),
            phase: "Deposit" as const,
            ledgerOps: transitionEffectToLedgerOps(effect),
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const sourceEvents = [
      ...withdrawalSourceEvents,
      ...forcedTransactionSourceEvents,
      ...l2TransactionSourceEvents,
      ...depositSourceEvents,
    ];
    const transitionLedgerOps = sourceEvents.flatMap((event) =>
      event.ledgerOps.map((op) => ({ ...op })),
    );
    const baseUtxoPayloadAggregate =
      config?.baseUtxoPayloadAggregate ??
      ledgerPayloadAggregateFromEntries(initialLedgerEntries);
    const utxoPayloadAggregate =
      yield* applyLedgerOpsToUtxoPayloadAggregateFromFullValues(
        baseUtxoPayloadAggregate,
        transitionLedgerOps,
        new Map(
          initialLedgerEntries.map((entry) => [
            entry[Ledger.Columns.OUTREF].toString("hex"),
            Buffer.from(entry[Ledger.Columns.OUTPUT]),
          ]),
        ),
        rawInsertedLedgerOutputsByOutRef,
      );
    const txRootFiber = yield* buildTransactionsSourceRoot(
      transactionSourceOps,
      SDK.ROOT_DOMAINS.transactionsV1,
    ).pipe(Effect.fork);
    const architectureGTransactionMpfFiber =
      ledgerMpf === undefined
        ? yield* Effect.gen(function* () {
            const startedAtMs = Date.now();
            yield* transactionsMpf.applyBatch(transactionOps).pipe(
              Effect.catchAll((error) =>
                transactionsMpf.resetToRoot(transactionRootBeforeApply).pipe(
                  Effect.catchAll(() => Effect.void),
                  Effect.flatMap(() => Effect.fail(error)),
                ),
              ),
            );
            return { durationMs: Date.now() - startedAtMs };
          }).pipe(Effect.fork)
        : undefined;
    const transitionTraceStartedAtMs = Date.now();
    const transitionTraceBuild = yield* ledgerMpf === undefined
      ? buildNativeTransitionTraceResult({
          nativeMpf: nativeMpf!,
          sourceEvents,
          withdrawalCount: includedWithdrawalEntries.length,
          forcedTransactionCount: includedForcedTransactionEntries.length,
          l2TransactionCount: processedMempoolTxs.length,
          depositCount: includedDepositEntries.length,
        }).pipe(
          Effect.catchAll((error) =>
            Effect.gen(function* () {
              if (architectureGTransactionMpfFiber !== undefined) {
                yield* Fiber.interrupt(architectureGTransactionMpfFiber);
              }
              yield* transactionsMpf
                .resetToRoot(transactionRootBeforeApply)
                .pipe(Effect.catchAll(() => Effect.void));
              return yield* Effect.fail(error);
            }),
          ),
        )
      : buildTransitionTraceResult({
          ledgerMpf,
          sourceEvents,
          withdrawalCount: includedWithdrawalEntries.length,
          forcedTransactionCount: includedForcedTransactionEntries.length,
          l2TransactionCount: processedMempoolTxs.length,
          depositCount: includedDepositEntries.length,
        }).pipe(
          Effect.catchAll((error) =>
            ledgerMpf
              .resetToRoot(ledgerRootBeforeApply)
              .pipe(Effect.flatMap(() => Effect.fail(error))),
          ),
        );
    yield* logCommitMpfPhaseTiming(
      "transition_trace_build",
      transitionTraceStartedAtMs,
      {
        base_entry_count: initialLedgerEntries.length,
        source_event_count: sourceEvents.length,
        ledger_op_count: transitionLedgerOps.length,
        prefetch_ms: transitionTraceBuild.pathHydration.prefetchMs,
        prefetch_unique_paths: transitionTraceBuild.pathHydration.uniquePaths,
        prefetch_nodes_requested:
          transitionTraceBuild.pathHydration.nodesRequested,
        prefetch_hydration_hits:
          transitionTraceBuild.pathHydration.hydrationHits,
        prefetch_hydration_misses:
          transitionTraceBuild.pathHydration.hydrationMisses,
        prefetch_max_in_flight: transitionTraceBuild.pathHydration.maxInFlight,
        prefetch_max_batch_keys:
          transitionTraceBuild.pathHydration.maxBatchKeys,
        prefetch_retained_bytes_estimate:
          transitionTraceBuild.pathHydration.retainedBytesEstimate,
        hydration_chunk_count: transitionTraceBuild.pathHydration.chunkCount,
        arena_checkpoint_ms: transitionTraceBuild.pathHydration.checkpointMs,
        arena_authentication_ms:
          transitionTraceBuild.pathHydration.authenticationMs,
        arena_materialize_ms: transitionTraceBuild.pathHydration.materializeMs,
        arena_collapse_ms: transitionTraceBuild.pathHydration.collapseMs,
        arena_checkpoint_serialized_nodes:
          transitionTraceBuild.pathHydration.checkpointSerializedNodes,
        arena_checkpoint_serialized_bytes:
          transitionTraceBuild.pathHydration.checkpointSerializedBytes,
        arena_verified_upper_nodes:
          transitionTraceBuild.pathHydration.verifiedUpperNodes,
        arena_retained_upper_nodes:
          transitionTraceBuild.pathHydration.retainedUpperNodes,
        arena_collapsed_nodes:
          transitionTraceBuild.pathHydration.collapsedNodes,
        arena_peak_decoded_nodes:
          transitionTraceBuild.pathHydration.peakDecodedNodes,
      },
    );
    const utxoPayloadEntries = shouldCheckPayloadRoot
      ? materializeUtxoPayloadEntries(
          initialLedgerEntries,
          transitionLedgerOps,
          rawInsertedLedgerOutputsByOutRef,
        )
      : [];
    const transactionMpfApplyStartedAtMs = Date.now();
    const transactionMpfApplyDurationMs =
      architectureGTransactionMpfFiber === undefined
        ? yield* transactionsMpf.applyBatch(transactionOps).pipe(
            Effect.catchAll((error) =>
              Effect.gen(function* () {
                yield* transactionsMpf
                  .resetToRoot(transactionRootBeforeApply)
                  .pipe(Effect.catchAll(() => Effect.void));
                yield* ledgerMpf!
                  .resetToRoot(ledgerRootBeforeApply)
                  .pipe(Effect.catchAll(() => Effect.void));
                return yield* Effect.fail(error);
              }),
            ),
            Effect.map(() => Date.now() - transactionMpfApplyStartedAtMs),
          )
        : (yield* Fiber.join(architectureGTransactionMpfFiber)).durationMs;
    yield* logCommitMpfPhaseTiming(
      "transaction_mpf_apply",
      Date.now() - transactionMpfApplyDurationMs,
      {
        transaction_op_count: transactionOps.length,
        overlapped_with_transition_trace:
          architectureGTransactionMpfFiber === undefined ? 0 : 1,
      },
    );

    const rawTxRoot = yield* transactionsMpf.rootHex();
    const txRoot = yield* Fiber.join(txRootFiber);
    const utxoRoot =
      ledgerMpf === undefined
        ? nativeMpf!.candidateRoot!
        : yield* ledgerMpf.rootHex();
    if (shouldCheckPayloadRoot) {
      const payloadRootCheckStartedAtMs = Date.now();
      const payloadUtxoRoot = yield* computeUtxoPayloadRoot(utxoPayloadEntries);
      yield* logCommitMpfPhaseTiming(
        "payload_root_check",
        payloadRootCheckStartedAtMs,
        {
          payload_entry_count: utxoPayloadEntries.length,
        },
      );
      if (payloadUtxoRoot !== utxoRoot) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolLedgerDB.tableName,
            message:
              "Refusing to build a block because the DA payload UTxO snapshot root does not match the computed ledger MPF root",
            cause: `payload_utxos_root=${payloadUtxoRoot},computed_utxos_root=${utxoRoot}`,
          }),
        );
      }
    }
    if (transitionTraceBuild.finalUtxosRoot !== utxoRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the transition trace final UTxO root does not match the computed ledger MPF root",
          cause: `trace_final_utxos_root=${transitionTraceBuild.finalUtxosRoot},computed_utxos_root=${utxoRoot}`,
        }),
      );
    }

    const validationTraceBuild: ValidationTraceBuildResult = yield* Effect.gen(
      function* () {
        const expectedValidationTraceCount =
          includedForcedTransactionEntries.length + processedMempoolTxs.length;
        if (expectedValidationTraceCount === 0) {
          return {
            validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            validationTraceMembers: [],
            validationTraceCount: 0,
          };
        }
        const validation = config!.forcedValidation!;
        const validationTraceBuilder =
          config?.validationTraceBuilder ??
          buildDeterministicValidationTraceMembers;
        const traceByEventKey = new Map<
          string,
          RetainedTransitionTraceMember
        >();
        for (const member of transitionTraceBuild.transitionTraceMembers) {
          const keyHex = (yield* eventKeyCbor(member.value.event_key)).toString(
            "hex",
          );
          if (traceByEventKey.has(keyHex)) {
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Transition trace contains a duplicate validation-trace event key",
                cause: `event_key_cbor=${keyHex}`,
              }),
            );
          }
          traceByEventKey.set(keyHex, member);
        }
        const forcedByOrderId = new Map(
          classifiedForcedTransactions.map((classified) => [
            classified.entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
              "hex",
            ),
            classified,
          ]),
        );
        const forcedValidationInputs = yield* Effect.forEach(
          includedForcedTransactionEntries,
          (entry) =>
            Effect.gen(function* () {
              const eventKey = yield* forcedTransactionTraceEventKey(entry);
              const keyCbor = yield* eventKeyCbor(eventKey);
              const trace = traceByEventKey.get(keyCbor.toString("hex"));
              const classified = forcedByOrderId.get(
                entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
              );
              const canonicalTransactionCbor =
                entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR];
              if (
                trace === undefined ||
                classified === undefined ||
                canonicalTransactionCbor == null
              ) {
                return yield* Effect.fail(
                  new DatabaseError({
                    table: ForcedTransactionsDB.tableName,
                    message:
                      "V1 forced transaction is missing validation-trace source material",
                    cause: `tx_order_id=${entry[
                      ForcedTransactionsDB.Columns.TX_ORDER_ID
                    ].toString("hex")}`,
                  }),
                );
              }
              return {
                eventKey,
                transactionId: Buffer.from(
                  entry[ForcedTransactionsDB.Columns.TX_ID],
                ),
                canonicalTransactionCbor: Buffer.from(canonicalTransactionCbor),
                programMaterialSidecarCbor: Buffer.from(
                  classified.programMaterialSidecarCbor,
                ),
                sourceKind: "forced" as const,
                priorUtxosRoot: trace.value.pre_utxos_root,
                postUtxosRoot: trace.value.post_utxos_root,
                ledgerOps: classified.ledgerOps,
                ledgerWitnessEntries: classified.ledgerWitnessEntries,
                ledgerMutationSteps: classified.ledgerMutationSteps,
                verdict:
                  classified.rejectionCode === null
                    ? ("accepted" as const)
                    : ("rejected" as const),
                rejectionCode: classified.rejectionCode,
              };
            }),
        );
        const normalValidationInputs = yield* Effect.forEach(
          processedMempoolTxs,
          (entry) =>
            Effect.gen(function* () {
              const eventKey = l2TransactionTraceEventKey(
                entry[Tx.Columns.TX_ID],
              );
              const keyCbor = yield* eventKeyCbor(eventKey);
              const trace = traceByEventKey.get(keyCbor.toString("hex"));
              const ledgerOps = proofNormalLedgerOpsByTxId.get(
                entry[Tx.Columns.TX_ID].toString("hex"),
              );
              const ledgerWitnessEntries = proofNormalLedgerWitnessesByTxId.get(
                entry[Tx.Columns.TX_ID].toString("hex"),
              );
              const ledgerMutationSteps = proofNormalLedgerMutationsByTxId.get(
                entry[Tx.Columns.TX_ID].toString("hex"),
              );
              const programMaterialSidecarCbor =
                proofNormalProgramMaterialByTxId.get(
                  entry[Tx.Columns.TX_ID].toString("hex"),
                );
              if (
                trace === undefined ||
                ledgerOps === undefined ||
                ledgerWitnessEntries === undefined ||
                ledgerMutationSteps === undefined ||
                programMaterialSidecarCbor === undefined
              ) {
                return yield* Effect.fail(
                  new DatabaseError({
                    table: MempoolDB.tableName,
                    message:
                      "V1 normal transaction is missing validation-trace source material",
                    cause: `tx_id=${entry[Tx.Columns.TX_ID].toString("hex")}`,
                  }),
                );
              }
              return {
                eventKey,
                transactionId: Buffer.from(entry[Tx.Columns.TX_ID]),
                canonicalTransactionCbor: Buffer.from(entry[Tx.Columns.TX]),
                programMaterialSidecarCbor: Buffer.from(
                  programMaterialSidecarCbor,
                ),
                sourceKind: "normal" as const,
                priorUtxosRoot: trace.value.pre_utxos_root,
                postUtxosRoot: trace.value.post_utxos_root,
                ledgerOps,
                ledgerWitnessEntries,
                ledgerMutationSteps,
                verdict: "accepted" as const,
                rejectionCode: null,
              };
            }),
        );
        const validationInputs = [
          ...forcedValidationInputs,
          ...normalValidationInputs,
        ];
        const validationTraceMembers = yield* validationTraceBuilder({
          consensusProfile,
          blockEndTime: effectiveEndTime!,
          expectedNetworkId: validation.expectedNetworkId,
          minFeeA: validation.minFeeA,
          minFeeB: validation.minFeeB,
          blockSlot: validation.slotForUnixTime(effectiveEndTime!.getTime()),
          transactions: validationInputs,
        });
        yield* validateValidationTraceEventKeySet({
          expectedEventKeys: validationInputs.map(
            (transaction) => transaction.eventKey,
          ),
          transitionEventKeyCbors: new Set(traceByEventKey.keys()),
          members: validationTraceMembers,
        });
        const validationTracesRoot =
          validationTraceMembers.length === 0
            ? SDK.EMPTY_MERKLE_TREE_ROOT
            : yield* countedRootFromEncodedEntries(
                SDK.ROOT_DOMAINS.validationTraces,
                validationTraceMembers.map((member) => ({
                  key: member.keyCbor,
                  value: member.valueCbor,
                })),
              );
        return {
          validationTracesRoot,
          validationTraceMembers,
          validationTraceCount: validationTraceMembers.length,
        };
      },
    );

    yield* Effect.logInfo(
      `🔹 New raw transaction MPF root found: ${rawTxRoot}`,
    );
    yield* Effect.logInfo(`🔹 New transaction source root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
    yield* Effect.logInfo(
      `🔹 New transition trace root found: ${transitionTraceBuild.transitionTraceRoot}`,
    );
    yield* Effect.logInfo(
      `🔹 New event-to-step root found: ${transitionTraceBuild.eventToStepRoot}`,
    );
    yield* Effect.logInfo(
      `🔹 New validation traces root found: ${validationTraceBuild.validationTracesRoot}`,
    );

    const recordCorpusPath = config?.recordCorpusPath?.trim() ?? "";
    if (recordCorpusPath.length > 0) {
      const finalUtxoEntries = materializeUtxoPayloadEntries(
        initialLedgerEntries,
        transitionLedgerOps,
        rawInsertedLedgerOutputsByOutRef,
      );
      const deposits = includedDepositEntries.map((entry) => ({
        key: Buffer.from(entry[DepositsDB.Columns.ID]),
        value: Buffer.from(entry[DepositsDB.Columns.INFO]),
      }));
      const forcedTransactions = includedForcedTransactionEntries.map(
        (entry) => ({
          key: Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]),
          value: Buffer.from(
            entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
          ),
        }),
      );
      const withdrawals = includedWithdrawalEntries.flatMap((entry) => {
        const value = entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
        return value === null
          ? []
          : [
              {
                key: Buffer.from(entry[WithdrawalsDB.Columns.ID]),
                value: Buffer.from(value),
              },
            ];
      });
      const [depositsRoot, withdrawalsRoot, forcedTransactionsRoot] =
        yield* Effect.all(
          [
            countedRootFromEncodedEntries(SDK.ROOT_DOMAINS.deposits, deposits),
            countedRootFromEncodedEntries(
              SDK.ROOT_DOMAINS.withdrawals,
              withdrawals,
            ),
            countedRootFromEncodedEntries(
              SDK.ROOT_DOMAINS.forcedTransactionsV1,
              forcedTransactions,
            ),
          ],
          { concurrency: 1 },
        );
      const encodeEntry = (entry: {
        readonly key: Buffer;
        readonly value: Buffer;
      }): CorpusHexEntry => ({
        key: entry.key.toString("hex"),
        value: entry.value.toString("hex"),
      });
      const encodeOp = (op: MpfBatchOp): CorpusLedgerOp =>
        op.type === "insert"
          ? {
              type: "insert",
              key: op.key.toString("hex"),
              value: op.value.toString("hex"),
            }
          : { type: "delete", key: op.key.toString("hex") };
      const block: MpfReplayCorpusBlock = {
        version: 1,
        label: `commit-${Date.now().toString()}`,
        initialLedgerEntries: initialLedgerEntries.map((entry) =>
          encodeEntry(ledgerEntryToInsertBatchOp(entry)),
        ),
        sourceEvents: yield* Effect.forEach(sourceEvents, (event) =>
          eventKeyCbor(event.eventKey).pipe(
            Effect.map((encoded) => ({
              phase: event.phase,
              eventKeyCbor: encoded.toString("hex"),
              ledgerOps: event.ledgerOps.map(encodeOp),
            })),
          ),
        ),
        transactionOps: transactionSourceOps.map(encodeEntry),
        deposits: deposits.map(encodeEntry),
        withdrawals: withdrawals.map(encodeEntry),
        forcedTransactions: forcedTransactions.map(encodeEntry),
        finalUtxoEntries: finalUtxoEntries.map((entry) =>
          encodeEntry(
            ledgerOutputToInsertBatchOp({
              outRef: entry.outref,
              outputCbor: entry.output,
            }),
          ),
        ),
        expected: {
          utxoRoot,
          rawTxRoot,
          txRoot,
          transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
          eventToStepRoot: transitionTraceBuild.eventToStepRoot,
          depositsRoot,
          withdrawalsRoot,
          forcedTransactionsRoot,
          transitionRoots: transitionTraceBuild.transitionTraceMembers.map(
            (member) => ({
              pre: member.value.pre_utxos_root,
              post: member.value.post_utxos_root,
            }),
          ),
        },
      };
      yield* Effect.try({
        try: () => {
          FS.mkdirSync(dirname(recordCorpusPath), { recursive: true });
          FS.appendFileSync(recordCorpusPath, `${JSON.stringify(block)}\n`);
        },
        catch: (cause) =>
          MpfError.rootBuild("MPF replay corpus record tap", cause),
      });
    }

    const includedWithdrawalEntriesCount = includedWithdrawalEntries.length;
    const includedWithdrawalEventIds = includedWithdrawalEntries.map((entry) =>
      Buffer.from(entry[WithdrawalsDB.Columns.ID]),
    );
    const nativeMpfReplay: NativeMpfReplayBuild | undefined =
      nativeMpf === undefined
        ? undefined
        : {
            schema: 1,
            ownerBinarySha256: Buffer.from(nativeMpf.ownerBinarySha256, "hex"),
            baseRoot: Buffer.from(nativeMpf.handle.baseRoot, "hex"),
            candidateRoot: Buffer.from(nativeMpf.candidateRoot!, "hex"),
            eventLog: Buffer.from(nativeMpf.eventLog!),
            eventLogDigest: Buffer.from(nativeMpf.eventLogDigest!, "hex"),
            eventRoots: Buffer.from(nativeMpf.eventRoots!.join(""), "hex"),
            eventCount: nativeMpf.eventRoots!.length,
          };

    return {
      utxoRoot,
      rawTxRoot,
      txRoot,
      transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
      eventToStepRoot: transitionTraceBuild.eventToStepRoot,
      validationTracesRoot: validationTraceBuild.validationTracesRoot,
      transitionTraceMembers: transitionTraceBuild.transitionTraceMembers,
      eventToStepMembers: transitionTraceBuild.eventToStepMembers,
      validationTraceMembers: validationTraceBuild.validationTraceMembers,
      transitionStepCount: transitionTraceBuild.transitionStepCount,
      validationTraceCount: validationTraceBuild.validationTraceCount,
      totalEventCount: transitionTraceBuild.totalEventCount,
      utxoPayloadEntries,
      ledgerDelta: collapseLedgerDelta(
        transitionLedgerOps,
        rawInsertedLedgerOutputsByOutRef,
      ),
      utxoPayloadAggregate,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount: rejectedTxHashes.length,
      rejectedMempoolTxHashes: rejectedTxHashes,
      rejectionEntries,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedForcedTransactionEntriesCount,
      includedForcedTransactionEntries,
      includedForcedTransactionEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
      transitionTraceBuild,
      nativeMpfReplay,
      nativeMpfHandle: nativeMpf?.handle,
    };
  });

export const withMpfRootTransaction = <A, E, R>(
  mpf: MidgardMpf,
  eff: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    const beforeRoot = yield* mpf.root();
    return yield* eff.pipe(
      Effect.catchAll((e) =>
        Effect.gen(function* () {
          yield* mpf.resetToRoot(beforeRoot);
          return yield* Effect.fail(e);
        }),
      ),
    );
  });

export const withMpfRootTransactions = <A, E, R>(
  mpfs: readonly MidgardMpf[],
  eff: Effect.Effect<A, E, R>,
  shouldPreserveRoots: (value: A) => boolean,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    const beforeRoots = yield* Effect.forEach(mpfs, (mpf) => mpf.root(), {
      concurrency: "unbounded",
    });
    const resetRoots = Effect.forEach(
      mpfs,
      (mpf, index) => mpf.resetToRoot(beforeRoots[index]!),
      {
        discard: true,
        concurrency: "unbounded",
      },
    );

    const result = yield* Effect.either(eff);
    if (result._tag === "Left") {
      yield* resetRoots;
      return yield* Effect.fail(result.left);
    }
    if (!shouldPreserveRoots(result.right)) {
      yield* resetRoots;
    }
    return result.right;
  });

export const withMpfBlockOverlays = <A, E, R>(
  mpfs: readonly MidgardMpf[],
  eff: Effect.Effect<A, E, R>,
  shouldPromote: (value: A) => boolean,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    yield* Effect.forEach(mpfs, (mpf) => mpf.beginBlockOverlay(), {
      discard: true,
      concurrency: "unbounded",
    });
    const discard = Effect.forEach(
      mpfs,
      (mpf) => mpf.discardBlockOverlayIfActive(),
      {
        discard: true,
        concurrency: "unbounded",
      },
    );
    const result = yield* Effect.either(eff);
    if (result._tag === "Left") {
      yield* discard;
      return yield* Effect.fail(result.left);
    }
    if (shouldPromote(result.right)) {
      yield* Effect.forEach(
        mpfs,
        (mpf) =>
          Effect.gen(function* () {
            const root = yield* mpf.root();
            yield* mpf.flushBlockOverlay(root);
          }),
        { discard: true, concurrency: "unbounded" },
      );
    } else {
      yield* discard;
    }
    return result.right;
  });
