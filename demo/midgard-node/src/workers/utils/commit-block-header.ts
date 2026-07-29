import type { MessagePort } from "node:worker_threads";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToUtxo,
  type SlotConfig,
  UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SlotAwareDueWork } from "@/fibers/slot-aware-due-work.js";
import type {
  SpeculativeCandidateSummary,
  SpeculativeInvalidationReason,
  UserEventBarrierWatermarks,
} from "@/fibers/speculative-commit-state.js";

export type SpeculativeCommitBaseInput = {
  readonly headerHash: string;
  readonly utxosRoot: string;
  readonly blockEndTimeMs: number;
  readonly submittedTxHash: string;
};

export type WorkerInput = {
  readonly nativeMpf?: {
    readonly port: MessagePort;
    readonly durableRoot: string;
    readonly ownerBinarySha256: string;
  };
  data: {
    availableConfirmedBlock: "" | SerializedStateQueueUTxO;
    availableLocalFinalizationBlock: "" | SerializedStateQueueUTxO;
    currentBlockStartTimeMs: number;
    localFinalizationPending: boolean;
    /**
     * Parent-generated identity for the logical PostgreSQL ledger-MPF lease.
     * The parent uses the same identity to release a lease only after the
     * worker thread has stopped, including timeout and interruption paths.
     */
    ledgerStoreLeaseOwner: string;
    /**
     * Immutable, node-selected time mapping used by canonical V1 forced
     * validation. Plain data keeps candidate construction provider-free.
     */
    forcedValidationSlotConfig?: SlotConfig;
    mempoolTxsCountSoFar: number;
    sizeOfProcessedTxsSoFar: number;
    stateQueueLeaseToken?: string;
    baseSnapshotId?: string;
    stateQueueHasUnmergedTail?: boolean;
    speculativeBuild?: {
      readonly base: SpeculativeCommitBaseInput;
      readonly watermarks: UserEventBarrierWatermarks;
      /** Payload already committed by the submitted base block. */
      readonly excludedMempoolTxIds: readonly string[];
      readonly excludedDepositEventIds: readonly string[];
      readonly excludedForcedTransactionEventIds: readonly string[];
      readonly excludedWithdrawalEventIds: readonly string[];
    };
  };
};

export type NativeMpfPromotion = {
  readonly handle: {
    readonly ownerEpoch: Uint8Array;
    readonly generationId: Uint8Array;
    readonly baseRoot: string;
  };
};

export type SuccessfulSubmissionOutput = {
  type: "SuccessfulSubmissionOutput";
  submittedTxHash: string;
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
  blockEndTimeMs: number;
  mempoolLedgerDeletedOutRefHexes: readonly string[];
  nativeMpfPromotion?: NativeMpfPromotion;
};

export type SkippedSubmissionOutput = {
  type: "SkippedSubmissionOutput";
  mempoolTxsCount: number;
  sizeOfProcessedTxs: number;
};

export type NothingToCommitOutput = {
  type: "NothingToCommitOutput";
};

export type FailureOutput = {
  type: "FailureOutput";
  error: string;
};

export type RegisteredDueWorkOutput = {
  type: "RegisteredDueWorkOutput";
  dueWork: SlotAwareDueWork;
};

export type AwaitingForeignDaOutput = {
  readonly type: "AwaitingForeignDaOutput";
  readonly foreignHeaderHash: string;
  readonly reason: string;
};

export type SubmittedAwaitingLocalFinalizationOutput = {
  type: "SubmittedAwaitingLocalFinalizationOutput";
  submittedTxHash: string;
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
  blockEndTimeMs: number;
  error: string;
  submittedHeaderHash: string;
  submittedUtxosRoot: string;
  nativeMpfPromotion?: NativeMpfPromotion;
};

export type SubmittedAwaitingConfirmationOutput = {
  type: "SubmittedAwaitingConfirmationOutput";
  submittedTxHash: string;
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
  blockEndTimeMs: number;
  submittedHeaderHash: string;
  submittedUtxosRoot: string;
  nativeMpfPromotion?: NativeMpfPromotion;
  speculativeExecution?: {
    readonly candidateId: string;
    readonly baseHydrationPassesBeforeReady: number;
    readonly mpfProcessingPassesBeforeReady: number;
    readonly baseHydrationPassesAfterReady: number;
    readonly mpfProcessingPassesAfterReady: number;
  };
};

export type SpeculativeCandidateReadyOutput = {
  readonly type: "SpeculativeCandidateReadyOutput";
  readonly candidate: SpeculativeCandidateSummary;
};

export type SpeculativeCandidateInvalidatedOutput = {
  readonly type: "SpeculativeCandidateInvalidatedOutput";
  readonly candidateId: string;
  readonly reason: SpeculativeInvalidationReason;
};

export type SpeculativeCommitWorkerInstruction =
  | {
      readonly type: "SubmitSpeculativeCandidate";
      readonly confirmedBlock: SerializedStateQueueUTxO;
      readonly stateQueueLeaseToken: string;
      readonly baseSnapshotId: string;
      readonly stateQueueHasUnmergedTail: boolean;
      readonly localFinalizationBlock?: SerializedStateQueueUTxO;
    }
  | {
      readonly type: "InvalidateSpeculativeCandidate";
      readonly reason: SpeculativeInvalidationReason;
    };

export type SuccessfulLocalFinalizationRecoveryOutput = {
  type: "SuccessfulLocalFinalizationRecoveryOutput";
  finalizedHeaderHash: string;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
  mempoolLedgerDeletedOutRefHexes: readonly string[];
};

export type WorkerOutput =
  | SuccessfulSubmissionOutput
  | SkippedSubmissionOutput
  | NothingToCommitOutput
  | FailureOutput
  | RegisteredDueWorkOutput
  | AwaitingForeignDaOutput
  | SubmittedAwaitingLocalFinalizationOutput
  | SubmittedAwaitingConfirmationOutput
  | SpeculativeCandidateReadyOutput
  | SpeculativeCandidateInvalidatedOutput
  | SuccessfulLocalFinalizationRecoveryOutput;

// Datatype to use CBOR hex of state queue UTxOs instead of `UTxO` from LE for
// transferability.
export type SerializedStateQueueUTxO = Omit<
  SDK.StateQueueUTxO,
  "utxo" | "datum"
> & { utxo: string; datum: string };

export const serializeStateQueueUTxO = (
  stateQueueUTxO: SDK.StateQueueUTxO,
): Effect.Effect<
  SerializedStateQueueUTxO,
  SDK.CmlUnexpectedError | SDK.CborSerializationError
> =>
  Effect.gen(function* () {
    const core: CML.TransactionUnspentOutput = yield* Effect.try({
      try: () => utxoToCore(stateQueueUTxO.utxo),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to serialize state queue UTxO: ${e}`,
          cause: e,
        }),
    });
    const datumCBOR = yield* Effect.try({
      try: () => SDK.encodeLinkedListNodeView(stateQueueUTxO.datum),
      catch: (e) =>
        new SDK.CborSerializationError({
          message: `Failed to serialize state queue datum: ${e}`,
          cause: e,
        }),
    });
    return {
      ...stateQueueUTxO,
      utxo: core.to_cbor_hex(),
      datum: datumCBOR,
    };
  });

export const deserializeStateQueueUTxO = (
  stateQueueUTxO: SerializedStateQueueUTxO,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.CmlUnexpectedError | SDK.CborDeserializationError
> =>
  Effect.gen(function* () {
    const u: UTxO = yield* Effect.try({
      try: () =>
        coreToUtxo(
          CML.TransactionUnspentOutput.from_cbor_hex(stateQueueUTxO.utxo),
        ),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to convert state queue UTxO to CML: ${e}`,
          cause: e,
        }),
    });
    const d = yield* SDK.getLinkedListNodeViewFromUTxO(u).pipe(
      Effect.mapError(
        (e) =>
          new SDK.CborDeserializationError({
            message: `Failed to deserialize datum: ${e}`,
            cause: e,
          }),
      ),
    );
    return {
      ...stateQueueUTxO,
      utxo: u,
      datum: d,
    };
  });
