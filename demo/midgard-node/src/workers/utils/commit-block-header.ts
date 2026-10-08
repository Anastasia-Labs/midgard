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

import type { SlotAwareDueWork } from "../../fibers/slot-aware-due-work.js";
import type { UnwrittenHold } from "../../services/intent-journal.holds.js";

export type WorkerInput = {
  readonly history?: import("../../services/follower-write-gate.js").FollowerWritePermit;
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

export type CommitCandidateRoots = {
  readonly utxos: string;
  /** Raw transaction MPF root retained by the local finalization path. */
  readonly rawTransactions: string;
  readonly transactions: string;
  readonly transitionTrace: string;
  readonly eventToStep: string;
};

export type SkippedSubmissionOutput = {
  type: "SkippedSubmissionOutput";
  mempoolTxsCount: number;
  sizeOfProcessedTxs: number;
  /** The block built and then deferred because no confirmed base was
   * available: its end time and the roots already computed while building it
   * (user-event roots are resolved only at submission). Absent when
   * submission failed. */
  candidate?: {
    readonly endTimeMs: number;
    readonly roots: CommitCandidateRoots;
  };
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

/** The commit waits for its base: a foreign tail landed-block processing
 * has not applied to the working ledger yet. */
export type AwaitingCommitBaseOutput = {
  readonly type: "AwaitingCommitBaseOutput";
  readonly baseHeaderHash: string;
  readonly detail: string;
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
  | AwaitingCommitBaseOutput
  | SubmittedAwaitingLocalFinalizationOutput
  | SubmittedAwaitingConfirmationOutput
  | SuccessfulLocalFinalizationRecoveryOutput;

/**
 * Posted by the commit worker ahead of its output, as soon as a commit-stage
 * rejection's mempool_ledger revert has committed: the parent reloads its
 * ledger cache without waiting for the block submission to finish.
 */
export type MempoolLedgerRevertedNotice = {
  readonly type: "MempoolLedgerRevertedNotice";
};

/**
 * Posted by the commit worker ahead of its output when its intent journal
 * refused a submission and the refusal hold's write to the node database
 * has not landed (I1-H1). The worker's journal ends with the thread, so the
 * parent's journal takes the holds over: `/readyz` names them, and its
 * refresh at every tip writes them until they land.
 */
export type IntentRefusalHoldsNotice = {
  readonly type: "IntentRefusalHoldsNotice";
  readonly holds: readonly UnwrittenHold[];
};

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
          message: `Failed to serialize state queue UTxO: ${String(e)}`,
          cause: e,
        }),
    });
    const datumCBOR = yield* Effect.try({
      try: () => SDK.encodeLinkedListNodeView(stateQueueUTxO.datum),
      catch: (e) =>
        new SDK.CborSerializationError({
          message: `Failed to serialize state queue datum: ${String(e)}`,
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
          message: `Failed to convert state queue UTxO to CML: ${String(e)}`,
          cause: e,
        }),
    });
    const d = yield* SDK.getLinkedListNodeViewFromUTxO(u).pipe(
      Effect.mapError(
        (e) =>
          new SDK.CborDeserializationError({
            message: `Failed to deserialize datum: ${e.message}`,
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
