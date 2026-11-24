import type { MessagePort } from "node:worker_threads";

import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import {
  CML,
  coreToUtxo,
  type SlotConfig,
  UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";

import type { CommitSubmitRefusalCode } from "../../fibers/block-commitment.worker-readiness.js";
import type { SlotAwareDueWork } from "../../fibers/slot-aware-due-work.js";
import type { LedgerDivergenceCheck } from "../../mpf/local-ledger-divergence.js";
import type { UnwrittenHold } from "../../services/intent-journal.holds.js";

export type WorkerInput = {
  readonly history?: import("../../services/follower-write-gate.js").FollowerWritePermit;
  /**
   * The frontier the main thread last verified `confirmed_ledger` derives
   * the root of (`confirmed-ledger-verification.ts`), at the generation it
   * read, or the frontier a clean fold advanced that verification to
   * (`confirmed-ledger-advance.ts`); built by `verifiedFrontierWorkerInput`
   * and read by `parseVerifiedFrontier`. Absent until a verification
   * passes, and after one finds a divergence.
   */
  readonly verifiedFrontier?: VerifiedFrontier;
  readonly nativeMpf?: {
    readonly port: MessagePort;
    readonly durableRoot: string;
    readonly ownerBinarySha256: string;
  };
  data: {
    availableConfirmedBlock: "" | SerializedStateQueueUTxO;
    currentBlockStartTimeMs: number;
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

/**
 * A frontier header (56 hex), the root of its post-state (64 hex) and the
 * frontier generation it was verified at (a positive `bigint`, in decimal
 * with no leading zero; migration 0038). The generation names the ledger
 * verified: a frontier unfolded and folded back to the same header and
 * root, or `confirmed_ledger` written outside a fold, carries another.
 */
export type VerifiedFrontier = Readonly<{
  headerHash: string;
  utxosRoot: string;
  generation: string;
}>;

/** `WorkerInput.verifiedFrontier` is not a frontier the parent could send. */
export class MalformedVerifiedFrontierError extends Data.TaggedError(
  "MalformedVerifiedFrontierError",
)<{ readonly detail: string }> {}

const HEADER_HASH_HEX = /^[0-9a-f]{56}$/u;
const ROOT_HEX = /^[0-9a-f]{64}$/u;
const GENERATION = /^[1-9][0-9]{0,18}$/u;
const GENERATION_MAX = 9_223_372_036_854_775_807n;

const isGeneration = (value: unknown): value is string =>
  typeof value === "string" &&
  GENERATION.test(value) &&
  BigInt(value) <= GENERATION_MAX;

/** The `WorkerInput` field the parent sends for `verified`: none while no
 * verification has passed. Only the frontier and its generation cross. */
export const verifiedFrontierWorkerInput = (
  verified: VerifiedFrontier | undefined,
): Pick<WorkerInput, "verifiedFrontier"> =>
  verified === undefined
    ? {}
    : {
        verifiedFrontier: {
          headerHash: verified.headerHash,
          utxosRoot: verified.utxosRoot,
          generation: verified.generation,
        },
      };

/** The verified frontier the worker received, `undefined` if none was sent.
 * Anything else fails: a payload the parent cannot produce is a broken
 * contract, not a frontier still awaiting verification. */
export const parseVerifiedFrontier = (
  value: unknown,
): Effect.Effect<
  VerifiedFrontier | undefined,
  MalformedVerifiedFrontierError
> => {
  if (value === undefined) return Effect.succeed(undefined);
  const pair = value as Partial<Record<keyof VerifiedFrontier, unknown>>;
  return typeof value === "object" &&
    value !== null &&
    Object.keys(value).sort().join() === "generation,headerHash,utxosRoot" &&
    typeof pair.headerHash === "string" &&
    HEADER_HASH_HEX.test(pair.headerHash) &&
    typeof pair.utxosRoot === "string" &&
    ROOT_HEX.test(pair.utxosRoot) &&
    isGeneration(pair.generation)
    ? Effect.succeed({
        headerHash: pair.headerHash,
        utxosRoot: pair.utxosRoot,
        generation: pair.generation,
      })
    : Effect.fail(
        new MalformedVerifiedFrontierError({
          detail: `verifiedFrontier is not exactly a 56-hex header hash, a 64-hex root and a decimal generation: ${JSON.stringify(value)}`,
        }),
      );
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
  /** Raw transaction MPF root. */
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
  /** Why the commit's submission was refused before broadcast, when it was
   * (`commit_submit_refused`): the attempt was abandoned and its payload
   * kept for the retry. */
  submitRefusal?: CommitSubmitRefusal;
};

/** A commit submission refused before broadcast: its bounded code and the
 * failure it was classified from. */
export type CommitSubmitRefusal = {
  readonly code: CommitSubmitRefusalCode;
  readonly detail: string;
};

export type NothingToCommitOutput = {
  type: "NothingToCommitOutput";
};

/**
 * `localFault` is set when the failure is a local fault: this process's
 * Ed25519 verifier fault (`LedgerSignatureVerifierUnavailableError`), or a
 * due withdrawal row this node cannot classify
 * (`WithdrawalUnclassifiableError`), whose event id (hex) is
 * `localFaultSubject` and whose case is `localFaultCase`. The parent retries
 * either for a bounded number of attempts without a successful commit before
 * the node exits.
 */
export type FailureOutput = {
  type: "FailureOutput";
  error: string;
} & LocalFaultMark;

/** `FailureOutput`'s local fault, if it is one. */
export type LocalFaultMark =
  | {
      localFault?: undefined;
      localFaultSubject?: undefined;
      localFaultCase?: undefined;
    }
  | {
      localFault: typeof VERIFIER_UNAVAILABLE_FAULT;
      localFaultSubject?: undefined;
      localFaultCase?: undefined;
    }
  | {
      localFault: typeof WITHDRAWAL_UNCLASSIFIABLE_FAULT;
      localFaultSubject: string;
      localFaultCase: WithdrawalUnclassifiableCase;
    };

/** Why a due withdrawal row cannot be classified on this node: the row, or
 * the ledger output it names, does not decode; or the row decodes and the
 * classifier failed on this node. */
export type WithdrawalUnclassifiableCase = "undecodable" | "classifier_failed";

/** `FailureOutput.localFault` of an Ed25519 verifier fault. */
export const VERIFIER_UNAVAILABLE_FAULT = "verifier_unavailable";
/** `FailureOutput.localFault` of a withdrawal row this node cannot classify. */
export const WITHDRAWAL_UNCLASSIFIABLE_FAULT = "withdrawal_unclassifiable";

export type RegisteredDueWorkOutput = {
  type: "RegisteredDueWorkOutput";
  dueWork: SlotAwareDueWork;
};

/** The commit waits for its base: a tail landed-block processing has not
 * applied to the working ledger yet, or this node's own block still in
 * flight. With `divergence`, the base's entries do not derive the root this
 * node stores for it (`local-ledger-divergence.ts`, by that check): the
 * commit is refused until they do. With `unverified`, the base is a landed
 * tail and the frontier it builds from is not the verified frontier the
 * worker received: it waits for the main thread's verification. */
export type AwaitingCommitBaseOutput = {
  readonly type: "AwaitingCommitBaseOutput";
  readonly baseHeaderHash: string;
  readonly detail: string;
  readonly divergence?: LedgerDivergenceCheck;
  readonly unverified?: true;
};

/** The commit waits for the next scheduler window: a dead journal whose
 * commit was signed holds this block's header hash (identical content on
 * the same base, built in the same window). Its signed bytes can still land,
 * and landing applies that journal, so it is kept; the next window's block
 * end time gives the replacement another header. */
export type AwaitingNextCommitWindowOutput = {
  readonly type: "AwaitingNextCommitWindowOutput";
  readonly heldHeaderHash: string;
  readonly detail: string;
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

export type WorkerOutput =
  | SuccessfulSubmissionOutput
  | SkippedSubmissionOutput
  | NothingToCommitOutput
  | FailureOutput
  | RegisteredDueWorkOutput
  | AwaitingCommitBaseOutput
  | AwaitingNextCommitWindowOutput
  | SubmittedAwaitingConfirmationOutput;

/**
 * Posted by the commit worker ahead of its output, as soon as a commit-stage
 * rejection's mempool_ledger revert has committed: the parent reloads its
 * ledger cache without waiting for the block submission to finish.
 */
export type MempoolLedgerRevertedNotice = {
  readonly type: "MempoolLedgerRevertedNotice";
};

/**
 * Posted by the commit worker ahead of its output: a commit stage held the
 * build (`held`), or its rejections reached the build's limit and it left the
 * rest pending (`breaker`). The parent names it on `/readyz`.
 */
export type CommitBuildAlarmNotice = {
  readonly type: "CommitBuildAlarmNotice";
  readonly alarm: "held" | "breaker";
  readonly stage: string;
};

/**
 * Posted by the commit worker ahead of its output when its intent journal
 * refused a submission and the refusal hold's write to the node database
 * has not landed. The worker's journal ends with the thread, so the
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
  SDKStateQueue.StateQueueUTxO,
  "utxo" | "datum"
> & { utxo: string; datum: string };

export const serializeStateQueueUTxO = (
  stateQueueUTxO: SDKStateQueue.StateQueueUTxO,
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
  SDKStateQueue.StateQueueUTxO,
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
