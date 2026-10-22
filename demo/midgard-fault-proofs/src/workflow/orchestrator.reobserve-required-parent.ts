import { formatUnknownError } from "@al-ft/midgard-core";

import { assertWorkflowJournalActuation } from "./actuation-permit.js";
import {
  readWorkflowFundingRecovery,
  reobserveWorkflowFundingReservationTransaction,
  type WorkflowJournalWriter,
} from "./funding-reservation-permit.js";
import type {
  FraudProofWorkflowJournalEvent,
  FraudProofWorkflowJournalStore,
} from "./journal.js";
import {
  FraudProofL1CheckpointChangedError,
  isFraudProofL1WaitError,
} from "./l1-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAdapterContext,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { normalizeTxHash } from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import { validateAction } from "./orchestrator.normalize-workflow-terminal.js";

type Submission = Extract<
  FraudProofWorkflowJournalEvent,
  { kind: "submission_intent" }
>;

/** Reads, without replay authority, whether `submission` is on chain. */
const readSubmissionOnChain = async ({
  adapter,
  context,
  headerHash,
  journal,
  submission,
  signedTransactionCborHex,
  stalled,
  resumeOnObservation,
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
  readonly headerHash: string;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly submission: Submission;
  readonly signedTransactionCborHex?: string;
  readonly stalled: (reason: string) => Promise<FraudProofWorkflowRunResult>;
  readonly resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult;
}): Promise<boolean | FraudProofWorkflowRunResult> => {
  const { deploymentFingerprint, category } = context.identity;
  let inclusion: FraudProofWorkflowReconcileResult;
  try {
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_reconcile",
    });
    inclusion = await adapter.reconcile({
      ...context,
      reconciliationOnly: true,
      action: validateAction({
        actionId: submission.actionId,
        input: submission.actionInput,
      }),
      txHash: submission.txHash,
      ...(signedTransactionCborHex === undefined
        ? {}
        : { signedTransactionCborHex }),
      ...(submission.durableRecovery === undefined
        ? {}
        : { durableRecovery: submission.durableRecovery }),
    });
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_reconcile",
    });
  } catch (cause) {
    if (cause instanceof FraudProofL1CheckpointChangedError)
      return resumeOnObservation(
        `reconciliation awaits a stable boundary for ${submission.actionId}: ${cause.message}`,
      );
    if (isFraudProofL1WaitError(cause)) throw cause;
    return await stalled(
      `reconciliation failed for ${submission.actionId}: ${formatUnknownError(cause)}`,
    );
  }
  if (inclusion.kind !== "confirmed") return false;
  if (
    normalizeTxHash(inclusion.txHash, "reconciled transaction hash") !==
    submission.txHash
  )
    return await stalled(
      `reconciliation for ${submission.actionId} returned ${inclusion.txHash}, expected ${submission.txHash}`,
    );
  return true;
};

export const reobserveRequiredWorkflowParent = async ({
  adapter,
  context,
  headerHash,
  writer,
  intent,
  hasUnresolvedDescendant,
  intentReadAbsent = false,
  supersedeDisplacedAttempt,
  stalled,
  resumeOnObservation,
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
  readonly headerHash: string;
  readonly writer: WorkflowJournalWriter;
  readonly intent: Extract<
    FraudProofWorkflowJournalEvent,
    { kind: "submission_intent" }
  >;
  readonly hasUnresolvedDescendant: boolean;
  /** The caller has just read `intent` as absent from the chain. */
  readonly intentReadAbsent?: boolean;
  /** Retires the pending later attempt that reobserving `intent` displaces. */
  readonly supersedeDisplacedAttempt: () => Promise<void>;
  readonly stalled: (reason: string) => Promise<FraudProofWorkflowRunResult>;
  readonly resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult;
}): Promise<
  | { readonly kind: "included" }
  | { readonly kind: "reobserved" }
  | FraudProofWorkflowRunResult
> => {
  const { deploymentFingerprint, category } = context.identity;
  const { journal } = writer;
  const { workflowId } = context;
  const identity = context.identity;
  const entries = context.entries;
  const onChain = async (
    submission: Submission,
    signedTransactionCborHex?: string,
  ) =>
    await readSubmissionOnChain({
      adapter,
      context,
      headerHash,
      journal,
      submission,
      ...(signedTransactionCborHex === undefined
        ? {}
        : { signedTransactionCborHex }),
      stalled,
      resumeOnObservation,
    });
  // A descendant can consume a parent's effects while its inclusion
  // remains canonical. Authenticate that inclusion without replay
  // authority before displacing the descendant's unresolved intent.
  let resolvedOnChain = false;
  if (hasUnresolvedDescendant && !intentReadAbsent) {
    const included = await onChain(intent);
    if (typeof included !== "boolean") return included;
    resolvedOnChain = included;
  }
  // Owner ruling (whichever lands wins): the pending later attempt is
  // superseded only while it has not landed. Once on chain it is the result
  // for its action and is reconciled as such: superseding it would let a
  // fresh attempt for the same action land beside it.
  if (!resolvedOnChain && hasUnresolvedDescendant) {
    const recovery = await readWorkflowFundingRecovery(journal);
    const displaced = recovery.transition;
    const submission =
      displaced === null || displaced.transactionHash === intent.txHash
        ? undefined
        : [...entries]
            .reverse()
            .map(({ event }) => event)
            .find(
              (event): event is Submission =>
                event.kind === "submission_intent" &&
                event.txHash === displaced.transactionHash,
            );
    if (displaced !== null && submission !== undefined) {
      const landed = await onChain(
        submission,
        displaced.signedTransactionCborHex,
      );
      if (typeof landed !== "boolean") return landed;
      resolvedOnChain = landed;
    }
  }
  if (resolvedOnChain) return { kind: "included" };
  await supersedeDisplacedAttempt();
  assertWorkflowJournalActuation({
    journal,
    deploymentFingerprint,
    category,
    headerHash,
    checkpoint: "before_reconcile",
  });
  const fundingAvailable = await reobserveWorkflowFundingReservationTransaction(
    {
      writer,
      transactionHash: intent.txHash,
      events: [
        {
          kind: "reobserved",
          actionId: intent.actionId,
          txHash: intent.txHash,
        },
      ],
    },
  );
  if (!fundingAvailable)
    return {
      kind: "pending",
      resumeOnObservation: true,
      workflowId,
      identity,
      entries,
      reason:
        "Required inputs remain reserved by another unresolved transaction",
    };
  return { kind: "reobserved" };
};
