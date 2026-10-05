import { formatUnknownError } from "@al-ft/midgard-core";

import { assertWorkflowJournalActuation } from "./actuation-permit.js";
import { reobserveWorkflowFundingReservationTransaction } from "./funding-reservation-permit.js";
import type {
  FraudProofWorkflowJournalEvent,
  FraudProofWorkflowJournalStore,
} from "./journal.js";
import { LocalKupmiosTransportUnavailableError } from "./local-kupmios-http-ogmios-source.js";
import { LocalKupmiosCheckpointChangedError } from "./local-kupmios-raw-l1-authority.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAdapterContext,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { normalizeTxHash } from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import { validateAction } from "./orchestrator.normalize-workflow-terminal.js";

export const reobserveRequiredWorkflowParent = async ({
  adapter,
  context,
  headerHash,
  journal,
  intent,
  hasUnresolvedDescendant,
  append,
  stalled,
  resumeOnObservation,
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
  readonly headerHash: string;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly intent: Extract<
    FraudProofWorkflowJournalEvent,
    { kind: "submission_intent" }
  >;
  readonly hasUnresolvedDescendant: boolean;
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<void>;
  readonly stalled: (reason: string) => Promise<FraudProofWorkflowRunResult>;
  readonly resumeOnObservation: (reason: string) => FraudProofWorkflowRunResult;
}): Promise<
  | { readonly kind: "included" }
  | { readonly kind: "reobserved" }
  | FraudProofWorkflowRunResult
> => {
  const { deploymentFingerprint, category } = context.identity;
  const { workflowId } = context;
  const identity = context.identity;
  const entries = context.entries;
  // A descendant can consume a parent's effects while its inclusion
  // remains canonical. Authenticate that inclusion without replay
  // authority before displacing the descendant's unresolved intent.
  let parentIncluded = false;
  if (hasUnresolvedDescendant) {
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
          actionId: intent.actionId,
          input: intent.actionInput,
        }),
        txHash: intent.txHash,
        ...(intent.durableRecovery === undefined
          ? {}
          : { durableRecovery: intent.durableRecovery }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_reconcile",
      });
    } catch (cause) {
      if (cause instanceof LocalKupmiosCheckpointChangedError)
        return resumeOnObservation(
          `reconciliation awaits a stable boundary for ${intent.actionId}: ${cause.message}`,
        );
      if (cause instanceof LocalKupmiosTransportUnavailableError) throw cause;
      return await stalled(
        `reconciliation failed for ${intent.actionId}: ${formatUnknownError(cause)}`,
      );
    }
    if (inclusion.kind === "confirmed") {
      if (
        normalizeTxHash(inclusion.txHash, "reconciled transaction hash") !==
        intent.txHash
      )
        return await stalled(
          `reconciliation for ${intent.actionId} returned ${inclusion.txHash}, expected ${intent.txHash}`,
        );
      parentIncluded = true;
    }
  }
  if (parentIncluded) return { kind: "included" };
  const fundingAvailable = await reobserveWorkflowFundingReservationTransaction(
    {
      journal,
      transactionHash: intent.txHash,
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
  assertWorkflowJournalActuation({
    journal,
    deploymentFingerprint,
    category,
    headerHash,
    checkpoint: "before_reconcile",
  });
  await append({
    kind: "reobserved",
    actionId: intent.actionId,
    txHash: intent.txHash,
  });
  return { kind: "reobserved" };
};
