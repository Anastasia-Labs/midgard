import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  slotAlignedLowerBoundAtOrAfter,
  type SlotClock,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type Network } from "@lucid-evolution/lucid";

import {
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  type StateQueueMutationLeaseCoordinator,
} from "./remove-fraudulent-block.js";
import {
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionJournalStore,
  type TimeoutCorrectionStepReconciliation,
  type TimeoutCorrectionTransactionStatus,
} from "./remove-unattested-block.parse-timeout-correction-journal.js";
import {
  reconcileLastTimeoutCorrectionStep,
  type TimeoutCorrectionRecovery,
} from "./remove-unattested-block.reconcile-last-timeout-correction-step.js";
import { type ResolvedProverSigner } from "./runtime.js";
import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
} from "./workflow/local-kupmios-http-ogmios-source.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./workflow/release-finality-policy.js";
import {
  reconcileSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./workflow/signed-transaction-reconciliation.js";

/** Same admitted, canonical signed-attempt recovery used by proof workflows. */
export const createLocalKupmiosTimeoutCorrectionRecovery = (input: {
  readonly deploymentManifest: unknown;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly network: Network;
}): TimeoutCorrectionRecovery => {
  const manifest = verifyFinalizedDeploymentManifest(input.deploymentManifest);
  if (manifest.network !== input.network)
    throw new Error(
      "Timeout recovery network differs from its finalized deployment.",
    );
  const policy = manifest.l1Finality;
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifest.manifestId,
    blueprintHash: manifest.artifacts.blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  });
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: `attestation-timeout:${releaseFinality.deploymentIdentityDigest}`,
    kupoHttpUrl: input.kupoUrl,
    ogmiosUrl: input.ogmiosUrl,
    releaseFinality,
    observationDepth: "inclusion",
  });
  return {
    observeSignedTransaction: (signed) =>
      readAdmittedLocalKupmiosSignedTransactionRecovery({ source, ...signed }),
    rebroadcastSignedTransaction: (signed) =>
      rebroadcastAdmittedLocalKupmiosSignedTransaction({ source, ...signed }),
  };
};

/** Bare not_found/failed is ambiguous; only admitted canonical observations retire attempts. */
export const recoverTimeoutCorrectionAttempt = async (input: {
  readonly journal: TimeoutCorrectionJournal;
  readonly queue: readonly StateQueueUTxO[];
  readonly transactionStatus: TimeoutCorrectionTransactionStatus;
  readonly allowRebroadcast?: boolean;
  readonly recovery?: TimeoutCorrectionRecovery;
  readonly authorizeResubmission: (
    signed: SignedWorkflowTransaction,
  ) => Promise<void>;
}): Promise<TimeoutCorrectionStepReconciliation> => {
  const step = input.journal.steps.find(
    (entry) => entry.status === "prepared" || entry.status === "submitted",
  );
  if (step === undefined)
    return { disposition: "none", journal: input.journal };
  let canonical: SignedTransactionRecoveryObservation | undefined;
  const result = await reconcileSignedWorkflowTransaction({
    transactionHash: step.txHash,
    signedTransactionCborHex: step.signedCbor,
    observe:
      input.recovery === undefined
        ? undefined
        : async (signed) => {
            canonical = await input.recovery!.observeSignedTransaction(signed);
            return canonical;
          },
    rebroadcast:
      input.allowRebroadcast === false
        ? undefined
        : input.recovery?.rebroadcastSignedTransaction,
    authorizeResubmission: input.authorizeResubmission,
  });
  if (result.kind === "conflict")
    throw new Error(`Timeout correction canonical conflict: ${result.reason}`);
  const status =
    canonical?.status === "included"
      ? "confirmed"
      : result.kind === "not_found" &&
          // Supersession at the tip is not retirement here.
          result.retirement !== undefined &&
          (canonical?.status === "expired" ||
            canonical?.status === "invalidated")
        ? canonical.status
        : input.recovery === undefined &&
            input.transactionStatus === "confirmed"
          ? "confirmed"
          : "unknown";
  return reconcileLastTimeoutCorrectionStep(input.journal, input.queue, status);
};

export type SubmitUnattestedTimeoutCorrectionParams = {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly journalStore: TimeoutCorrectionJournalStore;
  readonly awaitConfirmation?: boolean;
  readonly nowMs?: () => number;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  readonly recovery?: TimeoutCorrectionRecovery;
};

/**
 * The validity range of a timeout-correction transaction built at `nowMs`.
 *
 * The validator requires `inclusive lower bound >= header.end_time +
 * da_attestation_timeout`. Block end times usually end in 999 ms, and the
 * ledger presents the lower bound as the start of its slot, so the deadline
 * itself is rounded up to the next slot boundary before it is used as the
 * earliest admissible lower bound. The lower bound is otherwise backdated to
 * tolerate submit latency and provider clock skew.
 */
export const resolveTimeoutCorrectionValidityRange = (
  slotClock: SlotClock,
  deadlineMs: bigint,
  nowMs: bigint,
): { readonly validFrom: bigint; readonly validTo: bigint } => {
  const earliestAdmitted = slotAlignedLowerBoundAtOrAfter(
    slotClock,
    deadlineMs,
  );
  const backdated = nowMs - STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
  const validFrom = backdated > earliestAdmitted ? backdated : earliestAdmitted;
  return {
    validFrom,
    validTo: validFrom + STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  };
};
