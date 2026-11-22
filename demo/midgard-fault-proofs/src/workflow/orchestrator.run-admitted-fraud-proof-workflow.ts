import { formatUnknownError } from "@al-ft/midgard-core";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { WorkflowActionChangedError } from "./action-changed.js";
import { workflowActuationDecisionDigest } from "./actuation-permit.assert-workflow-actuation-permit-identity.js";
import type { WorkflowActuationCheckpoint } from "./actuation-permit.create-workflow-reconciliation-permit-controller.js";
import {
  assertWorkflowRunActuation,
  workflowRunIsReconciliationOnly,
} from "./actuation-permit.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
  normalizeFraudProofWorkflowIdentity,
  normalizeWorkflowJson,
  type WorkflowJsonObject,
} from "./identity.js";
import { isCallerOwnedWorkflowFailure } from "./orchestrator.caller-owned-failure.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowAdapterContext,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowRegistry,
  type FraudProofWorkflowSubmitResult,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { normalizeTxHash } from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import {
  normalizeWorkflowTerminal,
  validateAction,
  validatePreflight,
} from "./orchestrator.normalize-workflow-terminal.js";
import { adapterWithMaterialsSweep } from "./orchestrator.proof-materials-sweep-adapter.js";
import { resolvedByAnotherRecordResult } from "./orchestrator.resolved-by-another-record.js";
import {
  createUnbuiltWorkflowActions,
  workflowRunOutcomes,
} from "./orchestrator.unbuilt-actions.js";
import { isFraudProofRawL1DerivationError } from "./raw-l1-family-derivation.state-queue-topology.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  type FraudProofResolvedByAnotherRecord,
  isFraudProofResolvedByAnotherRecordError,
} from "./resolved-by-another-record.js";
import { workflowPreflightTransaction } from "./transaction-boundary.js";
import { artifactPendingResult } from "./workflow-artifact-pending.js";
import {
  asWorkflowFundingShortfall,
  WorkflowFundingShortfallError,
  WorkflowFundingUnavailableError,
} from "./workflow-funding-shortfall.js";
import type { WorkflowRun } from "./workflow-run.js";

/**
 * Q51/W-O4 single-command core. Every step is derived from authenticated L1
 * observation: the ledger at the node's tip with the node's mempool applied
 * (`next-state.ts`). After a submit the next iteration observes again, so
 * an action whose transaction waits in the mempool is no longer required and
 * the next one is derived from, and spends, what that transaction makes. A
 * transaction that left the mempool without landing is simply absent: its
 * action is required again and built afresh. A submit the node refuses is an
 * ordinary failed attempt, and the next iteration observes again.
 */
export const runAdmittedFraudProofWorkflow = async ({
  hubOraclePolicyId,
  category,
  headerHash,
  prepareFamilyArtifact,
  registry,
  run,
  terminalVerifier,
  releaseFinality,
  maxActions = 64,
}: {
  readonly hubOraclePolicyId: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly prepareFamilyArtifact: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => Promise<WorkflowJsonObject>;
  readonly registry: FraudProofWorkflowRegistry;
  /** This invocation of the objective: its wallet wiring, and the actuation
   * permit bound to it. */
  readonly run: WorkflowRun;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
  if (!Number.isSafeInteger(maxActions) || maxActions < 1) {
    throw new Error("maxActions must be a positive safe integer");
  }
  if (
    terminalVerifier.verifierVersion !== FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER
  ) {
    throw new Error("workflow requires the authenticated L1 terminal verifier");
  }
  const { objectiveWallet } = run;
  if (
    objectiveWallet.category !== category ||
    objectiveWallet.headerHash !== headerHash
  )
    throw new Error("workflow objective wallet belongs to another objective");
  const adapter = adapterWithMaterialsSweep(registry, category, {
    objectiveWallet,
    releaseFinality,
  });
  const assertActuation = (checkpoint: WorkflowActuationCheckpoint): void =>
    assertWorkflowRunActuation({
      run,
      hubOraclePolicyId,
      category,
      headerHash,
      checkpoint,
    });
  assertActuation("workflow_resume");
  const reconciliationOnly = workflowRunIsReconciliationOnly(run);
  const decisionDigest = workflowActuationDecisionDigest(run);
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    hubOraclePolicyId,
    category,
    target: { kind: "state_queue_header", headerHash },
    ...(decisionDigest === undefined ? {} : { decisionDigest }),
  });
  const workflowId = computeFraudProofWorkflowId(identity);
  // An action that does not build is skipped for the rest of the run, so a
  // lower-ranked move still goes; every result carries it as an alarm.
  const unbuilt = createUnbuiltWorkflowActions();
  const { stalled, pending, resumeOnObservation } = workflowRunOutcomes({
    workflowId,
    identity,
    unbuilt,
  });
  // A stage the family cannot derive from the admitted L1 view stalls this
  // run and is observed again on its next one. Every other failure reaches
  // the caller as before.
  const observeOrStall = async (
    input: FraudProofWorkflowAdapterContext,
  ): Promise<
    | { readonly observation: FraudProofWorkflowObservation }
    | { readonly stalled: FraudProofWorkflowRunResult }
    | { readonly resolved: FraudProofResolvedByAnotherRecord }
  > => {
    try {
      return { observation: await adapter.observe(input) };
    } catch (cause) {
      if (isFraudProofResolvedByAnotherRecordError(cause))
        return { resolved: cause.resolution };
      if (!isFraudProofRawL1DerivationError(cause)) throw cause;
      return {
        stalled: stalled(`observation failed: ${formatUnknownError(cause)}`),
      };
    }
  };

  const resolvedByAnotherRecord = async (
    resolution: FraudProofResolvedByAnotherRecord,
  ): Promise<FraudProofWorkflowRunResult> =>
    // The orchestrator is the guard: no run resolves while the adapter still
    // sees a thread of ours on the header, or cannot list them at all.
    resolvedByAnotherRecordResult({
      resolution,
      threads:
        resolution.standingThread === undefined
          ? await adapter.observeOwnThreads?.({ headerHash })
          : { standing: resolution.standingThread },
      workflowId,
      identity,
      releaseFinality,
      resumeOnObservation,
    });

  let artifact: WorkflowJsonObject;
  try {
    artifact = normalizeWorkflowJson(
      await prepareFamilyArtifact(adapter),
      `${category} prepared artifact`,
    ) as WorkflowJsonObject;
  } catch (cause) {
    return artifactPendingResult(cause, resumeOnObservation);
  }

  for (let actionNumber = 0; actionNumber < maxActions; actionNumber += 1) {
    const context: FraudProofWorkflowAdapterContext = {
      reconciliationOnly,
      identity,
      workflowId,
      artifact,
      unbuiltActionIds: unbuilt.ids,
    };
    assertActuation("before_observe");
    const observed = await observeOrStall(context);
    if ("stalled" in observed) return observed.stalled;
    if ("resolved" in observed)
      return await resolvedByAnotherRecord(observed.resolved);
    const observation: FraudProofWorkflowObservation = observed.observation;
    if (observation.kind === "completed") {
      let terminal: FraudProofWorkflowTerminal;
      const inclusionOnly =
        observation.terminal.observedAt.confirmationDepth <=
        releaseFinality.policy.automaticRecoveryMaxDepth + 1;
      try {
        assertActuation("before_terminal_verify");
        const verified = await (
          inclusionOnly
            ? (terminalVerifier.verifyIncluded ?? terminalVerifier.verify)
            : terminalVerifier.verify
        )({
          identity,
          workflowId,
          releaseFinality,
          candidate: observation.terminal,
          artifact,
        });
        terminal = normalizeWorkflowTerminal({
          identity,
          terminal: verified,
          releaseFinality,
          inclusionOnly,
        });
        assertActuation("before_terminal_verify");
      } catch (cause) {
        if (isCallerOwnedWorkflowFailure(cause)) throw cause;
        return stalled(
          `terminal verification failed: ${formatUnknownError(cause)}`,
        );
      }
      return {
        kind: inclusionOnly ? "terminal_included" : "completed",
        workflowId,
        identity,
        terminal,
      };
    }
    if (observation.kind === "conflict") {
      return stalled(`chain conflict: ${observation.reason}`);
    }
    const held = unbuilt.held(observation);
    if (held !== undefined) return resumeOnObservation(held);
    if (observation.kind === "pending") return pending(observation.reason);
    const action = validateAction(observation.action);
    let preflight: FraudProofWorkflowPreflight;
    let adapterPreflightFailed = false;
    try {
      assertActuation("before_preflight");
      let captured: FraudProofWorkflowPreflight;
      try {
        captured = await adapter.preflight({ ...context, action });
      } catch (failure) {
        adapterPreflightFailed = true;
        throw await namedShortfall({
          adapter,
          context,
          action,
          cause: asWorkflowFundingShortfall(action.actionId, failure),
        });
      }
      preflight = validatePreflight({ action, preflight: captured });
    } catch (cause) {
      if (unbuilt.skip(cause)) continue;
      if (
        cause instanceof WorkflowActionChangedError ||
        cause instanceof WorkflowFundingUnavailableError
      )
        return resumeOnObservation(cause.message);
      if (isCallerOwnedWorkflowFailure(cause)) throw cause;
      return stalled(
        `preflight failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        adapterPreflightFailed ? "preflight" : undefined,
      );
    }
    if (workflowPreflightTransaction(preflight) === undefined)
      return stalled(
        `preflight of ${action.actionId} omitted its captured signed transaction`,
      );
    assertActuation("before_submit");
    let submitted: FraudProofWorkflowSubmitResult;
    try {
      submitted = await adapter.submit({ ...context, action, preflight });
    } catch (cause) {
      submitted = {
        kind: "ambiguous",
        detail: `submit threw: ${formatUnknownError(cause)}`,
      };
    }
    // Whatever the node answered, the next observation says what stands: a
    // transaction it took is seen in its mempool, and one it refused is not.
    const reported =
      submitted.txHash === undefined
        ? preflight.txHash
        : normalizeTxHash(submitted.txHash, "submitted transaction hash");
    if (reported !== preflight.txHash)
      return stalled(
        `submission for ${action.actionId} reported ${reported}, but its preflight built ${preflight.txHash}`,
      );
  }
  return resumeOnObservation(
    `workflow processed ${maxActions.toString()} actions; objective remains active`,
  );
};

/**
 * A preflight that failed for want of wallet funds, under the action's own
 * alarm where its adapter names one; naming it never turns the pause into a
 * stall.
 */
const namedShortfall = async ({
  adapter,
  context,
  action,
  cause,
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
  readonly action: FraudProofWorkflowAction;
  readonly cause: unknown;
}): Promise<unknown> => {
  if (
    !(cause instanceof WorkflowFundingShortfallError) ||
    adapter.unfunded === undefined
  )
    return cause;
  try {
    return await adapter.unfunded({ ...context, action, shortfall: cause });
  } catch (naming) {
    if (
      naming instanceof WorkflowActionChangedError ||
      isCallerOwnedWorkflowFailure(naming)
    )
      throw naming;
    return cause;
  }
};
