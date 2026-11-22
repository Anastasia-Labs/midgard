import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type {
  FamilyCategory,
  ManifestBoundFamilyWorkflow,
} from "./family-definition.js";
import { observeFraudProofWorkflowHeader } from "./family-l1-observation.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { WorkflowRun } from "./workflow-run.js";

/**
 * Runs or resumes an assembled workflow: observes its header, then hands the
 * single-adapter registry, the definition's replayer and the workflow's
 * terminal verifier and release-finality authority to the retained-DA runner.
 */
export const runOrResumeManifestBoundFamilyWorkflow = async <
  Category extends FamilyCategory,
  Certificate extends boolean,
  StepCount extends number = number,
  Runtime extends object = Readonly<Record<never, never>>,
>({
  workflow,
  sources,
  run,
}: {
  readonly workflow: ManifestBoundFamilyWorkflow<
    Category,
    Certificate,
    StepCount,
    Runtime
  >;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly run: WorkflowRun;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    hubOraclePolicyId: workflow.binding.hubOraclePolicyId,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: workflow.replayer,
    ...(workflow.replayContext === undefined
      ? {}
      : { replayContext: workflow.replayContext }),
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [workflow.definition.category],
    }),
    run,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
