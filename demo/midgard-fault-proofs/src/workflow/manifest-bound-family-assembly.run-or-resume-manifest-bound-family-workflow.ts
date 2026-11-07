import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type {
  FamilyCategory,
  ManifestBoundFamilyWorkflow,
} from "./family-definition.js";
import { observeFraudProofWorkflowHeader } from "./family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";

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
  journal,
}: {
  readonly workflow: ManifestBoundFamilyWorkflow<
    Category,
    Certificate,
    StepCount,
    Runtime
  >;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
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
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
