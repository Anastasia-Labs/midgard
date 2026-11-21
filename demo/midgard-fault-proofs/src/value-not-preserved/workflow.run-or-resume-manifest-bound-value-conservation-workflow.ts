import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.create-complete-canonical-replay-union.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { WorkflowRun } from "../workflow/workflow-run.js";
import { createManifestBoundValueConservationWorkflow } from "./workflow.create-manifest-bound-value-conservation-workflow.js";

export type ManifestBoundValueConservationWorkflow = Awaited<
  ReturnType<typeof createManifestBoundValueConservationWorkflow>
>;

export const runOrResumeManifestBoundValueConservationWorkflow = async ({
  workflow,
  sources,
  run,
}: {
  readonly workflow: ManifestBoundValueConservationWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly run: WorkflowRun;
}) =>
  runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
    ...(workflow.replayContext === undefined
      ? {}
      : { replayContext: workflow.replayContext }),
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["valueNotPreserved"],
    }),
    run,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
