import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { createManifestBoundValueConservationWorkflow } from "./workflow.create-manifest-bound-value-conservation-workflow.js";

export type ManifestBoundValueConservationWorkflow = Awaited<
  ReturnType<typeof createManifestBoundValueConservationWorkflow>
>;

export const runOrResumeManifestBoundValueConservationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundValueConservationWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
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
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
