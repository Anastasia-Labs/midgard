/** The `executionSourceScriptDecoding` family definition and its
 * manifest-bound runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { defineFamily } from "../workflow/family-definition.js";
import {
  type FraudProofFamilyL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  createExecutionSourceScriptDecodingTransactionPort,
  executionSourceScriptDecodingWorkflowContracts,
  type ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
} from "./workflow.transaction-port.js";

export type ManifestBoundExecutionSourceScriptDecodingWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"executionSourceScriptDecoding">;
  l1: FraudProofFamilyL1ObservationPort<"executionSourceScriptDecoding">;
  transactions: CursorFamilyTransactionPort<"executionSourceScriptDecoding">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

const S = FraudProofComputationThreadStepDatum;

export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily({
  category: "executionSourceScriptDecoding",
  stepDatumSchemas: [S, S, S],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofExecutionSourceScriptDecoding",
      "fraudProofExecutionSourceScriptDecodingBindEvent",
      "fraudProofExecutionSourceScriptDecodingTerminal",
    ],
    transactionPort: (context) =>
      createExecutionSourceScriptDecodingTransactionPort({
        ...context,
        contracts: executionSourceScriptDecodingWorkflowContracts(
          context.binding,
          context.certificate,
        ),
      }),
  },
});

export const createManifestBoundExecutionSourceScriptDecodingWorkflow = (
  config: ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
): Promise<ManifestBoundExecutionSourceScriptDecodingWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundExecutionSourceScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }): Promise<FraudProofWorkflowRunResult> =>
    await runFraudProofWorkflowFromRetainedDa({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      observation: await observeFraudProofWorkflowHeader(workflow.l1, {
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
      replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      replayContext: workflow.replayContext,
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflow.adapter],
        launchScope: ["executionSourceScriptDecoding"],
      }),
      journal,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
