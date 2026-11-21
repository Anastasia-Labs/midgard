import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { DaLibp2pRetainedDaSource } from "../../src/transition-trace/fetch.da-libp2p-retained-da-source.js";
import type { WorkflowActuationPermit } from "../../src/workflow/actuation-permit.js";
import { defineFamilyApplication } from "../../src/workflow/family-application.assert-family-definition-roster.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowRunResult,
  FraudProofWorkflowTerminalVerifier,
} from "../../src/workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../../src/workflow/release-finality-policy.js";
import {
  createManifestBoundWorkflowRunner,
  WORKFLOW_RUNTIME_CONFIG,
} from "../../src/workflow/runtime.js";
import {
  emptyRosterReferenceScriptResolver,
  familyCommonInfrastructureForTest,
} from "./family-common-infrastructure.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

/**
 * Runs a workflow under its reconciliation permit through the manifest-bound
 * runner, as a restarted process does. The record
 * supplies the family's recovery surface; fresh execution and retained DA
 * are refused.
 */
export const prepareReconcileThroughRunner = ({
  category,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  permit,
  surface,
}: Readonly<{
  category: FraudProofCatalogueCategoryName;
  deploymentFingerprint: string;
  headerHash: string;
  decisionDigest: string;
  permit: WorkflowActuationPermit;
  surface: Readonly<{
    adapter: FraudProofFamilyWorkflowAdapter;
    terminalVerifier: FraudProofWorkflowTerminalVerifier;
    releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  }>;
}>): (() => Promise<FraudProofWorkflowRunResult>) => {
  // The invocation is ready now: a permit revoked after this point is
  // refused by the runner itself.
  const objectiveWallet = createTestWorkflowProcess().objectiveWallet({
    category,
    headerHash,
  });
  const runner = createManifestBoundWorkflowRunner({
    record: defineFamilyApplication({
      category,
      roster: {},
      requires: [],
      bindsDecisionDigest: false,
      bindConfig: () => undefined,
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint,
          definition: { category, headerHash },
        },
        adapter: surface.adapter,
        terminalVerifier: surface.terminalVerifier,
        releaseFinalityAuthority: surface.releaseFinalityAuthority,
      }),
      execute: async () => {
        throw new Error("reconciliation reached fresh execution");
      },
    }),
    loadRuntime: async () => ({
      schemaVersion: WORKFLOW_RUNTIME_CONFIG,
      infrastructure: familyCommonInfrastructureForTest({
        headerHash,
        decisionDigest,
      }),
      resolveReferenceScript: emptyRosterReferenceScriptResolver,
      retainedDaSources: [
        new DaLibp2pRetainedDaSource({
          deploymentFingerprint,
          peers: [{ peerId: "12D3KooWreconciliationOnly" }],
          transport: {
            request: async () => {
              throw new Error("reconciliation fetched retained DA");
            },
          },
        }),
      ],
      close: async () => undefined,
    }),
  });
  return async () =>
    (await runner.runOrResume({
      category,
      deploymentFingerprint,
      headerHash,
      decisionDigest,
      actuationPermit: permit,
      objectiveWallet,
      runtimeConfigPath: "/fixture/reconciliation-runtime.json",
    })) as FraudProofWorkflowRunResult;
};

/** Prepares and at once runs a workflow's reconciliation. */
export const reconcileThroughRunner = async (
  ...input: Parameters<typeof prepareReconcileThroughRunner>
): Promise<FraudProofWorkflowRunResult> =>
  await prepareReconcileThroughRunner(...input)();
