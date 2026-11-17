import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { DaLibp2pRetainedDaSource } from "../../src/transition-trace/fetch.js";
import type { WorkflowActuationPermit } from "../../src/workflow/actuation-permit.js";
import { defineFamilyApplication } from "../../src/workflow/family-application.js";
import { unsafeCreateWorkflowFundingReservationPermitForTest } from "../../src/workflow/funding-reservation-permit.js";
import type { FraudProofWorkflowJournalStore } from "../../src/workflow/journal.js";
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

/**
 * Resumes a saved workflow under its reconciliation permit through the
 * manifest-bound runner, the only path to recorded-workflow recovery. The
 * record supplies the family's recovery surface; fresh execution and retained
 * DA are refused.
 */
export const prepareReconcileThroughRunner = ({
  category,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  permit,
  store,
  surface,
}: Readonly<{
  category: FraudProofCatalogueCategoryName;
  deploymentFingerprint: string;
  headerHash: string;
  decisionDigest: string;
  permit: WorkflowActuationPermit;
  store: Pick<FraudProofWorkflowJournalStore, "load" | "append">;
  surface: Readonly<{
    adapter: FraudProofFamilyWorkflowAdapter;
    terminalVerifier: FraudProofWorkflowTerminalVerifier;
    releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  }>;
}>): (() => Promise<FraudProofWorkflowRunResult>) => {
  // Reserved now, as the host does before it invokes the runner: a permit
  // revoked after this point is refused by the runner itself.
  const fundingReservationPermit =
    unsafeCreateWorkflowFundingReservationPermitForTest({
      category,
      actuationPermit: permit,
      deploymentFingerprint,
      decisionDigest,
      rollbackGeneration: "0",
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
      mode: "resume",
      category,
      deploymentFingerprint,
      headerHash,
      decisionDigest,
      actuationPermit: permit,
      fundingReservationPermit,
      // A fresh view of the saved journal: each run binds its own.
      journal: {
        load: async (workflowId: string) => await store.load(workflowId),
        append: async (...entry: Parameters<typeof store.append>) =>
          await store.append(...entry),
      },
      runtimeConfigPath: "/fixture/reconciliation-runtime.json",
    })) as FraudProofWorkflowRunResult;
};

/** Prepares and at once runs a saved workflow's reconciliation. */
export const reconcileThroughRunner = async (
  ...input: Parameters<typeof prepareReconcileThroughRunner>
): Promise<FraudProofWorkflowRunResult> =>
  await prepareReconcileThroughRunner(...input)();
