/** A loaded workflow runtime over the family common infrastructure, and a
 * doubleSpend family record and workflow, for the runtime-boundary tests. */
import { DaLibp2pRetainedDaSource } from "../src/transition-trace/fetch.da-libp2p-retained-da-source.js";
import { defineFamilyApplication } from "../src/workflow/family-application.assert-family-definition-roster.js";
import type { FamilyApplicationWorkflowIdentity } from "../src/workflow/family-application.js";
import {
  type LoadedWorkflowRuntime,
  WORKFLOW_RUNTIME_CONFIG,
} from "../src/workflow/runtime.js";
import {
  emptyRosterReferenceScriptResolver,
  familyCommonInfrastructureForTest,
  unreachableRecoverySurfaceForTest,
} from "./support/family-common-infrastructure.js";
import { DEPLOYMENT } from "./workflow-runtime.admitted-actuation.js";

export const retainedDaSource = (): DaLibp2pRetainedDaSource =>
  new DaLibp2pRetainedDaSource({
    deploymentFingerprint: DEPLOYMENT,
    peers: [{ peerId: "12D3KooWproductionRuntimeTest" }],
    transport: {
      request: async () => {
        throw new Error("transport is not called by runtime-boundary test");
      },
    },
  });

export const loadedRuntime = (
  actuation: Readonly<{ headerHash: string; decisionDigest: string }>,
  overrides: Partial<LoadedWorkflowRuntime> = {},
): LoadedWorkflowRuntime => ({
  schemaVersion: WORKFLOW_RUNTIME_CONFIG,
  infrastructure: familyCommonInfrastructureForTest(actuation),
  resolveReferenceScript: emptyRosterReferenceScriptResolver,
  retainedDaSources: [retainedDaSource()],
  close: async () => undefined,
  ...overrides,
});

export type DoubleSpendIdentity =
  FamilyApplicationWorkflowIdentity<"doubleSpend">;

/** A doubleSpend workflow whose recovery surface these runs never reach. */
export const recoverableDoubleSpendWorkflow = (
  headerHash: string,
  hubOraclePolicyId: string = DEPLOYMENT,
) => ({
  binding: {
    hubOraclePolicyId,
    definition: { category: "doubleSpend" as const, headerHash },
  },
  ...unreachableRecoverySurfaceForTest,
});

/** A doubleSpend record with an empty roster and no requirements. */
export const doubleSpendRecord = <Config, Workflow extends DoubleSpendIdentity>(
  record: Partial<
    Parameters<
      typeof defineFamilyApplication<"doubleSpend", Config, Workflow>
    >[0]
  > &
    Pick<
      Parameters<
        typeof defineFamilyApplication<"doubleSpend", Config, Workflow>
      >[0],
      "bindConfig" | "constructWorkflow" | "execute"
    >,
) =>
  defineFamilyApplication<"doubleSpend", Config, Workflow>({
    category: "doubleSpend",
    roster: {},
    requires: [],
    bindsDecisionDigest: false,
    ...record,
  });
