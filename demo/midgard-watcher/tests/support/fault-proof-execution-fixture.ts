/**
 * The watcher's fault-proof execution adapter over one classified doubleSpend
 * decision: its objective resumed under that decision, with the workflow
 * runner, completion check and operations sink as mocks.
 */
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { WorkflowAdapterRunnerInput } from "@al-ft/midgard-fault-proofs/workflow/adapters";
import { WORKFLOW_RUNNER_FACTORIES } from "@al-ft/midgard-fault-proofs/workflow/runtime";
import { type Mock, vi } from "vitest";

import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import {
  classifiedDoubleSpend,
  deploymentIdentity,
} from "./classified-fault-fixture.js";

export const setupFaultProofExecution = async ({
  installedRunner = true,
}: {
  readonly installedRunner?: boolean;
} = {}) => {
  const { fresh } = await classifiedDoubleSpend();
  const controller = createWorkflowActuationPermitController({
    decision: fresh,
    rollbackGeneration: "2",
  });
  const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
    throw new Error("the execution adapter never builds through the runner");
  });
  const runOrResume: Mock<
    (invocation: WorkflowAdapterRunnerInput) => Promise<unknown>
  > = vi.fn(async () => ({
    kind: "pending",
    resumeOnObservation: true,
    reason: "signed transaction awaiting inclusion",
  }));
  const recordProofStep: Mock = vi.fn();
  const setAlert: Mock = vi.fn();
  const verifyCompleted: Mock<() => Promise<never>> = vi.fn(async () => {
    throw new Error("unexpected completed verification");
  });
  const execution = createWatcherFaultProofExecution({
    application: {
      runners: installedRunner ? { doubleSpend: runner } : {},
      runOrResume,
      verifyCompleted,
    },
    runtimeConfigPath: "/unused-test-runtime.json",
    deploymentFingerprint: deploymentIdentity.manifestId,
    operationsSink: { recordProofStep, setAlert },
  });
  const input = {
    job: {
      mode: "resume",
      category: "doubleSpend",
      headerHash: fresh.headerHash,
      decisionDigest: fresh.decisionDigest,
      rollbackGeneration: "2",
      deadline: null,
    },
    actuationPermit: controller.permit,
    admission: { mode: "resume" },
  } as const;
  return {
    fresh,
    controller,
    execution,
    input,
    /** The admission of a fresh run. */
    runAdmission: { mode: "run" } as const,
    runOrResume,
    verifyCompleted,
    recordProofStep,
    setAlert,
  };
};
