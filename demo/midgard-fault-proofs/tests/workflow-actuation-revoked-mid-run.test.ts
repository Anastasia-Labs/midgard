/** A permit revoked while a run is past its start hands the revocation back
 * to the caller, whichever stage it lands in, so the caller reads the run as
 * revoked, not as a stall of a proof that failed, and nothing more is
 * sent. */
import { describe, expect, it } from "vitest";

import { isWorkflowActuationRevokedError } from "../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { bindWorkflowRunActuation } from "../src/workflow/actuation-permit.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import type { FraudProofWorkflowTerminalVerifier } from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.output.js";
import { signedWorkflowTransactionFixture } from "./support/signed-workflow-transaction.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  terminal,
  terminalVerifier,
} from "./workflow.make-adapter.js";
import {
  admittedActuation,
  DEPLOYMENT,
} from "./workflow-runtime.admitted-actuation.js";

const ACTION = { actionId: "step-one", input: { actionKind: "step-one" } };

type Stage = "preflight" | "submit" | "terminal_verify";

/** A run whose permit is revoked while it is in `stage`. */
const scene = async (stage: Stage) => {
  const actuation = await admittedActuation();
  const { headerHash } = actuation;
  const objective = { category: "doubleSpend" as const, headerHash };
  const process = createTestWorkflowProcess();
  const revoke = () => actuation.revoke("fault_target_changed");
  const { signed } = await signedWorkflowTransactionFixture();
  const sent: string[] = [];
  const base = makeAdapter();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async () => {
      if (stage === "terminal_verify")
        return { kind: "completed" as const, terminal: terminal(headerHash) };
      // The target moves while the family reads L1, before preflight.
      if (stage === "preflight") revoke();
      return { kind: "action_required" as const, action: ACTION };
    },
    preflight: async ({ action }) =>
      bindWorkflowPreflightTransaction(
        Object.freeze({
          actionId: action.actionId,
          txHash: signed.toHash(),
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: "uplc-v1",
          },
          referenceScripts: [
            {
              role: "family-step",
              outRef: REFERENCE_OUT_REF,
              scriptHash: REFERENCE_SCRIPT_HASH,
            },
          ] as const,
        }),
        signed,
      ),
    submit: async ({ preflight }) => {
      sent.push(preflight.txHash);
      // The target moves while the sent transaction is submitted.
      if (stage === "submit") revoke();
      return { kind: "submitted" as const, txHash: preflight.txHash };
    },
  };
  const verifier: FraudProofWorkflowTerminalVerifier =
    stage === "terminal_verify"
      ? // The target moves while the landed proof is verified.
        terminalVerifierOf(async ({ candidate }) => {
          revoke();
          return candidate;
        })
      : terminalVerifier;
  const run = () =>
    runAdmittedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash,
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      // The run's own permit, as the watcher's runner binds it.
      run: bindWorkflowRunActuation({
        run: process.run(objective),
        permit: actuation.actuationPermit,
        decisionDigest: actuation.decisionDigest,
        deploymentFingerprint: DEPLOYMENT,
        category: "doubleSpend",
        headerHash,
      }),
      terminalVerifier: verifier,
      releaseFinality: {
        ...releaseFinality,
        deploymentIdentityDigest: DEPLOYMENT,
      },
    });
  return { sent, run };
};

describe("workflow: a permit revoked mid-run", () => {
  it.each(["preflight", "submit", "terminal_verify"] as const)(
    "hands the revocation back from %s",
    async (stage) => {
      const { sent, run } = await scene(stage);
      const attempt = () =>
        Promise.resolve()
          .then(run)
          .then(
            (result) => result,
            (error: unknown) => error,
          );
      // A revocation during a submit is handed back when the run next
      // asserts its permit, before it observes again.
      expect(isWorkflowActuationRevokedError(await attempt())).toBe(true);
      // Only the submit stage sent anything, before its permit was revoked.
      expect(sent).toHaveLength(stage === "submit" ? 1 : 0);
    },
  );
});
