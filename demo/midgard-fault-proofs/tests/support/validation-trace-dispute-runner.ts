/** Drives the validation-trace dispute workflow on the Open scene through the
 * shared orchestrator, one emulator block per run. */
import { createValidationTraceDisputeWorkflowAdapter } from "../../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceDisputeDeployment } from "../../src/validation-trace-dispute/transactions.js";
import {
  type FraudProofWorkflowJournalStore,
  MemoryFraudProofWorkflowJournalStore,
} from "../../src/workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflow,
} from "../../src/workflow/orchestrator.js";
import {
  DEPLOYMENT_FINGERPRINT,
  releaseFinalityAuthority,
  terminalVerifier,
} from "../workflow.make-adapter.js";
import { network } from "./submit-init-emulator-shared.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import {
  type Block,
  BOND_LOVELACE,
  detectOn,
  type Scene,
} from "./validation-trace-dispute-open-scene.js";

/** The deployment the runner builds against, from the scene's contracts. */
export const runnerDeployment = (
  scene: Scene,
): ValidationTraceDisputeDeployment => {
  const { h, chain } = scene;
  const category = h.catalogue.categories.validationTraceDispute;
  if (category === undefined)
    throw new Error("catalogue has no validation-trace category");
  return {
    network,
    blueprint: h.realBlueprint,
    init: {
      contracts: {
        steps: [h.contracts.fraudProofs.validationTraceDispute],
        computationThread: h.contracts.computationThread,
        hubOraclePolicyId: h.contracts.hubOracle.policyId,
        stateQueuePolicyId: h.contracts.stateQueue.policyId,
      },
      category,
      catalogue: {
        policyId: h.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          h.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: h.catalogue.root,
      },
      witnessReferenceScripts: h.witnessReferenceScripts,
    },
    stateQueue: {
      policyId: h.contracts.stateQueue.policyId,
      spendingScriptAddress: h.contracts.stateQueue.spendingScriptAddress,
    },
    chain,
    references: scene.references,
    bondLovelace: BOND_LOVELACE,
  };
};

export type Runner = Readonly<{
  journal: FraudProofWorkflowJournalStore;
  /** One orchestrator run over `block`'s detections, then one block. */
  runOnce: (block: Block) => Promise<FraudProofWorkflowRunResult>;
}>;

export const makeRunner = (
  scene: Scene,
  options: {
    readonly journal?: FraudProofWorkflowJournalStore;
    readonly signedRecovery?: FraudProofSignedTransactionRecovery;
  } = {},
): Runner => {
  const { h } = scene;
  const journal = options.journal ?? new MemoryFraudProofWorkflowJournalStore();
  const adapter = createValidationTraceDisputeWorkflowAdapter({
    lucid: h.proverLucid,
    signer: h.proverSigner,
    deployment: runnerDeployment(scene),
    l1SlotConfig,
    ...(options.signedRecovery === undefined
      ? {}
      : { signedRecovery: options.signedRecovery }),
    now: () => h.emulator.now(),
  });
  const registry = createFraudProofWorkflowRegistry({
    adapters: [adapter],
    launchScope: ["validationTraceDispute"],
  });
  return {
    journal,
    runOnce: async (block) => {
      const result = await runFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        evidence: block.evidence,
        detections: await detectOn(block),
        ...(block.context === undefined
          ? {}
          : { replayContext: block.context }),
        registry,
        journal,
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
        now: () => new Date(h.emulator.now()),
      });
      h.emulator.awaitBlock(1);
      return result;
    },
  };
};

/** Runs until the result is `pending` for `reasonPrefix`, or `limit` runs. */
export const runUntil = async (
  runner: Runner,
  block: Block,
  reasonPrefix: string,
  limit = 12,
): Promise<{
  readonly result: FraudProofWorkflowRunResult;
  readonly history: readonly string[];
}> => {
  const history: string[] = [];
  for (let attempt = 0; attempt < limit; attempt += 1) {
    const result = await runner.runOnce(block);
    history.push(
      "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
    );
    if (result.kind === "pending" && result.reason.startsWith(reasonPrefix))
      return { result, history };
    if (result.kind === "stalled" && result.resumeOnObservation !== true)
      throw new Error(`the workflow stalled: ${history.join("\n")}`);
  }
  throw new Error(
    `the workflow did not reach ${reasonPrefix}: ${history.join("\n")}`,
  );
};
