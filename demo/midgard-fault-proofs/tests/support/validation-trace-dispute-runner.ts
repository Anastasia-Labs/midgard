/** Drives the validation-trace dispute workflow on the Open scene through the
 * shared orchestrator, one emulator block per run. */
import { createValidationTraceDisputeWorkflowAdapter } from "../../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceCaseHistory } from "../../src/validation-trace-dispute/case-history.js";
import { withValidationTraceOwnThreadReclaim } from "../../src/validation-trace-dispute/own-thread-reclaim.js";
import type { ValidationTraceRemovalConfig } from "../../src/validation-trace-dispute/removal.js";
import type { ValidationTraceDisputeDeployment } from "../../src/validation-trace-dispute/transactions.js";
import {
  type FraudProofWorkflowJournalStore,
  MemoryFraudProofWorkflowJournalStore,
} from "../../src/workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
} from "../../src/workflow/orchestrator.js";
import {
  DEPLOYMENT_FINGERPRINT,
  RELEASE_FINALITY_POLICY,
  releaseFinalityAuthority,
  terminalVerifier,
} from "../workflow.make-adapter.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { emulatorPublicationObserver } from "./install-field-carriage.js";
import { network } from "./submit-init-emulator-shared.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import { sceneCaseHistory } from "./validation-trace-dispute-case-history.js";
import {
  type Block,
  BOND_LOVELACE,
  detectOn,
  type Scene,
} from "./validation-trace-dispute-open-scene.js";

/** The deployment the runner builds against, from the scene's contracts. */
export const runnerDeployment = (
  scene: Scene,
  deploymentFingerprint: string = DEPLOYMENT_FINGERPRINT,
): ValidationTraceDisputeDeployment => {
  const { h, chain } = scene;
  const category = h.catalogue.categories.validationTraceDispute;
  if (category === undefined)
    throw new Error("catalogue has no validation-trace category");
  return {
    deploymentFingerprint,
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
    fraudProof: {
      policyId: h.contracts.fraudProof.policyId,
      mintingScript: h.contracts.fraudProof.mintingScript,
      spendingScriptAddress: h.contracts.fraudProof.spendingScriptAddress,
    },
    references: scene.references,
    bondLovelace: BOND_LOVELACE,
  };
};

/**
 * The removal a runner that never reaches a conviction is given: asking it
 * for the convicted block's L1 view fails.
 */
export const unobservedRemoval: ValidationTraceRemovalConfig = {
  l1For: () => {
    throw new Error("this runner observes no convicted block's removal");
  },
  stateQueueMutationLeaseCoordinator: {
    acquire: async () => {
      throw new Error("this runner removes no convicted block");
    },
  },
  deploymentInfo: {},
  fraudProverRewardLovelace: 0n,
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
    /** By default the scene's deployment, without the one-step resolution. */
    readonly deployment?: ValidationTraceDisputeDeployment;
    /** By default the emulator's own view of each signed transaction. */
    readonly signedRecovery?: FraudProofSignedTransactionRecovery;
    /** By default the scene's recorded case history. */
    readonly caseHistory?: ValidationTraceCaseHistory;
    /** The convicted block's removal; by default none is observed. */
    readonly removal?: ValidationTraceRemovalConfig;
    /** Verifies the completed removal; by default the candidate is kept. */
    readonly terminalVerifier?: FraudProofWorkflowTerminalVerifier;
    /** Rewrites the adapter every run uses. */
    readonly wrap?: (
      adapter: FraudProofFamilyWorkflowAdapter,
    ) => FraudProofFamilyWorkflowAdapter;
    /** The adapter's clock; by default the emulator's. */
    readonly now?: () => number;
    /** The prover's wallet and key; by default the scene's prover. */
    readonly prover?: Readonly<{
      lucid: Scene["h"]["proverLucid"];
      signer: Scene["h"]["proverSigner"];
    }>;
  } = {},
): Runner => {
  const { h } = scene;
  const prover = options.prover ?? {
    lucid: h.proverLucid,
    signer: h.proverSigner,
  };
  const journal = options.journal ?? new MemoryFraudProofWorkflowJournalStore();
  const signedRecovery =
    options.signedRecovery ?? emulatorSignedTransactionRecovery(h.emulator);
  const deployment = options.deployment ?? runnerDeployment(scene);
  const { removal } = options;
  const adapter = createValidationTraceDisputeWorkflowAdapter({
    lucid: prover.lucid,
    signer: prover.signer,
    deployment,
    l1SlotConfig,
    removal: removal ?? unobservedRemoval,
    signedRecovery,
    caseHistory: options.caseHistory ?? sceneCaseHistory(scene),
    // A script-decode record transaction stands as the emulator shows it.
    publications: {
      ...emulatorPublicationObserver(prover.lucid),
      observeSignedTransaction: signedRecovery.observeSignedTransaction,
      rebroadcastSignedTransaction: signedRecovery.rebroadcastSignedTransaction,
    },
    // With the block's L1 view observed, the adapter is composed as the
    // deployment composes it: the reclaim of our thread in front of it.
    ...(removal === undefined
      ? {}
      : {
          ownThreadReclaim: (base) =>
            withValidationTraceOwnThreadReclaim({
              base,
              lucid: prover.lucid,
              signer: prover.signer,
              deployment,
              l1For: removal.l1For,
              signedRecovery,
              releaseFinality: { policy: RELEASE_FINALITY_POLICY },
            }),
        }),
    now: options.now ?? (() => h.emulator.now()),
  });
  const registry = createFraudProofWorkflowRegistry({
    adapters: [options.wrap?.(adapter) ?? adapter],
    launchScope: ["validationTraceDispute"],
  });
  // A block's detections are its complete replay, the same every run.
  const detections = new WeakMap<Block, ReturnType<typeof detectOn>>();
  const detectionsOf = (block: Block) => {
    let detected = detections.get(block);
    if (detected === undefined) {
      detected = detectOn(block);
      detections.set(block, detected);
    }
    return detected;
  };
  return {
    journal,
    runOnce: async (block) => {
      const result = await runFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        evidence: block.evidence,
        detections: await detectionsOf(block),
        ...(block.context === undefined
          ? {}
          : { replayContext: block.context }),
        registry,
        journal,
        terminalVerifier: options.terminalVerifier ?? terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
        now: () => new Date(h.emulator.now()),
      });
      h.emulator.awaitBlock(1);
      // A run on the emulator is microtasks only. Lucid's wasm objects are
      // freed by their finalizers, and the memory reserve's collector runs on
      // a timer; both run only between tasks, so each run ends on one.
      await new Promise((resolve) => setImmediate(resolve));
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
