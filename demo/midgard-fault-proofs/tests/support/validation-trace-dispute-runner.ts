/** Drives the validation-trace dispute workflow on the Open scene through the
 * shared orchestrator, one emulator block per run; by default one move per
 * run (`one-move-per-run.ts`). */
import { createValidationTraceDisputeWorkflowAdapter } from "../../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceCaseHistory } from "../../src/validation-trace-dispute/case-history.js";
import { withValidationTraceOwnThreadReclaim } from "../../src/validation-trace-dispute/own-thread-reclaim.js";
import type { ValidationTraceRemovalConfig } from "../../src/validation-trace-dispute/removal.js";
import type { ValidationTraceDisputeDeployment } from "../../src/validation-trace-dispute/transactions.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { createFraudProofWorkflowRegistry } from "../../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflow } from "../../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../../src/workflow/raw-l1-publication-observation.js";
import { restrictWorkflowWallet } from "../../src/workflow/workflow-wallet.js";
import { network } from "./emulator/blueprints.js";
import { emulatorPublicationObserver } from "./install-field-carriage.js";
import { oneMovePerRun } from "./one-move-per-run.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import {
  type ConfirmedValidationTraceStage,
  confirmedValidationTraceStages,
} from "./validation-trace-confirmed-stages.js";
import { sceneCaseHistory } from "./validation-trace-dispute-case-history.js";
import {
  type Block,
  BOND_LOVELACE,
  detectOn,
  type Scene,
} from "./validation-trace-dispute-open-scene.js";
import {
  DEPLOYMENT_FINGERPRINT,
  RELEASE_FINALITY_POLICY,
  releaseFinalityAuthority,
  terminalVerifier,
} from "./workflow-authorities.js";
import {
  createTestWorkflowProcess,
  type TestWorkflowProcess,
} from "./workflow-process.js";

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
  /** The process the runner runs in. */
  process: TestWorkflowProcess;
  /** The deployment the runner builds against. */
  deployment: ValidationTraceDisputeDeployment;
  /** One orchestrator run over `block`'s detections, then one block. */
  runOnce: (block: Block) => Promise<FraudProofWorkflowRunResult>;
  /** The stage transactions the process sent that the emulator confirmed. */
  confirmed: () => Promise<readonly ConfirmedValidationTraceStage[]>;
  /** The adapter, as `wrap` rewrites it, that `block`'s runs use. */
  adapterOf: (block: Block) => FraudProofFamilyWorkflowAdapter;
}>;

export const makeRunner = (
  scene: Scene,
  options: {
    /** The process the runner runs in; by default a fresh one, so a new
     * runner over the same scene is a restart. */
    readonly process?: TestWorkflowProcess;
    /** By default the scene's deployment, without the one-step resolution. */
    readonly deployment?: ValidationTraceDisputeDeployment;
    /** How many blocks under the tip a transaction of ours is final; by
     * default the tip. */
    readonly releaseDepth?: number;
    /** By default the scene's recorded case history. */
    readonly caseHistory?: ValidationTraceCaseHistory;
    /** Rewrites where the runner sees a publication of ours stand; by
     * default the emulator's own view. */
    readonly publications?: (
      observer: FraudProofAuthenticatedPublicationObserver,
    ) => FraudProofAuthenticatedPublicationObserver;
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
    /** Whether a run carries on over its own moves in the mempool, as the
     * watcher's does; by default it makes one move, which lands with the
     * run's block. */
    readonly chain?: boolean;
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
  const process = options.process ?? createTestWorkflowProcess();
  const deployment = options.deployment ?? runnerDeployment(scene);
  const { removal } = options;
  const caseHistory = options.caseHistory ?? sceneCaseHistory(scene);
  const now = options.now ?? (() => h.emulator.now());
  const objective = (block: Block) =>
    ({
      category: "validationTraceDispute",
      headerHash: block.evidence.headerHash,
    }) as const;
  // One adapter for each block's objective, over the signer the watcher
  // gives it, with what it submits recorded.
  const registries = new Map<
    string,
    Readonly<{
      adapter: FraudProofFamilyWorkflowAdapter;
      registry: ReturnType<typeof createFraudProofWorkflowRegistry>;
      signer: ReturnType<typeof restrictWorkflowWallet>;
      startRun: () => void;
    }>
  >();
  const registryFor = (block: Block) => {
    const known = registries.get(block.evidence.headerHash);
    if (known !== undefined) return known;
    const signer = restrictWorkflowWallet({
      signer: prover.signer,
      objectiveWallet: process.objectiveWallet(objective(block)),
    });
    const built = createValidationTraceDisputeWorkflowAdapter({
      lucid: prover.lucid,
      signer,
      deployment,
      l1SlotConfig,
      removal: removal ?? unobservedRemoval,
      releaseDepth: (options.releaseDepth ?? 0) + 1,
      caseHistory,
      // Our publications stand as the emulator shows them.
      publications: (options.publications ?? ((observer) => observer))(
        emulatorPublicationObserver(prover.lucid),
      ),
      // With the block's L1 view observed, the adapter is composed as the
      // deployment composes it: the reclaim of our thread in front of it.
      ...(removal === undefined
        ? {}
        : {
            ownThreadReclaim: (base) =>
              withValidationTraceOwnThreadReclaim({
                base,
                lucid: prover.lucid,
                signer,
                deployment,
                l1For: removal.l1For,
                releaseFinality: { policy: RELEASE_FINALITY_POLICY },
              }),
          }),
      now,
    });
    const recorded = process.recording(options.wrap?.(built) ?? built);
    const stepped =
      options.chain === true
        ? { adapter: recorded, startRun: () => undefined }
        : oneMovePerRun(recorded);
    const { adapter, startRun } = stepped;
    const registry = createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: ["validationTraceDispute"],
    });
    const composed = Object.freeze({ adapter, registry, signer, startRun });
    registries.set(block.evidence.headerHash, composed);
    return composed;
  };
  // A block's detections are its complete replay, the same every run once it
  // decides. A replay that does not decide (one held on the evaluator) is not
  // kept, so the next run replays the block again.
  const detections = new WeakMap<Block, ReturnType<typeof detectOn>>();
  const detectionsOf = (block: Block) => {
    let detected = detections.get(block);
    if (detected === undefined) {
      const replay = detectOn(block);
      detections.set(block, replay);
      replay.catch(() => {
        if (detections.get(block) === replay) detections.delete(block);
      });
      detected = replay;
    }
    return detected;
  };
  return {
    process,
    deployment,
    adapterOf: (block) => registryFor(block).adapter,
    confirmed: async () =>
      await confirmedValidationTraceStages({
        emulator: h.emulator,
        deployment,
        sent: process.sent(),
      }),
    runOnce: async (block) => {
      const { registry, signer, startRun } = registryFor(block);
      startRun();
      // A run starts on the wallet the watcher gives the objective.
      signer.selectWallet(prover.lucid);
      const result = await runFraudProofWorkflow({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        evidence: block.evidence,
        detections: await detectionsOf(block),
        ...(block.context === undefined
          ? {}
          : { replayContext: block.context }),
        registry,
        run: process.run(objective(block)),
        terminalVerifier: options.terminalVerifier ?? terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      });
      // The run leaves the prover's Lucid on the wallet the run restricts to
      // the prover's plain funding coins. Between runs the
      // Lucid is the test's: what a test builds or reads there itself pays
      // from, and lists, the whole wallet. Each move of ours selects its
      // restricted wallet again when it builds.
      prover.signer.selectWallet(prover.lucid);
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
