/**
 * The validation-trace dispute workflow run as the watcher runs an admitted
 * objective, over the Open scene's emulator deployment: the header is
 * classified through the production classifier, the actuation permit is
 * minted from that decision, and every turn runs the shared orchestrator in
 * the watcher process's in-flight set, bound to that permit, with the prover
 * signer restricted to the coins no transaction of the process holds.
 *
 * The calling test stands in for the family's install (the category is not
 * installed until its defender exists); the deployment is the fixture
 * manifest's with the scene's dispute contracts and one-step holders, whose
 * dispute bond is the emulator deployment's. Given the one-step holders'
 * published references, the runner drives the resolution; when those
 * references include a script selection, the deployment names the
 * emulator's script-decode deployment, whose records the watcher writes
 * through the shared script-decode prerequisite under the same permit.
 */
import { CML, Lucid } from "@lucid-evolution/lucid";
import {
  watcherDeploymentAvailabilityChallengeAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";

import { createValidationTraceDisputeWorkflowAdapter } from "../../src/validation-trace-dispute/adapter.js";
import {
  createValidationTraceCaseHistory,
  type ValidationTraceCaseHistory,
} from "../../src/validation-trace-dispute/case-history.js";
import type { ValidationTraceRemovalConfig } from "../../src/validation-trace-dispute/removal.js";
import type { ValidationTraceOneStepReferences } from "../../src/validation-trace-dispute/transactions.js";
import {
  createWorkflowActuationPermitController,
  createWorkflowReconciliationPermitController,
} from "../../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { bindWorkflowRunActuation } from "../../src/workflow/actuation-permit.js";
import { createValidationTraceDisputeCompleteCanonicalReplay } from "../../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { authenticatedStateQueueObservationDigest } from "../../src/workflow/header-classifier.authenticated-state-queue-observation-digest.js";
import { classifyHeader } from "../../src/workflow/header-classifier.classify-header.js";
import { createHeaderClassifier } from "../../src/workflow/header-classifier.create-header-classifier.js";
import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { createFraudProofWorkflowRegistry } from "../../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflow } from "../../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import { restrictWorkflowWallet } from "../../src/workflow/workflow-wallet.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import { terminalVerifier } from "../workflow.make-adapter.js";
import { emulatorManifestProtocolParameters } from "./emulator-protocol-parameters.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { emulatorPublicationObserver } from "./install-field-carriage.js";
import { FixtureRetainedDaSource } from "./script-decode-installed-host.js";
import { validationDisputeWalletMoveReserve } from "./validation-dispute-wallet-move-reserve.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import { confirmedValidationTraceStages } from "./validation-trace-confirmed-stages.js";
import { signedDeployment } from "./validation-trace-dispute-funded-roster.js";
import { oneStepDeployment } from "./validation-trace-dispute-one-step.js";
import {
  BOND_LOVELACE,
  detectOn,
  type Scene,
} from "./validation-trace-dispute-open-scene.js";
import {
  runnerDeployment,
  unobservedRemoval,
} from "./validation-trace-dispute-runner.js";
import { walletOnlySendAlwaysOpenOn } from "./wallet-only-send-always-open.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

const CATEGORY = "validationTraceDispute" as const;

/** Rewrites the adapter one process turn runs with. */
export type AdapterWrap = (
  adapter: FraudProofFamilyWorkflowAdapter,
) => FraudProofFamilyWorkflowAdapter;

/**
 * The fixture's signed deployment and its release finality, and the failing
 * block's header classified through the production classifier under them.
 */
export const classifyFailingHeader = async (scene: Scene) => {
  const block = scene.failing;
  const headerHash = block.evidence.headerHash;
  const identity = signedDeployment(scene);
  const fingerprint = identity.manifestId;
  const releaseFinalityAuthority =
    watcherDeploymentReleaseFinalityAuthority(identity);
  const releaseFinality = await releaseFinalityAuthority.verifyForWorkflow({
    deploymentFingerprint: fingerprint,
  });
  const sources = [
    new FixtureRetainedDaSource(
      fingerprint,
      new Map([[headerHash, block.payloadEnvelopeCbor]]),
    ),
  ];
  const observation = authenticatedHeaderObservation({
    header: block.evidence.header,
    headerHash,
  });
  const decision = await classifyHeader({
    classifier: await createHeaderClassifier({
      deploymentFingerprint: fingerprint,
      replayer:
        createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig),
      releaseFinalityAuthority,
    }),
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
      }),
    sources,
    predecessorLedgerAt: async (hash) =>
      hash === block.evidence.header.prevHeaderHash
        ? block.predecessorEntries
        : undefined,
  });
  // The install stand-in admits this category, which the type excludes.
  if (
    decision.decision !== "fault_detected" ||
    (decision.category as string) !== CATEGORY
  )
    throw new Error(`header classified ${decision.decision}`);
  return {
    identity,
    fingerprint,
    releaseFinalityAuthority,
    releaseFinality,
    decision,
  } as const;
};

/**
 * Classifies the failing block's header, then returns one watcher turn for
 * its objective in a fresh watcher process. By default the runner observes
 * no convicted block's removal, and does not drive the one-step resolution;
 * given the one-step holders' references, it does.
 */
export const openFundedDispute = async (
  scene: Scene,
  options: {
    readonly removal?: ValidationTraceRemovalConfig;
    readonly oneStep?: ValidationTraceOneStepReferences;
    /** Rewrites the case history the watcher reads; by default it reads the
     * authenticated one. */
    readonly caseHistory?: (
      authentic: ValidationTraceCaseHistory,
    ) => ValidationTraceCaseHistory;
    /** How many blocks under the tip a signed transaction of the watcher's
     * is final, read at each observation; by default the tip. */
    readonly releaseDepth?: () => number;
  } = {},
) => {
  const { h } = scene;
  const block = scene.failing;
  const headerHash = block.evidence.headerHash;
  const objective = { category: CATEGORY, headerHash } as const;
  const {
    identity,
    fingerprint,
    releaseFinalityAuthority,
    releaseFinality,
    decision,
  } = await classifyFailingHeader(scene);
  if (
    BigInt(
      watcherDeploymentAvailabilityChallengeAuthority(identity).parameters
        .challengerBondLovelace,
    ) !== BOND_LOVELACE
  )
    throw new Error("the fixture manifest binds another dispute bond");
  const authentic = createValidationTraceCaseHistory({
    authority: scene.l1.authority,
    releaseFinality,
  });
  const caseHistory = options.caseHistory?.(authentic) ?? authentic;
  let actuationPermit = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "0",
  }).permit;
  const deployment =
    options.oneStep === undefined
      ? runnerDeployment(scene)
      : oneStepDeployment(scene, options.oneStep);
  /** The watcher process; a restart is a new one. */
  let process = createTestWorkflowProcess({ nowMs: () => h.emulator.now() });
  /** Every process the dispute ran in, oldest first. */
  const processes = [process];
  /** Every signed transaction the emulator took again, by any path: the
   * same bytes as an earlier send of it. */
  const rebroadcasts: string[] = [];
  const submit = h.emulator.submitTx.bind(h.emulator);
  h.emulator.submitTx = async (cbor: string) => {
    const hash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(cbor).body(),
    ).to_hex();
    if (scene.l1.signedCbors.has(hash)) rebroadcasts.push(cbor);
    return await submit(cbor);
  };
  const { releaseDepth } = options;
  const signedRecovery: FraudProofSignedTransactionRecovery =
    releaseDepth === undefined
      ? emulatorSignedTransactionRecovery(h.emulator)
      : {
          ...emulatorSignedTransactionRecovery(h.emulator),
          observeSignedTransaction: async (input) =>
            await emulatorSignedTransactionRecovery(
              h.emulator,
              releaseDepth(),
            ).observeSignedTransaction(input),
        };

  /** One watcher turn: bind the permit to a fresh run, then run it. */
  const turn = async (
    wrap?: AdapterWrap,
  ): Promise<FraudProofWorkflowRunResult> => {
    const run = bindWorkflowRunActuation({
      run: process.run(objective),
      permit: actuationPermit,
      decisionDigest: decision.decisionDigest,
      deploymentFingerprint: fingerprint,
      category: CATEGORY,
      headerHash,
    });
    const lucid = await Lucid(h.emulator, "Custom", {
      slotConfig: h.proverLucid.config().slotConfig,
    });
    const signer = restrictWorkflowWallet({
      signer: h.proverSigner,
      sends: process.sends(objective),
    });
    signer.selectWallet(lucid);
    const adapter = createValidationTraceDisputeWorkflowAdapter({
      lucid,
      signer,
      deployment,
      l1SlotConfig,
      removal: options.removal ?? unobservedRemoval,
      signedRecovery,
      caseHistory,
      // Our publications stand as the emulator shows them.
      publications: {
        ...emulatorPublicationObserver(h.proverLucid),
        observeSignedTransaction: signedRecovery.observeSignedTransaction,
        rebroadcastSignedTransaction:
          signedRecovery.rebroadcastSignedTransaction,
      },
      now: () => h.emulator.now(),
    });
    const result = await runFraudProofWorkflow({
      walletOnlySend: walletOnlySendAlwaysOpenOn(lucid),
      deploymentFingerprint: fingerprint,
      evidence: block.evidence,
      detections: await detectOn(block),
      ...(block.context === undefined ? {} : { replayContext: block.context }),
      registry: createFraudProofWorkflowRegistry({
        adapters: [wrap === undefined ? adapter : wrap(adapter)],
        launchScope: [CATEGORY],
      }),
      run,
      // The runner follows what it sent through the emulator's own view.
      terminalVerifier: {
        ...terminalVerifier,
        observeSignedTransaction: signedRecovery.observeSignedTransaction,
      },
      releaseFinalityAuthority,
      nowMs: () => h.emulator.now(),
    });
    h.emulator.awaitBlock(1);
    return result;
  };

  return {
    decision,
    deployment,
    /** The case token's authenticated L1 history. */
    caseHistory: authentic,
    rebroadcasts,
    turn,
    /** The process the dispute runs in now. */
    process: () => process,
    /** Every transaction the dispute's processes recorded as sent, oldest
     * process first. */
    sent: () => processes.flatMap((each) => each.sent()),
    /** The stage transactions the dispute's processes sent that the
     * emulator confirmed. */
    confirmed: async () =>
      await confirmedValidationTraceStages({
        emulator: h.emulator,
        deployment,
        sent: processes.flatMap((each) => each.sent()),
      }),
    /** Stops the watcher process and starts another, which holds nothing
     * in flight. */
    restart: () => {
      process = createTestWorkflowProcess({ nowMs: () => h.emulator.now() });
      processes.push(process);
    },
    /** The header rolls back: every later turn runs under the
     * reconciliation-only permit the watcher derives from the decision. */
    rollBackHeader: () => {
      actuationPermit = createWorkflowReconciliationPermitController({
        decision,
        deploymentFingerprint: fingerprint,
        rollbackGeneration: "0",
      }).permit;
    },
    /** What a funding-adequacy check keeps for a move our wallet pays, at
     * the emulator's fees now. */
    walletMoveReserve: () =>
      validationDisputeWalletMoveReserve({
        parameters: emulatorManifestProtocolParameters(h.emulator),
        fundingPaymentKeyHash: h.proverSigner.paymentKeyHash,
      }),
  } as const;
};

export type FundedDispute = Awaited<ReturnType<typeof openFundedDispute>>;

/**
 * Turns until the run is pending for `reasonPrefix`; a stall ends it with
 * the history.
 */
export const turnUntil = async (
  dispute: FundedDispute,
  reasonPrefix: string,
  options: { readonly limit?: number; readonly wrap?: AdapterWrap } = {},
): Promise<{
  readonly result: FraudProofWorkflowRunResult;
  readonly history: readonly string[];
}> => {
  const history: string[] = [];
  for (let attempt = 0; attempt < (options.limit ?? 12); attempt += 1) {
    const result = await dispute.turn(options.wrap);
    history.push(
      "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
    );
    if (result.kind === "pending" && result.reason.startsWith(reasonPrefix))
      return { result, history };
    if (result.kind === "stalled" && result.resumeOnObservation !== true)
      return { result, history };
  }
  throw new Error(
    `the workflow did not reach ${reasonPrefix}: ${history.join("\n")}`,
  );
};
