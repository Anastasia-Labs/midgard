/**
 * The validation-trace dispute workflow run as the watcher runs an admitted
 * objective, over the Open scene's emulator deployment: the header is
 * classified through the production classifier, the actuation permit is
 * minted from that decision, and every turn mints the funding permit from
 * the real watcher prover-funding authority over a durable SQLite
 * reservation store, restricts the prover signer to it and runs the shared
 * orchestrator over the store's own workflow journal bound to both permits.
 *
 * The calling test stands in for the family's install (the category is not
 * installed until its defender exists); the deployment is the fixture
 * manifest's with the scene's dispute contracts and one-step holders, whose
 * dispute bond is the emulator deployment's. The policy's dispute terms are
 * the watcher's own, derived from that signed deployment. Given the one-step
 * holders' published references, the runner drives the resolution; when
 * those references include a script selection, the deployment names the
 * emulator's script-decode deployment, whose records the watcher writes
 * through the shared script-decode prerequisite under the same permits.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

import { Lucid } from "@lucid-evolution/lucid";
import {
  watcherDeploymentReleaseEconomicsAuthority,
  watcherDeploymentReleaseFinalityAuthority,
  watcherValidationDisputeFundingTerms,
} from "midgard-watcher";
import { watcherDeploymentAvailabilityChallengeAuthority } from "midgard-watcher/tests/support/prover-funding-hold";

import { createValidationTraceDisputeWorkflowAdapter } from "../../src/validation-trace-dispute/adapter.js";
import {
  createValidationTraceCaseHistory,
  type ValidationTraceCaseHistory,
} from "../../src/validation-trace-dispute/case-history.js";
import type { ValidationTraceRemovalConfig } from "../../src/validation-trace-dispute/removal.js";
import type { ValidationTraceOneStepReferences } from "../../src/validation-trace-dispute/transactions.js";
import {
  bindWorkflowActuationJournal,
  createWorkflowActuationPermitController,
} from "../../src/workflow/actuation-permit.js";
import { createValidationTraceDisputeCompleteCanonicalReplay } from "../../src/workflow/complete-replay.js";
import {
  bindWorkflowFundingReservationJournal,
  restrictWorkflowFundingSigner,
} from "../../src/workflow/funding-reservation-permit.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../../src/workflow/header-classifier.js";
import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  runFraudProofWorkflow,
} from "../../src/workflow/orchestrator.js";
import { createAdmittedWorkflowRunner } from "../../src/workflow/runner-admission.js";
import { validationDisputeWalletMoveReserve } from "../../src/workflow/runtime-funding-fee.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import { terminalVerifier } from "../workflow.make-adapter.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { emulatorPublicationObserver } from "./install-field-carriage.js";
import {
  emulatorProtocolParameterAuthority,
  openInstalledProverFunding,
} from "./script-decode-installed-funding.js";
import { FixtureRetainedDaSource } from "./script-decode-installed-host.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import {
  referenceUtxos,
  scriptHashes,
  signedDeployment,
} from "./validation-trace-dispute-funded-roster.js";
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
 * its objective over the durable state in a fresh directory. By default the
 * runner observes no convicted block's removal, and does not drive the
 * one-step resolution; given the one-step holders' references, it does.
 */
export const openFundedDispute = async (
  scene: Scene,
  options: {
    readonly removal?: ValidationTraceRemovalConfig;
    readonly oneStep?: ValidationTraceOneStepReferences;
    /** Whether the watcher's funding follows the emulator's live fees. */
    readonly liveParameters?: boolean;
    /** Rewrites the case history the watcher reads; by default it reads the
     * authenticated one. */
    readonly caseHistory?: (
      authentic: ValidationTraceCaseHistory,
    ) => ValidationTraceCaseHistory;
    /** Rewrites the contracts the watcher's roster governs, by their
     * manifest names; by default it governs the scene's. */
    readonly roster?: (
      hashes: Readonly<Record<string, string>>,
    ) => Readonly<Record<string, string>>;
    /** Whether the watcher observes publications; by default it does. */
    readonly publications?: boolean;
  } = {},
) => {
  const { h } = scene;
  const block = scene.failing;
  const headerHash = block.evidence.headerHash;
  const {
    identity,
    fingerprint,
    releaseFinalityAuthority,
    releaseFinality,
    decision,
  } = await classifyFailingHeader(scene);
  const bondLovelace = BigInt(
    watcherDeploymentAvailabilityChallengeAuthority(identity).parameters
      .challengerBondLovelace,
  );
  if (bondLovelace !== BOND_LOVELACE)
    throw new Error("the fixture manifest binds another dispute bond");
  const authentic = createValidationTraceCaseHistory({
    authority: scene.l1.authority,
    releaseFinality,
  });
  const caseHistory = options.caseHistory?.(authentic) ?? authentic;
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
    identity,
  ).verifyForWorkflow({ deploymentFingerprint: fingerprint });
  const protocolParameters = await emulatorProtocolParameterAuthority(
    identity,
    h.proverLucid,
    options.liveParameters === true ? h.emulator : undefined,
  );
  const actuationPermit = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "0",
  }).permit;
  const runner = createAdmittedWorkflowRunner({
    category: CATEGORY,
    runOrResume: async () => {
      throw new Error("the funded dispute runs the orchestrator itself");
    },
  });

  const directory = await mkdtemp(join(homedir(), "validation-dispute-"));
  const deployment =
    options.oneStep === undefined
      ? runnerDeployment(scene, fingerprint)
      : oneStepDeployment(scene, options.oneStep, fingerprint);
  const { validationDispute } = watcherValidationDisputeFundingTerms(
    identity,
    CATEGORY,
  );
  /** The watcher process's in-memory part; booting again is a restart. */
  const boot = async () =>
    await openInstalledProverFunding({
      emulator: h.emulator,
      identity,
      category: CATEGORY,
      runner,
      economics,
      protocolParameters,
      fundingPaymentKeyHash: h.proverSigner.paymentKeyHash,
      walletAddress: h.proverSigner.address,
      scriptHashes: (options.roster ?? ((hashes) => hashes))(
        scriptHashes(scene, deployment),
      ),
      referenceUtxos: referenceUtxos(scene, deployment),
      directory,
      // The watcher does not install the dispute until its defender exists.
      journalRoot: null,
      validationDispute,
      // The supervisor's factory queries its node's parameters afresh at
      // each turn.
      ...(options.liveParameters === true
        ? {
            currentParameters: async () =>
              await emulatorProtocolParameterAuthority(
                identity,
                h.proverLucid,
                h.emulator,
              ),
          }
        : {}),
    });
  let funding = await boot();
  /** Every signed transaction the emulator recovery rebroadcast. */
  const rebroadcasts: string[] = [];
  const emulatorRecovery = emulatorSignedTransactionRecovery(h.emulator);
  const signedRecovery: FraudProofSignedTransactionRecovery = {
    ...emulatorRecovery,
    rebroadcastSignedTransaction: async (input) => {
      rebroadcasts.push(input.signedTransactionCborHex);
      return await emulatorRecovery.rebroadcastSignedTransaction(input);
    },
  };

  /** One watcher turn: mint the funding permit, then run the workflow. */
  const turn = async (
    wrap?: AdapterWrap,
  ): Promise<FraudProofWorkflowRunResult> => {
    const fundingReservationPermit = await funding.mintPermit({
      actuationPermit,
      decisionDigest: decision.decisionDigest,
    });
    const journal = bindWorkflowFundingReservationJournal({
      permit: fundingReservationPermit,
      journal: bindWorkflowActuationJournal({
        journal: funding.workflowJournal.forObjective({
          category: CATEGORY,
          headerHash,
        }),
        permit: actuationPermit,
        decisionDigest: decision.decisionDigest,
        deploymentFingerprint: fingerprint,
        category: CATEGORY,
        headerHash,
      }),
    });
    const lucid = await Lucid(h.emulator, "Custom", {
      slotConfig: h.proverLucid.config().slotConfig,
    });
    const signer = restrictWorkflowFundingSigner({
      signer: h.proverSigner,
      permit: fundingReservationPermit,
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
      // A script-decode record transaction stands as the emulator shows it.
      ...(options.publications === false
        ? {}
        : {
            publications: {
              ...emulatorPublicationObserver(h.proverLucid),
              observeSignedTransaction: signedRecovery.observeSignedTransaction,
              rebroadcastSignedTransaction:
                emulatorRecovery.rebroadcastSignedTransaction,
            },
          }),
      now: () => h.emulator.now(),
    });
    const result = await runFraudProofWorkflow({
      deploymentFingerprint: fingerprint,
      evidence: block.evidence,
      detections: await detectOn(block),
      ...(block.context === undefined ? {} : { replayContext: block.context }),
      registry: createFraudProofWorkflowRegistry({
        adapters: [wrap === undefined ? adapter : wrap(adapter)],
        launchScope: [CATEGORY],
      }),
      journal,
      // Superseded attempts are read where the emulator holds them.
      terminalVerifier: {
        ...terminalVerifier,
        observeSignedTransaction: emulatorRecovery.observeSignedTransaction,
      },
      releaseFinalityAuthority,
      now: () => new Date(h.emulator.now()),
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
    /** Stops the watcher process and starts another over its durable state. */
    restart: async () => {
      funding.close();
      funding = await boot();
    },
    readReservations: async () => await funding.readReservations(),
    /** What the watcher's terms keep for a move our wallet pays, at the
     * emulator's fees now. */
    walletMoveReserve: async () =>
      validationDisputeWalletMoveReserve({
        parameters: (
          await emulatorProtocolParameterAuthority(
            identity,
            h.proverLucid,
            h.emulator,
          )
        ).snapshot,
        fundingPaymentKeyHash: h.proverSigner.paymentKeyHash,
      }),
    close: async () => {
      funding.close();
      await rm(directory, { recursive: true, force: true });
    },
  } as const;
};

export type FundedDispute = Awaited<ReturnType<typeof openFundedDispute>>;

/**
 * Turns until the run is pending for `reasonPrefix`; a turn whose funding
 * permit cannot be minted, or a stall, ends it with the history.
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
