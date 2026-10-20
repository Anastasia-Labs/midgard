/**
 * The validation-trace dispute workflow run as the watcher runs an admitted
 * objective, over the Open scene's emulator deployment: the header is
 * classified through the production classifier, the actuation permit is
 * minted from that decision, and every turn mints the funding permit from
 * the real watcher prover-funding authority over a durable SQLite
 * reservation store, restricts the prover signer to it and runs the shared
 * orchestrator over a durable directory journal bound to both permits.
 *
 * The calling test stands in for the family's install (the category is not
 * installed until its defender exists); the deployment is the fixture
 * manifest's with the scene's dispute contracts, whose dispute bond is the
 * emulator deployment's. The policy's dispute terms are the watcher's own,
 * derived from that signed deployment.
 */
import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Lucid, type Script, type UTxO } from "@lucid-evolution/lucid";
import {
  watcherDeploymentReleaseEconomicsAuthority,
  watcherDeploymentReleaseFinalityAuthority,
  watcherValidationDisputeFundingTerms,
} from "midgard-watcher";
import {
  makeWatcherAuthorityContracts,
  makeWatcherDeploymentAuthorityFixture,
} from "midgard-watcher/tests/support/deployment-authority-fixture";
import { watcherDeploymentAvailabilityChallengeAuthority } from "midgard-watcher/tests/support/prover-funding-hold";

import { createValidationTraceDisputeWorkflowAdapter } from "../../src/validation-trace-dispute/adapter.js";
import { createValidationTraceCaseHistory } from "../../src/validation-trace-dispute/case-history.js";
import type { ValidationTraceRemovalConfig } from "../../src/validation-trace-dispute/removal.js";
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
import { DirectoryFraudProofWorkflowJournalStore } from "../../src/workflow/journal.js";
import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  runFraudProofWorkflow,
} from "../../src/workflow/orchestrator.js";
import { createAdmittedWorkflowRunner } from "../../src/workflow/runner-admission.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import { terminalVerifier } from "../workflow.make-adapter.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import {
  emulatorProtocolParameterAuthority,
  openInstalledProverFunding,
} from "./script-decode-installed-funding.js";
import { FixtureRetainedDaSource } from "./script-decode-installed-host.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
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

/** The published reference scripts a dispute transaction may read. */
const referenceUtxos = (scene: Scene): readonly UTxO[] => {
  const { h } = scene;
  const byOutRef = new Map<string, UTxO>();
  for (const utxo of [
    ...Object.values(h.faultProofReferenceScripts).map(({ utxo }) => utxo),
    ...Object.values(
      h.witnessReferenceScripts as Readonly<Record<string, UTxO | undefined>>,
    ).filter((utxo): utxo is UTxO => utxo !== undefined),
    ...Object.values(scene.references),
  ])
    byOutRef.set(`${utxo.txHash}#${utxo.outputIndex.toString()}`, utxo);
  return [...byOutRef.values()];
};

/** The dispute contracts by the names a manifest gives them. */
const scriptHashes = (scene: Scene): Readonly<Record<string, string>> => {
  const { chain, h } = scene;
  return {
    fraudProofValidationTraceDispute:
      h.contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
    validationTraceDisputeOpener: chain.opener.spendingScriptHash,
    validationTraceDisputeSource: chain.source.spendingScriptHash,
    validationTraceDisputeCase: chain.case.spendingScriptHash,
    validationTraceDisputeBond: chain.disputeBond.spendingScriptHash,
    validationTraceDisputeGame: chain.game.spendingScriptHash,
    validationTraceDisputeTimeout: chain.timeout.spendingScriptHash,
    validationTraceDisputeAward: chain.award.spendingScriptHash,
    // The hub's fraud-proof record, where a case close locks its record.
    fraudProofSpend: h.contracts.fraudProof.spendingScriptHash,
    // A protocol-state contract, as every deployment's roster governs.
    stateQueueSpend: h.contracts.stateQueue.spendingScriptHash,
  };
};

/**
 * The fixture's signed deployment, its dispute bond, case, game, timeout
 * and award contracts (and their reference-script entries) the scene's.
 */
const signedDeployment = (scene: Scene) => {
  const { chain } = scene;
  const contractSet = makeWatcherAuthorityContracts();
  const deployed = (name: string, script: Script, scriptHash: string) => {
    contractSet.contracts[name] = {
      ...contractSet.contracts[name]!,
      contract: { type: script.type, cborHex: script.script },
      scriptHash,
    };
    for (const [role, contract] of Object.entries(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ))
      if (contract === name)
        contractSet.referenceScripts[role] = {
          ...contractSet.referenceScripts[role]!,
          scriptHash,
        };
  };
  deployed(
    "validationTraceDisputeBond",
    chain.disputeBond.spendingScript,
    chain.disputeBond.spendingScriptHash,
  );
  deployed(
    "validationTraceDisputeCase",
    chain.case.spendingScript,
    chain.case.spendingScriptHash,
  );
  deployed(
    "validationTraceDisputeCaseMint",
    chain.case.mintingScript,
    chain.case.policyId,
  );
  for (const [name, script] of [
    ["validationTraceDisputeGame", chain.game],
    ["validationTraceDisputeTimeout", chain.timeout],
    ["validationTraceDisputeAward", chain.award],
  ] as const)
    deployed(name, script.spendingScript, script.spendingScriptHash);
  deployed(
    "validationTraceDisputeAwardMint",
    chain.award.mintingScript,
    chain.award.policyId,
  );
  return makeWatcherDeploymentAuthorityFixture({ contractSet }).result;
};

/**
 * Classifies the failing block's header, then returns one watcher turn for
 * its objective over the durable state in a fresh directory. By default the
 * runner observes no convicted block's removal.
 */
export const openFundedDispute = async (
  scene: Scene,
  options: { readonly removal?: ValidationTraceRemovalConfig } = {},
) => {
  const { h } = scene;
  const block = scene.failing;
  const headerHash = block.evidence.headerHash;
  const identity = signedDeployment(scene);
  const fingerprint = identity.manifestId;
  const bondLovelace = BigInt(
    watcherDeploymentAvailabilityChallengeAuthority(identity).parameters
      .challengerBondLovelace,
  );
  if (bondLovelace !== BOND_LOVELACE)
    throw new Error("the fixture manifest binds another dispute bond");
  const releaseFinalityAuthority =
    watcherDeploymentReleaseFinalityAuthority(identity);
  const releaseFinality = await releaseFinalityAuthority.verifyForWorkflow({
    deploymentFingerprint: fingerprint,
  });
  const caseHistory = createValidationTraceCaseHistory({
    authority: scene.l1.authority,
    releaseFinality,
  });
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
    identity,
  ).verifyForWorkflow({ deploymentFingerprint: fingerprint });
  const protocolParameters = await emulatorProtocolParameterAuthority(
    identity,
    h.proverLucid,
  );

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
  const journalDirectory = join(directory, "journal");
  await mkdir(journalDirectory, { recursive: true, mode: 0o700 });
  const deployment = runnerDeployment(scene, fingerprint);
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
      scriptHashes: scriptHashes(scene),
      referenceUtxos: referenceUtxos(scene),
      directory,
      validationDispute,
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
        journal: new DirectoryFraudProofWorkflowJournalStore(journalDirectory),
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
    /** The case token's authenticated L1 history the watcher reads. */
    caseHistory,
    rebroadcasts,
    turn,
    /** Stops the watcher process and starts another over its durable state. */
    restart: async () => {
      funding.close();
      funding = await boot();
    },
    readReservations: async () => await funding.readReservations(),
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
