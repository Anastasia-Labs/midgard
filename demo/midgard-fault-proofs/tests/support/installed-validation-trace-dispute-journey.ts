import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type MidgardValidationTraceProof,
  selectMidgardValidationDisputeReveal,
} from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FraudProofComputationThreadStepDatum,
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import { submitValidationDisputeReveal } from "../../src/index.js";
import {
  createManifestBoundValidationTraceDisputeWorkflow,
  executeManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflowConfig,
} from "../../src/validation-dispute/workflow-v1.js";
import { admitValidationTraceChallenge } from "../../src/workflow/challenge-authority.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../../src/workflow/journal.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import {
  alwaysSucceedsBlueprintPath,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import { buildMinimalFaultProofContracts } from "./emulator/contracts.js";
import {
  createValidationDisputeParties,
  stageAuthenticatedValidationDisputePublication,
  withRealL1MaxTxSize,
} from "./emulator/dispute-staging.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
} from "./emulator/emulator-context.js";
import { createReferenceScriptPublisher } from "./emulator/reference-script-publisher.js";
import {
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
  publishPlainReferenceScriptUtxo,
} from "./emulator/reference-scripts.js";
import { submitSetupTx } from "./emulator/setup-tx.js";
import { buildAcceptedClaimOverMinAdaRejectingTransactionFixture } from "./emulator/validation-dispute-fixtures.js";
import { buildInstalledSignatureFixture } from "./installed-signature-fixture.js";
import { stageInstalledValidationResolutionReferences } from "./installed-validation-resolution-references.js";
import { installedWorkflowEmulatorClock } from "./installed-workflow.advance-emulator-observation.js";
import { useCanonicalValidationDisputeCursorReads } from "./validation-trace-dispute-canonical-cursor.js";
const DEPLOYMENT = "11".repeat(32);
const PAYLOAD_ENVELOPE_SHA = "ab".repeat(32);
const PAYLOAD_SHA = "cd".repeat(32);
const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;

/**
 * Stages the complete installed-workflow ledger for the sole interactive
 * family and returns the production R6 runner plus the emulator-side operator
 * counterparty. Every publication mirrors the reference dispute journey
 * (`dispute-scenario.ts`), but from staging onwards every watcher move is
 * driven exclusively through the production
 * `createManifestBoundValidationTraceDisputeWorkflow` /
 * `executeManifestBoundValidationTraceDisputeWorkflow` pair — no direct
 * submit-helper call plays the watcher side. The retained-DA currency check
 * the record-derived runner adds in front of execution is out of scope here:
 * the staged challenge carries a fixed payload coordinate.
 */
export const stageInstalledValidationTraceDisputeJourney = async (
  hooks: { binding: unknown; authority: unknown },
  terminalCounterMismatch = false,
  certifiedSignatures = false,
  repeatedRequiredSigners = false,
  fixtureBuilder?: typeof buildInstalledSignatureFixture,
) => {
  const recorder = recordCrossBlockRawEmulator();
  hooks.authority = recorder.authority;
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const {
    emulator,
    operator,
    challenger,
    operatorLucid,
    challengerLucid,
    operatorSigner,
    challengerSigner,
    validityRange,
  } = await createValidationDisputeParties();
  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
    await createReferenceScriptPublisher(operatorLucid, emulator.now());
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
        realValidationTraceDispute: true,
      },
    )),
    referenceScriptAuth,
    referenceScriptPublisher,
  };
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScripts({
        lucid: challengerLucid,
        contracts: baseContracts,
      }),
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: challengerLucid,
      realBlueprint,
      computationThreadMintingScript: contracts.computationThread.mintingScript,
      fraudProofMintingScript: contracts.fraudProof.mintingScript,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(
      operatorLucid,
      emulator.now() + 120_000,
    ) - 1;
  const fixture = await (
    fixtureBuilder ??
    (certifiedSignatures
      ? buildInstalledSignatureFixture
      : buildAcceptedClaimOverMinAdaRejectingTransactionFixture)
  )({
    operatorVkey: operatorSigner.paymentKeyHash,
    now: headerStartTime,
    terminalCounterMismatch,
    repeatedRequiredSigners,
  });
  emulator.awaitSlot(
    Math.max(0, Math.ceil((headerStartTime - 120_000 - emulator.now()) / 1000)),
  );
  const setup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header,
    }),
  );
  const {
    referenceScriptPublisherLucid,
    validationDisputePublication,
    validationDisputeControlPublications,
  } = await stageAuthenticatedValidationDisputePublication({
    emulator,
    operatorLucid,
    operatorSeedPhrase: challenger.seedPhrase,
    contracts,
    authPolicy: referenceScriptAuth,
    publisher: referenceScriptPublisher,
    runStage: runEmulatorLifecycleStage,
  });
  const {
    targetOperatorLucid,
    targetChallengerLucid,
    removal,
    deploymentInfo,
    resolvedContracts,
    semanticPublication,
    prepareResolverPublication,
    canonicalPublications,
  } = await stageInstalledValidationResolutionReferences({
    fixture,
    contracts,
    catalogue,
    realBlueprint,
    emulator,
    challengerLucid,
    operator,
    challenger,
    referenceScriptPublisherLucid,
    validationDisputePublication,
  });
  const restoreCursorReads = useCanonicalValidationDisputeCursorReads(
    targetChallengerLucid,
    emulator,
  );
  const challenge = await admitValidationTraceChallenge({
    coordinate: {
      schemaVersion: "midgard-production-w25-challenge-coordinate-v1",
      deploymentFingerprint: DEPLOYMENT,
      stateQueueObservationDigest: "22".repeat(32),
      headerHash: setup.headerHash,
      payloadEnvelopeSha256: PAYLOAD_ENVELOPE_SHA,
      payloadSha256: PAYLOAD_SHA,
      transcriptDigest: "33".repeat(32),
      blockReplayResultDigest: "44".repeat(32),
      coordinate: { domain: "transaction", index: "0" },
    },
    evidence: {
      headerHash: setup.headerHash,
      payloadEnvelopeSha256: PAYLOAD_ENVELOPE_SHA,
      payloadSha256: PAYLOAD_SHA,
      header: fixture.header,
    } as unknown as CanonicalBlockEvidence,
    claim: fixture.claim,
    challengerReplayInput: fixture.challengerReplayInput,
    exactL1ReferenceOutRefs: [],
  });
  const referenceScripts = {
    control: {
      opener: validationDisputeControlPublications.dispute.utxo,
      source: validationDisputeControlPublications.source.utxo,
      game: validationDisputeControlPublications.game.utxo,
      boundary: validationDisputeControlPublications.boundary.utxo,
      timeout: validationDisputeControlPublications.timeout.utxo,
      award: validationDisputeControlPublications.award.utxo,
    },
    witnesses: {
      computationThreadMint: witnessReferenceScripts.computationThreadMint!,
      fraudProofMint: witnessReferenceScripts.fraudProofMint!,
      phasMembershipWithdraw: witnessReferenceScripts.phasMembershipWithdraw!,
    },
    removal: removal.published,
  };
  const certificatePublication = certifiedSignatures
    ? await withRealL1MaxTxSize(emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: referenceScriptPublisherLucid,
          script: contracts.fieldPreimageCertificate.mintingScript,
          label: "field-preimage certificate policy",
        }),
      )
    : undefined;
  const referenceScriptsByContract = Object.fromEntries(
    Object.entries({
      validationTraceDispute: referenceScripts.control.opener,
      validationTraceDisputeSource: referenceScripts.control.source,
      validationTraceDisputeGame: referenceScripts.control.game,
      validationTraceDisputeBoundary: referenceScripts.control.boundary,
      validationTraceDisputeTimeout: referenceScripts.control.timeout,
      validationTraceDisputeAward: referenceScripts.control.award,
      validationSelectedSemantic: semanticPublication.utxo,
      validationSelectedPrepare: prepareResolverPublication.utxo,
      ...Object.fromEntries(
        Object.entries(canonicalPublications).map(([name, { utxo }]) => [
          name,
          utxo,
        ]),
      ),
      ...(certificatePublication === undefined
        ? {}
        : { fieldPreimageCertificateMint: certificatePublication.utxo }),
      ...referenceScripts.witnesses,
      ...referenceScripts.removal,
    }).map(([name, utxo]) => [
      name,
      {
        outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        scriptHash: validatorToScriptHash((utxo as UTxO).scriptRef!),
      },
    ]),
  );
  const binding = {
    deploymentFingerprint: DEPLOYMENT,
    blueprintHash: "bb".repeat(32),
    network,
    blueprint: realBlueprint,
    deploymentInfo,
    releaseFinality: {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
      policy: finalityPolicy,
    },
    releaseEconomics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
      policy: economicsPolicy,
    },
    referenceScriptsByContract,
    fieldPreimageCertificate: contracts.fieldPreimageCertificate,
    contractEntries: Object.fromEntries(
      Object.entries(referenceScriptsByContract).map(([name, entry]) => [
        name,
        {
          scriptHash: entry.scriptHash,
          refScriptUTxO: {
            txHash: entry.outRef.split("#")[0],
            outputIndex: Number(entry.outRef.split("#")[1]),
          },
        },
      ]),
    ),
    definition: {
      category: "validationTraceDispute" as const,
      categoryId: "00000006",
      headerHash: setup.headerHash,
      proverCredential: challengerSigner.paymentKeyHash,
      stateQueue: {
        policyId: contracts.stateQueue.policyId,
        address: contracts.stateQueue.spendingScriptAddress,
      },
      computationThread: {
        policyId: resolvedContracts.contracts.computationThread.policyId,
        steps: [
          {
            role: "computation_thread_step_01",
            address:
              resolvedContracts.contracts.validationTraceDispute.opener
                .spendingScriptAddress,
            datumSchema: FraudProofComputationThreadStepDatum,
          },
        ],
      },
      proofToken: {
        policyId: resolvedContracts.contracts.fraudProof.policyId,
        address: resolvedContracts.contracts.fraudProof.spendingScriptAddress,
      },
      operatorDirectory: {
        activePolicyId: contracts.activeOperators.policyId,
        activeAddress: contracts.activeOperators.spendingScriptAddress,
        retiredPolicyId: contracts.retiredOperators.policyId,
        retiredAddress: contracts.retiredOperators.spendingScriptAddress,
      },
      schedulerAddress: contracts.scheduler.spendingScriptAddress,
    },
    resolvedContracts,
  };
  hooks.binding = binding;
  const directory = await mkdtemp(
    join(tmpdir(), "validation-trace-installed-"),
  );
  const config: ManifestBoundValidationTraceDisputeWorkflowConfig = {
    manifest: {},
    blueprintJson: JSON.stringify(realBlueprint),
    deploymentInfo,
    headerHash: setup.headerHash,
    lucid: targetChallengerLucid,
    signer: challengerSigner,
    source: {} as never,
    decisionDigest: "dd".repeat(32),
    challenge,
    referenceScripts,
    stateQueueMutationLeaseCoordinator: {
      acquire: async () => ({
        token: "validation-trace-lease",
        source: "emulator",
        renew: async () => {},
        release: async () => {},
        fail: async () => {},
      }),
    },
  };
  const workflowClock = installedWorkflowEmulatorClock(emulator);
  workflowClock.awaitReleaseDepth();
  const clock = vi.spyOn(Date, "now").mockImplementation(() => emulator.now());
  const threadUnit = toUnit(
    resolvedContracts.contracts.computationThread.policyId,
    `00000006${setup.headerHash}`,
  );
  /**
   * One cold runner invocation: a brand-new installed constructor plus a
   * freshly reopened durable journal over the same directory. No in-memory
   * workflow object, plan, or cursor survives between invocations — the
   * dispute position is re-derived from live chain state only (ruling R2).
   */
  const runCold = async () => {
    const workflow =
      await createManifestBoundValidationTraceDisputeWorkflow(config);
    return {
      workflow,
      result: await executeManifestBoundValidationTraceDisputeWorkflow({
        workflow,
        journal: new DirectoryFraudProofWorkflowJournalStore(
          join(directory, "journal"),
        ),
      }),
    };
  };
  const gameDispute = async () => {
    const thread = await targetOperatorLucid.utxoByUnit(threadUnit);
    if (thread.datum == null) {
      throw new Error("operator found the dispute thread without a datum");
    }
    const datum = Data.from(thread.datum, ValidationDisputeDatum);
    if (datum.data === null) {
      throw new Error("operator found a null dispute state");
    }
    return {
      threadOutRef: `${thread.txHash}#${thread.outputIndex.toString()}`,
      dispute: validationDisputeCoreFromData(datum.data.dispute),
    };
  };
  /** The counterparty: the operator answers with its own committed trace. */
  const operatorResponds = async (
    overrideProof?: MidgardValidationTraceProof,
  ) => {
    const { threadOutRef, dispute } = await gameDispute();
    const move = selectMidgardValidationDisputeReveal({
      dispute,
      role: "operator",
      proofs: fixture.operatorTrace.tree.proofs,
    });
    if (move.type !== "revealOperator") {
      throw new Error(
        `operator asked to respond while the dispute is ${move.type}`,
      );
    }
    return await submitValidationDisputeReveal({
      lucid: targetOperatorLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: operatorSigner,
      threadOutRef,
      role: "operator",
      proof: overrideProof ?? move.proof,
      gameReferenceScriptUtxo: referenceScripts.control.game,
      validityRange: validityRange(),
      awaitConfirmation: true,
    });
  };
  const honestOperatorMove = async () => {
    const { dispute } = await gameDispute();
    const move = selectMidgardValidationDisputeReveal({
      dispute,
      role: "operator",
      proofs: fixture.operatorTrace.tree.proofs,
    });
    if (move.type !== "revealOperator") {
      throw new Error(
        `operator asked for a move while the dispute is ${move.type}`,
      );
    }
    return move.proof;
  };
  return {
    emulator,
    /** One block, or to and across the recovery horizon once the terminal is
     * included: the workflow completes only beyond it. */
    advance: workflowClock.advance,
    recorder,
    clock,
    directory,
    fixture,
    setup,
    challenge,
    config,
    binding,
    contracts,
    resolvedContracts,
    threadUnit,
    runCold,
    operatorResponds,
    honestOperatorMove,
    cleanup: async () => {
      clock.mockRestore();
      restoreCursorReads();
      recorder.restore();
      await rm(directory, { recursive: true, force: true });
    },
  };
};
