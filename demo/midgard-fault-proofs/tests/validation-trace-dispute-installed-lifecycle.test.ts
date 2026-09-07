import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type MidgardValidationTraceProof,
  selectMidgardValidationDisputeReveal,
} from "@al-ft/midgard-core";
import {
  createReferenceScriptAuthPolicy,
  FraudProofComputationThreadStepDatum,
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  Lucid,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/validation-dispute/workflow-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/validation-dispute/workflow-binding.js")
  >()),
  bindValidationTraceDisputeWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyLocalKupmiosL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyLocalKupmiosL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  resolveValidationTraceDisputeDeploymentContracts,
  submitValidationDisputeReveal,
  validationSemanticResolverGlobalIndex,
} from "../src/index.js";
import {
  createManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflowConfig,
  runOrResumeManifestBoundValidationTraceDisputeWorkflow,
} from "../src/validation-dispute/workflow-v1.js";
import {
  admitValidationTraceChallenge,
  type ValidationTraceChallenge,
} from "../src/workflow/challenge-authority.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import {
  alwaysSucceedsBlueprintPath,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { buildMinimalFaultProofContracts } from "./support/emulator/contracts.js";
import {
  createRealL1TargetLucids,
  createValidationDisputeParties,
  stageAuthenticatedValidationDisputePublication,
  withRealL1MaxTxSize,
} from "./support/emulator/dispute-staging.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
} from "./support/emulator/emulator-context.js";
import {
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildAcceptedClaimOverMinAdaRejectingTransactionFixture } from "./support/emulator/validation-dispute-fixtures.js";

const DEPLOYMENT = "11".repeat(32);
const PAYLOAD_ENVELOPE_SHA = "ab".repeat(32);
const PAYLOAD_SHA = "cd".repeat(32);
const finalityPolicy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
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
 * `runOrResumeManifestBoundValidationTraceDisputeWorkflow` pair — no direct
 * submit-helper call plays the watcher side.
 */
const stageInstalledValidationTraceDisputeJourney = async () => {
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
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const referenceScriptAuth = createReferenceScriptAuthPolicy(
    challengerLucid,
    emulator.now(),
  );
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
        realValidationTraceDispute: true,
        alwaysFraudProofCatalogue: true,
      },
    )),
    referenceScriptAuth,
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
  const fixture = await buildAcceptedClaimOverMinAdaRejectingTransactionFixture(
    {
      operatorVkey: operatorSigner.paymentKeyHash,
      now: headerStartTime,
    },
  );
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
    runStage: runEmulatorLifecycleStage,
  });
  const resolverIndex = fixture.evidence.oneStepArgument.resolverIndex;
  const semanticResolverIndex =
    fixture.evidence.oneStepArgument.semanticResolverIndex;
  const prepareResolverContract =
    contracts.fraudProofContracts.validationTraceDispute.prepareResolvers[
      resolverIndex
    ];
  if (prepareResolverContract === undefined) {
    throw new Error("Selected validation prepare resolver is not deployed");
  }
  const prepareResolverPublication = await withRealL1MaxTxSize(emulator, () =>
    publishPlainReferenceScriptUtxo({
      lucid: referenceScriptPublisherLucid,
      script: prepareResolverContract.spendingScript,
      label: "validation prepare resolver",
    }),
  );
  // Resolve the selected semantic resolver through the very helper the submit
  // path uses so the published body is byte-identical to the hash-checked one.
  const interimDeploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    { validationDisputePublication },
  );
  const semanticContract = (
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint: realBlueprint,
      deploymentInfo: interimDeploymentInfo,
      network,
    })
  ).contracts.validationTraceDispute.semanticResolvers[
    validationSemanticResolverGlobalIndex(resolverIndex, semanticResolverIndex)
  ];
  if (semanticContract === undefined) {
    throw new Error("Selected validation semantic resolver is not deployed");
  }
  const semanticPublication = await withRealL1MaxTxSize(emulator, () =>
    publishPlainReferenceScriptUtxo({
      lucid: referenceScriptPublisherLucid,
      script: semanticContract.spendingScript,
      label: "validation semantic resolver",
    }),
  );
  const {
    functionalProtocolParameters,
    functionalSlotConfig,
    targetOperatorLucid,
    targetChallengerLucid,
  } = await createRealL1TargetLucids({
    emulator,
    sourceLucid: challengerLucid,
    operatorSeedPhrase: operator.seedPhrase,
    challengerSeedPhrase: challenger.seedPhrase,
  });
  // Removal-validator publications run under the raised deployment-time
  // parameters, not the real 16,384-byte envelope: the ten-parameter
  // `state_queue.mint` applied to this deployment is 16,498 bytes, so its
  // publication cannot be built under the real L1 limit at all.
  // `publishRemovalReferenceScripts` marks that one entry `oversized`; its
  // deployability on real L1 parameters is tracked in
  // Anastasia-Labs/midgard#649 (ruling R7: pre-existing, tests may use the
  // established test-driver deployment mechanism; production never raises
  // limits).
  const removal = await (async () => {
    const prePublicationProtocolParameters = emulator.protocolParameters;
    emulator.protocolParameters = functionalProtocolParameters;
    try {
      const oversizedPublisherLucid = await Lucid(emulator, "Custom", {
        slotConfig: functionalSlotConfig,
      });
      oversizedPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
      return await publishRemovalReferenceScripts({
        lucid: oversizedPublisherLucid,
        contracts,
      });
    } finally {
      emulator.protocolParameters = prePublicationProtocolParameters;
    }
  })();
  const builtDeploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    validationDisputePublication,
    removalReferenceScripts: removal.published,
    validationValueAndMintSemanticReferences: [
      {
        semanticResolverIndex,
        scriptHash: semanticContract.spendingScriptHash,
        utxo: semanticPublication.utxo,
      },
    ],
  });
  // The boundary-selected prepare resolver has no canonical deployment-entry
  // name; the workflow engine resolves its publication by immutable script
  // hash across the manifest entries (ruling R4), so the entry name below is
  // only a label.
  const deploymentInfo = {
    ...builtDeploymentInfo,
    contracts: {
      ...builtDeploymentInfo.contracts,
      validationTraceDisputeValueAndMintPrepare: {
        scriptHash: prepareResolverContract.spendingScriptHash,
        refScriptUTxO: {
          txHash: prepareResolverPublication.utxo.txHash,
          outputIndex: prepareResolverPublication.utxo.outputIndex,
        },
      },
    },
  };
  const resolvedContracts =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      requireStateQueueMint: true,
      requireFraudProofSpend: true,
    });
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
  const referenceScriptsByContract = Object.fromEntries(
    Object.entries({
      validationTraceDispute: referenceScripts.control.opener,
      validationTraceDisputeSource: referenceScripts.control.source,
      validationTraceDisputeGame: referenceScripts.control.game,
      validationTraceDisputeBoundary: referenceScripts.control.boundary,
      validationTraceDisputeTimeout: referenceScripts.control.timeout,
      validationTraceDisputeAward: referenceScripts.control.award,
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
    releaseIdentityDigest: "bb".repeat(32),
    network,
    blueprint: realBlueprint,
    deploymentInfo,
    releaseFinality: {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT,
      releaseIdentityDigest: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
      policy: finalityPolicy,
    },
    releaseEconomics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT,
      releaseIdentityDigest: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
      policy: economicsPolicy,
    },
    referenceScriptsByContract,
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
      result: await runOrResumeManifestBoundValidationTraceDisputeWorkflow({
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
      recorder.restore();
      await rm(directory, { recursive: true, force: true });
    },
  };
};

type Journey = Awaited<
  ReturnType<typeof stageInstalledValidationTraceDisputeJourney>
>;

const runToCompletion = async (
  journey: Journey,
  onAwaitCounterparty: (deadline: number) => Promise<void>,
) => {
  let restarts = 0;
  let { result } = await journey.runCold();
  for (let hop = 0; hop < 200 && result.kind !== "completed"; hop++) {
    restarts++;
    journey.emulator.awaitBlock();
    if (result.kind === "awaiting_counterparty") {
      await onAwaitCounterparty(result.responseDeadline);
      journey.emulator.awaitBlock();
    }
    ({ result } = await journey.runCold());
  }
  return { result, restarts };
};

const expectRemoved = async (journey: Journey) => {
  const workflow = await createManifestBoundValidationTraceDisputeWorkflow(
    journey.config,
  );
  expect((await workflow.deriveStage(Date.now())).kind).toBe("removed");
  // The fraud-proof token is permanent by design; the removed state-queue
  // node NFT burns while the proof token stays at the proof address.
  expect(
    await journey.config.lucid.utxosAtWithUnit(
      journey.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
      toUnit(
        journey.resolvedContracts.contracts.fraudProof.policyId,
        `00000006${journey.setup.headerHash}`,
      ),
    ),
  ).toHaveLength(1);
};

describe("validation trace dispute installed production workflow", () => {
  it("plays the full honest game to award and removal, refusing forged and caller-authored material at the exact checks", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney();
    try {
      // Refusal polarity 1 (construction): a caller-authored challenge that
      // is byte-identical to the admitted one but not the admitted object is
      // refused by the admission registry before any binding work happens.
      await expect(
        createManifestBoundValidationTraceDisputeWorkflow({
          ...journey.config,
          challenge: {
            ...journey.challenge,
          } as unknown as ValidationTraceChallenge,
        }),
      ).rejects.toThrow(
        "production validation-trace challenge is not admitted",
      );
      let refusalExercised = false;
      const { result, restarts } = await runToCompletion(journey, async () => {
        if (!refusalExercised) {
          refusalExercised = true;
          // Refusal polarity 2 (game move): a forged operator midpoint —
          // the honest committed proof with a tampered state hash — is
          // refused at the exact midpoint-verification conjunct the game
          // validator enforces, before any transaction is built.
          const honest = await journey.honestOperatorMove();
          const tampered = Buffer.from(honest.stateHash);
          tampered[0] = tampered[0]! ^ 0xff;
          await expect(
            journey.operatorResponds({ ...honest, stateHash: tampered }),
          ).rejects.toThrow("Invalid operator midpoint proof");
          // Positive polarity of the same check: the honest reveal at the
          // same position is accepted.
        }
        await journey.operatorResponds();
      });
      expect(result.kind, "txHash" in result ? result.txHash : undefined).toBe(
        "completed",
      );
      expect(refusalExercised).toBe(true);
      expect(restarts).toBeGreaterThan(2);
      await expectRemoved(journey);
    } finally {
      await journey.cleanup();
    }
  }, 600_000);

  it("claims the timeout and removes the block when the operator stalls past its deadline", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney();
    try {
      let stalls = 0;
      const { result, restarts } = await runToCompletion(
        journey,
        async (responseDeadline) => {
          // The operator never responds: let its response clock lapse so
          // the re-derived cursor arms the timeout claim.
          stalls++;
          journey.emulator.awaitSlot(
            Math.max(
              1,
              Math.ceil((responseDeadline - journey.emulator.now()) / 1000) + 5,
            ),
          );
        },
      );
      expect(result.kind, "txHash" in result ? result.txHash : undefined).toBe(
        "completed",
      );
      expect(stalls).toBe(1);
      // The stall line mints the proof through the timeout chain, never
      // through the one-step resolution or the award stage.
      const journal = new DirectoryFraudProofWorkflowJournalStore(
        join(journey.directory, "journal"),
      );
      const stages = (await journal.load(result.workflowId))
        .map(({ event }) =>
          event.kind === "submission_intent"
            ? (event.actionInput as { stage?: string } | undefined)?.stage
            : undefined,
        )
        .filter((stage): stage is string => stage !== undefined);
      expect(stages).toContain("enter_timeout");
      expect(stages).toContain("timeout");
      expect(stages).toContain("remove");
      expect(stages).not.toContain("award");
      expect(restarts).toBeGreaterThan(2);
      await expectRemoved(journey);
    } finally {
      await journey.cleanup();
    }
  }, 600_000);

  it("resumes cold from re-derived chain state after an interruption at the counterparty turn", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney();
    try {
      // Play through the production runner until the dispute is first
      // waiting on the operator, then "kill" the runner: nothing below
      // reuses any workflow object, plan, or in-memory cursor from before
      // this point.
      let { result } = await journey.runCold();
      for (
        let hop = 0;
        hop < 50 && result.kind !== "awaiting_counterparty";
        hop++
      ) {
        journey.emulator.awaitBlock();
        ({ result } = await journey.runCold());
      }
      if (result.kind !== "awaiting_counterparty") {
        throw new Error("dispute never reached the counterparty turn");
      }
      const interruptedDeadline = result.responseDeadline;
      // Cold restart while it is still not the watcher's turn: the fresh
      // runner re-derives the same waiting position from chain state only.
      journey.emulator.awaitBlock();
      const resumed = await journey.runCold();
      expect(resumed.result).toMatchObject({
        kind: "awaiting_counterparty",
        responseDeadline: interruptedDeadline,
      });
      // The operator responds; the next cold runner acts on its turn.
      await journey.operatorResponds();
      journey.emulator.awaitBlock();
      const acting = await journey.runCold();
      expect(acting.result.kind).toBe("pending");
      // Land that move in a block before the next cold restart. The runner
      // itself is already idempotent across an unconfirmed submission — it
      // replays the journal's outstanding `submission_intent` and returns
      // `pending` rather than re-planning — but the emulator reports a
      // mempool transaction as confirmed while its outputs are not yet
      // queryable, so an un-awaited restart would re-derive a cursor from a
      // chain that does not yet show the move.
      journey.emulator.awaitBlock();
      // And the resumed runner completes the journey end to end.
      const { result: finalResult } = await runToCompletion(
        journey,
        async () => {
          await journey.operatorResponds();
        },
      );
      expect(
        finalResult.kind,
        "txHash" in finalResult ? finalResult.txHash : undefined,
      ).toBe("completed");
      await expectRemoved(journey);
    } finally {
      await journey.cleanup();
    }
  }, 600_000);
});
