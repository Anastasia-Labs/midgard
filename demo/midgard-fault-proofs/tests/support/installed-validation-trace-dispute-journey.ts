import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { toUnit } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import {
  createManifestBoundValidationTraceDisputeWorkflow,
  executeManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflowConfig,
} from "../../src/validation-dispute/workflow-v1.js";
import { admitValidationTraceChallenge } from "../../src/workflow/challenge-authority.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../../src/workflow/journal.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import {
  alwaysSucceedsBlueprintPath,
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
import { submitSecondHeaderTx, submitSetupTx } from "./emulator/setup-tx.js";
import { buildAcceptedClaimOverMinAdaRejectingTransactionFixture } from "./emulator/validation-dispute-fixtures.js";
import { buildInstalledSignatureFixture } from "./installed-signature-fixture.js";
import { stageInstalledValidationResolutionReferences } from "./installed-validation-resolution-references.js";
import { buildInstalledValidationWorkflowBinding } from "./installed-validation-trace-dispute-journey.build-workflow-binding.js";
import { installedValidationOperatorCounterparty } from "./installed-validation-trace-dispute-journey.operator-counterparty.js";
import type {
  InstalledValidationChallengeSupply,
  InstalledValidationJourneyFixture,
  InstalledValidationJourneyStaged,
} from "./installed-validation-trace-dispute-journey.types.js";
import { installedWorkflowEmulatorClock } from "./installed-workflow.advance-emulator-observation.js";
import { useCanonicalValidationDisputeCursorReads } from "./validation-trace-dispute-canonical-cursor.js";

export type * from "./installed-validation-trace-dispute-journey.types.js";

const DEPLOYMENT = "11".repeat(32);
const PAYLOAD_ENVELOPE_SHA = "ab".repeat(32);
const PAYLOAD_SHA = "cd".repeat(32);

/**
 * The fixture's challenge, admitted by the production challenge authority at
 * a fixed payload coordinate (the replay input is the fixture's own).
 */
export const admitFixedCoordinateChallenge = async ({
  fixture,
  setup,
}: Readonly<{
  fixture: Readonly<{
    header: InstalledValidationJourneyFixture["header"];
    claim: Parameters<typeof admitValidationTraceChallenge>[0]["claim"];
    challengerReplayInput: Parameters<
      typeof admitValidationTraceChallenge
    >[0]["challengerReplayInput"];
  }>;
  setup: Readonly<{ headerHash: string }>;
}>): Promise<InstalledValidationChallengeSupply> => ({
  deploymentFingerprint: DEPLOYMENT,
  challenge: await admitValidationTraceChallenge({
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
  }),
});

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
  const journey = await stageJourney<
    | Awaited<ReturnType<typeof buildInstalledSignatureFixture>>
    | Awaited<
        ReturnType<
          typeof buildAcceptedClaimOverMinAdaRejectingTransactionFixture
        >
      >
  >({
    hooks,
    certifiedSignatures,
    buildFixture: async ({ operatorVkey, now }) =>
      await (
        fixtureBuilder ??
        (certifiedSignatures
          ? buildInstalledSignatureFixture
          : buildAcceptedClaimOverMinAdaRejectingTransactionFixture)
      )({
        operatorVkey,
        now,
        terminalCounterMismatch,
        repeatedRequiredSigners,
      }),
    supplyChallenge: async (staged) =>
      await admitFixedCoordinateChallenge(staged),
  });
  return { ...journey, challenge: journey.challenge!, config: journey.config! };
};

/**
 * The same staged journey over a supplied fixture and challenge: the
 * supplier reads the staged ledger (for example to capture the challenge
 * from an L1 follower fed with the emulator's transactions) and names the
 * deployment the challenge is bound to.
 */
export const stageSuppliedValidationTraceDisputeJourney = async <
  Fixture extends InstalledValidationJourneyFixture,
>(
  hooks: { binding: unknown; authority: unknown },
  supply: Readonly<{
    fixture: (
      input: Readonly<{ operatorVkey: string; now: number }>,
    ) => Promise<Fixture>;
    challenge: (
      staged: InstalledValidationJourneyStaged<Fixture>,
    ) => Promise<InstalledValidationChallengeSupply>;
  }>,
) =>
  await stageJourney({
    hooks,
    certifiedSignatures: false,
    buildFixture: supply.fixture,
    supplyChallenge: supply.challenge,
  });

const stageJourney = async <Fixture extends InstalledValidationJourneyFixture>({
  hooks,
  certifiedSignatures,
  buildFixture,
  supplyChallenge,
}: {
  readonly hooks: { binding: unknown; authority: unknown };
  readonly certifiedSignatures: boolean;
  readonly buildFixture: (
    input: Readonly<{ operatorVkey: string; now: number }>,
  ) => Promise<Fixture>;
  readonly supplyChallenge: (
    staged: InstalledValidationJourneyStaged<Fixture>,
  ) => Promise<InstalledValidationChallengeSupply>;
}) => {
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
  const fixture = await buildFixture({
    operatorVkey: operatorSigner.paymentKeyHash,
    now: headerStartTime,
  });
  emulator.awaitSlot(
    Math.max(0, Math.ceil((headerStartTime - 120_000 - emulator.now()) / 1000)),
  );
  const firstSetup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.predecessorHeader ?? fixture.header,
    }),
  );
  const setup =
    fixture.predecessorHeader === undefined
      ? firstSetup
      : await runEmulatorLifecycleStage("second header", async () => {
          // The challenged header extends the committed predecessor; its
          // commit window opens at the predecessor's end.
          emulator.awaitSlot(
            Math.max(
              0,
              Math.ceil(
                (Number(fixture.header.startTime) - emulator.now()) / 1000,
              ) + 1,
            ),
          );
          const second = await submitSecondHeaderTx({
            lucid: operatorLucid,
            contracts,
            header: fixture.header,
          });
          return {
            ...firstSetup,
            fraudulentBlockOutRef: second.blockOutRef,
            headerHash: second.headerHash,
          };
        });
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
    referenceScriptAuth,
    referenceScriptPublisher,
  });
  const restoreCursorReads = useCanonicalValidationDisputeCursorReads(
    targetChallengerLucid,
    emulator,
  );
  const { challenge, deploymentFingerprint, decisionDigest } =
    await supplyChallenge({
      emulator,
      recorder,
      contracts,
      nonceUtxo,
      fixture,
      setup,
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
  const binding = buildInstalledValidationWorkflowBinding({
    deploymentFingerprint,
    realBlueprint,
    deploymentInfo,
    referenceScripts,
    stagedReferences: {
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
    },
    contracts,
    headerHash: setup.headerHash,
    proverCredential: challengerSigner.paymentKeyHash,
    resolvedContracts,
  });
  hooks.binding = binding;
  const directory = await mkdtemp(
    join(tmpdir(), "validation-trace-installed-"),
  );
  const context = {
    manifest: {},
    blueprintJson: JSON.stringify(realBlueprint),
    deploymentInfo,
    headerHash: setup.headerHash,
    lucid: targetChallengerLucid,
    signer: challengerSigner,
    l1Source: {} as never,
    decisionDigest: decisionDigest ?? "dd".repeat(32),
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
  } satisfies Omit<
    ManifestBoundValidationTraceDisputeWorkflowConfig,
    "challenge"
  >;
  const config: ManifestBoundValidationTraceDisputeWorkflowConfig | undefined =
    challenge === undefined ? undefined : { ...context, challenge };
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
    if (config === undefined)
      throw new Error("The journey was staged without a challenge");
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
  const { operatorResponds, honestOperatorMove } =
    installedValidationOperatorCounterparty({
      lucid: targetOperatorLucid,
      threadUnit,
      proofs: fixture.operatorTrace.tree.proofs,
      realBlueprint,
      deploymentInfo,
      signer: operatorSigner,
      gameReferenceScriptUtxo: referenceScripts.control.game,
      validityRange,
    });
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
    /** The workflow configuration without the challenge. */
    context,
    /** The staged validator references, by role. */
    referenceScripts,
    realBlueprint,
    binding,
    contracts,
    deploymentInfo,
    witnessReferenceScripts,
    operatorLucid,
    targetOperatorLucid,
    targetChallengerLucid,
    operatorSigner,
    challengerSigner,
    validityRange,
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
