import {
  encodeMidgardTxInputCanonical,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
  MissingNativeScriptTxStep02Datum,
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep06Datum,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep08Datum,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "../workflow/field-carriage-prerequisite.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  historicalNativeScriptPreimageFromCorpus,
  requireHistoricalNativeScriptCorpusPreimage,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import type { FraudProofRawL1Point } from "../workflow/raw-l1-snapshot.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMissingNativeScriptTxArtifact,
  type AdmittedMissingNativeScriptTxArtifact,
  prepareMissingNativeScriptTxArtifact,
} from "./artifact.js";
import type { MissingNativeScriptTxContracts } from "./contracts.js";
import {
  type HistoricalNativeScriptSourceRoster,
  requireHistoricalNativeScriptSourceRoster,
  resolveHistoricalNativeScriptEvidence,
} from "./historical-script.js";
import { submitMissingNativeScriptTxStep01 } from "./submit-missing-native-script-tx-step-01.js";
import { submitMissingNativeScriptTxStep02 } from "./submit-missing-native-script-tx-step-02.js";
import { submitMissingNativeScriptTxStep03 } from "./submit-missing-native-script-tx-step-03.js";
import { submitMissingNativeScriptTxStep04 } from "./submit-missing-native-script-tx-step-04.js";
import { submitMissingNativeScriptTxStep05 } from "./submit-missing-native-script-tx-step-05.js";
import { submitMissingNativeScriptTxStep06 } from "./submit-missing-native-script-tx-step-06.js";
import { submitMissingNativeScriptTxStep06StartGrammar } from "./submit-missing-native-script-tx-step-06-staged.js";
import { submitMissingNativeScriptTxStep07 } from "./submit-missing-native-script-tx-step-07.js";
import { submitMissingNativeScriptTxStep08 } from "./submit-missing-native-script-tx-step-08.js";

export type MissingNativeScriptTxWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptTx">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingNativeScriptTxContracts;
  references: MissingNativeScriptTxWorkflowReferenceScripts;
  historicalCorpus: () => HistoricalNativeScriptCorpus;
  historicalSourceRoster: HistoricalNativeScriptSourceRoster;
  historicalThroughPoint: () => FraudProofRawL1Point;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const direct = (admitted: AdmittedMissingNativeScriptTxArtifact): boolean =>
  admitted.evidence.badTxScriptWitnessItemCbors.length <=
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT;

const spendFieldPlan = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
  owner: string,
) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: admitted.evidence.badTxInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.evidence.badTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.badTxSpendInputs.map(
      encodeMidgardTxInputCanonical,
    ),
    owner,
    publish: true,
    label: "missing-native-script-tx field 0",
  });

const outputFieldPlan = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
  owner: string,
) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: admitted.evidence.producingTxInclusion.nativeTxId,
    nativeTxCompactCbor:
      admitted.evidence.producingTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.producingOutputItemCbors,
    owner,
    publish: true,
    label: "missing-native-script-tx field 1",
  });

const scriptFieldPlan = (
  admitted: AdmittedMissingNativeScriptTxArtifact,
  owner: string,
) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: admitted.evidence.badTxInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.evidence.badTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.badTxScriptWitnessItemCbors,
    owner,
    publish: true,
    witnessSet: admitted.evidence.badTxWitnessSet,
    anchorWitnessSetHash:
      admitted.evidence.badTxInclusion.nativeTx.witness_set_hash,
    label: "missing-native-script-tx field 6",
  });

const resolveField = async ({
  config,
  planned,
}: {
  readonly config: BoundConfig;
  readonly planned: ReturnType<typeof spendFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("missing-native-script-tx field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("missing-native-script-tx certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const expectedScriptHashFromDetection = (detectionId: string): string => {
  const fields = detectionId.split(":");
  const value = fields.length === 8 ? fields[7] : undefined;
  if (value === undefined || !/^[0-9a-f]{56}$/u.test(value)) {
    throw new Error(
      "missing-native-script-tx detection omitted its exact script hash",
    );
  }
  return value;
};

const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"missingNativeScriptTx"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "missingNativeScriptTx",
  prepare: async ({ evidence, classification }) => {
    const corpus = config.historicalCorpus();
    const expectedScriptHash = expectedScriptHashFromDetection(
      classification.selected.detectionId,
    );
    const preimage = historicalNativeScriptPreimageFromCorpus({
      corpus,
      scriptHash: expectedScriptHash,
    });
    if (preimage === null) {
      throw new Error(
        "missing-native-script-tx complete history omitted the script preimage",
      );
    }
    const admittedPreimage =
      requireHistoricalNativeScriptCorpusPreimage(preimage);
    if (
      admittedPreimage.providerRosterDigest !==
      config.historicalSourceRoster.applicationOverlayDigest
    ) {
      throw new Error(
        "missing-native-script-tx L1 and retained-history authorities came from different application overlays",
      );
    }
    const corroboration = await resolveHistoricalNativeScriptEvidence({
      roster: config.historicalSourceRoster,
      expectedScriptHash,
      throughPoint: config.historicalThroughPoint(),
      releaseFinality: config.binding.releaseFinality,
      retainedDaCorroboratingScriptBytes: Buffer.from(
        admittedPreimage.scriptBytesHex,
        "hex",
      ),
    });
    return await prepareMissingNativeScriptTxArtifact({
      evidence,
      classification,
      historicalNativeScriptCorpus: corpus,
      historicalL1Corroboration: corroboration,
    });
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitMissingNativeScriptTxArtifact({
      value: artifact,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      historicalSourceRoster: config.historicalSourceRoster,
      historicalThroughPoint: config.historicalThroughPoint(),
      releaseFinality: config.binding.releaseFinality,
    });
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "missing-native-script-tx artifact changed the bound header",
      );
    }
    const input = cursorFamilyActionInput({
      category: "missingNativeScriptTx",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    const evidence = admitted.evidence;
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "missingNativeScriptTx",
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: evidence.badTxInclusion,
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField({
        config,
        planned: spendFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: evidence.badTxInclusion.nativeTxCompactCbor,
              spendInputs: evidence.badTxSpendInputs,
              badInputIndex: evidence.badInputIndex,
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: evidence.producingTxInclusion,
              referenceScriptUtxo: config.references.steps[2],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      const carriage = await resolveField({
        config,
        planned: outputFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor:
                evidence.producingTxInclusion.nativeTxCompactCbor,
              outputItemCbors: evidence.producingOutputItemCbors,
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_05") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              missingNativeScriptBytes: evidence.missingNativeScriptBytes,
              referenceScriptUtxo: config.references.steps[4],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_06") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      const shared = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId,
        signer: config.signer,
        threadOutRef: threadOutRef(),
        nativeTxCompactCbor: evidence.badTxInclusion.nativeTxCompactCbor,
        witnessSet: evidence.badTxWitnessSet,
        scriptTxWitsItems: evidence.badTxScriptWitnessItemCbors,
        publishedCarriageUtxos: carriage.publications,
        ...(carriage.certificate === undefined
          ? {}
          : { certificateUtxo: carriage.certificate }),
        referenceScriptUtxo: config.references.steps[5],
        awaitConfirmation: false,
      } as const;
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            if (direct(admitted)) {
              await submitMissingNativeScriptTxStep06({
                ...shared,
                witnessReferenceScripts: config.references.witnesses,
                preSubmitBoundary,
              });
            } else {
              await submitMissingNativeScriptTxStep06StartGrammar({
                ...shared,
                preSubmitBoundary,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "step_07" || input.stage === "step_08") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      const shared = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId,
        signer: config.signer,
        threadOutRef: threadOutRef(),
        nativeTxCompactCbor: evidence.badTxInclusion.nativeTxCompactCbor,
        witnessSet: evidence.badTxWitnessSet,
        scriptTxWitsItems: evidence.badTxScriptWitnessItemCbors,
        publishedCarriageUtxos: carriage.publications,
        ...(carriage.certificate === undefined
          ? {}
          : { certificateUtxo: carriage.certificate }),
        referenceScriptUtxo:
          input.stage === "step_07"
            ? config.references.steps[6]
            : config.references.steps[7],
        awaitConfirmation: false,
      } as const;
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            if (input.stage === "step_07") {
              await submitMissingNativeScriptTxStep07({
                ...shared,
                preSubmitBoundary,
              });
            } else {
              await submitMissingNativeScriptTxStep08({
                ...shared,
                witnessReferenceScripts: config.references.witnesses,
                preSubmitBoundary,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureCursorRemoval({
        category: "missingNativeScriptTx",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    }
    throw new Error(
      `missing-native-script-tx unsupported stage ${input.stage}`,
    );
  },
});

export type ManifestBoundMissingNativeScriptTxWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingNativeScriptTxWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  historicalNativeScriptL1Roster: HistoricalNativeScriptSourceRoster;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingNativeScriptTxWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptTx">;
  l1: FraudProofFamilyL1ObservationPort<"missingNativeScriptTx">;
  transactions: CursorFamilyTransactionPort<"missingNativeScriptTx">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

type HistoricalCorpusCell = {
  value?: HistoricalNativeScriptCorpus;
  throughPoint?: FraudProofRawL1Point;
};
const historicalCorpusCells = new WeakMap<object, HistoricalCorpusCell>();

export const createManifestBoundMissingNativeScriptTxWorkflow = async (
  config: ManifestBoundMissingNativeScriptTxWorkflowConfig,
): Promise<ManifestBoundMissingNativeScriptTxWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "missingNativeScriptTx",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MissingNativeScriptTxStep02Datum,
      MissingNativeScriptTxStep03Datum,
      MissingNativeScriptTxStep04Datum,
      MissingNativeScriptTxStep05Datum,
      MissingNativeScriptTxStep06Datum,
      MissingNativeScriptTxStep07Datum,
      MissingNativeScriptTxStep08Datum,
    ],
  });
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  requireHistoricalNativeScriptSourceRoster(
    config.historicalNativeScriptL1Roster,
    binding.releaseFinality,
  );
  if (
    config.historicalNativeScriptHistorySource.providerRosterDigest !==
    config.historicalNativeScriptL1Roster.applicationOverlayDigest
  ) {
    throw new Error(
      "missing-native-script-tx history sources do not share one application overlay",
    );
  }
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.missingNativeScriptTx;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    chain.steps.length !== 8 ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "missing-native-script-tx manifest omitted required contracts",
    );
  }
  const stepNames = [
    "fraudProofMissingNativeScriptTx",
    "fraudProofMissingNativeScriptTxStep02",
    "fraudProofMissingNativeScriptTxStep03",
    "fraudProofMissingNativeScriptTxStep04",
    "fraudProofMissingNativeScriptTxStep05",
    "fraudProofMissingNativeScriptTxStep06",
    "fraudProofMissingNativeScriptTxStep07",
    "fraudProofMissingNativeScriptTxStep08",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as MissingNativeScriptTxWorkflowReferenceScripts["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScripts>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: MissingNativeScriptTxWorkflowReferenceScripts =
    Object.freeze({
      steps: Object.freeze(steps),
      witnesses: Object.freeze({
        computationThreadMint: witness(
          "computationThreadMint",
          "computationThreadMint",
        ),
        fraudProofMint: witness("fraudProofMint", "fraudProofMint"),
        phasMembershipWithdraw: witness(
          "phasMembershipWithdraw",
          "phasMembershipWithdraw",
        ),
        chunkedVerifyWithdraw: witness(
          "chunkedVerifyWithdraw",
          "chunkedVerifyWithdraw",
        ),
        pexcludesWithdraw: witness("pexcludesWithdraw", "pexcludesWithdraw"),
      }),
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    });
  const contracts: MissingNativeScriptTxContracts = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined) {
    throw new Error("missing-native-script-tx raw L1 authority is unavailable");
  }
  if (l1.observeBoundary === undefined) {
    throw new Error(
      "missing-native-script-tx raw L1 boundary authority is unavailable",
    );
  }
  const corpusCell: HistoricalCorpusCell = {};
  const bound: BoundConfig = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    historicalCorpus: () => {
      const corpus = corpusCell.value;
      if (corpus === undefined) {
        throw new Error(
          "missing-native-script-tx history was not derived from this workflow's public DA sources",
        );
      }
      return corpus;
    },
    historicalSourceRoster: config.historicalNativeScriptL1Roster,
    historicalThroughPoint: () => {
      const point = corpusCell.throughPoint;
      if (point === undefined) {
        throw new Error(
          "missing-native-script-tx L1 history boundary was not derived from the authenticated header observation",
        );
      }
      return point;
    },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createCursorFamilyWorkflowAdapter({
    spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withFieldCarriagePrerequisite({
    category: "missingNativeScriptTx",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
      category: "missingNativeScriptTx",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: async ({ action, artifact }) => {
        const admitted = await admitMissingNativeScriptTxArtifact({
          value: artifact,
          historicalNativeScriptCorpus: bound.historicalCorpus(),
          historicalSourceRoster: bound.historicalSourceRoster,
          historicalThroughPoint: bound.historicalThroughPoint(),
          releaseFinality: binding.releaseFinality,
        });
        const planned =
          action.input.stage === "step_02"
            ? spendFieldPlan(admitted, config.signer.paymentKeyHash)
            : action.input.stage === "step_04"
              ? outputFieldPlan(admitted, config.signer.paymentKeyHash)
              : typeof action.input.stage === "string" &&
                  ["step_06", "step_07", "step_08"].includes(action.input.stage)
                ? scriptFieldPlan(admitted, config.signer.paymentKeyHash)
                : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor:
            action.input.stage === "step_04"
              ? admitted.evidence.producingTxInclusion.nativeTxCompactCbor
              : admitted.evidence.badTxInclusion.nativeTxCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  const workflow = Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    historicalNativeScriptCheckpointStore:
      config.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      config.historicalNativeScriptHistorySource,
  });
  historicalCorpusCells.set(workflow, corpusCell);
  return workflow;
};

export const runOrResumeManifestBoundMissingNativeScriptTxWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingNativeScriptTxWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const throughPoint = await workflow.l1.observeBoundary?.({
    headerHash: workflow.binding.definition.headerHash,
  });
  if (throughPoint === undefined) {
    throw new Error(
      "missing-native-script-tx raw L1 boundary authority disappeared",
    );
  }
  const evidence = await fetchCanonicalBlockEvidence({
    observation,
    sources,
    minimumConfirmationDepth:
      workflow.binding.releaseFinality.policy.confirmationDepth,
  });
  const corpus = await resolveHistoricalNativeScriptCorpus({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.historicalNativeScriptCheckpointStore,
    historySource: workflow.historicalNativeScriptHistorySource,
    currentEvidence: evidence,
    sources,
  });
  const cell = historicalCorpusCells.get(workflow);
  if (cell === undefined) {
    throw new Error(
      "missing-native-script-tx workflow was not created by its manifest-bound constructor",
    );
  }
  if (
    cell.throughPoint !== undefined &&
    (cell.throughPoint.slot !== throughPoint.slot ||
      cell.throughPoint.blockHash !== throughPoint.blockHash ||
      cell.throughPoint.blockNo !== throughPoint.blockNo ||
      cell.throughPoint.pointId !== throughPoint.pointId)
  ) {
    throw new Error(
      "missing-native-script-tx authenticated L1 history boundary changed across resume",
    );
  }
  cell.throughPoint = throughPoint;
  if (
    cell.value !== undefined &&
    cell.value.corpusDigest !== corpus.corpusDigest
  ) {
    throw new Error(
      "missing-native-script-tx authenticated history changed across resume",
    );
  }
  cell.value = corpus;
  const decision =
    await MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer: MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY,
    decision,
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["missingNativeScriptTx"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
