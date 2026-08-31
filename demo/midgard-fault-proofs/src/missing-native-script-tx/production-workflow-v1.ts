import {
  encodeMidgardTxInputCanonicalV1,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1,
  MissingNativeScriptTxStep02Datum,
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep06Datum,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep08Datum,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import {
  requireDeploymentScriptHash,
  type ResolvedProverSigner,
} from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY_V1,
  requireCompleteCanonicalReplayDecisionV1,
} from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStoreV1 } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowV1,
} from "../workflow/orchestrator-v1.js";
import {
  createProductionClaimRegistryPrerequisiteV1,
  type ProductionClaimRegistryPrerequisiteV1,
  type ProductionClaimRegistryPublicProofDeriverV1,
} from "../workflow/production-claim-registry-prerequisite-v1.js";
import {
  createProductionCursorFamilyWorkflowAdapterV1,
  PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionCursorFamilyTransactionPortV1,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureProductionCursorRemovalV1,
  productionCursorFamilyActionInputV1,
  productionCursorStringFieldV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1 } from "../workflow/production-cursor-family-spec-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import {
  type ProductionHistoricalNativeScriptCheckpointStoreV1,
  type ProductionHistoricalNativeScriptCorpusV1,
  type ProductionHistoricalNativeScriptHistorySourceV1,
  productionHistoricalNativeScriptPreimageFromCorpusV1,
  requireProductionHistoricalNativeScriptCorpusPreimageV1,
  requireProductionHistoricalNativeScriptHistoryAuthorityV1,
  resolveProductionHistoricalNativeScriptCorpusV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import { withProductionProofChunkPrerequisiteV1 } from "../workflow/production-proof-chunk-prerequisite-v1.js";
import type { FraudProofRawL1PointV1 } from "../workflow/raw-l1-snapshot-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "../workflow/release-finality-policy-v1.js";
import { captureLocallyEvaluatedTransactionV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  type HistoricalNativeScriptSourceRosterV1,
  requireProductionHistoricalNativeScriptSourceRosterV1,
  resolveHistoricalNativeScriptEvidenceV1,
} from "./historical-script-v1.js";
import {
  admitProductionMissingNativeScriptTxArtifactV1,
  type AdmittedProductionMissingNativeScriptTxArtifactV1,
  prepareProductionMissingNativeScriptTxArtifactV1,
  productionMissingNativeScriptTxArtifactUsesDirectRouteV1,
} from "./production-artifact-v1.js";
import { submitMissingNativeScriptTxStep01 } from "./submit-missing-native-script-tx-step-01.js";
import { submitMissingNativeScriptTxStep02 } from "./submit-missing-native-script-tx-step-02.js";
import { submitMissingNativeScriptTxStep03 } from "./submit-missing-native-script-tx-step-03.js";
import { submitMissingNativeScriptTxStep04 } from "./submit-missing-native-script-tx-step-04.js";
import { submitMissingNativeScriptTxStep05 } from "./submit-missing-native-script-tx-step-05.js";
import { submitMissingNativeScriptTxStep06 } from "./submit-missing-native-script-tx-step-06.js";
import { submitMissingNativeScriptTxStep06StartGrammarV1 } from "./submit-missing-native-script-tx-step-06-staged.js";
import { submitMissingNativeScriptTxStep07V1 } from "./submit-missing-native-script-tx-step-07.js";
import { submitMissingNativeScriptTxStep08V1 } from "./submit-missing-native-script-tx-step-08.js";

export type MissingNativeScriptTxWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"missingNativeScriptTx">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingNativeScriptTxContractsV1;
  references: MissingNativeScriptTxWorkflowReferenceScriptsV1;
  historicalCorpus: () => ProductionHistoricalNativeScriptCorpusV1;
  historicalSourceRoster: HistoricalNativeScriptSourceRosterV1;
  historicalThroughPoint: () => FraudProofRawL1PointV1;
  claimRegistry: ProductionClaimRegistryPrerequisiteV1<"missingNativeScriptTx">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const direct = (
  admitted: AdmittedProductionMissingNativeScriptTxArtifactV1,
): boolean =>
  admitted.evidence.badTxScriptWitnessItemCbors.length <=
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1;

const spendFieldPlan = (
  admitted: AdmittedProductionMissingNativeScriptTxArtifactV1,
  owner: string,
) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: admitted.evidence.badTxInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.evidence.badTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.badTxSpendInputs.map(
      encodeMidgardTxInputCanonicalV1,
    ),
    owner,
    publish: true,
    label: "missing-native-script-tx field 0",
  });

const outputFieldPlan = (
  admitted: AdmittedProductionMissingNativeScriptTxArtifactV1,
  owner: string,
) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: admitted.evidence.producingTxInclusion.nativeTxId,
    nativeTxCompactCbor:
      admitted.evidence.producingTxInclusion.nativeTxCompactCbor,
    itemCbors: admitted.evidence.producingOutputItemCbors,
    owner,
    publish: true,
    label: "missing-native-script-tx field 1",
  });

const scriptFieldPlan = (
  admitted: AdmittedProductionMissingNativeScriptTxArtifactV1,
  owner: string,
) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
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
  readonly config: BoundConfigV1;
  readonly planned: ReturnType<typeof spendFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("missing-native-script-tx field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
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
  config: BoundConfigV1,
): ProductionCursorFamilyTransactionPortV1<"missingNativeScriptTx"> => ({
  portVersion: PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  category: "missingNativeScriptTx",
  prepare: async ({ evidence, classification }) => {
    const corpus = config.historicalCorpus();
    const expectedScriptHash = expectedScriptHashFromDetection(
      classification.selected.detectionId,
    );
    const preimage = productionHistoricalNativeScriptPreimageFromCorpusV1({
      corpus,
      scriptHash: expectedScriptHash,
    });
    if (preimage === null) {
      throw new Error(
        "missing-native-script-tx complete history omitted the script preimage",
      );
    }
    const admittedPreimage =
      requireProductionHistoricalNativeScriptCorpusPreimageV1(preimage);
    if (
      admittedPreimage.providerRosterDigest !==
      config.historicalSourceRoster.applicationOverlayDigest
    ) {
      throw new Error(
        "missing-native-script-tx L1 and retained-history authorities came from different application overlays",
      );
    }
    const corroboration = await resolveHistoricalNativeScriptEvidenceV1({
      roster: config.historicalSourceRoster,
      expectedScriptHash,
      throughPoint: config.historicalThroughPoint(),
      releaseFinality: config.binding.releaseFinality,
      retainedDaCorroboratingScriptBytes: Buffer.from(
        admittedPreimage.scriptBytesHex,
        "hex",
      ),
    });
    return await prepareProductionMissingNativeScriptTxArtifactV1({
      evidence,
      classification,
      historicalNativeScriptCorpus: corpus,
      historicalL1Corroboration: corroboration,
    });
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitProductionMissingNativeScriptTxArtifactV1({
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
    const input = productionCursorFamilyActionInputV1({
      category: "missingNativeScriptTx",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () =>
      productionCursorStringFieldV1(input, "threadOutRef");
    const evidence = admitted.evidence;
    if (input.stage === "init") {
      const mutation = await config.claimRegistry.resolveMutation({
        headerHash: admitted.artifact.headerHash,
        action,
        artifact,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "missingNativeScriptTx",
              fraudulentBlockOutRef: productionCursorStringFieldV1(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              preparedClaimRegistryMutation: mutation,
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: productionCursorStringFieldV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: productionCursorStringFieldV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
      const mutation = direct(admitted)
        ? await config.claimRegistry.resolveMutation({
            headerHash: admitted.artifact.headerHash,
            action,
            artifact,
          })
        : undefined;
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            if (direct(admitted)) {
              await submitMissingNativeScriptTxStep06({
                ...shared,
                witnessReferenceScripts: config.references.witnesses,
                claimRegistryMutation: mutation,
                preSubmitBoundary,
              });
            } else {
              await submitMissingNativeScriptTxStep06StartGrammarV1({
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
      const mutation =
        input.stage === "step_08"
          ? await config.claimRegistry.resolveMutation({
              headerHash: admitted.artifact.headerHash,
              action,
              artifact,
            })
          : undefined;
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            if (input.stage === "step_07") {
              await submitMissingNativeScriptTxStep07V1({
                ...shared,
                preSubmitBoundary,
              });
            } else {
              await submitMissingNativeScriptTxStep08V1({
                ...shared,
                witnessReferenceScripts: config.references.witnesses,
                claimRegistryMutation: mutation,
                preSubmitBoundary,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureProductionCursorRemovalV1({
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

export type ManifestBoundMissingNativeScriptTxWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingNativeScriptTxWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  historicalNativeScriptCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalNativeScriptHistorySource: ProductionHistoricalNativeScriptHistorySourceV1;
  historicalNativeScriptL1Roster: HistoricalNativeScriptSourceRosterV1;
  claimRegistryProofs: ProductionClaimRegistryPublicProofDeriverV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingNativeScriptTxWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"missingNativeScriptTx">;
  l1: FraudProofFamilyL1ObservationPortV1<"missingNativeScriptTx">;
  transactions: ProductionCursorFamilyTransactionPortV1<"missingNativeScriptTx">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
  historicalNativeScriptCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalNativeScriptHistorySource: ProductionHistoricalNativeScriptHistorySourceV1;
}>;

type HistoricalCorpusCellV1 = {
  value?: ProductionHistoricalNativeScriptCorpusV1;
  throughPoint?: FraudProofRawL1PointV1;
};
const historicalCorpusCells = new WeakMap<object, HistoricalCorpusCellV1>();

export const createManifestBoundMissingNativeScriptTxWorkflowV1 = async (
  config: ManifestBoundMissingNativeScriptTxWorkflowConfigV1,
): Promise<ManifestBoundMissingNativeScriptTxWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
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
  requireProductionHistoricalNativeScriptHistoryAuthorityV1({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  requireProductionHistoricalNativeScriptSourceRosterV1(
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
  assertManifestBoundWorkflowSignerV1({
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
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as MissingNativeScriptTxWorkflowReferenceScriptsV1["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScriptsV1>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: MissingNativeScriptTxWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze(steps),
      witnesses: Object.freeze({
        claimRegistrySpend: witness("claimRegistrySpend", "claimRegistrySpend"),
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
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    });
  const contracts: MissingNativeScriptTxContractsV1 = Object.freeze({
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
    claimRegistry: binding.claimRegistry,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
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
  const claimRegistry = createProductionClaimRegistryPrerequisiteV1({
    category: "missingNativeScriptTx",
    categoryId: binding.resolvedContracts.category.categoryId,
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    computationThreadPolicyId:
      binding.resolvedContracts.contracts.computationThread.policyId,
    claimRegistryAddress: credentialToAddress(binding.network, {
      type: "Script",
      hash: requireDeploymentScriptHash(
        binding.deploymentInfo,
        "claimRegistrySpend",
      ),
    }),
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    rawL1: l1.rawL1,
    releaseFinality: binding.releaseFinality,
    publications: l1.publications,
    proofs: config.claimRegistryProofs,
    mutationForAction: ({ action, artifact }) => {
      const stage = productionCursorFamilyActionInputV1({
        category: "missingNativeScriptTx",
        action,
      }).stage;
      if (stage === "init") return { kind: "open" };
      if (stage === "step_08") return { kind: "close" };
      return stage === "step_06" &&
        productionMissingNativeScriptTxArtifactUsesDirectRouteV1(artifact)
        ? { kind: "close" }
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  const corpusCell: HistoricalCorpusCellV1 = {};
  const bound: BoundConfigV1 = {
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
    claimRegistry,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createProductionCursorFamilyWorkflowAdapterV1({
    spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "missingNativeScriptTx",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "missingNativeScriptTx",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: async ({ action, artifact }) => {
        const admitted = await admitProductionMissingNativeScriptTxArtifactV1({
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
              : ["step_06", "step_07", "step_08"].includes(
                    String(action.input.stage),
                  )
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
        } satisfies ProductionFieldCarriageRequirementV1;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "missingNativeScriptTx",
    base: adapter,
    prerequisite: claimRegistry.proofChunks,
  });
  const workflow = Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
    historicalNativeScriptCheckpointStore:
      config.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      config.historicalNativeScriptHistorySource,
  });
  historicalCorpusCells.set(workflow, corpusCell);
  return workflow;
};

export const runOrResumeManifestBoundMissingNativeScriptTxWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingNativeScriptTxWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
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
  const evidence = await fetchCanonicalBlockEvidenceV1({
    observation,
    sources,
    minimumConfirmationDepth:
      workflow.binding.releaseFinality.policy.confirmationDepth,
  });
  const corpus = await resolveProductionHistoricalNativeScriptCorpusV1({
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
    await MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY_V1.replay(
      evidence,
    );
  const detections = requireCompleteCanonicalReplayDecisionV1({
    evidence,
    replayer: MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY_V1,
    decision,
  });
  return await runFraudProofWorkflowV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["missingNativeScriptTx"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
