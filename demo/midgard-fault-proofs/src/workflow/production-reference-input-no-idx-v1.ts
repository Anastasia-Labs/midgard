import {
  encodeMidgardTxInputCanonicalV1,
  encodeMidgardTxOutputCanonicalV1,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep03Datum,
  ReferenceInputNoIdxStep04Datum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import { prepareReferenceInputNoIdxFromCanonicalEvidenceV1 } from "../prepare-reference-input-no-idx.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitReferenceInputNoIdxStep01 } from "../submit-reference-input-no-idx-step-01.js";
import {
  parseSubmitReferenceInputNoIdxReferenceInputsPreimage,
  type SubmitReferenceInputNoIdxReferenceInputsPreimage,
  submitReferenceInputNoIdxStep02,
} from "../submit-reference-input-no-idx-step-02.js";
import { submitReferenceInputNoIdxStep03 } from "../submit-reference-input-no-idx-step-03.js";
import {
  parseSubmitReferenceInputNoIdxOutputsPreimage,
  type SubmitReferenceInputNoIdxOutputsPreimage,
  submitReferenceInputNoIdxStep04,
} from "../submit-reference-input-no-idx-step-04.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStoreV1,
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import {
  admitProductionNativeInclusionArtifactV1,
  admitProductionOutputCborListV1,
  admitProductionTxInputListV1,
  canonicalHexV1,
  canonicalNaturalStringV1,
  exactJournalRecordV1,
  HEX_28_V1,
  HEX_32_V1,
  NATURAL_DECIMAL_V1,
  type ProductionNativeInclusionArtifactV1,
  safeNaturalNumberV1,
} from "./production-native-index-artifact-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  resolveDirectFirstProofChunksV1,
  withProductionProofChunkPrerequisiteV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_REFERENCE_INPUT_NO_IDX_ARTIFACT_V1 =
  "midgard-production-reference-input-no-idx-artifact-v1" as const;

export type ProductionReferenceInputNoIdxArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_REFERENCE_INPUT_NO_IDX_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: ProductionNativeInclusionArtifactV1;
    producingTx: ProductionNativeInclusionArtifactV1;
    referenceInputs: readonly Readonly<{
      tx_id: string;
      output_index: string;
    }>[];
    badReferenceInputIndex: number;
    outputsPreimageCbor: readonly string[];
    badReferenceInputOutputIndex: string;
  }>;

type AdmittedArtifactV1 = Readonly<{
  artifact: ProductionReferenceInputNoIdxArtifactV1;
  badInclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  producingInclusion: ReturnType<
    typeof admitProductionNativeInclusionArtifactV1
  >["inclusion"];
  referenceInputs: SubmitReferenceInputNoIdxReferenceInputsPreimage;
  outputs: SubmitReferenceInputNoIdxOutputsPreimage;
  referenceInputFieldPlan: FaultProofFieldOpeningPlanV1;
  outputFieldPlan: FaultProofFieldOpeningPlanV1;
}>;

export const admitProductionReferenceInputNoIdxArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifactV1 => {
  if (!HEX_28_V1.test(carriageOwner)) {
    throw new Error("reference-input-no-idx carriage owner is malformed");
  }
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "producingTx",
      "referenceInputs",
      "badReferenceInputIndex",
      "outputsPreimageCbor",
      "badReferenceInputOutputIndex",
    ],
    "reference-input-no-idx artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_REFERENCE_INPUT_NO_IDX_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("reference-input-no-idx artifact identity changed");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "reference-input-no-idx header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "reference-input-no-idx position",
  );
  const badReferenceInputIndex = safeNaturalNumberV1(
    parsed.badReferenceInputIndex,
    "reference-input-no-idx selected reference input",
  );
  const badReferenceInputOutputIndex = canonicalNaturalStringV1(
    parsed.badReferenceInputOutputIndex,
    "reference-input-no-idx output index",
  );
  const bad = admitProductionNativeInclusionArtifactV1(
    parsed.badTx,
    "reference-input-no-idx bad transaction",
  );
  const producing = admitProductionNativeInclusionArtifactV1(
    parsed.producingTx,
    "reference-input-no-idx producing transaction",
  );
  if (
    bad.artifact.transactionsPhasRoot !==
    producing.artifact.transactionsPhasRoot
  ) {
    throw new Error(
      "reference-input-no-idx inclusions do not share one transactions root",
    );
  }
  const referenceInputList = admitProductionTxInputListV1(
    parsed.referenceInputs,
    "reference-input-no-idx reference inputs",
  );
  const outputList = admitProductionOutputCborListV1(
    parsed.outputsPreimageCbor,
    "reference-input-no-idx outputs",
  );
  const badReferenceInput = referenceInputList.inputs[badReferenceInputIndex];
  if (
    badReferenceInput === undefined ||
    badReferenceInput.tx_id !== producing.artifact.nativeTxId ||
    badReferenceInput.output_index.toString() !==
      badReferenceInputOutputIndex ||
    badReferenceInput.output_index < BigInt(outputList.outputs.length)
  ) {
    throw new Error(
      "reference-input-no-idx artifact does not re-derive its violation",
    );
  }
  const expectedDetection = `${REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1}:${position.toString()}:${badReferenceInputIndex.toString()}:${bad.artifact.nativeTxId}:${producing.artifact.nativeTxId}:${badReferenceInputOutputIndex}:${outputList.outputs.length.toString()}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error(
      "reference-input-no-idx artifact detection identity changed",
    );
  }
  const referenceInputFieldPlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: bad.artifact.nativeTxId,
    nativeTxCompactCbor: bad.artifact.nativeTxCompactCbor,
    itemCbors: referenceInputList.inputs.map(encodeMidgardTxInputCanonicalV1),
    owner: carriageOwner,
    label: "reference-input-no-idx reference inputs",
  });
  const outputFieldPlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: producing.artifact.nativeTxId,
    nativeTxCompactCbor: producing.artifact.nativeTxCompactCbor,
    itemCbors: outputList.outputs.map(encodeMidgardTxOutputCanonicalV1),
    owner: carriageOwner,
    label: "reference-input-no-idx outputs",
  });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_REFERENCE_INPUT_NO_IDX_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    badTx: bad.artifact,
    producingTx: producing.artifact,
    referenceInputs: referenceInputList.json,
    badReferenceInputIndex,
    outputsPreimageCbor: outputList.json,
    badReferenceInputOutputIndex,
  }) satisfies ProductionReferenceInputNoIdxArtifactV1;
  return Object.freeze({
    artifact,
    badInclusion: bad.inclusion,
    producingInclusion: producing.inclusion,
    referenceInputs: parseSubmitReferenceInputNoIdxReferenceInputsPreimage({
      value: referenceInputList.json,
      badReferenceInputIndex,
    }),
    outputs: parseSubmitReferenceInputNoIdxOutputsPreimage(outputList.json),
    referenceInputFieldPlan,
    outputFieldPlan,
  });
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  const badReferenceInputIndex = Number(fields[2]);
  if (
    classification.category !== "referenceInputNoIdx" ||
    classification.selected.violationId !==
      REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1 ||
    fields.length !== 7 ||
    fields[0] !== REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1 ||
    !NATURAL_DECIMAL_V1.test(fields[1] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[2] ?? "") ||
    !HEX_32_V1.test(fields[3] ?? "") ||
    !HEX_32_V1.test(fields[4] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[5] ?? "") ||
    !NATURAL_DECIMAL_V1.test(fields[6] ?? "") ||
    !Number.isSafeInteger(position) ||
    !Number.isSafeInteger(badReferenceInputIndex) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error(
      "reference-input-no-idx classification identity is malformed",
    );
  }
  return Object.freeze({
    position,
    badReferenceInputIndex,
    badTxId: fields[3]!,
    producingTxId: fields[4]!,
    badReferenceInputOutputIndex: fields[5]!,
    producingTxOutputCount: fields[6]!,
  });
};

export const prepareProductionReferenceInputNoIdxArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareReferenceInputNoIdxFromCanonicalEvidenceV1
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionReferenceInputNoIdxArtifactV1> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "reference-input-no-idx classification differs from evidence",
    );
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareReferenceInputNoIdxFromCanonicalEvidenceV1({
    evidence,
    badTxId: selected.badTxId,
    badReferenceInputIndex: selected.badReferenceInputIndex,
  });
  if (
    prepared.producingTxId !== selected.producingTxId ||
    prepared.badReferenceInput.output_index.toString() !==
      selected.badReferenceInputOutputIndex ||
    prepared.producingTxOutputCount.toString() !==
      selected.producingTxOutputCount
  ) {
    throw new Error(
      "reference-input-no-idx prepared evidence changed classification",
    );
  }
  const inclusion = (
    item: typeof prepared.badTxInclusion,
  ): ProductionNativeInclusionArtifactV1 => ({
    nativeTxId: item.nativeTxId,
    nativeTxCompactCbor: item.nativeTxCompactCbor,
    l2TransactionSourceCbor: item.l2TransactionSourceCbor,
    transactionsPhasRoot: item.transactionsPhasRoot,
    txMembershipProofCbor: item.txMembershipProofCbor,
  });
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_REFERENCE_INPUT_NO_IDX_ARTIFACT_V1,
    headerHash: prepared.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    badTx: inclusion(prepared.badTxInclusion),
    producingTx: inclusion(prepared.producingTxInclusion),
    referenceInputs: prepared.referenceInputsPreimage.map((input) => ({
      tx_id: input.txId,
      output_index: input.index.toString(),
    })),
    badReferenceInputIndex: prepared.badReferenceInputIndex,
    outputsPreimageCbor: prepared.outputsPreimageCbor,
    badReferenceInputOutputIndex:
      prepared.badReferenceInput.output_index.toString(),
  }) as ProductionReferenceInputNoIdxArtifactV1;
  admitProductionReferenceInputNoIdxArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type ReferenceInputNoIdxWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"referenceInputNoIdx">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: ReferenceInputNoIdxWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"referenceInputNoIdx">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = exactJournalRecordV1(
    action.input,
    Object.keys(action.input),
    "reference-input-no-idx workflow action",
  );
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "referenceInputNoIdx" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("reference-input-no-idx workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`reference-input-no-idx action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfigV1,
  planned: FaultProofFieldOpeningPlanV1,
) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("reference-input-no-idx publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("reference-input-no-idx certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const captureRemoval = async (
  config: BoundConfigV1,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransactionV1(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "referenceInputNoIdx",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "reference-input-no-idx removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease === undefined ? {} : { mutationLease }),
  });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"referenceInputNoIdx"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "referenceInputNoIdx",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionReferenceInputNoIdxArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionReferenceInputNoIdxArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "reference-input-no-idx artifact changed manifest-bound header",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: "referenceInputNoIdx",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01" || input.stage === "step_03") {
      const stepIndex = input.stage === "step_01" ? 0 : 2;
      const inclusion =
        input.stage === "step_01"
          ? admitted.badInclusion
          : admitted.producingInclusion;
      const proofCbor =
        input.stage === "step_01"
          ? admitted.artifact.badTx.txMembershipProofCbor
          : admitted.artifact.producingTx.txMembershipProofCbor;
      const chunks = await resolveDirectFirstProofChunksV1({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: inclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (input.stage === "step_01") {
              await submitReferenceInputNoIdxStep01(common);
            } else {
              await submitReferenceInputNoIdxStep03(common);
            }
          },
        ),
      });
    }
    if (input.stage === "step_02" || input.stage === "step_04") {
      const stepIndex = input.stage === "step_02" ? 1 : 3;
      const plan =
        input.stage === "step_02"
          ? admitted.referenceInputFieldPlan
          : admitted.outputFieldPlan;
      const carriage = await resolveField(config, plan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (input.stage === "step_02") {
              await submitReferenceInputNoIdxStep02({
                ...common,
                referenceInputsPreimage: admitted.referenceInputs,
                nativeTxCompactCbor:
                  admitted.artifact.badTx.nativeTxCompactCbor,
              });
            } else {
              await submitReferenceInputNoIdxStep04({
                ...common,
                outputsPreimage: admitted.outputs,
                nativeTxCompactCbor:
                  admitted.artifact.producingTx.nativeTxCompactCbor,
                witnessReferenceScripts: config.referenceScripts.witnesses,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `reference-input-no-idx action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundReferenceInputNoIdxWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: ReferenceInputNoIdxWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundReferenceInputNoIdxWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"referenceInputNoIdx">;
  l1: FraudProofFamilyL1ObservationPortV1<"referenceInputNoIdx">;
  transactions: ProductionLinearFamilyTransactionPortV1<"referenceInputNoIdx">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundReferenceInputNoIdxWorkflowV1 = async (
  config: ManifestBoundReferenceInputNoIdxWorkflowConfigV1,
): Promise<ManifestBoundReferenceInputNoIdxWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "referenceInputNoIdx",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      ReferenceInputNoIdxStep02Datum,
      ReferenceInputNoIdxStep03Datum,
      ReferenceInputNoIdxStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "reference-input-no-idx manifest omitted field-preimage certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const contractNames = [
    "fraudProofReferenceInputNoIdx",
    "fraudProofReferenceInputNoIdxStep02",
    "fraudProofReferenceInputNoIdxStep03",
    "fraudProofReferenceInputNoIdxStep04",
  ] as const;
  const step = (index: 0 | 1 | 2 | 3): UTxO =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: contractNames[index],
      utxo: config.referenceScripts.steps[index],
    });
  const steps = Object.freeze([step(0), step(1), step(2), step(3)] as const);
  const references: ReferenceInputNoIdxWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps,
      witnesses: Object.freeze({
        computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "computationThreadMint",
          utxo: config.referenceScripts.witnesses.computationThreadMint,
        }),
        fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "fraudProofMint",
          utxo: config.referenceScripts.witnesses.fraudProofMint,
        }),
        phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "phasMembershipWithdraw",
          utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
        }),
        chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "chunkedVerifyWithdraw",
          utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
        }),
      }),
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined) {
    throw new Error(
      "reference-input-no-idx raw-L1 authority omitted publications",
    );
  }
  const transactions = createTransactionPort({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    referenceScripts: references,
    certificate,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "referenceInputNoIdx",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "referenceInputNoIdx",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = actionInput(action);
      const admitted = admitProductionReferenceInputNoIdxArtifactV1(
        artifact,
        config.signer.paymentKeyHash,
      );
      const planned =
        input.stage === "step_02"
          ? admitted.referenceInputFieldPlan
          : input.stage === "step_04"
            ? admitted.outputFieldPlan
            : null;
      if (planned === null) return null;
      return {
        planned,
        compactCbor:
          input.stage === "step_02"
            ? admitted.artifact.badTx.nativeTxCompactCbor
            : admitted.artifact.producingTx.nativeTxCompactCbor,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        },
      } satisfies ProductionFieldCarriageRequirementV1;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "referenceInputNoIdx",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "referenceInputNoIdx",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = actionInput(action);
      const admitted = admitProductionReferenceInputNoIdxArtifactV1(
        artifact,
        config.signer.paymentKeyHash,
      );
      return input.stage === "step_01"
        ? admitted.artifact.badTx.txMembershipProofCbor
        : input.stage === "step_03"
          ? admitted.artifact.producingTx.txMembershipProofCbor
          : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "referenceInputNoIdx",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const runOrResumeManifestBoundReferenceInputNoIdxWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundReferenceInputNoIdxWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["referenceInputNoIdx"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
