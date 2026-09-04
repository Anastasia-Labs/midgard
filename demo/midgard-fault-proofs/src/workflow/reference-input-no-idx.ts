import {
  encodeMidgardTxInputCanonical,
  encodeMidgardTxOutputCanonical,
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep03Datum,
  ReferenceInputNoIdxStep04Datum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { prepareReferenceInputNoIdxFromCanonicalEvidence } from "../prepare-reference-input-no-idx.js";
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  admitNativeInclusionArtifact,
  admitOutputCborList,
  admitTxInputList,
  canonicalHex,
  canonicalNaturalString,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  type NativeInclusionArtifact,
  NATURAL_DECIMAL,
  safeNaturalNumber,
} from "./native-index-artifact.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const REFERENCE_INPUT_NO_IDX_ARTIFACT =
  "midgard-production-reference-input-no-idx-artifact-v1" as const;

export type ReferenceInputNoIdxArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof REFERENCE_INPUT_NO_IDX_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: NativeInclusionArtifact;
    producingTx: NativeInclusionArtifact;
    referenceInputs: readonly Readonly<{
      tx_id: string;
      output_index: string;
    }>[];
    badReferenceInputIndex: number;
    outputsPreimageCbor: readonly string[];
    badReferenceInputOutputIndex: string;
  }>;

type AdmittedArtifact = Readonly<{
  artifact: ReferenceInputNoIdxArtifact;
  badInclusion: ReturnType<typeof admitNativeInclusionArtifact>["inclusion"];
  producingInclusion: ReturnType<
    typeof admitNativeInclusionArtifact
  >["inclusion"];
  referenceInputs: SubmitReferenceInputNoIdxReferenceInputsPreimage;
  outputs: SubmitReferenceInputNoIdxOutputsPreimage;
  referenceInputFieldPlan: FaultProofFieldOpeningPlan;
  outputFieldPlan: FaultProofFieldOpeningPlan;
}>;

export const admitReferenceInputNoIdxArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("reference-input-no-idx carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
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
    parsed.schemaVersion !== REFERENCE_INPUT_NO_IDX_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("reference-input-no-idx artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "reference-input-no-idx header hash",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "reference-input-no-idx position",
  );
  const badReferenceInputIndex = safeNaturalNumber(
    parsed.badReferenceInputIndex,
    "reference-input-no-idx selected reference input",
  );
  const badReferenceInputOutputIndex = canonicalNaturalString(
    parsed.badReferenceInputOutputIndex,
    "reference-input-no-idx output index",
  );
  const bad = admitNativeInclusionArtifact(
    parsed.badTx,
    "reference-input-no-idx bad transaction",
  );
  const producing = admitNativeInclusionArtifact(
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
  const referenceInputList = admitTxInputList(
    parsed.referenceInputs,
    "reference-input-no-idx reference inputs",
  );
  const outputList = admitOutputCborList(
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
  const expectedDetection = `${REFERENCE_INPUT_NO_IDX_VIOLATION_ID}:${position.toString()}:${badReferenceInputIndex.toString()}:${bad.artifact.nativeTxId}:${producing.artifact.nativeTxId}:${badReferenceInputOutputIndex}:${outputList.outputs.length.toString()}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error(
      "reference-input-no-idx artifact detection identity changed",
    );
  }
  const referenceInputFieldPlan = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: bad.artifact.nativeTxId,
    nativeTxCompactCbor: bad.artifact.nativeTxCompactCbor,
    itemCbors: referenceInputList.inputs.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "reference-input-no-idx reference inputs",
  });
  const outputFieldPlan = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: producing.artifact.nativeTxId,
    nativeTxCompactCbor: producing.artifact.nativeTxCompactCbor,
    itemCbors: outputList.outputs.map(encodeMidgardTxOutputCanonical),
    owner: carriageOwner,
    label: "reference-input-no-idx outputs",
  });
  const artifact = Object.freeze({
    schemaVersion: REFERENCE_INPUT_NO_IDX_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    badTx: bad.artifact,
    producingTx: producing.artifact,
    referenceInputs: referenceInputList.json,
    badReferenceInputIndex,
    outputsPreimageCbor: outputList.json,
    badReferenceInputOutputIndex,
  }) satisfies ReferenceInputNoIdxArtifact;
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
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  const badReferenceInputIndex = Number(fields[2]);
  if (
    classification.category !== "referenceInputNoIdx" ||
    classification.selected.violationId !==
      REFERENCE_INPUT_NO_IDX_VIOLATION_ID ||
    fields.length !== 7 ||
    fields[0] !== REFERENCE_INPUT_NO_IDX_VIOLATION_ID ||
    !NATURAL_DECIMAL.test(fields[1] ?? "") ||
    !NATURAL_DECIMAL.test(fields[2] ?? "") ||
    !HEX_32.test(fields[3] ?? "") ||
    !HEX_32.test(fields[4] ?? "") ||
    !NATURAL_DECIMAL.test(fields[5] ?? "") ||
    !NATURAL_DECIMAL.test(fields[6] ?? "") ||
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

export const prepareReferenceInputNoIdxArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareReferenceInputNoIdxFromCanonicalEvidence
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ReferenceInputNoIdxArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "reference-input-no-idx classification differs from evidence",
    );
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareReferenceInputNoIdxFromCanonicalEvidence({
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
  ): NativeInclusionArtifact => ({
    nativeTxId: item.nativeTxId,
    nativeTxCompactCbor: item.nativeTxCompactCbor,
    l2TransactionSourceCbor: item.l2TransactionSourceCbor,
    transactionsPhasRoot: item.transactionsPhasRoot,
    txMembershipProofCbor: item.txMembershipProofCbor,
  });
  const artifact = normalizeJournalJson({
    schemaVersion: REFERENCE_INPUT_NO_IDX_ARTIFACT,
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
  }) as ReferenceInputNoIdxArtifact;
  admitReferenceInputNoIdxArtifact(artifact);
  return Object.freeze(artifact);
};

export type ReferenceInputNoIdxWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"referenceInputNoIdx">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: ReferenceInputNoIdxWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"referenceInputNoIdx">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = exactJournalRecord(
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
  config: BoundConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("reference-input-no-idx publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
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
  const transaction = await captureLocallyEvaluatedTransaction(
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
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
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
  config: BoundConfig,
): LinearFamilyTransactionPort<"referenceInputNoIdx"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "referenceInputNoIdx",
  prepare: async ({ evidence, classification }) =>
    await prepareReferenceInputNoIdxArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitReferenceInputNoIdxArtifact(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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

export type ManifestBoundReferenceInputNoIdxWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: ReferenceInputNoIdxWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundReferenceInputNoIdxWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"referenceInputNoIdx">;
  l1: FraudProofFamilyL1ObservationPort<"referenceInputNoIdx">;
  transactions: LinearFamilyTransactionPort<"referenceInputNoIdx">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundReferenceInputNoIdxWorkflow = async (
  config: ManifestBoundReferenceInputNoIdxWorkflowConfig,
): Promise<ManifestBoundReferenceInputNoIdxWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: contractNames[index],
      utxo: config.referenceScripts.steps[index],
    });
  const steps = Object.freeze([step(0), step(1), step(2), step(3)] as const);
  const references: ReferenceInputNoIdxWorkflowReferenceScripts = Object.freeze(
    {
      steps,
      witnesses: Object.freeze({
        computationThreadMint: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "computationThreadMint",
          utxo: config.referenceScripts.witnesses.computationThreadMint,
        }),
        fraudProofMint: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fraudProofMint",
          utxo: config.referenceScripts.witnesses.fraudProofMint,
        }),
        phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "phasMembershipWithdraw",
          utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
        }),
        chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "chunkedVerifyWithdraw",
          utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
        }),
      }),
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    },
  );
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
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
  let adapter = createLinearFamilyWorkflowAdapter({
    category: "referenceInputNoIdx",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "referenceInputNoIdx",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = actionInput(action);
      const admitted = admitReferenceInputNoIdxArtifact(
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
      } satisfies FieldCarriageRequirement;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "referenceInputNoIdx",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "referenceInputNoIdx",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = actionInput(action);
      const admitted = admitReferenceInputNoIdxArtifact(
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
  adapter = withProofChunkPrerequisite({
    category: "referenceInputNoIdx",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundReferenceInputNoIdxWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundReferenceInputNoIdxWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["referenceInputNoIdx"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
