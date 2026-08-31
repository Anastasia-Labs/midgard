import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompactV1 } from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonicalV1,
  encodeMidgardTxOutputCanonicalV1,
  FraudProofComputationThreadStepDatum,
  INPUT_NO_IDX_VIOLATION_ID_V1,
  InputNoIdxStep02Datum,
  InputNoIdxStep03Datum,
  InputNoIdxStep04Datum,
  MIDGARD_FIELD_INDEX_V1,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import { prepareInputNoIdxFromCanonicalEvidenceV1 } from "../prepare-input-no-idx.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitInputNoIdxStep01 } from "../submit-input-no-idx-step-01.js";
import {
  parseSubmitInputNoIdxInputsPreimage,
  type SubmitInputNoIdxInputsPreimage,
  submitInputNoIdxStep02,
} from "../submit-input-no-idx-step-02.js";
import { submitInputNoIdxStep03 } from "../submit-input-no-idx-step-03.js";
import {
  parseSubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxOutputsPreimage,
  submitInputNoIdxStep04,
} from "../submit-input-no-idx-step-04.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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

export const PRODUCTION_INPUT_NO_IDX_ARTIFACT_V1 =
  "midgard-production-input-no-idx-artifact-v1" as const;

type InclusionJsonV1 = Readonly<{
  nativeTxId: string;
  nativeTxCompactCbor: string;
  l2TransactionSourceCbor: string;
  transactionsPhasRoot: string;
  txMembershipProofCbor: string;
}>;

export type ProductionInputNoIdxArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_INPUT_NO_IDX_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: InclusionJsonV1;
    producingTx: InclusionJsonV1;
    inputs: readonly Readonly<{ tx_id: string; output_index: string }>[];
    badInputsIndex: number;
    outputsPreimageCbor: readonly string[];
    badInputOutputIndex: string;
  }>;

type AdmittedArtifactV1 = Readonly<{
  artifact: ProductionInputNoIdxArtifactV1;
  badInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  producingInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  inputs: SubmitInputNoIdxInputsPreimage;
  outputs: SubmitInputNoIdxOutputsPreimage;
  inputFieldPlan: FaultProofFieldOpeningPlanV1;
  outputFieldPlan: FaultProofFieldOpeningPlanV1;
}>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const hex = (value: unknown, pattern: RegExp, label: string): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const naturalString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

const safeNatural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const proofSteps = (
  proof: ReturnType<typeof parseSubmitStep01TxInclusion>["txMembershipProof"],
) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const parseInclusion = (
  value: unknown,
  label: string,
): Readonly<{
  artifact: InclusionJsonV1;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}> => {
  const parsed = exact(
    value,
    [
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
    ],
    label,
  );
  const artifact = Object.freeze({
    nativeTxId: hex(parsed.nativeTxId, HEX_32, `${label} tx id`),
    nativeTxCompactCbor: hex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      `${label} compact tx`,
    ),
    l2TransactionSourceCbor: hex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      `${label} source`,
    ),
    transactionsPhasRoot: hex(
      parsed.transactionsPhasRoot,
      HEX_32,
      `${label} root`,
    ),
    txMembershipProofCbor: hex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      `${label} proof`,
    ),
  });
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompactV1(
        Buffer.from(artifact.nativeTxCompactCbor, "hex"),
      ),
    ),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProofCbor: artifact.txMembershipProofCbor,
  });
  let openedRoot: Buffer | null;
  try {
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    throw new Error(`${label} membership proof cannot be replayed`);
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error(`${label} membership proof does not open its PHAS root`);
  }
  return Object.freeze({ artifact, inclusion });
};

const parseInputs = (
  value: unknown,
  badInputsIndex: number,
): Readonly<{
  json: readonly Readonly<{ tx_id: string; output_index: string }>[];
  parsed: SubmitInputNoIdxInputsPreimage;
}> => {
  if (!Array.isArray(value)) {
    throw new Error("input-no-idx artifact inputs must be an array");
  }
  const json = Object.freeze(
    value.map((entry, index) => {
      const parsed = exact(
        entry,
        ["tx_id", "output_index"],
        `input-no-idx artifact input ${index.toString()}`,
      );
      return Object.freeze({
        tx_id: hex(parsed.tx_id, HEX_32, "input transaction id"),
        output_index: naturalString(parsed.output_index, "input output index"),
      });
    }),
  );
  return Object.freeze({
    json,
    parsed: parseSubmitInputNoIdxInputsPreimage({
      inputsPreimage: json,
      badInputsIndex,
    }),
  });
};

const parseOutputCbors = (
  value: unknown,
): Readonly<{
  json: readonly string[];
  parsed: SubmitInputNoIdxOutputsPreimage;
}> => {
  if (!Array.isArray(value)) {
    throw new Error("input-no-idx artifact outputs must be an array");
  }
  const json = Object.freeze(
    value.map((item, index) =>
      hex(item, EVEN_HEX, `input-no-idx output ${index.toString()}`),
    ),
  );
  return Object.freeze({
    json,
    parsed: parseSubmitInputNoIdxOutputsPreimage({
      outputsPreimageCbor: json,
    }),
  });
};

export const admitProductionInputNoIdxArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifactV1 => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("input-no-idx carriage owner is malformed");
  }
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "producingTx",
      "inputs",
      "badInputsIndex",
      "outputsPreimageCbor",
      "badInputOutputIndex",
    ],
    "input-no-idx artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_INPUT_NO_IDX_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("input-no-idx artifact identity changed");
  }
  const headerHash = hex(parsed.headerHash, HEX_28, "artifact header hash");
  const position = safeNatural(parsed.position, "artifact position");
  const badInputsIndex = safeNatural(
    parsed.badInputsIndex,
    "artifact bad input index",
  );
  const badInputOutputIndex = naturalString(
    parsed.badInputOutputIndex,
    "artifact bad input output index",
  );
  const bad = parseInclusion(parsed.badTx, "input-no-idx bad transaction");
  const producing = parseInclusion(
    parsed.producingTx,
    "input-no-idx producing transaction",
  );
  if (
    bad.artifact.transactionsPhasRoot !==
    producing.artifact.transactionsPhasRoot
  ) {
    throw new Error(
      "input-no-idx inclusions do not share one transactions root",
    );
  }
  const inputs = parseInputs(parsed.inputs, badInputsIndex);
  const outputs = parseOutputCbors(parsed.outputsPreimageCbor);
  const badInput = inputs.parsed.inputsPreimage[badInputsIndex];
  if (
    badInput === undefined ||
    badInput.tx_id !== producing.artifact.nativeTxId ||
    badInput.output_index.toString() !== badInputOutputIndex ||
    badInput.output_index < BigInt(outputs.parsed.outputsPreimage.length)
  ) {
    throw new Error("input-no-idx artifact does not re-derive its violation");
  }
  const expectedDetection = `${INPUT_NO_IDX_VIOLATION_ID_V1}:${position.toString()}:${badInputsIndex.toString()}:${bad.artifact.nativeTxId}:${producing.artifact.nativeTxId}:${badInputOutputIndex}:${outputs.parsed.outputsPreimage.length.toString()}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-no-idx artifact detection identity changed");
  }
  const inputFieldPlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: bad.artifact.nativeTxId,
    nativeTxCompactCbor: bad.artifact.nativeTxCompactCbor,
    itemCbors: inputs.parsed.inputsPreimage.map(
      encodeMidgardTxInputCanonicalV1,
    ),
    owner: carriageOwner,
    label: "input-no-idx artifact spend inputs",
  });
  const outputFieldPlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: producing.artifact.nativeTxId,
    nativeTxCompactCbor: producing.artifact.nativeTxCompactCbor,
    itemCbors: outputs.parsed.outputsPreimage.map(
      encodeMidgardTxOutputCanonicalV1,
    ),
    owner: carriageOwner,
    label: "input-no-idx artifact outputs",
  });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_INPUT_NO_IDX_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    badTx: bad.artifact,
    producingTx: producing.artifact,
    inputs: inputs.json,
    badInputsIndex,
    outputsPreimageCbor: outputs.json,
    badInputOutputIndex,
  }) satisfies ProductionInputNoIdxArtifactV1;
  return Object.freeze({
    artifact,
    badInclusion: bad.inclusion,
    producingInclusion: producing.inclusion,
    inputs: inputs.parsed,
    outputs: outputs.parsed,
    inputFieldPlan,
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
  const badInputsIndex = Number(fields[2]);
  if (
    classification.category !== "nonExistentInputNoIndex" ||
    classification.selected.violationId !== INPUT_NO_IDX_VIOLATION_ID_V1 ||
    fields.length !== 7 ||
    fields[0] !== INPUT_NO_IDX_VIOLATION_ID_V1 ||
    !NATURAL.test(fields[1] ?? "") ||
    !NATURAL.test(fields[2] ?? "") ||
    !HEX_32.test(fields[3] ?? "") ||
    !HEX_32.test(fields[4] ?? "") ||
    !NATURAL.test(fields[5] ?? "") ||
    !NATURAL.test(fields[6] ?? "") ||
    !Number.isSafeInteger(position) ||
    !Number.isSafeInteger(badInputsIndex) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("input-no-idx classification identity is malformed");
  }
  return Object.freeze({
    position,
    badInputsIndex,
    badTxId: fields[3]!,
    producingTxId: fields[4]!,
    badInputOutputIndex: fields[5]!,
    producingTxOutputCount: fields[6]!,
  });
};

export const prepareProductionInputNoIdxArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareInputNoIdxFromCanonicalEvidenceV1
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionInputNoIdxArtifactV1> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("input-no-idx classification differs from evidence");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareInputNoIdxFromCanonicalEvidenceV1({
    evidence,
    badTxId: selected.badTxId,
    badInputsIndex: selected.badInputsIndex,
  });
  if (
    prepared.producingTxInclusion.nativeTxId !== selected.producingTxId ||
    prepared.step04.badInputOutputIndex !== selected.badInputOutputIndex ||
    prepared.outputsPreimage.length.toString() !==
      selected.producingTxOutputCount
  ) {
    throw new Error("input-no-idx prepared evidence changed classification");
  }
  const inclusionJson = (inclusion: typeof prepared.badTxInclusion) => ({
    nativeTxId: inclusion.nativeTxId,
    nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
    l2TransactionSourceCbor: inclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: inclusion.transactionsPhasRoot,
    txMembershipProofCbor: inclusion.txMembershipProofCbor,
  });
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_INPUT_NO_IDX_ARTIFACT_V1,
    headerHash: prepared.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    badTx: inclusionJson(prepared.badTxInclusion),
    producingTx: inclusionJson(prepared.producingTxInclusion),
    inputs: prepared.step02.inputsPreimage.map((input) => ({
      tx_id: input.tx_id,
      output_index: input.output_index.toString(),
    })),
    badInputsIndex: prepared.step02.badInputsIndex,
    outputsPreimageCbor: prepared.step04.outputsPreimageCbor,
    badInputOutputIndex: prepared.step04.badInputOutputIndex,
  }) as ProductionInputNoIdxArtifactV1;
  admitProductionInputNoIdxArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type InputNoIdxWorkflowReferenceScriptsV1 = Readonly<{
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
  network: FraudProofWorkflowDeploymentBindingV1<"nonExistentInputNoIndex">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: InputNoIdxWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"nonExistentInputNoIndex">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "input-no-idx workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "nonExistentInputNoIndex" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("input-no-idx workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`input-no-idx workflow action omitted ${field}`);
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
    throw new Error("input-no-idx field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("input-no-idx field certificate disappeared");
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
        fraudCategory: "nonExistentInputNoIndex",
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
              "input-no-idx removal changed its authenticated inputs",
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
): ProductionLinearFamilyTransactionPortV1<"nonExistentInputNoIndex"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "nonExistentInputNoIndex",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionInputNoIdxArtifactV1({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionInputNoIdxArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-no-idx artifact changed manifest-bound header");
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
              fraudCategory: "nonExistentInputNoIndex",
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
              await submitInputNoIdxStep01(common);
            } else {
              await submitInputNoIdxStep03(common);
            }
          },
        ),
      });
    }
    if (input.stage === "step_02" || input.stage === "step_04") {
      const stepIndex = input.stage === "step_02" ? 1 : 3;
      const plan =
        input.stage === "step_02"
          ? admitted.inputFieldPlan
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
              await submitInputNoIdxStep02({
                ...common,
                inputsPreimage: admitted.inputs,
                nativeTxCompactCbor:
                  admitted.artifact.badTx.nativeTxCompactCbor,
              });
            } else {
              await submitInputNoIdxStep04({
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
      `input-no-idx workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInputNoIdxWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InputNoIdxWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInputNoIdxWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"nonExistentInputNoIndex">;
  l1: FraudProofFamilyL1ObservationPortV1<"nonExistentInputNoIndex">;
  transactions: ProductionLinearFamilyTransactionPortV1<"nonExistentInputNoIndex">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundInputNoIdxWorkflowV1 = async (
  config: ManifestBoundInputNoIdxWorkflowConfigV1,
): Promise<ManifestBoundInputNoIdxWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "nonExistentInputNoIndex",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      InputNoIdxStep02Datum,
      InputNoIdxStep03Datum,
      InputNoIdxStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "input-no-idx manifest omitted field-preimage certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const contractNames = [
    "fraudProofNonExistentInputNoIndex",
    "fraudProofNonExistentInputNoIndexStep02",
    "fraudProofNonExistentInputNoIndexStep03",
    "fraudProofNonExistentInputNoIndexStep04",
  ] as const;
  const steps = contractNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
  const references: InputNoIdxWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze(steps),
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
    throw new Error("input-no-idx raw-L1 authority omitted publications");
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
    category: "nonExistentInputNoIndex",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "nonExistentInputNoIndex",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = record(action.input, "input-no-idx field prerequisite");
      const admitted = admitProductionInputNoIdxArtifactV1(
        artifact,
        config.signer.paymentKeyHash,
      );
      const planned =
        input.stage === "step_02"
          ? admitted.inputFieldPlan
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
    category: "nonExistentInputNoIndex",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "nonExistentInputNoIndex",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = record(action.input, "input-no-idx proof prerequisite");
      const admitted = admitProductionInputNoIdxArtifactV1(
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
    category: "nonExistentInputNoIndex",
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

export const runOrResumeManifestBoundInputNoIdxWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInputNoIdxWorkflowV1;
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
    replayer: INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["nonExistentInputNoIndex"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
