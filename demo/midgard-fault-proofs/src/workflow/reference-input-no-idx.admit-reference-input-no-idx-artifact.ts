import {
  encodeMidgardTxInputCanonical,
  encodeMidgardTxOutputCanonical,
  MIDGARD_FIELD_INDEX,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
} from "@al-ft/midgard-sdk";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import { prepareReferenceInputNoIdxFromCanonicalEvidence } from "../prepare-reference-input-no-idx.js";
import {
  parseSubmitReferenceInputNoIdxReferenceInputsPreimage,
  type SubmitReferenceInputNoIdxReferenceInputsPreimage,
} from "../submit-reference-input-no-idx-step-02.js";
import {
  parseSubmitReferenceInputNoIdxOutputsPreimage,
  type SubmitReferenceInputNoIdxOutputsPreimage,
} from "../submit-reference-input-no-idx-step-04.js";
import type { CanonicalBlockClassification } from "./classification.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
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
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: bad.artifact.nativeTxId,
    nativeTxCompactCbor: bad.artifact.nativeTxCompactCbor,
    itemCbors: referenceInputList.inputs.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "reference-input-no-idx reference inputs",
  });
  const outputFieldPlan = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
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

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type ReferenceInputNoIdxWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "referenceInputNoIdx",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "referenceInputNoIdx",
  (typeof WITNESS_ROLES)[number],
  true
>;
