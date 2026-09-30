import {
  encodeMidgardTxInputCanonical,
  encodeMidgardTxOutputCanonical,
  INPUT_NO_IDX_VIOLATION_ID,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { prepareInputNoIdxFromCanonicalEvidence } from "../prepare-input-no-idx.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FieldPreimageCertificateBinding,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.js";
import {
  type AdmittedArtifact,
  exact,
  hex,
  HEX_28,
  HEX_32,
  INPUT_NO_IDX_ARTIFACT,
  type InputNoIdxArtifact,
  NATURAL,
  naturalString,
  parseInclusion,
  parseInputs,
  parseOutputCbors,
  record,
  safeNatural,
} from "./input-no-idx.parse-inclusion.js";
import { normalizeJournalJson } from "./journal.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";

export const admitInputNoIdxArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
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
    parsed.schemaVersion !== INPUT_NO_IDX_ARTIFACT ||
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
  const expectedDetection = `${INPUT_NO_IDX_VIOLATION_ID}:${position.toString()}:${badInputsIndex.toString()}:${bad.artifact.nativeTxId}:${producing.artifact.nativeTxId}:${badInputOutputIndex}:${outputs.parsed.outputsPreimage.length.toString()}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-no-idx artifact detection identity changed");
  }
  const inputFieldPlan = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: bad.artifact.nativeTxId,
    nativeTxCompactCbor: bad.artifact.nativeTxCompactCbor,
    itemCbors: inputs.parsed.inputsPreimage.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "input-no-idx artifact spend inputs",
  });
  const outputFieldPlan = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: producing.artifact.nativeTxId,
    nativeTxCompactCbor: producing.artifact.nativeTxCompactCbor,
    itemCbors: outputs.parsed.outputsPreimage.map(
      encodeMidgardTxOutputCanonical,
    ),
    owner: carriageOwner,
    label: "input-no-idx artifact outputs",
  });
  const artifact = Object.freeze({
    schemaVersion: INPUT_NO_IDX_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    badTx: bad.artifact,
    producingTx: producing.artifact,
    inputs: inputs.json,
    badInputsIndex,
    outputsPreimageCbor: outputs.json,
    badInputOutputIndex,
  }) satisfies InputNoIdxArtifact;
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
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  const badInputsIndex = Number(fields[2]);
  if (
    classification.category !== "nonExistentInputNoIndex" ||
    classification.selected.violationId !== INPUT_NO_IDX_VIOLATION_ID ||
    fields.length !== 7 ||
    fields[0] !== INPUT_NO_IDX_VIOLATION_ID ||
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

export const prepareInputNoIdxArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareInputNoIdxFromCanonicalEvidence
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<InputNoIdxArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("input-no-idx classification differs from evidence");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareInputNoIdxFromCanonicalEvidence({
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
  const artifact = normalizeJournalJson({
    schemaVersion: INPUT_NO_IDX_ARTIFACT,
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
  }) as InputNoIdxArtifact;
  admitInputNoIdxArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type InputNoIdxWorkflowReferenceScripts = LinearFamilyReferenceScripts<
  "nonExistentInputNoIndex",
  (typeof WITNESS_ROLES)[number],
  true
>;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "nonExistentInputNoIndex",
  (typeof WITNESS_ROLES)[number],
  true
>;

export type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"nonExistentInputNoIndex">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: InputNoIdxWorkflowReferenceScripts;
  certificate: FieldPreimageCertificateBinding;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export const actionInput = (
  action: FraudProofWorkflowAction,
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

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`input-no-idx workflow action omitted ${field}`);
  }
  return value;
};

export const resolveField = async (
  config: BoundConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("input-no-idx field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
