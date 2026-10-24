import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  encodeMidgardTxInputCanonical,
  MIDGARD_FIELD_INDEX,
  ROOT_DOMAINS,
  WithdrawalSourceMembershipProof,
  type WithdrawalSourceMembershipProof as WithdrawalSourceMembershipProofV1,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import {
  prepareWithdrawnReferenceInput,
  verifyWithdrawnReferenceInputMembership,
} from "../withdrawn-reference-input/prepare-withdrawn-reference-input.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import {
  admitNativeInclusionArtifact,
  admitTxInputList,
  canonicalHex,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  type NativeInclusionArtifact,
  NATURAL_DECIMAL,
  safeNaturalNumber,
} from "./native-index-artifact.js";

export const WITHDRAWN_REFERENCE_INPUT_ARTIFACT =
  "midgard-production-withdrawn-reference-input-artifact-v1" as const;

type InputJson = Readonly<{ tx_id: string; output_index: string }>;

export type WithdrawnReferenceInputArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof WITHDRAWN_REFERENCE_INPUT_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    referenceInputs: readonly InputJson[];
    badReferenceInputIndex: number;
    withdrawalIndex: number;
    withdrawalMembershipCbor: string;
  }>;

type AdmittedArtifact = Readonly<{
  artifact: WithdrawnReferenceInputArtifact;
  inclusion: ReturnType<typeof admitNativeInclusionArtifact>["inclusion"];
  referenceInputs: ReturnType<typeof admitTxInputList>["inputs"];
  withdrawalMembership: WithdrawalSourceMembershipProofV1;
  referencePlan: FaultProofFieldOpeningPlan;
}>;

const verifyMembership = async (
  membership: WithdrawalSourceMembershipProofV1,
): Promise<void> => {
  if (
    JSON.stringify(membership.domain) !==
      JSON.stringify(ROOT_DOMAINS.withdrawals) ||
    membership.count <= 0n ||
    !HEX_32.test(membership.root) ||
    !HEX_32.test(membership.phas_root)
  ) {
    throw new Error("withdrawn-reference-input membership is malformed");
  }
  const counted = await Effect.runPromise(
    commitCountedRootProgram({
      domain: membership.domain,
      phasRoot: membership.phas_root,
      count: membership.count,
    }),
  );
  if (counted !== membership.root) {
    throw new Error(
      "withdrawn-reference-input membership count does not open its root",
    );
  }
  verifyWithdrawnReferenceInputMembership(membership);
};

export const admitWithdrawnReferenceInputArtifact = async (
  value: unknown,
  carriageOwner = "00".repeat(28),
): Promise<AdmittedArtifact> => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("withdrawn-reference-input carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "referenceInputs",
      "badReferenceInputIndex",
      "withdrawalIndex",
      "withdrawalMembershipCbor",
    ],
    "withdrawn-reference-input artifact",
  );
  if (
    parsed.schemaVersion !== WITHDRAWN_REFERENCE_INPUT_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("withdrawn-reference-input artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "withdrawn-reference-input header",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "withdrawn-reference-input position",
  );
  const badReferenceInputIndex = safeNaturalNumber(
    parsed.badReferenceInputIndex,
    "withdrawn-reference-input bad reference-input index",
  );
  const withdrawalIndex = safeNaturalNumber(
    parsed.withdrawalIndex,
    "withdrawn-reference-input withdrawal index",
  );
  const tx = admitNativeInclusionArtifact(
    parsed.tx,
    "withdrawn-reference-input transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("withdrawn-reference-input transaction is not accepted");
  }
  const referenceInputs = admitTxInputList(
    parsed.referenceInputs,
    "withdrawn-reference-input reference inputs",
  );
  const selectedInput = referenceInputs.inputs[badReferenceInputIndex];
  if (selectedInput === undefined) {
    throw new Error("withdrawn-reference-input selection is out of range");
  }
  const withdrawalMembershipCbor = canonicalHex(
    parsed.withdrawalMembershipCbor,
    EVEN_HEX,
    "withdrawn-reference-input membership",
  );
  let withdrawalMembership: WithdrawalSourceMembershipProofV1;
  try {
    withdrawalMembership = Data.from(
      withdrawalMembershipCbor,
      WithdrawalSourceMembershipProof,
    );
  } catch {
    throw new Error("withdrawn-reference-input membership is malformed");
  }
  if (
    Data.to(withdrawalMembership, WithdrawalSourceMembershipProof) !==
    withdrawalMembershipCbor
  ) {
    throw new Error("withdrawn-reference-input membership is non-canonical");
  }
  await verifyMembership(withdrawalMembership);
  const withdrawn = withdrawalMembership.value.body.l2_outref;
  if (
    withdrawalMembership.value.validity !== "WithdrawalIsValid" ||
    selectedInput.tx_id !== withdrawn.transactionId ||
    selectedInput.output_index !== withdrawn.outputIndex
  ) {
    throw new Error(
      "withdrawn-reference-input artifact does not prove its violation",
    );
  }
  const expectedDetection = `${WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID}:${position.toString()}:${badReferenceInputIndex.toString()}:${withdrawalIndex.toString()}:${tx.artifact.nativeTxId}:${committedWithdrawalKeyBytes(withdrawalMembership.key)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("withdrawn-reference-input detection identity changed");
  }
  const referencePlan = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: tx.artifact.nativeTxId,
    nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
    itemCbors: referenceInputs.inputs.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "withdrawn-reference-input artifact reference inputs",
  });
  const artifact = Object.freeze({
    schemaVersion: WITHDRAWN_REFERENCE_INPUT_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    referenceInputs: referenceInputs.json,
    badReferenceInputIndex,
    withdrawalIndex,
    withdrawalMembershipCbor,
  }) satisfies WithdrawnReferenceInputArtifact;
  return Object.freeze({
    artifact,
    inclusion: tx.inclusion,
    referenceInputs: referenceInputs.inputs,
    withdrawalMembership,
    referencePlan,
  });
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const [
    violationId,
    positionValue,
    inputValue,
    withdrawalValue,
    txId,
    withdrawalKey,
    ...rest
  ] = classification.selected.detectionId.split(":");
  if (
    classification.category !== "withdrawnReferenceInput" ||
    classification.selected.violationId !==
      WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID ||
    violationId !== WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID ||
    rest.length !== 0 ||
    !NATURAL_DECIMAL.test(positionValue ?? "") ||
    !NATURAL_DECIMAL.test(inputValue ?? "") ||
    !NATURAL_DECIMAL.test(withdrawalValue ?? "") ||
    !HEX_32.test(txId ?? "") ||
    !EVEN_HEX.test(withdrawalKey ?? "")
  ) {
    throw new Error("withdrawn-reference-input classification is malformed");
  }
  const position = Number(positionValue);
  const badReferenceInputIndex = Number(inputValue);
  const withdrawalIndex = Number(withdrawalValue);
  if (
    !Number.isSafeInteger(position) ||
    !Number.isSafeInteger(badReferenceInputIndex) ||
    !Number.isSafeInteger(withdrawalIndex) ||
    classification.selected.position !== BigInt(positionValue!)
  ) {
    throw new Error("withdrawn-reference-input classification index is unsafe");
  }
  return Object.freeze({
    position,
    badReferenceInputIndex,
    withdrawalIndex,
    txId: txId!,
    withdrawalKey: withdrawalKey!,
  });
};

export const prepareWithdrawnReferenceInputArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<WithdrawnReferenceInputArtifact> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("withdrawn-reference-input classification changed header");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareWithdrawnReferenceInput({
    header: evidence.header,
    blockTxs: evidence.transactions,
    withdrawals: evidence.reconstruction.withdrawals.map(({ key, value }) => ({
      id: key,
      info: value,
    })),
    accusedTxId: selected.txId,
  });
  const selectedWithdrawal =
    evidence.reconstruction.withdrawals[selected.withdrawalIndex];
  if (
    selectedWithdrawal === undefined ||
    prepared.badReferenceInputIndex !== selected.badReferenceInputIndex ||
    committedWithdrawalKeyBytes(selectedWithdrawal.key) !==
      selected.withdrawalKey ||
    committedWithdrawalKeyBytes(prepared.withdrawal.id) !==
      selected.withdrawalKey
  ) {
    throw new Error("withdrawn-reference-input public evidence changed");
  }
  const artifact = normalizeJournalJson({
    schemaVersion: WITHDRAWN_REFERENCE_INPUT_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: prepared.txInclusion.nativeTxId,
      nativeTxCompactCbor: prepared.txInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: prepared.txInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: prepared.txInclusion.transactionsPhasRoot,
      txMembershipProofCbor: prepared.txInclusion.txMembershipProofCbor,
    },
    referenceInputs: prepared.referenceInputs.map((input) => ({
      tx_id: input.tx_id,
      output_index: input.output_index.toString(),
    })),
    badReferenceInputIndex: prepared.badReferenceInputIndex,
    withdrawalIndex: selected.withdrawalIndex,
    withdrawalMembershipCbor: Data.to(
      prepared.withdrawalMembership,
      WithdrawalSourceMembershipProof,
    ),
  }) as WithdrawnReferenceInputArtifact;
  await admitWithdrawnReferenceInputArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;
