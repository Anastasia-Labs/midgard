import {
  forcedVerdictSubject,
  InputSetUniquenessVerdictSubjectSchema,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { planFaultProofFieldOpening } from "../field-opening.js";
import { INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID } from "../input-set-uniqueness/replay.js";
import { INPUT_SET_UNIQUENESS_VIOLATION_ID } from "../input-set-uniqueness/scan.js";
import {
  bindForcedDuplicateInput,
  inputSetUnionIsStrictlyIncreasing,
} from "../input-set-uniqueness/wrongful-rejection.js";
import type { CanonicalBlockClassification } from "./classification.js";
import {
  admitAcceptedInputSetUniquenessArtifact,
  type AdmittedAcceptedArtifact,
  type AdmittedArtifact,
  type AdmittedForcedArtifact,
  INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
  type InputSetUniquenessForcedArtifact,
  InputSetUniquenessForcedSourceSchema,
  parseItemList,
} from "./input-set-uniqueness.admit-accepted-input-set-uniqueness-artifact.js";
import {
  canonicalHex,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  NATURAL_DECIMAL,
  safeNaturalNumber,
} from "./native-index-artifact.js";

export const admitInputSetUniquenessForcedArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedForcedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "forcedIndex",
      "transactionId",
      "subjectCbor",
      "nativeTxCompactCbor",
      "spendInputItemCbors",
      "referenceInputItemCbors",
      "forcedSourceCbor",
    ],
    "input-set-uniqueness forced artifact",
  );
  if (
    parsed.schemaVersion !== INPUT_SET_UNIQUENESS_FORCED_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness forced artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "input-set-uniqueness forced header hash",
  );
  const transactionId = canonicalHex(
    parsed.transactionId,
    HEX_32,
    "input-set-uniqueness forced transaction id",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "input-set-uniqueness forced position",
  );
  const forcedIndex = safeNaturalNumber(
    parsed.forcedIndex,
    "input-set-uniqueness forced index",
  );
  if (position !== forcedIndex) {
    throw new Error("input-set-uniqueness forced position changed");
  }
  const subjectCbor = canonicalHex(
    parsed.subjectCbor,
    EVEN_HEX,
    "input-set-uniqueness forced subject",
  );
  const nativeTxCompactCbor = canonicalHex(
    parsed.nativeTxCompactCbor,
    EVEN_HEX,
    "input-set-uniqueness forced compact transaction",
  );
  const forcedSourceCbor = canonicalHex(
    parsed.forcedSourceCbor,
    EVEN_HEX,
    "input-set-uniqueness forced source",
  );
  const subject = Data.from(
    subjectCbor,
    InputSetUniquenessVerdictSubjectSchema as never,
  );
  const bound = bindForcedDuplicateInput(subject as never);
  const forcedSource = Data.from(
    forcedSourceCbor,
    InputSetUniquenessForcedSourceSchema as never,
  ) as Data.Static<typeof InputSetUniquenessForcedSourceSchema>;
  const leaf = forcedSource.membership.value;
  if (
    leaf.tx_id !== transactionId ||
    leaf.submitted_source.compact_cbor !== nativeTxCompactCbor ||
    leaf.verdict === "ForcedTxValid" ||
    Data.to(
      forcedVerdictSubject({
        transactionId: leaf.tx_id,
        sourceKey: forcedSource.membership.key,
        rejectionReason: leaf.verdict.ForcedTxInvalid.reason,
      }) as never,
      InputSetUniquenessVerdictSubjectSchema as never,
    ) !== subjectCbor
  ) {
    throw new Error("input-set-uniqueness forced subject/source changed");
  }
  const spends = parseItemList(
    parsed.spendInputItemCbors,
    "input-set-uniqueness forced spend inputs",
  );
  const references = parseItemList(
    parsed.referenceInputItemCbors,
    "input-set-uniqueness forced reference inputs",
  );
  if (
    !inputSetUnionIsStrictlyIncreasing({
      spendInputItemCbors: spends,
      referenceInputItemCbors: references,
    })
  ) {
    throw new Error("input-set-uniqueness forced union is not unique");
  }
  const count = (field: bigint) =>
    field === 0n
      ? BigInt(spends.length)
      : field === 1n
        ? BigInt(references.length)
        : -1n;
  if (
    bound.first_item_index < 0n ||
    bound.second_item_index < 0n ||
    bound.first_item_index >= count(bound.first_field_index) ||
    bound.second_item_index >= count(bound.second_field_index) ||
    bound.first_field_index > bound.second_field_index ||
    (bound.first_field_index === bound.second_field_index &&
      bound.first_item_index >= bound.second_item_index)
  ) {
    throw new Error("input-set-uniqueness forced coordinates changed");
  }
  const expectedDetection = `${INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transactionId}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness forced detection identity changed");
  }
  const makePlan = (fieldIndex: number, items: readonly string[]) =>
    planFaultProofFieldOpening({
      anchorSourceKind: 1n,
      fieldIndex,
      anchorTxId: transactionId,
      nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: carriageOwner,
      label: "input-set-uniqueness forced artifact",
    });
  const artifact = Object.freeze({
    schemaVersion: INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    forcedIndex,
    transactionId,
    subjectCbor,
    nativeTxCompactCbor,
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    forcedSourceCbor,
  }) satisfies InputSetUniquenessForcedArtifact;
  return Object.freeze({
    sourceKind: "forced" as const,
    artifact,
    forcedSource,
    spendPlan: makePlan(MIDGARD_FIELD_INDEX.spendInputs, spends),
    referencePlan: makePlan(MIDGARD_FIELD_INDEX.referenceInputs, references),
  });
};

export const admitAnyInputSetUniquenessArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
  const schemaVersion =
    typeof value === "object" && value !== null && "schemaVersion" in value
      ? (value as { readonly schemaVersion?: unknown }).schemaVersion
      : undefined;
  return schemaVersion === INPUT_SET_UNIQUENESS_FORCED_ARTIFACT
    ? admitInputSetUniquenessForcedArtifact(value, carriageOwner)
    : admitAcceptedInputSetUniquenessArtifact(value, carriageOwner);
};

/** Backwards-compatible accepted-invalid artifact admission. */
export const admitInputSetUniquenessArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedAcceptedArtifact =>
  admitAcceptedInputSetUniquenessArtifact(value, carriageOwner);

export const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  if (
    classification.category !== "inputSetUniqueness" ||
    classification.selected.violationId !== INPUT_SET_UNIQUENESS_VIOLATION_ID ||
    fields.length !== 6 ||
    fields[0] !== INPUT_SET_UNIQUENESS_VIOLATION_ID ||
    !NATURAL_DECIMAL.test(fields[1] ?? "") ||
    !HEX_32.test(fields[2] ?? "") ||
    ![
      "duplicateSpendInputs",
      "duplicateReferenceInputs",
      "spendReferenceOverlap",
    ].includes(fields[3] ?? "") ||
    !NATURAL_DECIMAL.test(fields[4] ?? "") ||
    !NATURAL_DECIMAL.test(fields[5] ?? "") ||
    !Number.isSafeInteger(position) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("input-set-uniqueness classification is malformed");
  }
  return Object.freeze({ position, txId: fields[2]! });
};
