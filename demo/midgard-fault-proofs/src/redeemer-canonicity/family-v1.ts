import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormedV1,
  buildMidgardRedeemerItemProofTraceV1,
  decodeMidgardFieldPreimageV1,
  decodeMidgardRedeemerWitnessItemV1,
  midgardFieldCommitmentV1,
  MidgardRedeemerItemProofModesV1,
  type MidgardRedeemerItemProofTraceV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const REDEEMER_CANONICITY_CATEGORY_V1 = "redeemerCanonicity" as const;
export const REDEEMER_CANONICITY_CATEGORY_ID_V1 = "00000028" as const;
export const REDEEMER_CANONICITY_FIELD_INDEX_V1 = 8 as const;
export const REDEEMER_CANONICITY_VIOLATION_ID_V1 =
  "redeemer-malformed" as const;

const fail = (message: string): never => {
  throw new Error(`${REDEEMER_CANONICITY_CATEGORY_V1}: ${message}`);
};

const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

export type RedeemerCanonicityFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  redeemerIndex: number;
}>;

export const classifyRedeemerCanonicityFindingV1 = (
  finding: RedeemerCanonicityFindingV1,
): RedeemerCanonicityFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    return fail("verdict subject is not canonical");
  exactIndex(finding.redeemerIndex, "redeemer index");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    const reason = finding.subject.rejection_reason;
    if (
      typeof reason === "string" ||
      reason === null ||
      !("RedeemerMalformed" in reason) ||
      Number(reason.RedeemerMalformed.redeemer_index) !== finding.redeemerIndex
    )
      return fail("typed reason/redeemer coordinate changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity changed");
  }
  return Object.freeze({ ...finding });
};

const canonicalData = (bytes: Uint8Array): boolean => {
  try {
    assertMidgardPlutusDataWellFormedV1(bytes);
    return Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        Buffer.from(bytes).toString("hex"),
      ),
      "hex",
    ).equals(Buffer.from(bytes));
  } catch {
    return false;
  }
};

export type RedeemerCanonicityEvidenceV1 = RedeemerCanonicityFindingV1 &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    itemHex: string;
    itemCount: number;
    canonical: boolean;
    trace: MidgardRedeemerItemProofTraceV1 | null;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

/** Evidence is derived from the exact retained field-8 preimage and commitment. */
export const prepareRedeemerCanonicityEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: RedeemerCanonicityFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): RedeemerCanonicityEvidenceV1 => {
  const finding = classifyRedeemerCanonicityFindingV1(rawFinding);
  const actual = midgardFieldCommitmentV1(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field 8 changed commitment");
  const items = decodeMidgardFieldPreimageV1(fieldPreimage);
  const item = items[finding.redeemerIndex];
  if (item === undefined) return fail("redeemer coordinate is outside field 8");
  let canonical = false;
  try {
    const decoded = decodeMidgardRedeemerWitnessItemV1(item);
    canonical =
      ["Spend", "Mint", "Reward", "Receive"].includes(decoded.purpose) &&
      canonicalData(decoded.redeemerCbor);
  } catch {
    canonical = false;
  }
  let trace: MidgardRedeemerItemProofTraceV1 | null = null;
  if (canonical) {
    trace = buildMidgardRedeemerItemProofTraceV1({
      itemIndex: finding.redeemerIndex,
      itemCount: items.length,
      itemBytes: item,
      mode: MidgardRedeemerItemProofModesV1.Data,
    });
  }
  return Object.freeze({
    ...finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    itemHex: item.toString("hex"),
    itemCount: items.length,
    canonical,
    trace,
    carriage: selectMidgardFieldCarriageTierV1(fieldPreimage.length),
  });
};

export const redeemerCanonicityEvidenceClosesV1 = (
  evidence: RedeemerCanonicityEvidenceV1,
): boolean =>
  terminalVerdictContradictionV1(evidence.subject, !evidence.canonical);

export const selectCanonicalRedeemerCanonicityEvidenceV1 = (
  values: readonly RedeemerCanonicityEvidenceV1[],
): RedeemerCanonicityEvidenceV1 => {
  if (values.length === 0) return fail("no authenticated detection");
  return [...values].sort(
    (left, right) =>
      left.redeemerIndex - right.redeemerIndex ||
      left.subject.transaction_id.localeCompare(right.subject.transaction_id),
  )[0]!;
};
